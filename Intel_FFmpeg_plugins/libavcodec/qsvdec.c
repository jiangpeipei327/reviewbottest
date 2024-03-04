/*
 * Intel MediaSDK QSV codec-independent code
 *
 * copyright (c) 2013 Luca Barbato
 * copyright (c) 2015 Anton Khirnov <anton@khirnov.net>
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include <string.h>
#include <sys/types.h>

#include <mfx/mfxvideo.h>

#include "libavutil/common.h"
#include "libavutil/hwcontext.h"
#include "libavutil/hwcontext_qsv.h"
#include "libavutil/mem.h"
#include "libavutil/log.h"
#include "libavutil/pixdesc.h"
#include "libavutil/pixfmt.h"
#include "libavutil/time.h"
#include "libavutil/imgutils.h"

#include "avcodec.h"
#include "internal.h"
#include "qsv.h"
#include "qsv_internal.h"
#include "qsvdec.h"

static int qsv_init_session(AVCodecContext *avctx, QSVContext *q, mfxSession session,
                            AVBufferRef *hw_frames_ref, AVBufferRef *hw_device_ref)
{
    int ret;

    if (session) {
        q->session = session;
    } else if (hw_frames_ref) {
        if (q->internal_session) {
            MFXClose(q->internal_session);
            q->internal_session = NULL;
        }
        av_buffer_unref(&q->frames_ctx.hw_frames_ctx);

        q->frames_ctx.hw_frames_ctx = av_buffer_ref(hw_frames_ref);
        if (!q->frames_ctx.hw_frames_ctx)
            return AVERROR(ENOMEM);

        ret = ff_qsv_init_session_frames(avctx, &q->internal_session,
                                         &q->frames_ctx, q->load_plugins,
                                         q->iopattern == MFX_IOPATTERN_OUT_OPAQUE_MEMORY,
                                         q->gpu_copy);
        if (ret < 0) {
            av_buffer_unref(&q->frames_ctx.hw_frames_ctx);
            return ret;
        }

        q->session = q->internal_session;
    } else if (hw_device_ref) {
        if (q->internal_session) {
            MFXClose(q->internal_session);
            q->internal_session = NULL;
        }

        ret = ff_qsv_init_session_device(avctx, &q->internal_session,
                                         hw_device_ref, q->load_plugins, q->gpu_copy);
        if (ret < 0)
            return ret;

        q->session = q->internal_session;
    } else {
        if (!q->internal_session) {
            ret = ff_qsv_init_internal_session(avctx, &q->internal_session,
                                               q->load_plugins, q->gpu_copy);
            if (ret < 0)
                return ret;
        }

        q->session = q->internal_session;
    }

    /* make sure the decoder is uninitialized */
    MFXVideoDECODE_Close(q->session);

    return 0;
}

static int qsv_decode_init(AVCodecContext *avctx, QSVContext *q, mfxBitstream *bs)
{
    const AVPixFmtDescriptor *desc;
    mfxSession  session = NULL;
    int       iopattern = 0;
    mfxVideoParam param = { 0 };
    int ret;

    desc = av_pix_fmt_desc_get(avctx->sw_pix_fmt);
    if (!desc)
        return AVERROR_BUG;

    if (!q->async_fifo) {
        q->async_fifo = av_fifo_alloc((1 + q->async_depth) *
                                      (sizeof(mfxSyncPoint*) + sizeof(QSVFrame*)));
        if (!q->async_fifo)
            return AVERROR(ENOMEM);
    }

    if (avctx->pix_fmt == AV_PIX_FMT_QSV && avctx->hwaccel_context) {
        AVQSVContext *user_ctx = avctx->hwaccel_context;
        session           = user_ctx->session;
        iopattern         = user_ctx->iopattern;
        q->ext_buffers    = user_ctx->ext_buffers;
        q->nb_ext_buffers = user_ctx->nb_ext_buffers;
    }

    if (avctx->hw_frames_ctx) {
        AVHWFramesContext    *frames_ctx = (AVHWFramesContext*)avctx->hw_frames_ctx->data;
        AVQSVFramesContext *frames_hwctx = frames_ctx->hwctx;

        if (!iopattern) {
            if (frames_hwctx->frame_type & MFX_MEMTYPE_OPAQUE_FRAME)
                iopattern = MFX_IOPATTERN_OUT_OPAQUE_MEMORY;
            else if (frames_hwctx->frame_type & MFX_MEMTYPE_VIDEO_MEMORY_DECODER_TARGET)
                iopattern = MFX_IOPATTERN_OUT_VIDEO_MEMORY;
        }
    }

    if (!iopattern)
        iopattern = MFX_IOPATTERN_OUT_SYSTEM_MEMORY;
    q->iopattern = iopattern;

    ret = qsv_init_session(avctx, q, session, avctx->hw_frames_ctx, avctx->hw_device_ctx);
    if (ret < 0) {
        av_log(avctx, AV_LOG_ERROR, "Error initializing an MFX session\n");
        return ret;
    }

    ret = ff_qsv_codec_id_to_mfx(avctx->codec_id);
    if (ret < 0)
        return ret;

    param.mfx.CodecId = ret;

    ret = MFXVideoDECODE_DecodeHeader(q->session, bs, &param);
    if (ret < 0)
        return ff_qsv_print_error(avctx, ret,
                                  "Error decoding stream header");

    param.IOPattern   = q->iopattern;
    param.AsyncDepth  = q->async_depth;
    param.ExtParam    = q->ext_buffers;
    param.NumExtParam = q->nb_ext_buffers;

    ret = MFXVideoDECODE_Init(q->session, &param);
    if (ret < 0)
        return ff_qsv_print_error(avctx, ret,
                                  "Error initializing the MFX video decoder");

#ifndef USE_PARSER
    avctx->width        = param.mfx.FrameInfo.CropW;
    avctx->height       = param.mfx.FrameInfo.CropH;
    avctx->coded_width  = param.mfx.FrameInfo.Width;
    avctx->coded_height = param.mfx.FrameInfo.Height;
    avctx->level        = param.mfx.CodecProfile;
    avctx->profile      = param.mfx.CodecLevel;
#endif

    q->frame_info = param.mfx.FrameInfo;

    if (avctx->pix_fmt != AV_PIX_FMT_QSV)
        q->pool = av_buffer_pool_init(av_image_get_buffer_size(avctx->pix_fmt,
                                                         FFALIGN(avctx->width, 128),
                                                         FFALIGN(avctx->height, 64), 1),
                                      av_buffer_allocz);

    return 0;
}

static int alloc_frame(AVCodecContext *avctx, QSVContext *q, QSVFrame *frame)
{
    int ret;

    if (!q->pool) {
        ret = ff_get_buffer(avctx, frame->frame, AV_GET_BUFFER_FLAG_REF);
        if (ret < 0)
            return ret;
    } else {
        ff_decode_frame_props(avctx, frame->frame);

        frame->frame->width       = avctx->width;
        frame->frame->height      = avctx->height;
        frame->frame->linesize[0] = FFALIGN(avctx->width, 128);
        frame->frame->linesize[1] = frame->frame->linesize[0];
        frame->frame->buf[0]      = av_buffer_pool_get(q->pool);
        if (!frame->frame->buf[0])
            return AVERROR(ENOMEM);

        frame->frame->data[0] = frame->frame->buf[0]->data;
        frame->frame->data[1] = frame->frame->data[0] +
                                frame->frame->linesize[0] * FFALIGN(avctx->height, 64);
    }

    if (frame->frame->format == AV_PIX_FMT_QSV) {
        frame->surface = *(mfxFrameSurface1*)frame->frame->data[3];
    } else {
        frame->surface.Info = q->frame_info;

        frame->surface.Data.PitchLow = frame->frame->linesize[0];
        frame->surface.Data.Y        = frame->frame->data[0];
        frame->surface.Data.UV       = frame->frame->data[1];
    }

    if (q->frames_ctx.mids) {
        ret = ff_qsv_find_surface_idx(&q->frames_ctx, frame);
        if (ret < 0)
            return ret;

        frame->surface.Data.MemId = &q->frames_ctx.mids[ret];
    }
    frame->surface.Data.ExtParam = &frame->ext_param;
    frame->surface.Data.NumExtParam = 1;
    frame->ext_param = (mfxExtBuffer*)&frame->dec_info;
    frame->dec_info.Header.BufferId = MFX_EXTBUFF_DECODED_FRAME_INFO;
    frame->dec_info.Header.BufferSz = sizeof(frame->dec_info);

    frame->used = 1;

    return 0;
}

static void qsv_clear_unused_frames(QSVContext *q)
{
    QSVFrame *cur = q->work_frames;
    while (cur) {
        if (cur->used && !cur->surface.Data.Locked && !cur->queued) {
            cur->used = 0;
            av_frame_unref(cur->frame);
        }
        cur = cur->next;
    }
}

static int get_surface(AVCodecContext *avctx, QSVContext *q, mfxFrameSurface1 **surf)
{
    QSVFrame *frame, **last;
    int ret;

    qsv_clear_unused_frames(q);

    frame = q->work_frames;
    last  = &q->work_frames;
    while (frame) {
        if (!frame->used) {
            ret = alloc_frame(avctx, q, frame);
            if (ret < 0)
                return ret;
            *surf = &frame->surface;
            return 0;
        }

        last  = &frame->next;
        frame = frame->next;
    }

    frame = av_mallocz(sizeof(*frame));
    if (!frame)
        return AVERROR(ENOMEM);
    frame->frame = av_frame_alloc();
    if (!frame->frame) {
        av_freep(&frame);
        return AVERROR(ENOMEM);
    }
    *last = frame;

    ret = alloc_frame(avctx, q, frame);
    if (ret < 0)
        return ret;

    *surf = &frame->surface;

    return 0;
}

static QSVFrame *find_frame(QSVContext *q, mfxFrameSurface1 *surf)
{
    QSVFrame *cur = q->work_frames;
    while (cur) {
        if (surf == &cur->surface)
            return cur;
        cur = cur->next;
    }
    return NULL;
}

static int qsv_decode(AVCodecContext *avctx, QSVContext *q,
                      AVFrame *frame, int *got_frame,
                      mfxBitstream *bs)
{
    QSVFrame *out_frame;
    mfxFrameSurface1 *insurf;
    mfxFrameSurface1 *outsurf;
    mfxSyncPoint *sync;
    int ret;

    sync = av_mallocz(sizeof(*sync));
    if (!sync) {
        av_freep(&sync);
        return AVERROR(ENOMEM);
    }

    do {
        ret = get_surface(avctx, q, &insurf);
        if (ret < 0) {
            av_freep(&sync);
            return ret;
        }

        ret = MFXVideoDECODE_DecodeFrameAsync(q->session, bs, insurf, &outsurf, sync);
        if (ret == MFX_WRN_DEVICE_BUSY)
            av_usleep(500);

    } while (ret == MFX_WRN_DEVICE_BUSY || ret == MFX_ERR_MORE_SURFACE);

    if (ret == MFX_ERR_INCOMPATIBLE_VIDEO_PARAM) {
        q->reinit_pending = 1;
        av_freep(&sync);
        ff_qsv_print_warning(avctx, ret, "Warning during QSV decoding");
        return bs->DataOffset;
    }

    if (ret != MFX_ERR_NONE &&
        ret != MFX_ERR_MORE_DATA &&
        ret != MFX_WRN_VIDEO_PARAM_CHANGED &&
        ret != MFX_ERR_MORE_SURFACE) {
        av_freep(&sync);
        return ff_qsv_print_error(avctx, ret,
                                  "Error during QSV decoding");
    }

    /* make sure we do not enter an infinite loop if the SDK
     * did not consume any data and did not return anything */
    if (!*sync && bs && !bs->DataOffset) {
        bs->DataOffset = bs->DataLength;
        ++q->zero_consume_run;
        if (q->zero_consume_run > 1)
            ff_qsv_print_warning(avctx, ret, "A decode call did not consume any data");
    } else {
        q->zero_consume_run = 0;
    }

    if (*sync) {
        QSVFrame *out_frame = find_frame(q, outsurf);

        if (!out_frame) {
            av_log(avctx, AV_LOG_ERROR,
                   "The returned surface does not correspond to any frame\n");
            av_freep(&sync);
            return AVERROR_BUG;
        }

        out_frame->queued = 1;
        av_fifo_generic_write(q->async_fifo, &out_frame, sizeof(out_frame), NULL);
        av_fifo_generic_write(q->async_fifo, &sync,      sizeof(sync),      NULL);
    } else {
        av_freep(&sync);
    }

    if (!av_fifo_space(q->async_fifo) ||
        (!bs && av_fifo_size(q->async_fifo))) {
        AVFrame *src_frame;

        av_fifo_generic_read(q->async_fifo, &out_frame, sizeof(out_frame), NULL);
        av_fifo_generic_read(q->async_fifo, &sync,      sizeof(sync),      NULL);
        out_frame->queued = 0;

        do {
            ret = MFXVideoCORE_SyncOperation(q->session, *sync, 1000);
        } while (ret == MFX_WRN_IN_EXECUTION);

        av_freep(&sync);

        src_frame = out_frame->frame;

        ret = av_frame_ref(frame, src_frame);
        if (ret < 0)
            return ret;

        outsurf = &out_frame->surface;

#if FF_API_PKT_PTS
FF_DISABLE_DEPRECATION_WARNINGS
        frame->pkt_pts = outsurf->Data.TimeStamp;
FF_ENABLE_DEPRECATION_WARNINGS
#endif
        frame->pts = outsurf->Data.TimeStamp;

        frame->repeat_pict =
            outsurf->Info.PicStruct & MFX_PICSTRUCT_FRAME_TRIPLING ? 4 :
            outsurf->Info.PicStruct & MFX_PICSTRUCT_FRAME_DOUBLING ? 2 :
            outsurf->Info.PicStruct & MFX_PICSTRUCT_FIELD_REPEATED ? 1 : 0;
        frame->top_field_first =
            outsurf->Info.PicStruct & MFX_PICSTRUCT_FIELD_TFF;
        frame->interlaced_frame =
            !(outsurf->Info.PicStruct & MFX_PICSTRUCT_PROGRESSIVE);
        if (avctx->field_order == AV_FIELD_UNKNOWN)
            avctx->field_order = ff_qsv_map_picstruct(outsurf->Info.PicStruct);
        frame->pict_type = ff_qsv_map_pictype(out_frame->dec_info.FrameType);
        frame->key_frame = !!(out_frame->dec_info.FrameType & MFX_FRAMETYPE_IDR);

        /* update the surface properties */
        if (avctx->pix_fmt == AV_PIX_FMT_QSV)
            ((mfxFrameSurface1*)frame->data[3])->Info = outsurf->Info;

        *got_frame = 1;
    }

    return bs ? bs->DataOffset : 0;
}

int ff_qsv_decode_preinit(AVCodecContext *avctx, QSVContext *q)
{
    if (!avctx || !q)
        return AVERROR(EINVAL);

    q->reinit_pending = 1;

    return 0;
}

int ff_qsv_decode_close(QSVContext *q)
{
    QSVFrame *cur = q->work_frames;

    if (q->session)
        MFXVideoDECODE_Close(q->session);

    while (q->async_fifo && av_fifo_size(q->async_fifo)) {
        QSVFrame *out_frame;
        mfxSyncPoint *sync;

        av_fifo_generic_read(q->async_fifo, &out_frame, sizeof(out_frame), NULL);
        av_fifo_generic_read(q->async_fifo, &sync,      sizeof(sync),      NULL);

        av_freep(&sync);
    }

    while (cur) {
        q->work_frames = cur->next;
        av_frame_free(&cur->frame);
        av_freep(&cur);
        cur = q->work_frames;
    }

    av_fifo_free(q->async_fifo);
    q->async_fifo = NULL;

#ifdef USE_PARSER
    av_parser_close(q->parser);
    avcodec_free_context(&q->avctx_internal);
#endif

    if (q->internal_session)
        MFXClose(q->internal_session);

    av_buffer_unref(&q->frames_ctx.hw_frames_ctx);
    av_buffer_unref(&q->frames_ctx.mids_buf);
    av_buffer_pool_uninit(&q->pool);

    return 0;
}

int ff_qsv_process_data(AVCodecContext *avctx, QSVContext *q,
                        AVFrame *frame, int *got_frame, AVPacket *pkt)
{
    int ret;
    mfxBitstream bs = { { { 0 } } };

#ifdef USE_PARSER
    uint8_t *dummy_data;
    int dummy_size;

    if (!q->avctx_internal) {
        q->avctx_internal = avcodec_alloc_context3(NULL);
        if (!q->avctx_internal)
            return AVERROR(ENOMEM);

        q->parser = av_parser_init(avctx->codec_id);
        if (!q->parser)
            return AVERROR(ENOMEM);

        q->parser->flags |= PARSER_FLAG_COMPLETE_FRAMES;
        q->orig_pix_fmt   = AV_PIX_FMT_NONE;
    }
#endif

    if (!pkt->size)
        return qsv_decode(avctx, q, frame, got_frame, NULL);
    else {
        bs.Data       = pkt->data;
        bs.DataLength = pkt->size;
        bs.MaxLength  = bs.DataLength;
        bs.TimeStamp  = pkt->pts;
        /*If we confirm the input stream is progressive,
         * we set this flag to make MSDK output frames as
         * soon as possible.
         */
        if (avctx->field_order == AV_FIELD_UNKNOWN ||
            avctx->field_order == AV_FIELD_PROGRESSIVE)
            bs.DataFlag   |= MFX_BITSTREAM_COMPLETE_FRAME;
    }

#ifdef USE_PARSER
    /* we assume the packets are already split properly and want
     * just the codec parameters here */
    av_parser_parse2(q->parser, q->avctx_internal,
                     &dummy_data, &dummy_size,
                     pkt->data, pkt->size, pkt->pts, pkt->dts,
                     pkt->pos);

    /* TODO: flush delayed frames on reinit */
    if (q->parser->format       != q->orig_pix_fmt    ||
        q->parser->coded_width  != avctx->coded_width ||
        q->parser->coded_height != avctx->coded_height) {
        enum AVPixelFormat pix_fmts[3] = { AV_PIX_FMT_QSV,
                                           AV_PIX_FMT_NONE,
                                           AV_PIX_FMT_NONE };
        enum AVPixelFormat qsv_format;

        qsv_format = ff_qsv_map_pixfmt(q->parser->format, &q->fourcc);
        if (AV_PIX_FMT_NONE == qsv_format) {
            av_log(avctx, AV_LOG_ERROR,
                   "Decoding pixel format '%s' is not supported\n",
                   av_get_pix_fmt_name(q->parser->format));
            ret = AVERROR(ENOSYS);
            goto reinit_fail;
        }

        q->orig_pix_fmt     = q->parser->format;
        avctx->pix_fmt      = pix_fmts[1] = qsv_format;
        avctx->width        = q->parser->width;
        avctx->height       = q->parser->height;
        avctx->coded_width  = q->parser->coded_width;
        avctx->coded_height = q->parser->coded_height;
        avctx->field_order  = q->parser->field_order;
        avctx->level        = q->avctx_internal->level;
        avctx->profile      = q->avctx_internal->profile;

        ret = ff_get_format(avctx, pix_fmts);
        if (ret < 0)
            goto reinit_fail;

        avctx->pix_fmt = ret;

        ret = qsv_decode_init(avctx, q, &bs);
        if (ret < 0)
            goto reinit_fail;
    }
#else
    if (q->reinit_pending) {
        enum AVPixelFormat pix_fmts[3] = { AV_PIX_FMT_QSV,
                                           AV_PIX_FMT_NV12,
                                           AV_PIX_FMT_NONE };
        ret = ff_get_format(avctx, pix_fmts);
        if (ret < 0)
            goto reinit_fail;

        avctx->pix_fmt = ret;

        ret = qsv_decode_init(avctx, q, &bs);
        if (ret < 0)
            goto reinit_fail;
        q->reinit_pending = 0;
    }
#endif

    return qsv_decode(avctx, q, frame, got_frame, &bs);

reinit_fail:
#ifdef USE_PARSER
    q->orig_pix_fmt = q->parser->format = avctx->pix_fmt = AV_PIX_FMT_NONE;
#endif
    return ret;
}

void ff_qsv_decode_flush(AVCodecContext *avctx, QSVContext *q)
{
#ifdef USE_PARSER
    q->orig_pix_fmt = AV_PIX_FMT_NONE;
#else
    q->reinit_pending = 1;
#endif
}
