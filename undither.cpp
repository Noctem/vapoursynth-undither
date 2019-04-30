#include <gif_lib.h>
#include <vapoursynth/VapourSynth.h>

#include <algorithm>
#include <climits>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <iterator>
#include <memory>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

using namespace std;

using IndexInt = uint16_t;

struct rgb {
    rgb(GifByteType r1, GifByteType g1, GifByteType b1) : r(r1), g(g1), b(b1) {}

    rgb(GifColorType color) {
        r = color.Red;
        g = color.Green;
        b = color.Blue;
    }

    bool isBlack() const { return (r == 0 && g == 0 && b == 0); }

    bool operator==(const rgb &p2) const {
        return (r == p2.r && g == p2.g && b == p2.b);
    }

    uint32_t operator-(const rgb &p2) const {
        return (r - p2.r) * (r - p2.r) + (g - p2.g) * (g - p2.g) +
               (b - p2.b) * (b - p2.b);
    }

    GifByteType r, g, b;
};

struct UnditherData {
    VSVideoInfo vi;
    int height, width, pixels;
    vector<rgb> *palette;
    vector<IndexInt *> *frames;
};

class Acc {
   public:
    Acc(vector<rgb> *pal, IndexInt *frame, int pixels) : palette(pal) {
        if (pal->size() <= 256) {
            for (IndexInt i = 0; i < pal->size(); i++) localpal.insert(i);
        } else {
            for (int i = 0; i < pixels; i++) localpal.insert(frame[i]);
        }
    }

    void add_center(IndexInt cent) {
        center = cent;
        cpal = (*palette)[center];
        r = cpal.r * center_w;
        g = cpal.g * center_w;
        b = cpal.b * center_w;
        weight = center_w;
    }

    void add(const IndexInt idx, uint8_t w) {
        uint8_t sim = similarity(idx);
        if (sim) {
            w *= sim;
            rgb c = (*palette)[idx];
            r += c.r * w;
            g += c.g * w;
            b += c.b * w;
            weight += w;
        }
    }

    uint32_t r, g, b, weight;

   private:
    unordered_set<IndexInt> localpal;
    unordered_map<uint32_t, uint8_t> simcache;
    const vector<rgb> *palette;
    static const int center_w = 8;
    IndexInt center;
    rgb cpal = {0, 0, 0};

    inline uint32_t similarity(const IndexInt c2) {
        const uint32_t pos = center < c2
                                 ? (static_cast<uint32_t>(center) << 16) | c2
                                 : (static_cast<uint32_t>(c2) << 16) | center;
        auto search = simcache.find(pos);
        if (search != simcache.end()) return search->second;

        const rgb p2 = (*palette)[c2];
        const rgb avg((cpal.r + p2.r) / 2, (cpal.g + p2.g) / 2,
                      (cpal.b + p2.b) / 2);

        const uint32_t allowed_diff = avg - cpal;
        uint32_t min_diff = 1 << 31;

        for (const auto &i : localpal) {
            if (i == center || i == c2) continue;
            uint32_t diff = avg - (*palette)[i];
            if (diff < min_diff) min_diff = diff;
        }

        uint8_t sim;
        if (min_diff >= allowed_diff * 2)
            sim = 8;
        else if (min_diff >= allowed_diff)
            sim = 6;
        else if (min_diff * 2 >= allowed_diff)
            sim = 2;
        else
            sim = 0;

        simcache[pos] = sim;
        return sim;
    }
};

static void VS_CC unditherInit(VSMap *in, VSMap *out, void **instanceData,
                               VSNode *node, VSCore *core, const VSAPI *vsapi) {
    UnditherData *d = static_cast<UnditherData *>(*instanceData);
    vsapi->setVideoInfo(&d->vi, 1, node);
}

static const VSFrameRef *VS_CC unditherGetFrame(
    int n, int activationReason, void **instanceData, void **frameData,
    VSFrameContext *frameCtx, VSCore *core, const VSAPI *vsapi) {
    UnditherData *d = static_cast<UnditherData *>(*instanceData);

    VSFrameRef *dst =
        vsapi->newVideoFrame(d->vi.format, d->width, d->height, 0, core);
    int stride = vsapi->getStride(dst, 0);
    uint8_t *rp = vsapi->getWritePtr(dst, 0);
    uint8_t *gp = vsapi->getWritePtr(dst, 1);
    uint8_t *bp = vsapi->getWritePtr(dst, 2);

    IndexInt *frame = (*d->frames)[n];

    Acc acc(d->palette, frame, d->pixels);

    for (int y = 0; y < d->height; y++) {
        for (int x = 0; x < d->width; x++) {
            int ind = (d->width * y) + x;
            int ind2 = (stride * y) + x;

            acc.add_center(frame[ind]);

            if (y > 0) {
                if (x > 0) acc.add(frame[ind - d->width - 1], 1);
                acc.add(frame[ind - d->width], 2);
                if (x < d->width - 1) acc.add(frame[ind - d->width + 1], 1);
            }

            if (x > 0) acc.add(frame[ind - 1], 2);
            if (x < d->width - 1)
                acc.add(frame[ind + 1], 3);  // floyd-steinberg

            if (y < d->height - 1) {
                if (x > 0)
                    acc.add(frame[ind + d->width - 1], 2);  // floyd-steinberg
                acc.add(frame[ind + d->width], 2);
                if (x < d->width - 1) acc.add(frame[ind + d->width + 1], 1);
            }

            if (acc.weight) {
                rp[ind2] = acc.r / acc.weight;
                gp[ind2] = acc.g / acc.weight;
                bp[ind2] = acc.b / acc.weight;
            } else {
                rp[ind2] = 0;
                gp[ind2] = 0;
                bp[ind2] = 0;
            }
        }
    }

    return dst;
}

static void VS_CC unditherFree(void *instanceData, VSCore *core,
                               const VSAPI *vsapi) {
    UnditherData *d = (UnditherData *)instanceData;

    delete d->palette;
    for (auto &frame : *d->frames) delete[] frame;
    delete d->frames;
    delete d;
}

static int64_t gcd(int64_t m) {
    int64_t remainder;
    int64_t n = 100;

    while (n != 0) {
        remainder = m % n;
        m = n;
        n = remainder;
    }

    return m;
}

static void throwError(VSMap *out, const VSAPI *vsapi, int errorCode) {
    switch (errorCode) {
        case D_GIF_ERR_OPEN_FAILED:
            vsapi->setError(out, "Undither: failed to open GIF file");
            return;
        case D_GIF_ERR_READ_FAILED:
            vsapi->setError(out, "Undither: error reading GIF file");
            return;
        case D_GIF_ERR_NOT_GIF_FILE:
            vsapi->setError(out, "Undither: input is not a GIF file");
            return;
        case D_GIF_ERR_NO_SCRN_DSCR:
            vsapi->setError(out, "Undither: no screen description in GIF file");
            return;
        case D_GIF_ERR_NO_IMAG_DSCR:
            vsapi->setError(out, "Undither: missing image description in file");
            return;
        case D_GIF_ERR_NO_COLOR_MAP:
            vsapi->setError(out, "Undither: file has no color map");
            return;
        case D_GIF_ERR_WRONG_RECORD:
            vsapi->setError(out, "Undither: error, wrong record");
            return;
        case D_GIF_ERR_DATA_TOO_BIG:
            vsapi->setError(out, "Undither: error, data too big");
            return;
        case D_GIF_ERR_NOT_ENOUGH_MEM:
            vsapi->setError(out, "Undither: error, not enough memory");
            return;
        case D_GIF_ERR_CLOSE_FAILED:
            vsapi->setError(out, "Undither: error, closing failed");
            return;
        case D_GIF_ERR_NOT_READABLE:
            vsapi->setError(out, "Undither: error, not readable");
            return;
        case D_GIF_ERR_IMAGE_DEFECT:
            vsapi->setError(out, "Undither: error, image defect");
            return;
        case D_GIF_ERR_EOF_TOO_SOON:
            vsapi->setError(out, "Undither: error, EOF too soon");
            return;
        default:
            vsapi->setError(out, "Undither: unknown error");
            return;
    }
}

static void VS_CC unditherCreate(const VSMap *in, VSMap *out, void *userData,
                                 VSCore *core, const VSAPI *vsapi) {
    unique_ptr<UnditherData> d(new UnditherData);

    int err = D_GIF_SUCCEEDED;

    GifFileType *gif =
        DGifOpenFileName(vsapi->propGetData(in, "path", 0, 0), &err);

    if (gif == nullptr) {
        throwError(out, vsapi, err);
        return;
    }

    d->vi.format = vsapi->getFormatPreset(pfRGB24, core);
    d->width = d->vi.width = gif->SWidth;
    d->height = d->vi.height = gif->SHeight;
    d->pixels = d->width * d->height;

    int64_t delaySum = 0;
    int delayCount = 0;

    d->palette = new vector<rgb>;
    d->frames = new vector<IndexInt *>;

    IndexInt canvas[d->pixels];
    fill(canvas, canvas + d->pixels, gif->SBackGroundColor);

    IndexInt globalRemap[256];
    IndexInt localRemap[256];
    IndexInt *remap;

    // load global color map into palette
    if (gif->SColorMap != nullptr) {
        for (int i = 0; i < gif->SColorMap->ColorCount; i++) {
            d->palette->emplace_back(gif->SColorMap->Colors[i]);
            globalRemap[i] = i;
        }
        // remove redundant black pixels from end
        while (d->palette->back().isBlack()) d->palette->pop_back();
        int ind = d->palette->size();
        if (ind != gif->SColorMap->ColorCount) {
            fill(globalRemap + ind, globalRemap + 256, ind);
            d->palette->emplace_back(0, 0, 0);
        }
    } else {
        fill(globalRemap, globalRemap + 256, 0);
    }

    if (DGifSlurp(gif) == GIF_ERROR) {
        throwError(out, vsapi, gif->Error);
        DGifCloseFile(gif, &err);
        return;
    }

    d->vi.numFrames = gif->ImageCount;

    for (int i = 0; i < gif->ImageCount; i++) {
        SavedImage *sp = gif->SavedImages + i;

        int disposal = DISPOSAL_UNSPECIFIED;
        int transparentIndex = -1;

        for (int j = 0; j < sp->ExtensionBlockCount; j++) {
            if (sp->ExtensionBlocks[j].Function == GRAPHICS_EXT_FUNC_CODE) {
                GraphicsControlBlock gcb;
                if (DGifExtensionToGCB(sp->ExtensionBlocks[j].ByteCount,
                                       sp->ExtensionBlocks[j].Bytes,
                                       &gcb) == GIF_OK) {
                    disposal = gcb.DisposalMode;
                    transparentIndex = gcb.TransparentColor;
                    delaySum += gcb.DelayTime;
                    delayCount++;
                    break;
                }
            }
        }

        if (sp->ImageDesc.ColorMap != nullptr) {
            remap = localRemap;
            for (int j = 0; j < sp->ImageDesc.ColorMap->ColorCount; j++) {
                if (j == transparentIndex) continue;

                rgb cmp(sp->ImageDesc.ColorMap->Colors[j]);

                auto it = find(d->palette->begin(), d->palette->end(), cmp);
                if (it == d->palette->end()) {
                    d->palette->push_back(cmp);
                    remap[j] = d->palette->size() - 1;
                } else
                    remap[j] = distance(d->palette->begin(), it);
            }
        } else
            remap = globalRemap;

        if (d->palette->size() > USHRT_MAX) {
            vsapi->setError(out,
                            "Undither: combined palette size is larger than "
                            "uint16_t, recompile with larger IndexInt.");
            DGifCloseFile(gif, &err);
            return;
        }

        for (int y = 0; y < sp->ImageDesc.Height; y++) {
            int pos = (sp->ImageDesc.Top + y) * d->width;
            for (int x = 0; x < sp->ImageDesc.Width; x++) {
                GifByteType ind = sp->RasterBits[y * sp->ImageDesc.Width + x];
                if (ind == transparentIndex) {
                    switch (disposal) {
                        case DISPOSE_BACKGROUND:
                            canvas[pos + sp->ImageDesc.Left + x] =
                                gif->SBackGroundColor;
                            break;
                        case DISPOSE_PREVIOUS:
                            if (i >= 2)
                                canvas[pos + sp->ImageDesc.Left + x] =
                                    (*d->frames)[i - 2]
                                                [pos + sp->ImageDesc.Left + x];
                            break;
                        default:
                            break;
                    }
                } else {
                    canvas[pos + sp->ImageDesc.Left + x] = remap[ind];
                }
            }
        }
        IndexInt *frame = new IndexInt[d->pixels];
        copy(canvas, canvas + d->pixels, frame);
        d->frames->push_back(frame);
    }

    DGifCloseFile(gif, &err);
    if (err) cerr << "error closing file" << endl;

    if (delaySum == 0) {
        cerr << "No frame durations found, defaulting to 10FPS" << endl;
        d->vi.fpsNum = 10;
        d->vi.fpsDen = 1;
    } else {
        double average = (delaySum / static_cast<double>(d->vi.numFrames));
        if (average > 3.28 && average < 3.38) {
            d->vi.fpsNum = 30000;
            d->vi.fpsDen = 1001;
        } else if (average > 4.09 && average < 4.25) {
            d->vi.fpsNum = 24000;
            d->vi.fpsDen = 1001;
        } else if (average > 1.65 && average < 1.68) {
            d->vi.fpsNum = 60000;
            d->vi.fpsDen = 1001;
        } else {
            delaySum /= d->vi.numFrames;
            int64_t divisor = gcd(delaySum);
            d->vi.fpsNum = 100 / divisor;
            d->vi.fpsDen = delaySum / divisor;
        }
    }

    vsapi->createFilter(in, out, "Undither", unditherInit, unditherGetFrame,
                        unditherFree, fmParallel, 0, d.release(), core);
}

VS_EXTERNAL_API(void)
VapourSynthPluginInit(VSConfigPlugin configFunc,
                      VSRegisterFunction registerFunc, VSPlugin *plugin) {
    configFunc("xyz.noctem.undither", "undither",
               "Smart filter to remove Floyd-Steinberg dithering from paletted "
               "images.",
               VAPOURSYNTH_API_VERSION, 1, plugin);
    registerFunc("Undither", "path:data[];", unditherCreate, nullptr, plugin);
}
