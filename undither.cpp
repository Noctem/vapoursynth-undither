#include <gif_lib.h>
#include <vapoursynth/VapourSynth.h>

#include <algorithm>
#include <array>
#include <climits>
#include <cstddef>
#include <forward_list>
#include <iostream>
#include <utility>
#include <vector>

using namespace std;

struct Pixel;
struct RGB;
using Palette = vector<RGB>;
using PalIt = forward_list<Palette>::const_iterator;

struct RGB {
    RGB() = default;
    RGB(uint8_t r, uint8_t g, uint8_t b) : r(r), g(g), b(b) {}

    RGB(const GifColorType &color)
        : r(color.Red), g(color.Green), b(color.Blue) {}

    RGB avg(const RGB &p2) const {
        return RGB((r + p2.r) / 2, (g + p2.g) / 2, (b + p2.b) / 2);
    }

    bool operator==(const RGB &p2) const {
        return (r == p2.r && g == p2.g && b == p2.b);
    }

    uint32_t operator-(const RGB &p2) const {
        return (r - p2.r) * (r - p2.r) + (g - p2.g) * (g - p2.g) +
               (b - p2.b) * (b - p2.b);
    }

    uint8_t r, g, b;
};

struct Pixel {
    Pixel() = default;
    Pixel(uint8_t ind, PalIt palette) : ind(ind), palette(palette) {}

    const RGB &color() const { return (*palette)[ind]; }

    bool operator==(const Pixel &p2) const {
        return (ind == p2.ind && palette == p2.palette);
    }

    bool operator>(const Pixel &p2) const { return ind > p2.ind; }

    uint8_t ind;
    PalIt palette;
};

class Acc {
   public:
    virtual ~Acc() = default;

    static void clearCache() { fill(simcache.begin(), simcache.end(), -1); }

    uint32_t r, g, b, weight;

   protected:
    static array<char, 256 * 256> simcache;
    static const int center_w = 8;
    RGB crgb;
    uint8_t cind;
};

class LocalAcc : public Acc {
   public:
    void add(const Pixel &pix, uint32_t w) {
        RGB color;
        w *= similarity(pix, color);
        if (w) {
            r += color.r * w;
            g += color.g * w;
            b += color.b * w;
            weight += w;
        }
    }

    void add_center(const Pixel &cent) {
        if (cpalit != cent.palette) clearCache();
        cpalit = cent.palette;
        crgb = cent.color();
        r = crgb.r * center_w;
        g = crgb.g * center_w;
        b = crgb.b * center_w;
        cind = cent.ind;
        weight = center_w;
    }

   private:
    inline uint32_t similarity(const Pixel &p2, RGB &c2) {
        char sim;
        uint16_t hash;
        if (p2.palette == cpalit) {
            hash = cind > p2.ind ? static_cast<uint16_t>(cind << 8) | p2.ind
                                 : static_cast<uint16_t>(p2.ind << 8) | cind;
            sim = simcache[hash];
            if (sim >= 0) {
                if (sim != 0) c2 = p2.color();
                return sim;
            }
        }

        c2 = p2.color();
        const RGB avg = crgb.avg(c2);

        const uint32_t allowed_diff = avg - crgb;
        uint32_t min_diff = UINT_MAX;

        if (cpalit == p2.palette) {
            for (int i = 0; i < cpalit->size(); i++) {
                if (i == cind || i == p2.ind) continue;
                uint32_t diff = avg - (*cpalit)[i];
                if (diff < min_diff) min_diff = diff;
            }
        } else {
            for (int i = 0; i < cpalit->size(); i++) {
                const RGB color = (*cpalit)[i];
                if (i == cind || color == c2) continue;
                uint32_t diff = avg - color;
                if (diff < min_diff) min_diff = diff;
            }
            for (int i = 0; i < p2.palette->size(); i++) {
                const RGB color = (*cpalit)[i];
                if (i == p2.ind || color == crgb) continue;
                uint32_t diff = avg - color;
                if (diff < min_diff) min_diff = diff;
            }
        }

        if (min_diff >= allowed_diff * 2)
            sim = 8;
        else if (min_diff >= allowed_diff)
            sim = 6;
        else if (min_diff * 2 >= allowed_diff)
            sim = 2;
        else
            sim = 0;

        if (p2.palette == cpalit) simcache[hash] = sim;
        return sim;
    }

    PalIt cpalit;
};

class GlobalAcc : public Acc {
   public:
    void add(const uint8_t pix, uint32_t w) {
        RGB color;
        w *= similarity(pix, color);
        if (w) {
            r += color.r * w;
            g += color.g * w;
            b += color.b * w;
            weight += w;
        }
    }

    void add_center(const uint8_t cent) {
        cind = cent;
        crgb = palette[cind];
        r = crgb.r * center_w;
        g = crgb.g * center_w;
        b = crgb.b * center_w;
        weight = center_w;
    }

    static void setPalette(const Palette &pal) { palette = pal; }

   private:
    inline uint32_t similarity(const uint8_t p2, RGB &c2) {
        char sim;
        uint16_t hash = cind > p2 ? static_cast<uint16_t>(cind << 8) | p2
                                  : static_cast<uint16_t>(p2 << 8) | cind;
        sim = simcache[hash];
        if (sim >= 0) {
            if (sim != 0) c2 = palette[p2];
            return sim;
        }

        c2 = palette[p2];
        const RGB avg = crgb.avg(c2);

        const uint32_t allowed_diff = avg - crgb;
        uint32_t min_diff = UINT_MAX;

        for (int i = 0; i < palette.size(); i++) {
            if (i == cind || i == p2) continue;
            uint32_t diff = avg - palette[i];
            if (diff < min_diff) min_diff = diff;
        }

        if (min_diff >= allowed_diff * 2)
            sim = 8;
        else if (min_diff >= allowed_diff)
            sim = 6;
        else if (min_diff * 2 >= allowed_diff)
            sim = 2;
        else
            sim = 0;

        simcache[hash] = sim;
        return sim;
    }

    static Palette palette;
};

array<char, 256 * 256> Acc::simcache;
Palette GlobalAcc::palette;

struct UnditherData {
    VSVideoInfo vi;
    int height, width;
};

struct GlobalData : UnditherData {
    vector<vector<uint8_t>> *frames;
};

struct LocalData : UnditherData {
    vector<vector<Pixel>> *frames;
    forward_list<Palette> *palettes;
};

template <class D>
static void VS_CC unditherInit(VSMap *in, VSMap *out, void **instanceData,
                               VSNode *node, VSCore *core, const VSAPI *vsapi) {
    D *d = static_cast<D *>(*instanceData);
    vsapi->setVideoInfo(&d->vi, 1, node);
}

template <class A, class D>
static const VSFrameRef *VS_CC unditherGetFrame(
    int n, int activationReason, void **instanceData, void **frameData,
    VSFrameContext *frameCtx, VSCore *core, const VSAPI *vsapi) {
    D *d = static_cast<D *>(*instanceData);

    VSFrameRef *dst =
        vsapi->newVideoFrame(d->vi.format, d->width, d->height, 0, core);
    int stride = vsapi->getStride(dst, 0);
    uint8_t *rp = vsapi->getWritePtr(dst, 0);
    uint8_t *gp = vsapi->getWritePtr(dst, 1);
    uint8_t *bp = vsapi->getWritePtr(dst, 2);

    auto frame = (*d->frames)[n];

    A acc;

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

static void VS_CC globalFree(void *instanceData, VSCore *core,
                             const VSAPI *vsapi) {
    GlobalData *d = (GlobalData *)instanceData;

    delete d->frames;
    delete d;
}

static void VS_CC localFree(void *instanceData, VSCore *core,
                            const VSAPI *vsapi) {
    LocalData *d = (LocalData *)instanceData;

    delete d->palettes;
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

static UnditherData *createLocal(Palette &global, GifFileType *gif,
                                 int64_t &delaySum, int &delayCount) {
    LocalData *d = new LocalData;

    d->palettes = new forward_list<Palette>;
    d->palettes->push_front(move(global));
    PalIt globalRef = d->palettes->cbegin();

    d->frames = new vector<vector<Pixel>>;
    d->frames->reserve(gif->ImageCount);

    vector<Pixel> canvas;
    canvas.resize(gif->SWidth * gif->SHeight,
                  Pixel(gif->SBackGroundColor, globalRef));

    for (int frameNum = 0; frameNum < gif->ImageCount; frameNum++) {
        SavedImage *sp = gif->SavedImages + frameNum;

        int disposal = DISPOSAL_UNSPECIFIED;
        int transparentIndex = -1;

        for (int i = 0; i < sp->ExtensionBlockCount; i++) {
            if (sp->ExtensionBlocks[i].Function == GRAPHICS_EXT_FUNC_CODE) {
                GraphicsControlBlock gcb;
                if (DGifExtensionToGCB(sp->ExtensionBlocks[i].ByteCount,
                                       sp->ExtensionBlocks[i].Bytes,
                                       &gcb) == GIF_OK) {
                    disposal = gcb.DisposalMode;
                    transparentIndex = gcb.TransparentColor;
                    delaySum += gcb.DelayTime;
                    delayCount++;
                    break;
                }
            }
        }

        PalIt palit;

        if (sp->ImageDesc.ColorMap != nullptr) {
            Palette palette;
            palette.reserve(sp->ImageDesc.ColorMap->ColorCount);
            for (int i = 0; i < sp->ImageDesc.ColorMap->ColorCount; i++)
                palette.emplace_back(sp->ImageDesc.ColorMap->Colors[i]);
            if (palette != d->palettes->front())
                d->palettes->push_front(move(palette));
            palit = d->palettes->cbegin();
        } else
            palit = globalRef;

        int padding = gif->SWidth - sp->ImageDesc.Width - sp->ImageDesc.Left;
        uint8_t *bits = sp->RasterBits;
        auto it = canvas.begin();
        for (int y = 0; y < sp->ImageDesc.Height; y++) {
            it += sp->ImageDesc.Left;
            for (int x = 0; x < sp->ImageDesc.Width; x++) {
                uint8_t ind = *bits++;
                if (ind == transparentIndex) {
                    switch (disposal) {
                        case DISPOSE_BACKGROUND:
                            *it++ = Pixel(gif->SBackGroundColor, globalRef);
                            break;
                        case DISPOSE_PREVIOUS:
                            if (frameNum >= 2)
                                *it++ = (*d->frames)[frameNum - 2]
                                                    [(sp->ImageDesc.Top + y) *
                                                         gif->SWidth +
                                                     sp->ImageDesc.Left + x];
                            break;
                        default:
                            it++;
                            break;
                    }
                } else {
                    *it++ = Pixel(ind, palit);
                }
            }
            it += padding;
        }
        d->frames->push_back(canvas);
    }
    return d;
}

static UnditherData *createGlobal(Palette &global, GifFileType *gif,
                                  int64_t &delaySum, int &delayCount) {
    GlobalData *d = new GlobalData;
    GlobalAcc::setPalette(global);
    Acc::clearCache();
    d->frames = new vector<vector<uint8_t>>;
    d->frames->reserve(d->vi.numFrames);

    vector<uint8_t> canvas;
    canvas.resize(gif->SWidth * gif->SHeight,
                  static_cast<uint8_t>(gif->SBackGroundColor));

    for (int frameNum = 0; frameNum < gif->ImageCount; frameNum++) {
        SavedImage *sp = gif->SavedImages + frameNum;

        int disposal = DISPOSAL_UNSPECIFIED;
        int transparentIndex = -1;

        for (int i = 0; i < sp->ExtensionBlockCount; i++) {
            if (sp->ExtensionBlocks[i].Function == GRAPHICS_EXT_FUNC_CODE) {
                GraphicsControlBlock gcb;
                if (DGifExtensionToGCB(sp->ExtensionBlocks[i].ByteCount,
                                       sp->ExtensionBlocks[i].Bytes,
                                       &gcb) == GIF_OK) {
                    disposal = gcb.DisposalMode;
                    transparentIndex = gcb.TransparentColor;
                    delaySum += gcb.DelayTime;
                    delayCount++;
                    break;
                }
            }
        }

        int padding = gif->SWidth - sp->ImageDesc.Width - sp->ImageDesc.Left;
        uint8_t *bits = sp->RasterBits;
        auto it = canvas.begin();
        for (int y = 0; y < sp->ImageDesc.Height; y++) {
            it += sp->ImageDesc.Left;
            for (int x = 0; x < sp->ImageDesc.Width; x++) {
                uint8_t ind = *bits++;
                if (ind == transparentIndex) {
                    switch (disposal) {
                        case DISPOSE_BACKGROUND:
                            *it++ = gif->SBackGroundColor;
                            break;
                        case DISPOSE_PREVIOUS:
                            if (frameNum >= 2)
                                *it++ = (*d->frames)[frameNum - 2]
                                                    [(sp->ImageDesc.Top + y) *
                                                         gif->SWidth +
                                                     sp->ImageDesc.Left + x];
                            break;
                        default:
                            it++;
                            break;
                    }
                } else {
                    *it++ = ind;
                }
            }
            it += padding;
        }
        d->frames->push_back(canvas);
    }
    return d;
}

static void VS_CC unditherCreate(const VSMap *in, VSMap *out, void *userData,
                                 VSCore *core, const VSAPI *vsapi) {
    int err = D_GIF_SUCCEEDED;

    GifFileType *gif =
        DGifOpenFileName(vsapi->propGetData(in, "path", 0, 0), &err);

    if (gif == nullptr) {
        throwError(out, vsapi, err);
        return;
    }

    if (DGifSlurp(gif) == GIF_ERROR) {
        throwError(out, vsapi, gif->Error);
        DGifCloseFile(gif, &err);
        return;
    }

    Palette global;
    // load global color map into palette
    if (gif->SColorMap != nullptr) {
        global.reserve(gif->SColorMap->ColorCount);
        for (int i = 0; i < gif->SColorMap->ColorCount; i++)
            global.emplace_back(gif->SColorMap->Colors[i]);
    }

    bool allGlobal = true;
    VSFilterInit init;
    VSFilterFree freeFunc;
    VSFilterGetFrame getFrame;
    for (int frameNum = 0; frameNum < gif->ImageCount; frameNum++) {
        SavedImage *sp = gif->SavedImages + frameNum;
        if (sp->ImageDesc.ColorMap != nullptr) {
            allGlobal = false;
            freeFunc = localFree;
            getFrame = unditherGetFrame<LocalAcc, LocalData>;
            init = unditherInit<LocalData>;
            break;
        }
    }
    if (allGlobal) {
        freeFunc = globalFree;
        getFrame = unditherGetFrame<GlobalAcc, GlobalData>;
        init = unditherInit<GlobalData>;
    }

    int64_t delaySum = 0;
    int delayCount = 0;

    UnditherData *d;
    if (allGlobal)
        d = createGlobal(global, gif, delaySum, delayCount);
    else
        d = createLocal(global, gif, delaySum, delayCount);

    d->vi.format = vsapi->getFormatPreset(pfRGB24, core);
    d->width = d->vi.width = gif->SWidth;
    d->height = d->vi.height = gif->SHeight;
    d->vi.numFrames = gif->ImageCount;

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

    vsapi->createFilter(in, out, "Undither", init, getFrame, freeFunc,
                        fmParallel, 0, d, core);
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
