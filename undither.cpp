#include <gif_lib.h>
#include <vapoursynth/VapourSynth.h>

#include <algorithm>
#include <array>
#include <climits>
#include <cstddef>
#include <iostream>
#include <list>
#include <unordered_map>
#include <utility>
#include <vector>

using namespace std;

struct Pixel;
struct RGB;
using Palette = vector<RGB>;

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
    Pixel(uint8_t ind, uint16_t palInd) : ind(ind), palInd(palInd) {}

    uint64_t hash() const { return static_cast<uint64_t>(palInd << 8) | ind; }

    bool operator==(const Pixel &p2) const {
        return (ind == p2.ind && palInd == p2.palInd);
    }

    bool operator>(const Pixel &p2) const { return ind > p2.ind; }

    uint8_t ind;
    uint16_t palInd;
};

class SimCache {
   public:
    SimCache() = default;

    void setSize(size_t s) {
        size = s;
        ref.reserve(s);
    }

    void add(const uint64_t hash, const uint8_t sim) {
        if (similarities.size() == size) {
            uint64_t last = similarities.back().first;
            similarities.pop_back();
            ref.erase(last);
        }
        similarities.push_front(pair<uint64_t, uint8_t>(hash, sim));
        ref[hash] = similarities.begin();
    }

    auto find(const uint64_t hash) {
        auto it = ref.find(hash);
        if (it != ref.end()) return it->second;
        return similarities.end();
    }

    auto end() { return similarities.end(); }

   private:
    list<pair<uint64_t, uint8_t>> similarities;
    unordered_map<uint64_t, list<pair<uint64_t, uint8_t>>::iterator> ref;
    size_t size;
};

class Acc {
   public:
    virtual ~Acc() = default;

    uint32_t r, g, b, weight;

   protected:
    static const int center_w = 8;
    RGB crgb;
    uint8_t cind;
};

class LocalAcc : public Acc {
   public:
    LocalAcc(vector<Palette> *palettes) : palettes(palettes) {}

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
        cPalInd = cent.palInd;
        cpal = (*palettes)[cPalInd];
        cind = cent.ind;
        crgb = cpal[cind];
        r = crgb.r * center_w;
        g = crgb.g * center_w;
        b = crgb.b * center_w;
        weight = center_w;
        cHash = cent.hash();
    }

    void setCacheSize(size_t s) { simcache.setSize(s); }

   private:
    inline uint32_t similarity(const Pixel &p2, RGB &c2) {
        uint8_t sim;
        uint64_t hash = cind > p2.ind ? (cHash << 32) | p2.hash()
                                      : (p2.hash() << 32) | cHash;
        auto cached = simcache.find(hash);
        if (cached != simcache.end()) {
            sim = cached->second;
            if (sim) c2 = (*palettes)[p2.palInd][p2.ind];
            return sim;
        }

        Palette pal2 = (*palettes)[p2.palInd];
        c2 = pal2[p2.ind];
        const RGB avg = crgb.avg(c2);

        const uint32_t allowed_diff = avg - crgb;
        uint32_t min_diff = UINT_MAX;

        if (cPalInd == p2.palInd) {
            for (int i = 0; i < cpal.size(); i++) {
                if (i == cind || i == p2.ind) continue;
                uint32_t diff = avg - cpal[i];
                if (diff < min_diff) min_diff = diff;
            }
        } else {
            for (int i = 0; i < cpal.size(); i++) {
                const RGB color = cpal[i];
                if (i == cind || color == c2) continue;
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

        simcache.add(hash, sim);
        return sim;
    }

    Palette cpal;
    uint16_t cPalInd;
    uint64_t cHash;
    vector<Palette> *palettes;
    SimCache simcache;
};

class GlobalAcc : public Acc {
   public:
    GlobalAcc(const Palette &pal) : palette(pal) {
        fill(simcache.begin(), simcache.end(), -1);
    }

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

    Palette palette;
    array<char, 256 * 256> simcache;
};

struct UnditherData {
    VSVideoInfo vi;
    int height, width;
};

struct GlobalData : UnditherData {
    vector<vector<uint8_t>> *frames;
    GlobalAcc *acc;
};

struct LocalData : UnditherData {
    vector<vector<Pixel>> *frames;
    vector<Palette> *palettes;
    LocalAcc *acc;
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
    A acc = d->acc;

    for (int y = 0; y < d->height; y++) {
        for (int x = 0; x < d->width; x++) {
            int ind = (d->width * y) + x;
            int ind2 = (stride * y) + x;

            acc->add_center(frame[ind]);

            if (y > 0) {
                if (x > 0) acc->add(frame[ind - d->width - 1], 1);
                acc->add(frame[ind - d->width], 2);
                if (x < d->width - 1) acc->add(frame[ind - d->width + 1], 1);
            }

            if (x > 0) acc->add(frame[ind - 1], 2);
            if (x < d->width - 1)
                acc->add(frame[ind + 1], 3);  // floyd-steinberg

            if (y < d->height - 1) {
                if (x > 0)
                    acc->add(frame[ind + d->width - 1], 2);  // floyd-steinberg
                acc->add(frame[ind + d->width], 2);
                if (x < d->width - 1) acc->add(frame[ind + d->width + 1], 1);
            }

            if (acc->weight) {
                rp[ind2] = acc->r / acc->weight;
                gp[ind2] = acc->g / acc->weight;
                bp[ind2] = acc->b / acc->weight;
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

    delete d->acc;
    delete d->frames;
    delete d;
}

static void VS_CC localFree(void *instanceData, VSCore *core,
                            const VSAPI *vsapi) {
    LocalData *d = (LocalData *)instanceData;

    delete d->acc;
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

    uint8_t maxGlobalInd = global.size() - 1;

    d->palettes = new vector<Palette>;
    d->palettes->push_back(move(global));
    d->acc = new LocalAcc(d->palettes);
    d->acc->setCacheSize((gif->SWidth + 2) * 8);

    uint8_t maxInd;
    uint16_t palInd;

    d->frames = new vector<vector<Pixel>>;
    d->frames->reserve(gif->ImageCount);

    vector<Pixel> canvas;
    canvas.resize(gif->SWidth * gif->SHeight, Pixel(gif->SBackGroundColor, 0));

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

        if (sp->ImageDesc.ColorMap != nullptr) {
            Palette palette;
            palette.reserve(sp->ImageDesc.ColorMap->ColorCount);
            for (int i = 0; i < sp->ImageDesc.ColorMap->ColorCount; i++)
                palette.emplace_back(sp->ImageDesc.ColorMap->Colors[i]);

            // remove redundant entries from end
            const RGB back = palette.back();
            palette.pop_back();
            while (palette.back() == back) palette.pop_back();
            palette.push_back(move(back));
            maxInd = palette.size() - 1;

            if (palette != d->palettes->back())
                d->palettes->push_back(move(palette));
            palInd = d->palettes->size() - 1;
        } else {
            palInd = 0;
            maxInd = maxGlobalInd;
        }

        int padding = gif->SWidth - sp->ImageDesc.Width - sp->ImageDesc.Left;
        uint8_t *bits = sp->RasterBits;
        auto it = canvas.begin();
        it += (gif->SWidth * sp->ImageDesc.Top);
        for (int y = 0; y < sp->ImageDesc.Height; y++) {
            it += sp->ImageDesc.Left;
            for (int x = 0; x < sp->ImageDesc.Width; x++) {
                uint8_t ind = *bits++;
                if (ind == transparentIndex) {
                    switch (disposal) {
                        case DISPOSE_BACKGROUND:
                            *it++ = Pixel(gif->SBackGroundColor <= maxInd
                                              ? gif->SBackGroundColor
                                              : maxInd,
                                          palInd);
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
                    *it++ = Pixel(ind <= maxInd ? ind : maxInd, palInd);
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
    d->acc = new GlobalAcc(global);
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

        uint8_t maxInd = global.size() - 1;
        uint8_t bgColor =
            gif->SBackGroundColor <= maxInd ? gif->SBackGroundColor : maxInd;

        int padding = gif->SWidth - sp->ImageDesc.Width - sp->ImageDesc.Left;
        uint8_t *bits = sp->RasterBits;
        auto it = canvas.begin();
        it += (gif->SWidth * sp->ImageDesc.Top);
        for (int y = 0; y < sp->ImageDesc.Height; y++) {
            it += sp->ImageDesc.Left;
            for (int x = 0; x < sp->ImageDesc.Width; x++) {
                uint8_t ind = *bits++;
                if (ind == transparentIndex) {
                    switch (disposal) {
                        case DISPOSE_BACKGROUND:
                            *it++ = bgColor;
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
                    *it++ = ind <= maxInd ? ind : maxInd;
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
        // remove redundant entries from end
        RGB back = global.back();
        global.pop_back();
        while (global.back() == back) {
            global.pop_back();
        }
        global.push_back(move(back));
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
            getFrame = unditherGetFrame<LocalAcc *, LocalData>;
            init = unditherInit<LocalData>;
            break;
        }
    }
    if (allGlobal) {
        freeFunc = globalFree;
        getFrame = unditherGetFrame<GlobalAcc *, GlobalData>;
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
