#include <gif_lib.h>
#include <vapoursynth/VapourSynth.h>

#include <algorithm>
#include <array>
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

class Palette {
   public:
    Palette(GifColorType *glb, size_t size) {
        if (size == 0)
            allGlobal = false;
        else {
            for (size_t i = 0; i < size; i++) {
                global[i] = i;
                universal.emplace_back(glb[i]);
            }
        }
    }

    ~Palette() {
        for (auto pal : palettes) {
            if (pal != &global) delete pal;
        }
    }

    void addGlobal() { palettes.push_back(&global); }

    void addNull() {
        palettes.push_back(nullptr);
        allGlobal = false;
    }

    void add(IndexInt *pal, size_t size) {
        array<IndexInt, 256> *local = palettes.emplace_back();
        for (size_t i = 0; i < size; i++) (*local)[i] = pal[i];
        allGlobal = false;
    }

    int addToUniversal(const rgb &cmp) {
        auto it = find(universal.cbegin(), universal.cend(), cmp);
        if (it == universal.cend()) {
            universal.push_back(cmp);
            return universal.size() - 1;
        }
        return distance(universal.cbegin(), it);
    }

    size_t size() const { return universal.size(); }

    void setCurrent(int frameNumber) { current = palettes[frameNumber]; }

    const rgb &operator[](IndexInt pos) const { return universal[pos]; }

    bool allGlobal = true;
    array<IndexInt, 256> *current = nullptr;

   private:
    vector<array<IndexInt, 256> *> palettes;
    vector<rgb> universal;
    array<IndexInt, 256> global;
};

class Acc {
   public:
    Acc(const Palette *pal, IndexInt *frame, int pixels) : palette(pal) {
        if (pal->current == nullptr)
            for (int i = 0; i < pixels; i++) localpal.insert(frame[i]);
        else
            for (const auto &i : *(pal->current)) localpal.insert(i);
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
    const Palette *palette;
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

struct UnditherData {
    VSVideoInfo vi;
    int height, width, pixels;
    Palette *palette = nullptr;
    vector<IndexInt *> *frames;
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

    d->palette->setCurrent(n);

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

    d->frames = new vector<IndexInt *>;

    IndexInt canvas[d->pixels];
    fill(canvas, canvas + d->pixels, gif->SBackGroundColor);

    IndexInt remap[256];

    // load global color map into palette
    if (gif->SColorMap != nullptr) {
        d->palette =
            new Palette(gif->SColorMap->Colors, gif->SColorMap->ColorCount);
    } else
        d->palette = new Palette(nullptr, 0);

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

        bool global;
        if (sp->ImageDesc.ColorMap != nullptr) {
            for (int j = 0; j < sp->ImageDesc.ColorMap->ColorCount; j++) {
                if (j == transparentIndex) continue;

                const rgb cmp(sp->ImageDesc.ColorMap->Colors[j]);
                remap[j] = d->palette->addToUniversal(cmp);
            }
            global = false;
        } else
            global = true;

        if (d->palette->size() > USHRT_MAX) {
            vsapi->setError(out,
                            "Undither: combined palette size is larger than "
                            "uint16_t, recompile with larger IndexInt.");
            DGifCloseFile(gif, &err);
            return;
        }

        bool hasForeign = ((sp->ImageDesc.Height != d->height ||
                            sp->ImageDesc.Width != d->width) &&
                           !d->palette->allGlobal);
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
                            if (i >= 2) {
                                IndexInt ind =
                                    (*d->frames)[i - 2]
                                                [pos + sp->ImageDesc.Left + x];
                                canvas[pos + sp->ImageDesc.Left + x] = ind;
                                if (!hasForeign && !d->palette->allGlobal) {
                                    int j;
                                    for (j = 0;
                                         j < sp->ImageDesc.ColorMap->ColorCount;
                                         j++) {
                                        if (global) {
                                            if (j == ind) break;
                                        } else if (remap[j] == ind) {
                                            break;
                                        }
                                    }
                                    if (j == sp->ImageDesc.ColorMap->ColorCount)
                                        hasForeign = true;
                                }
                            }
                            break;
                        default:
                            if (!hasForeign && !d->palette->allGlobal) {
                                IndexInt ind =
                                    canvas[pos + sp->ImageDesc.Left + x];
                                int j;
                                for (j = 0;
                                     j < sp->ImageDesc.ColorMap->ColorCount;
                                     j++) {
                                    if (global) {
                                        if (j == ind) break;
                                    } else if (remap[j] == ind)
                                        break;
                                }
                                if (j == sp->ImageDesc.ColorMap->ColorCount)
                                    hasForeign = true;
                            }
                            break;
                    }
                } else {
                    canvas[pos + sp->ImageDesc.Left + x] =
                        global ? remap[ind] : ind;
                }
            }
        }
        if (d->palette->allGlobal)
            d->palette->addGlobal();
        else if (hasForeign)
            d->palette->addNull();
        else
            d->palette->add(remap, sp->ImageDesc.ColorMap->ColorCount);

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
