#include <vapoursynth/VapourSynth.h>

#include <algorithm>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <iterator>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "gifdec.h"

using namespace std;

struct prgb {
    prgb(uint16_t r1, uint16_t g1, uint16_t b1) : r(r1), g(g1), b(b1) {}

    prgb(uint8_t *p) {
        r = *p++ * 255;
        g = *p++ * 255;
        b = *p * 255;
    }

    bool operator==(const prgb &p2) {
        return ((r == p2.r) && (g == p2.g) && (b == p2.b));
    }

    uint16_t r, g, b;
};

struct UnditherData {
    VSVideoInfo vi;
    int height, width, pixels;
    vector<prgb> *palette;
    vector<uint32_t *> *frames;
};

class Acc {
   public:
    Acc(vector<prgb> *pal, uint32_t *frame, int pixels) : palette(pal) {
        for (int i = 0; i < pixels; i++) {
            localpal.insert(frame[i]);
        }
    }

    void add_center(uint32_t cent) {
        center = cent;
        cpal = (*palette)[center];
        r = cpal.r * center_w;
        g = cpal.g * center_w;
        b = cpal.b * center_w;
        a = 255 * center_w;
    }

    void add(const uint32_t idx, uint8_t w) {
        uint8_t sim = similarity(idx);
        if (sim) {
            w *= sim;
            w *= 255;
            prgb c = (*palette)[idx];
            r += c.r * w;
            g += c.g * w;
            b += c.b * w;
            a += 255 * w;
        }
    }

    uint32_t r, g, b, a;

   private:
    unordered_set<uint32_t> localpal;
    unordered_map<uint64_t, uint8_t> simcache;
    const vector<prgb> *palette;
    static const int center_w = 6 * 255;
    uint32_t center;
    prgb cpal = {0, 0, 0};

    inline uint8_t similarity(const uint32_t c2) {
        const uint64_t pos = center < c2
                                 ? (static_cast<uint64_t>(center) << 32) | c2
                                 : (static_cast<uint64_t>(c2) << 32) | center;
        auto search = simcache.find(pos);
        if (search != simcache.end()) return search->second;

        const prgb p2 = (*palette)[c2];
        const prgb avg((cpal.r + p2.r) / 2, (cpal.g + p2.g) / 2,
                       (cpal.b + p2.b) / 2);

        const unsigned int allowed_diff = pal_diff(avg, cpal);
        unsigned int min_diff = 1 << 31;

        for (const auto &i : localpal) {
            if (i == center || i == c2) continue;
            unsigned int diff = pal_diff(avg, (*palette)[i]);
            if (diff < min_diff) min_diff = diff;
        }

        if (min_diff >= allowed_diff * 2) {
            simcache[pos] = 7;
            return 7;
        }
        if (min_diff >= allowed_diff) {
            simcache[pos] = 6;
            return 6;
        }
        if (min_diff * 2 >= allowed_diff) {
            simcache[pos] = 3;
            return 3;
        }
        simcache[pos] = 0;
        return 0;
    }

    inline uint32_t pal_diff(prgb p1, prgb p2) {
        return (p1.r - p2.r) * (p1.r - p2.r) + (p1.g - p2.g) * (p1.g - p2.g) +
               (p1.b - p2.b) * (p1.b - p2.b);
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

    uint32_t *frame = (*d->frames)[n];

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

            if (acc.a) {
                rp[ind2] = acc.r / acc.a;
                gp[ind2] = acc.g / acc.a;
                bp[ind2] = acc.b / acc.a;
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

static void VS_CC unditherCreate(const VSMap *in, VSMap *out, void *userData,
                                 VSCore *core, const VSAPI *vsapi) {
    unique_ptr<UnditherData> d(new UnditherData);

    gd_GIF *gif = gd_open_gif(vsapi->propGetData(in, "path", 0, 0));

    if (gif == nullptr) {
        vsapi->setError(out, "Undither: failed to open GIF file");
        return;
    }

    d->vi.format = vsapi->getFormatPreset(pfRGB24, core);
    d->vi.numFrames = 0;
    d->width = d->vi.width = gif->width;
    d->height = d->vi.height = gif->height;
    d->pixels = d->width * d->height;

    int64_t delay_sum = 0;

    d->palette = new vector<prgb>;
    d->frames = new vector<uint32_t *>;

    uint32_t *buf = new uint32_t[d->pixels]();
    if (gif->bgindex) {
        for (int i = 0; i < d->pixels; i++) buf[i] = gif->bgindex;
    }
    while (gd_get_frame(gif)) {
        if (d->vi.numFrames == 0) {
            for (int i = 0; i < gif->palette->size; i++) {
                d->palette->emplace_back(&gif->palette->colors[i * 3]);
            }
            for (int y = 0; y < gif->fh; y++) {
                for (int x = 0; x < gif->fw; x++) {
                    int ind =
                        gif->frame[(gif->fy + y) * gif->width + gif->fx + x];
                    if (ind != gif->gce.tindex)
                        buf[(gif->fy + y) * gif->width + gif->fx + x] = ind;
                }
            }
        } else {
            uint32_t remap[gif->palette->size];
            auto it = d->palette->begin();
            for (int i = 0; i < gif->palette->size; i++) {
                if (i == gif->gce.tindex) continue;

                prgb cmp(gif->palette->colors + (i * 3));
                if (cmp == *it++)
                    remap[i] = i;
                else {
                    auto it2 =
                        find(d->palette->begin(), d->palette->end(), cmp);
                    if (it2 == d->palette->end()) {
                        d->palette->push_back(cmp);
                        remap[i] = d->palette->size() - 1;
                    } else
                        remap[i] = distance(d->palette->begin(), it2);
                }
            }

            for (int y = 0; y < gif->fh; y++) {
                for (int x = 0; x < gif->fw; x++) {
                    uint8_t ind =
                        gif->frame[(gif->fy + y) * gif->width + gif->fx + x];
                    if (ind != gif->gce.tindex)
                        buf[(gif->fy + y) * gif->width + gif->fx + x] =
                            remap[ind];
                }
            }
        }
        uint32_t *buf2 = new uint32_t[d->pixels];
        memcpy(buf2, buf, (d->pixels) * sizeof(uint32_t));
        d->frames->push_back(buf2);

        delay_sum += gif->gce.delay;
        d->vi.numFrames++;
    }
    delete[] buf;
    if (delay_sum == 0) {
        cerr << "All frame durations are 0, defaulting to 10FPS" << endl;
        d->vi.fpsNum = 10;
        d->vi.fpsDen = 1;
    } else {
        double average = (delay_sum / static_cast<double>(d->vi.numFrames));
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
            delay_sum /= d->vi.numFrames;
            int64_t divisor = gcd(delay_sum);
            d->vi.fpsNum = 100 / divisor;
            d->vi.fpsDen = delay_sum / divisor;
        }
    }

    gd_close_gif(gif);

    vsapi->createFilter(in, out, "Undither", unditherInit, unditherGetFrame,
                        unditherFree, fmParallel, 0, d.get(), core);
    d.release();
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
