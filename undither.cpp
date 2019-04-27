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

#include <vapoursynth/VapourSynth.h>

#include "gifdec.h"

using namespace std;

struct rgb {
    uint8_t r, g, b;
};
struct rgb_sum {
    uint32_t r, g, b, a;
};
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
/*
struct Frame {

    uint16_t fx, fy, fw, fh;
    uint32_t *indices;
};*/

struct UnditherData {
    VSVideoInfo vi;
    int height, width;
    vector<prgb> *palette;
    vector<uint32_t *> *frames;
};

inline static unsigned int pal_diff(prgb p1, prgb p2) {
    return (p1.r - p2.r) * (p1.r - p2.r) + (p1.g - p2.g) * (p1.g - p2.g) +
           (p1.b - p2.b) * (p1.b - p2.b);
}

inline static uint8_t similarity(const uint32_t c1, const uint32_t c2,
                                 const vector<prgb> *palette,
                                 unordered_set<uint32_t> &localpal,
                                 unordered_map<uint64_t, uint8_t> &simcache) {
    const uint64_t pos = c1 < c2 ? (static_cast<uint64_t>(c1) << 32) | c2
                                 : (static_cast<uint64_t>(c2) << 32) | c1;
    auto search = simcache.find(pos);
    if (search != simcache.end()) return search->second;

    const prgb p1 = (*palette)[c1];
    const prgb p2 = (*palette)[c2];
    const prgb avg((p1.r + p2.r) / 2, (p1.g + p2.g) / 2, (p1.b + p2.b) / 2);

    // cout << avg.r << " " << avg.g << " " << avg.b << endl;

    const unsigned int allowed_diff = pal_diff(avg, p1);
    unsigned int min_diff = 1 << 31;

    // cout << c1 << " " << c2 << " " << endl;

    for (const auto &i : localpal) {
        if (i == c1 || i == c2) continue;
        unsigned int diff = pal_diff(avg, (*palette)[i]);
        if (diff < min_diff) min_diff = diff;
    }

    if (min_diff >= allowed_diff * 2) {
        simcache[pos] = 8;
        return 8;
    }
    if (min_diff >= allowed_diff) {
        simcache[pos] = 6;
        return 6;
    }
    if (min_diff * 2 >= allowed_diff) {
        simcache[pos] = 2;
        return 2;
    }
    simcache[pos] = 0;
    return 0;
}

inline static void add_to_acc(rgb_sum *acc, const uint32_t center,
                              const uint32_t idx, const vector<prgb> *palette,
                              unordered_set<uint32_t> &localpal,
                              unordered_map<uint64_t, uint8_t> &simcache,
                              uint8_t w) {
    uint8_t sim = similarity(center, idx, palette, localpal, simcache);
    if (sim) {
        w *= sim;
        w *= 255;
        prgb c = (*palette)[idx];
        acc->r += c.r * w;
        acc->g += c.g * w;
        acc->b += c.b * w;
        acc->a += 255 * w;
    }
}

static void VS_CC unditherInit(VSMap *in, VSMap *out, void **instanceData,
                               VSNode *node, VSCore *core, const VSAPI *vsapi) {
    UnditherData *d = (UnditherData *)*instanceData;
    vsapi->setVideoInfo(&d->vi, 1, node);
}

static const VSFrameRef *VS_CC unditherGetFrame(
    int n, int activationReason, void **instanceData, void **frameData,
    VSFrameContext *frameCtx, VSCore *core, const VSAPI *vsapi) {
    // if (activationReason != arInitial) return nullptr;

    UnditherData *d = (UnditherData *)*instanceData;
    unordered_map<uint64_t, uint8_t> simcache;
    unordered_set<uint32_t> localpal;

    VSFrameRef *dst =
        vsapi->newVideoFrame(d->vi.format, d->width, d->height, 0, core);
    int stride = vsapi->getStride(dst, 0);
    uint8_t *rp = vsapi->getWritePtr(dst, 0);
    uint8_t *gp = vsapi->getWritePtr(dst, 1);
    uint8_t *bp = vsapi->getWritePtr(dst, 2);

    uint32_t *frame = (*d->frames)[n];

    for (int i = 0; i < d->width * d->height; i++) {
        localpal.insert(frame[i]);
    }

    static const int center_w = 8 * 255;

    for (int y = 0; y < d->height; y++) {
        for (int x = 0; x < d->width; x++) {
            int ind = (d->width * y) + x;
            int ind2 = (stride * y) + x;

            const uint32_t center = frame[ind];
            const prgb cpal = (*d->palette)[center];
            // cout << cpal.r << " " << cpal.g << " " << cpal.b << endl;
            rgb_sum acc = {.r = static_cast<uint32_t>(cpal.r * center_w),
                           .g = static_cast<uint32_t>(cpal.g * center_w),
                           .b = static_cast<uint32_t>(cpal.b * center_w),
                           .a = 255 * center_w};

            if (y > 0) {
                if (x > 0)
                    add_to_acc(&acc, center, frame[ind - d->width - 1],
                               d->palette, localpal, simcache, 1);
                add_to_acc(&acc, center, frame[ind - d->width], d->palette,
                           localpal, simcache, 2);
                if (x < d->width - 1)
                    add_to_acc(&acc, center, frame[ind - d->width + 1],
                               d->palette, localpal, simcache, 1);
            }

            if (x > 0)
                add_to_acc(&acc, center, frame[ind - 1], d->palette, localpal,
                           simcache, 2);
            if (x < d->width - 1)
                add_to_acc(&acc, center, frame[ind + 1], d->palette, localpal,
                           simcache, 3);  // floyd-steinberg

            if (y < d->height - 1) {
                if (x > 0)
                    add_to_acc(&acc, center, frame[ind + d->width - 1],
                               d->palette, localpal, simcache,
                               2);  // floyd-steinberg
                add_to_acc(&acc, center, frame[ind + d->width], d->palette,
                           localpal, simcache, 2);
                if (x < d->width - 1)
                    add_to_acc(&acc, center, frame[ind + d->width + 1],
                               d->palette, localpal, simcache, 1);
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

    free(d);
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
    UnditherData *d = (UnditherData *)calloc(sizeof(UnditherData), 1);

    gd_GIF *gif = gd_open_gif(vsapi->propGetData(in, "path", 0, 0));
    d->vi.format = vsapi->getFormatPreset(pfRGB24, core);
    d->width = d->vi.width = gif->width;
    d->height = d->vi.height = gif->height;

    int64_t delay_sum = 0;

    d->palette = new vector<prgb>;
    d->frames = new vector<uint32_t *>;

    uint32_t *buf = new uint32_t[d->width * d->height];
    if (gif->bgindex) memset(buf, gif->bgindex, d->width * d->height);

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
                    } else {
                        remap[i] = distance(d->palette->begin(), it2);
                    }
                }
            }
            uint8_t ind;
            for (int y = 0; y < gif->fh; y++) {
                for (int x = 0; x < gif->fw; x++) {
                    ind = gif->frame[(gif->fy + y) * gif->width + gif->fx + x];
                    if (ind != gif->gce.tindex)
                        buf[(gif->fy + y) * gif->width + gif->fx + x] =
                            remap[ind];
                }
            }
        }
        uint32_t *buf2 = new uint32_t[d->width * d->height];
        memcpy(buf2, buf, (d->width * d->height) * sizeof(uint32_t));
        d->frames->push_back(buf2);

        delay_sum += gif->gce.delay;
        d->vi.numFrames++;
    }
    delay_sum = (delay_sum / d->vi.numFrames) + 0.5;
    int64_t divisor = gcd(delay_sum);
    d->vi.fpsNum = 100 / divisor;
    d->vi.fpsDen = delay_sum / divisor;

    gd_close_gif(gif);

    vsapi->createFilter(in, out, "Undither", unditherInit, unditherGetFrame,
                        unditherFree, fmParallel, 0, d, core);
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
