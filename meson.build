project('undither', 'cpp',
    default_options: ['buildtype=release', 'b_ndebug=if-release', 'b_lto=true', 'cpp_std=c++17'],
    meson_version: '>=0.46.0',
    version: '0.1')

shared_module('undither',
    sources: ['undither.cpp', 'gifdec.cpp'],
    dependencies: dependency('vapoursynth').partial_dependency(compile_args: true, includes: true),
    cpp_args: ['-march=native', '-Ofast'],
    install: true)
