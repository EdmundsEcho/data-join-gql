# obsetl

Build the docker images

## src

    docker image build . -t ghcr.io/lucivia/obs-src:2021-08-11 -f docker/src.Dockerfile

## base

    docker image build docker -t ghcr.io/lucivia/obs-base:2021-08-11 -f docker/base.Dockerfile

## runtime

    docker image build docker -t ghcr.io/lucivia/obs-runtime:2021-08-11 -f docker/runtime.Dockerfile
