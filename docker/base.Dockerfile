FROM ghcr.io/lucivia/obs-src:2929174650
WORKDIR /build-workdir

################################################################################
# Install the right GHC version and update package index

RUN pwd && stack setup && stack update

################################################################################
# Install the snapshot and system dependencies

RUN stack build --only-snapshot --test --no-run-tests
WORKDIR /
RUN rm -rf /build-workdir
