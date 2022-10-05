# Aug 25, 2022: use the production
# This is out of date

FROM debian:9-slim@sha256:1a470b92197dd16a46f7aa9cb308fa91f7d0948e0dccd625a03cbbdf2d4516e6 as build

################################################################################
# Haskell system dependencies (basically never changes)

RUN apt-get update && \
    apt-get install -yq --no-install-suggests --no-install-recommends --force-yes -y -qq \
            netbase git ca-certificates xz-utils build-essential curl unzip libgmp-dev \
            libz-dev libicu-dev libtinfo-dev
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y locales
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    dpkg-reconfigure --frontend=noninteractive locales && \
    update-locale LANG=en_US.UTF-8
ENV LANG en_US.UTF-8

################################################################################
# Download a specific Stack version

RUN curl https://github.com/commercialhaskell/stack/releases/download/v2.7.3/stack-2.7.3-linux-x86_64-static.tar.gz \
    --silent -L \
    -o stack.tar.gz && \
    tar zxf stack.tar.gz && mv stack-2.7.3-linux-x86_64-static/stack /usr/bin/

################################################################################
# Switch to work dir

ADD stack.yaml /build-workdir/stack.yaml
ADD stack.yaml.lock /build-workdir/stack.yaml.lock
ADD package.yaml /build-workdir/package.yaml


WORKDIR /build-workdir

################################################################################
# Install the right GHC version and update package index

RUN pwd && stack setup && stack update

################################################################################
# Install the snapshot and system dependencies

RUN stack build --only-snapshot --test --no-run-tests
WORKDIR /
RUN rm -rf /build-workdir

COPY . /work
WORKDIR /work

RUN stack build \
    --copy-bins \
    --local-bin-path=/bin/

FROM fpco/pid1:18.04@sha256:f9deec3b086faa6a7a66f3a06a3dcd53315f29d688e36184f8e0d6cdd88985e4

RUN apt-get update && \
    apt-get install -yq --no-install-suggests --no-install-recommends --force-yes -y -qq \
            netbase ca-certificates libgmp-dev libz-dev libicu-dev libtinfo-dev

COPY --from=build /bin/obsetl-exe /bin/obsetl-exe

CMD: [ "/bin/obsetl-exe", "--port", "5003", "--data", "diamonds", "--mount", "shared" ]
