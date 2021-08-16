FROM ghcr.io/lucivia/obs-base:latest

COPY . /work
WORKDIR /work

RUN stack build \
    --copy-bins \
    --local-bin-path=/bin/

FROM ghcr.io/lucivia/obs-runtime:latest

COPY --from=0 /bin/obsetl-exe /bin/obsetl-exe

CMD ["/bin/obsetl-exe"]
