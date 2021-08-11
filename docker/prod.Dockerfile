FROM ghcr.io/lucivia/obs-base:2021-08-11

COPY . /work
WORKDIR /work

RUN stack build \
    --copy-bins \
    --local-bin-path=/bin/

FROM ghcr.io/lucivia/obs-runtime:2021-08-11

COPY --from=0 /bin/obsetl-exe /bin/obsetl-exe

CMD ["/bin/obsetl-exe"]
