FROM ghcr.io/lucivia/obs-base:2929201810

COPY . /work
WORKDIR /work

RUN stack build \
    --copy-bins \
    --local-bin-path=/bin/

FROM ghcr.io/lucivia/obs-runtime:2929229145

COPY --from=0 /bin/obsetl-exe /bin/obsetl-exe

# mount point of shared volume
RUN mkdir /shared

CMD ["/bin/obsetl-exe", "--port", "5003", "--data", "diamonds", "--mount", "shared"]
