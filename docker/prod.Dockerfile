FROM ghcr.io/lucivia/obs-base:2929201810

COPY . /work
WORKDIR /work

RUN stack build \
    --copy-bins \
    --local-bin-path=/bin/

FROM ghcr.io/lucivia/obs-runtime:2929229145

# appuser
RUN groupadd -g 999 appuser && \
    useradd -r -u 999 -g appuser appuser

COPY --chown=appuser:appuser --from=0 /bin/obsetl-exe /bin/obsetl-exe

# mount point of shared volume
RUN mkdir /shared
RUN chown appuser:appuser /shared

USER appuser
CMD ["/bin/obsetl-exe", "--port", "5003", "--data", "diamonds", "--mount", "shared"]
