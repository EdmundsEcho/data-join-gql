FROM ghcr.io/lucivia/obs-base:1135615579

COPY . /work
WORKDIR /work

RUN stack build \
    --copy-bins \
    --local-bin-path=/bin/

FROM ghcr.io/lucivia/obs-runtime:1135610655

COPY --from=0 /bin/obsetl-exe /bin/obsetl-exe

# mount point of shared volume
RUN mkdir /shared

CMD ["/bin/obsetl-exe", "--port", "5003", "--data", "diamonds", "--mount", "shared"]
