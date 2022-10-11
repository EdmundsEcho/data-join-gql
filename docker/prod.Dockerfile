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

# only docker listens to exposed port
EXPOSE 5003

USER appuser
# CMD ["/bin/obsetl-exe", "--port", "5003", "--data", "diamonds", "--mount", "shared"]
CMD [ "/bin/obsetl-exe", \
      "--port", "5003", \
      "--data", "diamonds", \
      "--mount", "shared" , \
      "--shareuri", "https://luci-space.sfo3.digitaloceanspaces.com", \
      "--region", "sfo3", \
      "--secret", "Y+4Ld8Cu/PCOgtMUmWTFR1O00T4g5YpR15tAX177PJI", \
      "--accessid", "DO003ZZWAE34HMFMRCQ3" \
]
