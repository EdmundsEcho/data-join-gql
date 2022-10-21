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

# do not include in production
ENV AWS_ACCESS_KEY_ID="DO003ZZWAE34HMFMRCQ3"
ENV AWS_SECRET_ACCESS_KEY="Y+4Ld8Cu/PCOgtMUmWTFR1O00T4g5YpR15tAX177PJI"
ENV S3_REGION="US"
ENV S3_HOST_BASE="sfo3.digitaloceanspaces.com"
ENV S3_HOST_BUCKET="luci-space.sfo3.digitaloceanspaces.com"

CMD [ "/bin/obsetl-exe", \
      "--port", "5003", \
      "--data", "diamonds", \
      "--mount", "shared" ]
