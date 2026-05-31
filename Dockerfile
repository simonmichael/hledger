# Docker build instructions for hledger developers:
#
# Development image (with full build chain, good for testing):
#   docker build --target dev -t hledger-dev .
#   docker run --rm -it hledger-dev hledger --version
#
# Minimal production image (smaller, runtime-only):
#   docker build -t hledger .
#   docker run --rm -v /path/to/journal:/data hledger hledger -f /data/hledger.journal bal
#
# The dev stage compiles hledger with stack; the final slim stage copies only
# the binaries and runtime deps (libtinfo6, libgmp10).  Ports 5000
# (hledger-web) and 5001 are exposed; /data is the default VOLUME.

FROM haskell:9.12.2 AS dev

RUN mkdir /root/hledger
WORKDIR /root/hledger

# Install GHC
COPY ./stack*.yaml ./
RUN stack setup

# Pre-cache dependencies
RUN mkdir hledger-lib hledger hledger-ui hledger-web
COPY hledger-lib/package.yaml hledger-lib/package.yaml
COPY hledger/package.yaml hledger/package.yaml
COPY hledger-ui/package.yaml hledger-ui/package.yaml
COPY hledger-web/package.yaml hledger-web/package.yaml
RUN stack install --dependencies-only

# Actually compile sources
COPY . .
RUN stack install

FROM debian:stable-slim

RUN apt-get update && apt-get -y install libtinfo6 libgmp10 && rm -rf /var/lib/apt/lists

COPY --from=dev /root/.local/bin/hledger* /usr/bin/

ENV LC_ALL=C.UTF-8

RUN mkdir /data && touch /data/hledger.journal
VOLUME /data

EXPOSE 5000 5001

COPY docker/start.sh /start.sh

CMD ["/start.sh"]
