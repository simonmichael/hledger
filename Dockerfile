FROM debian:stable-slim as dev

RUN apt-get update
RUN apt-get install -y curl libtinfo-dev
RUN (curl -sSL https://get.haskellstack.org/ | sh)
RUN mkdir /root/hledger
WORKDIR /root/hledger

# Install GHC
COPY ./stack*.yaml ./
RUN stack setup

# Pre-cache dependencies
RUN mkdir hledger-lib hledger hledger-ui hledger-web hledger-api
COPY hledger-lib/package.yaml hledger-lib/package.yaml
COPY hledger/package.yaml hledger/package.yaml
COPY hledger-ui/package.yaml hledger-ui/package.yaml
COPY hledger-web/package.yaml hledger-web/package.yaml
COPY hledger-api/package.yaml hledger-api/package.yaml
RUN stack install --dependencies-only

# Actually compile sources
COPY . .
RUN stack install

FROM debian:stable-slim

RUN apt-get update && apt-get install libgmp10 && rm -rf /var/lib/apt/lists

COPY --from=dev /root/.local/bin/hledger* /usr/bin/

ENV LC_ALL C.UTF-8

RUN mkdir /data && touch /data/hledger.journal
VOLUME /data

EXPOSE 5000 5001

COPY docker/start.sh /start.sh

CMD ["/start.sh"]
