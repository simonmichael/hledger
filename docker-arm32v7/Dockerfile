FROM arm32v7/ubuntu:20.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get -qq update && apt-get install -y --no-install-recommends tzdata && apt-get install -y gcc g++ make xz-utils wget vim libtinfo5 git python3 libgmp-dev libtinfo-dev zlib1g-dev locales && rm -rf /var/lib/apt/lists/*

RUN locale-gen en_US.UTF-8 && update-locale LC_ALL=en_US.UTF-8 && update-locale LANG=en_US.UTF-8 && update-locale LANGUAGE=en_US.UTF-8

ENV LC_ALL=en_US.UTF-8
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US.UTF-8

# Download and install LLVM 9.0, GHC 8.10.4, cabal 3.0, and newer libffi (for cabal)
RUN wget --no-check-certificate "https://github.com/llvm/llvm-project/releases/download/llvmorg-9.0.1/clang+llvm-9.0.1-armv7a-linux-gnueabihf.tar.xz" \
        && wget --no-check-certificate "https://downloads.haskell.org/~ghc/8.10.4/ghc-8.10.4-armv7-deb10-linux.tar.xz" \
        && wget --no-check-certificate "http://ports.ubuntu.com/ubuntu-ports/pool/universe/libf/libffi/libffi8ubuntu1-udeb_3.4~20200819gead65ca871-0ubuntu3_armhf.udeb" \
        && wget --no-check-certificate "http://ports.ubuntu.com/ubuntu-ports/pool/universe/h/haskell-cabal-install/cabal-install_3.0.0.0-3build1_armhf.deb" \
        && echo "91638613913537f8c0c84a59f59f00fafeafb6b189164cae471d4e3bccaf1df3  clang+llvm-9.0.1-armv7a-linux-gnueabihf.tar.xz" | sha256sum -c \
        && echo "0d18ef83593272f6196a41cc3abdc48dfe5e14372db75d71ea19fe35320c4e81  ghc-8.10.4-armv7-deb10-linux.tar.xz" | sha256sum -c \
        && echo "d9c2ab89b31b156e83320431922a8cc9302f0e4ddd89900028362d18a938cf00  libffi8ubuntu1-udeb_3.4~20200819gead65ca871-0ubuntu3_armhf.udeb" | sha256sum -c \
        && echo "3afd883584ea01d8253b0273a3704c60c4f704ae7d7ac92dbb709ef951f8b2d6  cabal-install_3.0.0.0-3build1_armhf.deb" | sha256sum -c \
        && tar xvf clang+llvm-9.0.1-armv7a-linux-gnueabihf.tar.xz \
        && tar xvf ghc-8.10.4-armv7-deb10-linux.tar.xz \
        && cd clang+llvm-9.0.1-armv7a-linux-gnueabihf && mv bin/* /usr/local/bin/ && mv include/* /usr/local/include/ && mv lib/* /usr/local/lib/ && cd .. && rm clang+llvm-9.0.1-armv7a-linux-gnueabihf.tar.xz \
        && cd ghc-8.10.4 && ./configure && make install && cd .. && rm -r ghc-8.10.4 && rm ghc-8.10.4-armv7-deb10-linux.tar.xz \
        && dpkg -i --force-all libffi8ubuntu1-udeb_3.4~20200819gead65ca871-0ubuntu3_armhf.udeb cabal-install_3.0.0.0-3build1_armhf.deb \
        && rm libffi8ubuntu1-udeb_3.4~20200819gead65ca871-0ubuntu3_armhf.udeb cabal-install_3.0.0.0-3build1_armhf.deb

# Run cabal update
RUN cabal update

# Get access to hledger source to build
COPY . /hledger

# Build static hledger binary
RUN cd /hledger && cabal build --enable-executable-static --with-compiler=/usr/local/bin/ghc-8.10.4 hledger

# Strip symbols from binary
RUN cp /hledger/dist-newstyle/build/arm-linux/ghc-*/hledger-*/x/hledger/build/hledger/hledger /root/ && strip /root/hledger
