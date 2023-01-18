# Loosely based on https://www.fpcomplete.com/blog/2017/12/building-haskell-apps-with-docker
FROM fpco/stack-build:lts-20.7 as dependencies
RUN mkdir /opt/build
WORKDIR /opt/build

# GHC dynamically links its compilation targets to lib gmp
RUN apt-get update \
  && apt-get download libgmp10 \
  && apt-get install -y libcurl4-gnutls-dev \
  && apt-get install -y libpq-dev

RUN mv libgmp*.deb libgmp.deb

# Docker build should not use cached layer if any of these is modified
COPY stack.yaml package.yaml stack.yaml.lock /opt/build/
RUN stack build --system-ghc --dependencies-only

# -------------------------------------------------------------------------------------------
FROM fpco/stack-build:lts-20.7 as build

RUN apt-get update \
  && apt-get download libgmp10 \
  && apt-get install -y libcurl4-gnutls-dev \
  && apt-get install -y libpq-dev

# Copy compiled dependencies from previous stage
COPY --from=dependencies /root/.stack /root/.stack
COPY . /opt/build/

WORKDIR /opt/build

RUN stack build --system-ghc

RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

# -------------------------------------------------------------------------------------------
# Base image for stack build so compiled artifact from previous
# stage should run
FROM ubuntu:22.04 as app
ENV COMMAND none
ENV CONNECTION_STRING none
ENV GDIRECTIONS_API_KEY none

RUN apt-get update \
  && apt-get install -y libcurl4-gnutls-dev \
  && apt-get install -y libpq-dev

RUN mkdir -p /opt/app
WORKDIR /opt/app

# Install lib gmp
COPY --from=dependencies /opt/build/libgmp.deb /tmp
RUN dpkg -i /tmp/libgmp.deb && rm /tmp/libgmp.deb

COPY --from=build /opt/build/bin .
CMD ["sh", "-c", "/opt/app/estate-crawler-exe $COMMAND"]
