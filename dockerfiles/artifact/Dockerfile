FROM ubuntu:24.04 AS build

RUN apt-get update && apt-get install -y build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev gnupg2 git

# install ghcup
RUN echo ${ARCH}
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh
RUN cp ~/.ghcup/bin/ghcup /usr/local/bin/ghcup

ARG GHC=9.10.3
RUN ghcup install ghc   --isolate /usr/local --force ${GHC}
ARG CABAL=3.14.2.0
RUN ghcup install cabal --isolate /usr/local/bin --force ${CABAL}

RUN mkdir -p /workspace
ARG PURE_BORROW_COMMIT=75a96849ad97b69bf71ea4f571bdee77b16c1091
RUN git clone https://github.com/SoftwareFoundationGroupAtKyotoU/pure-borrow.git /workspace/pure-borrow && \
  cd /workspace/pure-borrow && \
  git checkout ${PURE_BORROW_COMMIT}

WORKDIR /workspace/pure-borrow
ENV PATH="/usr/local/bin:${PATH}"

ARG CABAL_INDEX_STATE=2026-02-16T11:32:42Z
RUN cabal update --index-state=${CABAL_INDEX_STATE}
ARG NUM_CPUS=8
RUN cabal configure --enable-static --enable-executable-static --enable-tests --enable-benchmarks --disable-shared --semaphore -j${NUM_CPUS}
RUN cabal build all --only-dependencies
RUN cabal build all
RUN mkdir -p /opt/pure-borrow
RUN for bin in pure-borrow-test qsort-bench convert-qsort-bench-csv qsort ; do \
      cp "$(cabal list-bin $bin)" /opt/pure-borrow/ ; \
    done

FROM debian:trixie-slim

COPY --from=build /opt/pure-borrow/ /opt/pure-borrow
COPY run.sh /opt/pure-borrow/run.sh
RUN chmod +x /opt/pure-borrow/run.sh
RUN mkdir -p /workspace

ENV PATH="/opt/pure-borrow:${PATH}"
WORKDIR /workspace

ENTRYPOINT ["/opt/pure-borrow/run.sh"]
