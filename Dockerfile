FROM ubuntu:bionic

RUN apt-get update && apt-get install -y \
    curl \
    emacs \
    git \
    make \
    python \
    && rm -rf /var/lib/apt/lists/*

RUN cd /tmp \
    && curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python \
    && rm -rf /tmp/*

ENV PATH="/root/.cask/bin:${PATH}"
