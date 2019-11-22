FROM ubuntu:bionic

RUN apt-get update && \
  apt-get install -y \
  make curl python git emacs

RUN curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
ENV PATH="/root/.cask/bin:${PATH}"

RUN mkdir -p /typescript-mode
COPY . /typescript-mode
WORKDIR /typescript-mode

CMD ["make", "test"]
