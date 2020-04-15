FROM alpine:3.10.3

WORKDIR /app

RUN apk add --no-cache \
    clang \
    make \
    gcc \
    musl-dev
