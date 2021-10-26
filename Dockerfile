FROM debian:bullseye-slim
ENV LANG C.UTF-8
RUN apt update
RUN apt-get install -y --no-install-recommends clang ca-certificates bison
RUN apt-get install -y --no-install-recommends autoconf autogen automake
RUN apt-get install -y --no-install-recommends pkg-config libgcrypt20-dev libpcre3-dev
RUN apt-get install -y --no-install-recommends make libjson-c-dev

COPY . /ldmud
WORKDIR /ldmud/src
RUN ./autogen.sh
RUN ./configure --enable-compat-mode --enable-use-json
RUN make install-driver
FROM debian:bullseye-slim
COPY --from=0 /usr/local/mud /usr/local/mud
RUN apt update
RUN apt-get install -y --no-install-recommends libjson-c-dev

EXPOSE 6039
ENV LANG C.UTF-8
VOLUME /gribmud
CMD ["/usr/local/mud/bin/ldmud", "--mudlib", "/gribmud", "6039"]
