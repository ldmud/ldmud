FROM alpine:3.14
RUN apk add g++ clang make git zlib-dev openssl-dev autoconf
COPY . /ldmud
WORKDIR /ldmud/src
RUN ./autogen.sh
RUN ./configure --enable-compat-mode --enable-use-tls --disable-use-pcre
RUN make
RUN make install-all
FROM alpine:3.14
COPY --from=0 /usr/local/mud /usr/local/mud
CMD ["/usr/local/mud/bin/ldmud", "--mudlib", "/gribmud", "6039"]
