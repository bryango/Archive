FROM xucheng/texlive-full:latest

COPY \
  compile.sh \
  /root/

RUN apk --no-cache add bash findutils 

ENTRYPOINT /root/compile.sh $PWD
