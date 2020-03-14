FROM xucheng/texlive-full:latest

COPY \
  compile.sh \
  /root/

ENTRYPOINT ["/root/compile.sh"]
