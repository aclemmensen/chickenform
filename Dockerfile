FROM theodesp/chicken-scheme-alpine
RUN chicken-install medea http-client
RUN chicken-install typed-records
RUN chicken-install alist-lib
