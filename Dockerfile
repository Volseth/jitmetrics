FROM r-base:4.1.2
COPY OurExperiments /usr/local/src/scripts/OurExperiments

RUN apt-get update
RUN apt-get install -y build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev
RUN apt-get install -y cmake

WORKDIR /usr/local/src/scripts

ADD packages.tar /usr/local/lib/

COPY entrypoint.sh .
RUN chmod +x entrypoint.sh
CMD ["./entrypoint.sh"]