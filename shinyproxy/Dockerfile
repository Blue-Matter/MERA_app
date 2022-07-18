
FROM openanalytics/r-base

LABEL maintainer "Adrian Hordyk <adrian@bluematterscience.com>"

RUN apt-get update && apt-get install --no-install-recommends -y \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    && rm -rf /var/lib/apt/lists/*
	
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive \
    apt-get -y install default-jre-headless && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

COPY shinyproxy-2.6.1.jar /root/

EXPOSE 8080

WORKDIR /opt/shinyproxy/

ENTRYPOINT ["java","-jar","/root/shinyproxy-2.6.1.jar"]

