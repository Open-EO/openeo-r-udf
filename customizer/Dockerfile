FROM openeor/r-udf-base:0.2.1

ENV PLUMBER_PORT=5555

# copy RScripts from the current directory into the container
WORKDIR /opt/openeo-r-udf
COPY /*.R .
COPY /libraries.json .

# some R packages depend on some system libraries, make sure to install them
# RUN apt-get update && apt-get install libudunits2-dev libgdal-dev -y

RUN ["Rscript", "install_libraries.R"]


# open port 5555 to traffic
EXPOSE ${PLUMBER_PORT}
