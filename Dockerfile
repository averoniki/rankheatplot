FROM rocker/rstudio:latest

# RUN apt-get update && apt-get install -y libudunits2-dev libproj-dev libgdal-dev libxt6

# Install R packages
RUN install2.r --error \
    BiocManager \
    readxl \
    circlize \
    RColorBrewer

RUN R -e "BiocManager::install('ComplexHeatmap')"