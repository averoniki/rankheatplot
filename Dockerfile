FROM rocker/rstudio:latest

RUN apt-get update && apt-get install -y libxml2 libxt6

# Install R packages
RUN install2.r --error \
    BiocManager \
    circlize \
    mvtnorm \
    netmeta \
    RColorBrewer \
    readxl \
    shiny \
    shinycssloaders \
    shinyjs \
    stringr \
    rmarkdown \
    writexl

RUN R -e "BiocManager::install('ComplexHeatmap')"