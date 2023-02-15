FROM rocker/rstudio:4.2.2

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
    shinybusy \
    shinyjs \
    stringr \
    rmarkdown \
    writexl

ENV PROJECT_ROOT=/home/rstudio/src/rankheatplot

RUN R -e "BiocManager::install('ComplexHeatmap')"