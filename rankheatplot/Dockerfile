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
    sortable \
    stringr \
    rmarkdown \
    writexl

RUN R -e "BiocManager::install('ComplexHeatmap')"
