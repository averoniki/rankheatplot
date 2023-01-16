FROM rocker/rstudio:latest

# Install R packages
RUN install2.r --error \
    BiocManager \
    readxl \
    circlize \
    RColorBrewer \
    shiny \
    shinycssloaders \
    rmarkdown

RUN R -e "BiocManager::install('ComplexHeatmap')"