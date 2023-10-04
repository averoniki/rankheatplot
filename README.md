# Rank-Heat Plot

[https://rankheatplot.com/rankheatplot/](https://rankheatplot.com)

The rank-heat plot is an [R Shiny](https://shiny.rstudio.com/) application that provides an efficient way to present the results of ranking statistics, particularly when a large amount of data is available, and is targeted to users from various backgrounds.

# Getting Started

The fasted way to get started using the Rank-Heat Plot locally is with [Docker](https://www.docker.com/) and [Docker Compose](https://docs.docker.com/compose/install/). To bring up the application, navigate to the root directory of the project and run `docker compose up`. This command will build the application image with all required dependencies and launch an instance of RStudio that you may access by navigating to `localhost:8852` in your browser. You can then open the file `src/rankheatplot/R/server.R` and click the "Run App" button in the upper right hand side of the file pane.

Alternately, you can run the app on your host machine by cloning the repo and installing the required dependencies (see the `Imports` section [here](./src/rankheatplot/DESCRIPTION)). Note also that you will need to install ComplexHeatMap via BiocManager (`BiocManager::install('ComplexHeatmap')`).

# Deploying

If you wish to deploy the Rank-Heat Plot RShiny application to a server, the [docker-compose.traefik.yml](./docker-compose.traefik.yml) contains a [Shiny Server](https://posit.co/products/open-source/shinyserver/) configuration and a [Traefik](https://traefik.io/) reverse-proxy with automatic TLS via Let's Encrypt, allowing for up to 5 simultaneous R processes. Please note that each process is allowed a maximum of 2 CPUs by default. The convenience script [deploy.sh](./deploy.sh) contains the commands needed to build and deploy the service. Before doing so, however, you will create a file called `.env` based on the [sample](./.env.sample) and fill in the appropriate email address.

# Using Rank-Heat Plot

For a walk-through of the app and detailed usage instructions, please see the "Tutoral and FAQ" section of the [site](https://rankheatplot.com).

# Authors

Veroniki AA, Straus SE, Fyraridis A, Tricco AC

# Citation

Veroniki AA, Straus SE, Fyraridis A, Tricco AC.
The rank-heat plot is a novel way to present the results from a network meta-analysis including multiple outcomes. J. Clin. Epidemiol. 2016; pii: S0895-4356(16)00153-0 (PubMed)
