#! /usr/bin/env bash

# Run this script to bring up the shiny-server implementation of the application

set -euo pipefail

docker compose -f docker-compose.traefik.yml build

docker compose -f docker-compose.traefik.yml up -d
