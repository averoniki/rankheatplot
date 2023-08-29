#! /usr/bin/env bash

# Run this script to bring up the "production" build after a code change

set -euo pipefail

docker compose -f docker-compose.traefik.yml build

docker compose -f docker-compose.traefik.yml up -d
