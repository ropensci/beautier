#!/bin/bash
#
# Create the Docker definition file 'beautier.docker'
#
# Usage:
#
#  ./scripts/create_docker_container.sh
#

if [ ! -f Singularity ]; then
  echo "File 'Singularity' not found. "
  echo "Please run from beautier root folder. "
  echo " "
  echo "From there, do: "
  echo " "
  echo "  ./scripts/create_docker_container.sh"
  echo " "
  exit 42
fi
