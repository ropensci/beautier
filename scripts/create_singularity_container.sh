#!/bin/bash
#
# Create the Singularity container 'plinkr.sif'
#
# Usage:
#
#  ./scripts/create_singularity_container.sh
#
singularity build --fakeroot plinkr.sif Singularity
