#!/bin/bash

set -e

# Run the wavefront program in all currently working modes:
wavefront.exe parfor1 $1 $2
wavefront.exe parfor2 $1 $2
wavefront.exe dnc $1 $2 #for dnc, the choices must be even
