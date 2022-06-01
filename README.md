## Reproduction
This repository contains dockerfile for building docker image with our experiments.
To build image you need R packages used by our scrips. 

All binaries for Linux are available at: [Dropbox](https://www.dropbox.com/s/fat2prc3uff6qpz/packages.tar?dl=0)

## Running our experiments
Docker and docker-compose is required to run our experminets. The following steps must be done to run our scripts: 
1. Put downloaded archive packages.tar into root directory of this repository.
2. Edit docker-compose.yml file. This file contains environment variable ARGS which is executed during runtime of container. This variable has two parts: One is path to our experiment setup inside the container, which shoudn't be changes and the second is metric that should be not included during experiment. If you want to exclude 2 metrics which'll be for example: **ns** and **nf**, the ARGS variable will be: **ARGS=/usr/local/src/scripts/OurExperiments/SZZ-TSE ns nf**
3. Run our scripts by running command: docker-compose run. The results will be stored inside docker volume **results** managed by docker. 
