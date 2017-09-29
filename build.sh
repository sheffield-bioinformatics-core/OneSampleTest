#!/bin/bash

# build docker image
docker build --tag="crukci-bioinformatics/one-sample-test" .

# or to build docker image from scratch without using the cache
#docker build --tag="crukci-bioinformatics/one-sample-test" --no-cache .

# remove dangling/untagged images
#docker rmi $(docker images --filter "dangling=true" -q --no-trunc)

