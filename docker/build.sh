#!/bin/bash
cp -r ../inst/plumber .
sudo docker build --no-cache -t openstac .
