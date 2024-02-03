#!/bin/bash
sudo docker build -t openstac .
sudo docker run --rm -p 8000:8000 -dt openstac
