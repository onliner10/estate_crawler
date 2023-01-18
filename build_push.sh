#!/bin/bash

sudo docker build . -t 192.168.50.25:5000/estate_crawler:latest
sudo docker push 192.168.50.25:5000/estate_crawler:latest
