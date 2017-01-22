#!/bin/bash

cat data/log.csv | grep -i bot | awk -F '"' '{print $2}' | sort | uniq > data/robots_ip.txt
