#!/bin/bash

# Remove previous results
rm -rf results/

# Make sure you have the following installed before running:
# Python 3.5 or newer
# pip install pyhocon
# pip install psutil

python3 runner.py benchmark.conf
