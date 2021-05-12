#!/bin/bash

for ts in $(seq 10 10 5000); do
    echo "working on $ts"
    python3 plot_water_height.py tsunamiParallel.out $ts
done
