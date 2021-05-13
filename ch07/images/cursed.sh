#!/bin/bash

for ts in $(seq 0 5 5000); do
    echo "working on $ts"
    python3 plot_water_height.py tsunamiParallel.out $ts
done

convert -delay 5 *.png tsunami1D.gif
