#!/usr/bin/env bash

# install pipreqs
python -m pip install pipreqs

# generate requirements.txt
pipreqs ./ --ignore chirps,containers,era5_land,grilla_lat_lon,input,output --savepath requirements.txt
