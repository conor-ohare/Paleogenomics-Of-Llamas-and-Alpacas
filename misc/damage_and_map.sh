#!/bin/sh
## Add damage and map to Alpaca genome
## $1 - Code name for sample

gargammel -c 1 --comp 0,0,1 -f Fragment_lengths.txt -mapdamage misincorporation.txt single -rl 80 \
-o simulation/$1.damaged input

sh Ancient_mapping_SE_v1.sh 10 simulation/ $1 results \
~/workplace/data1/Reference/GCF_000164845.3_VicPac3.1_genomic.fna 30 0.04