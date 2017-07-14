#!/usr/bin/env sh
scp -r * peregrine:~/osl
ssh peregrine -t 'cd osl; srun --ntasks=1 --cpus-per-task=22 --time=00:30:00 --partition=nodes Rscript run.R'
