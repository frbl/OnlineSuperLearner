#!/usr/bin/env sh
#rsync -ue ssh ./ peregrine:~/osl

if [ $# -eq 0 ]; then
  server_name=peregrine-interactive
  scp -r * $server_name:~/osl
  ssh $server_name -t 'mkdir -p ~/tmp/osl/; cd osl; Rscript run.R'
  rm -r /tmp/osl/
  mkdir -p /tmp/osl/
  scp -r $server_name:~/tmp/* /tmp/osl/
else
  server_name=peregrine
  scp -r * $server_name:~/osl2
  if [ $1 == bootstrap ]; then
    # 4 is the number of configurations
    for i in $(seq 1 100);
    do
      echo $i
      ssh $server_name -t "cd osl2; sbatch -o ~/tmp/$i-output.out -e ~/tmp/$i-error.out --job-name=cfg-$i --ntasks=1 --cpus-per-task=23 --time=24:00:00 --partition=regular --mail-user=peregrine@compsy.nl --mail-type=all --wrap=\"Rscript run.R 1\""
    done
  else
    # 4 is the number of configurations
    #for i in $(seq 1 4);
    for i in $(seq 5 8);
    do
      echo $i
      ssh $server_name -t "cd osl2; sbatch -o ~/tmp/$i-output.out -e ~/tmp/$i-error.out --job-name=cfg-$i --ntasks=1 --cpus-per-task=23 --time=48:00:00 --partition=regular --mail-user=peregrine@compsy.nl --mail-type=all --wrap=\"Rscript run.R $i\""
    done
  fi
fi

