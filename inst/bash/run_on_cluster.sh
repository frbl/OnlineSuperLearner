#!/usr/bin/env sh
#rsync -ue ssh ./ peregrine:~/osl

if [ $# -eq 0 ]; then
  server_name=peregrine-interactive
  scp -r * $server_name:~/osl
  ssh $server_name -t 'rm -rf ~/tmp; mkdir -p ~/tmp/osl/; cd osl; Rscript run.R'
  rm -r /tmp/osl/
  mkdir -p /tmp/osl/
  scp -r $server_name:~/tmp/* /tmp/osl/
else
  server_name=peregrine
  scp -r * $server_name:~/osl
  # 4 is the number of configurations
  for i in $(seq 1 4);
  do
    ssh $server_name -t "cd osl; sbatch -o ~/tmp/$i-output.out -e ~/tmp/$i-error.out --job-name=cfg-$i --ntasks=1 --cpus-per-task=23 --time=16:00:00 --partition=regular --mail-user=peregrine@compsy.nl --mail-type=all --wrap=\"Rscript run.R $i\""
  done
fi

