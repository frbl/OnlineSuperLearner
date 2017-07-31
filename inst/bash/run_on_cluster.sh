#!/usr/bin/env sh
server_name=peregrine
server_name=peregrine-interactive
scp -r * $server_name:~/osl
#rsync -ue ssh ./ peregrine:~/osl
if [[ $server_name != 'peregrine-interactive' ]]; then
  ssh $server_name -t 'cd osl; srun --ntasks=1 --cpus-per-task=22 --time=00:30:00 --partition=nodes Rscript run.R'
else
  ssh $server_name -t 'rm -rf ~/tmp; mkdir -p ~/tmp/osl/; cd osl; Rscript run.R'
  #ssh $server_name -t 'rm -r ~/tmp; mkdir ~/tmp/osl/; cd osl; R'
fi
rm -r /tmp/osl/
mkdir -p /tmp/osl/
scp -r $server_name:~/tmp/* /tmp/osl/
