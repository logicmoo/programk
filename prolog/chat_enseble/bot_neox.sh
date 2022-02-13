#!/bin/bash
sleep 10


while [ 1==1 ]
   do
      [ ! -f /opt/logicmoo_workspace/nofederation ] && grep_return_code=1

      if (( $grep_return_code == 0 )); then
         apt install zstd
         pip install gptj
         
         # the "slim" version contain only bf16 weights and no optimizer parameters, which minimizes bandwidth and memory         
         MODEL=step_383500_slim

         [ ! -f $MODEL.tar.zstd ] && time wget -c http://eaidata.bmk.sh/data/GPT-J-6B/$MODEL.tar.zstd
         
         time tar -I zstd -xf $MODEL.tar.zstd
         
         git clone https://github.com/kingoflolz/mesh-transformer-jax.git
         pip install -r mesh-transformer-jax/requirements.txt
         
         # jax 0.2.12 is required due to a regression with xmap in 0.2.13
         pip install mesh-transformer-jax/ jax==0.2.12 tensorflow==2.5.0
         python3 ./bot_neox.py -port 4082
      else 
         echo $0 in federated mode
         sleep 1000
      fi
   done

