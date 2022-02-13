#!/bin/bash

sleep 2
LIB=python-aiml

while [ 1==1 ]
   do
      pip3 list | grep $LIB
      grep_return_code=$?
      echo grep_return_code=$grep_return_code
      
      [ ! -f /opt/logicmoo_workspace/nofederation ] && grep_return_code=1

      if (( $grep_return_code == 0 )); then
         python ./bot_factoids.py -port 4083
      else 
         echo $0 in federated mode
         sleep 1000
      fi
   done

