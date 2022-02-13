#!/bin/bash -x

sleep 1
LIB=python-aiml
grep_return_code=0
export python_aiml=/opt/logicmoo_workspace/packs_xtra/python-aiml

while [ 1==1 ]
   do      
      [ ! -f /opt/logicmoo_workspace/nofederation ] && grep_return_code=1

      if (( $grep_return_code == 0 )); then
         pip install -e $python_aiml
         python ./bot_factoids.py -port 4081
      else 
         echo $0 in federated mode
         sleep 1000
      fi
   done

