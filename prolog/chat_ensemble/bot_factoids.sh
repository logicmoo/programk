#!/bin/bash -x
sleep 1
grep_return_code=0
port=4083
[ ! -f /opt/logicmoo_workspace/nofederation ] && grep_return_code=1


   rm -rf /usr/local/lib/python*/?*-packages/yaml
   rm -rf /usr/local/lib/python*/?*-packages/PyYAML-*
   rm -rf /usr/lib/python*/?*-packages/yaml
   rm -rf /usr/lib/python*/?*-packages/PyYAML-*
   
   sudo -H pip3 install --ignore-installed PyYAML
   
   export programY=/opt/logicmoo_workspace/packs_xtra/program-y/src
   export python_aiml_learn=$programY/programy/clients/embed/basicbot
   ls -l $python_aiml_learn

   #pip install -r $programY/../requirements.txt sleekxmpp==1.3.1
   pip install -e $programY



while [ 1==1 ]
   do      
      [ ! -f /opt/logicmoo_workspace/nofederation ] && grep_return_code=1

      if (( $grep_return_code == 0 )); then         
         ( python ./bot_factoids.py -port $port )
         sleep 0.5
      else 
         echo $0 in federated mode
         sleep 1000
      fi
   done

