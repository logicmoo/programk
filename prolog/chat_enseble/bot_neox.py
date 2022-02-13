#!/usr/bin/env python
import socket
import selectors
import types
#-*- coding:utf-8 -*-  
import sys, select, socket
import os, time

moddate = os.stat(os.path.realpath(__file__))[8] # there are 10 attributes this call returns and you want the next to last
originalsysargv = sys.argv

sysargv = sys.argv
sysargv.pop(0)

# single quote prolog atoms
def qt(s):
 str = s
 if ("'" in str or "\\" in str or " " in str or '"' in str or '.' in str or '?' in str or ',' in str): 
  return "'"+str.replace("\\","\\\\").replace("'","\\'")+"'"
 #if (str.lower()==str and str.upper()!=str): return str
 return "'"+str+"'"

def dqt(s):
 return '"'+str(s).replace("\\","\\\\").replace('"','\\"')+'"'

def refresh_on_file_mod():
   if moddate == os.stat(os.path.realpath(__file__))[8]:
        return
   with open(os.path.dirname(__file__) + os.sep + 'refresh.py', 'r') as f:    \
    exec(compile(f.read().replace('__BASE__',                              \
        os.path.basename(__file__).replace('.py', '')).replace('__FILE__', \
            __file__), __file__, 'exec'))

verbose = 0
if len(sysargv) > 0 and sysargv[0]=='-v':
  sysargv.pop(0)
  verbose = 1

show_comment = 0
if len(sysargv) > 0 and sysargv[0]=='-sc':
  sysargv.pop(0)
  show_comment = 1
if len(sysargv) > 0 and sysargv[0]=='-nc':
   sysargv.pop(0)
   show_comment = 0

cmdloop = 0
if len(sysargv) > 0 and sysargv[0]=='-cmdloop':
   sysargv.pop(0)
   cmdloop = 1
else:
 if select.select([sys.stdin,],[],[],0.0)[0]:
  cmdloop = 1


#from gpt_j.Basic_api import simple_completion
#from gpt_j.gptj_api import Completion
#from textsynth import TextSynth
#synth = TextSynth()
import requests
import json

headers = {
    "Authorization": f"Bearer 7e60a3256ed1b4af7fff42ef065bce4c"
}

data = {
    "prompt": "A florida man was",
    #"stream": True,
    "temperature": 0.8,
    "top_k": 40,
    "stop": "\n\n",
    "max_tokens": 200,
    "top_p": 1,
    "seed": 0,
    "engine": "gptj_6B"
}

engine = "gptj_6B"
# engine = "fairseq_gpt_13B"

def do_nlp_proc(text0):
    global data
    global engine
    
    if text0.find("{") == -1:
        data['prompt'] = text0.strip(' \t\n\r')
    else:
        data = json.loads(text0)

    if "engine" in data:
      engine = data["engine"]
      del data["engine"]

    url = "https://api.textsynth.com/v1/engines/"+engine+"/completions"
    try:
        resp = requests.post(url,headers={"Authorization": "Bearer 7e60a3256ed1b4af7fff42ef065bce4c"}, json=data).json()
    except Exception as ex:
        resp = data
        resp["error"] = str(ex)
        resp["error_type"] = f"{type(ex)=}"
        resp["error_args"] = "{0}".format(ex.args)

    data['engine'] = engine
    resp['engine'] = engine
    text0 = json.dumps(data)
    output = 'neox('+ qt(text0)+','+ qt(json.dumps(resp)) + '").'
    return output

data["engine"]="fairseq_gpt_13B"
print(do_nlp_proc("a flora man was"))
data["engine"]="gptj_6B"
print(do_nlp_proc("a flora man was"))


port=0
if len(sysargv) > 0 and sysargv[0]=='-port':
   sysargv.pop(0)
   port = sysargv.pop(0)

if verbose==1: print("% " + sysargv)

print("", end='',  flush=True)

def service_connection(key, mask):
    sock = key.fileobj
    data = key.data
    if mask & selectors.EVENT_READ:
        recv_data = sock.recv(1024)  # Should be ready to read
        if recv_data:
            data.outb += recv_data            
        else:
            print('closing connection to', data.addr)
            sel.unregister(sock)
            sock.close()
    if mask & selectors.EVENT_WRITE:
        if data.outb:
            print('replying to', repr(data.outb), 'to', data.addr)
            recv = str(data.outb.decode())
            size = len(recv)
            print("got: ",recv,size);
            data.outb = data.outb[size:]
            # Should be ready to write
            sock.send((do_nlp_proc(recv)+"\n").encode())
            

def accept_wrapper(sock):
    conn, addr = sock.accept()  # Should be ready to read
    print('accepted connection from', addr)
    conn.setblocking(False)
    data = types.SimpleNamespace(addr=addr, inb=b'', outb=b'')
    events = selectors.EVENT_READ | selectors.EVENT_WRITE
    sel.register(conn, events, data=data)

if port!=0:
    import selectors
    sel = selectors.DefaultSelector()
    # ...
    lsock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    lsock.bind(('0.0.0.0', int(port)))
    lsock.listen()
    print('listening on',('0.0.0.0', int(port)))
    lsock.setblocking(False)
    sel.register(lsock, selectors.EVENT_READ, data=None)
    while True:
        events = sel.select(timeout=None)
        for key, mask in events:
            if key.data is None:
                accept_wrapper(key.fileobj)
            else:
                service_connection(key, mask)
            

if cmdloop==1: 
 print("\n cmdloop_Ready. \n", end='',  flush=True)

if cmdloop==1:
 for line in sys.stdin: 
  print(do_nlp_proc(line), flush=True)
  if cmdloop==0: 
   sys.exit(0)
  refresh_on_file_mod()
else:
 sentence=' '.join(sysargv)
 if sentence=="": sentence="George Washington went to Washington."
 print(do_nlp_proc(sentence), flush=True)
 

