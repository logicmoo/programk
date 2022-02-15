#!/usr/bin/env python

import socket
import selectors
import types
#-*- coding:utf-8 -*-  
import sys, select, socket
import os, time

moddate = os.stat(os.path.realpath(__file__))[8] # there are 10 attributes this call returns and you want the next to last
originalsysargv = sys.argv

firstArgStr = sys.argv.pop(0)
firstArg = 0

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
if len(sys.argv) > firstArg and sys.argv[firstArg]=='-v':
  sys.argv.pop(firstArg)
  verbose = 1

show_comment = 0
if len(sys.argv) > firstArg and sys.argv[firstArg]=='-sc':
  sys.argv.pop(firstArg)
  show_comment = 1
if len(sys.argv) > firstArg and sys.argv[firstArg]=='-nc':
   sys.argv.pop(firstArg)
   show_comment = 0

cmdloop = 0
if len(sys.argv) > firstArg and sys.argv[firstArg]=='-cmdloop':
   sys.argv.pop(firstArg)
   cmdloop = 1
else:
 if select.select([sys.stdin,],[],[],0.0)[0]:
  cmdloop = 1

port=0
if len(sys.argv) > firstArg and sys.argv[firstArg]=='-port':
   sys.argv.pop(firstArg)
   port = sys.argv.pop(firstArg)

if verbose==1: print("% " + sys.argv)

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

aimldir='/opt/logicmoo_workspace/packs_xtra/program-y/src/programy/clients/embed/basicbot'

if "python_aiml_learn" in os.environ:
    aimldir=os.environ["python_aiml_learn"]
sys.path.append(aimldir)
os.chdir(aimldir)
from programy.clients.embed.basic import EmbeddedBasicBot

sys.argv.insert(0,firstArgStr)
my_bot = EmbeddedBasicBot()
sys.argv.pop(0)

def do_nlp_proc(text0):
 global my_bot
 text0 = text0.strip(' \t\n\r')
 result = my_bot.ask_question(text0)
 output = 'factoids('+ qt(text0)+','+ dqt(result)+ ').'
 return output

print("%" + do_nlp_proc("learn red is a pretty color"))

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
 sentence=' '.join(sys.argv)
 if sentence=="": sentence="George Washington went to Washington."
 print(do_nlp_proc(sentence), flush=True)
 

