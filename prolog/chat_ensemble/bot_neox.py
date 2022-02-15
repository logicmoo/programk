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


import requests
import json
# List Engines (Models) engines = openai.Engine.list() for engine in engines.data: print(engine.id)
#"gpt-neo-20b","gpt-j-6b","gpt-neo-2-7b","gpt-neo-1-3b","gpt-neo-125m","fairseq-13b",
#"fairseq-6-7b","fairseq-2-7b","fairseq-1-3b","fairseq-125m"
engine = "fairseq-125m"

data = {
    #"stop": "\n\n", #"seed": 0,
    "prompt": "A florida man was", #Note that <|endoftext|> is the document separator that the model sees during training, so if a prompt is not specified the model will generate as if from the beginning of a new document
    "engine": engine,
    "stream": False,
    #Truncates logits to the set value.
    "top_k": 40,
    #Number between 0 and 1.0. An alternative to sampling with temperature, called nucleus sampling, where the model considers the results of the tokens with top_p probability mass. So 0.1 means only the tokens comprising the top 10% probability mass are considered.
    #"top_p": 1.0, 
    #We generally recommend altering top_p or temperature but not both.
    #Number between 0 and 1.0. What sampling temperature to use. Higher values means the model will take more risks. Try 0.9 for more creative applications, and 0 (argmax sampling) for ones with a well-defined answer.
    "temperature": 0.2, 
    #Include the log probabilities on the logprobs most likely tokens, as well the chosen tokens. For example, if logprobs is 5, the API will return a list of the 5 most likely tokens. The API will always return the logprob of the sampled token, so there may be up to logprobs+1 elements in the response.
    #"logprobs": [], 
    # Number between 0 and 1.0. Selects tokens according to the expected amount of information they contribute. Ref: Typical Decoding for Natural Language Generation
    "typical_p": 1.0, 
    #Echo back the prompt in addition to the completion
    "echo": False, 
    #Number between 0 and 1.0. Similar to nucleus sampling, but it sets its cutoff point based on the cumulative sum of the accelerations (second derivatives) of the sorted token probabilities rather than the probabilities themselves.
    "tfs": 1.0, 
    #Number between 0 and 1.0. Remove all tokens that have probability below the threshold of: limit = pow(max(probs), 2.0) * top_a
    "top_a": 1.0, 
    #Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far, increasing the model's likelihood to talk about new topics.
    "presence_penalty": 1.0,         
    #Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far, decreasing the model's likelihood to repeat the same line verbatim.
    "frequency_penalty":0.3 ,# | number | Optional | Defaults to 0
    #NEW Number between 0 and 8.0. HuggingFace repetition penalty implementation, uses a divisor. Ref: CTRL - A Conditional Transformer Language Model for Controllable Generation
    "repetition_penalty": 1.0, # | number | Optional | Defaults to 1.0, disabled
    #NEW Number between 0 and 1.0. Slope applied to repetition penalty: m * (x*2-1) / (1 + abs(x*2-1) * (m - 1)), x = [0, 1] Ref: Wolfram Alpha Equation
    "repetition_penalty_slope":0, #| number | Optional | Defaults to 0, disabled    
    #NEW Number between 0 and 2048. The token range to apply the repetition_penalty and repetition_penalty_slope
    "repetition_penalty_range":0, # | number | Optional | Defaults to 0, disabled
    #The maximum number of tokens to generate in the completion. The token count of your prompt plus max_tokens cannot exceed the model's context length. Most models have a context length of 2048 tokens.
    "max_tokens": 100 
}

def do_nlp_proc(text0):
    global data
    try:
        return do_nlp_proc2(text0)
    except Exception as ex:
        resp = data.copy();
        resp['prompt'] = text0;
        resp["neox_error"] = str(ex)
        resp["neox_error_type"] = f"{type(ex)=}"
        resp["neox_error_args"] = "{0}".format(ex.args)
        output = 'neox('+ qt(text0)+','+ qt(json.dumps(resp)) + ').'
        return output

def do_nlp_proc2(text0):
    global data
    global engine
    
    if text0.find("{") == -1:
        data['prompt'] = text0.strip(' \t\n\r')
    else:
        data.update(json.loads(text0))

    if "engine" in data:
      engine = data["engine"]
      del data["engine"]

    try:
        resp = requests.post("https://api.goose.ai/v1/engines/"+engine+"/completions",
             headers={"Authorization": "Bearer "+ os.environ["goose_api_key"]}, json=data).json()

    except Exception as ex:
        resp = data.copy();
        resp["neox_error"] = str(ex)
        resp["neox_error_type"] = f"{type(ex)=}"
        resp["neox_error_args"] = "{0}".format(ex.args)

    data['engine'] = engine
    resp.update(data)
    output = 'neox('+ qt(text0)+','+ qt(json.dumps(resp)) + ').'
    return output

data["engine"]=engine
#print("%" + do_nlp_proc("a florida man was"))
#data["engine"]="fairseq_gpt_13B"
#print("%" + do_nlp_proc("a florida man was"))
#data["engine"]="gpt-neo-20b"
#print("%" + do_nlp_proc("a florida man was"))

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
 if sentence=="": sentence=data["prompt"];
 print(do_nlp_proc(sentence), flush=True)
 

