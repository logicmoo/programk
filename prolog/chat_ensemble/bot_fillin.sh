#@markdown # ? **Step 1.** Press this button to set up the server (only run this once)
#@markdown Sets up environment for running our ILM demo.
#@markdown This will take a few seconds to finish.
#@markdown You only need to run this once.

# Install Ngrok (exposes colab server to public URL)
!!wget https://bin.equinox.io/c/4VmDzA7iaHb/ngrok-stable-linux-amd64.zip
!!unzip ngrok-stable-linux-amd64.zip

# Install Python deps
!!pip install torch>=1.2.0
!!pip install transformers==2.0.0
!!pip install Flask==1.1.1
!!pip install bs4==0.0.1
!!pip install nltk==3.4.5

import os
import subprocess
import time
import nltk

nltk.download('punkt')

# https://medium.com/@paudelanjanchandra/download-google-drive-files-using-wget-3c2c025a8b99
DOWNLOAD_TEMPLATE = """wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=FILEID' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\\1\\n/p')&id=FILEID" -O FILENAME && rm -rf /tmp/cookies.txt"""
def download_gdrive_file(file_id, output_fp):
  stem = os.path.split(output_fp)[0]
  if len(stem.strip()) > 0:
    if not os.path.isdir(stem):
      os.makedirs(stem)
  cmd = DOWNLOAD_TEMPLATE.replace('FILEID', file_id).replace('FILENAME', output_fp)
  !!{cmd}

def spawn_background_task_get_pid(cmd):
  process = subprocess.Popen(cmd.split())
  return process.pid

DATA_FILES = {
    'stories': {
        'id': '1APYZigQoYBAcQ-55JWQHtknlTgXrdFxW',
        'fp': 'demo_resources/data/stories/valid.txt',
        'style': 'abstract'
    },
    'abstracts': {
        'id': '1puRxh1b1Qnqt8d9KTaf8q0i8QZYXOf0L',
        'fp': 'demo_resources/data/abstracts/valid.txt',
        'style': 'abstract'
    },
    'lyrics': {
        'id': '1l9ijVM88Hult6hTIjXWJBXg4f8FGgwFD',
        'fp': 'demo_resources/data/lyrics/valid.txt',
        'style': 'verse'
    },
}

SERVER_CODE_ID = '1FGMV8MV-HLYMuRK6e36AuD3ao-2lZjHk'
CONFIG_ID = '11KQ0y_n9DqplaBPAow-xh99G7hzNm2Qd'

MODEL_FILES = {
    'stories': {
        'id': '1ixEkWRjGS-JhfZ6Pu9_wugyUKEJfvc9b',
        'fp': 'demo_resources/models/stories/pytorch_model.bin',
        'cfg_fp': 'demo_resources/models/stories/config.json'
    },
    'abstracts': {
        'id': '1bg-LI3p5rIS4-GvKWy5kxVRuc9Id9amX',
        'fp': 'demo_resources/models/abstracts/pytorch_model.bin',
        'cfg_fp': 'demo_resources/models/abstracts/config.json'
    },
    'lyrics': {
        'id': '1EMRFDiBHezjGRoPX_aZMrBqD6lfOzbDG',
        'fp': 'demo_resources/models/lyrics/pytorch_model.bin',
        'cfg_fp': 'demo_resources/models/lyrics/config.json'
    },
}

# Download server code
download_gdrive_file(SERVER_CODE_ID, 'server.tar.gz')
!!tar xvfz server.tar.gz

from IPython.display import clear_output
clear_output()
setup_complete = True
print('Setup complete! Please run the next cell now :)')

#@markdown #? **Step 2.** Press this button to start the server!
#@markdown Once the server has started, a server address will be printed.
#@markdown 
#@markdown **Leave this tab/window running** and open the demo in a new tab: <a href="https://chrisdonahue.com/ilm" target="_blank">chrisdonahue.com/ilm</a>
#@markdown 
#@markdown Paste the server address into that tab.
#@markdown 
#@markdown ### **Step 3.** Try out different text domains
#@markdown 
#@markdown To switch text domains, re-run this cell after selecting the desired domain:
domain = 'Stories' #@param ['Stories', 'Abstracts', 'Lyrics']
#@markdown - **Stories** are short stories
#@markdown - **Abstracts** are CS paper abstracts from arXiv
#@markdown - **Lyrics** are song lyrics
#@markdown 
#@markdown ### (_Optional_) Improve demo reliability
#@markdown 
#@markdown If the demo is giving you connection errors, please try the following:
#@markdown - Sign up for a free ngrok account: <a href="https://dashboard.ngrok.com/signup" target="_blank">ngrok.com</a>
#@markdown - Copy your authtoken from <a href="https://dashboard.ngrok.com/auth" target="_blank">dashboard.ngrok.com/auth</a> and paste it below (won't be shared with us)
#@markdown - Rerun this cell
optional_ngrok_auth_token = '22khKjhyqFTSp3qnMhvDrfEiGT3_4gwAYYKXqMu1A3YLEh2Yd' #@param {type:"string"}

domain = domain.lower()

# Ensure setup has been run
try:
  setup_complete
except:
  raise SystemExit('Please run the setup cell first (above this one).')

# Download data from Google Drive
print('Downloading text data')
download_gdrive_file(DATA_FILES[domain]['id'], DATA_FILES[domain]['fp'])
print('Downloading model')
download_gdrive_file(MODEL_FILES[domain]['id'], MODEL_FILES[domain]['fp'])
download_gdrive_file(CONFIG_ID, MODEL_FILES[domain]['cfg_fp'])
for fp in [DATA_FILES[domain]['fp'], MODEL_FILES[domain]['fp'], MODEL_FILES[domain]['cfg_fp']]:
  try:
    size = os.path.getsize(fp)
  except:
    size = 0
  if size < 500:
    raise ValueError('Failed to download. Please run this cell again!')

# Spawn server, killing previous
print('Starting server...')
server_cmd = 'python api_server.py demo_resources/models/{domain} demo_resources/data/{domain}/valid.txt {style}'.format(domain=domain, style=DATA_FILES[domain]['style'])
try:
  server_pid
  !!kill -9 {server_pid}
except:
  pass
server_pid = spawn_background_task_get_pid(server_cmd)

# Server takes a few seconds to load... hold tight
time.sleep(15)

# Authenticate ngrok
optional_ngrok_auth_token = optional_ngrok_auth_token.strip()
if len(optional_ngrok_auth_token) > 0:
  print('Authenticating ngrok')
  !!./ngrok authtoken {optional_ngrok_auth_token}

# Spawn ngrok
ngrok_cmd = './ngrok http 6006'
try:
  ngrok_pid
  !!kill -9 {ngrok_pid}
except:
  pass
ngrok_pid = spawn_background_task_get_pid(ngrok_cmd)

# Ngrok takes a bit to load... hold tight
time.sleep(10)

# Print address
print('Leave this notebook open and copy the following to https://chrisdonahue.com/ilm :')
!curl -s http://localhost:4040/api/tunnels | python3 -c \
    "import sys, json; address = json.load(sys.stdin)['tunnels'][0]['public_url']; print(address.split('/')[-1])"
