# Build the docs

Install Roswell

ros install 40ants/cldomain
add ~/.roswell/bin/cldomain to path

apt install cl-launch

In this folder:

virtualenv -p python3 venv
source venv/bin/activate
pip install -r requirements.txt
make html
