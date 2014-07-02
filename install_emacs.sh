# Install emacs24
# https://launchpad.net/~cassou/+archive/emacs
sudo apt-get install -y python-software-properties
sudo apt-add-repository -y ppa:cassou/emacs
sudo apt-add-repository -y ppa:nviennot/tmate
sudo apt-get -qq update

# Installing emacs with X
sudo apt-get install -y emacs24 emacs24-el emacs24-common-non-dfsg

# Install AG
sudo apt-get install -y silversearcher-ag

# Install screen
sudo apt-get install -y screen

# Install tmate
sudo apt-get install -y tmate
