#!/bin/bash

# Stop on error
set -e

mkdir -p deps

cd deps

# Clone the repository with the code that we need to run.
git clone https://github.com/enewe101/relational-nouns-LREC-2018
cd relational-nouns-LREC-2018
git checkout d6d1689b9107401c12cb74e3a68dd75cda45266d # The commit I tested this with.
cd ..

pip install nltk==3.4.5
conda install -c franzinc python-cjson
pip install corenlp-xml-reader
pip install t4k
pip install iterable-queue
pip install sklearn

## Download and decompress pretrained model.
#wget -O model.tar https://www.dropbox.com/s/55wxplk70uqf0z2/model.tar?dl=1
#tar xvf model.tar
#rm model.tar
#
#cd ..

