# reproduction_wlwf

## Installation

1. clone this repository

2. install dependencies
```bash
cd reproduction_wlwf
pip install -r requirements.txt
```

## Create document-term matrix for a given public ("congress" and "media" below).
For the congress, do not use the --vocab flag since it will infer the vocabulary from the congress tweets.

For other publics, pass the congress terms as input in order to compute a document-term matrix based on the words of the congress.
See the example below.

* congress:
```bash
python 01-create-dtm.py /store/medialex/tweets/deputes/ congress
```
The results will be saved in data_prod/dfm/congress-....txt
* media
```bash
python 01-create-dtm.py /store/medialex/tweets/media_IPG/ media --vocab data_prod/dfm/congress-words.txt
```
The results will be saved in data_prod/dfm/media-....txt

## Encoding with Sentence-BERT
```bash
python 01_encode_with_sbert.py data_source/tweets_from_deputes data_prod/embeddings/deputes/
```
