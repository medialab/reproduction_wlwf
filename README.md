# reproduction_wlwf

## Installation

1. clone this repository

2. install dependencies
```bash
cd reproduction_wlwf
pip install -r requirements.txt
```
## Format your data in the following tree

```
deputes
├── lr
│   ├── 20220620.csv
│   ├── 20220621.csv
│   ├── 20220622.csv
    ...
├── majority
│   ├── 20220620.csv
│   ├── 20220621.csv
│   ├── 20220622.csv
    ...
├── nupes
│   ├── 20220620.csv
│   ├── 20220621.csv
│   ├── 20220622.csv
    ...
└── rn
│   ├── 20220620.csv
│   ├── 20220621.csv
│   ├── 20220622.csv
│
media
├── 20220620.csv
├── 20220621.csv
├── 20220622.csv
...
```

## Create document-term matrix for a given public
For the congress, do not use the `--vocab` flag since the script will directly infer the vocabulary from the congress tweets.
**Do** use the `--party` flag since it will search the user's party ("majority", "rn", etc.) in the file path.


For other publics, pass the congress terms stored in `data_prod/dfm/congress-words.txt` as input in order to compute a document-term matrix based on the words of the congress.
See the examples below.

* congress:
```bash
python 01-create-dtm.py {FOLDER}/deputes/ congress --party
```
The results will be saved in `data_prod/dfm/congress-....txt`
* media
```bash
python 01-create-dtm.py {FOLDER}/media/ media --vocab data_prod/dfm/congress-words.txt
```
The results will be saved in data_prod/dfm/media-....txt

## Encoding with Sentence-BERT
```bash
python 01_encode_with_sbert.py data_source/tweets_from_deputes data_prod/embeddings/deputes/
```
