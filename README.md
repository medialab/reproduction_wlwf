# reproduction_wlwf

## Installation

1. clone this repository

2. install dependencies
```bash
cd reproduction_wlwf
pip install -r requirements.txt
```
## Format your data in the following trees

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

```
```
media
├── 20220620.csv
├── 20220621.csv
├── 20220622.csv
    ...

```
```
supporters
├── lr
│   ├── 20220620.csv
│   ├── 20220621.csv
│   ├── 20220622.csv
    ...
...
```

The csv files should have the following columns: `id`, `local_time`, `text`, `user_screen_name`, `user_id`, `retweeted_id`
```
id                  local_time          text                 user_screen_name user_id             retweeted_id
1587218214638985216 2022-11-01T00:01:26 RT @UEFrance: 🆕 Es… trudigoz         347374931           1587030788331159553
1587355550840414208 2022-11-01T09:07:09 RT @midy_paul: #Sai… midy_paul        1090311673985056770 1587112047480918018
1587374936288632833 2022-11-01T10:24:11 Cérémonies du Souve… Bannier_G        866695760905154560  <empty>

```


## Create document-term matrix for a given public

See the examples below.

* congress:
```bash
python 01-create-dtm.py congress your/path/to/folder/deputes/
```
The results will be saved in `data_prod/dfm/congress-....txt`
* media
```bash
python 01-create-dtm.py media your/path/to/folder/media/
```
The results will be saved in `data_prod/dfm/media-....txt`
* supporters
```bash
python 01-create-dtm.py supporter your/path/to/folder/supporters/
```
The results will be saved in `data_prod/dfm/supporter-....txt`

Etc.

## Encoding with Sentence-BERT
```bash
python 01_encode_with_sbert.py data_source/tweets_from_deputes data_prod/embeddings/deputes/
```
