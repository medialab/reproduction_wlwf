# reproduction_wlwf

## Installation

1. clone this repository

2. install dependencies
```bash
cd reproduction_wlwf
pip install -r requirements.txt
```
## Format your data in the following trees
These folders have to be put in a data_source folder in your actual repository.

```
congress
├── lr
│   ├── 2022-06-20.csv
│   ├── 2022-06-21.csv
│   ├── 2022-06-22.csv
    ...
├── majority
│   ├── 2022-06-20.csv
│   ├── 2022-06-21.csv
│   ├── 2022-06-22.csv
    ...
├── nupes
│   ├── 2022-06-20.csv
│   ├── 2022-06-21.csv
│   ├── 2022-06-22.csv
    ...
└── rn
│   ├── 2022-06-20.csv
│   ├── 2022-06-21.csv
│   ├── 2022-06-22.csv

```
```
media
├── 2022-06-20.csv
├── 2022-06-21.csv
├── 2022-06-22.csv
    ...

```
```
supporter
├── lr
│   ├── 2022-06-20.csv
│   ├── 2022-06-21.csv
│   ├── 2022-06-22.csv
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
* supporter
```bash
python 01-create-dtm.py supporter your/path/to/folder/supporter/
```
The results will be saved in `data_prod/dfm/supporter-....txt`

Etc.

## Encoding with Sentence-BERT
```bash
python 01_encode_with_sbert.py public --origin_path
```
Example to encode congress data from another store called distant_store where you have the repository_wlwf: 
```bash
python 01_encode_with_sbert.py congress --origin_path /distant_store/reproduction_wlwf
```
With a group in the following categories : congress, attentive, media, supporter, general. 
--origin_path is by default your current repository, but you can also select another origin to your file tree. Be careful to respect the structure of files and folders of this repository. You can find more informations in <a href="https://github.com/medialab/reproduction_wlwf/tree/main/documentation">Documentation</a>. 

NB : If you are using Windows, use "\" instead of "/" in your paths. 

## Run BERTopic model 
```bash
python 02_run_bertopic.py model_path --origin_path --public 
```
Example to run model for congress, media and general public: 
```bash
python 02_run_bertopic.py data_prod/topics/bert-model/ --origin_path /distant_store/reproduction_wlwf/ --public congress,media,general
```
With model_path as a directory where you want to find or export your trained BERTopic model. 
--origin_path has the same function as in 01_encode_with_sbert.py script. Be careful to keep the same origin-path between the two scripts. 
--group allows choosing the group(s) you want to use to run the model (by default, all groups are included). You can write a group between : congress, attentive, media, supporter, general. You can write several groups by separating them by a comma (for example: python 02_run_bertopic.py model_path congress,media). Be careful to include congress if you haven't used the script before (otherwise, you won't have a trained model).

NB : If you are using Windows, use "\" instead of "/" in your paths. 