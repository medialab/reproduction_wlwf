# reproduction_wlwf

## Installation

1. clone this repository

2. install dependencies
```bash
cd reproduction_wlwf
pip install -r requirements.txt
```

## Create document-term matrix for a given public ("media-rs" below). The results will be saved in data_prod/dfm/{media-rs}-....txt
    ```bash
    python 01-create-dtm.py /store/medialex/tweets/media_IPG/ media-rs
    ```

## Encoding with Sentence-BERT
    ```bash
    python 01_encode_with_sbert.py data_source/tweets_from_deputes data_prod/embeddings/deputes/
    ```
