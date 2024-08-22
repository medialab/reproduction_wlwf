'''
# 01-create-dtm.py

Date: 6/6/2015
Author: Pablo Barberá

Takes txt files with tweets grouped by chamber and party and prepares
a sparse matrix with unigram + bigram counts per document.

Then applies same tokenizer to user and media corpus

NOTE: running this script requires access to the original corpus of
tweets, which we cannot share publicly in order to comply with Twitter's
terms of service. For advice on how to re-run this script, please
contact the corresponding author.

All other scripts can be run without running this one first, since we
are providing the files required to replicate the document-feature matrices
we use in the analysis.

'''
import glob
import re
import numpy as np
import casanova
from unidecode import unidecode
from tqdm import tqdm

from utils import vectorizer, nb_files, TWEETS_FOLDER


####################################
# creating matrix for French MPs (députés)
####################################
def preprocess(root, nb_files):
    for filename in tqdm(glob.iglob(root + '/**/*.csv', recursive=True), total=nb_files):
        reader = casanova.reader(filename)
        text_pos = reader.headers.text
        file_text = ""
        for row in reader:
            try:
                row_text = unidecode(re.sub(r'http\S+|RT|&amp;|,|\.|\xe2\x80\xa6|-', "", row[text_pos].replace("\n", "")))
            except IndexError:
                print(filename)
                print(row)
                continue
            file_text += row_text + " "
        # yield the text of all tweets of the day, remove last character - which is a space
        yield file_text[:-1]


if __name__ == "__main__":
    X = vectorizer.fit_transform(preprocess(TWEETS_FOLDER, nb_files))
    
    # checking words
    words = vectorizer.get_feature_names_out()
    print(words[:10], words[-10:])
    
    
    ####################################
    # exporting DFM matrix
    ####################################
    
    np.savetxt('data/dfm/congress-dtm-indices.txt', X.indices, fmt='%.0f')
    np.savetxt('data/dfm/congress-dtm-pointers.txt', X.indptr, fmt='%.0f')
    np.savetxt('data/dfm/congress-dtm-values.txt', X.data, fmt='%.0f')
    with open('data/dfm/nb_files.txt', 'w') as f:
        f.write(str(nb_files))
    
    ## words
    with open('data/dfm/congress-words.txt', 'w') as f:
        for item in words:
          f.write("%s\n" % item)
