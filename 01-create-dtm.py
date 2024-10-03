"""
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

"""

import glob
import sys
import numpy as np
import casanova
from unidecode import unidecode
from tqdm import tqdm

from utils import vectorizer, nb_files, clean_text


####################################
# creating matrix for French MPs (députés)
####################################
def preprocess(root, nb_files):
    counter_all = 0
    counter_original = 0
    for filename in tqdm(
        glob.iglob(root + "/**/*.csv", recursive=True), total=nb_files
    ):
        reader = casanova.reader(filename)
        text_pos = reader.headers.text
        rt_pos = reader.headers.retweeted_id

        file_text = ""
        for row in reader:
            counter_all += 1
            if not row[rt_pos]:
                counter_original += 1
                try:
                    row_text = unidecode(clean_text(row[text_pos].replace("\n", "")))
                except IndexError:
                    print(filename)
                    print(row)
                    continue
                file_text += row_text + " "

        # yield the text of all tweets of the day, remove last character - which is a space
        yield file_text[:-1]
    print(
        "nb of tweets: {}, nb of original tweets: {}".format(
            counter_all, counter_original
        )
    )


if __name__ == "__main__":
    folder = sys.argv[1]

    X = vectorizer.fit_transform(preprocess(folder, nb_files))

    # checking words
    words = vectorizer.get_feature_names_out()
    print(words[:10], words[-10:])

    ####################################
    # exporting DFM matrix
    ####################################

    np.savetxt("data_prod/dfm/congress-dtm-indices.txt", X.indices, fmt="%.0f")
    np.savetxt("data_prod/dfm/congress-dtm-pointers.txt", X.indptr, fmt="%.0f")
    np.savetxt("data_prod/dfm/congress-dtm-values.txt", X.data, fmt="%.0f")
    with open("data_prod/dfm/nb_files.txt", "w") as f:
        f.write(str(nb_files))

    ## words
    with open("data_prod/dfm/congress-words.txt", "w") as f:
        for item in words:
            f.write("%s\n" % item)
