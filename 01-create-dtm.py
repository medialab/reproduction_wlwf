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

##################
# reading documents
##################

import os
import glob
import re
import casanova
from unidecode import unidecode
from fog.tokenizers.words import WordTokenizer
from tqdm import tqdm



####################################
# creating matrix for French MPs (députés)
####################################

from sklearn.feature_extraction.text import CountVectorizer
import numpy as np

STOP_WORDS_FR = ['', "'d", "'ll", "'m", "'re", "'s", "'ve", '-', '0', '1', '2', '3', 
                 'a', 'ah', 'ai', 'aime', 'aller', 'alors', 'ans', 'apres', 'après', 
                 'as', 'au', 'aussi', 'autre', 'autres', 'aux', 'avais', 'avait', 
                 'avant', 'avec', 'avez', 'avoir', 'b', 'bah', 'bcp', 'beaucoup', 
                 'bien', 'bon', 'bonjour', 'bonne', 'bref', 'c', "c'", "c'est", 
                 "c'était", 'ca', 'cc', 'ce', 'cela', 'celle', 'celui', 'ces', 'cest', 
                 'cet', 'cetait', 'cette', 'ceux', 'chaque', 'chez', 'co', 'comme', 
                 'comment', 'compte', 'contre', 'coup', 'cours', 'crois', 'cétait', 
                 'c’est', 'd', "d'", 'dans', 'de', 'deja', 'depuis', 'des', 'detre', 
                 'deux', 'dire', 'dis', 'dit', 'dm', 'dois', 'doit', 'donc', 'du', 
                 'déjà', 'dêtre', 'e', 'eh', 'elle', 'elles', 'en', 'encore', 'entre', 
                 'envie', 'es', 'est', 'estce', 'et', 'etais', 'etait', 'etc', 'ete', 
                 'etes', 'etre', 'eu', 'f', 'faire', 'fais', 'fait', 'faites', 'faut', 
                 'fois', 'font', 'g', 'genre', 'gens', 'grave', 'gros', 'gt', 'h', 
                 'hein', 'https', 'i', 'il', 'ils', 'it', 'j', "j'", "j'", "j'ai", 
                 "j'aime", "j'avais", "j'me", "j'suis", "j'vais", 'jai', 'jaime', 
                 'jamais', 'javais', 'je', 'jen', 'jetais', 'jme', 'jour', 'journee', 
                 'journée', 'jsp', 'jsuis', 'jte', 'juste', 'jvais', 'jveux', 'jétais', 
                 'j’ai', 'k', 'l', "l'", 'la', 'le', 'les', 'leur', 'leurs', 'll', 
                 'lol', 'lui', 'là', 'm', "m'", 'ma', 'maintenant', 'mais', 'mal', 
                 'mdr', 'mdrr', 'mdrrr', 'mdrrrr', 'me', 'mec', 'meme', 'merci', 
                 'merde', 'mes', 'met', 'mettre', 'mieux', 'mis', 'mm', 'moi', 'moins', 
                 'moment', 'mon', 'monde', 'mtn', 'même', 'n', "n'", 'na', 'nan', 'ne', 
                 'nest', 'ni', 'nn', 'non', 'nos', 'notre', 'nous', 'o', 'of', 'oh', 
                 'ok', 'on', 'ont', 'ou', 'ouais', 'oui', 'où', 'p', 'par', 'parce', 
                 'parle', 'pas', 'passe', 'pcq', 'pense', 'personne', 'peu', 'peut', 
                 'peutetre', 'peutêtre', 'peux', 'plus', 'pour', 'pourquoi', 'pq', 'pr', 
                 'prend', 'prendre', 'prends', 'pris', 'ptdr', 'ptdrrr', 'ptn', 'pu', 
                 'putain', 'q', 'qd', 'qu', "qu'", "qu'il", "qu'on", 'quand', 'que', 
                 'quel', 'quelle', 'quelque', 'quelques', 'quelquun', 'qui', 'quil', 
                 'quils', 'quoi', 'quon', 'r', 're', 'rien', 'rt', 's', "s'", 'sa', 
                 'sais', 'sait', 'sans', 'se', 'sera', 'ses', 'sest', 'si', 'sil', 
                 'soir', 'soit', 'son', 'sont', 'suis', 'super', 'sur', 't', "t'", 'ta', 
                 'tas', 'te', 'tellement', 'temps', 'tes', 'tete', 'the', 'tjrs', 'tjs', 
                 'to', 'toi', 'ton', 'toujours', 'tous', 'tout', 'toute', 'toutes', 
                 'tres', 'trop', 'trouve', 'trouvé', 'très', 'tt', 'tu', 'tête', 'u', 
                 'un', 'une', 'v', 'va', 'vais', 'vas', 've', 'veut', 'veux', 'via', 
                 'vie', 'viens', 'voila', 'voilà', 'voir', 'vois', 'voit', 'vont', 
                 'vos', 'votre', 'vous', 'vrai', 'vraiment', 'vs', 'vu', 'w', 'wsh', 
                 'x', 'xd', 'y', 'ya', 'you', 'your', 'z', 'zu', 'zum', 'à', 'ça', 
                 'ça', 'étais', 'était', 'été', 'êtes', 'être', '–—']

def custom_tokenizer(document):
    tokenizer = WordTokenizer(
        keep=['word', 'mention'],
        lower=True,
        unidecode=True,
        split_hashtags=False,
        stoplist=STOP_WORDS_FR,
        reduce_words=True,
        decode_html_entities=True
    )
    return list(token for _, token in tokenizer(document))

def preprocess(root):
    for filename in tqdm(glob.iglob(root + '/**/*.csv', recursive=True)):
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
        
        

def tokenizer(text):
	doc = tokenizeRawTweetText(text)
	doc = [d for d in doc if len(d)>2]
	return(doc)

vectorizer = CountVectorizer(stop_words=STOP_WORDS_FR, tokenizer=custom_tokenizer,
    max_features=75000, ngram_range=(1,2), 
    min_df=10, max_df=.90)

X = vectorizer.fit_transform(preprocess("tweets_from_deputesXVI_220620-230313"))

# checking words
words = vectorizer.get_feature_names_out()
print(words[:10], words[-10:])


####################################
# exporting DFM matrix
####################################

np.savetxt('data/dfm/congress-dtm-indices.txt', X.indices, fmt='%.0f')
np.savetxt('data/dfm/congress-dtm-pointers.txt', X.indptr, fmt='%.0f')
np.savetxt('data/dfm/congress-dtm-values.txt', X.data, fmt='%.0f')

## words
words = vectorizer.get_feature_names_out()
f = open('data/dfm/congress-words.txt', 'w')
for item in words:
  f.write("%s\n" % item)

f.close()

