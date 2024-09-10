import os
import glob
import tarfile
from tqdm import tqdm
import casanova
from fog.tokenizers.words import WordTokenizer
from sklearn.feature_extraction.text import CountVectorizer

TWEETS_FOLDER = "tweets_from_deputesXVI_220620-230313"
SBERT_NAME = "dangvantuan/sentence-camembert-large"
EMB_DIMENSION = 1024

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


def count_nb_files(folder):
    count = 0
    for r, d, files in os.walk(folder):
        count += len(files)
    return count


def preprocess(root, nb_files):
    counter_all = 0
    counter_original = 0
    docs = []

    _, file_extension = os.path.splitext(root)
    if file_extension:
        if file_extension == ".xz":
            tar = tarfile.open(root, "r:xz")
            loop = (tarinfo.name for tarinfo in tar)
        else:
            raise ValueError("Invalid file extension: {}".format(file_extension))
    else:
        loop = tqdm(glob.iglob(root + '/**/*.csv', recursive=True), total=nb_files, desc="Read csv files")

    for filename in loop:
        reader = casanova.reader(filename)
        text_pos = reader.headers.text
        rt_pos = reader.headers.retweeted_id
        for row in reader:
            counter_all += 1
            if not row[rt_pos]:
                counter_original += 1
                try:
                     docs.append(row[text_pos].replace("\n", " ")[:500])
                except IndexError:
                    print(filename)
                    print(row)
                    continue
    if file_extension:
        if file_extension == ".xz":
            tar.close()
    print("nb of tweets: {}, nb of original tweets: {}".format(counter_all, counter_original))
    return docs


vectorizer = CountVectorizer(stop_words=STOP_WORDS_FR, tokenizer=custom_tokenizer,
    max_features=75000, ngram_range=(1,2), min_df=10)