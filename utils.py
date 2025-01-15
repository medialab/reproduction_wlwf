import io
import os
import re
import glob
import tarfile
import casanova
import numpy as np
from tqdm import tqdm
from unidecode import unidecode
from transformers import CamembertTokenizer
from fog.tokenizers.words import WordTokenizer
from sklearn.feature_extraction.text import CountVectorizer

GROUPS = [
    "majority",
    "lr",
    "rn",
    "nupes",
]  # MPs tweets should be stored in {input_folder}/{group}/YYYYMMDD.csv, e.g. data_source/lr/20221224.csv
SBERT_NAME = "dangvantuan/sentence-camembert-large"  # Sentence-BERT for French tweets
EMB_DIMENSION = 1024  # Dimension of sentence-BERT embeddings
AN_HASHTAGS_PATTERN = r"(#directAN|#assembl[ée]enationale|#assembl[ée]national)"  # Exclude hashtags linked to French National Assembly
DEFAULT_SAVE_SIZE = 100_000
RANDOM_SEED = 98347
TRAILING_MENTIONS_PATTERN = r"^(@\w+(?:\s+@\w+)*)"
URLS_PATTERN = r"([\w+]+\:\/\/)?([\w+]+\:\/\/)?([\w\d-]+\.)*[\w-]+[\.\:]\w+([\/\?\=\&\#.]?[\w-]+)*\/?"
AN_HASHTAGS_PATTERN = r"(#directAN|#assembl[ée]enationale|#assembl[ée]national)"


STOP_WORDS_FR = [
    "",
    "'d",
    "'ll",
    "'m",
    "'re",
    "'s",
    "'ve",
    "-",
    "0",
    "1",
    "2",
    "3",
    "a",
    "ah",
    "ai",
    "aime",
    "aller",
    "alors",
    "annee",
    "année",
    "ans",
    "apres",
    "après",
    "as",
    "au",
    "aujourdhui",
    "aujourd'hui",
    "aussi",
    "autre",
    "autres",
    "aux",
    "avais",
    "avait",
    "avant",
    "avec",
    "avez",
    "avoir",
    "avons",
    "b",
    "bah",
    "bcp",
    "beaucoup",
    "bien",
    "bon",
    "bonjour",
    "bonne",
    "bref",
    "c",
    "c'",
    "c'est",
    "c'était",
    "ca",
    "cc",
    "ce",
    "cela",
    "celle",
    "celui",
    "ces",
    "cest",
    "cet",
    "cetait",
    "cette",
    "ceux",
    "chaque",
    "chez",
    "co",
    "comme",
    "comment",
    "compte",
    "contre",
    "coup",
    "cours",
    "crois",
    "cétait",
    "c’est",
    "d",
    "d'",
    "dans",
    "de",
    "deja",
    "depuis",
    "des",
    "detre",
    "deux",
    "devons",
    "dire",
    "dis",
    "dit",
    "dm",
    "dois",
    "doit",
    "donc",
    "du",
    "déjà",
    "dêtre",
    "e",
    "eh",
    "elle",
    "elles",
    "en",
    "encore",
    "entre",
    "envie",
    "es",
    "est",
    "estce",
    "et",
    "etais",
    "etait",
    "etc",
    "ete",
    "etes",
    "etre",
    "eu",
    "f",
    "faire",
    "fais",
    "fait",
    "faite",
    "faites",
    "faut",
    "fois",
    "font",
    "francais",
    "francaise",
    "france",
    "français",
    "française",
    "g",
    "genre",
    "gens",
    "grave",
    "gros",
    "gt",
    "h",
    "hein",
    "https",
    "i",
    "ici",
    "il",
    "ils",
    "it",
    "j",
    "j'",
    "j'",
    "j'ai",
    "j'aime",
    "j'avais",
    "j'me",
    "j'suis",
    "j'vais",
    "jai",
    "jaime",
    "jamais",
    "javais",
    "je",
    "jen",
    "jetais",
    "jme",
    "jour",
    "journee",
    "journée",
    "jsp",
    "jsuis",
    "jte",
    "juste",
    "jvais",
    "jveux",
    "jétais",
    "j’ai",
    "k",
    "l",
    "l'",
    "la",
    "le",
    "les",
    "leur",
    "leurs",
    "ll",
    "lol",
    "lui",
    "là",
    "m",
    "m'",
    "ma",
    "maintenant",
    "mais",
    "mal",
    "matin",
    "mdr",
    "mdrr",
    "mdrrr",
    "mdrrrr",
    "me",
    "mec",
    "meme",
    "merci",
    "merde",
    "mes",
    "met",
    "mettre",
    "mieux",
    "mis",
    "mm",
    "moi",
    "moins",
    "moment",
    "mon",
    "monde",
    "mtn",
    "même",
    "n",
    "n'",
    "na",
    "nan",
    "ne",
    "nest",
    "ni",
    "nn",
    "non",
    "nos",
    "notre",
    "nous",
    "o",
    "of",
    "oh",
    "ok",
    "on",
    "ont",
    "ou",
    "ouais",
    "oui",
    "où",
    "p",
    "par",
    "parce",
    "parle",
    "pas",
    "passe",
    "pcq",
    "pense",
    "personne",
    "peu",
    "peut",
    "peutetre",
    "peutêtre",
    "peux",
    "plus",
    "pour",
    "pourquoi",
    "pq",
    "pr",
    "prend",
    "prendre",
    "prends",
    "pris",
    "ptdr",
    "ptdrrr",
    "ptn",
    "pu",
    "putain",
    "q",
    "qd",
    "qu",
    "qu'",
    "qu'il",
    "qu'on",
    "quand",
    "que",
    "quel",
    "quelle",
    "quelque",
    "quelques",
    "quelquun",
    "qui",
    "quil",
    "quils",
    "quoi",
    "quon",
    "r",
    "re",
    "rien",
    "rt",
    "s",
    "s'",
    "sa",
    "sais",
    "sait",
    "sans",
    "savent",
    "se",
    "sera",
    "ses",
    "sest",
    "si",
    "sil",
    "soir",
    "soit",
    "son",
    "sont",
    "suis",
    "super",
    "sur",
    "t",
    "t'",
    "ta",
    "tas",
    "te",
    "tellement",
    "temps",
    "tes",
    "tete",
    "the",
    "tjrs",
    "tjs",
    "to",
    "toi",
    "ton",
    "toujours",
    "tous",
    "tout",
    "toute",
    "toutes",
    "tres",
    "trop",
    "trouve",
    "trouvé",
    "très",
    "tt",
    "tu",
    "tête",
    "u",
    "un",
    "une",
    "v",
    "va",
    "vais",
    "vas",
    "ve",
    "veut",
    "veux",
    "via",
    "vie",
    "viens",
    "voila",
    "voilà",
    "voir",
    "vois",
    "voit",
    "vont",
    "vos",
    "votre",
    "vous",
    "vrai",
    "vraiment",
    "vs",
    "vu",
    "w",
    "wsh",
    "x",
    "xd",
    "y",
    "ya",
    "you",
    "your",
    "z",
    "zu",
    "zum",
    "à",
    "ça",
    "ça",
    "étais",
    "était",
    "été",
    "êtes",
    "être",
    "–—",
]


def clean_text(doc):
    # Remove trailing mentions
    doc = re.sub(
        TRAILING_MENTIONS_PATTERN,
        "",
        doc,
        flags=re.MULTILINE | re.IGNORECASE,
    )
    # Remove urls
    doc = re.sub(
        URLS_PATTERN,
        "",
        doc,
        flags=re.MULTILINE | re.IGNORECASE,
    )
    # Remove AN hashtags
    doc = re.sub(
        AN_HASHTAGS_PATTERN,
        "",
        doc,
        flags=re.MULTILINE | re.IGNORECASE,
    )
    return doc


def existing_dir_path(string):
    if os.path.isdir(string):
        return string
    elif os.path.splitext(string)[1] == ".xz":
        return string
    else:
        raise NotADirectoryError(string)


def create_dir(string):
    try:
        os.mkdir(string)
    # If a parent directory in the path does not exist, FileNotFoundError is raised.
    except FileExistsError:
        os.makedirs(string, exist_ok=True)
        return string

    return string


def reduce_doc_size(doc, length=512):
    tokens = camembert_tokenizer.tokenize(doc)
    while len(tokens) > length:
        point_index = -1
        for i, token in reversed(list(enumerate(tokens))):
            if token == "." and i != len(tokens) - 1:
                point_index = i + 1
                break
        if point_index == -1:
            tokens = tokens[:length]
        else:
            tokens = tokens[:point_index]
    short_doc = "".join(tokens).replace("▁", " ").strip()
    return short_doc


def custom_tokenizer(document):
    tokenizer = WordTokenizer(
        keep=["word", "mention"],
        lower=True,
        unidecode=True,
        split_hashtags=False,
        stoplist=STOP_WORDS_FR,
        reduce_words=True,
        decode_html_entities=True,
    )
    return list(token for _, token in tokenizer(document))


def count_nb_files(folder):
    count = 0
    for r, d, files in os.walk(folder):
        count += len([f for f in files if f.endswith(".csv")])
    return count


def grep_group_name(filename):
    # We search for 'LREM' before searching for 'LR'
    for group in GROUPS:
        if group in filename.lower():
            group_name = group
            if group_name == "lrem":
                group_name = "majority"
            return group_name
    return ""


def iter_on_files(root, nb_files):
    compressed = False
    tar = None

    _, file_extension = os.path.splitext(root)
    if file_extension:
        if file_extension == ".xz":
            compressed = True
            tar = tarfile.open(root, "r:xz")
            loop = tqdm(
                (
                    member
                    for member in sorted(tar.getmembers(), key=lambda x: x.name)
                    if member.isreg()
                ),
                total=nb_files,
                desc="Read compressed files",
            )
        else:
            raise ValueError("Invalid file extension: {}".format(file_extension))
    else:
        loop = tqdm(
            sorted(glob.iglob(root + "/**/*.csv", recursive=True)),
            total=nb_files,
            desc="Read csv files",
        )
    return tar, loop, compressed

def extract_and_format_date(date_str): #Fonction pour extraire les dates des noms de fichier 
    match = re.search(r'(\d{8})', date_str)  # Extraction des 8 chiffres
    if match:
        date_raw = match.group(1)  # "20250101"
        # Reformater en "AAAA-MM-JJ"
        return date_raw[:4] + '-' + date_raw[4:6] + '-' + date_raw[6:]
    return None  # Retourne None si aucune date valide n'est trouvée


def preprocess(root, nb_files, d={}, apply_unidecode=False, write_files=False, small=False):
    counter_all = 0
    counter_original = 0
    counter_threads = 0
    counter_date = 0 

    tar, loop, compressed = iter_on_files(root, nb_files)

    for file in loop:
        if compressed:
            filename = file.name
        else:
            filename = file

        loop.set_description(filename)

        file_date = extract_and_format_date(filename)
       
        group_name = grep_group_name(filename)

        thread_ids = dict()
        threads = dict()

        if compressed:
            filestream = io.TextIOWrapper(tar.extractfile(file))
        else:
            filestream = open(file)
        reader = casanova.reader(filestream)

        d[filename] = (file_date, , group_name, counter_date)

        line_count = sum(1 for _ in reader) #On compte le nombre de lignes associés à cette date 
        counter_date = counter_date + line_count #On associe alors au compteur de la date l'index à partir du quel la prochaine date commencera

        text_pos = reader.headers.text
        id_pos = reader.headers.id
        rt_pos = reader.headers.retweeted_id
        user_pos = reader.headers.user_id
        to_user_pos = reader.headers.to_userid
        to_id_pos = reader.headers.to_tweetid

        for row in reader:
            counter_all += 1
            if not row[rt_pos]:
                counter_original += 1
                # if the tweet is a reply to another tweet of the same user, keep its id to form threads
                if row[to_user_pos] == row[user_pos] and row[to_id_pos]:
                    thread_ids[row[id_pos]] = (row[to_id_pos], row[text_pos])
                    if row[to_id_pos] not in thread_ids:
                        thread_ids[row[to_id_pos]] = None
        if not compressed:
            filestream.close()

        for key, value in sorted(thread_ids.items()):
            if not value:
                threads[key] = ""
            else:
                origin = value[0]
                while origin not in threads:
                    if thread_ids[origin] is not None:
                        origin = thread_ids[origin][0]
                    else:
                        threads[origin] = ""
                threads[origin] += " " + value[1]

        if compressed:
            input_file = io.TextIOWrapper(tar.extractfile(file))
        else:
            input_file = open(file)

        if write_files:
            output_file = open(filename.replace(".csv", "_preprocessed.csv"), "w")
            enricher = casanova.enricher(
                input_file, output_file, add=["is_thread", "group"]
            )
        else:
            enricher = casanova.reader(input_file)

        for row in enricher:
            if not row[rt_pos]:
                is_thread = 0
                if row[id_pos] in threads:
                    is_thread = 1
                    doc = row[text_pos] + " " + threads[row[id_pos]]

                elif row[id_pos] not in thread_ids:
                    doc = row[text_pos]

                else:
                    continue

                doc = clean_text(doc)

                # Keep only documents whith more than 50 characters
                if len(doc) > 50:
                    # A common value for BERT-based models is 512 tokens
                    doc = reduce_doc_size(doc, length=500)

                    if apply_unidecode:
                        doc = unidecode(doc)
                    if write_files:
                        row[text_pos] = doc
                        enricher.writerow(row, [is_thread, group_name])
                    counter_threads += 1
                    yield doc

        if write_files:
            output_file.close()
        if not compressed:
            input_file.close()

    if compressed:
        tar.close()
    print(
        "nb of tweets: {}, nb of original tweets: {}, nb of original tweets grouped by threads: {}".format(
            counter_all, counter_original, counter_threads
        )
    )


def load_embeddings(path, save_size, nb_docs, resume_encoding=False):
    max_index = 0
    embeddings = np.empty((save_size, EMB_DIMENSION)) if resume_encoding else np.empty((nb_docs, EMB_DIMENSION))
    for file in glob.glob(path.replace(".npz", "_*")):
        index = int(file[len(path) - 3 : -len(".npz")])
        if index > max_index:
            max_index = index

        if not resume_encoding:
            if index % save_size == 0:
                embeddings[index - save_size : index] = np.load(file)["embeddings"]
            else:
                embeddings[embeddings.shape[0] - (embeddings.shape[0] % save_size) :] = (
                    np.load(file)["embeddings"]
                )
    if not resume_encoding:
        print("Loaded {} previously encoded rows".format(np.any(embeddings, axis=1).sum()))
    return max_index, embeddings


camembert_tokenizer = CamembertTokenizer.from_pretrained(SBERT_NAME)

vectorizer = CountVectorizer(
    stop_words=STOP_WORDS_FR,
    tokenizer=custom_tokenizer,
    max_features=75000,
    ngram_range=(1, 2),
    min_df=10,
)