import io
import os
import glob
import tarfile
from tqdm import tqdm
import casanova
from unidecode import unidecode
from fog.tokenizers.words import WordTokenizer
from sklearn.feature_extraction.text import CountVectorizer

TWEETS_FOLDER = "tweets_from_deputesXVI_220620-230313"
SBERT_NAME = "dangvantuan/sentence-camembert-large"
EMB_DIMENSION = 1024

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
    "ans",
    "apres",
    "après",
    "as",
    "au",
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
    "faites",
    "faut",
    "fois",
    "font",
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
        count += len(files)
    return count


def preprocess(root, nb_files, apply_unidecode=False, write_files=False):
    counter_all = 0
    counter_original = 0
    counter_threads = 0

    _, file_extension = os.path.splitext(root)
    if file_extension:
        if file_extension == ".xz":
            tar = tarfile.open(root, "r:xz")
            loop = tqdm(
                sorted(
                    (
                        io.TextIOWrapper(tar.extractfile(member))
                        for member in tar.getmembers()
                        if member.isreg()
                    )
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

    for filename in loop:
        group_name = ""
        # We search for 'LREM' before searching for 'LR'
        for group in ["LREM", "LR", "RN", "NUPES"]:
            if group in filename:
                group_name = group
                break

        thread_ids = dict()
        threads = dict()
        reader = casanova.reader(filename)

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

        for key, value in sorted(thread_ids.items()):
            if not value:
                threads[key] = ""
            else:
                origin = value[0]
                while origin not in threads:
                    origin = thread_ids[origin][0]
                threads[origin] += " " + value[1]

        with open(filename) as input_file:
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

                    # A common value for BERT-based models are 512 tokens, which corresponds to about 300-400 words (for English)
                    doc = " ".join(doc.split()[:300]).replace("\n", " ")
                    if apply_unidecode:
                        doc = unidecode(doc)

                    if write_files:
                        row[text_pos] = doc
                        enricher.writerow(row, [is_thread, group_name])
                    counter_threads += 1
                    yield doc
            if write_files:
                output_file.close()

    if file_extension:
        if file_extension == ".xz":
            tar.close()
    print(
        "nb of tweets: {}, nb of original tweets: {}, nb of original tweets grouped by threads: {}".format(
            counter_all, counter_original, counter_threads
        )
    )


vectorizer = CountVectorizer(
    stop_words=STOP_WORDS_FR,
    tokenizer=custom_tokenizer,
    max_features=75000,
    ngram_range=(1, 2),
    min_df=10,
)
