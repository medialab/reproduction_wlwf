import io
import os
import re
import glob
import tarfile
import casanova
import csv
import numpy as np
import pandas as pd
from tqdm import tqdm
from unidecode import unidecode
from transformers import CamembertTokenizer
from fog.tokenizers.words import WordTokenizer
from sklearn.feature_extraction.text import CountVectorizer
from collections import defaultdict

GROUPS = [
    "majority",
    "lr",
    "rn",
    "nupes",
]  # MPs tweets should be stored in {input_folder}/{group}/YYYYMMDD.csv, e.g. data_source/lr/20221224.csv
SBERT_NAME = "Lajavaness/sentence-camembert-large"  # Sentence-BERT for French tweets
EMB_DIMENSION = 1024  # Dimension of sentence-BERT embeddings
AN_HASHTAGS_PATTERN = r"(#directAN|#assembl[ée]enationale|#assembl[ée]national)"  # Exclude hashtags linked to French National Assembly
DEFAULT_SAVE_SIZE = 100_000
RANDOM_SEED = 98347
choices = ["congress", "media", "supporter", "attentive", "general"]

# Nb docs used for tests. Should be smaller than DEFAULT_SAVE_SIZE.
NB_DOCS_SMALL_TRAIN = 1000  # Choose a small number to have a fast computation
NB_DOCS_SMALL_INFER = 90000  # You need a larger one in script 03 to have various days in your small version

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


def format_npz_output(save_path, size):
    return save_path.replace(".npz", "_" + str(size) + ".npz")


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
            members = [m for m in tar.getmembers() if m.isreg()]
            if members == []:
                raise ValueError(f"Tar archive {root} doesn't contain files")
            loop = tqdm(
                sorted(members, key=lambda x: x.name),
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


def generate_threads(
    file,
    filename,
    compressed,
    tar,
    empty_warn,
    counters,
    apply_unidecode,
    small,
    small_size,
    party_day_counts,
    metadata=False,
):
    thread_ids = dict()
    threads = dict()

    if compressed:
        filestream = io.TextIOWrapper(tar.extractfile(file))
    else:
        filestream = open(file)
    reader = casanova.reader(filestream)

    if reader.empty:
        empty_warn.append(filename)

    text_pos = reader.headers.text
    id_pos = reader.headers.id
    rt_pos = reader.headers.retweeted_id
    rt_count_pos = reader.headers.retweet_count
    user_pos = reader.headers.user_id
    to_user_pos = reader.headers.to_userid
    to_id_pos = reader.headers.to_tweetid
    local_time_pos = reader.headers.local_time
    screen_name_pos = reader.headers.user_screen_name

    for row in reader:
        counters["counter_all"] += 1
        if not row[rt_pos]:
            counters["counter_original"] += 1
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

    reader = casanova.reader(input_file)

    for row in reader:
        if not row[rt_pos]:
            doc_id = row[id_pos]
            if doc_id in threads:
                doc = row[text_pos] + " " + threads[doc_id]

            elif doc_id not in thread_ids:
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

                if small and counters["counter_threads"] >= small_size:
                    break
                counters["counter_threads"] += 1

                if metadata:
                    doc = {
                        "id": doc_id,
                        "local_time": row[local_time_pos],
                        "text": doc,
                        "user_screen_name": row[screen_name_pos],
                        "retweet_count": row[rt_count_pos],
                    }
                yield doc
    if not compressed:
        input_file.close()


def preprocess(
    root,
    nb_files,
    party_day_counts=None,
    apply_unidecode=False,
    write_files=False,
    small=False,
    small_size=NB_DOCS_SMALL_TRAIN,
):
    counters = {"counter_all": 0, "counter_original": 0, "counter_threads": 0}

    tar, loop, compressed = iter_on_files(root, nb_files)
    empty_warn = []
    for file in loop:
        counter_threads_file = counters["counter_threads"]
        if compressed:
            filename = file.name
        else:
            filename = file

        loop.set_description(filename)

        file_date = os.path.basename(filename)[:10]

        group_name = grep_group_name(filename)

        for thread in generate_threads(
            file,
            filename,
            compressed,
            tar,
            empty_warn,
            counters,
            apply_unidecode,
            small,
            small_size,
            party_day_counts,
        ):
            yield thread
        if party_day_counts is not None:
            if group_name != "":
                party_day_counts.append(
                    (
                        counters["counter_threads"] - counter_threads_file,
                        group_name,
                        file_date,
                    )
                )
            else:
                party_day_counts.append(
                    (counters["counter_threads"] - counter_threads_file, file_date)
                )

        if small and counters["counter_threads"] >= small_size:
            break

    if compressed:
        tar.close()
    print(
        "nb of tweets: {}, nb of original tweets: {}, nb of original tweets grouped by threads: {}\n".format(
            counters["counter_all"],
            counters["counter_original"],
            counters["counter_threads"],
        )
    )
    if empty_warn:
        print("The following files are empty:")
        for f in empty_warn:
            print(f)
        print()


def load_embeddings(path, save_size, nb_docs, resume_encoding=False, small=False):
    max_index = 0
    embeddings = (
        np.empty((save_size, EMB_DIMENSION))
        if resume_encoding
        else np.empty((nb_docs, EMB_DIMENSION))
    )

    if small:
        # In the --small case, return only the first npz file
        file = format_npz_output(path, save_size)
        return None, np.load(file)["embeddings"][:nb_docs]

    for file in glob.glob(path.replace(".npz", "_*")):
        index = int(file[len(path) - 3 : -len(".npz")])
        if index > max_index:
            max_index = index

        if not resume_encoding:
            if index % save_size == 0:
                embeddings[index - save_size : index] = np.load(file)["embeddings"]
            else:
                embeddings[
                    embeddings.shape[0] - (embeddings.shape[0] % save_size) :
                ] = np.load(file)["embeddings"]
    if not resume_encoding:
        print(
            "Loaded {} previously encoded rows".format(np.any(embeddings, axis=1).sum())
        )
    return max_index, embeddings


camembert_tokenizer = CamembertTokenizer.from_pretrained(SBERT_NAME)

vectorizer = CountVectorizer(
    stop_words=STOP_WORDS_FR,
    tokenizer=custom_tokenizer,
    max_features=75000,
    ngram_range=(1, 2),
    min_df=10,
)


def load_docs_embeddings(
    root_doc,
    nb_files,
    path_embed,
    save_size,
    party_day_counts=None,
    apply_unidecode=False,
    write_files=False,
    small=False,
    small_size=NB_DOCS_SMALL_TRAIN,
    resume_encoding=False,
):
    docs = np.array(
        [
            doc
            for doc in preprocess(
                root_doc,
                nb_files,
                party_day_counts,
                apply_unidecode,
                write_files,
                small,
                small_size,
            )
        ]
    )

    max_index, embeddings = load_embeddings(
        path_embed,
        save_size,
        docs.shape[0],
        resume_encoding,
        small,
    )

    return docs, max_index, embeddings


def extract_representative_docs(docs, topics, topic_model):
    documents_df = pd.DataFrame(
        {"Document": docs, "ID": range(len(docs)), "Topic": topics, "Image": None}
    )

    # Extract 10 representative docs per topic
    _, _, _, repr_docs_ids = topic_model._extract_representative_docs(
        c_tf_idf=topic_model.c_tf_idf_,
        documents=documents_df,
        topics=topic_model.topic_representations_,
        nr_samples=500,
        nr_repr_docs=10,
    )

    return repr_docs_ids


def write_ids_and_representative_docs(
    repr_docs_ids, party_day_counts, public, path, small, small_size
):
    doc_topic_pairs = []
    for topic_id, doc_ids in enumerate(repr_docs_ids):
        for doc_id in doc_ids:
            doc_topic_pairs.append((doc_id, topic_id - 1))
    doc_topic_pairs = sorted(doc_topic_pairs)

    with open(
        os.path.join(
            path,
            "data_prod",
            "dashboard",
            "bertopic",
            f"representative_docs_{public}.csv",
        ),
        "w",
    ) as f_representative_docs, open(
        os.path.join(
            path, "data_prod", "dashboard", "bertopic", f"ids_topics_{public}.csv"
        ),
        "w",
    ) as f_ids:
        writer = csv.writer(f_representative_docs)
        writer.writerow(
            [
                "id",
                "local_time",
                "text",
                "user_screen_name",
                "topic",
                "party",
            ]
        )

        writer_ids = csv.writer(f_ids)
        writer_ids.writerow(
            [
                "id",
                "topic",
            ]
        )

        doc_index = 0
        doc_topic_pairs_index = 0
        counters = {"counter_all": 0, "counter_original": 0, "counter_threads": 0}
        doc_count_sum = 0
        for row in tqdm(party_day_counts, desc="Write representative documents"):
            if len(row) == 3:
                doc_count, party, day = row
                file_path = os.path.join(
                    path, "data_source", public, party, f"{day}.csv"
                )
            else:
                party = ""
                doc_count, day = row
                file_path = os.path.join(path, "data_source", public, f"{day}.csv")

            doc_count_sum += doc_count

            # Skip file if no representative doc inside
            if doc_topic_pairs[doc_topic_pairs_index][0] > doc_count_sum:
                doc_index += doc_count
                continue

            for doc in generate_threads(
                file_path,
                f"{day}.csv",
                compressed=False,
                tar=None,
                empty_warn=[],
                counters=counters,
                apply_unidecode=False,
                small=small,
                small_size=small_size,
                party_day_counts=party_day_counts,
                metadata=True,
            ):
                if doc_topic_pairs_index >= len(doc_topic_pairs):
                    return
                doc_id, topic = doc_topic_pairs[doc_topic_pairs_index]

                if int(doc["retweet_count"]) > 0:
                    writer_ids.writerow([doc["id"], topic])

                if doc_index == doc_id:
                    writer.writerow(
                        [
                            doc["id"],
                            doc["local_time"],
                            doc["text"],
                            doc["user_screen_name"],
                            topic,
                            party,
                        ]
                    )
                    doc_topic_pairs_index += 1
                doc_index += 1


def count_topics_info(topics, party_day_counts, group_type):
    """
    party_day_count is a list with the following structure:
    [
        (29, 'lr', '2022-06-20'),
        (46, 'lr', '2022-06-21'),
        (13, 'lr', '2022-06-22'),
        (17, 'lr', '2022-06-23'),
        ...
    ]
    """

    file_index = 0

    if group_type == "supporter" or group_type == "congress":
        doc_count, party, day = party_day_counts[file_index]
        topics_info = defaultdict(lambda: defaultdict(lambda: defaultdict(int)))
        doc_count_sum = doc_count
        for i, topic in enumerate(topics):
            while i >= doc_count_sum:
                file_index += 1
                doc_count, party, day = party_day_counts[file_index]
                doc_count_sum += doc_count

            topics_info[topic][party][day] += 1

    else:
        doc_count, day = party_day_counts[file_index]
        topics_info = defaultdict(lambda: defaultdict(int))
        doc_count_sum = doc_count
        for i, topic in enumerate(topics):
            while i >= doc_count_sum:
                file_index += 1
                doc_count, day = party_day_counts[file_index]
                doc_count_sum += doc_count

            topics_info[topic][day] += 1

    return topics_info


def write_bertopic_TS(topics, topics_info, group_type, party_day_counts, origin_path):
    for topic in tqdm(topics, desc="Write time series"):
        with open(
            os.path.join(
                origin_path,
                "data_prod",
                "dashboard",
                "bertopic",
                "data",
                f"bertopic_ts_{topic}.csv",
            ),
            "w" if group_type == "congress" else "a",
        ) as f:
            writer = csv.writer(f)
            if group_type == "congress":
                writer.writerow(["date", "party", "topic", "prop", "nb_tweets"])
            if group_type == "supporter" or group_type == "congress":
                for doc_count, party, day in party_day_counts:
                    writer.writerow(
                        [
                            day,
                            f"{party}_supp" if group_type == "supporter" else party,
                            topic,
                            round(topics_info[topic][party][day] / (doc_count), 5),
                            topics_info[topic][party][day] 
                        ]
                    )
            else:
                for doc_count, day in party_day_counts:
                    writer.writerow(
                        [
                            day,
                            group_type,
                            topic,
                            round(topics_info[topic][day] / (doc_count), 5),
                            topics_info[topic][day] 
                        ]
                    )
