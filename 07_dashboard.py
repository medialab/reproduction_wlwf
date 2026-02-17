import os
import altair as alt
from vega_datasets import data
import pandas as pd
from jinja2 import Environment, FileSystemLoader

INPUT_PATH = os.path.join("data_prod", "dashboard", "bertopic")
OUTPUT_PATH = os.path.join(INPUT_PATH, "site")
TEMPLATE_PATH = os.path.join(INPUT_PATH, "template")
JSON_PATH = os.path.join(OUTPUT_PATH, "json")

os.makedirs(JSON_PATH, exist_ok=True)


df = []
for public in ["media", "congress"]:
    d = pd.read_csv(os.path.join(INPUT_PATH, f"representative_docs_{public}.csv"))
    d["public"] = public
    df.append(d)

df = pd.concat(df)
df["text_length"] = df.text.apply(len)

merge = {82: 0, 50: 5, 60: 40}
for topic, parent in merge.items():
    df["topic"] = df["topic"].replace(topic, parent)

display_strings = {"media": "médias", "congress": "députés"}

chart = (
    alt.Chart(data.cars.url)
    .mark_point()
    .encode(x="Horsepower:Q", y="Miles_per_Gallon:Q", color="Origin:N")
)

chart.save(os.path.join(JSON_PATH, "data.json"))


def tweet_exists(username, tweet_id):
    return True
    # render = random.choices([1, 0], weights=[0.8, 0.2])
    # if render:
    #     url = "https://publish.twitter.com/oembed"
    #     params = {"url": f"https://twitter.com/{username}/status/{tweet_id}"}
    #     r = requests.get(url, params=params, timeout=10)
    #     return r.status_code == 200
    # return False


env = Environment(loader=FileSystemLoader(TEMPLATE_PATH))
template = env.get_template("dashboard.html")

for topic in sorted(df.topic.unique()):
    subset = df[df.topic == topic].sort_values("text_length")

    tweets = []
    for _, row in subset.iterrows():
        tweets.append(
            {
                "exists": tweet_exists(row.user_screen_name, row.id),
                "user": row.user_screen_name,
                "id": row.id,
                "text": row.text,
                "time": row.local_time.replace("T", " "),
                "public": row.public,
            }
        )

    all_topics = sorted(df.topic.unique())

    html = template.render(
        topic=topic,
        tweets=tweets,
        all_topics=all_topics,
        display_strings=display_strings,
    )

    output_file = os.path.join(OUTPUT_PATH, f"topic_{topic}.html")

    with open(output_file, "w", encoding="utf-8") as f:
        f.write(html)
