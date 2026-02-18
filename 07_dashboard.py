import os
import random
import requests
import pandas as pd
import altair as alt
from jinja2 import Environment, FileSystemLoader

INPUT_PATH = os.path.join("data_prod", "dashboard", "bertopic")
OUTPUT_PATH = os.path.join(INPUT_PATH, "site")
TEMPLATE_PATH = os.path.join(INPUT_PATH, "template")

df = []
for public in ["media", "congress"]:
    part = pd.read_csv(os.path.join(INPUT_PATH, f"representative_docs_{public}.csv"))
    part["public"] = public
    df.append(part)

df = pd.concat(df)
df["text_length"] = df.text.apply(len)

merge = {82: 0, 50: 5, 60: 40}
for topic, parent in merge.items():
    df["topic"] = df["topic"].replace(topic, parent)

display_strings = {"media": "médias", "congress": "députés"}


def tweet_exists(username, tweet_id):
    render = random.choices([1, 0], weights=[0.8, 0.2])
    if render:
        url = "https://publish.twitter.com/oembed"
        params = {"url": f"https://twitter.com/{username}/status/{tweet_id}"}
        r = requests.get(url, params=params, timeout=30)
        return r.status_code == 200
    return False


env = Environment(loader=FileSystemLoader(TEMPLATE_PATH))
template = env.get_template("dashboard.html")

all_topics = sorted(list(df.topic.unique()))
all_topics.remove(-1)

for topic in all_topics:
    ts_data = pd.read_csv(os.path.join(INPUT_PATH, "data", f"bertopic_ts_{topic}.csv"))
    ts_data["date"] = pd.to_datetime(ts_data["date"])
    ts_data["party"] = ts_data["party"].replace(
        [
            "lr",
            "rn",
            "ensemble",
            "nupes",
            "lr_supp",
            "rn_supp",
            "ensemble_supp",
            "nupes_supp",
            "media",
            "attentive",
        ],
        [
            "députés LR",
            "députés RN",
            "députés Ensemble",
            "députés NUPES",
            "supporters LR",
            "supporters RN",
            "supporters Ensemble",
            "supporters NUPES",
            "médias",
            "public attentif",
        ],
    )

    selection = alt.selection_point(
        fields=["party"], bind="legend", value="députés Ensemble", toggle="true"
    )
    # "ensemble": "#ffd16a",
    # "lr": "#0068C9",
    # "nupes": "#ff2b2b",
    # "rn": "#6d3fc0",
    # "media": "#29b09d",
    # "ensemble_supp": "#ff8700",
    # "lr_supp": "#83c9ff",
    # "nupes_supp": "#ffabab",
    # "rn_supp": "mediumpurple",
    # "attentive": "#d5dae5",
    chart = (
        alt.Chart(
            ts_data,
            title=alt.Title(
                f"Proportion d'attention accordée au topic {topic} par chacun des publics au cours du temps",
                subtitle="Il s'agit du nombre de tweets consacrés à ce topic rapporté au nombre de tweets émis le même jour par le même public.",
            ),
        )
        .configure_range(
            category=[
                "#ffd16a",
                "#ff2b2b",
                "#6d3fc0",
                "#0068C9",
                "#ff8700",
                "#ffabab",
                "mediumpurple",
                "#83c9ff",
                "#29b09d",
                "black",
            ]
        )
        .mark_line()
        .encode(
            x="date:T",
            y=alt.Y("prop:Q").axis(format="%").title("proportion d'attention"),
            color=alt.Color(
                "party:N",
                sort=[
                    "députés Ensemble",
                    "députés NUPES",
                    "députés RN",
                    "députés LR",
                    "supporters Ensemble",
                    "supporters NUPES",
                    "supporters RN",
                    "supporters LR",
                    "médias",
                    "attentive",
                ],
            ).legend(alt.Legend(title="Public")),
            opacity=alt.when(selection).then(alt.value(1)).otherwise(alt.value(0.01)),
        )
        .add_params(selection)
        .properties(width=1000, height=200)
    )

    json_chart = chart.to_json()
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

    html = template.render(
        topic=topic,
        tweets=tweets,
        all_topics=all_topics,
        display_strings=display_strings,
        json_chart=json_chart,
    )

    output_file = os.path.join(OUTPUT_PATH, f"topic_{topic}.html")

    with open(output_file, "w", encoding="utf-8") as f:
        f.write(html)
