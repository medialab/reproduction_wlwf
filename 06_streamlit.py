import streamlit as st
import streamlit.components.v1 as components
import pandas as pd
import os
import argparse
import requests
import random
import plotly.express as px

from utils import existing_dir_path

color_palette = {
    "ensemble": "#ffd16a",
    "lr": "#0068C9",
    "nupes": "#ff2b2b",
    "rn": "#6d3fc0",
    "media": "#29b09d",
    "ensemble_supp": "#ff8700",
    "lr_supp": "#83c9ff",
    "nupes_supp": "#ffabab",
    "rn_supp": "mediumpurple",
    "attentive": "#d5dae5",
}

public_types = {
    "députés": ["ensemble", "lr", "nupes", "rn"],
    "supporters": ["ensemble_supp", "lr_supp", "nupes_supp", "rn_supp"],
    "médias": ["media"],
    "public attentif": ["attentive"],
}

display_strings = {"congress": "députés", "media": "médias"}

parser = argparse.ArgumentParser()

parser.add_argument(
    "--origin_path",
    help="Path to an origin of your code",
    type=existing_dir_path,
    default=os.getcwd(),
)

args = parser.parse_args()

input_path = os.path.join(args.origin_path, "data_prod", "dashboard", "bertopic")


df = pd.DataFrame()
for public in ["media", "congress"]:
    representative_docs = pd.read_csv(
        os.path.join(input_path, f"representative_docs_{public}.csv")
    )
    representative_docs["public"] = public
    df = pd.concat((df, representative_docs))
df["text_length"] = df.text.apply(len)


@st.cache_data(ttl=3600)
def tweet_exists(username, tweet_id):
    render = random.choices([1, 0], weights=[0.8, 0.2])
    if render:
        url = "https://publish.twitter.com/oembed"
        params = {"url": f"https://twitter.com/{username}/status/{tweet_id}"}
        r = requests.get(url, params=params, timeout=10)
        return r.status_code == 200
    return False


@st.cache_data(ttl=3600)
def render_tweets(df, height=1200):
    columns = []
    for public, group in df.groupby("public"):
        blocks = []
        for _, row in group.iterrows():
            if tweet_exists(row["user_screen_name"], row["id"]):
                blocks.append(f"""
                <blockquote class="twitter-tweet" data-conversation="none" data-cards="hidden" data-dnt="true" width=350>
                    <a href="https://twitter.com/{row["user_screen_name"]}/status/{row["id"]}"></a>
                </blockquote>
                """)
            else:
                blocks.append(f"""
                <div class="tweet-fallback">
                    <strong>@{row["user_screen_name"]}</strong><br><br>
                    {row["text"]}<br><br>
                    <a target=”_blank” href="https://twitter.com/{row["user_screen_name"]}/status/{row["id"]}">
                        {row["local_time"].replace("T", " ")}
                    </a>
                </div>
                """)
        columns.append(f"""
        <div class="tweet-column">
            <h4>Tweets représentatifs des {display_strings[public]}</h3>
            {"".join(blocks)}
        </div>
        """)

    html = f"""
    <html>
    <head>
        <style>
            body {{
                margin: 0;
                padding: 0;
                font-family: system-ui, -apple-system, BlinkMacSystemFont;
            }}

            .tweet-fallback {{
                border: 1px solid #ddd;
                border-radius: 12px;
                padding: 12px;
                margin-bottom: 20px;
                background-color: #fafafa;
                font-size: 14px;
            }}

            .grid {{
                display: grid;
                grid-template-columns: repeat(2, 1fr);
                gap: 24px;
                padding: 12px;
                align: left;
            }}

            .tweet-column h3 {{
                text-align: center;
                margin-bottom: 16px;
            }}

        </style>
    </head>
    <body>
        <div class="grid">
            {"".join(columns)}
        </div>
        <!-- Load Twitter widgets ONCE -->
        <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
    </body>
    </html>
    """

    components.html(html, height=height, scrolling=True, width=700)


with st.sidebar:
    selected_topic = st.selectbox(
        "Choisir le topic à afficher",
        sorted(df.topic.astype(int).unique(), reverse=True),
    )
    st.image(
        os.path.join(input_path, "img", f"bertopic_{selected_topic}.png"),
        caption=f"Mots représentatifs du topic {selected_topic}",
    )
with st.container():
    ts_data = pd.read_csv(
        os.path.join(input_path, "data", f"bertopic_ts_{selected_topic}.csv")
    )
    ts_data["date"] = pd.to_datetime(ts_data["date"])
    # st.line_chart(ts_data, x="date", y="prop", color="party")
    options = list(public_types.keys())

    selection = st.pills("", options, selection_mode="multi", default="députés")
    selected_publics = []
    for sel in selection:
        selected_publics.extend(public_types[sel])
    ts_data = ts_data[ts_data.party.isin(selected_publics)]

    fig = px.line(
        ts_data,
        x="date",
        y="prop",
        color="party",
        height=300,
        color_discrete_sequence=list(color_palette.values()),
        category_orders={"party": list(color_palette.keys())},
    )
    fig.update_layout(
        legend=dict(
            orientation="h",
            yanchor="bottom",
            y=-0.5,
            xanchor="center",
            x=0.5,
            title=None,
        )
    )
    fig.update_xaxes(title=None)

    st.plotly_chart(
        fig,
        theme="streamlit",
        use_container_width=True,
        config={"displayModeBar": False},
    )

render_tweets(df[df.topic == selected_topic].sort_values("text_length"), height=1200)
