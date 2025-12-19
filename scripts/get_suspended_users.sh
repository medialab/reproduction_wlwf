# Use the undocumented Twitter API endpoint cdn.syndication.twimg.com to get info about suspended users
# xan version 0.54.1
# minet version


pyenv activate minet

xan cat rows /store/medialex/v3_data_reproduction_wlwf/data_source/attentive/*.csv | \
xan dedup -s user_id | \
xan select user_id,user_name,user_screen_name,user_followers,user_description,id | \
xan map 'concat("https://cdn.syndication.twimg.com/tweet-result?id=",id,"&token=123") as frame_url' | \
minet fetch frame_url -i - | \
xan select user_id,user_name,user_screen_name,user_followers,user_description,frame_url,mimetype,path \
> attentive_users_fetch_report.csv

xan search -s mimetype "application/json" attentive_users_fetch_report.csv | \
xan map -p '"downloaded".pjoin(path).read_json().tombstone as json' | \
xan search -s json "suspended account" | \
xan map 'json.parse_json().text as json_text' > attentive_users_suspended.csv