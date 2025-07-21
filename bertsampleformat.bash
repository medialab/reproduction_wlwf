xan map '"supporter"' id data_prod/topics/bert-sample/sample_supporter.csv -o data_prod/topics/bert-sample/sample_supporter_concat.csv
xan map '"media"' id data_prod/topics/bert-sample/sample_media.csv -o data_prod/topics/bert-sample/sample_media_concat.csv
xan map '"attentive"' id data_prod/topics/bert-sample/sample_attentive.csv -o data_prod/topics/bert-sample/sample_attentive_concat.csv
xan map '"congress"' id data_prod/topics/bert-sample/sample_congress.csv -o data_prod/topics/bert-sample/sample_congress_ID.csv

xan cat rows data_prod/topics/bert-sample/sample_*_concat.csv -o data_prod/topics/bert-sample/sample_predict_ID.csv