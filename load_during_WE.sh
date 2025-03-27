echo "START DFM : media"
python 01-create-dtm.py media /store/medialex/reproduction_wlwf/data_source/media/
echo "supp"
python 01-create-dtm.py supporter /store/medialex/reproduction_wlwf/data_source/supporter/
echo "attentive"
python 01-create-dtm.py attentive /store/medialex/reproduction_wlwf/data_source/attentive/
echo "general"
python 01-create-dtm.py general /store/medialex/reproduction_wlwf/data_source/general/

echo "CHOOSE TOPIC NUMBER"
Rscript 02-choosing-number-topics.r

echo "Start running LDA"
Rscript 03-running-lda.r

echo "Generating summary for dashboard"
Rscript 04a-dashboard-generating-summaries_lda.r