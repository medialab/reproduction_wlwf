for elem in lr majority nupes rn lr_supp majority_supp nupes_supp rn_supp attentive general media
do
    echo $elem
    #xan filter "$elem eq \"OK\"" data_prod/var/bertopic/issue-level/statio_details.csv | xan count | xan v
    #xan filter 'topic >=0' data_prod/var/bertopic/general_TS.csv | xan groupby topic --pivot lr,majority,nupes,rn,lr_supp,majority_supp,nupes_supp,rn_supp,attentive,general,media 'sum(cell)' | xan join --left topic - Topic data_prod/figures/translate_number_name/BERTOPIC_merged.csv | xan top -R $elem -l 3 | xan v
done
