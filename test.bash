variables=("lr" "majority" "nupes" "rn" "lr_supp" "majority_supp" "nupes_supp" "rn_supp" "attentive" "media" "general")

# Boucle sur chaque variable
for var in "${variables[@]}"
do
    # Exécuter la commande pour chaque variable
    xan filter "topic >= 0" data_prod/var/bertopic/general_TS_reduced.csv | xan groupby topic "stddev($var)" | xan top -R "stddev($var)" | xan v
done
