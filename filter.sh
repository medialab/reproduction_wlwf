for file in "/store/medialex/reproduction_wlwf/data_source/attentive/"*".csv"
do 
    filename=$(basename "$file")
    cat $file | xan search --patterns "data_prod/key_words.txt"  -s text $file > "/home/fynch.meynent/storage/data_source/attentive/$filename" #Think to change root
    echo "Fichier $filename traité"
done