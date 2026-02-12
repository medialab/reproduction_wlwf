# xan version: 0.54.0

DATA_PATH=$1

echo "public,count,sum,mean,median" > descriptive_stats_per_user_with_rt.csv
echo "public,mean,median" > descriptive_stats_per_day_with_rt.csv

# congress

for group in ensemble rn lr nupes ;
do
    echo "congress $group"
    xan parallel groupby user_id 'count() as tweet_count' $DATA_PATH/data_source/congress/$group/*.csv | \
    xan stats -q -s tweet_count | \
    xan transform mean,median 'round' | \
    xan map "'$group congress' as public" | \
    xan transform count,sum,mean,median 'replace(numfmt(_), ",", " ")' | \
    xan select -- -1,count,sum,mean,median | \
    xan behead >> descriptive_stats_per_user_with_rt.csv ;

    xan parallel cat $DATA_PATH/data_source/congress/$group/*.csv -P "map 'local_time[:10] as day'" | \
    xan groupby day 'count() as tweet_count' | \
    xan stats -q -s tweet_count | \
    xan transform mean,median 'round' | \
    xan map "'$group congress' as public" | \
    xan transform mean,median 'replace(numfmt(_), ",", " ")' | \
    xan select -- -1,mean,median | \
    xan behead >> descriptive_stats_per_day_with_rt.csv ;

done

echo "total congress"
xan parallel groupby user_id 'count() as tweet_count' $DATA_PATH/data_source/congress/*/*.csv | \
xan stats -q -s tweet_count | \
xan transform mean,median 'round' | \
xan map "'total congress' as public" | \
xan transform count,sum,mean,median 'replace(numfmt(_), ",", " ")' | \
xan select -- -1,count,sum,mean,median | \
xan behead >> descriptive_stats_per_user_with_rt.csv ;

xan parallel cat $DATA_PATH/data_source/congress/*/*.csv -P "map 'local_time[:10] as day'" | \
xan groupby day 'count() as tweet_count' | \
xan stats -q -s tweet_count | \
xan transform mean,median 'round' | \
xan map "'total congress' as public" | \
xan transform mean,median 'replace(numfmt(_), ",", " ")' | \
xan select -- -1,mean,median | \
xan behead >> descriptive_stats_per_day_with_rt.csv ;

# supporters

for group in ensemble rn lr nupes ;
do
    echo "supporters $group"
    xan parallel groupby user_id 'count() as tweet_count' $DATA_PATH/data_source/supporter/$group/*.csv | \
    xan stats -q -s tweet_count | \
    xan transform mean,median 'round' | \
    xan map "'supporters $group' as public" | \
    xan transform count,sum,mean,median 'replace(numfmt(_), ",", " ")' | \
    xan select -- -1,count,sum,mean,median | \
    xan behead >> descriptive_stats_per_user_with_rt.csv ;

    xan parallel cat $DATA_PATH/data_source/supporter/$group/*.csv -P "map 'local_time[:10] as day'" | \
    xan groupby day 'count() as tweet_count' | \
    xan stats -q -s tweet_count | \
    xan transform mean,median 'round' | \
    xan map "'supporters $group' as public" | \
    xan transform mean,median 'replace(numfmt(_), ",", " ")' | \
    xan select -- -1,mean,median | \
    xan behead >> descriptive_stats_per_day_with_rt.csv ;

done

echo "total supporters"
xan parallel groupby user_id 'count() as tweet_count' $DATA_PATH/data_source/supporter/*/*.csv | \
xan stats -q -s tweet_count | \
xan transform mean,median 'round' | \
xan map "'total supporters' as public" | \
xan transform count,sum,mean,median 'replace(numfmt(_), ",", " ")' | \
xan select -- -1,count,sum,mean,median | \
xan behead >> descriptive_stats_per_user_with_rt.csv ;

xan parallel cat $DATA_PATH/data_source/supporter/*/*.csv -P "map 'local_time[:10] as day'" | \
xan groupby day 'count() as tweet_count' | \
xan stats -q -s tweet_count | \
xan transform mean,median 'round' | \
xan map "'total supporters' as public" | \
xan transform mean,median 'replace(numfmt(_), ",", " ")' | \
xan select -- -1,mean,median | \
xan behead >> descriptive_stats_per_day_with_rt.csv ;

# media & attentive

for public in attentive media ;
do
    echo $public ;
    xan parallel groupby user_id 'count() as tweet_count' $DATA_PATH/data_source/$public/*.csv | \
    xan stats -q -s tweet_count | \
    xan transform mean,median 'round' | \
    xan map "'$public' as public" | \
    xan transform count,sum,mean,median 'replace(numfmt(_), ",", " ")' | \
    xan select -- -1,count,sum,mean,median | \
    xan behead >> descriptive_stats_per_user_with_rt.csv

    xan parallel cat $DATA_PATH/data_source/$public/*.csv -P "map 'local_time[:10] as day'" | \
    xan groupby day 'count() as tweet_count' | \
    xan stats -q -s tweet_count | \
    xan transform mean,median 'round' | \
    xan map "'$public' as public" | \
    xan transform mean,median 'replace(numfmt(_), ",", " ")' | \
    xan select -- -1,mean,median | \
    xan behead >> descriptive_stats_per_day_with_rt.csv ;
done

xan join public descriptive_stats_per_user_with_rt.csv descriptive_stats_per_day_with_rt.csv --prefix-right "daily group " > descriptive_stats_with_rt.csv

rm descriptive_stats_per_user_with_rt.csv descriptive_stats_per_day_with_rt.csv
xan view --right "*" -I descriptive_stats_with_rt.csv