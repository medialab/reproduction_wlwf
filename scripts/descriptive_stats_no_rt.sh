# xan version: 0.54.0

DATA_PATH=$1

echo "public,count,sum,mean,median" > descriptive_stats_per_user.csv
echo "public,mean,median" > descriptive_stats_per_day.csv

# congress

for group in majority rn lr nupes ;
do
    echo "congress $group"
    xan parallel groupby user_id 'count() as tweet_count' $DATA_PATH/data_source/congress/$group/*.csv -P "search -s retweeted_id -E" | \
    xan stats -q -s tweet_count | \
    xan transform mean,median 'round' | \
    xan map "'$group congress' as public" | \
    xan transform count,sum,mean,median 'replace(numfmt(_), ",", " ")' | \
    xan select -- -1,count,sum,mean,median | \
    xan behead >> descriptive_stats_per_user.csv ;

    xan parallel cat $DATA_PATH/data_source/congress/$group/*.csv -P "search -s retweeted_id -E | map 'local_time[:10] as day'" | \
    xan groupby day 'count() as tweet_count' | \
    xan stats -q -s tweet_count | \
    xan transform mean,median 'round' | \
    xan map "'$group congress' as public" | \
    xan transform mean,median 'replace(numfmt(_), ",", " ")' | \
    xan select -- -1,mean,median | \
    xan behead >> descriptive_stats_per_day.csv ;

done

echo "total congress"
xan parallel groupby user_id 'count() as tweet_count' $DATA_PATH/data_source/congress/*/*.csv -P "search -s retweeted_id -E" | \
xan stats -q -s tweet_count | \
xan transform mean,median 'round' | \
xan map "'total congress' as public" | \
xan transform count,sum,mean,median 'replace(numfmt(_), ",", " ")' | \
xan select -- -1,count,sum,mean,median | \
xan behead >> descriptive_stats_per_user.csv ;

xan parallel cat $DATA_PATH/data_source/congress/*/*.csv -P "search -s retweeted_id -E | map 'local_time[:10] as day'" | \
xan groupby day 'count() as tweet_count' | \
xan stats -q -s tweet_count | \
xan transform mean,median 'round' | \
xan map "'total congress' as public" | \
xan transform mean,median 'replace(numfmt(_), ",", " ")' | \
xan select -- -1,mean,median | \
xan behead >> descriptive_stats_per_day.csv ;

# supporters

for group in majority rn lr nupes ;
do
    echo "supporters $group"
    xan parallel groupby user_id 'count() as tweet_count' $DATA_PATH/data_source/supporter/$group/*.csv -P "search -s retweeted_id -E" | \
    xan stats -q -s tweet_count | \
    xan transform mean,median 'round' | \
    xan map "'supporters $group' as public" | \
    xan transform count,sum,mean,median 'replace(numfmt(_), ",", " ")' | \
    xan select -- -1,count,sum,mean,median | \
    xan behead >> descriptive_stats_per_user.csv ;

    xan parallel cat $DATA_PATH/data_source/supporter/$group/*.csv -P "search -s retweeted_id -E | map 'local_time[:10] as day'" | \
    xan groupby day 'count() as tweet_count' | \
    xan stats -q -s tweet_count | \
    xan transform mean,median 'round' | \
    xan map "'supporters $group' as public" | \
    xan transform mean,median 'replace(numfmt(_), ",", " ")' | \
    xan select -- -1,mean,median | \
    xan behead >> descriptive_stats_per_day.csv ;

done

echo "total supporters"
xan parallel groupby user_id 'count() as tweet_count' $DATA_PATH/data_source/supporter/*/*.csv -P "search -s retweeted_id -E" | \
xan stats -q -s tweet_count | \
xan transform mean,median 'round' | \
xan map "'total supporters' as public" | \
xan transform count,sum,mean,median 'replace(numfmt(_), ",", " ")' | \
xan select -- -1,count,sum,mean,median | \
xan behead >> descriptive_stats_per_user.csv ;

xan parallel cat $DATA_PATH/data_source/supporter/*/*.csv -P "search -s retweeted_id -E | map 'local_time[:10] as day'" | \
xan groupby day 'count() as tweet_count' | \
xan stats -q -s tweet_count | \
xan transform mean,median 'round' | \
xan map "'total supporters' as public" | \
xan transform mean,median 'replace(numfmt(_), ",", " ")' | \
xan select -- -1,mean,median | \
xan behead >> descriptive_stats_per_day.csv ;

# media & attentive

for public in attentive media ;
do
    echo $public ;
    xan parallel groupby user_id 'count() as tweet_count' $DATA_PATH/data_source/$public/*.csv -P "search -s retweeted_id -E" | \
    xan stats -q -s tweet_count | \
    xan transform mean,median 'round' | \
    xan map "'$public' as public" | \
    xan transform count,sum,mean,median 'replace(numfmt(_), ",", " ")' | \
    xan select -- -1,count,sum,mean,median | \
    xan behead >> descriptive_stats_per_user.csv

    xan parallel cat $DATA_PATH/data_source/$public/*.csv -P "search -s retweeted_id -E | map 'local_time[:10] as day'" | \
    xan groupby day 'count() as tweet_count' | \
    xan stats -q -s tweet_count | \
    xan transform mean,median 'round' | \
    xan map "'$public' as public" | \
    xan transform mean,median 'replace(numfmt(_), ",", " ")' | \
    xan select -- -1,mean,median | \
    xan behead >> descriptive_stats_per_day.csv ;
done

xan join public descriptive_stats_per_user.csv descriptive_stats_per_day.csv --prefix-right "daily group " > descriptive_stats_no_rt.csv

rm descriptive_stats_per_user.csv descriptive_stats_per_day.csv
xan view --right "*" -I descriptive_stats_no_rt.csv