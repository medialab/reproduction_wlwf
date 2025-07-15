t
suppressPackageStartupMessages({
library(tidyverse)
library(topicmodels)
library(reshape)
library(ggplot2)
library(gtable)
library(gridExtra)
library(scales)
library(ggdendro)
library(ggthemes)
library(slam)
library(Matrix)
library(tm)
})

tw.embed <- function(text, name, screen_name, id_str, created_at, dt, js=FALSE){
    txt <- paste0('<blockquote class="twitter-tweet" data-cards="hidden" data-conversation="none" width="450"><p>',
        text, '</p> ', name, " (@", screen_name,
        ") <a href='https://twitter.com/", screen_name,
        '/status/', id_str, "'>",
        dt, '</a></blockquote>')
    if (js){
        txt <- paste0(txt,
            ' <script src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')
    }
    return(txt)
}

# preparing embed ----
#For congress
rs <- read_csv("data_prod/topics/bert-sample/sample_congress_ID.csv")
rs$embed <- NA
for (i in 1:nrow(rs)){

  rs$embed[i] <- tw.embed(rs$tweet[i], name = rs$id[i], screen_name = rs$id[i], id_str = rs$id[i],
    dt = NA, "")

}
congress_rs_sample <- rs


print(head(congress_rs_sample,10))
save(congress_rs_sample, file="data_prod/dashboard/bertopic/congress-rs-sample-tweets.rdata")

#For predict
rs <- read_csv("data_prod/topics/bert-sample/sample_predict_ID.csv")
rs <- rs %>% filter(id != "general")
rs$embed <- NA
for (i in 1:nrow(rs)){

  rs$embed[i] <- tw.embed(rs$tweet[i], name = rs$id[i], screen_name = rs$id[i], id_str = rs$id[i],
    dt = NA, "")

}
predict_rs_sample <- rs %>% filter()

print(head(predict_rs_sample,10))
save(predict_rs_sample, file="data_prod/dashboard/bertopic/predict-rs-sample-tweets.rdata")
