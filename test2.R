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

K <- 100

#Plots 
topwords <-scan("data_prod/topics/lda-python/topwords.txt", what="character", sep="\n")
words <- topwords[!seq_along(topwords) %% 16 == 1]  # On saute chaque 16ème élément
n.topics <- rep(seq(1, K, 1), each=15)
ranking <-  rep(seq(1, 15, 1), times = K)

info.df <- data.frame(topic = n.topics, order = ranking, word = words)

# Afficher le data.frame
print(head(info.df, 30))
stop()

#topic, rank, work
# generating figures
cat("building one figure of top words for each topic\n")

for (k in 1:K){

	cat(k, '\n')

	df <- info.df[info.df$topic==paste0('Topic ', k),]

	p <- ggplot(data=df,
		aes(y=order, x=order, label=word))
	pq <- p + geom_text(aes(size=score), hjust=1) +
    	scale_size_continuous(range=c(3,5)) +
    	scale_y_discrete("Top 10 scoring words for each topic",
        	expand=c(0.03, 0.1)) +
    	scale_x_continuous("Specificity of word to each topic",
        	limit=c(min(df$order)-.75, 1)) +
    	theme_bw() +
    	theme(axis.line = element_blank(),
    	panel.grid.major = element_blank(),
    	panel.grid.minor = element_blank(),
    	panel.border = element_blank(),
    	panel.background = element_blank(),
    	axis.text = element_blank(),
    	axis.ticks = element_blank(),
    	axis.title.y = element_text(size = rel(.6)),
    	axis.title.x = element_text(size = rel(.6)),
    	legend.position = "none") +
    	geom_vline(xintercept = min(df$specificity)-.75, color="grey80", linetype=7) +
    	geom_hline(yintercept = 0.5, color="grey80", linetype=7)

	pq

	ggsave(pq, file=paste0("data_prod/dashboard/lda/img/words-plot-", k, '.png'), height=3.5, width=3, create.dir = TRUE)

}

build_ts <- function(
                     actor_ldafile_naming,
                     actor_label, 
                     path = "data_prod/topics/lda-output/",
                     first_date = "2022-06-20", 
                     last_date = "2023-03-14"
                     ){

# charge le fichier lda
  results <- read_csv(paste0("data_prod/topics/lda-python/results-", actor_ldafile_naming, ".csv")
# vecteur des dates
  dates <- seq(as.Date(first_date), as.Date(last_date), by = "day")
# construction du df
  results |>
  as_tibble() |> 
  # passage en format tidy
  pivot_longer(everything(),
               names_to = "topic",
               names_pattern = "(\\d+)", # get the numbers in the column-names
               values_to = "prop") |>  
  mutate(date = rep(dates, each = K),
         actor = actor_label,
  ) |> 
  mutate(topic = as.integer(topic)) |> 
  relocate(c(topic, prop), .after = "actor") |> 
    arrange(topic, date, actor)
}

# >> medias ----

media <- build_ts(
  actor_ldafile_naming = "media",
  actor_label = "medias"
  )


# >> supporters (majority) ----

majority <- build_ts(
  actor_ldafile_naming = "majority_supporters",
  actor_label = "sup. majo."
)

# >> supporters (lr) ----
lr <- build_ts(
  "lr_supporters",
  "sup. lr"
)

# >> supporters (nupes) ----
nupes <- build_ts(
  "nupes_supporters",
  "sup. nupes"
)


# >> supporters (rn) ----
rn <- build_ts(
  "rn_supporters",
  "sup. rn"
)

# >> general public ----

general <- build_ts(
  "general",
  "pub. general"
)

# créé le répertoire data si besoin
if (!dir.exists("data_prod/dashboard/lda/data")) {
  dir.create("data_prod/dashboard/lda/data",
             recursive = TRUE)
}

for (k in 1:K){
  sbs <- congress |>
    filter(topic == k) |> 
    rbind(media |> filter(topic == k)) |> 
    rbind(majority |> filter(topic == k)) |> 
    rbind(lr |> filter(topic == k)) |> 
    rbind(nupes |> filter(topic == k)) |> 
    rbind(rn |> filter(topic == k)) |> 
    rbind(attentive |> filter(topic == k)) |> 
    rbind(general |> filter(topic == k))
    
  # exports date
  write.csv(sbs, file=paste0("data_prod/dashboard/lda/data/ts-", k, '.csv'),
		row.names=FALSE, quote=FALSE)
}

# ### D) Representative tweets ----
# ###############################################################################
### >> Deputies ----
cat("selecting representative deputies' tweets for each topic\n")
#
suppressPackageStartupMessages(library(slam))
suppressPackageStartupMessages(library(Matrix))
suppressPackageStartupMessages(library(tm))

ind <- scan("data_prod/dfm/congress-rs-dtm-indices.txt")
pointers <- scan("data_prod/dfm/congress-rs-dtm-pointers.txt")
values <- scan("data_prod/dfm/congress-rs-dtm-values.txt")
words <- scan("data_prod/dfm/congress-words.txt", what="character", sep="\n")
tweets <- data.table::fread("data_prod/dfm/congress-rs-tweet-list.csv")
#
X <- sparseMatrix(j=ind, p=pointers, x=values,
   dims=c(
     as.numeric(nrow(tweets)), 
          length(words)
     ),
  index1=FALSE)
dimnames(X)[[2]] <- words

# check the dimensions of the matrix. Stop and give an error in case of incoherence
X |> check_matrix_dimensions(expected_rows =  nrow(tweets),
                             expected_cols = length(words)
)

# deleting empty rows
todelete <- which(rowSums(X)==0)
X <- X[-todelete,]
tweets <- tweets[-todelete,]
#
results <- read_csv("data_prod/topics/lda-python/results-congress_rs.csv")
#
# deleting duplicated tweets
duplicated <- which(duplicated(tweets$text))
tweets <- tweets[-duplicated,]
results <- results[-duplicated,]

# deleting weird tweets
#todelete <- grep('xss', tweets$text)
#tweets <- tweets[-todelete,]
#results$topics <- results$topics[-todelete,]
#
K <- 100
rs <- list()

for (k in 1:K){
  choices <- tail(order(results[,k]),n=10)
  rs[[k]] <- tweets[choices,]
  rs[[k]]$topic <- k
}
rs <- do.call(rbind, rs)

# function to display embedded tweet
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
# > for LDA ----
rs$embed <- NA
for (i in 1:nrow(rs)){

  rs$embed[i] <- tw.embed(rs$text[i], name = rs$user_screen_name[i], screen_name = rs$user_screen_name[i], id_str = rs$id[i],
    dt = rs$local_time[i], "")

}
congress_rs <- rs
save(congress_rs, file="data_prod/dashboard/lda/congress-rs-tweets.rdata")

# for BERTOPIC ----
rs <- read_csv("data_prod/dashboard/bertopic/representative_docs_congress.csv")

rs$embed <- NA
for (i in 1:nrow(rs)){
  
  rs$embed[i] <- tw.embed(rs$text[i], name = rs$user_screen_name[i], screen_name = rs$user_screen_name[i], id_str = rs$id[i],
                          dt = rs$local_time[i], "")
  
}

rep_docs_congress <- rs

save(rep_docs_congress, file="data_prod/dashboard/bertopic/representative_docs_congress.rdata")

#
### >> Medias ----
cat("selecting representative medias tweets for each topic\n")

ind <- scan("data_prod/dfm/media-rs-dtm-indices.txt")
pointers <- scan("data_prod/dfm/media-rs-dtm-pointers.txt")
values <- scan("data_prod/dfm/media-rs-dtm-values.txt")
words <- scan("data_prod/dfm/congress-words.txt", what="character", sep="\n")
tweets <- data.table::fread("data_prod/dfm/media-rs-tweet-list.csv")
#
X <- sparseMatrix(j=ind, p=pointers, x=values,
  dims=c(nrow(tweets), 
         length(words)), 
  index1=FALSE)
dimnames(X)[[2]] <- words

# check the dimensions of the matrix. Stop and give an error in case of incoherence
X |> check_matrix_dimensions(expected_rows =  nrow(tweets),
                             expected_cols = length(words)
)

# deleting empty rows
todelete <- which(rowSums(X)==0)
X <- X[-todelete,]
tweets <- tweets[-todelete,]
#
load('data_prod/topics/lda-output/lda-media-rs-results.Rdata')
#
# # deleting duplicated tweets
duplicated <- which(duplicated(tweets$text))
tweets <- tweets[-duplicated,]
results$topics <- results$topics[-duplicated,]

K <- 100
rs <- list()

for (k in 1:K){
  choices <- tail(order(results$topics[,k]),n=10)
  rs[[k]] <- tweets[choices,]
  rs[[k]]$topic <- k
}
rs <- do.call(rbind, rs)

# function to display embedded tweet
tw.embed <- function(text, name, 
  screen_name, id_str, created_at, dt, 
  js=FALSE){
  embed_html <- paste0('<blockquote class="twitter-tweet" data-cards="hidden" data-conversation="none" width="450"><p>',
         text, '</p> ', name, " (@", screen_name,
         ") <a href='https://twitter.com/", screen_name,
         '/status/', id_str, "'>",
         dt, '</a></blockquote>')
  #embed_html <- paste0(
  #  '<blockquote class="twitter-tweet" data-theme="light" data-conversation="none">',
  #  '<a href="https://twitter.com/', screen_name, '/status/', id_str, '"></a>',
  #  '</blockquote>'
  #)
    if (js){
        embed_hmtl <- paste0(embed_html,
        '<script src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')
        #'<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')
    }
    return(embed_html)
}

# preparing embed ----
# > for LDA ----
rs$embed <- NA
for (i in 1:nrow(rs)){

  rs$embed[i] <- tw.embed(rs$text[i], name = rs$user_screen_name[i], 
    screen_name = rs$user_screen_name[i], id_str = rs$id[i], dt = rs$local_time[i], ""
    )

}

media_rs <- rs
save(media_rs, file="data_prod/dashboard/lda/media-rs-tweets.rdata")

# > for BERTOPIC ----

rs <- read_csv("data_prod/dashboard/bertopic/representative_docs_media.csv")

rs$embed <- NA
for (i in 1:nrow(rs)){
  
  rs$embed[i] <- tw.embed(rs$text[i], name = rs$user_screen_name[i], screen_name = rs$user_screen_name[i], id_str = rs$id[i],
                          dt = rs$local_time[i], "")
  
}

rep_docs_media <- rs

save(rep_docs_media, file="data_prod/dashboard/bertopic/representative_docs_media.rdata")


