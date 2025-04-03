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


check_matrix_dimensions <- function(mat, expected_rows, expected_cols) {
  if (nrow(mat) != expected_rows | ncol(mat) != expected_cols) {
    stop(sprintf("La matrice a des dimensions incorrectes : attendu %d lignes et %d colonnes, mais obtenu %d lignes et %d colonnes.",
                 expected_rows, expected_cols, nrow(mat), ncol(mat)))
  }
}


K <- 100

#Plots 
topwords <-scan("data_prod/topics/lda-python/topwords.txt", what="character", sep="\n")
words <- topwords[!seq_along(topwords) %% 16 == 1]  # On saute chaque 16ème élément
n.topics <- rep(seq(1, K, 1), each=15)
ranking <-  rep(seq(15, 1, -1), times = K)

info.df <- data.frame(topic = n.topics, order = ranking, word = words)

info.df$topic <- paste0("Topic ", info.df$topic)
info.df$topic <- factor(info.df$topic, levels=paste0("Topic ", 1:K))

#topic, rank, work
# generating figures
cat("building one figure of top words for each topic\n")

for (k in 1:K){

	cat(k, '\n')

	df <- info.df[info.df$topic==paste0('Topic ', k),]

	p <- ggplot(data=df,
		aes(x= 1, y=order, label=word))
	pq <- p + geom_text(hjust=1, size=4)  +
    	scale_size_continuous(range=c(3,5)) +
    	scale_y_discrete("Top 10 scoring words for each topic",
        	limit= rev(levels(df$order))) +
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
    	geom_vline(xintercept = .75, color="grey80", linetype=7) +
    	geom_hline(yintercept = 0.5, color="grey80", linetype=7)

	pq

	ggsave(pq, file=paste0("data_prod/dashboard/lda/img/words-plot-", k, '.png'), height=3.5, width=3, create.dir = TRUE)

}

#TS

#getting list of dates
fls <- scan("data_prod/dfm/congress-day-party-list.txt", what = "character", sep = "\n") # liste des partis-dates préalablement extraites des noms de fichier

dates <- fls |>
  str_extract("\\d{4}-\\d{2}-\\d{2}")

# parties <- fls |>
#   str_extract("[^/]*") |> # tout ce qui précède "/"
#   unique()

parties <- c("dep. lr", "dep. majo.", "dep. nupes", "dep. rn")

# ## computing differences across parties
congress <- read_csv("data_prod/topics/lda-python/results-congress.csv", show_col_types = FALSE)
suppressMessages({
congress <- congress |>  # probabilité partis / jour en ligne, topics en colonne
  # À VERIFIER !!!!!!!!!!!!!
  # /!\ Est-ce que l'ordre des lignes du lda.fit@gamma est bien le même que l'ordre dans fls ? /!\
  as_tibble() |> 
  # passage en format tidy
  pivot_longer(everything(),
               names_to = "topic",
               names_pattern = "(\\d+)", # récupère les chiffres dans les noms de colonnes
               values_to = "prop") |>  
  mutate(date = rep(as.Date(dates), each = K),
         actor = rep(.env$parties, each = K*length(dates)/length(.env$parties)), # /!\ Attention à la diff entre times et each /!\
         ) |> 
  relocate(c(topic, prop), .after = "actor") |> 
  arrange(topic, date, actor)
})
# chaque jour, la somme du "score d'attention" de chaque acteur réparti entre les différents topics vaut 1
# verif
cat("check the coherence of congress time-series df\n")
suppressMessages({
congress |> summarise(s = sum(prop), .by = date) # la somme des probas chaque jour vaut 4 : 1 pour chaque parti
congress |> summarise(s = sum(prop), .by = actor) # la somme des probas pour chaque parti vaut 268 (1 par jour)
congress |> summarise(m = mean(prop), .by = actor)
})


build_ts <- function(
                     actor_ldafile_naming,
                     actor_label, 
                     path = "data_prod/topics/lda-python/",
                     first_date = "2022-06-20", 
                     last_date = "2023-03-14"
                     ){

# charge le fichier lda
  results <- read_csv(paste0("data_prod/topics/lda-python/results-", actor_ldafile_naming, ".csv"), show_col_types = FALSE)
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


# >> attentive public ----
attentive <- build_ts(
  "attentive",
  "pub. attentif"
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
results <- read_csv("data_prod/topics/lda-python/results-congress_rs.csv", show_col_types = FALSE)
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
  choices <- tail(order(as.numeric(pull(results, k))), n = 10)
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
rs <- read_csv("data_prod/dashboard/bertopic/representative_docs_congress.csv", show_col_types = FALSE)

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
results <- read_csv("data_prod/topics/lda-python/results-media_rs.csv", show_col_types = FALSE)
#
# # deleting duplicated tweets
duplicated <- which(duplicated(tweets$text))
tweets <- tweets[-duplicated,]
results <- results[-duplicated,]

K <- 100
rs <- list()

for (k in 1:K){
  choices <- tail(order(as.numeric(pull(results, k))), n = 10)
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

rs <- read_csv("data_prod/dashboard/bertopic/representative_docs_media.csv", show_col_types = FALSE)

rs$embed <- NA
for (i in 1:nrow(rs)){
  
  rs$embed[i] <- tw.embed(rs$text[i], name = rs$user_screen_name[i], screen_name = rs$user_screen_name[i], id_str = rs$id[i],
                          dt = rs$local_time[i], "")
  
}

rep_docs_media <- rs

save(rep_docs_media, file="data_prod/dashboard/bertopic/representative_docs_media.rdata")


