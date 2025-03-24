#===============================================================================
#  File-Name:	01-generating-summaries.R
#  Date:	May 6, 2019
#  Paper:   Who Leads? Who Follows? Measuring Issue Attention and Agenda Setting
#           by Legislators and the Mass Public Using Social Media Data
#  Journal: American Political Science Review
#  Authors: Pablo Barbera, Andreu Casas, Jonathan Nagler, Patrick J. Egan,
#           Richard Bonneau, John Jost, Joshua A. Tucker
#  Purpose: generate data for html pages in dashboard
#  Data In:
#           ./data/tweets/text/* & var/lda_results-twokenizer.Rdata &
#           var/lda-ouput/*
#===============================================================================


# PACKAGES
#===============================================================================
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load("topicmodels", "reshape", "ggplot2", "gtable", "gridExtra", "scales", "ggdendro", "ggthemes", "slam", "Matrix", "tm", "tidyverse", "khroma")

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


# DATA
#===============================================================================
load("data_prod/topics/lda_results-twokenizer.Rdata")

K <- 100

###############################################################################
### A) Computing top 10 words for each topic
###############################################################################

num.words <- 15
normalized.topics <- exp(lda.fit@beta) / rowSums(exp(lda.fit@beta)) # exp(lda.fit@beta) contient la matrice de probabilités associées
# aux mots (en colonne) dans les thèmes (en ligne). Les valeurs brutes sont probablement des log-probabilités, puisqu'elles sont négatives. 
#normalized-topics correspond alors à la matrice donnant pour chaque case : proba du mot dans le topic / somme des probas de chaque mot dans le topic 

calculate.specificity <- function(mod) {
  # 1. Vérification du type de l'objet 'mod'
  if (!inherits(mod, "LDA") &
      !inherits(mod, "CTM"))
    stop("mod object must inherit from LDA or CTM")
  # 2. Extraction des résultats du modèle
  terms <- posterior(mod)$terms # contient les termes de la matrice des probas
  topics <- posterior(mod)$topics

  # 3. Définition des dimensions
  Nwords <- ncol(terms)
  Ntopics <- ncol(topics)
  Ndocs <- nrow(topics)

  # 4. Calcul de la distribution des sujets (probabilité d'un sujet sur l'ensemble des documents)
  ptopic <- apply(topics, 2, sum) / Ndocs

  # 5. Calcul de la probabilité des mots pondérée par la distribution des sujets
  pwords <- apply(terms, 2, function(x)
    sum(x * ptopic)
    )

  # 6. numer : p of word in a topic * p of topic
  # Chaque élément de la matrice représente la contribution d'un mot à un sujet, pondérée par la probabilité d'apparition de ce sujet dans le corpus.
  numer <- terms * ptopic

  # 7. denom : proba de chaque mot répétée pour chaque sujet, pour avoir une matrice de même dimension que numer
  denom  <- matrix(pwords,
                   nrow = Ntopics,
                   ncol = Nwords,
                   byrow = TRUE)

  # 8. Spécificité = p word in each topic * p topic / overall p of words
  return(numer / denom)
}

normalized.words <- calculate.specificity(lda.fit)

scores <- apply(normalized.topics, 2, function(x)
    x * ( log(x + 1e-05) - sum(log(x + 1e-05))/length(x)) ) #Pour chaque mot, on multiplie sa probabilité d'être dans chaque topic 
    #par l'écart entre le log de sa proba  et de la moyenne des log-proba du mot parmi tous les topics 
    # 1e-05 est ajouté pour éviter les erreurs de 0 
    # Cela met donc en avant les mots qui ont des probas hétérogènes. Par ex, un mot très probable dans le topic 1 mais peu dans les autres aura un score + élevé 
    # Cette formule fait penser à un tf-idf  

colnames(scores) <- lda.fit@terms

# les 15 mots avec le score pseudo tf-idf le plus proche 
words <- apply(scores, 1, function(x)
        colnames(scores)[order(x, decreasing = TRUE)[1:num.words]])

f.scores <- apply(scores, 1, function(x)
        x[order(x, decreasing = TRUE)[1:num.words]])

n.topics <- rep(seq(1, K, 1), each=num.words)

# construction d'un df des mots avec les plus gros tf-idf pour chaque topic pour chaque Topic
info.df <- data.frame(
    topic = n.topics,
    word = c(words),
    score = c(f.scores),
    stringsAsFactors=F)

info.df$specificity <- NA
for (i in 1:length(info.df$topic)){
    info.df$specificity[i] <- normalized.words[info.df$topic[i], which(colnames(scores) %in% info.df$word[i])]
}
info.df$topic <- paste0("Topic ", info.df$topic)
info.df$topic <- factor(info.df$topic, levels=paste0("Topic ", 1:K))
info.df$order <- factor(order.topics <- rep(seq(1, num.words, 1), times=K), levels=as.character(15:1)) #Ajoute le rank en fonction du score pseudo tf-idf 


# generating figures
cat("building one figure of top words for each topic\n")

for (k in 1:K){

	cat(k, '\n')

	df <- info.df[info.df$topic==paste0('Topic ', k),]

	p <- ggplot(data=df,
		aes(y=order, x=specificity, label=word))
	pq <- p + geom_text(aes(size=score), hjust=1) +
    	scale_size_continuous(range=c(3,5)) +
    	scale_y_discrete("Top 10 scoring words for each topic",
        	expand=c(0.03, 0.1)) +
    	scale_x_continuous("Specificity of word to each topic",
        	limit=c(min(df$specificity)-.75, 1)) +
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

###############################################################################
### B) Computing time series for each topic ####
###############################################################################
cat("computing one time series data-frame for each topic\n")

### > congress ----
# getting list of dates
fls <- scan("data_prod/dfm/congress-day-party-list.txt", what = "character", sep = "\n") # liste des partis-dates préalablement extraites des noms de fichier

dates <- fls |>
  str_extract("\\d{4}-\\d{2}-\\d{2}")

# parties <- fls |>
#   str_extract("[^/]*") |> # tout ce qui précède "/"
#   unique()

parties <- c("dep. lr", "dep. majo.", "dep. nupes", "dep. rn")

# ## computing differences across parties
suppressMessages({
congress <- lda.fit@gamma |>  # probabilité partis / jour en ligne, topics en colonne
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

# verif

# visu

congress |>
  filter(topic == 1) |>
  ggplot() +
  aes(x = date, y = prop, color = actor) +
  geom_line() +
  scale_x_date(date_breaks = "month", date_labels = "%Y-%b") +
  labs(color = "",
       x = "",
       y = "Proportion de l'attention accordée au topic") +
  ggthemes::theme_clean() +
  theme(axis.text.x = element_text(size = rel(0.8)
                                   )
        ) +
  ggtitle("Topic 1")

### > posteriors actors ----

# vecteur des dates

# fonction de construction du df time series
build_ts <- function(
                     actor_ldafile_naming,
                     actor_label, 
                     path = "data_prod/topics/lda-output/",
                     first_date = "2022-06-20", 
                     last_date = "2023-03-14"
                     ){

# charge le fichier lda
  load(paste0(path, "lda-", actor_ldafile_naming, "-results.Rdata"))
# vecteur des dates
  dates <- seq(as.Date(first_date), as.Date(last_date), by = "day")
# construction du df
  results$topics |> 
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

#
# ###############################################################################
# ### C) Computing quantities of interest ####
# ###############################################################################
#
cat("computing quantites of interest for each topic\n")
# # topic proportions
load("data_prod/topics/lda_results-twokenizer.Rdata")

# calcul du niveau d'attention par jour moyen de chaque acteur pour chaque topic
# en moyenne, chaque jour, chaque acteur prête 1% de son attention à chaque topic.
# un score supérieur à 0.01 signifie que l'acteur donné a prêté une attention moyenne plus forte à ce topic
# qu'aux autre topics.

build_qois <- function(df){
  df |> 
    group_by(topic, actor) |> 
    mutate(topic = as.integer(topic)
    ) |> 
    summarise(mean_score_day = mean(prop)) |> 
    ungroup()
}

# build qois for each actor and concatenate them all into a single df.
suppressMessages({
qois <- congress |> 
  build_qois() |> 
  rbind(media |> build_qois()) |> 
  rbind(majority |> build_qois()) |> 
  rbind(lr |> build_qois()) |> 
  rbind(nupes |> build_qois()) |> 
  rbind(rn |> build_qois()) |> 
  rbind(attentive |> build_qois()) |> 
  rbind(general |> build_qois())

# transform to wide format

qois <- qois |> 
  mutate(actor = str_replace_all(actor, " ", "."),
         actor = tolower(actor)
         ) |> 
  pivot_wider(
    names_from = actor,
    names_glue = "{actor}_{.value}",
    values_from = mean_score_day
  )

# add some quantities

qois <- qois |> 
  rowwise() |> # group by line
  mutate(overall_mean_score_day = mean(c_across(ends_with("_day")))
         ) |> 
  ungroup() |> 
  mutate(
    across(
      ends_with("_day"),
     ~ rank(-.x, ties.method = "average"),
     .names = "topic_{str_replace(.col, '_.*', '')}_score_rank"
    )
  )
})
#
# # saving to disk
save(qois, file="data_prod/dashboard/qois.rdata")
#
# ###############################################################################
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
load('data_prod/topics/lda-output/lda-rs-results.Rdata')
#
# deleting duplicated tweets
duplicated <- which(duplicated(tweets$text))
tweets <- tweets[-duplicated,]
results$topics <- results$topics[-duplicated,]

# deleting weird tweets
#todelete <- grep('xss', tweets$text)
#tweets <- tweets[-todelete,]
#results$topics <- results$topics[-todelete,]
#
K <- 100
rs <- list()

for (k in 1:K){
  choices <- tail(order(results$topics[,k]),n=10)
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
# > for LDA
rs$embed <- NA
for (i in 1:nrow(rs)){

  rs$embed[i] <- tw.embed(rs$text[i], name = rs$user_screen_name[i], screen_name = rs$user_screen_name[i], id_str = rs$id[i],
    dt = rs$local_time[i], "")

}
congress_rs <- rs
save(congress_rs, file="data_prod/dashboard/congress-rs-tweets.rdata")

# for BERTOPIC
rs <- read_csv("dashboard/bertopic/representative_docs_congress.csv")

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
# > for LDA
rs$embed <- NA
for (i in 1:nrow(rs)){

  rs$embed[i] <- tw.embed(rs$text[i], name = rs$user_screen_name[i], 
    screen_name = rs$user_screen_name[i], id_str = rs$id[i], dt = rs$local_time[i], ""
    )

}

media_rs <- rs
save(media_rs, file="data_prod/dashboard/media-rs-tweets.rdata")

# > for BERTOPIC

# for BERTOPIC
rs <- read_csv("dashboard/bertopic/representative_docs_media.csv")

rs$embed <- NA
for (i in 1:nrow(rs)){
  
  rs$embed[i] <- tw.embed(rs$text[i], name = rs$user_screen_name[i], screen_name = rs$user_screen_name[i], id_str = rs$id[i],
                          dt = rs$local_time[i], "")
  
}

rep_docs_media <- rs

save(rep_docs_media, file="data_prod/dashboard/bertopic/representative_docs_media.rdata")
#
# file.remove("data_temp/all_media_IPG_tweets.csv",
#             "data_temp/all_deputesXVI_tweets.csv")
# ###############################################################################
# ### F) MCs using each topic the most
# ###############################################################################
#
# load('data_prod/topics/lda-output/lda-mcs-results.Rdata')
# mcs <- scan("data/dfm/mcs-list.txt", what='character')
#
# K <- 100
# topmcs <- list()
#
# for (k in 1:K){
#   choices <- tail(order(results$topics[,k]),n=5)
#   topmcs[[k]] <- data.frame(screenName = mcs[choices], stringsAsFactors=F)
#   topmcs[[k]]$topic <- k
# }
#
# topmcs <- do.call(rbind, topmcs)
# topmcs$order <- 1:nrow(topmcs)
# mcs <- read.csv("data/congress_data.csv", stringsAsFactors=F)
#
# topmcs <- merge(topmcs,
#   mcs[,c("screenName", "party", "state")], all.x=TRUE, sort=FALSE)
#
# topmcs <- topmcs[order(topmcs$order),]
# topmcs$text <- paste0(
#   ' <a href="http://www.twitter.com/', topmcs$screenName, '">@', topmcs$screenName, ' (',
#     topmcs$party, '-', topmcs$state, ')</a>')
#
# save(topmcs, file="dashboard/top-mcs.rdata")
#
