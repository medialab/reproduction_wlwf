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
    x * ( log(x + 1e-05) - sum(log(x + 1e-05))/length(x)) )

colnames(scores) <- lda.fit@terms

# les 15 mots avec le score de spécificité le plus élevé par topic ?
words <- apply(scores, 1, function(x)
        colnames(scores)[order(x, decreasing = TRUE)[1:num.words]])

f.scores <- apply(scores, 1, function(x)
        x[order(x, decreasing = TRUE)[1:num.words]])

n.topics <- rep(seq(1, K, 1), each=num.words)

# construction d'un df des mots les plus spécifiques pour chaque Topic
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
info.df$order <- factor(order.topics <- rep(seq(1, num.words, 1), times=K), levels=as.character(15:1))


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

	ggsave(pq, file=paste0("data_prod/dashboard/files/img/words-plot-", k, '.png'), height=3.5, width=3, create.dir = TRUE)

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

# chaque jour, la somme du "score d'attention" de chaque acteur réparti entre les différents topics vaut 1
# verif
cat("check the coherence of congress time-series df\n")
suppressMessages({
congress |> summarise(s = sum(prop), .by = date) # la somme des probas chaque jour vaut 4 : 1 pour chaque parti
congress |> summarise(s = sum(prop), .by = actor) # la somme des probas pour chaque parti vaut 268 (1 par jour)
congress |> summarise(m = mean(prop), .by = actor)
})
# time.series <- t(lda.fit@gamma) # transpose la matrice des probabilités document-sujet, pour avoir les topics en ligne et les partis / jours en colonnes

#congress <- data.frame(
#    date = rep(as.Date(dates), each = 100),
#    actor = rep(party, each = 100),
#    topic = rep(1:100, length(dates)),
#    prop = c(time.series # concatène
#             # la distribution des sujets pour chaque doc,
#                          # = le score moyen par jour de chaque topic.
#             ),
#    stringsAsFactors = F)

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
               names_pattern = "(\\d+)", # récupère les chiffres dans les noms de colonnes
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

#load("data_prod/topics/lda-output/lda-media-results.Rdata")
#media <- data.frame(
#  date = rep(dd, 100),
#  topic = rep(1:100,
#              each = length(dd)),
#  prop = c(results$topics), # on concatène les probabilités / jours / topics
#  stringsAsFactors=F) |> 
#  add_column(actor = "Medias", .after = "date")
#
#
# >> supporters (majority) ----

majority <- build_ts(
  actor_ldafile_naming = "majority_supporters",
  actor_label = "sup. majo."
)
# dd <- substr(users[grep('_democr', users)], 1, 10)
# load("data_prod/topics/lda-output/lda-majority_supporters-results.Rdata")
# majority <- data.frame(
#   date = rep(as.Date(dd), K),
#   topic = rep(1:K, each=nrow(results$topics)),
#   prop = c(results$topics), stringsAsFactors=F)|> 
#   add_column(actor = "supporters de la majorité", .after = "date")

# >> supporters (lr) ----
lr <- build_ts(
  "lr_supporters",
  "sup. lr"
)

# dd <- substr(users[grep('_repub', users)], 1, 10)
# load("data_prod/topics/lda-output/lda-lr_supporters-results.Rdata")
# lr <- data.frame(
#   date = rep(as.Date(dd), K),
#   topic = rep(1:K, each=nrow(results$topics)),
#   prop = c(results$topics), stringsAsFactors=F)|> 
#   add_column(actor = "supporters LR", .after = "date")

# >> supporters (nupes) ----
nupes <- build_ts(
  "nupes_supporters",
  "sup. nupes"
)

# load("data_prod/topics/lda-output/lda-nupes_supporters-results.Rdata")
# nupes <- data.frame(
#   date = rep(as.Date(dd), K),
#   topic = rep(1:K, each=nrow(results$topics)),
#   prop = c(results$topics), stringsAsFactors=F) |> # on concatène les probabilités / jours / topics
#   add_column(actor = "supporter NUPES", .after = "date")
#
# >> supporters (rn) ----
rn <- build_ts(
  "rn_supporters",
  "sup. rn"
)

# load("data_prod/topics/lda-output/lda-rn_supporters-results.Rdata")
# rn <- data.frame(
#   date = rep(as.Date(dd), K),
#   topic = rep(1:K, each=nrow(results$topics)),
#   prop = c(results$topics), stringsAsFactors=F) |> # récupère 
#   add_column(actor = "supporters RN", .after = "date")
#
# >> attentive public ----
attentive <- build_ts(
  "attentive",
  "pub. attentif"
)

# users <- scan("data_prod/dfm/users-list.txt", what='character')
# load("data_prod/topics/lda-output/lda-attentive-results.Rdata")
# # dd <- substr(users[grep('_public', users)], 1, 10)
# attentive <- data.frame(
#   date = rep(as.Date(dd), K),
#   topic = rep(1:K, each=nrow(results$topics)),
#   prop = c(results$topics), stringsAsFactors=F) |> 
#   add_column(actor = "Public attentif", .after = "date")

# >> general public ----

general <- build_ts(
  "general",
  "pub. general"
)
# load("data_prod/topics/lda-output/lda-general-results.Rdata")
# general <- data.frame(
#   date = rep(as.Date(dd), K),
#   topic = rep(1:K, each = nrow(results$topics)),
#   prop = c(results$topics), stringsAsFactors=F) |> 
#   add_column(actor = "Public général", .after = "date")

# créé le répertoire data si besoin
if (!dir.exists("data_prod/dashboard/files/data")) {
  dir.create("data_prod/dashboard/files/data",
             recursive = TRUE)
}

for (k in 1:K){
  # pour les graphiques de time series générés dans 04b, on produit un fichier "long" par topic
  sbs <- congress |>
    filter(topic == k) |> 
    rbind(media |> filter(topic == k)) |> 
    rbind(majority |> filter(topic == k)) |> 
    rbind(lr |> filter(topic == k)) |> 
    rbind(nupes |> filter(topic == k)) |> 
    rbind(rn |> filter(topic == k)) |> 
    rbind(attentive |> filter(topic == k)) |> 
    rbind(general |> filter(topic == k))
    # group_by(date, party) |>
    # summarise(prop = mean(prop)) |>
    # mutate(prop = round(prop, 3)) |>
    # ungroup() # |>
    # pivot_wider(names_from = party,
    #             names_prefix = "prop_",
    #             values_from = x)


	# sbs <- df[df$topic==k,]
	# sbs <- aggregate(sbs$prop, by=list(date=sbs$date, party=sbs$party), FUN=mean)
	# media
  # mm <- merge(data.frame(date=unique(sbs$date), stringsAsFactors=F),
  #   media[media$topic==k, c("date", "prop")], all.x=TRUE)
  # mm$prop[is.na(mm$prop)] <- 0

  #
  # # public
  # pp <- merge(data.frame(date=unique(sbs$date), stringsAsFactors=F),
  #   pub[pub$topic==k, c("date", "prop")], all.x=TRUE)
  # pp$prop[is.na(pp$prop)] <- 0
  # dd <- merge(data.frame(date=unique(sbs$date), stringsAsFactors=F),
  #   dem[dem$topic==k, c("date", "prop")], all.x=TRUE)
  # dd$prop[is.na(dd$prop)] <- 0
  # rr <- merge(data.frame(date=unique(sbs$date), stringsAsFactors=F),
  #   rep[rep$topic==k, c("date", "prop")], all.x=TRUE)
  # rr$prop[is.na(rr$prop)] <- 0
  #
  # ## random users
  # rnn <- merge(data.frame(date=unique(sbs$date), stringsAsFactors=F),
  #   rnd[rnd$topic==k, c("date", "prop")], all.x=TRUE)
  # rnn$prop[is.na(rnn$prop)] <- 0


#   sbs <- data.frame(
# 		date = unique(sbs$date),
# 		Democrats = round(sbs$x[sbs$party=="dem"], 3),
# 		Republicans = round(sbs$x[sbs$party=="rep"], 3),
#     Media = round(mm$prop, 3),
#     Public = round(pp$prop, 3),
#     Democratic_Supporters = round(dd$prop, 3),
#     Republican_Supporters = round(rr$prop, 3),
#     Random_Users= round(rnn$prop, 3))

	write.csv(sbs, file=paste0("data_prod/dashboard/files/data/ts-", k, '.csv'),
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
  dims=c(nrow(tweets), length(words)), index1=FALSE)
dimnames(X)[[2]] <- words

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

# preparing embed
rs$embed <- NA
for (i in 1:nrow(rs)){

  rs$embed[i] <- tw.embed(rs$text[i], rs$screen_name[i], rs$screen_name[i], rs$id_str[i],
    "", "")

}
congress_rs <- rs
save(congress_rs, file="data_prod/dashboard/congress-rs-tweets.rdata")
#
#
# ###############################################################################
# ### D) Representative media tweets ----
# ###############################################################################
### >> Medias ----
cat("selecting representative medias tweets for each topic\n")

ind <- scan("data_prod/dfm/media-rs-dtm-indices.txt")
pointers <- scan("data_prod/dfm/media-rs-dtm-pointers.txt")
values <- scan("data_prod/dfm/media-rs-dtm-values.txt")
words <- scan("data_prod/dfm/congress-words.txt", what="character", sep="\n")
# # Lit et concatène tous les fichiers dans un seul data frame
if (!dir.exists("data_temp")) {
  dir.create("data_temp",
             recursive = TRUE)
}

tweets <- data.table::fread("data_prod/dfm/media-rs-tweet-list.csv")
# tweets <- data.table::fread("data_temp/all_media_IPG_tweets.csv")
#
X <- sparseMatrix(j=ind, p=pointers, x=values,
  dims=c(nrow(tweets), length(words)), index1=FALSE)
dimnames(X)[[2]] <- words

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

# deleting weird tweets
#todelete <- grep('xss', tweets$text)
#tweets <- tweets[-todelete,]
#results$topics <- results$topics[-todelete,]

K <- 100
rs <- list()

for (k in 1:K){
  choices <- tail(order(results$topics[,k]),n=6)
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

# preparing embed
rs$embed <- NA
for (i in 1:nrow(rs)){

  rs$embed[i] <- tw.embed(rs$text[i], rs$screen_name[i], rs$screen_name[i], rs$id_str[i],
    "", "")

}

media_rs <- rs
save(media_rs, file="data_prod/dashboard/media-rs-tweets.rdata")
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
