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

  # 8. Spécificité = p word in each topic * p topic / proba de l'apparition du mot dans l'ensemble des documents (par la formule des probas totales)
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

#Find common keyword between topics 
word_counts <- info.df %>%
  group_by(word) %>%
  mutate(word = str_replace(word, ".*retraite.*", "retraite")) %>%
  summarise(count = n(), topics = as.character(list(unique(topic)))) %>%
  mutate(topic_count = sapply(topics, function(x) length(strsplit(x, ",")[[1]]))) %>%
  select(-count) %>%
  filter(topic_count > 1) %>%
  arrange(desc(topic_count))

print(word_counts)
write.csv(word_counts, file="data_prod/topics/common_keywords_lda.csv", row.names=FALSE, quote=TRUE)