#===============================================================================
#  File:    03-running-lda.R
#  Date:    May 3, 2019
#  Paper:   Who Leads? Who Follows? Measuring Issue Attention and Agenda Setting
#           by Legislators and the Mass Public Using Social Media Data
#  Journal: American Political Science Review
#  Authors: Pablo Barbera, Andreu Casas, Jonathan Nagler, Patrick J. Egan,
#           Richard Bonneau, John Jost, Joshua A. Tucker
#  Purpose: run topic model on tweets by Members of Congress; then apply model
#           to compute posterior model probabilities for all corporsa
#  Data In:
#           ./data/tweets/text/* & data/dfm/congress-*
#  Data Out:
#           LDA output
#===============================================================================
# PACKAGES AND PERSONNAL FUNCTIONS
#===============================================================================
library(slam)
library(Matrix)
library(tm)
library(topicmodels)

 check_matrix_dimensions <- function(mat, expected_rows, expected_cols) {
   if (nrow(mat) != expected_rows | ncol(mat) != expected_cols) {
     stop(sprintf("La matrice a des dimensions incorrectes : attendu %d lignes et %d colonnes, mais obtenu %d lignes et %d colonnes.",
                  expected_rows, expected_cols, nrow(mat), ncol(mat)))
   }
 }
#===============================================================================
# DATA
#===============================================================================
if (!file.exists("data_prod/topics/lda_results-twokenizer.Rdata")) {
cat("run topic model on aggregated tweets / party / day of Deputies from the National Assembly\n")
# preparing Congress matrix
nb_fls <- as.numeric(scan("data_prod/dfm/congress-nb-files.txt"))
ind <- scan("data_prod/dfm/congress-day-dtm-indices.txt")
pointers <- scan("data_prod/dfm/congress-day-dtm-pointers.txt")
values <- scan("data_prod/dfm/congress-day-dtm-values.txt")
words <- scan("data_prod/dfm/congress-words.txt", what="character", sep="\n")

X <- sparseMatrix(j=ind, p=pointers, x=values,
                  dims=c(nb_fls, length(words)), index1=FALSE)
dimnames(X)[[2]] <- words

mat <- as.simple_triplet_matrix(X)
dtm <- as.DocumentTermMatrix(mat, weighting=function(x) weightTf(x))

# sanity check
rs <- apply(dtm, 2, sum)
tail(sort(rs),n=100)

# removing empty rows
cs <- row_sums(dtm)
dtm <- dtm[cs>0,]

#===============================================================================
# MAIN: CONGRESS
# ===============================================================================
# running regular LDA
lda.fit <- LDA(dtm, k=100, method="Gibbs",
    	control=list(verbose=50L, iter=2000))

save(lda.fit, file="data_prod/topics/lda_results-twokenizer.Rdata")
} else{
   cat("results file from the lda run on deputies already present. Not running again")
 }

#===============================================================================
# OTHER GROUPS
# ===============================================================================

# ########################
# #### MEDIA ARTICLES ####
# ########################
#
if (!file.exists("data_prod/topics/lda-output/lda-media-results.Rdata")) {
cat("apply initial topic model on aggregated tweets / day from IPG Medias\n")

load("data_prod/topics/lda_results-twokenizer.Rdata")
#
# ## preparing  matrix
nb_fls <- as.numeric(scan("data_prod/dfm/media-nb-files.txt"))
ind <- scan("data_prod/dfm/media-day-dtm-indices.txt")
pointers <- scan("data_prod/dfm/media-day-dtm-pointers.txt")
values <- scan("data_prod/dfm/media-day-dtm-values.txt")
words <- scan("data_prod/dfm/congress-words.txt", what="character", sep="\n")

X <- sparseMatrix(j=ind,
                  p=pointers,
                  x=values,
	dims=c(nb_fls,
	  length(words)), 
	index1=FALSE)

dimnames(X)[[2]] <- words
#
mat <- as.simple_triplet_matrix(X)
dtm <- as.DocumentTermMatrix(mat, weighting=function(x) weightTf(x))

# # sanity check
rs <- col_sums(dtm)
tmp <- data.frame(word=Terms(dtm),rs=rs,stringsAsFactors=F)
tmp <- tmp[order(tmp$rs),]
tail(tmp,n=100)

# removing empty rows
cs <- row_sums(dtm)
dtm <- dtm[cs>0,]

## getting posterior estimates
results <- posterior(lda.fit, dtm,
    control=list(verbose=10L, iter=1000))

# create repertory if necessary
if (!dir.exists("data_prod/topics/lda-output")) {
  dir.create("data_prod/topics/lda-output",
             recursive = TRUE)
}

# saving output
save(results, file = 'data_prod/topics/lda-output/lda-media-results.Rdata')
} else{
  cat("results file from lda applied to media documents already present. Not running again. \n")
}

# ########################################
# #### MEDIA TWEETS (RANDOM SAMPLE) ####
# ########################################
if (!file.exists("data_prod/topics/lda-output/lda-media-rs-results.Rdata")) {
cat("apply initial topic model to a random sample of individual media tweets\n")

load("data_prod/topics/lda_results-twokenizer.Rdata")

ind <- scan("data_prod/dfm/media-rs-dtm-indices.txt")
pointers <- scan("data_prod/dfm/media-rs-dtm-pointers.txt")
values <- scan("data_prod/dfm/media-rs-dtm-values.txt")
words <- scan("data_prod/dfm/congress-words.txt", what="character", sep="\n")

tweets <- read.csv("data_prod/dfm/media-rs-tweet-list.csv", stringsAsFactors=F, colClasses="character")

X <- sparseMatrix(j=ind, p=pointers, x=values,
    dims=c(
      nrow(tweets),
      length(words)
      ),
    index1=FALSE)
dimnames(X)[[2]] <- words

# check matrix dimension
X |> check_matrix_dimensions(expected_rows =  nrow(tweets),
                            expected_cols = length(words)
                            )
# an error occurred probably due to  floating number : see nrow(X) == nrow(tweets) and all.equal(nrow(X), 10000)

# deleting empty rows
todelete <- which(rowSums(X)==0)
X <- X[-todelete,]
tweets <- tweets[-todelete,]

mat <- as.simple_triplet_matrix(X)
dtm <- as.DocumentTermMatrix(mat, weighting=function(x) weightTf(x))

# sanity check
rs <- col_sums(dtm)
tmp <- data.frame(word=Terms(dtm),rs=rs,stringsAsFactors=F)
tmp <- tmp[order(tmp$rs),]

## getting posterior estimates
results <- posterior(lda.fit, dtm,
    control=list(verbose=10L, iter=1000))

# saving output
if (!dir.exists("data_prod/topics/lda-output")) {
  dir.create("data_prod/topics/lda-output",
             recursive = TRUE)
}
save(results, file='data_prod/topics/lda-output/lda-media-rs-results.Rdata')
} else{
  cat("results file from lda applied to the random sample of media tweets already present. Not running again. \n")
}
#
# #################################
# #### RANDOM SAMPLE OF TWEETS ####
# #################################
if (!file.exists("data_prod/topics/lda-output/lda-rs-results.Rdata")) {
cat("apply initial topic model to a random sample of deputies' tweets\n")
load("data_prod/topics/lda_results-twokenizer.Rdata")

## preparing  matrix
##### mettre congress-rs à la place de rs ou mcs...
ind <- scan("data_prod/dfm/congress-rs-dtm-indices.txt")
pointers <- scan("data_prod/dfm/congress-rs-dtm-pointers.txt")
values <- scan("data_prod/dfm/congress-rs-dtm-values.txt")
words <- scan("data_prod/dfm/congress-words.txt", what="character", sep="\n")
tweets <- read.csv("data_prod/dfm/congress-rs-tweet-list.csv", stringsAsFactors=F, colClasses="character"
                   )

X <- sparseMatrix(j=ind, p=pointers, x=values,
                  # the dims are directly inferred to avoid a so-far unexplained error
	dims=c(nrow(tweets), length(words)), 
	index1=FALSE)
dimnames(X)[[2]] <- words

# check the dimensions of the matrix
X |> check_matrix_dimensions(expected_rows =  nrow(tweets),
                             expected_cols = length(words)
)

# deleting empty rows
todelete <- which(rowSums(X)==0)
X <- X[-todelete,]
tweets <- tweets[-todelete,]

mat <- as.simple_triplet_matrix(X)
dtm <- as.DocumentTermMatrix(mat, weighting=function(x) weightTf(x))

# sanity check
rs <- col_sums(dtm)
tmp <- data.frame(word=Terms(dtm),rs=rs,stringsAsFactors=F)
tmp <- tmp[order(tmp$rs),]

## getting posterior estimates
results <- posterior(lda.fit, dtm,
    control=list(verbose=10L, iter=1000))

# saving output
if (!dir.exists("data_prod/topics/lda-output")) {
  dir.create("data_prod/topics/lda-output",
             recursive = TRUE)
}

save(results, file='data_prod/topics/lda-output/lda-rs-results.Rdata')
} else{
  cat("results file from lda applied to media tweets sample already present. Not running again. \n")
}

# ########################
# #### INDIVIDUAL MCS ####
# ########################
# if (!file.exists("data_prod/topics/lda-output/lda-mcs-results.Rdata")) {
# cat("apply initial topic model to all individual tweets from deputies\n")
# load("data_prod/topics/lda_results-twokenizer.Rdata")
#
# ## preparing  matrix
# ind <- scan("data/dfm/mcs-dtm-indices.txt")
# pointers <- scan("data/dfm/mcs-dtm-pointers.txt")
# values <- scan("data/dfm/mcs-dtm-values.txt")
# words <- scan("data/dfm/congress-words.txt", what="character", sep="\n")
# mcs <- scan("data/dfm/mcs-list.txt", what='character')
#
# X <- sparseMatrix(j=ind, p=pointers, x=values,
# 	dims=c(length(mcs), length(words)), index1=FALSE)
# dimnames(X)[[2]] <- words
#
# mat <- as.simple_triplet_matrix(X)
# dtm <- as.DocumentTermMatrix(mat, weighting=function(x) weightTf(x))
#
# # sanity check
# rs <- col_sums(dtm)
# tmp <- data.frame(word=Terms(dtm),rs=rs,stringsAsFactors=F)
# tmp <- tmp[order(tmp$rs),]
#
# ## getting posterior estimates
# results <- posterior(lda.fit, dtm,
#     control=list(verbose=10L, iter=500))
#
# # saving output
# if (!dir.exists("data_prod/topics/lda-output")) {
#   dir.create("data_prod/topics/lda-output",
#              recursive = TRUE)
# }

# save(results, file='data_prod/topics/lda-output/lda-mcs-results.Rdata')
#
# }

# #################################
# #### SUPPORTER PUBLICS     ####
# #################################
#
if (!all(file.exists("data_prod/topics/lda-output/lda-lr_supporters-results.Rdata",
                 "data_prod/topics/lda-output/lda-majority_supporters-results.Rdata",
                 "data_prod/topics/lda-output/lda-nupes_supporters-results.Rdata",
                 "data_prod/topics/lda-output/lda-rn_supporters-results.Rdata")
    )) {
cat("apply initial topic model on aggregated tweets / day by supporters of each political group\n")
#
load("data_prod/topics/lda_results-twokenizer.Rdata")

## preparing  matrix

nb_fls <- as.numeric(scan("data_prod/dfm/supporter-nb-files.txt"))

ind <- scan("data_prod/dfm/supporter-day-dtm-indices.txt")
pointers <- scan("data_prod/dfm/supporter-day-dtm-pointers.txt")
values <- scan("data_prod/dfm/supporter-day-dtm-values.txt")
words <- scan("data_prod/dfm/congress-words.txt", what="character", sep="\n")
users <- scan("data_prod/dfm/supporter-day-party-list.txt", what='character')

X <- sparseMatrix(j=ind, p=pointers, x=values,
	dims=c(nb_fls,
	       length(words)), 
	index1=FALSE)

dimnames(X)[[2]] <- words

mat <- as.simple_triplet_matrix(X)
dtm <- as.DocumentTermMatrix(mat, weighting=function(x) weightTf(x))

# sanity check
rs <- col_sums(dtm)
tmp <- data.frame(word=Terms(dtm),rs=rs,stringsAsFactors=F)
tmp <- tmp[order(tmp$rs),]
tail(tmp,n=100)
}
# #################
# ## LR
# #################
#
if (!file.exists("data_prod/topics/lda-output/lda-lr_supporters-results.Rdata")) {

cat("lr supporters\n")

lr <- grep('lr', users)
dtm2 <- dtm[lr,]

## getting posterior estimates
results <- posterior(lda.fit, dtm2,
    control=list(verbose=1L, iter=500))

# saving output
if (!dir.exists("data_prod/topics/lda-output")) {
  dir.create("data_prod/topics/lda-output",
             recursive = TRUE)
}

save(results, file='data_prod/topics/lda-output/lda-lr_supporters-results.Rdata')
} else{
  cat("results file from lda applied to LR supporters' documents already present. Not running again. \n")
}
#
# # #################
# # ## MAJORITY
# # #################
# #
if (!file.exists("data_prod/topics/lda-output/lda-majority_supporters-results.Rdata")) {

cat("majority supporters\n")
majority <- grep('majority', users)
dtm2 <- dtm[majority,]

## getting posterior estimates
results <- posterior(lda.fit, dtm2,
                     control=list(verbose=1L, iter=500))

# saving output
if (!dir.exists("data_prod/topics/lda-output")) {
  dir.create("data_prod/topics/lda-output",
             recursive = TRUE)
}
save(results, file='data_prod/topics/lda-output/lda-majority_supporters-results.Rdata')
} else{
  cat("results file from lda applied to majority supporters' documents already present. Not running again. \n")
}
#
# # #################
# # ## NUPES
# # #################
# #
if (!file.exists("data_prod/topics/lda-output/lda-nupes_supporters-results.Rdata")) {
cat("nupes supporters\n")

nupes <- grep('nupes', users)
dtm2 <- dtm[nupes,]

## getting posterior estimates
results <- posterior(lda.fit, dtm2,
                     control=list(verbose=1L, iter=500))

# saving output
if (!dir.exists("data_prod/topics/lda-output")) {
  dir.create("data_prod/topics/lda-output",
             recursive = TRUE)
}

save(results, file='data_prod/topics/lda-output/lda-nupes_supporters-results.Rdata')
} else{
  cat("results file from lda applied to NUPES supporters' documents already present. Not running again. \n")
}

# # #################
# # ## RN
# # #################
# #
if (!file.exists("data_prod/topics/lda-output/lda-rn_supporters-results.Rdata")) {
cat("rn supporters\n")
rn <- grep('rn', users)
dtm2 <- dtm[rn,]

## getting posterior estimates
results <- posterior(lda.fit, dtm2,
                     control=list(verbose=1L, iter=500))

# saving output
if (!dir.exists("data_prod/topics/lda-output")) {
  dir.create("data_prod/topics/lda-output",
             recursive = TRUE)
}

save(results, file='data_prod/topics/lda-output/lda-rn_supporters-results.Rdata')
} else{
  cat("results file from lda applied to RN supporters' documents already present. Not running again. \n")
}
 
# #################################
# #### GENERAL PUBLIC            ####
# #################################
#
if (!file.exists("data_prod/topics/lda-output/lda-general-results.Rdata")) {

cat("apply initial topic model to aggregated tweets / day from general public\n")

load("data_prod/topics/lda_results-twokenizer.Rdata")

## preparing  matrix
nb_fls <- as.numeric(scan("data_prod/dfm/general-nb-files.txt"))

ind <- scan("data_prod/dfm/general-day-dtm-indices.txt")
pointers <- scan("data_prod/dfm/general-day-dtm-pointers.txt")
values <- scan("data_prod/dfm/general-day-dtm-values.txt")
words <- scan("data_prod/dfm/congress-words.txt", what="character", sep="\n")

X <- sparseMatrix(j=ind, p=pointers, x=values,
    dims=c(nb_fls, length(words)), index1=FALSE)

dimnames(X)[[2]] <- words

mat <- as.simple_triplet_matrix(X)
dtm <- as.DocumentTermMatrix(mat, weighting=function(x) weightTf(x))

# sanity check
rs <- col_sums(dtm)
tmp <- data.frame(word=Terms(dtm),rs=rs,stringsAsFactors=F)
tmp <- tmp[order(tmp$rs),]
tail(tmp,n=100)

## getting posterior estimates
results <- posterior(lda.fit, dtm,
    control=list(verbose=1L, iter=500))

# saving output
if (!dir.exists("data_prod/topics/lda-output")) {
  dir.create("data_prod/topics/lda-output",
             recursive = TRUE)
}

save(results, file='data_prod/topics/lda-output/lda-general-results.Rdata')
} else{
  cat("results file from lda applied to general public's documents already present. Not running again. \n")
}

# #################################
# #### ATTENTIVE PUBLIC            ####
# #################################
#
if (!file.exists("data_prod/topics/lda-output/lda-attentive-results.Rdata")) {

cat("apply initial topic model to aggregated tweets / day from attentive public\n")

load("data_prod/topics/lda_results-twokenizer.Rdata")

## preparing  matrix
nb_fls <- as.numeric(scan("data_prod/dfm/attentive-nb-files.txt"))

ind <- scan("data_prod/dfm/attentive-day-dtm-indices.txt")
pointers <- scan("data_prod/dfm/attentive-day-dtm-pointers.txt")
values <- scan("data_prod/dfm/attentive-day-dtm-values.txt")
words <- scan("data_prod/dfm/congress-words.txt", what="character", sep="\n")

X <- sparseMatrix(j=ind, p=pointers, x=values,
                  dims=c(nb_fls, length(words)), index1=FALSE)

dimnames(X)[[2]] <- words

mat <- as.simple_triplet_matrix(X)
dtm <- as.DocumentTermMatrix(mat, weighting=function(x) weightTf(x))

# sanity check
rs <- col_sums(dtm)
tmp <- data.frame(word=Terms(dtm),rs=rs,stringsAsFactors=F)
tmp <- tmp[order(tmp$rs),]
tail(tmp,n=100)

## getting posterior estimates
results <- posterior(lda.fit, dtm,
                     control=list(verbose=1L, iter=500))

# saving output
if (!dir.exists("data_prod/topics/lda-output")) {
  dir.create("data_prod/topics/lda-output",
             recursive = TRUE)
}

save(results, file='data_prod/topics/lda-output/lda-attentive-results.Rdata')

} else{
  cat("results file from lda applied to attentive public's documents already present. Not running again. \n")
}
