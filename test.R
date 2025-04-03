library(slam)
library(Matrix)
library(tm)
library(topicmodels)

cat("run topic model on aggregated tweets / party / day of Deputies from the National Assembly\n")
# preparing Congress matrix
nb_fls <- as.numeric(scan("data_prod/test/congress-nb-files.txt"))
ind <- scan("data_prod/test/congress-dtm-indices.txt")
pointers <- scan("data_prod/test/congress-dtm-pointers.txt")
values <- scan("data_prod/test/congress-dtm-values.txt")
words <- scan("data_prod/test/congress-words.txt", what="character", sep="\n")

X <- sparseMatrix(j=ind, p=pointers, x=values,
                  dims=c(nb_fls, length(words)), index1=FALSE)
dimnames(X)[[2]] <- words

mat <- as.simple_triplet_matrix(X)
dtm <- as.DocumentTermMatrix(mat, weighting=function(x) weightTf(x))

print(dim(X))

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
lda.fit <- LDA(dtm, k=6, method="Gibbs",
    	control=list(verbose=50L, iter=2000))

save(lda.fit, file="data_prod/test/lda__TEST_results-twokenizer.Rdata")

load("data_prod/test/lda__TEST_results-twokenizer.Rdata")

## preparing  matrix
nb_fls <- as.numeric(scan("data_prod/test/general-nb-files.txt"))
ind <- scan("data_prod/test/general-dtm-indices.txt")
pointers <- scan("data_prod/test/general-dtm-pointers.txt")
values <- scan("data_prod/test/general-dtm-values.txt")
words <- scan("data_prod/test/congress-words.txt", what="character", sep="\n")

X <- sparseMatrix(j=ind, p=pointers, x=values,
    dims=c(nb_fls, length(words)), index1=FALSE)


print(dim(X))

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


save(results, file='data_prod/test/lda-predict-results.Rdata')