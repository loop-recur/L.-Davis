library(jsonlite)
library(tm)

insights <- fromJSON(readLines('data/insights2.json'))$text
corp <- Corpus(VectorSource(insights))

dtm.control <- list(tolower = TRUE,
                    removePunctuation = TRUE,
                    removeNumbers = TRUE,
                    stopwords = c(stopwords("SMART"),
                                  stopwords("en")),
                    stemming = TRUE,
                    wordLengths = c(3, "inf"),
                    weighting = weightTf)

sparse.dtm <- DocumentTermMatrix(corp, control = dtm.control)
dtm <- as.matrix(sparse.dtm)
class(dtm) <- "integer"

vocab <- sparse.dtm$dimnames$Terms
# Compute some statistics related to the data set:
D <- length(corp)  # number of documents
W <- length(vocab)  # number of terms in the vocab
doc.length <- rowSums(dtm)  # number of tokens per document
N <- sum(doc.length)  # total number of tokens in the data
term.frequency <- colSums(dtm)  # frequencies of terms in the corpus

# MCMC and model tuning parameters:
K <- 20
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)

foo <- lapply(1:nrow(dtm), function (i) rbind(1:ncol(dtm), t(dtm[i,])) )

t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = foo,
                                   K = K, vocab = vocab,
                                   num.iterations = G, alpha = alpha,
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  # about 24 minutes on laptop


theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

library(LDAvis)

# create the JSON object to feed the visualization:
json <- createJSON(phi = phi,
                   theta = theta,
                   doc.length = doc.length,
                   vocab = vocab,
                   term.frequency = term.frequency)

serVis(json)
