library(tm)
library(lda)
library(LDAvis)

fileName <- "recreation-focus-group.txt"
transcript <- readChar(fileName, file.info(fileName)$size)
corp <- Corpus(VectorSource(transcript))

dtm.control <- list(
  tolower = TRUE,
  removePunctuation = TRUE,
  removeNumbers = TRUE,
  stopwords = c(stopwords("SMART")
                # extendedstopwords,
                #namez
                ),
  stemming = TRUE,
  wordLengths = c(3, "inf"),
  weighting = weightTf)

dtm <- DocumentTermMatrix(corp, control = dtm.control)

vocab <- dtm$dimnames$Terms

burnin <- 800
G <- 5000
keep <- 30

k <- 10
alpha <- 0.02
eta <- 0.02

fit <- lda.collapsed.gibbs.sampler(documents = list(rbind(as.integer(dtm$v), as.integer(dtm$j))),
                                   K = k, vocab = vocab,
                                   num.iterations = G, alpha = alpha,
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

json <- createJSON(phi = phi,
                   theta = theta,
                   doc.length = sum(dtm),
                   vocab = matrix(unlist(vocab), nrow=1),
                   term.frequency = dtm$v)

serVis(json)
