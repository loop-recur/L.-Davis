library(tm)
library(topicmodels)
library(LDAvis)

fileName <- "recreation-focus-group.txt"
transcript <- readChar(fileName, file.info(fileName)$size)
corp <- Corpus(VectorSource(transcript))

dtm.control <- list(
  tolower = TRUE,
  removePunctuation = TRUE,
  removeNumbers = TRUE,
  stopwords = c(stopwords("english")
                # extendedstopwords,
                #namez
                ),
  stemming = TRUE,
  wordLengths = c(3, "inf"),
  weighting = weightTf)

dtm <- DocumentTermMatrix(corp, control = dtm.control)

burnin <- 500
iter <- 1000
keep <- 30

k <- 10
mods <- LDA(dtm, k, method = "Gibbs",
            control = list(burnin = burnin,
                           iter = iter,
                           keep = keep))

doc.id <- mods@wordassignments$i
token.id <- mods@wordassignments$j
topic.id <- mods@wordassignments$v
vocab <- mods@terms

# phi <- t(apply(t(topics(mods)) + 0.02, 2, function(x) x/sum(x)))
# theta <- t(apply(mods@wordassignments + 0.02, 2, function(x) x/sum(x)))
# dat <- getProbs(token.id, doc.id, topic.id, vocab, K = max(topic.id), sort.topics = "byTerms")
# phi <- t(dat$phi.hat)
# theta <- dat$theta.hat[, dat$topic.order]

zson <- createJSON(K = K,
                   phi = mods@beta,
                   theta = mods@gamma,
                   doc.length = sum(dtm),
                   term.frequency = as.numeric(table(token.id)),
                   topic.proportion = as.numeric(table(topic.id)/length(topic.id)),
                   vocab = vocab)

serVis(zson)
