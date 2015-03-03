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
inspect(dtm)

burnin <- 500
iter <- 100
keep <- 30

k <- 40
mods <- LDA(dtm, k, method = "Gibbs",
            control = list(burnin = burnin,
                           iter = iter,
                           keep = keep))

doc.id <- mods@wordassignments$i
token.id <- mods@wordassignments$j
topic.id <- mods@wordassignments$v
vocab <- mods@terms

# dat <- getProbs(token.id, doc.id, topic.id, vocab, K = max(topic.id), sort.topics = "byTerms")
# phi <- t(dat$phi.hat)
# theta <- dat$theta.hat[, dat$topic.order]

zson <- createJSON(K = K,
                   phi = t(phi),
                   theta = theta,
                   doc.length = sum(dtm),
                   term.frequency = term.frequency,
                   topic.proportion = topic.proportion,
                   vocab = vocab)

serVis(zson)
