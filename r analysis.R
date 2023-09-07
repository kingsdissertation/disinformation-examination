install.packages("tidyverse")
install.packages("quanteda")
install.packages("tidytext")
install.packages("topicmodels")
install.packages("stm")
options(scipen=999)
library(tidyverse)
library(quanteda)
require(tidytext)
require(topicmodels)
library(stm)
library(dplyr)
read.csv("~/Documents/dissertation/EN_Propaganda_diary.csv")
en_propoganda <- read.csv("~/Documents/dissertation/EN_Propaganda_diary.csv") %>%
  select(id,ENG_Translation, Date, Media,Country)
install.packages("readtext")
library(readtext)
require(quanteda)
corp_propoganda <- corpus(en_propoganda, text_field = "ENG_Translation")
require(quanteda.textstats)
toks_propoganda <- tokens(corp_propoganda)
toks_nopunct_propoganda <- tokens(corp_propoganda, remove_punct = TRUE)
toks_clean <- tokens_remove(toks_nopunct_propoganda,pattern=stopwords("english"))
toks_clean_1 <- tokens(toks_clean, remove_numbers = TRUE)
toks_clean_2 <- tokens_tolower(toks_clean_1)
toks_clean_3 <- tokens(toks_clean_2, remove_separators = TRUE)
toks_clean_4 <- tokens(toks_clean_3, remove_symbols = TRUE)
toks_clean_5 <- tokens(toks_clean_4, split_hyphens = TRUE)
toks_clean_6 <- tokens_select(
  toks_clean_5,
  c("[\\d-]", "[[:punct:]]", "^.{1,2}$"),
  selection = "remove",
  valuetype = "regex",
  verbose = TRUE
)
dfmat_propoganda <- dfm(toks_clean_6)
print(dfmat_propoganda)
dfmat_propoganda_trim <- dfm_trim(dfmat_propoganda, min_docfreq = 0.001, docfreq_type = "prop")
print(dfmat_propoganda_trim)

install.packages("stm")
require(stm)
topic.count <- 38
dfm2stm <- convert(dfmat_propoganda_trim, to = "stm")
poliblogPrevFit <- stm(dfm2stm$documents, dfm2stm$vocab, K=38, max.em.its=90, data=dfm2stm$meta, init.type="Spectral", seed=8458159)
plot(poliblogPrevFit, type="summary", xlim=c(0,.3),n=5)
plot(poliblogPrevFit, type="summary", labeltype = "frex", xlim=c(0,.3),n=5)
mod.out.gs.corr = topicCorr(poliblogPrevFit)
plot.topicCorr(mod.out.gs.corr)
plot(poliblogPrevFit, type="labels",labeltype = "frex", topics=c(36,37,38))
install.packages("devtools")
devtools::install_github("quanteda/quanteda.sentiment")
1
library(quanteda.sentiment)
install.packages("syuzhet")
require(syuzhet)
?data_dictionary_NRC
library(tm)
get_nrc_sentiment(corp_propoganda[1])
sum(get_sentiment(corp_propoganda, method="nrc"))
get_sentiment(corp_propoganda, method="nrc")
nrc <- get_sentiment(corp_propoganda, method="nrc")

emotions <- get_nrc_sentiment(corp_propoganda)
emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])
install.packages("plotly")
require(plotly)
install.packages("wordcloud")
require(wordcloud)
plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Distribution of emotion categories for fake news")
require(tm)
all_corp = c(
  paste(corp_propoganda[emotions$anger > 0], collapse=" "),
  paste(corp_propoganda[emotions$anticipation > 0], collapse=" "),
  paste(corp_propoganda[emotions$disgust > 0], collapse=" "),
  paste(corp_propoganda[emotions$fear > 0], collapse=" "),
  paste(corp_propoganda[emotions$joy > 0], collapse=" "),
  paste(corp_propoganda[emotions$sadness > 0], collapse=" "),
  paste(corp_propoganda[emotions$surprise > 0], collapse=" "),
  paste(corp_propoganda[emotions$trust > 0], collapse=" "),

)
all <- removeWords(all_corp, stopwords("english"))
corpus = Corpus(VectorSource(all))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
tdm1 <- tdm[nchar(rownames(tdm)) < 11,]
colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
colnames(tdm1) <- colnames(tdm)
comparison.cloud(tdm1, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1, max.words=200, scale=c(2.0, 0.4),rot.per=0.4)

