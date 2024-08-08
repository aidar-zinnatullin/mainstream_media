library(here)
library(tidyverse)
library(LSX)
library(quanteda)
library(xml2)
library(XML)
library(reticulate)
library(spacyr)
covid <- read_xml(here("..","..", "t_online_export", "corona.xml"))
covid

covid_xml <- xmlParse(covid)
xml_structure(covid)

url <- xml_text(xml_find_all(covid, ".//url"))
date_pub <- xml_text(xml_find_all(covid, ".//publication_date"))
text <- xml_text(xml_find_all(covid, ".//text"))
headline <- xml_text(xml_find_all(covid, ".//headline"))

# format as tibble
covid_tonline <- tibble(url_to = url, date_to = date_pub, title = headline, article = text)
dim(covid_tonline)
names(covid_tonline)
head(covid_tonline)

class(covid_tonline$date_to)

# lemmatization with spacy
Sys.setenv(SPACY_PYTHON = "/Users/ajdarzinnatullin/anaconda3/envs/another_de")
#spacy_download_langmodel(lang_models = "de_dep_news_trf", force = TRUE)
#spacy_install(lang_models = "de_dep_news_trf")
spacy_initialize(model = "de_dep_news_trf")
#spacy_finalize()

covid_tonline$id_article <- c(1:nrow(covid_tonline))
#n_distinct(covid_tonline$id_article) == nrow(covid_tonline)

names(covid_tonline)
covid_tonline_lemmatization <-  corpus(x = covid_tonline, text_field = "article", docid_field = "id_article")
lemmatization_corpus <- spacy_parse(covid_tonline_lemmatization, dependency = FALSE, lemma = TRUE, tag = FALSE, entity = FALSE) # 15:25 - 
lemmatization_corpus

#save(lemmatization_corpus, file = "lemmatization_corpus.RData")
gc()



lemmatization_corpus$lemma[1:5]
lemmatization_corpus$lemma_lower <- str_to_lower(lemmatization_corpus$lemma)
names(lemmatization_corpus)[7] <- "word"
new_stopwords <- read.table("https://raw.githubusercontent.com/stopwords-iso/stopwords-de/master/stopwords-de.txt") # 563 words
new_stopwords <- as_tibble(new_stopwords)
names(new_stopwords) <- 'word'
lemmatization_corpus <- lemmatization_corpus %>%
  anti_join(new_stopwords)


lemmatization_corpus <- lemmatization_corpus[!grepl("SPACE", lemmatization_corpus$pos), ]
lemmatization_corpus <- lemmatization_corpus[!grepl("X", lemmatization_corpus$pos), ]
lemmatization_corpus <- lemmatization_corpus[!grepl("http", lemmatization_corpus$word), ]
lemmatization_corpus <- lemmatization_corpus[!grepl("NUM", lemmatization_corpus$pos), ]
lemmatization_corpus <- lemmatization_corpus[!grepl("[\U0001F600-\U0001F64F\U0001F300-\U0001F5FF\U0001F680-\U0001F6FF\U0001F700-\U0001F77F\U0001F780-\U0001F7FF\U0001F800-\U0001F8FF\U0001F900-\U0001F9FF\U0001FA00-\U0001FA6F\U0001FA70-\U0001FAFF\U00002702-\U000027B0\U000024C2-\U0001F251]+", lemmatization_corpus$word), ]
nrow(lemmatization_corpus)
lemmatization_corpus <- lemmatization_corpus[!grepl("#", lemmatization_corpus$word), ]

lemmatization_corpus <- lemmatization_corpus[!grepl("t.me", lemmatization_corpus$word), ]
lemmatization_corpus <- lemmatization_corpus[!grepl("@", lemmatization_corpus$word), ]
lemmatization_corpus <- lemmatization_corpus[!grepl("\\d{2}\\.\\d{2}\\.\\d{4}", lemmatization_corpus$word), ]
lemmatization_corpus <- lemmatization_corpus[!grepl("[[:digit:]]", lemmatization_corpus$word), ]
nrow(lemmatization_corpus)

nrow(lemmatization_corpus)
lemmatization_corpus <- lemmatization_corpus[!grepl("[[:cntrl:]]", lemmatization_corpus$word), ]
lemmatization_corpus <- lemmatization_corpus[!grepl("[[:space:]]", lemmatization_corpus$word), ]
nrow(lemmatization_corpus)

nrow(lemmatization_corpus)
lemmatization_corpus <- lemmatization_corpus[!grepl("\\[", lemmatization_corpus$word), ]
lemmatization_corpus <- lemmatization_corpus[!grepl("\\]", lemmatization_corpus$word), ]
lemmatization_corpus <- lemmatization_corpus[!grepl("%", lemmatization_corpus$word), ]
lemmatization_corpus <- lemmatization_corpus[!grepl("\\b\\w+\\.", lemmatization_corpus$word), ]
lemmatization_corpus <- lemmatization_corpus[!grepl("\\*", lemmatization_corpus$word), ]
lemmatization_corpus <- lemmatization_corpus[!grepl("\\§", lemmatization_corpus$word), ]

lemmatization_corpus <- lemmatization_corpus[!grepl("\\S*\\++\\S*", lemmatization_corpus$word), ]

# full stops to be retained
lemmatization_corpus$word[lemmatization_corpus$pos=="PUNCT" & lemmatization_corpus$token=="."] <- "."
length(lemmatization_corpus$word[lemmatization_corpus$pos=="PUNCT" & lemmatization_corpus$token=="."])

lemmatization_corpus$pos[lemmatization_corpus$pos=="PUNCT" & lemmatization_corpus$token=="."] <- "FULL"
lemmatization_corpus <- lemmatization_corpus[!grepl("PUNCT", lemmatization_corpus$pos), ]
lemmatization_corpus <- lemmatization_corpus[!grepl("\\$|#|%|_|\\}|\\{|=", lemmatization_corpus$word), ]

nrow(lemmatization_corpus)
lemmatization_corpus$word <- str_trim(lemmatization_corpus$word, side = "both") 



enghlish_stopwords <- read.table("https://gist.githubusercontent.com/sebleier/554280/raw/7e0e4a1ce04c2bb7bd41089c9821dbcf6d0c786c/NLTK's%2520list%2520of%2520english%2520stopwords") # 563 words
enghlish_stopwords <- as_tibble(enghlish_stopwords)
names(enghlish_stopwords) <- 'word'
nrow(lemmatization_corpus)
lemmatization_corpus <- lemmatization_corpus %>%
  anti_join(enghlish_stopwords)
nrow(lemmatization_corpus)

lemmatization_corpus_ver2 <- lemmatization_corpus

save(lemmatization_corpus_ver2, file = "cleaned_tokens.RData")
gc()

load("cleaned_tokens.RData")
names(lemmatization_corpus_ver2)
lemmatization_corpus_ver2 <- lemmatization_corpus_ver2[, c("doc_id", "word")]


lemma_covid_tonline <- lemmatization_corpus_ver2 %>% 
  nest(word) %>% 
  mutate(message = map(data, unlist),
         message = map_chr(message, paste, collapse = " "))

# 


lemma_covid_tonline$data <- NULL
n_distinct(lemma_covid_tonline$doc_id)
n_distinct(covid_tonline$id_article)

names(lemma_covid_tonline) <- c("id_article", "lemmatized_text")
class(lemma_covid_tonline$id_article)
class(covid_tonline$id_article)

covid_tonline$id_article <- as.character(covid_tonline$id_article)

covid_tonline <- right_join(covid_tonline, lemma_covid_tonline, by = "id_article")

n_distinct(covid_tonline$id_article)
n_distinct(lemma_covid_tonline$id_article)


covid_tonline$lemmatized_text <- gsub(" \\.", ".", covid_tonline$lemmatized_text)

covid_tonline$lemmatized_text <- ifelse(grepl("\\.$", covid_tonline$lemmatized_text), 
                                        covid_tonline$lemmatized_text, 
                                        paste0(covid_tonline$lemmatized_text, "."))


covid_tonline$lemmatized_text <- gsub(">", "", covid_tonline$lemmatized_text)
covid_tonline$lemmatized_text <- gsub("<", "", covid_tonline$lemmatized_text)

save(covid_tonline, file = "covid_tonline.RData")




