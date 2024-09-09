
# Extract Personalities from the overall set of articles ------------------

library(here)
library(tidyverse)
library(stringr)
library(stringi)
library(data.table)
library(writexl)
library(readr)
library(tokenizers)

ner_dataset <- read_csv(here("overall_ner.csv"))


names(ner_dataset)
head(ner_dataset)


# extract NER personalities from this set of articles ---------------------
n_distinct(ner_dataset$id_article)==nrow(ner_dataset)
names(ner_dataset)[5] <- "link_to"


# data.table enters the game ----------------------------------------------

xxx <- data.table(ner_dataset)
xxx[, named_entities:=gsub("\\[|\\]", "", named_entities, perl = T)] #remove brackets
xxx[, parsed := str_split(named_entities, "\\),")]                     # split string
out <- xxx[, .(unlist(parsed, recursive = F)), by = .(link_to)]                # unlist elements
out [ , V1  := gsub("\\)?$",")", V1)][]                                

out <- as.data.frame(out)

out <- transform(out,
                 Name = sapply(out$V1, function(x) trimws(unlist(strsplit(sub("^\\('|'\\)$", "", x), "', '"))[1])),
                 Type = sapply(out$V1, function(x) trimws(unlist(strsplit(sub("^\\('|'\\)$", "", x), "', '"))[2]))
)

out$Name <- gsub(pattern = "('",replacement = "", x = out$Name, fixed = TRUE)
out$Type <- gsub(pattern = "')",replacement = "", x = out$Type, fixed = TRUE)

names(ner_dataset)
ner_dataset <- as.data.frame(ner_dataset)
merging_set_ner <- right_join(ner_dataset, out, by = "link_to")

### let's keep only PER-sonalities (and with full names, therefore we have "\\S+"  >=2)

specific_points_1 <- merging_set_ner %>% filter(Type =="PER" ) %>% 
  filter(Name != "") %>%
  filter(Type == "PER" & str_count(Name, "\\S+") >= 2)
names(specific_points_1)


extract_and_expand_2 <- function(article, name, type, link_to) {
  if (!is.character(article) || length(article) != 1) {
    stop("The article must be a non-empty single string.")
  }
  
  sentences <- unlist(tokenize_sentences(article))
  
  name_positions <- grep(name, sentences, ignore.case = TRUE)
  
  if (length(name_positions) > 0) {
    contexts <- lapply(name_positions, function(pos) {
      start <- max(1, pos - 2)
      end <- min(length(sentences), pos + 2)
      
      context <- sentences[pos]
      expanded_context <- paste(sentences[start:end], collapse = " ")
      
      data.frame(article = article, 
                 context = context, 
                 expanded_context = expanded_context, 
                 name = name, 
                 Type = type, 
                 link_to = link_to,
                 stringsAsFactors = FALSE)
    })
    
    return(do.call(rbind, contexts))
  } else {
    return(data.frame(article = article, 
                      context = NA, 
                      expanded_context = NA, 
                      name = name, 
                      Type = type, 
                      link_to = link_to,
                      stringsAsFactors = FALSE))
  }
}

experiments_2sentence_window_2 <- specific_points_1 %>% select(link_to, title, article, date_to, Name, Type)


experiments_2sentence_window_2 <- data.frame(
  articles = experiments_2sentence_window_2$article,
  Name = stri_enc_toutf8(experiments_2sentence_window_2$Name),
  Type = experiments_2sentence_window_2$Type,
  link_to = experiments_2sentence_window_2$link_to,
  stringsAsFactors = FALSE)

experiments_2sentence_window_2 <- experiments_2sentence_window_2 %>%
  rowwise() %>%
  do(extract_and_expand_2(.$articles, .$Name, .$Type, .$link_to)) %>%
  ungroup()
experiments_2sentence_window_2 <- experiments_2sentence_window_2 %>% distinct()
table(is.na(experiments_2sentence_window_2$context))
table(is.na(experiments_2sentence_window_2$expanded_context))
nas <- experiments_2sentence_window_2[experiments_2sentence_window_2$context == "NA",]

# thousand statements for validation --------------------------------------


experiments_2sentence_window_2 <- experiments_2sentence_window_2 %>% filter(is.na(expanded_context)!=TRUE) # as a solution, I can manually add everythin
names(experiments_2sentence_window_2)
elwe_t_online_version <- experiments_2sentence_window_2 %>% select(expanded_context, name, link_to)
write_csv(elwe_t_online_version, here("t_online_personalities_whole_corpus.csv"))

