library(here)
library(tidyverse)
library(stringr)
library(stringi)
library(data.table)
library(writexl)
library(readr)

first <- read_csv(here("ner_output_1.csv"))
second <- read_csv(here("ner_output_2.csv"))
third <- read_csv(here("ner_output_3.csv"))
fourth <- read_csv(here("output_test.csv"))

names(first)
names(second)
names(third)
names(fourth)
table(first$id_article %in% second$id_article)
table(third$id_article %in% fourth$id_article)

temp_all <- rbind(first, second, third, fourth) 

# randomly select 100 articles

set.seed(123)
temp_all %>% 
  sample_n(100)  -> sample_articles
table(sample_articles$id_article %in% first$id_article)


# extract NER personalities from this set of articles ---------------------


names(sample_articles)[5] <- "link_to"


# fwrite(sample_articles,"temp.csv")
# sample_articles <- fread("temp.csv",encoding = "UTF-8")


#xxx <- sample_articles
xxx <- data.table(sample_articles)
xxx[, named_entities:=gsub("\\[|\\]", "", named_entities, perl = T)] #remove brackets
xxx[, parsed := str_split(named_entities, "\\),")]                     # split string
out <- xxx[, .(unlist(parsed, recursive = F)), by = .(link_to)]                # unlist elements
out [ , V1  := gsub("\\)?$",")", V1)][]                                

out <- as.data.frame(out)
# create two columns by splitting the 'V1' column
out <- transform(out,
                 Name = sapply(out$V1, function(x) trimws(unlist(strsplit(sub("^\\('|'\\)$", "", x), "', '"))[1])),
                 Type = sapply(out$V1, function(x) trimws(unlist(strsplit(sub("^\\('|'\\)$", "", x), "', '"))[2]))
)

out$Name <- gsub(pattern = "('",replacement = "", x = out$Name, fixed = TRUE)
out$Type <- gsub(pattern = "')",replacement = "", x = out$Type, fixed = TRUE)

names(sample_articles)
sample_articles <- as.data.frame(sample_articles)
checccks <- right_join(sample_articles, out, by = "link_to")

### let's keep only ORG, PER????

#specific_points_1 <- checccks %>% filter(Type=="ORG" | Type =="PER" )

specific_points_1 <- checccks %>% filter(Type =="PER" ) %>% 
  filter(Name != "") %>%
  filter(Type == "PER" & str_count(Name, "\\S+") >= 2)
names(specific_points_1)



# here, we derive actors and 2 sentence window
names(specific_points_1)
experiments_2sentence_window <- specific_points_1 %>% select(link_to, title, article, date_to, Name, Type)
#Encoding(experiments_2sentence_window$Name) <- 'UTF-8'
#Encoding(experiments_2sentence_window$articles) <- 'UTF-8'
#experiments_2sentence_window$articles <- str_replace_all(experiments_2sentence_window$articles, "(?<![0-9])(\\.)([A-Za-z])", "\\1 \\2") # check number 9
#experiments_2sentence_window$articles <- str_replace_all(experiments_2sentence_window$articles, "([a-z])([A-Z])", "\\1 \\2") # check number 9

extract_and_expand <- function(article, name, type, link_to) {
  # Split the article into sentences
  sentences <- unlist(str_split(article, "(?<=[.!?])\\s+"))
  
  # Find the positions of sentences containing the name, ignoring case
  name_positions <- grep(name, sentences, ignore.case = TRUE)
  
  # Extract contexts if name positions are found
  contexts <- lapply(name_positions, function(pos) {
    start <- max(1, pos - 2)
    end <- min(length(sentences), pos + 2)
    context <- sentences[pos]
    expanded_context <- paste(sentences[start:end], collapse = ". ")
    data.frame(article = article, context = context, expanded_context = expanded_context, name = name, Type = type, link_to = link_to)
  })
  
  # Return the extracted contexts or a dataframe with NAs if no matches are found
  if (length(contexts) > 0) {
    return(do.call(rbind, contexts))
  } else {
    return(data.frame(article = article, context = NA, expanded_context = NA, name = name, Type = type, link_to = link_to))
  }
}

# Apply the function and expand the data frame
experiments_2sentence_window <- data.frame(
  articles = experiments_2sentence_window$article,
  Name = stri_enc_toutf8(experiments_2sentence_window$Name),
  Type = experiments_2sentence_window$Type,
  link_to = experiments_2sentence_window$link_to,
  stringsAsFactors = FALSE)
experiments_2sentence_window <- experiments_2sentence_window %>%
  rowwise() %>%
  do(extract_and_expand(.$articles, .$Name, .$Type, .$link_to)) %>%
  ungroup()

# Remove duplicate rows
experiments_2sentence_window <- experiments_2sentence_window %>% distinct()

# Display the result
#View(experiments_2sentence_window)
#experiments_2sentence_window <- experiments_2sentence_window %>% filter(link_to!=1)
table(is.na(experiments_2sentence_window$context))
table(is.na(experiments_2sentence_window$expanded_context))
research_na <- experiments_2sentence_window %>% filter(is.na(expanded_context)==TRUE) # as a solution, I can manually add everythin
unique(experiments_2sentence_window$name)
unique(research_na$name)
experiments_2sentence_window <- anti_join(experiments_2sentence_window, research_na, by = "link_to") # think about this command
experiments_2sentence_window$expanded_context <- gsub(x = experiments_2sentence_window$expanded_context, pattern = "..", ".", fixed = TRUE )
experiments_2sentence_window$expanded_context <- gsub(x = experiments_2sentence_window$expanded_context, pattern = "!.", ".", fixed = TRUE )
experiments_2sentence_window$expanded_context <- gsub(x = experiments_2sentence_window$expanded_context, pattern = "?.", ".", fixed = TRUE )


set.seed(123)
validation_100 <- experiments_2sentence_window %>% slice_sample(n = 100)
n_distinct(validation_100$link_to)
validation_100$id <- seq.int(nrow(validation_100))
save(validation_100, file = "validation_100.RData")
write_xlsx(validation_100, "validation_tonline.xlsx") 




# the second approach with tokenizers. ------------------------------------

library(tokenizers)


extract_and_expand_2 <- function(article, name, type, link_to) {
  # Split the article into sentences
  sentences <- unlist(tokenize_sentences(article))
  
  # Find the positions of sentences containing the name, ignoring case
  name_positions <- grep(name, sentences, ignore.case = TRUE)
  
  # Extract contexts if name positions are found
  contexts <- lapply(name_positions, function(pos) {
    start <- max(1, pos - 2)
    end <- min(length(sentences), pos + 2)
    context <- sentences[pos]
    expanded_context <- paste(sentences[start:end], collapse = ".")
    data.frame(article = article, context = context, expanded_context = expanded_context, name = name, Type = type, link_to = link_to)
  })
  
  # Return the extracted contexts or a dataframe with NAs if no matches are found
  if (length(contexts) > 0) {
    return(do.call(rbind, contexts))
  } else {
    return(data.frame(article = article, context = NA, expanded_context = NA, name = name, Type = type, link_to = link_to))
  }
}




# test --------------------------------------------------------------------

extract_and_expand_2 <- function(article, name, type, link_to) {
  # Ensure that the article is not empty and is a character vector
  if (!is.character(article) || length(article) != 1) {
    stop("The article must be a non-empty single string.")
  }
  
  # Tokenize the article into sentences
  sentences <- unlist(tokenize_sentences(article))
  
  # Find the positions of sentences containing the name, ignoring case
  name_positions <- grep(name, sentences, ignore.case = TRUE)
  
  # Extract contexts if name positions are found
  if (length(name_positions) > 0) {
    contexts <- lapply(name_positions, function(pos) {
      # Define start and end of the expanded context window
      start <- max(1, pos - 2)
      end <- min(length(sentences), pos + 2)
      
      # Extract the context and expanded context
      context <- sentences[pos]
      expanded_context <- paste(sentences[start:end], collapse = " ")
      
      # Create a data frame with the extracted data
      data.frame(article = article, 
                 context = context, 
                 expanded_context = expanded_context, 
                 name = name, 
                 Type = type, 
                 link_to = link_to,
                 stringsAsFactors = FALSE)
    })
    
    # Combine all the contexts into a single data frame
    return(do.call(rbind, contexts))
  } else {
    # Return a data frame with NAs if no matches are found
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






