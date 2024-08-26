library(here)
library(tidyverse)
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



