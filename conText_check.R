library(data.table)
library(tidyverse)
library(readr)
library(ggplot2)
library(here)
library(conText)
library(quanteda)
library(dplyr)
library(data.table)
library(readr)
library(ggplot2)
library(here)

load("covid_tonline.RData")
names(covid_tonline)
str(covid_tonline)


data <- corpus(covid_tonline, text_field = "article")
corpus_tonline <- corpus(data) %>% 
  corpus_trim(what = "documents",
              min_ntoken = 10)

toks <- tokens(corpus_tonline, remove_punct=T, remove_symbols=T) %>% 
  tokens_tolower()

# without stops
toks_nostop <- tokens_select(toks, pattern = stopwords("de", source = "marimo"), 
                                        selection = "remove", min_nchar=3)

# only use features that appear at least 10 times in the corpus
feats <- dfm(toks_nostop, tolower=T, verbose = FALSE) %>%
  dfm_trim(min_termfreq = 10) %>% featnames()
toks_nostop <- tokens_select(toks_nostop, feats, padding = TRUE)

save(toks_nostop, corpus_tonline, toks, feats, file = "scrutinize_fasttext/everything_t_online.RData")


# ConText Analysis---------------------------------------------

transform <- readRDS(here("..", "..", "Telegram in Germany Corona related topics", "fastText", "fasttext_transform_dewiki_25.rds"))
not_all_na <- function(x) any(!is.na(x))
fasttext <-  setDT(read_delim(here("..", "..", "Telegram in Germany Corona related topics", "fastText", "fasttext_vectors_dewiki.vec"),
                              delim = " ",
                              quote = "",
                              skip = 1,
                              col_names = F,
                              col_types = cols())) %>%
  dplyr::select(where(not_all_na)) # remove last column which is all NA
word_vectors <-  as.matrix(fasttext, rownames = 1)                                                                                                                         
colnames(word_vectors) = NULL
rm(fasttext)



target_toks_massnahme <- tokens_context(x = toks_nostop, pattern = c("maßnahme*"), window = 5L)
model_dfm_massnahme <- dfm(target_toks_massnahme)

model_dem_massnahme <- dem(x = model_dfm_massnahme, pre_trained = word_vectors, transform = TRUE, transform_matrix = transform, verbose = TRUE)

model_wv_massnahme <- dem_group(model_dem_massnahme, groups = model_dem_massnahme@docvars$date_to)

dataa_1 <- cos_sim(model_wv_massnahme, pre_trained = word_vectors, features = c("protest", 'unterstützung'), as_list = FALSE)
dataa_1$target <-as.Date(dataa_1$target, format = "%Y-%m-%d")

dataa_1 <- dataa_1 %>%  mutate(week = floor_date(as.Date(target), unit = "week"))
table(is.na(dataa_1$week))


checks_massnahme <- dataa_1 %>% group_by(feature, week) %>% summarise(s_mean = mean(value))


candidate_colors <- c(
  'unterstützung' = 'green',  # green
  'protest' = '#d62728'        # red
)
unique(checks_massnahme$week)
names(checks_massnahme)[1] <- "Candidate Words"
ggplot(checks_massnahme, aes(x = week, y = s_mean, color = `Candidate Words`)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  scale_x_date(date_breaks = '6 month', date_labels = "%Y-%m") +
  ylim(0, .5) +
  #xlim(as.Date("2020-01-12"), as.Date("2020-09-06"))+
  geom_vline(xintercept = as.Date("2020-04-01"), linetype = "dotted", color = "slateblue4") +
  annotate("text", label= "Heinsberg study by Hendrik Streeck, 06.04.2020", x = as.Date("2021-01-01"), y = 0.4, vjust = -1, color = "slateblue4") +
  
  labs(y = "Cosine Similarity of Covid/Corona with Candidate Words", x = "Year-Month") +
  scale_color_manual(values = candidate_colors) +
  theme_classic()

ggsave(here("Figures", "conspiracy",  "Corona_Covid_conspiracy.jpeg"), width = 9, height = 6, dpi = 300)
