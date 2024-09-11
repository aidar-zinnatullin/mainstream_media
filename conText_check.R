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
  #geom_vline(xintercept = as.Date("2020-04-01"), linetype = "dotted", color = "slateblue4") +
  #annotate("text", label= "Heinsberg study by Hendrik Streeck, 06.04.2020", x = as.Date("2021-01-01"), y = 0.4, vjust = -1, color = "slateblue4") +
  
  labs(y = "Cosine Similarity of Measure with Candidate Words: Support vs Protest", x = "Year-Month") +
  scale_color_manual(values = candidate_colors) +
  theme_minimal()

ggsave(here("Figures",  "massnahmen_support_protest.jpeg"), width = 9, height = 6, dpi = 300)





# specific measure and support vs criticism -------------------------------

target_toks_specific <- tokens_context(x = toks_nostop, pattern = c("maßnahme*", "lockdown", "impfung*", "einreisebeschränkung*",
                                                                     "schulschließung*", "masken"), window = 5L)
model_dfm_specific <- dfm(target_toks_specific)

model_dem_specific <- dem(x = model_dfm_specific, pre_trained = word_vectors, transform = TRUE, transform_matrix = transform, verbose = TRUE)

model_wv_specific <- dem_group(model_dem_specific, groups = model_dem_specific@docvars$date_to)

dataa_2 <- cos_sim(model_wv_specific, pre_trained = word_vectors, features = c("kritik", 'unterstützung'), as_list = FALSE)
dataa_2$target <-as.Date(dataa_2$target, format = "%Y-%m-%d")

dataa_2 <- dataa_2 %>%  mutate(week = floor_date(as.Date(target), unit = "week"))
table(is.na(dataa_2$week))


checks_specific <- dataa_2 %>% group_by(feature, week) %>% summarise(s_mean = mean(value))


candidate_colors <- c(
  'unterstützung' = 'green',  # green
  'kritik' = '#d62728'        # red
)
unique(checks_specific$week)
names(checks_specific)[1] <- "Candidate Words"
ggplot(checks_specific, aes(x = week, y = s_mean, color = `Candidate Words`)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  scale_x_date(date_breaks = '6 month', date_labels = "%Y-%m") +
  ylim(0, .5) +
  #xlim(as.Date("2020-01-12"), as.Date("2020-09-06"))+
  #geom_vline(xintercept = as.Date("2020-04-01"), linetype = "dotted", color = "slateblue4") +
  #annotate("text", label= "Heinsberg study by Hendrik Streeck, 06.04.2020", x = as.Date("2021-01-01"), y = 0.4, vjust = -1, color = "slateblue4") +
  
  labs(y = "Cosine Similarity of Measures with Candidate Words: Criticism vs Protest", x = "Year-Month") +
  scale_color_manual(values = candidate_colors) +
  theme_minimal()

ggsave(here("Figures",  "specific_support_criticism.jpeg"), width = 9, height = 6, dpi = 300)



# focus on only one policy measure: impfungen -----------------------------


target_toks_vaccination <- tokens_context(x = toks_nostop, pattern = c("impfung*"), window = 5L)
model_dfm_vaccination <- dfm(target_toks_vaccination)

model_dem_vaccination <- dem(x = model_dfm_vaccination, pre_trained = word_vectors, transform = TRUE, transform_matrix = transform, verbose = TRUE)

model_wv_vaccination <- dem_group(model_dem_vaccination, groups = model_dem_vaccination@docvars$date_to)

dataa_3 <- cos_sim(model_wv_vaccination, pre_trained = word_vectors, features = c("kritik", 'unterstützung'), as_list = FALSE)
dataa_3$target <-as.Date(dataa_3$target, format = "%Y-%m-%d")

dataa_3 <- dataa_3 %>%  mutate(week = floor_date(as.Date(target), unit = "week"))
table(is.na(dataa_3$week))


checks_vaccination <- dataa_3 %>% group_by(feature, week) %>% summarise(s_mean = mean(value))


candidate_colors <- c(
  'unterstützung' = 'green',  # green
  'kritik' = '#d62728'        # red
)
unique(checks_vaccination$week)
names(checks_vaccination)[1] <- "Candidate Words"
ggplot(checks_vaccination, aes(x = week, y = s_mean, color = `Candidate Words`)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  scale_x_date(date_breaks = '6 month', date_labels = "%Y-%m") +
  ylim(0, 0.5) +
  #xlim(as.Date("2020-01-12"), as.Date("2020-09-06"))+
  #geom_vline(xintercept = as.Date("2020-04-01"), linetype = "dotted", color = "slateblue4") +
  #annotate("text", label= "Heinsberg study by Hendrik Streeck, 06.04.2020", x = as.Date("2021-01-01"), y = 0.4, vjust = -1, color = "slateblue4") +
  
  labs(y = "Cosine Similarity of Vaccination with Candidate Words: Criticism vs Protest", x = "Year-Month") +
  scale_color_manual(values = candidate_colors) +
  theme_minimal()

ggsave(here("Figures",  "vaccination_support_criticism.jpeg"), width = 9, height = 6, dpi = 300)




# focus on only one policy measure: masks -----------------------------


target_toks_masks <- tokens_context(x = toks_nostop, pattern = c("mask*"), window = 5L)
model_dfm_masks <- dfm(target_toks_masks)

model_dem_masks <- dem(x = model_dfm_masks, pre_trained = word_vectors, transform = TRUE, transform_matrix = transform, verbose = TRUE)

model_wv_masks <- dem_group(model_dem_masks, groups = model_dem_masks@docvars$date_to)

dataa_4 <- cos_sim(model_wv_masks, pre_trained = word_vectors, features = c("kritik", 'unterstützung'), as_list = FALSE)
dataa_4$target <-as.Date(dataa_4$target, format = "%Y-%m-%d")

dataa_4 <- dataa_4 %>%  mutate(week = floor_date(as.Date(target), unit = "week"))
table(is.na(dataa_4$week))


checks_masks <- dataa_4 %>% group_by(feature, week) %>% summarise(s_mean = mean(value))


candidate_colors <- c(
  'unterstützung' = 'green',  # green
  'kritik' = '#d62728'        # red
)
unique(checks_masks$week)
names(checks_masks)[1] <- "Candidate Words"
ggplot(checks_masks, aes(x = week, y = s_mean, color = `Candidate Words`)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  scale_x_date(date_breaks = '6 month', date_labels = "%Y-%m") +
  ylim(0, 0.5) +
  #xlim(as.Date("2020-01-12"), as.Date("2020-09-06"))+
  #geom_vline(xintercept = as.Date("2020-04-01"), linetype = "dotted", color = "slateblue4") +
  #annotate("text", label= "Heinsberg study by Hendrik Streeck, 06.04.2020", x = as.Date("2021-01-01"), y = 0.4, vjust = -1, color = "slateblue4") +
  
  labs(y = "Cosine Similarity of Masks with Candidate Words: Criticism vs Protest", x = "Year-Month") +
  scale_color_manual(values = candidate_colors) +
  theme_minimal()

ggsave(here("Figures",  "masks_support_criticism.jpeg"), width = 9, height = 6, dpi = 300)




# focus on only one policy measure: lockdown -----------------------------


target_toks_lockdown <- tokens_context(x = toks_nostop, pattern = c("lockdown*"), window = 5L)
model_dfm_lockdown <- dfm(target_toks_lockdown)

model_dem_lockdown <- dem(x = model_dfm_lockdown, pre_trained = word_vectors, transform = TRUE, transform_matrix = transform, verbose = TRUE)

model_wv_lockdown <- dem_group(model_dem_lockdown, groups = model_dem_lockdown@docvars$date_to)

dataa_5 <- cos_sim(model_wv_lockdown, pre_trained = word_vectors, features = c("kritik", 'unterstützung'), as_list = FALSE)
dataa_5$target <-as.Date(dataa_5$target, format = "%Y-%m-%d")

dataa_5 <- dataa_5 %>%  mutate(week = floor_date(as.Date(target), unit = "week"))
table(is.na(dataa_5$week))


checks_lockdown <- dataa_5 %>% group_by(feature, week) %>% summarise(s_mean = mean(value))


candidate_colors <- c(
  'unterstützung' = 'green',  # green
  'kritik' = '#d62728'        # red
)
unique(checks_lockdown$week)
names(checks_lockdown)[1] <- "Candidate Words"
ggplot(checks_lockdown, aes(x = week, y = s_mean, color = `Candidate Words`)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  scale_x_date(date_breaks = '6 month', date_labels = "%Y-%m") +
  ylim(0, 0.5) +
  #xlim(as.Date("2020-01-12"), as.Date("2020-09-06"))+
  #geom_vline(xintercept = as.Date("2020-04-01"), linetype = "dotted", color = "slateblue4") +
  #annotate("text", label= "Heinsberg study by Hendrik Streeck, 06.04.2020", x = as.Date("2021-01-01"), y = 0.4, vjust = -1, color = "slateblue4") +
  
  labs(y = "Cosine Similarity of Lockdown with Candidate Words: Criticism vs Protest", x = "Year-Month") +
  scale_color_manual(values = candidate_colors) +
  theme_minimal()

ggsave(here("Figures",  "lockdown_support_criticism.jpeg"), width = 9, height = 6, dpi = 300)

