# Latent Semantic Scaling -------------------------------------------------

# dimension of politicization: 
# corona-protestersstrategically communicate scientific uncertainty after controversial events to mobilize the supporters
library(quanteda)
library(LSX)
library(here)
library(tidyverse)
load("covid_tonline.RData")
names(covid_tonline)
data <- corpus(covid_tonline, text_field = "lemmatized_text")
new_stopwords <- read.table("https://raw.githubusercontent.com/stopwords-iso/stopwords-de/master/stopwords-de.txt") # 563 words
corp_sent <- corpus_reshape(data, to =  "sentences")

toks_sent <- corp_sent %>% 
  tokens(remove_punct = TRUE, remove_symbols = TRUE, 
         remove_numbers = TRUE, remove_url = TRUE,) %>% 
  tokens_remove(stopwords("de", source = "marimo")) %>%
  tokens_remove(c(new_stopwords$V1)) %>% 
  tokens_tolower()

# create a document feature matrix from the tokens object
dfmat_sent <- toks_sent %>% 
  dfm() %>% 
  dfm_remove(pattern = "") %>% 
  dfm_trim(min_termfreq = 10)
topfeatures(dfmat_sent, 20)




negatives <- data.frame(name = c("kontrovers*", "debatte*", "abweichende*", "streiten*",
                                  "spannung", "hitzig", "konflikt*", "umstritten", "streitsüchtig*", "verachtung*", 
                                 "kluft", "zorn", "explodieren*", "ausbrechen*",  "verachtung*", "wütend*", "gegner*", "opposition", 
                                  "konkurrierend"),
                        values = c(-1, -1,-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1))

positives <- data.frame(name = c("unterstützung", "kooperierend", "einvernehmen", "frieden", "respekt", "zusammenführen", "vereinen",
  "beruhigen", "übereinkunft", "friedfertig", "konsens*"), 
  values = c(1,1,1,1,1,1,1,1,1,1,1))


politicization <- rbind(negatives, positives)

politicization <- tibble::deframe(politicization)


tmod_lss <- textmodel_lss(dfmat_sent, seeds = politicization, k = 300, cache = TRUE, simil_method = "cosine")

head(coef(tmod_lss), 20) # most positive words
tail(coef(tmod_lss), 20) # most negative words
dict <- dictionary(list(negative =  c("kontrovers*", "debatte*", "abweichende*", "streiten*",
                                       "spannung", "hitzig", "konflikt*", "umstritten", "streitsüchtig*", "verachtung*", 
                                      "kluft", "zorn", "explodieren*", "ausbrechen*", "verachtung*", "wütend*", "gegner*", "opposition", 
                                      "konkurrierend"),
                        positive = c("unterstützung", "kooperierend", "einvernehmen", "frieden", "respekt", "zusammenführen", "vereinen",
                                      "beruhigen", "übereinkunft", "friedfertig", "konsens*")))


textplot_terms(tmod_lss, highlighted = dict)



jpeg("Figures/Internal_validity_seed.jpeg", width = 9, height = 6, units = 'in', res = 500)
textplot_terms(tmod_lss, highlighted = dict)
dev.off()

# 
dfmat_doc <- dfm_group(dfmat_sent)
dat <- docvars(dfmat_doc)
dat$fit <- predict(tmod_lss, newdata = dfmat_doc)
#names(dat)[2] <- "date"
class(dat$date)
dat$date <- as.Date(dat$date)

dat_smooth <- smooth_lss(dat, engine = "locfit")
head(dat_smooth)

sort(unique(dat$date))
class(dat$date)

ggplot() +
  geom_point(data = dat, aes(x = date, y = fit), color = rgb(0.5, 0.5, 0.5, 0.01), shape = 0.5) +
  geom_line(data = dat_smooth, aes(x = date, y = fit)) +
  geom_line(data = dat_smooth, aes(x = date, y = fit + se.fit * 1.96), linetype = "dotted") +
  geom_line(data = dat_smooth, aes(x = date, y = fit - se.fit * 1.96), linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Time", y = "Sentiment towards Scientific Information about COVID-19") +
  ylim(c(-0.5, 0.5)) +
  #geom_vline(xintercept = as.Date("2021-07-01"), linetype = "dotted", color = "black") +
  #annotate("text", label= "Floods in Germany and the climate debate, July 2021", x = as.Date("2021-07-01"), y = 0.4, vjust = -1, color = "black") +
  
  geom_vline(xintercept = as.Date("2020-11-02"), linetype = "dotted", color = "brown") +
  annotate("text", label= "Lockdown light, 02.11.2020", x = as.Date("2020-11-01"), y = 0.3, vjust = -1, color = "brown") +
  geom_vline(xintercept = as.Date("2020-12-16"), linetype = "dotted", color = "violetred") +
  annotate("text", label= "Hard lockdown made FFP2 masks mandatory on public transport, 16.12.2020", x = as.Date("2021-11-01"), y = 0.45, vjust = -1, color = "violetred") +
  geom_vline(xintercept = as.Date("2020-03-22"), linetype = "dotted", color = "orange") +
  annotate("text", label= "First federal lockdown, 22.03.2020", x = as.Date("2020-08-01"), y = 0.10, vjust = 0.05, color = "orange") +
  geom_vline(xintercept = as.Date("2020-04-06"), linetype = "dotted", color = "slateblue4") +
  annotate("text", label= "Heinsberg study by Hendrik Streeck, 06.04.2020", x = as.Date("2020-11-01"), y = 0.20, vjust = 0.1, color = "slateblue4") +
  scale_x_date(date_breaks = '6 months', date_labels = "%Y-%m") +  
  theme_minimal()
ggsave( here("Figures", "LSS_only_covid_politicization.jpeg"), width = 9, height = 6, units = "in", dpi = 500)



