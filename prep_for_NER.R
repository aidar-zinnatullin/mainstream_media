library(tidyverse)
load("covid_tonline.RData")
nrow(covid_tonline)
head(covid_tonline)

30166/4
covid_tonline_1 <- covid_tonline[c(1:5000),]
covid_tonline_1$lemmatized_text <- NULL
write_csv(covid_tonline_1, file = "csv_for_ner_vienna_1.csv")


covid_tonline_2 <- covid_tonline[c(5001:6000),]
covid_tonline_2$lemmatized_text <- NULL
write_csv(covid_tonline_2, file = "csv_for_ner_vienna_2.csv")


covid_tonline_3 <- covid_tonline[c(6001:7000),]
covid_tonline_3$lemmatized_text <- NULL
write_csv(covid_tonline_3, file = "csv_for_ner_vienna_3.csv")

covid_tonline_4 <- covid_tonline[c(7001:7400),]
covid_tonline_4$lemmatized_text <- NULL
write_csv(covid_tonline_4, file = "csv_for_ner_vienna_4.csv")

# for HPC -----------------------------------------------------------------

covid_tonline_5 <- covid_tonline[c(7401:14802),]
covid_tonline_5$lemmatized_text <- NULL
write_csv(covid_tonline_5, file = "csv_for_ner_vienna_5.csv")

