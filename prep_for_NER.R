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

covid_tonline_5 <- covid_tonline[c(7401:8400),]
covid_tonline_5$lemmatized_text <- NULL
write_csv(covid_tonline_5, file = "csv_for_ner_vienna_5.csv")


covid_tonline_6 <- covid_tonline[c(8401:9400),]
covid_tonline_6$lemmatized_text <- NULL
write_csv(covid_tonline_6, file = "csv_for_ner_vienna_6.csv")


covid_tonline_7 <- covid_tonline[c(9401:10400),]
covid_tonline_7$lemmatized_text <- NULL
write_csv(covid_tonline_7, file = "csv_for_ner_vienna_7.csv")


covid_tonline_8 <- covid_tonline[c(10401:11400),]
covid_tonline_8$lemmatized_text <- NULL
write_csv(covid_tonline_8, file = "csv_for_ner_vienna_8.csv")



covid_tonline_9 <- covid_tonline[c(11401:12400),]
covid_tonline_9$lemmatized_text <- NULL
write_csv(covid_tonline_9, file = "csv_for_ner_vienna_9.csv")


covid_tonline_10 <- covid_tonline[c(12401:13400),]
covid_tonline_10$lemmatized_text <- NULL
write_csv(covid_tonline_10, file = "csv_for_ner_vienna_10.csv")



covid_tonline_11 <- covid_tonline[c(13401:14400),]
covid_tonline_11$lemmatized_text <- NULL
write_csv(covid_tonline_11, file = "csv_for_ner_vienna_11.csv")


covid_tonline_12 <- covid_tonline[c(14401:15400),]
covid_tonline_12$lemmatized_text <- NULL
write_csv(covid_tonline_12, file = "csv_for_ner_vienna_12.csv")

covid_tonline_13 <- covid_tonline[c(15401:16400),]
covid_tonline_13$lemmatized_text <- NULL
write_csv(covid_tonline_13, file = "csv_for_ner_vienna_13.csv")


covid_tonline_14 <- covid_tonline[c(16401:17400),]
covid_tonline_14$lemmatized_text <- NULL
write_csv(covid_tonline_14, file = "csv_for_ner_vienna_14.csv")


covid_tonline_15 <- covid_tonline[c(17401:18400),]
covid_tonline_15$lemmatized_text <- NULL
write_csv(covid_tonline_15, file = "csv_for_ner_vienna_15.csv")


covid_tonline_16 <- covid_tonline[c(18401:19400),]
covid_tonline_16$lemmatized_text <- NULL
write_csv(covid_tonline_16, file = "csv_for_ner_vienna_16.csv")


# new batch ---------------------------------------------------------------

covid_tonline_17 <- covid_tonline[c(19401:20400),]
covid_tonline_17$lemmatized_text <- NULL
write_csv(covid_tonline_17, file = "csv_for_ner_vienna_17.csv")


covid_tonline_18 <- covid_tonline[c(20401:21400),]
covid_tonline_18$lemmatized_text <- NULL
write_csv(covid_tonline_18, file = "csv_for_ner_vienna_18.csv")


# three new sets ----------------------------------------------------------

covid_tonline_19 <- covid_tonline[c(21401:22400),]
covid_tonline_19$lemmatized_text <- NULL
write_csv(covid_tonline_19, file = "csv_for_ner_vienna_19.csv")

covid_tonline_20 <- covid_tonline[c(22401:23400),]
covid_tonline_20$lemmatized_text <- NULL
write_csv(covid_tonline_20, file = "csv_for_ner_vienna_20.csv")

covid_tonline_21 <- covid_tonline[c(23401:24400),]
covid_tonline_21$lemmatized_text <- NULL
write_csv(covid_tonline_21, file = "csv_for_ner_vienna_21.csv")

covid_tonline_22 <- covid_tonline[c(24401:25400),]
covid_tonline_22$lemmatized_text <- NULL
write_csv(covid_tonline_22, file = "csv_for_ner_vienna_22.csv")


# two new -----------------------------------------------------------------

covid_tonline_23 <- covid_tonline[c(25401:26400),]
covid_tonline_23$lemmatized_text <- NULL
write_csv(covid_tonline_23, file = "csv_for_ner_vienna_23.csv")

covid_tonline_24 <- covid_tonline[c(26401:27400),]
covid_tonline_24$lemmatized_text <- NULL
write_csv(covid_tonline_24, file = "csv_for_ner_vienna_24.csv")


covid_tonline_25 <- covid_tonline[c(27401:28400),]
covid_tonline_25$lemmatized_text <- NULL
write_csv(covid_tonline_25, file = "csv_for_ner_vienna_25.csv")


