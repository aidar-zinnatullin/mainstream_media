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

covid_tonline_26 <- covid_tonline[c(28401:29400),]
covid_tonline_26$lemmatized_text <- NULL
write_csv(covid_tonline_26, file = "csv_for_ner_vienna_26.csv")


covid_tonline_27 <- covid_tonline[c(29401:30166),]
covid_tonline_27$lemmatized_text <- NULL
write_csv(covid_tonline_27, file = "csv_for_ner_vienna_27.csv")

# combine all -------------------------------------------------------------
output_1 <- read_csv("ner_output_1.csv")
output_2 <- read_csv("ner_output_2.csv")
output_3 <- read_csv("ner_output_3.csv")
output_4 <- read_csv("output_test.csv")
output_5 <- read_csv("output_5.csv")
output_6 <- read_csv("output_6.csv")
output_7 <- read_csv("output_7.csv")
output_8 <- read_csv("output_8.csv")
output_9 <- read_csv("output_9.csv")
output_10 <- read_csv("output_10.csv")
output_11 <- read_csv("output_11.csv")
output_12 <- read_csv("output_12.csv")
output_13 <- read_csv("output_13.csv")
output_14 <- read_csv("output_14.csv")
output_15 <- read_csv("output_15.csv")
output_16 <- read_csv("output_16.csv")
output_17 <- read_csv("output_17.csv")
output_18 <- read_csv("output_18.csv")
output_19 <- read_csv("output_19.csv")
output_20 <- read_csv("output_20.csv")
output_21 <- read_csv("output_21.csv")
output_22 <- read_csv("output_22.csv")
output_23 <- read_csv("output_23.csv")
output_24 <- read_csv("output_24.csv")
output_25 <- read_csv("output_25.csv")
output_26 <- read_csv("output_26.csv")
output_27 <- read_csv("output_27.csv")

overall_ner <- rbind(output_1, output_2, output_3, output_4, output_5, output_6, output_7, output_8, output_9, output_10, output_11, output_12, output_13, output_14, output_15, output_16, output_17, output_18, output_19, output_20, output_21, output_22, output_23, output_24, output_25, output_26, output_27)
nrow(overall_ner)==nrow(covid_tonline)
write_csv(overall_ner, file = "overall_ner.csv")



