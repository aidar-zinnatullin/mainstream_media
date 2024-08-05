library(here)
library(tidyverse)
library(LSX)
library(quanteda)
library(xml2)
library(XML)

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
