# This script intends to test various news APIs and scrape their top news stories related to infrastrcture investment by sectors as preliminary research on sentiment analysis
# APIs tested: Google, Yahoo News, Financial Times (TBD)


## Loading packages
library(dplyr) # for pipes and the data_frame function
library(rvest) # webscraping
library(stringr)
library(newsanchor)

# Google News API

# Google API Key: 11bf12dbb1fb4028b1654a2419d3444e
# Google News API Bible: https://github.com/mkearney/newsAPI


# Build a function to get news
 
api_news <- function(kw, news){
# extracting the whole website
NEWSAPI_KEY <- "11bf12dbb1fb4028b1654a2419d3444e"
url <- paste0("https://newsapi.org/v2/everything?q=",kw,"&sources=",news,"&pageSize=100&apiKey=",NEWSAPI_KEY)

## save to .Renviron file
#cat(
#  paste0("NEWSAPI_KEY=", NEWSAPI_KEY),
#  append = TRUE,
#  fill = TRUE,
#  file = file.path("~", ".Renviron")
#)

#if (!"devtools" %in% installed.packages()) {
#  install.packages("devtools")
#}
#devtools::install_github("mkearney/newsAPI")


# save the api_key in the .Renviron file
# set_api_key(api_key = "11bf12dbb1fb4028b1654a2419d3444e",path = "~/.Renviron")

headlines <- read_html(url) %>%
  html_text()

# split the long string by some identifiers to detect breakers between different headlines
test <- strsplit(headlines, "\"source\":")

# title and description as identifyer breakers
res <- str_match(test[[1]][2], "title\":\"(.*?)\",\"description")
topic <- lapply(test[[1]],function(x)str_match(x,"title\":\"(.*?)\",\"description")[,2]) %>% unlist()
description <- lapply(test[[1]],function(x)str_match(x,"description\":\"(.*?)\",\"url\"")[,2]) %>% unlist()
source_name <- lapply(test[[1]],function(x)str_match(x,"\"name\":\"(.*?)\"\\},\"author")[,2]) %>% unlist() # need to add \\ before {} braces in order to capture them
timepublished <- lapply(test[[1]],function(x)str_match(x,"publishedAt\":\"(.*?)\",\"content")[,2]) %>% unlist()
content <- lapply(test[[1]],function(x)str_match(x,"content\":\"(.*?)\\[+")[,2]) %>% unlist()

complete <- data.frame(
  topic = topic,
  description = description,
  source = source_name,
  time = timepublished,
  content =content,
  stringsAsFactors = F
)
complete

}

# write a loop to construct full dataset
api_source <- c("bbc-news",'the-new-york-times','abc-news',"the-wall-street-journal","bloomberg","google-news","reuters","associated-press")
keyword <- c("clean-energy","renewable","renewables","green-energy","solar","wind")
# ??????????????????,????????????????????????????????????Key Word

dat <- data.frame()

for (i in api_source){
  for (j in keyword){
  temp <- api_news(j,i)
  dat <- rbind(dat, temp)
  }
  }

# save the data in case further block
test1 <- dat


