library(magrittr)
library(tidyverse)
library(httr)
library(jsonlite)

res <- GET(
  "https://en.wikipedia.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Countries_in_Europe&cmlimit=500&format=json"
)
res$content

#The rest of this file will be filled in during the live code-along on Thursday.