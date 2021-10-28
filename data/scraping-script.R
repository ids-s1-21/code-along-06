library(tidyverse)
library(httr)
library(jsonlite)

res <- GET(
  "https://en.wikipedia.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Countries_in_Europe&cmlimit=100&format=json"
)

temp <- res$content %>%
  rawToChar() %>%
  fromJSON()

europe_data_initial <- temp$query$categorymembers

europe_data <- europe_data_initial %>%
  as_tibble() %>%
  filter(
    !str_starts(title, pattern = "Category:"),
    !(title %in% c("Post-Soviet states", "European microstates"))
  ) %>%
  mutate(url_title = str_replace_all(title, pattern = " ", replacement = "_"))