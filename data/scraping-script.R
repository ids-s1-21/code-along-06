library(tidyverse)
library(httr)
library(jsonlite)
library(here)

#We first get the articles in the "Countries of Europe" category

res <- GET(
  "https://en.wikipedia.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:Countries_in_Europe&cmlimit=100&format=json"
)
#See Wikipedia's documentation:
#https://www.mediawiki.org/wiki/API:Categorymembers

#Standard three steps for extracting the resulting JSON into a list:

temp <- res$content %>%
  rawToChar() %>%
  fromJSON()

#Type `temp` into console to see what we need to extract
europe_data_initial <- temp$query$categorymembers

europe_data <- europe_data_initial %>%
  as_tibble() %>%
  filter( #Remove non-(articles for countries)
    !str_starts(title, pattern = "Category:"),
    !(title %in% c("Post-Soviet states", "European microstates"))
  ) %>%
  #Create new column with underscores instead of spaces
  mutate(
    page_url_name = str_replace_all(title, pattern = " ", replacement = "_")
  )

#We could have done this in a single pipeline using functions from `magrittr`
# europe_data <- res$content %>%
#   rawToChar() %>%
#   fromJSON() %>%
#   extract2("query") %>%
#   extract2("categorymembers") %>%
#   filter(
#     !str_detect(title, "^Category:"),
#     !(title %in% c("Post-Soviet states", "European microstates"))
#   ) %>%
#   mutate(page_url_name = str_replace_all(title, " ", "_"))

#We now want page views for each article, using a different API: see
#https://wikitech.wikimedia.org/wiki/Analytics/AQS/Pageviews
#We can write a function to make the query of the form we need, for page views
#of an article in 2020

make_query <- function(x) {
  paste(
    "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/en.wikipedia/all-access/all-agents/",
    x,
    "/daily/20200101/20201231",
    sep = ""
  )
}

europe_data <- europe_data %>%
  mutate(query = make_query(page_url_name))

#Let's do a test with one article:
qq <- europe_data %>%
  slice(1) %>%
  pull(query)

res <- GET(qq)

#Standard lines again:
temp <- res$content %>%
  rawToChar() %>%
  fromJSON()

#Inspecting `temp` reveals that `temp$items` extracts the data we want.
#Let's write a function that takes a query and returns a tibble.

get_page_views <- function(query) {
  res <- GET(query)
  temp <- res$content %>%
    rawToChar() %>%
    fromJSON()
  temp$items %>%
    as_tibble() #returns this
}

#Testing: run
# get_page_views(qq)

#We now want to apply that function to everything in the `query` column of
#the `europe_data` dataset.  We need one of the `map_` functions for this.

#The output in each case is going to be a tibble.  We want to bind those
#tibble together by rows.  For this we need `map_dfr`.

views_data <- europe_data %>%
  pull(query) %>%
  map_dfr(get_page_views)

#Finally, we save the result so we can access it in our .Rmd file.  We might
#as well save the `europe_data` tibble as well.

saveRDS(europe_data, here("data/countries-in-europe.Rds"))
saveRDS(views_data, here("data/country-article-views.Rds"))
