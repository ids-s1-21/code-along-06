library(tidyverse)
library(rvest) #For scraping and related processes
library(robotstxt) #For checking if scraping is allowed

#First check if scraping is allowed.
paths_allowed(
  "https://www.hsm.ox.ac.uk/collections-online#/search/simple-search/*/%257B%257D/1/96/catalogue"
)
# Returns TRUE

#The rest of this file will be filled in during the live code-along on Thursday.
