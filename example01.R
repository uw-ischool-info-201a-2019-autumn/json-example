# Pretty JSON

#----------------------------------------------
# You will need a JSON viewer so that JSON markup 
# is formatted.  Here is what I use with Atom:
# https://atom.io/packages/pretty-json
#
# You need to install it into Atom
#----------------------------------------------

#----------------------------------------------
# You need to use .gitignore which tells git 
# to IGNORE some files - that is, the files in 
# .gitignore will NOT BE added, committed, or 
# pushed.
#----------------------------------------------

#----------------------------------------------
# You need to decide on a service for getting 
# get "feeds" or data.  For example, the New 
# York Time has a number of truly remarkable 
# API.  See: 
# https://developer.nytimes.com/
#
# I have a developer API key from them; you 
# can get one tool
#----------------------------------------------

#install.packages("httr")
#install.packages("jsonlite")
library("httr")
library("jsonlite")

#----------------------------------------------
# Make a URI
# Note: endpoint needs a beginning forward slash (/)
# For example: 
# base_uri  ~~ "https://api.github.com"
# endpoint  ~~ "/search/repositories"
#----------------------------------------------
make_resource_uri <- function(base_uri, endpoint) {
  return(paste0(base_uri,endpoint))
}

#----------------------------------------------
# Get data and so some data wrangling 
#----------------------------------------------
call_uri <- function(base_uri, endpoint, query_params) {
  resource_uri <- make_resource_uri(base_uri,endpoint)
  response <- GET(resource_uri, query=query_params)
  response_text <- content(response, type="text")
  response_data <- fromJSON(response_text)
  return (response_data)
}

#----------------------------------------------
# A search on GitHub  (see chap. 14)
#----------------------------------------------
get_github_df_for_dply_search <- function () {
  query_params <- list(q = "dply", sort = "forks")
  base_uri <-  "https://api.github.com"
  endpoint <- "/search/repositories"
  response_data <- call_uri(base_uri, endpoint, query_params)
  df <- response_data$items
  return(df)
}

df <- get_github_df_for_dply_search()
View(df)