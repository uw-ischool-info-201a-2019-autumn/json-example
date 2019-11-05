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

# install.packages("httr")
# install.packages("jsonlite")
library("httr")
library("jsonlite")
library("stringr")

setwd("~/Documents/__INFO-201/12code/JSON/json-example")
source("nytimes_api_key.R")

#----------------------------------------------
# Make a URI (unified resource identify)
#----------------------------------------------
make_resource_uri <- function(base_uri, endpoint) {
  return(paste0(base_uri, endpoint))
}

#----------------------------------------------
# Make call to a URI and process the data
#----------------------------------------------
call_uri <- function(base_uri, endpoint, query_params) {
  # Make the uri string
  resource_uri <- make_resource_uri(base_uri, endpoint)

  # Send the query off with uri string and the query paramters
  response <- GET(resource_uri, query = query_params)

  # Get the response back - as a long character string
  # We are assuming that everything works - which is a very big assumption
  response_text <- content(response, type = "text")

  # Convert the text string into an R object that can be processed in R
  response_data <- fromJSON(response_text)
  return(response_data)
}

#----------------------------------------------
# https://api.nytimes.com/svc/books/v3/lists/current/hardcover-fiction.json?api-key=XXX
# Note: This value "api-key" is a problem for R.  Why? What is the solution?
# Search on: "r list identifier with dash"
# See: https://stackoverflow.com/questions/36312953/including-a-dash-in-an-argument-name-in-r
#----------------------------------------------
make_basic_nytimes_list_query <- function(book_list_name="hardcover-fiction") {
  query_params <- list("api-key" = NYTIMES_KEY)
  base_uri <- "https://api.nytimes.com/svc/books/v3/lists/current"
  endpoint <- paste0("/",book_list_name,".json")
  response_data <- call_uri(base_uri, endpoint, query_params)

  # By inspection with str() we determined that books is a data frame that we can work with
  df <- response_data$results$books
  return(df)
}

book_list_query_to_title <- function(query) {
  t <- str_replace_all(query,"-"," ")
  t <- str_to_title(t)
  return(t)
}

make_book_list_names_query <- function() {
  query_params <- list("api-key" = NYTIMES_KEY)
  base_uri <- "https://api.nytimes.com/svc/books/v3/lists"
  endpoint <- "/names"
  response_data <- call_uri(base_uri, endpoint, query_params)
  # The following conversion is required for making the queries
  df <- response_data$results$list_name
  df <- tolower(df)
  df <- str_replace_all(df," ","-")
  return(df)
}

get_random_book_list_query <- function() {
  query_list <- make_book_list_names_query()
  n <- sample(1:length(query_list),1,replace=TRUE)
  book_list_name <- query_list[n]
  return(book_list_name)
}

#----------------------------------------------
# Create a simple bibliograhic item
#----------------------------------------------
get_book_bib_item <- function(book_rank, book_list) {

  # If there is nothing in the data frame, book, return immediately
  book <- book_list[book_list$rank == book_rank, ]
  if (is.null(book) | nrow(book) == 0) {
    return("")
  }

  # Extact some basic info about the book
  b_title <- book$title
  b_author <- book$author
  b_review_link <- book$book_review_link
  b_descr <- book$description

  # Create a review string if necessary
  bib_title <- ""
  if (b_review_link != "") {
    bib_title <- paste0("\n* _", b_title, "_ by ", b_author, " (<a href='", b_review_link, "'>Review</a>)", collapse="")
  }
  else {
    bib_title <- paste0("\n* _",b_title, "_ by ", b_author,collapse="")
  }

  # Add the description to the bibliograhic item
  bib_title <- paste0(bib_title, "<br>", b_descr, "\n", collapse="")

  return(bib_title)
}

get_nytimes_book_by_rank <- function(rank) {
  book_list <- make_basic_nytimes_list_query()
  return (get_book_bib_item(rank, book_list))
}

get_nytimes_top_x_book_list <- function(num, book_list_name="hardcover-fiction") {
  print(book_list_name)
  book_list <- make_basic_nytimes_list_query(book_list_name)
  titles <- lapply(seq(1:num), get_book_bib_item, book_list)
  return(titles)
}

# query_list <- make_book_list_names_query()
# n <- sample(1:length(query_list),1,replace=TRUE)
# book_list_name <- query_list[n]
# t <- get_nytimes_top_x_book_list(10,book_list_name)
# 
# get_nytimes_top_x_book_list(10,"indigenous-americans")
