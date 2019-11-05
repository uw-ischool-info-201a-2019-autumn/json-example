# filename: nytimes_ap_functions.R
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
# APIs.  See:
# https://developer.nytimes.com/
#
# I have a developer API key from them; you
# can get one too!s
#----------------------------------------------

# install.packages("httr")
# install.packages("jsonlite")
library("httr")
library("jsonlite")
library("stringr")

setwd("~/Documents/__INFO-201/12code/JSON/json-example")
source("nytimes_api_key.R")

#----------------------------------------------
# Make call to a URI and process the data
#----------------------------------------------
call_uri <- function(base_uri, endpoint, query_params) {
  # Make the uri string
  resource_uri <- paste0(base_uri, endpoint)

  # Send the query off with uri string and the query paramters
  # We are assuming that everything works - which is a very big assumption
  # The network could time out - the query could fail - etc.
  # Ideally a lot of error-handing is required here
  response <- GET(resource_uri, query = query_params)

  # Get the response back - as a long character string
  response_text <- content(response, type = "text", encoding = "UTF-8")

  # Convert the text string into an R object that can be processed in R
  response_data <- fromJSON(response_text)
  return(response_data)
}

#----------------------------------------------
# This function get the best selling books for a category,
# passed in as book_list_name. Note: the default is "hardcover-fiction"
# https://api.nytimes.com/svc/books/v3/lists/current/hardcover-fiction.json?api-key=XXX
# Note: This value "api-key" is a problem for R.  Why? What is the solution?
# Search on: "r list identifier with dash"
# See: https://stackoverflow.com/questions/36312953/including-a-dash-in-an-argument-name-in-r
#----------------------------------------------
make_basic_nytimes_list_query <- function(book_list_name = "hardcover-fiction") {
  query_params <- list("api-key" = NYTIMES_KEY)
  base_uri <- "https://api.nytimes.com/svc/books/v3/lists/current"
  endpoint <- paste0("/", book_list_name, ".json")
  response_data <- call_uri(base_uri, endpoint, query_params)

  # By inspection with str() we determined that books is a data frame
  # that we can work with. Note the data frame within the data frame
  df <- response_data$results$books
  return(df)
}
#----------------------------------------------
# This takes a book list query and makes it look
# nice - not perfect but okay for now.
#----------------------------------------------
book_list_query_to_title <- function(query) {
  t <- str_replace_all(query, "-", " ")
  t <- str_to_title(t)
  return(t)
}

#----------------------------------------------
# This will return a list of book lists. For example:
#    hardcover-fiction
#    hardcover-nonfiction
#    trade-fiction-paperback
#    ...
# There are about 50 such book lists.
#----------------------------------------------
make_book_list_names_query <- function() {
  query_params <- list("api-key" = NYTIMES_KEY)
  base_uri <- "https://api.nytimes.com/svc/books/v3/lists"
  endpoint <- "/names"
  response_data <- call_uri(base_uri, endpoint, query_params)
  # The following conversion is required for making the queries
  df <- response_data$results$list_name
  df <- tolower(df)
  df <- str_replace_all(df, " ", "-")
  df <- sort(df)
  return(df)
}

#----------------------------------------------
# Select a random book list - useful for
# testing and fun too
#----------------------------------------------
get_random_book_list_query <- function() {
  query_list <- make_book_list_names_query()
  # Use sample to get a random number beween 1 and X
  n <- sample(1:length(query_list), 1, replace = TRUE)
  book_list_name <- query_list[n]
  return(book_list_name)
}

#----------------------------------------------
# Create a simple bibliograhic item
# Note: By data wrangling the data fields in
# the variable, book, we could add a lot of
# information to this bibiliography record.
# It is meant to be printed in a markdown
# document.
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
    bib_title <- paste0("_", b_title, "_ by ", b_author, " (<a href='", b_review_link, "'>Review</a>)", collapse = "")
  }
  else {
    bib_title <- paste0("_", b_title, "_ by ", b_author, collapse = "")
  }

  # Add the description to the bibliograhic item
  bib_title <- paste0(bib_title, "<br>", b_descr, collapse = "")

  return(bib_title)
}

#----------------------------------------------
# Get the top books for a particular list
#----------------------------------------------
get_nytimes_top_x_book_list <- function(num, book_list_name = "hardcover-fiction") {
  book_list <- make_basic_nytimes_list_query(book_list_name)
  if (nrow(book_list) < num) {
    num <- nrow(book_list)
  }
  titles <- lapply(seq(1:num), get_book_bib_item, book_list)
  # This gives us a bullet list for markdown
  t <- paste("- ", titles, collapse = "\n")
  return(t)
}
