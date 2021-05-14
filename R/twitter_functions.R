#' Embed tweet
#'
#' @param id status id of the tweet to embed
#'
#' @return html embeddable tweet, e.g. inside a [shiny::renderUI()] 
#'
#' @examples
#' embed_tweet("266031293945503744")
#' 
#' @importFrom htmltools HTML
#' @importFrom jsonlite fromJSON
#' 
#' @export
#' 
embed_tweet <- function(id) {
  
  # url to produce json of tweet given the tweet id
  url <- paste0("https://publish.twitter.com/oembed?url=https%3A%2F%2Ftwitter.com%2FInterior%2Fstatus%2F", id)
  # convert the json to a list
  jsonlite::validate(url)
  tweet_list <- jsonlite::fromJSON(url)
  # convert the tweet url to HTML
  tweet <- htmltools::HTML(tweet_list$html)
  
  tweet
}

#' @param tweets tweet ids
#' @param n number of tweets
#' @param var variable to filter on
#'
#' @importFrom rlang enquo
#' 
most <- function(tweets, n, var) {
  
  var <- rlang::enquo(var)
  
  tweets %>% 
    dplyr::top_n(n, !!var ) %>% 
    dplyr::arrange(desc(favorite_count)) %>% 
    dplyr::pull(status_id)
}

#' most popular tweets
#'
#' @param tweets tweets data
#' @param n number of tweets to extract
#'
#' @export
#'
#' @importFrom dplyr top_n arrange desc pull
most_popular <- function(tweets, n = 10) {
  most(tweets, n, favorite_count)
}

#' @rdname most_popular
#' @export
most_retweeted <- function(tweets, n = 10) {
  most(tweets, n, retweet_count)
}

#' @rdname most_popular
#' @export
#' @importFrom utils head
most_recent <- function(tweets, n = 10) {
  tweets %>% 
    dplyr::arrange(desc(created_at)) %>% 
    head(n) %>% 
    dplyr::pull(status_id)
}
