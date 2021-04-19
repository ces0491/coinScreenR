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
  url <- paste0( "https://publish.twitter.com/oembed?url=https%3A%2F%2Ftwitter.com%2FInterior%2Fstatus%2F", id)
  tweet <- htmltools::HTML(jsonlite::fromJSON(url)$html)
  
  class(tweet) <- c("tweet", class(tweet))
  tweet
}

#' @importFrom htmltools html_print
#' @export
print.tweet <- function(x, ...) {
  htmltools::html_print(x)
  invisible(x)
}


#' @importFrom rlang enquo
most <- function(tweets, n = 10, var) {
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

#' @importFrom dplyr bind_rows
update_search <- function( data, ... ){
  res <- search_tweets( ... )
  bind_rows( filter( data, ! status_id %in% res$status_id  ), res )
}