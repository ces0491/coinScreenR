# twts <- rtweet::search_tweets("BTC", n = 500, include_rts = TRUE, filter = "verified", lang = "en",
#                               token = readRDS(Sys.getenv("TWITTER_PAT")))

tokenize_txt <- function(twts) {
  twt_reqd <- dplyr::select(twts, screen_name, text)
  
  tidy_txt <- twt_reqd %>%
    tidytext::unnest_tweets(output = word, 
                            input = text) %>%
    dplyr::mutate(word_stem = SnowballC::wordStem(word)) %>%
    dplyr::anti_join(tidytext::stop_words, by = "word")
  
  tidy_txt
}

get_top_n <- function(tidy_txt, n_top = 10, searched_ticker) {
  
  top_n <- tidy_txt %>% 
    dplyr::filter(!stringr::str_detect(word, '#')) %>% 
    dplyr::filter(!stringr::str_detect(word, 'amp')) %>% 
    dplyr::filter(!stringr::str_detect(word, searched_ticker)) %>% 
    dplyr::count(word, sort = TRUE) %>% 
    dplyr::top_n(n_top) %>% 
    dplyr::mutate(word = stats::reorder(word, n))
  
  top_n
}

get_sentiment <- function(tidy_txt, n_top = 10) {
  
  bing_word_counts <- tidy_txt %>%
    dplyr::inner_join(tidytext::get_sentiments("bing")) %>% # join sentiment classification to the tweet words
    dplyr::count(word, sentiment, sort = TRUE) %>%
    dplyr::group_by(sentiment) %>%
    dplyr::top_n(n_top) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(word = stats::reorder(word, n))
  
  bing_word_counts
}

twts <- rtweet::search_tweets(q = 'BTC', 
                              n = 500, 
                              include_rts = TRUE,
                              filter = "verified", 
                              lang = "en")

twts_with_pol <- twts %>% 
  dplyr::select(user_id, created_at, screen_name, text) %>% 
  sentimentr::get_sentences() %>% 
  sentimentr::sentiment() %>% 
  dplyr::mutate(polarity_level = ifelse(sentiment < 0.1, "Negative",
                                        ifelse(sentiment > 0.1, "Positive",
                                               "Neutral")))

twts_with_pol %>% 
  dplyr::count(sentiment, polarity_level) %>% 
  ggplot2::ggplot() + ggplot2::geom_col(ggplot2::aes(x = sentiment, y = n, fill = polarity_level)) +
  ggplot2::theme_minimal()

twts_with_pol %>% 
  ggplot2::ggplot() + ggplot2::geom_boxplot(ggplot2::aes(y = polarity_level, x = sentiment))


twts_with_pol %>% 
  sentimentr::sentiment_by(by = NULL) %>% #View()
  ggplot2::ggplot() + ggplot2::geom_density(ggplot2::aes(ave_sentiment))
