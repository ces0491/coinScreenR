twts <- rtweet::search_tweets("BTC", n = 500, include_rts = TRUE, filter = "verified", lang = "en",
                              token = readRDS(Sys.getenv("TWITTER_PAT")))

twt_reqd <- dplyr::select(twts, screen_name, text)

tidy_txt <- twt_reqd %>%
  tidytext::unnest_tweets(output = word, 
                          input = text) %>%
  dplyr::mutate(word_stem = SnowballC::wordStem(word)) %>%
  dplyr::anti_join(tidytext::stop_words, by = "word")

top10 <- tidy_txt %>% 
  dplyr::filter(!stringr::str_detect(word, '#')) %>% 
  dplyr::filter(!stringr::str_detect(word, 'btc')) %>% 
  dplyr::count(word, sort = TRUE) %>% 
  dplyr::top_n(10) %>% 
  dplyr::mutate(word = stats::reorder(word, n))

ggplot2::ggplot(data = top10, ggplot2::aes(x = word, y = n)) +
  ggplot2::geom_col() + 
  ggplot2::xlab(NULL) + 
  ggplot2::coord_flip() + 
  ggplot2::theme_minimal() + 
  ggplot2::labs(x = "Count",
                y = "Unique Words",
                title = "Top 10 words")

# join sentiment classification to the tweet words
bing_word_counts <- tidy_txt %>%
  dplyr::inner_join(tidytext::get_sentiments("bing")) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  dplyr::group_by(sentiment) %>%
  dplyr::top_n(10) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(word = stats::reorder(word, n))


  ggplot2::ggplot(data = bing_word_counts, ggplot2::aes(word, n, fill = sentiment)) +
  ggplot2::geom_col(show.legend = FALSE) +
  ggplot2::facet_wrap(~sentiment, scales = "free_y") +
  ggplot2::labs(title = "",
       y = "Contribution to sentiment",
       x = NULL) +
  ggplot2::coord_flip()
  

