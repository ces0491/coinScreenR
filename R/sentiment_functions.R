twts <- rtweet::search_tweets("BTC", n = 250, include_rts = TRUE, `filter` = "verified", lang = "en")

twt_reqd <- dplyr::select(twts, screen_name, text)

tidy_txt <- twt_reqd %>%
  tidytext::unnest_tokens(word, text) %>%
  dplyr::mutate(word_stem = SnowballC::wordStem(word)) %>%
  dplyr::anti_join(tidytext::stop_words, by = "word") %>%
  dplyr::filter(!grepl("\\.|http", word))