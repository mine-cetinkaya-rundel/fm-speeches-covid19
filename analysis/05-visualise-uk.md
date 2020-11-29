05-visualise-uk
================
2020-11-29

## Remove stop words

``` r
covid_speeches_uk_words <- covid_speeches_uk_words %>%
  anti_join(stop_words)
```

    ## Joining, by = "word"

## Length of speech

``` r
ggplot(covid_speeches_uk, aes(x = n_words)) +
  geom_density(color = ukred, fill = ukred, alpha = 0.5) +
  labs(
    title = "Distribution of number of words",
    subtitle = "of UK daily briefings",
    x = "Number of words",
    y = "Density"
  ) +
  theme(axis.text.y = element_blank())
```

<img src="05-visualise-uk_files/figure-gfm/unnamed-chunk-1-1.png" width="100%" />

``` r
lm_words <- lm(n_words ~ date, data = covid_speeches_uk)
lm_words_rsq <- glance(lm_words)$r.squared

covid_speeches_uk %>%
  ggplot(aes(x = date, y = n_words)) +
  geom_point(color = ukred, alpha = 0.7) +
  geom_smooth(aes(x = date, y = n_words), method = lm, formula = y ~ x, color = "darkgray") +
  labs(
    title = "Length of UK COVID-19 speeches",
    subtitle = glue("Measured in number of words, R-squared = {percent(lm_words_rsq)}"),
    x = NULL, y = "Number of words", color = NULL, shape = NULL
  )
```

<img src="05-visualise-uk_files/figure-gfm/unnamed-chunk-2-1.png" width="100%" />

## Word frequency

``` r
threshold <- 100

covid_speeches_uk_words %>%
  count(word, sort = TRUE) %>%
  filter(n > threshold) %>%
  ggplot(aes(y = fct_reorder(word, n), x = n, fill = n)) +
  geom_col() +
  guides(fill = FALSE) +
  labs(
    title = "Frequency of words in UK COVID-19 briefings",
    subtitle = glue("Words occurring more than {threshold} times"),
    y = NULL, x = NULL
  )
```

<img src="05-visualise-uk_files/figure-gfm/unnamed-chunk-3-1.png" width="100%" />

## Sentiment analysis

“positive” isn’t really a positive word in this context. Remove and plot
again.

``` r
covid_speeches_uk_words %>%
  filter(word != "positive") %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(sentiment, word, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_head(n = 20) %>%
  ggplot(aes(y = fct_reorder(word, n), x = n, fill = sentiment)) +
  geom_col() +
  guides(fill = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  labs(
    title = "Sentiment and frequency of words in UK COVID-19 briefings",
    subtitle = "Bing lexicon",
    y = NULL, x = NULL
  )
```

<img src="05-visualise-uk_files/figure-gfm/unnamed-chunk-4-1.png" width="100%" />

## Daily sentiments

### Lexicon: Bing

Sentiments: Positive and negative.

``` r
covid_speeches_uk_words %>%
  filter(word != "positive") %>% 
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(date, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n) %>%
  mutate(sentiment = positive - negative) %>%
  ggplot(aes(x = date, y = sentiment)) +
  geom_line(color = "gray") +
  geom_point(aes(color = sentiment > 0), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "lightgray") +
  guides(color = FALSE) +
  labs(
    title = "Daily sentiment score of words in UK COVID-19 briefings",
    subtitle = "Bing lexicon",
    x = "Date", y = "Sentiment score (positive - negative)"
  ) +
  theme(legend.position = "bottom")
```

<img src="05-visualise-uk_files/figure-gfm/unnamed-chunk-5-1.png" width="100%" />

and now with a smooth curve…

``` r
covid_speeches_uk_words %>%
  filter(word != "positive") %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(date, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n) %>%
  mutate(sentiment = positive - negative) %>%
  ggplot(aes(x = date, y = sentiment)) +
  geom_smooth(color = "gray", method = "lm", formula = y ~ x) +
  geom_point(aes(color = sentiment > 0), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "lightgray") +
  guides(color = FALSE) +
  labs(
    title = "Daily sentiment score of words in UK COVID-19 briefings",
    subtitle = "Bing lexicon",
    x = "Date", y = "Sentiment score (positive - negative)"
  ) +
  theme(legend.position = "bottom")
```

<img src="05-visualise-uk_files/figure-gfm/unnamed-chunk-6-1.png" width="100%" />

### Lexicon: NRC

Sentiments: rust, fear, negative, sadness, anger, surprise, positive,
disgust, joy, and anticipation.

``` r
covid_speeches_uk_words %>%
  filter(word != "positive") %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  mutate(
    sentiment = fct_relevel(sentiment, "positive", "anticipation", "joy", "surprise", "trust",
                            "negative", "anger", "disgust", "fear", "sadness"),
    sentiment_binary = if_else(sentiment %in% c("positive", "anticipation", "joy", "surprise", "trust"), "positive", "negative")
  ) %>%
  count(sentiment_binary, sentiment, word, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice_head(n = 10) %>%
  ggplot(aes(y = fct_reorder(word, n), x = n, fill = sentiment_binary)) +
  geom_col() +
  guides(fill = FALSE) +
  facet_wrap(~ sentiment, scales = "free_y", ncol = 5) +
  labs(
    title = "Sentiment and frequency of words in UK COVID-19 briefings",
    subtitle = "NRC lexicon",
    y = NULL, x = NULL
  ) +
  scale_x_continuous(breaks = c(0, 200)) +
  theme_minimal(base_size = 11)
```

<img src="05-visualise-uk_files/figure-gfm/unnamed-chunk-7-1.png" width="100%" />

``` r
covid_speeches_uk_words %>%
  filter(word != "positive") %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  mutate(
    sentiment = fct_relevel(sentiment, "positive", "anticipation", "joy", "surprise", "trust",
                            "negative", "anger", "sadness", "disgust", "fear"),
    sentiment_binary = if_else(sentiment %in% c("positive", "anticipation", "joy", "surprise", "trust"), "positive", "negative")
    ) %>%
  count(date, sentiment_binary, sentiment) %>%
  ggplot(aes(x = date, y = n, color = sentiment_binary)) +
  geom_line(size = 0.3) +
  guides(color = FALSE) +
  facet_wrap(~ sentiment, ncol = 5) +
  labs(
    title = "Sentiment score of words in UK COVID-19 briefings over time",
    subtitle = "NRC lexicon",
    x = "Date", y = "Sentiment score", color = NULL
  )
```

<img src="05-visualise-uk_files/figure-gfm/unnamed-chunk-8-1.png" width="100%" />

## Bigram frequency

``` r
threshold <- 15

covid_speeches_uk_bigrams %>%
  mutate(
    bigram = if_else(bigram == "care home", "care home(s)", bigram),
    bigram = if_else(bigram == "care homes", "care home(s)", bigram)
  ) %>%
  count(bigram, sort = TRUE) %>%
  filter(n > threshold) %>%
  ggplot(aes(y = fct_reorder(bigram, n), x = n, fill = n)) +
  geom_col() +
  guides(fill = FALSE) +
  labs(
    title = "Frequency of bigrams in UK COVID-19 briefings",
    subtitle = glue("Bigrams occurring more than {threshold} times"),
    y = NULL, x = NULL
  )
```

<img src="05-visualise-uk_files/figure-gfm/unnamed-chunk-9-1.png" width="100%" />

## Social vs. physical distancing

They never say physical distancing!

``` r
covid_speeches_uk %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(str_detect(bigram, "social dist|physical dist")) %>%
  mutate(soc_phys = if_else(str_detect(bigram, "social"), "S", "P")) %>%
  count(date, soc_phys) %>%
  ggplot(aes(x = date, y = n, color = soc_phys)) +
  geom_text(aes(label = soc_phys)) +
  guides(color = FALSE) +
  labs(x = "Date", y = "Frequency",
       title = "Social (S) vs. physical (P) distancing",
       subtitle = "Number of mentions over time") +
  scale_color_manual(values = c(ukred, "darkgray")) +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, 2))
```

<img src="05-visualise-uk_files/figure-gfm/unnamed-chunk-10-1.png" width="100%" />
