07-visualise-compare
================
2020-11-29

``` r
covid_speeches_scot_words <- covid_speeches_scot_words %>%
  mutate(origin = "Scotland")

covid_speeches_uk_words <- covid_speeches_uk_words %>%
  mutate(origin = "UK")
```

``` r
covid_speeches_words <- covid_speeches_scot_words %>%
  select(-location, -speaker) %>% # uk data doesn't have these
  bind_rows(covid_speeches_uk_words)
```

## Basic comparison

More speeches in Scotland than UK and longer on average.

``` r
covid_speeches_words %>%
  group_by(origin) %>%
  summarise(
    n_speeches = max(speech_no),
    n_words    = n(),
    avg_words  = n_words / n_speeches
    )
```

    ## `summarise()` has ungrouped output. You can override using the `.groups` argument.

    ## # A tibble: 2 x 4
    ##   origin   n_speeches n_words avg_words
    ## * <chr>         <int>   <int>     <dbl>
    ## 1 Scotland        157  324931     2070.
    ## 2 UK               55   65954     1199.

``` r
covid_speeches_words %>%
  distinct(origin, speech_no, .keep_all = TRUE) %>%
  ggplot(aes(x = n_words, color = origin, fill = origin)) +
  geom_density(alpha = 0.7) +
  scale_color_manual(values = c(scotblue, ukred)) +
  scale_fill_manual(values = c(scotblue, ukred)) +
  labs(
    x = "Number of words",
    y = "Density",
    color = "Origin", fill = "Origin"
  ) +
  theme(axis.text.y = element_blank())
```

<img src="07-visualise-compare_files/figure-gfm/unnamed-chunk-2-1.png" width="100%" />

## TF-IDF

Calculate tf-idf

``` r
covid_speeches_tf_ifd <- covid_speeches_words %>%
  count(origin, word, sort = TRUE) %>%
  group_by(origin) %>%
  mutate(total = sum(n)) %>%
  bind_tf_idf(word, origin, n)
```

View words with high TF-IDF

``` r
covid_speeches_tf_ifd %>%
  select(-total) %>%
  arrange(desc(tf_idf))
```

    ## # A tibble: 12,183 x 6
    ## # Groups:   origin [2]
    ##    origin   word            n       tf   idf   tf_idf
    ##    <chr>    <chr>       <int>    <dbl> <dbl>    <dbl>
    ##  1 UK       slide          34 0.000516 0.693 0.000357
    ##  2 Scotland aberdeen      154 0.000474 0.693 0.000329
    ##  3 UK       defeat         30 0.000455 0.693 0.000315
    ##  4 UK       sir            28 0.000425 0.693 0.000294
    ##  5 Scotland measurement   125 0.000385 0.693 0.000267
    ##  6 Scotland lanarkshire   119 0.000366 0.693 0.000254
    ##  7 UK       ventilators    22 0.000334 0.693 0.000231
    ##  8 Scotland clyde         106 0.000326 0.693 0.000226
    ##  9 UK       speaker        21 0.000318 0.693 0.000221
    ## 10 Scotland cluster        97 0.000299 0.693 0.000207
    ## # … with 12,173 more rows

Visualise TF-IDF

``` r
covid_speeches_tf_ifd %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(origin) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(y = word, x = tf_idf, fill = origin)) +
  geom_col(show.legend = FALSE) +
  labs(y = NULL, x = "tf-idf", title = "Common words in COVID briefings") +
  facet_wrap(~origin, ncol = 2, scales = "free") +
  scale_fill_manual(values = c(scotblue, ukred)) +
  scale_x_continuous(breaks = c(0, 0.0005, 0.001), labels = label_number())
```

    ## Selecting by tf_idf

<img src="07-visualise-compare_files/figure-gfm/unnamed-chunk-5-1.png" width="100%" />
