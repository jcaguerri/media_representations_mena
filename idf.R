                #INVERSE DOCUMENT FREQUENCY

                      #ELPAIS
#tokenizamos preservando una varialbe: MENCIÓN MENAS
elpais_words_idf2 <- elpais_txt %>%
  unnest_tokens(words, text) %>%
  count(mena_word, words, sort = TRUE)
total_words2 <- elpais_words_idf2 %>% 
  group_by(mena_word) %>% 
  summarize(total = sum(n))
elpais_words_idf2 <- left_join(elpais_words_idf2, total_words2)

#Vemos frecuencias
freq_by_rank <- elpais_words_idf2 %>% 
  group_by(mena_word) %>% 
  mutate(rank = row_number(), 
         term_frequency = n/total)
#idf
elpais_words_idf2 <- elpais_words_idf2  %>%
  bind_tf_idf(words, mena_word, n)
elpais_words_idf2 %>% arrange(desc(tf_idf))

#Visualizamos:
elpais_words_idf2 %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(words, levels = rev(unique(words)))) %>% 
  group_by(mena_word) %>% 
  top_n(20) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = mena_word)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~mena_word, ncol = 2, scales = "free") +
  coord_flip()

#N-GRMAS POR IDF
elpais_ngram <- elpais_txt %>% 
  unnest_tokens(threegram, text, token = "ngrams", n= 3)

threegram_tf_idf3 <- elpais_ngram %>%
  count(mena_word, threegram) %>%
  bind_tf_idf(threegram, mena_word, n) %>%
  arrange(desc(tf_idf))

#Visualizamos
threegram_tf_idf3 %>%
  arrange(desc(tf_idf)) %>%
  mutate(threegrams = factor(threegram, levels = rev(unique(threegram)))) %>% 
  group_by(mena_word) %>% 
  top_n(20) %>% 
  ungroup() %>%
  ggplot(aes(threegrams, tf_idf, fill = mena_word)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~mena_word, ncol = 2, scales = "free") +
  coord_flip()

#Preparamos threegramas
pais_threegram_count_nac <- elpais_ngram %>% filter(mena_word== "si") %>%
  separate(threegram, c("word1", "word2", "word3"), sep = " ") %>%
  count(word1, word2,word3, sort = TRUE) 
pais_threegram_graph_nac2 <- pais_threegram_count_nac %>%
  filter(n > 15) %>%
  graph_from_data_frame()

#visualizanos
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
set.seed(2017)
ggraph(pais_threegram_graph_nac2, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

                        #ABC
#tokenizamos preservando una varialbe: MENCIÓN MENAS
abc_words_idf<- abc_txt %>%
  unnest_tokens(words, text) %>%
  count(mena_word, words, sort = TRUE)
total_wordsabc <- abc_words_idf %>% 
  group_by(mena_word) %>% 
  summarize(total = sum(n))
abc_words_idf2 <- left_join(abc_words_idf, total_wordsabc)

#Vemos frecuencias
freq_by_rankabc <- abc_words_idf2 %>% 
  group_by(mena_word) %>% 
  mutate(rank = row_number(), 
         term_frequency = n/total)
#idf
abc_words_idf2 <- abc_words_idf2  %>%
  bind_tf_idf(words, mena_word, n)
abc_words_idf2 %>% arrange(desc(tf_idf))

#Visualizamos:
abc_words_idf2 %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(words, levels = rev(unique(words)))) %>% 
  group_by(mena_word) %>% 
  top_n(20) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = mena_word)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~mena_word, ncol = 2, scales = "free") +
  coord_flip()

              #N-GRAMAS

#ELPAIS

elpais_threegram_graph <- elpais_ngram %>%
  filter(mena_word == "si") %>%
  separate(threegram, c("word1", "word2", "word3"), sep = " ") %>%
  count(word1, word2,word3, sort = TRUE) %>%
  unite("threegram", c(word1, word2, word3), sep = " ") %>%
  filter(n > 15) %>%
  graph_from_data_frame()

set.seed(123)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(elpais_threegram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, repel = TRUE) +
  theme_void()

#ABC
abc_ngram <- abc_txt %>% 
  unnest_tokens(threegram, text, token = "ngrams", n= 3)
abc_threegram_graph <- abc_ngram %>%
  filter(mena_word == "si") %>%
  separate(threegram, c("word1", "word2", "word3"), sep = " ") %>%
  count(word1, word2,word3, sort = TRUE) %>%
  unite("threegram", c(word1, word2, word3), sep = " ") %>%
  filter(n > 10) %>%
  graph_from_data_frame()

set.seed(123)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(abc_threegram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, repel = TRUE) +
  theme_void()


