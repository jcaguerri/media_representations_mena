


idf_by_newspaper <- news_dataset  %>%
  unnest_tokens(words, text) %>%
  count(newspaper, words, sort = TRUE)
total_words <- idf_by_newspaper %>% 
  group_by(newspaper) %>% 
  summarize(total = sum(n))
idf_by_newspaper<- left_join(idf_by_newspaper, total_words)

#Vemos frecuencias
freq_by_rank <- idf_by_newspaper %>% 
  group_by(newspaper) %>% 
  mutate(rank = row_number(), 
         term_frequency = n/total)
#idf
idf_by_newspaper<- idf_by_newspaper  %>%
  bind_tf_idf(words, newspaper, n)
idf_by_newspaper%>% arrange(desc(tf_idf))

#CHART
idf_by_newspaper %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(words, levels = rev(unique(words)))) %>% 
  group_by(newspaper) %>% 
  top_n(20) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = newspaper)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~newspaper, ncol = 2, scales = "free") +
  coord_flip()
