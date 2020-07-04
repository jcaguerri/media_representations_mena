#Nacional Internacional
elpais_wordsf %>% 
  group_by(scope) %>% 
  count(words) %>% 
  arrange(desc(n)) %>% 
  top_n(20) %>%
  ungroup() %>%
  mutate(scope = factor(scope),
         text_order = nrow(.):1) %>%
  ggplot(aes(reorder(words, text_order), n, fill = scope)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ scope, scales = "free_y") +
  labs(x = "NULL", y = "Frequency") +
  coord_flip() +
  theme(legend.position="none")

#por año a partir de 2013
elpais_wordsf %>% 
  filter(year>= 2017)%>%
  mutate(years = factor(year))%>%
  group_by(years) %>% 
  count(words) %>% 
  arrange(desc(n)) %>% 
  top_n(20) %>%
  ungroup() %>%
  mutate(text_order = nrow(.):1) %>%
  ggplot(aes(reorder(words, text_order), n, fill = years)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ years, scales = "free_y") +
  labs(x = "NULL", y = "Frequency") +
  coord_flip() +
  theme(legend.position="none")

#Aparición o no del término Mena
elpais_wordsf %>% 
  group_by(mena_ment) %>% 
  count(words) %>% 
  arrange(desc(n)) %>% 
  top_n(20) %>%
  ungroup() %>%
  mutate(mena_ment = factor(mena_ment),
         text_order = nrow(.):1) %>%
  ggplot(aes(reorder(words, text_order), n, fill = mena_ment)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ mena_ment, scales = "free_y") +
  labs(x = "NULL", y = "Frequency") +
  coord_flip() +
  theme(legend.position="none")
  

#GRAFICO N_GRAMAS
#GRAFICO CUANDO SE MENCIONA LA PALABRA MENA
threegram_graph <- elpais_ngram %>%
  filter(mena_ment == TRUE) %>%
  separate(threegram, c("word1", "word2", "word3"), sep = " ") %>%
  count(word1, word2,word3, sort = TRUE) %>%
  unite("threegram", c(word1, word2, word3), sep = " ") %>%
  filter(n > 5) %>%
  graph_from_data_frame()

set.seed(123)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(threegram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, repel = TRUE) +
  theme_void()

# GRAFICO GENERAL
threegram_graph_gen <- elpais_ngram %>%
  separate(threegram, c("word1", "word2", "word3"), sep = " ") %>%
  count(word1, word2,word3, sort = TRUE) %>%
  unite("threegram", c(word1, word2, word3), sep = " ") %>%
  filter(n > 25) %>%
  graph_from_data_frame()

set.seed(123)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(threegram_graph_gen, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name),size= 2, vjust = 1, hjust = 1, repel =TRUE) +
  theme_void()

#Visualizamos idf según nacional/internacional
elpais_words_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(words, levels = rev(unique(words)))) %>% 
  group_by(scope) %>% 
  top_n(20) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = scope)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~scope, ncol = 2, scales = "free") +
  coord_flip()

#Visualizamos idf según se menciona a los menas o no
elpais_words_idf2 %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(words, levels = rev(unique(words)))) %>% 
  group_by(mena_ment) %>% 
  top_n(20) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = mena_ment)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~mena_ment, ncol = 2, scales = "free") +
  coord_flip()

#Visualizamos idf según AÑO
elpais_words_idf3 %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(words, levels = rev(unique(words)))) %>% 
  group_by(year) %>% 
  top_n(20) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = year)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~year, ncol = 2, scales = "free") +
  coord_flip()

#GRAFICOS N-GRAMAS
#Visualizamos idf según nacional/internacional
threegram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(threegrams = factor(threegram, levels = rev(unique(threegram)))) %>% 
  group_by(scope) %>% 
  top_n(20) %>% 
  ungroup() %>%
  ggplot(aes(threegrams, tf_idf, fill = scope)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~scope, ncol = 2, scales = "free") +
  coord_flip()

#Visualizamos idf según AÑO
threegram_tf_idf2 %>%
  arrange(desc(tf_idf)) %>%
  mutate(threegrams = factor(threegram, levels = rev(unique(threegram)))) %>% 
  group_by(year) %>% 
  top_n(20) %>% 
  ungroup() %>%
  ggplot(aes(threegrams, tf_idf, fill = year)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~year, ncol = 2, scales = "free") +
  coord_flip()

#Visualizamos idf por menas o no
threegram_tf_idf3 %>%
  arrange(desc(tf_idf)) %>%
  mutate(threegrams = factor(threegram, levels = rev(unique(threegram)))) %>% 
  group_by(mena_ment) %>% 
  top_n(20) %>% 
  ungroup() %>%
  ggplot(aes(threegrams, tf_idf, fill = mena_ment)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~mena_ment, ncol = 2, scales = "free") +
  coord_flip()


#GRÁFICO N-GRAMAS MEJORADOS
#bigramas
  #General
set.seed(2017)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph_elpais, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


  #nacional solo
set.seed(2017)
ggraph(bigram_graph_elpais_nac, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#Threegramas
  #General
set.seed(2017)
ggraph(threegram_graph_gen2, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

  #nacional solo
set.seed(2017)
ggraph(threegram_graph_nac2, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
#Threegramas filtrados
#General
set.seed(2017)
ggraph(t_g_g, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#nacional solo
set.seed(2017)
ggraph(t_g_n, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#RED DE LAS CORRELACIONES
word_cors %>%
  filter(item1 == "mena") %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

                    # CHART ABC
#Noticias por año
abc_txt %>% select(url, tittle,year, scope)%>%
  distinct(url,.keep_all= TRUE) %>%
  filter(year >= 2012) %>%
  ggplot(aes(as.factor(year), color=scope, fill=scope))+
  geom_bar() +
  theme_ipsum()+
  theme(axis.text.x=element_text(angle=60, hjust=1))

abc_txt %>% 
  group_by(year) %>% 
  summarise(news_by_year = n_distinct(url)) %>%
  arrange(desc(year))
