#PAIRWISE CORRELATIONS
#correlations through news
  #tokkenize and filter stopwords
      #Stopwords:
stop_word <-stopwords(kind = "spanish")
stop_word <- c(stop_word, "según", "sin", "so", "sobre", "tras", "si", "ser", "dos")
    #Tokenize
general_words <- news_1719 %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_word) %>%
  mutate(word = str_replace(word, "menas", "mena"))

# count words co-occuring within news
gen_word_pairs <- general_words %>%
  pairwise_count(word, url, sort = TRUE)
#MENA:
gen_word_pairs %>%
  filter(item1 == "mena") %>% head(20)

#correlation bewtween words:
gen_word_cors <- general_words %>%
  group_by(word) %>%
  filter(n()>100)%>%
  pairwise_cor(word, url, sort = TRUE) #estoy provadno filtros de más de 20, 50, de
  #momento queda muy bien con 50 todo.

#MENA:
gen_word_cors %>%
  filter(item1 == "mena") %>% head(20)



set.seed(2016)
#figure general:
gen_word_cors %>%
  filter(correlation > .30) %>% #rpovando indices correlacion
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

#figure just mena word
gen_word_cors %>%
  filter(item1== "mena") %>%
  filter(correlation > .11) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

# PAIRWISE JUST FOR NEWS WHERE THE WORLD MENA IS USED
#Tokenize
mena_general_words <- news_1719 %>% filter(mena_word == "si") %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_word)

mena_word_pairs <- mena_general_words %>%
  pairwise_count(word, url, sort = TRUE)
mena_word_pairs %>% head(20)

#correlation bewtween words:
mena_word_cors <- mena_general_words %>%
  group_by(word) %>%
  filter(n()>=88)%>%
  pairwise_cor(word, url, sort = TRUE) 

#figure
fig_mena_cor <- mena_word_cors %>%
  filter(correlation > .3) %>% #rpovando indices correlacion
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()+ labs(
  title = "Red de correlaciones en las piezas donde se usa el término MENA",
  subtitle = "Correlaciones con un coeficiente de phi > 0.3 encontradas entre las 150 palabras 
  más mencinadas",
  caption = "Fuente: elaboración propia"
) 
ggsave("fig_mena_cor.jpg", width = 15, height = 17, units = "cm")
#figure2 <-
mena_word_cors %>%
  filter(correlation >= 0.3) %>% #rpovando indices correlacion
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes( edge_alpha = correlation, edge_width = correlation), edge_colour = "cyan4") +
  geom_node_point(size = 3) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()
# PAIRWISE JUST FOR NEWS WHERE THE WORLD MENA IS NOT USED
#Tokenize
nomena_general_words <- news_1719 %>% filter(mena_word == "no") %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_word)

nomena_word_pairs <- nomena_general_words %>%
  pairwise_count(word, url, sort = TRUE)
nomena_word_pairs %>% head(20)

#correlation bewtween words:
nomena_word_cors <- nomena_general_words %>%
  group_by(word) %>%
  filter(n()>72)%>% #2.2 times more than in menaword, because there are 2.2. times more news
  pairwise_cor(word, url, sort = TRUE) 

#figure
fig_nomena_cor <- nomena_word_cors %>%
  filter(correlation > .3) %>% #rpovando indices correlacion
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() + 
  labs(
  title = "Red de correlaciones en las piezas donde NO se usa el término MENA",
  subtitle = "Correlaciones con un coeficiente de phi > 0.3 encontradas entre las 150 palabras 
  más mencinadas",
  caption = "Fuente: elaboración propia"
) 
ggsave("fig_nomena_cor.jpg", width = 15, height = 17, units = "cm")
