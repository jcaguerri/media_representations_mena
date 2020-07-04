  
#DATOS DEFINITIVOS
elpais_txt <- elpais_menas %>% 
  na.omit() %>%
  mutate(mena_ment = str_detect(text , 
                                "mena|menas|MENA|M.E.N.A.S.|Mena|Menas|MENAS|M.E.N.A.")) %>%
  mutate(year= year(date),
         scope = ifelse(str_detect(url, "internacional"),"internacional", "nacional"))
elpais_txt <- elpais_txt %>% 
  group_by(url) %>%
  mutate(mena_word = if_else(any(mena_ment == TRUE), "si", "no")) %>%
  ungroup()
save(elpais_txt, file = "data/elpais_txt.Rdata")

 #tokenizamos
elpais_words <- elpais_txt %>% unnest_tokens(words, text)
elpais_words %>% count(words) %>% arrange(desc(n))
save(elpais_words, file = "data/elpais_words.Rdata")
  #filtramos stopwords con la base de datos del paquete tm
stop_word <-stopwords(kind = "spanish")
stop_word <- c(stop_word, "según", "sin", "so", "sobre", "tras", "si", "ser", "dos")
elpais_wordsf <- elpais_txt %>%
  unnest_tokens(words, text) %>%
  filter(!words %in% stop_word)
save(elpais_wordsf, file = "data/elpais_wordsf.Rdata")
#20 palabras más mencionadas
elpais_wordsf %>% count(words) %>% arrange(desc(n)) %>% head(20)
#frecuencia palabras
elpais_wordsf %>% count(words) %>% mutate(fr= n/sum(n)) %>% arrange(desc(fr))%>%
  top_n(20)
#20 palabras más mencionadas en  internacional
elpais_wordsf %>% 
  filter(scope == "internacional") %>% 
  count(words) %>% 
  arrange(desc(n)) %>% 
  head(20)
#20 palabras más mencionadas en  nacional
elpais_wordsf %>% 
  filter(scope == "nacional") %>% 
  count(words) %>% 
  arrange(desc(n)) %>% 
  head(20)

#20 palabras más menciones 2019
elpais_wordsf %>% 
  filter(year==2019) %>% 
  count(words) %>% 
  arrange(desc(n)) %>% 
  head(20)
#20 palabras más menciones 2018
elpais_wordsf %>% 
  filter(year==2018) %>% 
  count(words) %>% 
  arrange(desc(n)) %>% 
  head(20)

#20 palabras cuando aparece también el término mena
elpais_wordsf %>% 
  filter(mena_ment == TRUE) %>% 
  count(words) %>% 
  arrange(desc(n)) %>% 
  head(20)

elpais_wordsf %>%  
  filter(mena_ment == TRUE) %>%
  count(words) %>% 
  mutate(fr= n/sum(n)) %>% 
  arrange(desc(fr))%>%
  top_n(20)

#Ratios nacional internacional:
nac_int_or <- elpais_wordsf  %>% 
  count(words, scope) %>% spread(scope, n, fill = 0) %>% 
  mutate(or = (nacional + 0.5) / (sum(nacional) - nacional + 0.5) /
           ( (internacional + 0.5) / (sum(internacional) - internacional + 0.5)))
  #diferencias nacional
nac_int_or %>% arrange(desc(or)) %>% head(20)
  #diferencias internacional
nac_int_or %>% arrange(or) %>% head(20)
  #ponemos filtro de al menos un uso
nac_int_or %>% filter(internacional >=1) %>%
  arrange(desc(or)) %>% head(20)

#Ratios mena o no mena
mena_nomena_or <- elpais_wordsf  %>% 
  count(words, mena_word) %>% spread(mena_word, n, fill = 0) %>% 
  mutate(or = (si + 0.5) / (sum(si) - si + 0.5) /
           ( (no + 0.5) / (sum(no) - no + 0.5)))
#diferencias menas sí
mena_nomena_or %>% arrange(desc(or)) %>% head(20)
#diferencias mena no
mena_nomena_or %>% arrange(or) %>% head(20)
#ponemos filtro de al menos un uso
mena_nomena_or %>% filter(no >=1) %>%
  arrange(desc(or)) %>% head(20)
#ponemos filtro de uso conjunto:
mena_nomena_or %>% filter(no + si >=70) %>%
  arrange(desc(or)) %>% head(20)

#N-GRAMS
elpais_ngram <- elpais_txt %>% 
  unnest_tokens(threegram, text, token = "ngrams", n= 3)
elpais_ngram %>% count(threegram) %>% arrange(desc(n)) %>% head(20)
#20 palabras más mencionadas en  internacional
elpais_ngram%>% 
  filter(scope == "internacional") %>% 
  count(threegram) %>% 
  arrange(desc(n)) %>% 
  head(20)
#20 palabras más mencionadas en  nacional
elpais_ngram %>% 
  filter(scope == "nacional") %>% 
  count(threegram) %>% 
  arrange(desc(n)) %>% 
  head(20)

#20 palabras más menciones 2019
elpais_ngram %>% 
  filter(year==2019) %>% 
  count(threegram) %>% 
  arrange(desc(n)) %>% 
  head(20)
#20 palabras más menciones 2018
elpais_ngram %>% na.omit()%>%
  filter(year==2018) %>% 
  count(threegram) %>% 
  arrange(desc(n)) %>% 
  head(20)

#20 palabras cuando aparece también el término mena
elpais_ngram %>% 
  filter(mena_ment == TRUE) %>% 
  count(threegram) %>% 
  arrange(desc(n)) %>% 
  head(20)

#INVERSE DOCUMENT FREQUENCY
  #tokenizamos preservando una varialbe: SCOPE
elpais_words_idf <- elpais_txt %>%
  unnest_tokens(words, text) %>%
  count(scope, words, sort = TRUE)
total_words <- elpais_words_idf %>% 
  group_by(scope) %>% 
  summarize(total = sum(n))
elpais_words_idf <- left_join(elpais_words_idf, total_words)

#Vemos frecuencias
freq_by_rank <- elpais_words_idf %>% 
  group_by(scope) %>% 
  mutate(rank = row_number(), 
         term_frequency = n/total)
#idf
elpais_words_idf <- elpais_words_idf  %>%
  bind_tf_idf(words, scope, n)
elpais_words_idf %>% arrange(desc(tf_idf))


  #tokenizamos preservando una varialbe: MENCIÓN MENAS
elpais_words_idf2 <- elpais_txt %>%
  unnest_tokens(words, text) %>%
  count(mena_ment, words, sort = TRUE)
total_words2 <- elpais_words_idf2 %>% 
  group_by(mena_ment) %>% 
  summarize(total = sum(n))
elpais_words_idf2 <- left_join(elpais_words_idf2, total_words2)

#Vemos frecuencias
freq_by_rank <- elpais_words_idf %>% 
  group_by(scope) %>% 
  mutate(rank = row_number(), 
         term_frequency = n/total)
#idf
elpais_words_idf2 <- elpais_words_idf2  %>%
  bind_tf_idf(words, mena_ment, n)
elpais_words_idf2 %>% arrange(desc(tf_idf))


#tokenizamos preservando una varialbe: AÑO
elpais_words_idf3 <- elpais_txt %>% filter(year>= 2017)
elpais_words_idf3 <- elpais_words_idf3 %>%
  unnest_tokens(words, text) %>%
  count(year, words, sort = TRUE)
total_words3 <- elpais_words_idf3 %>% 
  group_by(year) %>% 
  summarize(total = sum(n))
elpais_words_idf3 <- left_join(elpais_words_idf3, total_words3)

#Vemos frecuencias
freq_by_rank <- elpais_words_idf %>% 
  group_by(scope) %>% 
  mutate(rank = row_number(), 
         term_frequency = n/total)
#idf
elpais_words_idf3 <- elpais_words_idf3  %>%
  bind_tf_idf(words, year, n) 
elpais_words_idf3 %>% arrange(desc(tf_idf)) %>% head(20)

#INVERSE DOCUMENT FREQUENCY CON N-GRAMS

#Preservando scope
threegram_tf_idf <- elpais_ngram %>%
  count(scope, threegram) %>%
  bind_tf_idf(threegram, scope, n) %>%
  arrange(desc(tf_idf))

#Preservando year
threegram_tf_idf2 <- elpais_ngram %>%  filter(year>=2017)
threegram_tf_idf2 <-threegram_tf_idf2 %>%
  count(year, threegram) %>%
  bind_tf_idf(threegram, year, n) %>%
  arrange(desc(tf_idf))

#Preservando menciona menas o no
threegram_tf_idf3 <- elpais_ngram %>%
  count(mena_ment, threegram) %>%
  bind_tf_idf(threegram, mena_ment, n) %>%
  arrange(desc(tf_idf))


#BIGRAMAS PARA RED
bigram_elpais <- elpais_txt %>% 
  unnest_tokens(bigram, text, token = "ngrams", n= 2)

bigrams_separated <- bigram_elpais %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_word) %>%
  filter(!word2 %in% stop_word)

bigram_counts_elpais <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigram_counts_elpais
bigram_graph_elpais <- bigram_counts_elpais %>%
  filter(n > 10) %>%
  graph_from_data_frame()

#BIGRAMAS PARA RED SOLO NACIONAL
bigram_counts_elpais_nac <- bigrams_filtered %>% filter(scope == "nacional") %>%
  count(word1, word2, sort = TRUE)
bigram_counts_elpais_nac
bigram_graph_elpais_nac <- bigram_counts_elpais_nac %>%
  filter(n > 10) %>%
  graph_from_data_frame()

#THREEGRAMAS PARA RED
threegram_count_gen <- elpais_ngram %>%
  separate(threegram, c("word1", "word2", "word3"), sep = " ") %>%
  count(word1, word2,word3, sort = TRUE) 
threegram_graph_gen2 <- threegram_count_gen %>%
  filter(n > 25) %>%
  graph_from_data_frame()

#THREEGRAMAS PARA RED SOLO NACIONAL
threegram_count_nac <- elpais_ngram %>% filter(scope== "nacional") %>%
  separate(threegram, c("word1", "word2", "word3"), sep = " ") %>%
  count(word1, word2,word3, sort = TRUE) 
threegram_graph_nac2 <- threegram_count_nac %>%
  filter(n > 20) %>%
  graph_from_data_frame()

#REPTETIMOS TIGRAMAS PERO FILTRANDO STOPWORDS
t_c_g <- elpais_ngram %>%
  separate(threegram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_word) %>%
  filter(!word3 %in% stop_word) %>%
  count(word1, word2,word3, sort = TRUE)
t_g_g <- t_c_g %>%
  filter(n > 10) %>%
  graph_from_data_frame()

#REPTETIMOS TIGRAMAS PERO FILTRANDO STOPWORDS SOLO NACIONAL
t_c_n <- elpais_ngram %>%
  filter(scope== "nacional") %>%
  separate(threegram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_word) %>%
  filter(!word3 %in% stop_word) %>%
  count(word1, word2,word3, sort = TRUE)
t_g_n <- t_c_n %>%
  filter(n > 10) %>%
  graph_from_data_frame()

#CORRELACIONES A TRAVÉS DE NOTICIAS
ep_wordpais <- elpais_words %>% pairwise_count(words, url, sort = TRUE)
ep_wordpais %>% head(20) #palabras que aparecen juntas en más noticias
ep_wordpais %>% filter(item1== "mena") %>% head(20) #junto con mena
    #PHI COEFICIENT
word_cors <- elpais_words %>%
  group_by(words) %>%
  filter(n() >= 20) %>%
  pairwise_cor(words, url, sort = TRUE)
word_cors
word_cors %>% filter(item1 == "mena") #correlaciona con mossos
