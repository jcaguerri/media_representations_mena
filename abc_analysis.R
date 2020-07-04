abc_menas <- read_csv("data/abc_menas.csv")
save(abc_menas, file = "data/abc_menas.Rdata")
View(abc_menas)
#Number of different news
length(unique(abc_menas$url))
#Periodo cubierto
range(abc_menas$date)

#TO CLEAN THE DATA:
abc_menas <- abc_menas %>% filter(!text %in% "")
abc_menas <- abc_menas %>% separate(date, c("date", "hours"), sep = "T") %>%
  mutate(date= ymd(date))
#Solve the problem with code
  #In headlines
abc_menasl <- abc_menas %>% mutate(tittle = str_replace_all(tittle, c("Ã¡" ="á",
                                                                   "Â«" = "'",
                                                                   "Â»" = "'",
                                                                   "Ã¡" = "á",
                                                                   "íº" = "ú",
                                                                   "í±" = "ñ",
                                                                   "Ã" = "í",
                                                                   "í³" = "ó",
                                                                   "í©" = "é")))
abc_menasl <- abc_menasl %>% mutate(tittle = str_remove_all(tittle, "Â"))
    #in text
abc_menasl2 <- abc_menasl %>% mutate(text = str_replace_all(text, c("Ã¡" ="á",
                                                                      "Â«" = "'",
                                                                      "Â»" = "'",
                                                                      "Ã¡" = "á",
                                                                      "íº" = "ú",
                                                                      "í±" = "ñ",
                                                                      "Ã" = "í",
                                                                      "í³" = "ó",
                                                                      "í©" = "é")))

abc_menasl3 <-  abc_menasl2 %>% mutate(text = str_replace_all(text, c( "í\u008dí±igo" = "Íñigo",
                                                                       "íº" = "ú",
                                                                       "í±" = "ñ"
                                                                       )))




#TO PREPARE THE DEFINITIVE DATA
abc_txt <- abc_menasl3 %>% 
  na.omit() %>%
  mutate(mena_ment = str_detect(text , 
                                "mena|menas|MENA|M.E.N.A.S.|Mena|Menas|MENAS|M.E.N.A.")) %>%
  mutate(year= year(date),
         scope = ifelse(str_detect(url, "internacional"),"internacional", "nacional"))
abc_txt <- abc_txt %>% 
  group_by(url) %>%
  mutate(mena_word = if_else(any(mena_ment == TRUE), "si", "no")) %>%
  ungroup()
abc_txt <- abc_txt %>% filter(date <= "2019-10-1")
save(abc_txt, file = "data/abc_txt.Rdata")
view(abc_txt)

#tokenizamos
abc_words <- abc_txt %>% unnest_tokens(words, text)
abc_words %>% count(words) %>% arrange(desc(n))
save(abc_words, file = "data/abc_words.Rdata")
#filtramos stopwords con la base de datos del paquete tm
stop_word <-stopwords(kind = "spanish")
stop_word <- c(stop_word, "según", "sin", "so", "sobre", "tras", "si", "ser", "dos")
abc_wordsf <- abc_txt %>%
  unnest_tokens(words, text) %>%
  filter(!words %in% stop_word)
save(abc_wordsf, file = "data/abc_wordsf.Rdata")
#20 palabras más mencionadas
abc_wordsf %>% count(words) %>% arrange(desc(n)) %>% head(20)
#frecuencia palabras
abc_wordsf %>% count(words) %>% mutate(fr= n/sum(n)) %>% arrange(desc(fr))%>%
  top_n(20)
#20 palabras más mencionadas en  internacional
abc_wordsf %>% 
  filter(scope == "internacional") %>% 
  count(words) %>% 
  arrange(desc(n)) %>% 
  head(20)
#20 palabras más mencionadas en  nacional
abc_wordsf %>% 
  filter(scope == "nacional") %>% 
  count(words) %>% 
  arrange(desc(n)) %>% 
  head(20)

#20 palabras más menciones 2019
abc_wordsf %>% 
  filter(year==2019) %>% 
  count(words) %>% 
  arrange(desc(n)) %>% 
  head(20)
#20 palabras más menciones 2018
abc_wordsf %>% 
  filter(year==2018) %>% 
  count(words) %>% 
  arrange(desc(n)) %>% 
  head(20)

#20 palabras cuando aparece también el término mena
abc_wordsf %>% 
  filter(mena_word == "si") %>% 
  count(words) %>% 
  arrange(desc(n)) %>% 
  head(20)

abc_wordsf %>%  
  filter(mena_word == "no") %>%
  count(words) %>% 
  mutate(fr= n/sum(n)) %>% 
  arrange(desc(fr))%>%
  top_n(20)






#Ratios nacional internacional:
nac_int_or <- elpais_words  %>% 
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
mena_nomena_or <- elpais_words  %>% 
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