 # NOTICAS EN EL TIEMPO
    #ELPAIS
  #tabla:
news_year <- elpais_menas %>% 
  mutate(year= year(date)) %>% 
  group_by(year) %>% 
  summarise(news_by_year = n_distinct(url)) %>%
  arrange(year)
news_year
  #Gráfico:
elpais2 %>% select(url, title,year, scope)%>%
  distinct(url,.keep_all= TRUE) %>%
  filter(year >= 2012) %>%
  ggplot(aes(as.factor(year), color=scope, fill=scope))+
  geom_bar() +
  theme_ipsum()+
  theme(axis.text.x=element_text(angle=60, hjust=1))
  
    #ABC:
  #Tabla
abc_txt %>% 
  group_by(year) %>% 
  summarise(news_by_year = n_distinct(url)) %>%
  arrange(desc(year))
  #Gráfico
abc_txt %>% select(url, tittle,year, scope)%>%
  distinct(url,.keep_all= TRUE) %>%
  ggplot(aes(as.factor(year), color=scope, fill=scope))+
  geom_bar() +
  theme_ipsum()+
  theme(axis.text.x=element_text(angle=60, hjust=1))



#CORRELACIONES A TRAVÉS DE NOTICIAS
ep_wordpaisb<- elpais_wordsf %>% 
  mutate(wordsb = str_replace_all(words, "menas", "mena"))
ep_wordpais <- ep_wordpaisb %>% pairwise_count(wordsb, url, sort = TRUE)
ep_wordpais %>% head(20) #palabras que aparecen juntas en más noticias
ep_wordpais %>% filter(item1== "mena") %>% head(20)#junto con mena
ep_wordpais %>% filter(item1== "menas") %>% head(20)

#PHI COEFICIENT
word_corspais <- ep_wordpaisb %>%
  group_by(wordsb) %>%
  filter(n() >= 20) %>%
  pairwise_cor(words, url, sort = TRUE)
word_corspais
word_corspais %>% filter(item1 == "mena") #correlaciona con mossos
word_cors %>% filter(item1 == "mena")
#VISUALIZAMOS:
word_corspais %>%
  filter(item1 == "mena") %>%
  filter(correlation > .20) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
            
                  #ABC:
#CORRELACIONES A TRAVÉS DE NOTICIAS
  #primero equiparamos mena con menas

abc_wordsfb<- abc_wordsf %>% 
  mutate(wordsb = str_replace_all(words, "menas", "mena"))
ep_wordabc <- abc_wordsfb %>% pairwise_count(wordsb, url, sort = TRUE)
ep_wordabc %>% head(20) #palabras que aparecen juntas en más noticias
ep_wordabc %>% filter(item1== "mena") %>% head(20) #junto con mena
ep_wordabc %>% filter(item1== "menas")
#PHI COEFICIENT
word_corsabc<- abc_wordsfb %>%
  group_by(wordsb) %>%
  filter(n() >= 20) %>%
  pairwise_cor(words, url, sort = TRUE)
word_corsabc
word_corsabc %>% filter(item1 == "mena") #correlaciona con mossos

#VISUALIZAMOS:
word_corsabc %>%
  filter(item1 == "mena") %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


