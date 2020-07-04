tags_elpais <- elpais_menas %>% select(url, title, tags)

tags_elpais2 <- tags_elpais %>% distinct(url,.keep_all= TRUE)
#Tidy tags
tidy_tag <- tags_elpais2 %>% 
  mutate(tag = str_split(tags,"\n")) %>% 
  unnest()
    #tags más comunes
com_tags <- tidy_tag %>% 
  group_by(tag) %>% 
  summarise(freq= n()) %>% 
  arrange(desc(freq))
view(com_tags)

#Filtro por sección
 scope_news <- tidy_tag %>% 
   mutate(international = str_detect(tidy_tag$url, "internacional"))
    #Tags más comunes internacionales
int <-scope_news %>% filter(international == "TRUE") %>%
  group_by(tag) %>% 
  summarise(freq= n()) %>% 
  arrange(desc(freq))
    #Tags más comunes nacionales
nac <- scope_news %>% filter(international == "FALSE") %>%
  group_by(tag) %>% 
  summarise(freq= n()) %>% 
  arrange(desc(freq))
view(nac)

#Aparicion de la palabra mena
elpais3 <- elpais2 %>% group_by(url)%>%
  mutate(mena_ment = str_detect(text , 
                                "mena|menas|MENA|M.E.N.A.S.|Mena|Menas|MENAS|M.E.N.A.")) %>%
  ungroup() %>% 
  select(url,title, date, year, scope, mena_ment) %>%
  distinct(url,.keep_all= TRUE) %>% filter(mena_ment == TRUE) %>%
  arrange(date)
    #Primera aparición:
elpais3
    #Evolución en el tiempo
elpais3 %>% group_by(year, scope) %>% summarise(me_ment = sum(mena_ment))

