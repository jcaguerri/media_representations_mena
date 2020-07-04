library(readr)
elpais_menas <- read_csv("data/elpais_menas.csv")
View(elpais_menas)
#Number of different news
length(unique(elpais_menas$title))
#Periodo cubierto
range(elpais_menas$date)


#noticias en el tiempo
      #GRÁFICO LINEA
news_month <- elpais_menas %>% 
  mutate(month= floor_date(date, "month")) %>% 
  group_by(month) %>% 
  summarise(news_by_month = n_distinct(url)) %>%
  arrange(month)
news_month %>% mutate(mes= as_date(month)) %>%
  ggplot(aes(mes, news_by_month)) +
  geom_line()+
  xlab("") +
  scale_x_date(date_breaks = "1 year", 
               date_labels = "%Y",
               limit=c(as.Date("2013-08-01"),as.Date("2019-09-01")))+
  theme_ipsum()+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_y_continuous(limits = c(0, 20))
 
  #GRAFICO BARRAS
news_year <- elpais_menas %>% 
  mutate(year= year(date)) %>% 
  group_by(year) %>% 
  summarise(news_by_year = n_distinct(url)) %>%
  arrange(year)

news_year  %>% 
  filter(news_by_year >=2) %>% 
  ggplot(aes(as_factor(year), news_by_year)) +
  geom_bar(stat = "identity", color= "blue", fill= "blue") +
  theme_ipsum()+
  theme(axis.text.x=element_text(angle=60, hjust=1))

    #AMBITO Y AÑOS
elpais_scope <- elpais_menas %>% mutate(scope = ifelse(str_detect(url, "internacional"),
                                                       "internacional", "nacional"))
elpais2 <- elpais_menas %>% 
  mutate(year= year(date),
         scope = ifelse(str_detect(url, "internacional"),
                                         "internacional", "nacional"))
save(elpais2, file= "data/elpais2.Rdata")
elpais2 %>% select(url, title,year, scope)%>%
  distinct(url,.keep_all= TRUE) %>%
  filter(year >= 2012) %>%
  ggplot(aes(as.factor(year), color=scope, fill=scope))+
  geom_bar() +
  theme_ipsum()+
  theme(axis.text.x=element_text(angle=60, hjust=1))
  

