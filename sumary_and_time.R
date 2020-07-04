#GENERAL:
#Number of news by newspaper
news_dataset %>% 
  group_by(newspaper) %>% 
  summarise("nº noticias" =  length(unique(url)),
            "fecha 1ºnoticia" = range(date)[1])
#Number of news by newspaper and year
  #table
grouped_table <- news_dataset %>% select(url, year, newspaper, date) %>% 
  distinct(url, .keep_all = TRUE)
table_by_year <- table(periodico = grouped_table$newspaper, año = grouped_table$year)                                               
  #Chart
grouped_table %>% group_by(year, newspaper) %>% summarise(noticias = n()) %>%
  ggplot(aes(year, noticias, color = newspaper)) +
  geom_line(size= 1)+
  geom_point(size=2.5) +
  scale_x_continuous(name="Años", breaks = seq(2001, 2019, 1))+
  scale_y_continuous(name="Número de piezas", breaks = seq(0, 150, 25))

              #FILTER BY THE USE OF THE WORD MENA

#Number of news by newspaper
mena_w_news_by_newspaper <- news_1719 %>%
  filter(mena_word== "si") %>%
  group_by(newspaper) %>% 
  summarise("mención mena" =  length(unique(url)))
mena_w_news_by_newspaper2 <- news_1719 %>%
  filter(mena_word== "no") %>%
  group_by(newspaper) %>% 
  summarise("no mención mena" =  length(unique(url)))
mena_w_by_newspaper <- left_join(mena_w_news_by_newspaper, mena_w_news_by_newspaper2)
tot <- tibble(newspaper = "TOTAL",
              "mención mena" = sum(mena_w_news_by_newspaper$`mención mena`), 
              "no mención mena" = sum(mena_w_news_by_newspaper2$`no mención mena`))
mena_w_by_newspaper <- bind_rows(mena_w_by_newspaper, tot) #DEFINITIVE TABLE
write.table(mena_w_by_newspaper, file = "mena_w_by_newspaper.txt", sep = ",", quote = FALSE, row.names = T)
save(mena_w_by_newspaper, file = "mena_w_by_newspaper.rda")


#News divided by the use of "mena"
news_1719 %>% group_by(mena_word) %>% summarise(n= length(unique(url)))


#Number of news by newspaper and year
#table
mena_w_grouped_table <- news_dataset %>% 
  filter(mena_word == "si") %>%
  select(url, year, newspaper, date) %>% 
  distinct(url, .keep_all = TRUE)
mena_w_table_by_year <- table(periodico = mena_w_grouped_table$newspaper, año = mena_w_grouped_table$year)                                               
#most of news that use the word mena have been published between 2017 and 2019
#Chart BY YEAR
mena_w_grouped_table %>% group_by(year, newspaper) %>% summarise(noticias = n()) %>%
  ggplot(aes(year, noticias, color = newspaper)) +
  geom_line(size= 1)+
  geom_point(size=2.5) +
  scale_x_continuous(name="Años", breaks = seq(2001, 2019, 1))+
  scale_y_continuous(name="Número de noticias", breaks = seq(0, 150, 25))   

#NEWS FROM 2017 TO 219
news_last_y <- as_tibble(mena_w_table_by_year) %>% 
  spread(periodico, n) %>% 
  filter(año >= 2017) 
n_by_y_n_t <- news_last_y %>%
  add_row(año= "TOTAL", 
          "abc"= sum(news_last_y$abc), 
          "elmundo"=sum(news_last_y$elmundo), "elpais"=sum(news_last_y$elpais),
          "lavanguardia" = sum(news_last_y$lavanguardia))

write.table(n_by_y_n_t, file = "n_by_y_n_t.txt", sep = ",", quote = FALSE, row.names = T)
save(n_by_y_n_t, file = "n_by_y_n_t.rda")

  
#Chart by month
p1 <- mena_w_grouped_table %>% 
  filter(year>= 2017) %>%
  group_by(date= floor_date(date, unit = "month"), newspaper) %>% summarise(noticias = n()) %>%
  ggplot(aes(date, noticias, color = newspaper)) +
  geom_line(size= 1) +
  scale_x_date(date_breaks = '2 month', 
               labels = scales::date_format("%b-%y")) +
  scale_y_continuous(limits=c(0, 20), breaks= seq(0,20,2))+
  labs(
    x = "Mes de publicación",              # título del eje x
    y = "Nº de piezas",   # título del eje y
    title = "Piezas en las que se menciona el término MENA",   # título principal de la figura
    color = "Periódico"   # título de la leyenda
  ) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.position = "top")

ggsave("p1.jpg", width = 15, height = 12, units = "cm")

p2_detalle <- mena_w_grouped_table %>% 
  filter(year>= 2018) %>%
  group_by(date= floor_date(date, unit = "month"), newspaper) %>% summarise(noticias = n()) %>%
  ggplot(aes(date, noticias, color = newspaper)) +
  geom_line(size= 1) +
  scale_x_date(date_breaks = '2 month', 
               labels = scales::date_format("%b-%y"),) +
  scale_y_continuous(limits=c(0, 20), breaks= seq(0,20,2))+
  labs(
    x = "Mes de publicación",              # título del eje x
    y = "Nº de piezas",   # título del eje y
    title = "Piezas en las que se menciona el término MENA",   # título principal de la figura
    color = "Periódico"   # título de la leyenda
  ) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.position = "top")

#se solapan claramente
mena_w_grouped_table %>% 
  mutate(newspaper= factor(newspaper, levels = c("lavanguardia","elpais", "elmundo", "abc"))) %>%
  filter(year>= 2018) %>%
  group_by(date= floor_date(date, unit = "month"), newspaper) %>% summarise(noticias = n()) %>%
  ggplot(aes(date, noticias, fill= newspaper)) +
  geom_area(alpha= 0.7, size= 0.2, stat = "identity", position = "stack")+
  scale_y_continuous()
  
