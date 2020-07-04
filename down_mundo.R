#GET THE LINKS FROM downloaded html
#There is a problem with the html code, so I couldn´t  use a function. So I had to
#download a piece of code to my computer from the website and save it as a html site. 
mundo_links_1 <- read_html("data/mundo1.html") %>% html_nodes("h3") %>%
  html_nodes("a") %>% html_attr("href")
mundo_links_2 <- read_html("data/mundo2.html") %>% html_nodes("h3") %>%
  html_nodes("a") %>% html_attr("href")
mundo_links_3 <- read_html("data/mundo3.html") %>% html_nodes("h3") %>%
  html_nodes("a") %>% html_attr("href")
mundo_links_4 <- read_html("data/mundo4.html") %>% html_nodes("h3") %>%
  html_nodes("a") %>% html_attr("href")
mundo_links_5 <- read_html("data/mundo5.html") %>% html_nodes("h3") %>%
  html_nodes("a") %>% html_attr("href")
mundo_links_6 <- read_html("data/mundo6.html") %>% html_nodes("h3") %>%
  html_nodes("a") %>% html_attr("href")
mundo_links_7 <- read_html("data/mundo7.html") %>% html_nodes("h3") %>%
  html_nodes("a") %>% html_attr("href")
mundo_links <- combine(mundo_links_1, 
                       mundo_links_2, 
                       mundo_links_3, 
                       mundo_links_4,
                       mundo_links_5,
                       mundo_links_6,
                       mundo_links_7)
#Some links don´t have the "https:", so we have to add it
link_correction_a <- mundo_links[1:58]
link_correction <- mundo_links[59:350] %>% str_replace(pattern = "//", replacement = "https://")
mundo_links <- combine(link_correction_a, link_correction) 

#GET THE CONTENT
mundo_get_content <- function(url) {
  # Function: Scrape the title, author, date, and text from a provided
  # content link. Return as a tibble/data.frame
  html <- read_html(url) # load html from selected url
  
  #Tittle
  tittle <- html %>% 
    html_nodes("h1.ue-c-article__headline.js-headline") %>% 
    html_text( trim=TRUE) #get headline
  #Subtittle
  subtittle <- html %>% 
    html_nodes("p.ue-c-article__standfirst") %>% 
    html_text( trim=TRUE) #get headline
  #Date
  date <- html %>% 
    html_nodes("time") %>% 
    html_attr("datetime")
  date <- date[1] #get date and plase. HAVE TO BE CLEANED
  
  #Text
  text <- html %>% 
    html_nodes("div.ue-l-article__body.ue-c-article__body p") %>% 
    html_text( trim=TRUE)
  
  # Check to see if the article is text based
  # - only one paragraph suggests a non-text article (cartoon/ video/ album)
  if (length(text) > 1) { 
    # Create tibble/data.frame
    return(tibble(url, tittle, date, text, paragraph = (1:length(text))))
  } else {
    message("Non-text based article. Link skipped.")
    return(NULL)
  }
}

cont_mundo <- map(mundo_links, mundo_get_content)
mundo_menas <- bind_rows(cont_mundo)

#Some articles are in an old website version, so we have to change the code
mundo_links_2 <- mundo_links[78:350]
probando <- mundo_links_2[1]
html <- read_html(probando)

mundo_get_content_2 <- function(url) {
  # Function: Scrape the title, author, date, and text from a provided
  # content link. Return as a tibble/data.frame
  html <- read_html(url) # load html from selected url
  
  #Tittle
  tittle <- html %>% 
    html_nodes("h1.js-headline") %>% 
    html_text( trim=TRUE) #get headline
  #Date
  date <- html %>% 
    html_nodes("time.date") %>% 
    html_attr("datetime") #get date and plase.
  
  #Text
  text <- html %>% 
    html_nodes("div.row.content.cols-70-30 p") %>% 
    html_text( trim=TRUE)
  
  # Check to see if the article is text based
  # - only one paragraph suggests a non-text article (cartoon/ video/ album)
  if (length(text) > 1) { 
    # Create tibble/data.frame
    return(tibble(url, tittle, date, text, paragraph = (1:length(text))))
  } else {
    message("Non-text based article. Link skipped.")
    return(NULL)
  }
}

cont_mundo_2 <- map(mundo_links_2, mundo_get_content_2)
mundo_menas_2 <- bind_rows(cont_mundo_2)

        #THE SAME BLOODY HISTORY
#Some articles are in an old website version, so we have to change the code
mundo_links_3 <- mundo_links_2[68:273]
probando <- mundo_links_3[1]
html <- read_html(probando)

mundo_get_content_3 <- function(url) {
  # Function: Scrape the title, author, date, and text from a provided
  # content link. Return as a tibble/data.frame
  html <- read_html(url) # load html from selected url
  
  #Tittle
  tittle <- html %>% 
    html_nodes("h1.js-headline") %>% 
    html_text( trim=TRUE) #get headline
  #Date
  date <- html %>% 
    html_nodes("time.date") %>% 
    html_attr("datetime") #get date and plase.
  
  #Text
  text <- html %>% 
    html_nodes("div.row.content.cols-30-70 p") %>% 
    html_text( trim=TRUE)
  
  # Check to see if the article is text based
  # - only one paragraph suggests a non-text article (cartoon/ video/ album)
  if (length(text) > 1) { 
    # Create tibble/data.frame
    return(tibble(url, tittle, date, text, paragraph = (1:length(text))))
  } else {
    message("Non-text based article. Link skipped.")
    return(NULL)
  }
}

cont_mundo_3 <- map(mundo_links_3, mundo_get_content_3)
mundo_menas_3 <- bind_rows(cont_mundo_3)

# AND AGAIN THE SAME BLOODY HISTORY
#Some articles are in an old website version, so we have to change the code
mundo_links_4 <- mundo_links_3[33:206]
probando <- mundo_links_4[1]
html <- read_html(probando)

mundo_get_content_4 <- function(url) {
  # Function: Scrape the title, author, date, and text from a provided
  # content link. Return as a tibble/data.frame
  html <- read_html(url) # load html from selected url
  
  #Tittle
  tittle <- html %>% 
    html_nodes("header h1") %>% 
    html_text( trim=TRUE)
  tittle <- tittle[1] #get headline
  #Date
  date <- html %>% 
    html_nodes("time") %>% 
    html_attr("datetime") #get date and plase.
  date <- date[1]
  #Text
  text <- html %>% 
    html_nodes("div.tamano p") %>% 
    html_text( trim=TRUE)
  
  # Check to see if the article is text based
  # - only one paragraph suggests a non-text article (cartoon/ video/ album)
  if (length(text) > 1) { 
    # Create tibble/data.frame
    return(tibble(url, tittle, date, text, paragraph = (1:length(text))))
  } else {
    message("Non-text based article. Link skipped.")
    return(NULL)
  }
}

cont_mundo_4 <- map(mundo_links_4, mundo_get_content_4)
mundo_menas_4 <- bind_rows(cont_mundo_4)
# AND AGAIN  again THE SAME BLOODY HISTORY
#Some articles are in an old website version, so we have to change the code
mundo_links_5 <- mundo_links_4[26:174]
probando <- mundo_links_5[1]
html <- read_html(probando)

mundo_get_content_5 <- function(url) {
  # Function: Scrape the title, author, date, and text from a provided
  # content link. Return as a tibble/data.frame
  html <- read_html(url) # load html from selected url
  
  #Tittle
  tittle <- html %>% 
    html_nodes("h2") %>% 
    html_text( trim=TRUE) 
  tittle <- tittle[1]#get headline
  #Date
  date <- url %>% str_extract("[0-9]{4}/[0-9]{2}/[0-9]{2}") 
    #get date and plase.
  #Text
  text <- html %>% 
    html_nodes("div#tamano p") %>% 
    html_text( trim=TRUE)
  
  # Check to see if the article is text based
  # - only one paragraph suggests a non-text article (cartoon/ video/ album)
  if (length(text) > 1) { 
    # Create tibble/data.frame
    return(tibble(url, tittle, date, text, paragraph = (1:length(text))))
  } else {
    message("Non-text based article. Link skipped.")
    return(NULL)
  }
}
as.character(date) %>% 
  str_extract("[0-9]{2}/[0-9]{2}/[0-9]{4}")

cont_mundo_5 <- map(mundo_links_5, mundo_get_content_5)
cont_mundo_5 <- cont_mundo_5[1:80]
mundo_menas_5 <- bind_rows(cont_mundo_5)

#el 6 será con los links 81 a 149
# AND AGAIN  again THE SAME BLOODY HISTORY
#Some articles are in an old website version, so we have to change the code
mundo_links_6 <- mundo_links_5[82:149]
probando <- mundo_links_6[1]
html <- read_html(probando)
    #toca del 81 al 149, editar
mundo_get_content_6 <- function(url) {
  # Function: Scrape the title, author, date, and text from a provided
  # content link. Return as a tibble/data.frame
  html <- read_html(url) # load html from selected url
  
  #Tittle
  tittle <- html %>% 
    html_nodes("h1") %>% 
    html_text( trim=TRUE) 
  tittle <- tittle[1]#get headline
  #Date
  date <- url %>% str_extract("[0-9]{4}/[0-9]{2}/[0-9]{2}") 
  #get date and plase.
  #Text
  text <- html %>% 
    html_nodes("div#tamano p") %>% 
    html_text( trim=TRUE)
  
  # Check to see if the article is text based
  # - only one paragraph suggests a non-text article (cartoon/ video/ album)
  if (length(text) > 1) { 
    # Create tibble/data.frame
    return(tibble(url, tittle, date, text, paragraph = (1:length(text))))
  } else {
    message("Non-text based article. Link skipped.")
    return(NULL)
  }
}
as.character(date) %>% 
  str_extract("[0-9]{2}/[0-9]{2}/[0-9]{4}")

cont_mundo_6 <- map(mundo_links_6, mundo_get_content_6)
mundo_menas_6 <- bind_rows(cont_mundo_6)

# AND AGAIN  again THE SAME BLOODY HISTORY
#Some articles are in an old website version, so we have to change the code
mundo_links_7 <- mundo_links_6[39:68]
probando <- mundo_links_7[1]
html <- read_html(probando)
#editar
mundo_get_content_7 <- function(url) {
  # Function: Scrape the title, author, date, and text from a provided
  # content link. Return as a tibble/data.frame
  html <- read_html(url) # load html from selected url
  
  #Tittle
  tittle <- html %>% 
    html_nodes("span.titulonoticia") %>% 
    html_text( trim=TRUE) 
  #Date
  date <- url %>% str_extract("[0-9]{4}/[0-9]{2}/[0-9]{2}") 
  #get date 
  #Text
  text <- html %>% 
    html_nodes("td.trecepixnegro p") %>% 
    html_text( trim=TRUE)
  text <- text[-1]
  
  # Check to see if the article is text based
  # - only one paragraph suggests a non-text article (cartoon/ video/ album)
  if (length(text) > 1) { 
    # Create tibble/data.frame
    return(tibble(url, tittle, date, text, paragraph = (1:length(text))))
  } else {
    message("Non-text based article. Link skipped.")
    return(NULL)
  }
}

cont_mundo_7 <- map(mundo_links_7, mundo_get_content_7)
mundo_menas_7 <- bind_rows(cont_mundo_7)
cont_mundo_5 <- cont_mundo_5[39:68]

# AND AGAIN  again THE SAME BLOODY HISTORY
#Some articles are in an old website version, so we have to change the code
mundo_links_8 <- mundo_links_7[22:30]
probando <- mundo_links_8[1]
html <- read_html(probando)
#editar
mundo_get_content_8 <- function(url) {
  # Function: Scrape the title, author, date, and text from a provided
  # content link. Return as a tibble/data.frame
  html <- read_html(url) # load html from selected url
  
  #Tittle
  tittle <- html %>% 
    html_nodes("span.tituno") %>% 
    html_text( trim=TRUE) 
  #Date
  date <- url %>% str_extract("[0-9]{4}/[0-9]{2}/[0-9]{2}") 
  #get date 
  #Text
  text <- html %>% 
    html_nodes("td.piefot p") %>% 
    html_text( trim=TRUE)
  text <- text[-1]
  
  # Check to see if the article is text based
  # - only one paragraph suggests a non-text article (cartoon/ video/ album)
  if (length(text) > 1) { 
    # Create tibble/data.frame
    return(tibble(url, tittle, date, text, paragraph = (1:length(text))))
  } else {
    message("Non-text based article. Link skipped.")
    return(NULL)
  }
}

cont_mundo_8 <- map(mundo_links_8, mundo_get_content_8)
mundo_menas_8 <- bind_rows(cont_mundo_8)

mundo_menas_bueno <- bind_rows(mundo_menas, 
                               mundo_menas_2, 
                               mundo_menas_3, 
                               mundo_menas_4,
                               mundo_menas_5,
                               mundo_menas_6,
                               mundo_menas_7,
                               mundo_menas_8)
mundo_menas_bueno %>%  distinct(url) #hay 346 noticias
mundo_menas <- mundo_menas_bueno
#write the data
mundo_write_content <- function(content, target_file) {
  # Function: Write the tibble content to disk. Create the directory if
  # it does not already exist.
  target_dir <- dirname(target_file) # identify target file directory structure
  dir.create(path = target_dir, recursive = TRUE, showWarnings = FALSE) # create directory
  write_csv(content, target_file) # write csv file to target location
  cat("Content written to disk!\n")
}

#Apply the function and write the data to disk
mundo_write_content(content= mundo_menas,
                  target_file = "data/mundo_menas.csv")
save(mundo_menas, file = "data/mundo_menas.Rdata")


#LIMPIAMOS Y PREPARAMOS LA TABLA DEFINITIVA
mundo_menas1 <- mundo_menas %>% filter(!text %in% "")
mundo_menas <- mundo_menas %>% 
  filter(!text %in% "") %>%
  mutate(date = str_remove_all(date, "T[0-9]{2}:[0-9]{2}:[0-9]{2}Z")) %>%
  mutate(date= str_remove_all(date, "[0-9]{2}:[0-9]{2}:[0-9]{2}")) %>%
  mutate(date= ymd(date))

mundo_txt <- mundo_menas %>% 
  na.omit() %>%
  mutate(mena_ment = str_detect(text , 
                                "mena|menas|MENA|M.E.N.A.S.|Mena|Menas|MENAS|M.E.N.A.")) %>%
  mutate(year= year(date),
         scope = ifelse(str_detect(url, "internacional"),"internacional", "nacional"))

mundo_txt <- mundo_txt %>% 
  group_by(url) %>%
  mutate(mena_word = if_else(any(mena_ment == TRUE), "si", "no")) %>%
  ungroup()
mundo_txt <- mundo_txt %>% filter(date <= "2019-10-1")
save(mundo_txt, file = "data/mundo_txt.Rdata")
view(mundo_txt)
