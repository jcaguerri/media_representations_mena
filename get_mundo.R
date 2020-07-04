#GET THE ARCHIVES
n <- c(1:49)
n <- as.character(n)
abc_url_arch <- "https://www.abc.es/hemeroteca/resultados-busqueda-avanzada/noticia?exa=menores+extranjeros"
remplazo <- paste0("/noticia/pagina-", n)
abc_get_archive_pages <-  str_replace(abc_url_arch, "/noticia", remplazo)
str(abc_get_archive_pages) 

#GET THE LINKS
abc_get_content_links <- function(url) {
  # Function: Scrape the content links from a tag archive page
  html <- read_html(url) # load html from selected url
  urls <- 
    html %>% # pass html
    html_nodes("h2 a.titulo") %>% # isolate links
    html_attr("href") 
  cat(length(urls),"content links scraped from tag archives.\n")
  return(urls)
}
links <- map(abc_get_archive_pages, abc_get_content_links)
links <- combine(links)

#GET THE CONTENT
abc_get_content <- function(url) {
  # Function: Scrape the title, author, date, and text from a provided
  # content link. Return as a tibble/data.frame
  html <- read_html(url, encoding = "ISO-8859-1") # load html from selected url
  
  #Tittle
  tittle <- html %>% 
    html_nodes("span.titular") %>% 
    html_text( trim=TRUE) #get headline
  #Subtittle
  subtittle <- html %>% 
    html_nodes("h2.subtitulo") %>% 
    html_text( trim=TRUE) #get headline
  #Date
  date <- html %>% 
    html_nodes("time.actualizado") %>% 
    html_attr("datetime") #get date and plase. HAVE TO BE CLEANED
  
  #Text
  text <- html %>% 
    html_nodes("span.cuerpo-texto p") %>% 
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
#CLEAN A BROKEN URL
links <- links[!links %in% "https://www.abc.es/espana/20140312/rc-guardia-civil-impide-melilla-201403121757.html"]

#APPLY THE FUNCTION TO URLS AND TO GET THE CONTENT
#LAS PRIMERAS 200 HAN FUNCIONADO, HAY QUE SOLUCIONAR EL PROBLEMA DEL ENCODING
cont_abc <- map(links, abc_get_content)
#bind the resoults in a single tibble
abc_menas <- bind_rows(cont_abc)

#WRITE THE TABULAR DATA TO DISK
abc_write_content <- function(content, target_file) {
  # Function: Write the tibble content to disk. Create the directory if
  # it does not already exist.
  target_dir <- dirname(target_file) # identify target file directory structure
  dir.create(path = target_dir, recursive = TRUE, showWarnings = FALSE) # create directory
  write_csv(content, target_file) # write csv file to target location
  cat("Content written to disk!\n")
}

#Apply the function and write the data to disk
abc_write_content(content= abc_menas,
                  target_file = "data/abc_menas.csv")

