#GET THE ARCHIVES
n <- c(1:30)
n <- as.character(n)
vanguardia_url_arch <- "https://www.lavanguardia.com/search?q=%22menores%20extranjeros%22#gsc.tab=0&gsc.q=%22menores%20extranjeros%22&gsc.page=1"
remplazo <- paste0("page=", n)
vanguardia_get_archive_pages <-  str_replace(abc_url_arch, "page=1", remplazo)
str(abc_get_archive_pages) 

#GET THE LINKS
library(RSelenium)

driver <- rsDriver(port = 4444L, browser=c("firefox"))
remote_driver <- driver[["client"]]
remote_driver$open()
master <-c()
n <- 31

vanguardia_get_content_links <- function(url) {
  remote_driver$open()
  remote_driver$navigate(url)
  cuerpo<-remote_driver$findElement(using = 'css',  ".gsc-webResult")
  cuerpotxt <- cuerpo$getElementAttribute("outerHTML")[[1]] # gets us the HTML
  cuerpoxml <- htmlTreeParse(cuerpotxt, useInternalNodes=T) # parse string into HTML tree to allow for querying with XPath
  fundList <- unlist(xpathApply(cuerpoxml, '//*[contains(concat( " ", @class, " " ), concat( " ", "gs-title", " " ))]', xmlGetAttr, 'href')) # parses out just the fund name and ticker using XPath
  urls <- unique(fundList)
  return(urls)
}
#Three times to avoid to crash Rstudio
vanguardia_get_archive_pagesa <- vanguardia_get_archive_pages[1:10]
links_vana <- map(vanguardia_get_archive_pagesa, vanguardia_get_content_links)
links_vana1 <- combine(links_vana)

vanguardia_get_archive_pagesb <- vanguardia_get_archive_pages[11:20]
links_vanb <- map(vanguardia_get_archive_pagesb, vanguardia_get_content_links)
links_vanb1 <- combine(links_vanb) %>% unique()

vanguardia_get_archive_pagesc <- vanguardia_get_archive_pages[21:30]
links_vanc <- map(vanguardia_get_archive_pagesc, vanguardia_get_content_links)
links_vanc1 <- combine(links_vanc)

#Definitive list of news
vanguardia_links <- combine(links_vana1, links_vanb1, links_vanc1)


#GET THE CONTENT
vanguardia_get_content <- function(url) {
  # Function: Scrape the title, author, date, and text from a provided
  # content link. Return as a tibble/data.frame
  html <- read_html(url) # load html from selected url
  #Tittle
tittle <- html %>% 
  html_nodes(".story-leaf-title") %>% 
  html_text( trim=TRUE) #get headline .col-xs-12.col-md-10
#Subtittle
subtittle <- html %>% 
  html_nodes("h2") %>% 
  html_text( trim=TRUE) #get headline
#Date
date <- url %>% str_extract("/20[0-9]{6}/") 
#get date and plase. HAVE TO BE CLEANED
#Text
text <- html %>% 
  html_nodes(".story-leaf-txt-p p") %>% 
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

vanguardia_mundo <- map(vanguardia_links, vanguardia_get_content)
vanguardia_menas <- bind_rows(vanguardia_mundo)
#WRITE THE TABULAR DATA TO DISK
vanguardia_write_content <- function(content, target_file) {
  # Function: Write the tibble content to disk. Create the directory if
  # it does not already exist.
  target_dir <- dirname(target_file) # identify target file directory structure
  dir.create(path = target_dir, recursive = TRUE, showWarnings = FALSE) # create directory
  write_csv(content, target_file) # write csv file to target location
  cat("Content written to disk!\n")
}
vanguardia_write_content(content= vanguardia_menas,
                  target_file = "data/vanguardia_menas.csv")
save(vanguardia_menas, file = "data/vanguardia_menas.Rdata")

#LIMPIAMOS Y PREPARAMOS LA TABLA DEFINITIVA
vanguardia_menas <- vanguardia_menas %>% 
  filter(!text %in% "") %>%
  mutate(date= ymd(date))

vanguardia_txt <- vanguardia_menas %>% 
  na.omit() %>%
  mutate(mena_ment = str_detect(text , 
                                "mena|menas|MENA|M.E.N.A.S.|Mena|Menas|MENAS|M.E.N.A.")) %>%
  mutate(year= year(date),
         scope = ifelse(str_detect(url, "internacional"),"internacional", "nacional"))

vanguardia_txt <- vanguardia_txt %>% 
  group_by(url) %>%
  mutate(mena_word = if_else(any(mena_ment == TRUE), "si", "no")) %>%
  ungroup()

unique(vanguardia_txt$date)
#vanguardia_txt2 <- vanguardia_txt %>% filter(date <= "2019-10-1")
save(vanguardia_txt, file = "data/vanguradia_txt.Rdata")
view(vanguardia_txt)