            #FUNCTIONS
#The functions have been provided by Jerid Francom https://francojc.github.io/
#GET THE TOTAL NUMBER OF ARCHIVE PAGES
get_archive_pages <- function(tag_name, sample_size = 1) {
  # Function: Scrape tag main page and return selected number of archive pages
  url <- paste0("https://elpais.com/tag/", tag_name)
  html <- read_html(url) # load html from selected url
  pages_available <- 
    html %>% # pass html
    html_node("li.paginacion-siguiente a") %>% # isolate 'next page' link
    html_attr("href") %>% # extract 'next page' link
    str_extract("\\d+$") %>% # extract the numeric value (num pages of links) in link
    as.numeric() + 1 # covert to a numeric vector and add 1 (to include first page)
  cat(pages_available, "pages available for the", tag_name, "tag.\n")
  archive_pages <- paste0(url, "/a/", (pages_available - (sample_size - 1)):pages_available) # compile urls
  cat(sample_size, "pages selected.\n")
  return(archive_pages)
}

#GET THE LINKS
get_content_links <- function(url) {
  # Function: Scrape the content links from a tag archive page
  html <- read_html(url) # load html from selected url
  urls <- 
    html %>% # pass html
    html_nodes("h2.articulo-titulo a") %>% # isolate links
    html_attr("href") %>% # extract urls
    str_replace(pattern = "//", replacement = "https://") # create valid urls
  cat(length(urls),"content links scraped from tag archives.\n")
  return(urls)
}

#GET THE CONTENT
get_content <- function(url) {
  # Function: Scrape the title, author, date, and text from a provided
  # content link. Return as a tibble/data.frame
  cat("Scraping:", url, "\n")
  html <- read_html(url) # load html from selected url
  
  # Title
  title <- 
    html %>% # pass html
    html_node("h1.articulo-titulo") %>% # isolate title
    html_text(trim = TRUE) # extract title and trim whitespace
  
  #Subtittle
  subtittle <- html %>% 
    html_nodes("div.articulo-subtitulos") %>% 
    html_text( trim=TRUE)
  
  # Author
  author <- 
    html %>% # pass html
    html_node("div.firma") %>% # isolate author
    html_text(trim = TRUE) # extract author and trim whitespace
  
  # Date
  date <- 
    html %>% # pass html
    html_nodes("div.articulo-datos time") %>% # isolate date
    html_attr("datetime") # extract date
  #tags
  tags <- html %>% 
    html_nodes("div.articulo-tags__interior") %>% 
    html_text(trim=TRUE)
  
  # Text
  text <- 
    html %>% # pass html
    html_nodes("div.articulo-cuerpo p") %>% # isolate text by paragraph
    html_text(trim = TRUE) # extract paragraphs and trim whitespace
  
  # Check to see if the article is text based
  # - only one paragraph suggests a non-text article (cartoon/ video/ album)
  if (length(text) > 1) { 
    # Create tibble/data.frame
    return(tibble(url, title, author, date, tags, text, paragraph = (1:length(text))))
  } else {
    message("Non-text based article. Link skipped.")
    return(NULL)
  }
}

#WRITE THE TABULAR DATA TO DISK
write_content <- function(content, target_file) {
  # Function: Write the tibble content to disk. Create the directory if
  # it does not already exist.
  target_dir <- dirname(target_file) # identify target file directory structure
  dir.create(path = target_dir, recursive = TRUE, showWarnings = FALSE) # create directory
  write_csv(content, target_file) # write csv file to target location
  cat("Content written to disk!\n")
}

#FINAL FUNCTION THAT JOINS ALL THE FUNCTIONS
download_elpais_tag <- function(tag_name, sample_size, target_file, force = FALSE) {
  # Function: Download articles from elpais.com based on tag name. Select
  # number of archive pages to consult, then scrape and write the content 
  # to disk. If the target file exists, do not download again.
  if(!file.exists(target_file) | force == TRUE) {
    cat("Downloading data.\n")
    get_archive_pages(tag_name, sample_size) %>% # select tag archive pages
      map(get_content_links) %>% # get content links from pages sampled
      combine() %>% # combine the results as a single vector
      map(get_content) %>% # get the content for each content link
      bind_rows() %>% # bind the results as a single tibble
      write_content(target_file) # write content to disk
  } else {
    cat("Data already downloaded!\n")
  }
}


                #APPLY FINAL FUNCTION
download_elpais_tag(tag_name = "mena_menores_extranjeros_no_acompanados", 
                    target_file = "data/elpais_menas.csv",
                    sample_size = 7)#hay 7 páginas, quiero las 7
