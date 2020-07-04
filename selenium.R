#selenium
# RSelenium::startServer() if required
library(RSelenium)

driver <- rsDriver(port = 4444L, browser=c("firefox"))
remote_driver <- driver[["client"]]
remote_driver$open()

remote_driver$navigate("https://www.lavanguardia.com/search?q=%22menores%20extranjeros%22#gsc.tab=0&gsc.q=%22menores%20extranjeros%22&gsc.page=1")
cuerpo<-remote_driver$findElement(using = 'css',  ".gsc-webResult")
cuerpotxt <- cuerpo$getElementAttribute("outerHTML")[[1]] # gets us the HTML

cuerpoxml <- htmlTreeParse(cuerpotxt, useInternalNodes=T) # parse string into HTML tree to allow for querying with XPath
fundList <- unlist(xpathApply(cuerpoxml, '//*[contains(concat( " ", @class, " " ), concat( " ", "gs-title", " " ))]', xmlGetAttr, 'href')) # parses out just the fund name and ticker using XPath
vanguardia_articulos <- unique(fundList)


a <- vanguardia_get_content_links("https://www.lavanguardia.com/search?q=%22menores%20extranjeros%22#gsc.tab=0&gsc.q=%22menores%20extranjeros%22&gsc.page=1")
b <- vanguardia_get_content_links("https://www.lavanguardia.com/search?q=%22menores%20extranjeros%22#gsc.tab=0&gsc.q=%22menores%20extranjeros%22&gsc.page=2")
vanguardia_get_content_links()


html_nodes(".story-leaf-datetimepub") %>% 
  html_attr("datetime") 

h1.story-leaf-title.
html <- read_html("https://www.lavanguardia.com/vida/20110224/54118732809/denuncian-la-expulsion-de-una-veintena-de-menores-extranjeros-de-centros-de-acogida.html")
tittle <- html %>% 
  html_nodes(".story-leaf-title") %>% 
  html_text( trim=TRUE)

text <- html %>% 
  html_nodes(".story-leaf-txt-p p") %>% 
  html_text( trim=TRUE)
text <- text[-1]
