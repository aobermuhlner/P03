library(rvest)


url <- "https://fis.fda.gov/extensions/FPD-QDE-FAERS/FPD-QDE-FAERS.html"

# HTML-Elemente mit den Links finden und extrahieren
links <- read_html(url) %>% 
  html_nodes(xpath = '//td[@class="fpd-table"]/a') %>% 
  html_attr("href")

# Ausgabe der Links
print(links)
