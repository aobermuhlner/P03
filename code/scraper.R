library(rvest)

url <- "https://fis.fda.gov/extensions/FPD-QDE-FAERS/FPD-QDE-FAERS.html"

links <- read_html(url) |>
  html_nodes(xpath = '//td[@class="fpd-td"]/a') |>
  html_attr("href")

print(links)

for (i in 1:5){
  local_file <- gsub("https://fis.fda.gov/content/Exports/", "data/", links[i])
  curl::curl_download(links[i], destfile = local_file)
  unzip(local_file, exdir = "data/")
}