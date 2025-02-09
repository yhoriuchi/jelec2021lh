#' Download, merge, and wrangle municipality-level election data
#' 2024 House of Representatives Election in Japan
#' Step 1: Download files

# Initial settings --------------------------------------------------------

library(tidyverse)
library(rvest)

# Make a list of excel files to be downloaded -----------------------------

# URL of the webpage
url <- "https://www.soumu.go.jp/senkyo/senkyo_s/data/shugiin50/shikuchouson.html"

# Read the webpage
page <- read_html(url)

# Extract all hyperlinks
links <- page %>% html_nodes("a") %>% html_attr("href")
texts <- page %>% html_nodes("a") %>% html_text()

# Remove NA and filter only valid links
valid_links <- !is.na(links) & links != ""
links <- links[valid_links]
texts <- texts[valid_links]

# Convert relative links to absolute links
base_url <- "https://www.soumu.go.jp/senkyo/senkyo_s/data/shugiin50/"
full_links <- ifelse(grepl("^http", links), links, paste0(base_url, links))

# Further wrangling
hyperlinks <- data.frame(url = hyperlink_vector, 
                         text = texts) %>% 
  filter(str_detect(url, "main_content") & text != "") %>% 
  mutate(url = str_remove(url, "senkyo/senkyo_s/data/shugiin50//"))

pr_links <- hyperlinks %>% 
  filter(str_detect(text, "選挙区")) 

smd_links <- hyperlinks %>% 
  filter(!str_detect(text, "選挙区")) 

# Download all files ------------------------------------------------------

for (i in 1:47){
  
  source1 <- smd_links[i, 1]
  destfile1 <- paste0("downloaded/smd/", smd_links[i, 2], ".xlsx")
  download.file(source1, destfile1)

}

for (i in 1:11){
  
  source2 <- pr_links[i, 1]
  destfile2 <- paste0("downloaded/pr/",  pr_links[i, 2], ".xlsx")
  download.file(source2, destfile2)
  
}
