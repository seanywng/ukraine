
  
url <- "http://stackoverflow.com/questions/3746256/extract-links-from-webpage-using-r"
html <- paste(readLines(url), collapse="\n")
library(stringr)
matched <- str_match_all(html, "<a href=\"(.*?)\"")

# Use html_nodes to select the elements you want to scrape
headings <- html_nodes(webpage, ".card-title > a")

# Extract the text from the selected elements
headings_text <- html_text(headings)

# Print the scraped text
headings_text

url <- "https://reliefweb.int/updates?advanced-search=%28PC241%29_%28DA20220101-20230104%29&page=0"
page <- read_html(url)
page %>% html_nodes(".rw-river-article__title a") %>% html_attr("href") %>% as_tibble()


## This method works, but perhaps is not the best option ##

scrape_links <- function(site) {
  
  site %>% 
    read_html() %>%  
    html_elements("a") %>% 
    html_attr("href") %>% 
    as_tibble() %>% 
    filter(str_detect(value, "https://"))
}

results <-  
  bind_rows(map(links, scrape_links))

results %>% 
  write_csv("reliefweb_ukr_links.csv")