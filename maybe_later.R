
  
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

bigrams_full <- rw %>% 
  select(id, body) %>% 
  unnest_tokens(bigram, body, token = "ngrams", n = 2) %>% 
  filter(!is.na(bigram)) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word & (nchar(word1) < 20)) %>% 
  filter(!word2 %in% stop_words$word & (nchar(word2) < 20)) %>% 
  filter(str_detect(word1, "[a-z]") & !str_detect(word1, "<")) %>% 
  filter(str_detect(word2, "[a-z]") & !str_detect(word2, "<")) %>% 
  unite(bigram, word1, word2, sep = " ")

bigram_patchwork <- list()

bigram_frequency_by_source <- function(source_) {
  
  source <- enquo(source_)
  sourcen <- quo_name(source_)
  
  tf_idf_bigrams %>% 
    filter(source == !!source) %>%
    arrange(desc(tf)) %>% 
    head(20) %>%
    ggplot(aes(x = tf, 
               y = fct_reorder(word, tf, .fun = sum)))+
    geom_col(show.legend = FALSE, 
             fill = "cornflowerblue") + 
    labs(x = "Term frequency", y = "", 
         title = paste0("Term frequency: ", sourcen), 
         subtitle = "2022-01-01 to 2023-01-01") + 
    
    tf_idf_bigrams %>%
    filter(source == !!source) %>% 
    arrange(desc(tf_idf)) %>% 
    head(20) %>%
    ggplot(aes(x = tf_idf, 
               y = fct_reorder(word, tf_idf, .fun = sum)))+
    geom_col(show.legend = FALSE, 
             fill = "tomato") + 
    labs(x = "Tf-idf", y = "", 
         title = paste0("Tf-idf: ", sourcen), 
         subtitle = "2022-01-01 to 2023-01-01") +
    
    tf_idf_bigrams %>% 
    filter(source == !!source) %>%
    filter(!is.infinite(log_odds_weighted)) %>% 
    arrange(desc(log_odds_weighted)) %>% 
    head(20) %>%
    ggplot(aes(x = log_odds_weighted, 
               y = fct_reorder(word, log_odds_weighted, .fun = sum)))+
    geom_col(show.legend = FALSE, 
             fill = "forestgreen") + 
    labs(x = "Log odds", y = "", 
         title = paste0("Log odds: ", sourcen), 
         subtitle = "2022-01-01 to 2023-01-01")  
  
}

bigram_patchwork <- sources %>% 
  
  
  
  
  penguins %>%
  select(ends_with("_mm")) %>%
  colnames() |> 
  purrr::walk(my_plot)


sectors <- c("Agriculture",
             "Camp Coordination and Camp Management",
             "Climate Change and Environment",
             "Contributions",
             "Coordination",
             "Disaster Management",
             "Education",
             "Food and Nutrition",
             "Gender",
             "Health",
             "HIV/Aids",
             "Humanitarian Financing",
             "Logistics and Telecommunications",
             "Mine Action",
             "Peacekeeping and Peacebuilding",
             "Protection and Human Rights",
             "Recovery and Reconstruction",
             "Safety and Security",
             "Shelter and Non-Food Items",
             "Water Sanitation Hygiene")

frequency_by_source <- function(tbl, source) {
  
  source <- enquo(source)
  sourcen <- quo_name(source)
  
  tbl %>% 
    filter(source == !!source) %>%
    arrange(desc(tf)) %>% 
    head(20) %>%
    ggplot(aes(x = tf, 
               y = fct_reorder(word, tf, .fun = sum)))+
    geom_col(show.legend = FALSE, 
             fill = "cornflowerblue") + 
    labs(x = "Term frequency", y = "", 
         title = paste0("Term frequency -- ", sourcen), 
         subtitle = "2022-01-01 to 2023-01-01") + 
    
    tbl %>%
    filter(source == !!source) %>% 
    arrange(desc(tf_idf)) %>% 
    head(20) %>%
    ggplot(aes(x = tf_idf, 
               y = fct_reorder(word, tf_idf, .fun = sum)))+
    geom_col(show.legend = FALSE, 
             fill = "tomato") + 
    labs(x = "Tf-idf", y = "", 
         title = paste0("Tf-idf -- ", sourcen), 
         subtitle = "2022-01-01 to 2023-01-01") + 
    
    tbl %>%
    filter(source == !!source) %>%
    filter(!is.infinite(log_odds_weighted)) %>% 
    arrange(desc(log_odds_weighted)) %>%
    head(15) %>%
    ggplot(aes(x = log_odds_weighted,
               y = fct_reorder(word, log_odds_weighted))) +
    geom_col(show.legend = FALSE,
             fill = "forestgreen") +
    labs(x = "Log odds",y = "",
         title = paste0("Term frequency -- ", sourcen),
         subtitle = "2022-01-01 to 2023-01-01")
  
}

frequency_by_source(tf_idf, "UNDP")

![](./plots/title_network_graph.png)

A brief examination of the most common word pairs in RW article titles yields this network graph. As with the graph above, the thickness of the line indicates the number of co-occurrences. 

<br>
  
  [![title network graph](./plots/title_network_graph.png)](https://raw.githubusercontent.com/seanywng/ukraine-reliefweb/main/plots/title_network_graph.png){target="_blank"}
*[Full-sized graph](https://raw.githubusercontent.com/seanywng/ukraine-reliefweb/main/plots/title_network_graph.png){target="_blank"}*
  
