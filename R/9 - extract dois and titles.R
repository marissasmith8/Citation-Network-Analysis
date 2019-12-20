library(rscopus)
library(glue)
library(tidyverse)
library(readxl)
library(purrr)
library(curl)
library(rvest)
load("data/full_text_searches.rdata")


# test starting point -----------------------------------------------------



# bahl_search <- scopus_search("TITLE-ABS-KEY(Comparison of electronic cigarette refill fluid cytotoxicity using embryonic and adult models)")
# 
# bahl_md <- metadata_retrieval("SCOPUS_ID:84866168663")


# reading in titles -------------------------------------------------------


titles <- read_xlsx("./data/Refs full - corrected.xlsx", sheet = "Full References ")[1]
ids <- read_xlsx("./data/Refs full - corrected.xlsx", sheet = "Clean References ")[1]

full_df <- bind_cols(titles, ids) 

index <- c(1, which(is.na(full_df[1]))+1)

df <- full_df[-index,] %>% 
  filter(!is.na(`Full Reference`),
         `Full Reference` != "No references given")

dfb <-
  df %>% 
    # mutate(orgname = str_replace_all(Reference, "(\\d|[^\\w\\s])", "")) %>% 
    mutate(orgname = str_remove_all(Reference, "\\(.*\\)")) %>%
    mutate(orgname = str_remove_all(orgname, "[:punct:]")) %>%
    mutate(orgname = str_trim(orgname)) %>% 
    mutate(trunc_ref = str_remove_all(`Full Reference`, orgname)) %>% 
  mutate(search1 = str_split(trunc_ref, "(\\.,? |/|;|\\&|[A-Z]{1,2}(\\.|,))")) %>% 
  mutate(max = search1 %>% map_dbl(~ nchar(.x) %>% which.max())) %>% 
  mutate(search1 = map2_chr(.$search1, .$max, ~ pluck(.x, .y)),
         search2 = str_trunc(`Full Reference`, 150) %>% str_remove_all("[:punct:]") %>% str_replace_all("(^| )[A-Z]{1,2}( |[:punct:])", " "),
         doi = ifelse(grepl("doi", .$`Full Reference`),
                      gsub("^.*(doi: ?|dx.doi.org/|doi/)(\\S*\\d)(\\.?).*$", "\\2", .$`Full Reference`),
                      NA))



dfc <- dfb %>%
  select(Reference, search2, doi) %>%
    mutate(search_title = 
  str_remove_all(search2, "[\\(\\)\\d]")   %>%
  str_remove("^(or|and|[:punct:])") %>%
  str_trim() %>%
  str_remove("(or|and|[:punct:])$") %>%
  str_trim()) %>%
  group_by(Reference) %>%
  mutate(nchar = nchar(search_title)) 

df_urls <-  dfb %>% 
  mutate(url = str_extract(`Full Reference`, "(http|ftp|https)://([\\w_-]+(?:(?:\\.[\\w_-]+)+))([\\w.,@?^=%&:/~+#-]*[\\w@?^=%&/~+#-])?")) %>% 
  select(Reference, `Full Reference`, doi, url)

searches <- dfc %>% 
  select(Reference, doi) %>% 
  filter(!is.na(doi)) %>% 
  full_join(dfc %>% select(-doi), by = "Reference") %>% 
  top_n(1,nchar) %>% 
  unique()


# search 2 using first 100 characters -------------------------------------


for (j in 1:nrow(searches)){
  
  if(j==62){next}
  
  print(glue("Searching {j} of {nrow(searches)} records"))
  
  if(!is.na(searches[j,"doi"])) {next}
  
  err <- FALSE
  
  p <- 1
  
  search_str <- searches[j, "search2"]
  
  tryCatch(
    repeat({
        test <- scopus_search(paste0("TITLE-ABS-KEY-AUTH(", search_str, ")"))
        p <- p+1
        search_str <- str_remove(search_str, "^\\w* ") %>% str_remove(" \\w*$")
      if(test$total_results != 0 | p == 3){break}
    }),
    error = function(e){err = TRUE})
  
  if(err) {next}
  
  searches[j, "nresults"] <- test$total_results
  
  
  if(test$total_results>0){
    
    searches[j, "title"]     <- test[["entries"]][[1]][["dc:title"]]
    searches[j, "au1"]       <- ifelse(!is.null(test[["entries"]][[1]][["dc:creator"]]), test[["entries"]][[1]][["dc:creator"]], NA)
    searches[j, "doi"]       <- ifelse(!is.null(test[["entries"]][[1]][["prism:doi"]]), test[["entries"]][[1]][["prism:doi"]], NA)
    searches[j, "scopus_id"] <- test[["entries"]][[1]][["dc:identifier"]]
    searches[j, "journal"]   <- ifelse(!is.null(test[["entries"]][[1]][["prism:publicationName"]]), test[["entries"]][[1]][["prism:publicationName"]], NA)
    
    
  }
  
}



# write dois to bibtex ----------------------------------------------------

# dois <- searches %>% 
#   filter(!is.na(doi)) %>% 
#   select(Reference, doi)

dois_search <- searches %>% 
  filter(!is.na(doi))


dois <- dois_search %>% mutate(au_x = str_extract(au1, "^\\w*"),
                 ti_x = str_extract(title, "^\\w*")) %>% 
  filter(str_detect(Reference, au_x)) %>% 
  select(Reference, doi) %>% 
  unique() %>% 
  bind_rows(dfb %>% filter(!is.na(doi)) %>% select(Reference, doi))




text1 <- ""
for (k in 1:nrow(dois)) {
text1 <- paste0(text1,
                "\n@article{a1,\ndoi = {", dois$doi[k], ",\nnote = {", dois$Reference[k], "}\n}"
                )  
}
text2 <- ""
for (k in 1:nrow(dois)) {
text2 <- paste0(text2,
                "\n@article{a1,\ndoi = {", dois$doi[k], ",\nnote = {", dois$Reference[k], "}\n}"
                )  
}

  write_file(text1,
           path = "Documents/export1.bib")
write_file(text2,
           path = "Documents/export3.bib")


# joining dfs -------------------------------------------------------------

all_urls <-
  searches %>%
  anti_join(dois, by = "Reference") %>% 
  filter(!is.na(nresults)) %>%
  left_join(df_urls, by = c("Reference", "doi")) %>% 
  mutate(lurl = nchar(url)) %>% 
  arrange(desc(lurl)) %>% 
  mutate(x = n(), y = 1:x) %>% 
  top_n(y)

first_results <- dfb %>%
  select(-doi) %>% 
  left_join(dois, by = "Reference") %>% 
  mutate(url = str_extract(`Full Reference`, "(http|ftp|https)://([\\w_-]+(?:(?:\\.[\\w_-]+)+))([\\w.,@?^=%&:/~+#-]*[\\w@?^=%&/~+#-])?")) %>% 
  mutate(lurl = nchar(url)) %>% 
  arrange(doi, desc(lurl)) %>%
  group_by(Reference) %>% 
  mutate(x = n(), y = 1:x) %>% 
  top_n(1, -y) %>%
  left_join(doc_dirs %>% select(Reference, dir) %>% filter(!is.na(dir)), by = "Reference") %>% 
  select(-lurl, -x, -y)

pdf_urls <- all_urls %>% 
  filter(str_detect(url, "\\.pdf")) %>% 
  select(Reference, url)

for (i in 4:nrow(pdf_urls)){
  jump <-  FALSE
  tryCatch({
      download.file(pdf_urls$url[i], glue("Documents/pdfs/{pdf_urls$Reference[i]}.pdf"),
                    mode = "wb")
  }, error = function(e){jump <- TRUE})
  if(jump){next}
}

doc_dirs <- tibble(
  Reference = str_remove(list.files("Documents/pdfs"), ".pdf$"),
  dir = paste0("Documents/pdfs/", list.files("Documents/pdfs"))
) %>% right_join(all_urls, by = "Reference")


# save(searches, dfc, dfb, doc_dirs, first_results, file = "data/full_text_searches.rdata")





# search 3 - updated strategy -----------------------------------------------------------

library(tidytext)

searches2 <-
  first_results %>% 
  filter(is.na(doi), is.na(dir)) %>% 
  mutate(search3 = `Full Reference` %>% 
           str_remove_all("[:punct:]") %>% 
           str_replace_all("(^| )[A-Z]{1,2}( |[:punct:])", " ") %>% 
           str_remove_all("\\w*$") %>% 
           str_replace_all("\\d*", "") %>% 
           str_remove_all("http\\w*") %>% 
           str_trim(side = "both"))

for (j in 1:nrow(searches2)){

  
  search_words <-   searches2[j, 'search3'] %>% 
    unnest_tokens(word, search3) %>% 
    anti_join(stop_words) %>% 
    unique()
  
  err <- FALSE
  
  p <- 1
  
  print(glue("Searching {j} of {nrow(searches2)} records"))
  
  tryCatch(
    repeat({
      test <- scopus_search(paste0("TITLE-ABS-KEY-AUTH(", paste0(search_words$word, collapse  = " AND "), ")"))
      p <- p+1
      search_str <- str_remove(search_str, " \\w*$")
      if(test$total_results != 0 | p == 3){break}
    }),
    error = function(e){err = TRUE})
  
  if(err) {next}
if(test$total_results == 0){
  p <- 10
    tryCatch(
    repeat({
      test <- scopus_search(paste0("TITLE-ABS-KEY-AUTH(", paste0(sample(search_words$word, p, replace = TRUE), collapse  = " AND "), ")"))
      p <- p-1
      search_str <- str_remove(search_str, " \\w*$")
      if(test$total_results != 0 | p == 5){break}
    }),
    error = function(e){err = TRUE})
  
  if(err) {next}
}
  searches2[j, "nresults"] <- test$total_results
  
  
  if(test$total_results>0){
    
    searches2[j, "title"]     <- test[["entries"]][[1]][["dc:title"]]
    searches2[j, "au1"]       <- ifelse(!is.null(test[["entries"]][[1]][["dc:creator"]]), test[["entries"]][[1]][["dc:creator"]], NA)
    searches2[j, "doi"]       <- ifelse(!is.null(test[["entries"]][[1]][["prism:doi"]]), test[["entries"]][[1]][["prism:doi"]], NA)
    searches2[j, "scopus_id"] <- test[["entries"]][[1]][["dc:identifier"]]
    searches2[j, "journal"]   <- ifelse(!is.null(test[["entries"]][[1]][["prism:publicationName"]]), test[["entries"]][[1]][["prism:publicationName"]], NA)
    
    
  }
  
  
}


# testing search2 results -------------------------------------------------

dois_s2 <- searches2 %>% 
  filter(!is.na(doi)) %>%
  mutate(au_x = str_extract(au1, "^\\w*")) %>% 
  filter(str_detect(Reference, au_x)) %>% 
  select(Reference, doi) %>% 
  anti_join(first_results, by = "doi")

text3 <- ""
for (k in 1:nrow(dois_s2)) {
  text3 <- paste0(text3,
                  "\n@article{a1,\ndoi = {", dois_s2$doi[k], ",\nnote = {", dois_s2$Reference[k], "}\n}"
  )  
}

write_file(text3,
           path = "Documents/export4.bib")



# google scholar search test # DEFUNCT ----------------------------------------------

searches3 <- searches2 %>% 
  select(-doi) %>% 
  left_join(dois_s2, by = "Reference") %>%
  filter(is.na(doi), is.na(dir)) %>% 
  select(-doi) %>% 
  left_join(dois, by = "Reference") %>% 
  filter(is.na(doi), is.na(dir))

test_search <- searches3$`Full Reference`[645]

scholar_search <- searches3 %>% 
  filter(is.na(url))

# save(searches, searches2, searches3, dois, dois_s2, scholar_search, dfc, dfb, doc_dirs, first_results, file = "data/full_text_searches.rdata")

for (t in 1:20){
# for (t in 1:nrow(scholar_search)){
  
  print(glue("Searching {t} of {nrow(scholar_search)} records"))
  
  Sys.sleep(5)

  
  tryCatch({
    
       doi1 <-  scholar_search[[t, "Full Reference"]] %>%
        str_to_lower() %>%
        str_remove("(http|ftp|https)://([\\w_-]+(?:(?:\\.[\\w_-]+)+))([\\w.,@?^=%&:/~+#-]*[\\w@?^=%&/~+#-])?") %>% 
        str_remove("available from") %>% 
        str_remove_all("[^a-z ]") %>% 
        str_trim() %>%
        str_squish() %>% 
        str_replace_all(" ", "+") %>% 
        paste0("https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=", .) %>% 
        read_html() %>% 
        html_node(".gs_rt a") %>% 
        html_attr("href") %>% 
        read_html() %>% 
        html_node("body") %>% 
        html_text() %>% 
        str_extract("doi.org/[:graph:]+")
     print(doi1)
       
}, error = function(e) {message("Failed!")}, finally = function(f) {scholar_search[t, "doi"] <- doi1})

  
      # scholar_search[t, "doi"] <- doi1
  
}


"https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=nides+ma+et+al+nicotine+blood+levels+and+shortterm+smoking+reduction+with+an+electronic+nicotine+delivery+system+american+journal+of+health+behavior+p" %>% 
  read_html() %>% 
  html_node(".gs_rt a") %>% 
  html_attr("href") %>% 
  read_html() %>% 
  html_node("body") %>% 
  html_text() %>% 
  str_extract("doi.org/[:graph:]+")


# joining to get full dataframe so far ------------------------------------

# search1 full results

results1 <- searches %>%
  mutate(au_x = str_extract(au1, "^\\w*")) %>% 
  filter(str_detect(Reference, au_x)) %>% 
  select(Reference, doi, title, au1, scopus_id, journal) %>% 
  right_join(first_results %>% select(`Full Reference`, Reference, doi, url, dir), by = c("Reference", "doi")) %>% 
  filter(!(is.na(doi)&is.na(dir))) %>% 
  select(Reference, `Full Reference`, title, au1, journal, doi, scopus_id, url, dir)

# search2 full results join

results2 <-
  searches2 %>% 
  mutate(au_x = str_extract(au1, "^\\w*")) %>% 
  filter(str_detect(Reference, au_x)) %>% 
  bind_rows(results1) %>% 
    unique() %>% 
  select(Reference, `Full Reference`, title, au1, journal, doi, scopus_id) 

joined_results <- first_results %>%
  select(`Full Reference`, Reference, doi, url, dir) %>% 
  filter(!(Reference %in% results2$Reference)) %>% 
  bind_rows(results2) %>% 
  left_join(dfe %>% select(Reference, nrefs), by = "Reference") %>% 
  arrange(desc(nrefs, doi)) %>% unique()


save(joined_results, file = "data/search_in_progress.rdata")

write_csv(joined_results %>% filter(!is.na(doi)), "data/complete.csv")

write_csv(joined_results, "data/Screen_input.csv")

# join to google sheets ---------------------------------------------------

library(googlesheets)

sheets_results <- gs_title("Ongoing results screening")
# keeping track

progress_df <- sheets_results %>% gs_read(ws = 2)

progress_df <- progress_df %>% 
  mutate(n_complete = joined_results %>% filter(!is.na(title)|!is.na(dir)) %>% nrow(),
         n_doi = joined_results %>% filter(!is.na(doi)) %>% nrow(),
         n_complete_r1 = joined_results %>% filter(!is.na(doi), nrefs > 2) %>% nrow(),
         n_complete_r2 = joined_results %>% filter(!is.na(doi), nrefs > 1) %>% nrow())


sheets_results %>% 
  gs_edit_cells(ws = "Progress", input = progress_df)

sheets_results <- sheets_results %>% gs_ws_new(ws_title = "Screen_input", input = results2[1:2,])

for (i in 5:nrow(results2)){
  gs_add_row(sheets_results, "Screen_input", input = results2[i,])
}

gs_ws

# further categorise by search

results2 %>% 
  mutate(news = ifelse(str_detect(url,
                                  paste(
                                    "theguardian",
                                    "dailymail",
                                    "independent.co.uk",
                                    "washingtonpost",
                                    "nytimes",
                                    "bbc.co.uk",
                                    "mirror.co.uk",
                                    "scotsman.com",
                                    sep = "|"
                                    )),
                       1, 0),
         )

