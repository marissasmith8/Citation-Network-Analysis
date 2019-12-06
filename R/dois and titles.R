library(rscopus)
library(glue)

bahl_search <- scopus_search("TITLE-ABS-KEY(Comparison of electronic cigarette refill fluid cytotoxicity using embryonic and adult models)")

bahl_md <- metadata_retrieval("SCOPUS_ID:84866168663")


# reading in titles -------------------------------------------------------

library(tidyverse)
library(readxl)

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
         search2 = str_trunc(`Full Reference`, 150) %>% str_remove_all("[:punct:]") %>% str_replace_all(" [A-Z]{1,2} ", " "),
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


searches <- dfc %>% 
  select(Reference, doi) %>% 
  filter(!is.na(doi)) %>% 
  full_join(dfc %>% select(-doi), by = "Reference") %>% 
  top_n(1,nchar) %>% 
  unique()


# search 2 using first 100 characters -------------------------------------


for (j in 828:nrow(searches)){
  
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


# abs <- list()

# for (i in 1:length(dois)){
# abs[[i]] <- abstract_retrieval(dois[i], "doi")
# }



# write dois to bibtex ----------------------------------------------------

# dois <- searches %>% 
#   filter(!is.na(doi)) %>% 
#   select(Reference, doi)

dois_search <- searches %>% 
  filter(!is.na(doi))

dois <- dois_search %>% mutate(au_x = str_extract(au1, "^\\w*"),
                 ti_x = str_extract(title, "^\\w*")) %>% 
  filter(str_detect(Reference, au_x)|
         is.na(nresults)) %>% 
  select(Reference, doi) %>% 
  unique()




text1 <- ""
for (k in 1:nrow(dois)) {
text1 <- paste0(text1,
                "\n@article{a1,\ndoi = {", dois$doi[k], ",\nnote = {", dois$Reference[k], "}\n}"
                )  
}

  write_file(text1,
           path = "Documents/export1.bib")
write_file(text1,
           path = "Documents/export2.bib")

