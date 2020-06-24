library(googlesheets4)
library(readr)
library(tidyverse)
library(rscopus)
library(fuzzyjoin)
library(janitor)

# sheet <- gs_title("Ongoing results screening")

# sheets_results <- gs_read(sheet, ws = "complete", col_types = cols("dir" = "c", "nrefs" = "i", "url" = "c"))

# save(sheets_results, file = "data/googlesheets_results.rda")


# importing from pdfs -----------------------------------------------------

library(pdftools)
library(stringr)
library(tabulizer)
library(fuzzyjoin)

load("data/googlesheets_results.rda")
sheets_results <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1boszwpnoQ-2stzg396kcM7eh0fibB2oA_voZEVw-rWs/edit#gid=134539849", sheet = "complete")
files <- list.files(path = "Documents/E-cigarette citation library.Data/PDF/", recursive = TRUE, pattern = "*.pdf")
dois <- str_remove_all(files, "(^\\d*/|-\\d*\\.pdf$)") %>% 
  str_remove("\\.pdf$") %>% 
  unique() 

dirs <- vector("character", length(dois))

for (i in 1:length(dois)){
  
  dirs[i] <-   files[str_detect(files, dois[i] %>% str_replace("\\(", "\\\\(") %>% str_replace("\\)", "\\\\)"))][1]
  
}

dirs <- paste0("Documents/E-cigarette citation library.Data/PDF/", dirs)

full_texts <- tibble(doi = dois %>% 
                       str_replace_all("_", "/"),
                     dir_pdf = dirs) %>% 
  mutate(doi = str_replace(doi, "J069v19n04/02", "J069v19n04_02"),
         doi = str_replace(doi, "978-3-540-69248-5/2", "978-3-540-69248-5_2"))

conflicts_screening <- inner_join(sheets_results, full_texts, by = "doi") %>% 
  select(Reference, doi, dir = dir_pdf, title, journal, nrefs, type)

# anti_join(full_texts, sheets_results, by = "doi") %>% View()

load("data/full_texts.rda")
# texts <- map_chr(conflicts_screening$dir, ~ extract_text(.x))

conflicts_screening["text"] <- texts %>% 
  str_replace_all("[^\\d\\w\\s]|\n|\r", " ")


conflicts <- conflicts_screening %>% 
  mutate(conflicts = str_extract_all(text, ".{1,100}([Ff]unding|[In]erests|[Cc]onflict|[Dd]eclar).{1,300}")) %>% 
  select(-text)

conflicts <- conflicts %>% 
  mutate(conflicts = map_chr(.$conflicts, ~paste(.x, collapse = "\n")))


# save(conflicts, file = "data/conflicts.rda")
# save(texts, file = "data/full_texts.rda")


# find articles without dois from full text -------------------------------

others <- sheets_results %>% 
  mutate(year = str_extract(Reference, "\\d{4}"),
         au1 = str_extract(au1, "^\\w*")) %>% 
  select(-dir) %>% 
  right_join(results %>% 
               filter(is.na(doi)) %>%
               select(-doi) %>% 
               mutate(au1 = str_extract(au1, "^\\w*")),
             by = c("au1", "year"))

sheets_results %>% filter(str_detect(au1, "Fewell"))  


# articles with full texts already found through endnote ------------------

indexing <- sheets_results %>% 
  mutate(year = str_extract(Reference, "\\d{4}")) %>% 
  filter(!is.na(doi)) %>% 
  select(-dir)


# conflict statements -----------------------------------------------------

# conflicts <- results %>% 
#   mutate(conflicts = str_extract_all(text, ".{1,100}([Ff]unding|[In]erests|[Cc]onflict|[Dd]eclar).{1,300}")) %>% 
#   select(-text)
# 
# conflicts <- conflicts %>% 
#   mutate(conflicts = map_chr(.$conflicts, ~paste(.x, collapse = "\n")))
# 
# View(conflicts)

# save(conflicts, file = "data/conflicts.rda")
# save(texts, file = "data/full_texts.rda")


# upload to google sheets ---------------------------------------------------------


library(googlesheets)
library(tidyverse)

load("data/conflicts.rda")

conflicts_sheet <- gs_title("new_conflicts")

# sheet <- conflicts_sheet %>% gs_read("conflicts")

conflicts_sheet %>% gs_ws_new("conflicts_1203", input = conflicts)

write_csv(conflicts, "data/conflicts.csv")

# gs_upload("data/conflicts.csv", sheet_title = "new_conflicts")

# vis results -------------------------------------------------------------


sheets_results <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1boszwpnoQ-2stzg396kcM7eh0fibB2oA_voZEVw-rWs/edit#gid=2077854862", sheet = "complete")

# sheets_results %>% group_by(type) %>% tally() 

sheets_tidied <- sheets_results %>%
  select(Reference,type) %>% 
  unique()

tots <-   sheets_tidied %>% 
  group_by(type) %>% 
  tally() %>% 
  select(type, `Total across all contexts` = n) 

tots %>% 
    write.csv("outputs/results.csv")

## Numbered by doc - not used?
# sheets_tidied %>% 
#   left_join(dfe,by="Reference") %>% 
#   select(-nrefs, -nr_fill, -Reference) %>% 
#   pivot_longer(-type, names_to = "gl_doc", values_to = "cited") %>% 
#   group_by(gl_doc,type) %>% 
#   summarise(ncited=sum(cited,na.rm = TRUE)) %>% 
#   pivot_wider(names_from = gl_doc, values_from = ncited) %>% 
#   adorn_totals(where=c("row","col"))
 

  sheets_tidied %>% 
    left_join(dfe,by="Reference") %>% 
    select(-nrefs, -nr_fill) %>% 
    pivot_longer(-c(type, Reference), names_to = "gl_doc", values_to = "cited") %>%
    left_join(gl_cols %>% 
                mutate(gl_doc =str_replace_all(Report,"\\s","\\.")) %>% 
                select(gl_doc,context  ),by="gl_doc") %>% 
    filter(cited==1) %>% 
    select(-gl_doc) %>% 
    unique() %>% 
    select (-Reference) %>% 
    group_by(context,type) %>% 
    summarise(ncited=sum(cited,na.rm = TRUE)) %>% 
    pivot_wider(names_from = context, values_from = ncited, values_fill = list(ncited=0)) %>% 
    right_join(tots, by = "type") %>% 
    adorn_totals(where="row") %>% 
    export::table2doc(digits=0,"outputs/citation context groups.docx")


 

# defunct - trial rscopus full texts --------------------------------------

# 
# load("data/googlesheets_results.rda")
# 
# get_data <- function(scopus_id = NULL, doi = NULL) {
#   if((is.null(scopus_id))&(is.null(doi))){
#     stop("Provide doi or scopus_id")
#   }
#   
#   if(!is.null(scopus_id)){
#   text <- article_retrieval(scopus_id, view = "FULL", identifier = "scopus_id")
#   } else {
#   text <- article_retrieval(doi, view = "FULL", identifier = "doi")
#   }
#   return(text)
# }
# 
# get_full_text <- function(scopus_id = NULL, doi = NULL) {
#   data <- get_data(scopus_id, doi)
#   if(!is.null(data$content$`service-error`$status$statusCode)){
#     text <- "None"
#   } else {
#   text <- data$content$`full-text-retrieval-response`$originalText
#   }
#   
#   return(text)
# }
# 
# get_conflicts <- function(scopus_id = NULL, doi = NULL) {
#  conflicts <- get_full_text(scopus_id, doi) %>% str_extract_all("[Cc]onflict\\.{1,300}") 
#  return(conflicts)
# }




# conflicts for top ones --------------------------------------------------

search1 <-  sheets_results %>% 
  filter(!is.na(scopus_id), type == "Journal article") %>% 
  arrange(scopus_id) 

for (i in 1:nrow(search1)){
 search1$text <- get_full_text(search1[[i, "scopus_id"]]) 
} 



search1$text

