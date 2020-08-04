library(tidyverse)
library(googlesheets4)

source("./R/20- Tidying dataframe to remove duplicates new.R")


titles <- readxl::read_xlsx("./data/Refs full - corrected Jul20.xlsx", sheet = "Full References")[,1:2]

titles_f <- titles %>% 
  group_by(Reference) %>% 
  mutate(id = row_number()) %>% 
  filter(id == 1) %>% 
  select(-id)

screen_input <- dfe %>% 
  select(Reference, nrefs) %>% 
  left_join(titles_f, by = "Reference")

write_csv(screen_input, "data/Screen_input.csv")


# get dois and export to endnote ------------------------------------------

sheets_results <- read_sheet("https://docs.google.com/spreadsheets/d/1boszwpnoQ-2stzg396kcM7eh0fibB2oA_voZEVw-rWs/edit#gid=134539849", sheet = "complete")
already_done <- read_sheet("https://docs.google.com/spreadsheets/d/1Dg1b2GwAxX07zXZQaO2Rb9U8Of3xhRwn25IULYRNAh4", sheet = "conflicts_1203")

shiny_extr_dois <-
  dfe %>% 
  left_join(sheets_results, by = "Reference") %>% 
  filter(!is.na(doi), doi != "NA") %>% 
  group_by(Reference) %>% 
  mutate(id = seq_along(Reference)) %>% 
  filter(id == 1) %>% 
  select(-id) %>% 
  anti_join(already_done, by = "Reference")



text4 <- ""
for (k in 1:nrow(shiny_extr_dois)) {
  text4 <- paste0(text4,
                  "\n@article{a1,\ntitle = {", shiny_extr_dois$title[k], "},\ndoi = {", shiny_extr_dois$doi[k], "},\nnote = {", shiny_extr_dois$Reference[k], "}\n}"
  )  
}

write_file(text4,
           path = "Documents/export6.bib")


# importing from pdfs -----------------------------------------------------

library(tabulizer)


sheets_results <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1boszwpnoQ-2stzg396kcM7eh0fibB2oA_voZEVw-rWs/edit#gid=134539849", sheet = "complete")

files <- list.files(path = "Documents/new_pdfs", recursive = TRUE, pattern = "*.pdf")

dirs <- tibble(files = files, 
               dirs = paste0("Documents/new_pdfs/", files),
               Reference = str_remove(files, "\\.pdf$"))


conflicts_screening <- inner_join(sheets_results, dirs, by = "Reference") %>% 
  select(Reference, doi, dirs, title, journal, nrefs, type)

# anti_join(full_texts, sheets_results, by = "doi") %>% View()

# load("data/full_texts.rda")
texts <- map_chr(conflicts_screening$dirs, ~ extract_text(.x))

conflicts_screening["text"] <- texts %>% 
  str_replace_all("[^\\d\\w\\s]|\n|\r", " ")


conflicts <- conflicts_screening %>% 
  mutate(conflicts = str_extract_all(text, ".{1,100}([Ff]unding|[In]erests|[Cc]onflict|[Dd]eclar).{1,300}")) %>% 
  select(-text)

conflicts <- conflicts %>% 
  mutate(conflicts = map_chr(.$conflicts, ~paste(.x, collapse = "\n")))

sheet_append(data = conflicts, ss = "https://docs.google.com/spreadsheets/d/1Dg1b2GwAxX07zXZQaO2Rb9U8Of3xhRwn25IULYRNAh4", sheet = "conflicts_1203")
