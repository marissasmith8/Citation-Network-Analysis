library(readxl)
library(tidyverse)
library(rebus)

full <- read_xlsx("Refs full (08.04.19).xlsx", sheet = "Full References ")[,1]
clean <- read_xlsx("Refs full (08.04.19).xlsx", sheet = "Clean References ")[,1]

table(is.na(full))
table(is.na(clean))

refs <- colnames(read_xlsx("Refs full (08.04.19).xlsx", sheet = "Clean References "))[2:19]
cleannames <- c("WHO 2014", clean[(which(is.na(clean$Reference))+1),][["Reference"]])
fullnames <- c("WHO 2014", full[(which(is.na(full$`Full Reference`))+1),][["Full Reference"]])

tibble(refs, full = refs %in% full$`Full Reference`,
clean = refs %in% clean$Reference,
refname = cleannames,
fullname = fullnames)

full2 <- full[which(!full$`Full Reference` %in% fullnames), ]
clean2 <- clean[which(!clean$Reference %in% cleannames), ]

full3 <- rbind(full2[1:1356,], tibble("Full Reference" = "National Institute for Health and Care Excellence (2013)"), full2[1357:2870,])

allrefs <- tibble(full = full3$`Full Reference`, clean = clean2$Reference) %>% na.omit()

updatedTb <- allrefs %>% 
  mutate(year = str_match(allrefs$clean, "(\\d{4})([az]?)")[,2],
         addTerm = str_match(allrefs$clean, "(\\d{4})([az]?)")[,3],
         firstWrdFull = str_match(allrefs$full, "^\\s?([A-Za-z]*)")[,2],
         firstWrdClean = str_match(allrefs$clean, "^\\s?([A-Za-z]*)")[,2])
         
title_pattern <- "\\. ?" %R%
  capture(one_or_more(WRD) %R% 
  optional(or(":", "-")) %R% 
  one_or_more(WRD) %R% 
  SPC %R%
  one_or_more(WRD) %R% 
  optional(or(":", "-")) %R% 
  one_or_more(WRD))

title_pattern2 <-   START %R% capture(one_or_more(WRD) %R% 
  optional(or(":", "-")) %R% 
  optional(one_or_more(WRD)) %R% 
  SPC %R%
  one_or_more(WRD) %R% 
  optional(or(":", "-")) %R% 
  optional(one_or_more(WRD))) %R%
  optional(".")


str_view(updatedTb[1:100,]$full,
         pattern = ifelse(str_detect(updatedTb[1:100,]$full, START %R% updatedTb[1:100,]$firstWrdClean),
                          title_pattern,
                          title_pattern2))

         