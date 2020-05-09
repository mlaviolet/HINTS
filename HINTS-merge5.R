# merge HINTS 5, Cycles 1, 2, 3 into single table

library(srvyr)
library(survey)
library(broom)
library(haven)  
library(tidyverse)

# cycle 1
cycle1a <- read_sas(unz("HINTS-5_Cycle1_SAS.zip",
                        "HINTS-5_Cycle1_SAS/hints5_cycle1_public.sas7bdat")) 
# final sampling weight and replicate weights 1-50
cycle1b <- cycle1a %>% 
  select(PersonID, c(paste0("PERSON_FINWT", 0:50))) %>% 
  rename_at(paste0("PERSON_FINWT", 0:50), ~ paste0("Merged_NWGT", 0:50)) 
# replicate weights 51-150
# copy sampling weight 100 times as Merged_NWGT51 : Merged_NWGT100
cycle1c <- map(1:100, ~ select(cycle1b, PersonID, Merged_NWGT0)) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  set_names(c("PersonID", paste0("Merged_NWGT", 51:150)))  
# merge new sample and replicate weights 
cycle1 <- list(cycle1a, cycle1b, cycle1c) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  mutate(survey = 1) 
rm(cycle1a, cycle1b, cycle1c)

# cycle 2
cycle2a <- 
  read_sas(unz("HINTS_5_Cycle_2_SAS_03192020.zip",
               "HINTS 5- Cycle 2-SAS-03192020/hints5_cycle2_public.sas7bdat"))
# final sampling weight and replicate weights 51-100
cycle2b <- cycle2a %>% 
  select(PersonID, c(paste0("PERSON_FINWT", 0:50))) %>% 
  rename_at(paste0("PERSON_FINWT", 0:50), ~ paste0("Merged_NWGT", c(0, 51:100))) 
# replicate weights 1-50 and 101-150
# copy sampling weight 100 times as Merged_NWGT1 : Merged_NWGT50 and
#   Merged_NWGT101 : Merged_NWGT150
cycle2c <- map(1:100, ~ select(cycle2b, PersonID, Merged_NWGT0)) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  set_names(c("PersonID", paste0("Merged_NWGT", c(1:50, 101:150)))) 
# merge new sample and replicate weights 
cycle2 <- list(cycle2a, cycle2b, cycle2c) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  mutate(survey = 2) 
rm(cycle2a, cycle2b, cycle2c)

# cycle 3
cycle3a <- read_sas(unz("HINTS5_Cycle3_SAS_03112020.zip",
                         "hints5_cycle3_public.sas7bdat"))
# final sampling weight and replicate weights 101-150
cycle3b <- cycle3a %>% 
  select(PersonID, c(paste0("TG_all_FINWT", 0:50))) %>% 
  rename_at(paste0("TG_all_FINWT", 0:50), 
            ~ paste0("Merged_NWGT", c(0, 101:150))) 
# replicate weights 1-100
cycle3c <- map(1:100, ~ select(cycle3b, PersonID, Merged_NWGT0)) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  set_names(c("PersonID", paste0("Merged_NWGT", 1:100)))
# merge new sample and replicate weights 
cycle3 <- list(cycle3a, cycle3b, cycle3c) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  mutate(survey = 3) 
rm(cycle3a, cycle3b, cycle3c)

# concatenate all three cycles
hints5_svy <- bind_rows(cycle1, cycle2, cycle3) %>% 
  # reorder columns to put new weights in front
  select(survey, PersonID, num_range("Merged_NWGT", 0:150), everything()) %>% 
  # variable to distinguish survey iterations
  mutate_at("survey", factor, 1:3, paste("HINTS 5 Cycle", 1:3)) %>% 
  as_survey_rep(weights = "Merged_NWGT0",
                repweights = paste0("Merged_NWGT", 1:150), 
                type = "JK1", scale = 49/50, mse = TRUE)
rm(cycle1, cycle2, cycle3)



