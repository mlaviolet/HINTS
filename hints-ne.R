# investigate indicators for New England

library(srvyr)
library(survey)
library(broom)
library(haven)  
library(tidyverse)

# import HINTS 5, Cycles 1, 2, 3
hints5_3 <- read_sas(unz("HINTS5_Cycle3_SAS_03112020.zip",
                         "hints5_cycle3_public.sas7bdat"))
hints5_2 <- 
  read_sas(unz("HINTS_5_Cycle_2_SAS_03192020.zip",
               "HINTS 5- Cycle 2-SAS-03192020/hints5_cycle2_public.sas7bdat"))
hints5_1 <- read_sas(unz("HINTS-5_Cycle1_SAS.zip",
                         "HINTS-5_Cycle1_SAS/hints5_cycle1_public.sas7bdat"))

division_lbl <- c("New England", "Middle Atlantic", "East North Central", 
                  "West North Central", "South Atlantic", "East South Central", 
                  "West South Central", "Mountain", "Pacific")

# Combine Cycles 2 and 3, may add Cycle 1 later
# Cycle 2, 100 replicate weights
# hints5_2, import Cycle 2 data
# cycle2b, new replicate weights 1 - 50: copy PERSON_FINWT1 - PERSON_FINWT50 as
#   Merged_NWGT1 - Merged_NWGT50
# cycle 2c, new replicate weights 51 - 100: copy PERSON_FINWT0 50 times as 
#   Merged_NWGT51 - Merged_NWGT100 
# temp_hints5_cycle2: join hints5_2, cycle2b, cycle2c by PersonID

cycle2b <- hints5_2 %>% 
  select(PersonID, c(paste0("PERSON_FINWT", 0:50))) %>% 
  mutate(Merged_NWGT0 = PERSON_FINWT0) %>% 
  select(-PERSON_FINWT0) %>% 
  rename_at(paste0("PERSON_FINWT", 1:50), ~ paste0("Merged_NWGT", 1:50))  
cycle2c <- map(1:50, ~ select(cycle2b, PersonID, Merged_NWGT0)) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  set_names(c("PersonID", paste0("Merged_NWGT", 51:100)))
temp_hints5_cycle2 <- list(hints5_2, cycle2b, cycle2c) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  mutate(survey = 1) 
# clean up
rm(hints5_2, cycle2b, cycle2c)

# Cycle 3, 100 replicate weights
# hints5_3, import Cycle 3 data
# cycle3b, new replicate weights 51 - 100: copy TG_all_FINWT1 - TG_all_FINWT50 as
#   Merged_NWGT51 - Merged_NWGT100
# cycle 3c, new replicate weights 1 - 50: copy TG_all_FINWT0 50 times as 
#   Merged_NWGT1 - Merged_NWGT50 
# temp_hints5_cycle3: join hints5_3, cycle3b, cycle3c by PersonID
hints5_3 <- read_sas(unz("HINTS5_Cycle3_SAS_03112020.zip",
                        "hints5_cycle3_public.sas7bdat"))
cycle3b <- hints5_3 %>% 
  select(PersonID, c(paste0("TG_all_FINWT", 0:50))) %>% 
  mutate(Merged_NWGT0 = TG_all_FINWT0) %>% 
  select(-TG_all_FINWT0) %>% 
  rename_at(paste0("TG_all_FINWT", 1:50), ~ paste0("Merged_NWGT", 51:100))
cycle3c <- map(1:50, ~ select(cycle3b, PersonID, Merged_NWGT0)) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  set_names(c("PersonID", paste0("Merged_NWGT", 1:50)))
temp_hints5_cycle3  <- list(hints5_3, cycle3b, cycle3c) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  mutate(survey = 2) 
rm(hints5_3, cycle3b, cycle3c)

# stack Cycle 2 and Cycle 3 into a single table and create survey object with
#   the new 100 replicate weights
# resulting table has 8,942 rows (3,504 for Cycle 2 and 5,438 for Cycle 3)
#   and 1,016 columns
#     914 variables appear in either or both of Cycle 2 and Cycle 3
#     102 added variables: cycle ID, final sample weight, 100 replicate
#     weights
# mergeHINTSS5C2_HINTS5C3 <- 
hints_svy <- bind_rows(temp_hints5_cycle2, temp_hints5_cycle3) %>% 
  # reorder columns to put new weights in front
  select(survey, PersonID, num_range("Merged_NWGT", 0:100), everything()) %>% 
  # variable to distinguish survey iterations
  mutate_at("survey", factor, levels = 1:2,
            labels = c("HINTS 5 Cycle 2", "HINTS 5 Cycle 3")) %>% 
  mutate_at("CENSDIV", factor, labels = division_lbl) %>% 
  # survey design object
  as_survey_rep(weights = "Merged_NWGT0",
                repweights = paste0("Merged_NWGT", 1:100), 
                type = "JK1", scale = 49/50, mse = TRUE)
rm(temp_hints5_cycle2, temp_hints5_cycle3)

hints_svy %>% 
  group_by(CENSDIV) %>% 
  summarize(n = unweighted(n()))



