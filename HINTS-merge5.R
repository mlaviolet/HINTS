# merge HINTS 5, Cycles 1, 2, 3 into single table
# creating 150 replicate weights
# assuming no differences between the three modalities of HINTS 5 Cycle 3, so
#   using TG_all_FINWT0 through TG_all_FINWT50 from Cycle 3
# if differences found, would need 250 replicate weights: 50 each for Cycle 1
#   and Cycle 2, and 150 for the three modalities of Cycle 3; use 
#   nwgt0 through nwgt150 from Cycle 3

library(dplyr)
library(purrr)
library(haven)  
library(srvyr)

percent <- function(x, decimals = 4) round(100 * x, decimals)

# merging HINTS 5, Cycle 1, 2
# cycle 1
cycle1a <- read_sas(unz("HINTS-5_Cycle1_SAS.zip",
                        "HINTS-5_Cycle1_SAS/hints5_cycle1_public.sas7bdat")) 
# final sampling weight and replicate weights 1-50
cycle1b <- cycle1a %>% 
  select(PersonID, c(paste0("PERSON_FINWT", 0:50))) %>% 
  rename_at(paste0("PERSON_FINWT", 0:50), ~ paste0("Merged_NWGT", 0:50)) 
# replicate weights 51-100
# copy sampling weight 100 times as Merged_NWGT51 : Merged_NWGT100
cycle1c <- map(1:50, ~ select(cycle1b, PersonID, Merged_NWGT0)) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  set_names(c("PersonID", paste0("Merged_NWGT", 51:100)))  
# merge new sample and replicate weights 
cycle1 <- list(cycle1a, cycle1b, cycle1c) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  mutate(survey = 1) 
rm(cycle1b, cycle1c)

# cycle 2
cycle2a <- 
  read_sas(unz("HINTS_5_Cycle_2_SAS_03192020.zip",
               "HINTS 5- Cycle 2-SAS-03192020/hints5_cycle2_public.sas7bdat"))
# final sampling weight and replicate weights 51-100
cycle2b <- cycle2a %>% 
  select(PersonID, c(paste0("PERSON_FINWT", 0:50))) %>% 
  rename_at(paste0("PERSON_FINWT", 0:50), ~ 
              paste0("Merged_NWGT", c(0, 51:100))) 
# replicate weights 1-50 and 101-150
# copy sampling weight 100 times as Merged_NWGT1 : Merged_NWGT50 and
#   Merged_NWGT1 : Merged_NWGt10
cycle2c <- map(1:50, ~ select(cycle2b, PersonID, Merged_NWGT0)) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  set_names(c("PersonID", paste0("Merged_NWGT", c(1:50)))) 
# merge new sample and replicate weights 
cycle2 <- list(cycle2a, cycle2b, cycle2c) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  mutate(survey = 2) 
rm(cycle2b, cycle2c)

# concatenate cycle 1 and cycle 2
hints5_12 <- bind_rows(cycle1, cycle2) %>% 
  select(survey, PersonID, num_range("Merged_NWGT", 0:100), everything()) %>% 
  # variable to distinguish survey iterations
  mutate_at("survey", factor, 1:2, paste("HINTS 5 Cycle", 1:2)) %>% 
  as_survey_rep(weights = "Merged_NWGT0",
                repweights = paste0("Merged_NWGT", 1:100), 
                type = "JK1", scale = 49/50, mse = TRUE)

rm(cycle1, cycle2)  
# lapply(merge_12$variables[paste0("Merged_NWGT", 0:100)], summary)

hints5_12 %>% 
  mutate_at("SeekHealthInfo", factor, labels = c("NA", "Yes", "No")) %>% 
  group_by(survey, SeekHealthInfo) %>% 
  summarize(n = unweighted(n()),
            pct = survey_mean(na.rm = TRUE)) %>% 
  mutate_at(vars(starts_with("pct")), percent)

# looks good

# final sampling weight and replicate weights 1-50
# Cycle 1
cycle1b <- cycle1a %>% 
  select(PersonID, c(paste0("PERSON_FINWT", 0:50))) %>% 
  rename_at(paste0("PERSON_FINWT", 0:50), ~ paste0("Merged_NWGT", 0:50)) 
# replicate weights 51-150
# copy sampling weight 100 times as Merged_NWGT51 : Merged_NWGT150
cycle1c <- map(1:100, ~ select(cycle1b, PersonID, Merged_NWGT0)) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  set_names(c("PersonID", paste0("Merged_NWGT", 51:150)))  
cycle1 <- list(cycle1a, cycle1b, cycle1c) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  mutate(survey = 1) 
rm(cycle1a, cycle1b, cycle1c)

# Cycle 2
# replicate weights 51-150
cycle2b <- cycle2a %>% 
  select(PersonID, c(paste0("PERSON_FINWT", 0:50))) %>% 
  rename_at(paste0("PERSON_FINWT", 0:50), ~ 
              paste0("Merged_NWGT", c(0, 51:100))) 
#   Merged_NWGT101 : Merged_NWGT150
cycle2c <- map(1:100, ~ select(cycle2b, PersonID, Merged_NWGT0)) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  set_names(c("PersonID", paste0("Merged_NWGT", c(1:50, 101:150)))) 
cycle2 <- list(cycle2a, cycle2b, cycle2c) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  mutate(survey = 2) 
rm(cycle2a, cycle2b, cycle2c)

# cycle 3, assuming no differences between modality groups
cycle3a <- read_sas(unz("HINTS5_Cycle3_SAS_03112020.zip",
                         "hints5_cycle3_public.sas7bdat"))
# final sampling weight and replicate weights 101-150
cycle3b <- cycle3a %>% 
  select(PersonID, c(paste0("TG_all_FINWT", 0:50))) %>% 
  rename_at(paste0("TG_all_FINWT", 0:50), 
            ~ paste0("Merged_NWGT", c(0, 101:150))) 
# replicate weights 51-150
cycle3c <- map(1:100, ~ select(cycle3b, PersonID, Merged_NWGT0)) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  set_names(c("PersonID", paste0("Merged_NWGT", 1:100)))
# merge new sample and replicate weights 
cycle3_no_diff <- list(cycle3a, cycle3b, cycle3c) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  mutate(survey = 3) 
rm(cycle3a, cycle3b, cycle3c)

# concatenate cycles 1, 2, 3 into single table and create survey object
hints5_svy_no_diff <- bind_rows(cycle1, cycle2, cycle3_no_diff) %>% 
  # reorder columns to put new weights in front
  select(survey, PersonID, num_range("Merged_NWGT", 0:150), everything()) %>% 
  # variable to distinguish survey iterations
  mutate_at("survey", factor, 1:3, paste("HINTS 5 Cycle", 1:3)) %>% 
  as_survey_rep(weights = "Merged_NWGT0",
                repweights = paste0("Merged_NWGT", 1:150), 
                type = "JK1", scale = 49/50, mse = TRUE)
rm(cycle1, cycle2, cycle3_no_diff)

hints5_svy_no_diff %>% 
  mutate_at("SeekHealthInfo", factor, labels = c("NA", "Yes", "No")) %>% 
  group_by(survey, SeekHealthInfo) %>% 
  summarize(n = unweighted(n()),
            pct = survey_mean(na.rm = TRUE)) %>% 
  mutate_at(vars(starts_with("pct")), percent)


# cycle 3, controlling for group differences
cycle1a <- read_sas(unz("HINTS-5_Cycle1_SAS.zip",
                        "HINTS-5_Cycle1_SAS/hints5_cycle1_public.sas7bdat")) 
cycle2a <- 
  read_sas(unz("HINTS_5_Cycle_2_SAS_03192020.zip",
               "HINTS 5- Cycle 2-SAS-03192020/hints5_cycle2_public.sas7bdat"))
cycle3a <- read_sas(unz("HINTS5_Cycle3_SAS_03112020.zip",
                        "hints5_cycle3_public.sas7bdat"))

# Cycle 1
# replicate weights 1-50
cycle1b <- cycle1a %>% 
  select(PersonID, c(paste0("PERSON_FINWT", 0:50))) %>% 
  rename_at(paste0("PERSON_FINWT", 0:50), ~ paste0("Merged_NWGT", 0:50)) 
# replicate weights 51-250
# copy sampling weight 200 times as Merged_NWGT51 : Merged_NWGT250
cycle1c <- map(1:200, ~ select(cycle1b, PersonID, Merged_NWGT0)) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  set_names(c("PersonID", paste0("Merged_NWGT", 51:250)))  
cycle1 <- list(cycle1a, cycle1b, cycle1c) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  mutate(survey = 1) 
rm(cycle1a, cycle1b, cycle1c)

# Cycle 2
# replicate weights 51-100
cycle2b <- cycle2a %>% 
  select(PersonID, c(paste0("PERSON_FINWT", 0:50))) %>% 
  rename_at(paste0("PERSON_FINWT", 0:50), ~ 
              paste0("Merged_NWGT", c(0, 51:100))) 
# replicate weights 1-50, 51-250
cycle2c <- map(1:200, ~ select(cycle2b, PersonID, Merged_NWGT0)) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  set_names(c("PersonID", paste0("Merged_NWGT", c(1:50, 101:250)))) 
cycle2 <- list(cycle2a, cycle2b, cycle2c) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  mutate(survey = 2) 
rm(cycle2a, cycle2b, cycle2c)

# Cycle 3
# replicate weights 101-250
cycle3b <- cycle3a %>% 
  select(PersonID, c(paste0("nwgt", 0:150))) %>% 
  rename_at(paste0("nwgt", 0:150), 
            ~ paste0("Merged_NWGT", c(0, 101:250))) 
# replicate weights 1-100
cycle3c <- map(1:100, ~ select(cycle3b, PersonID, Merged_NWGT0)) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  set_names(c("PersonID", paste0("Merged_NWGT", 1:100)))

cycle3 <- list(cycle3a, cycle3b, cycle3c) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  mutate(survey = 3)
rm(cycle3a, cycle3b, cycle3c)

hints5_svy_with_diff <- bind_rows(cycle1, cycle2, cycle3) %>% 
  # reorder columns to put new weights in front
  select(survey, PersonID, num_range("Merged_NWGT", 0:250), everything()) %>% 
  # variable to distinguish survey iterations
  mutate_at("survey", factor, 1:3, paste("HINTS 5 Cycle", 1:3)) %>% 
  as_survey_rep(weights = "Merged_NWGT0",
                repweights = paste0("Merged_NWGT", 1:250), 
                type = "JK1", scale = 49/50, mse = TRUE)
rm(cycle1, cycle2, cycle3)

hints5_svy_with_diff %>% 
  mutate_at("SeekHealthInfo", factor, labels = c("NA", "Yes", "No")) %>% 
  group_by(survey, SeekHealthInfo) %>% 
  summarize(n = unweighted(n()),
            pct = survey_mean(na.rm = TRUE)) %>% 
  mutate_at(vars(starts_with("pct")), percent)
# OK TO HERE --------------------------------------------------------------

# 
# 
# # final sampling weight and replicate weights 101-250
# cycle3d <- cycle3a %>% 
#   select(PersonID, c(paste0("nwgt", 0:150))) %>% 
#   rename_at(paste0("nwgt", 0:150), 
#             ~ paste0("Merged_NWGT", c(0, 101:250))) 
# # replicate weights 1-100
# cycle3e <- map(1:100, ~ select(cycle3b, PersonID, Merged_NWGT0)) %>% 
#   reduce(inner_join, by = "PersonID") %>% 
#   set_names(c("PersonID", paste0("Merged_NWGT", 1:100)))
# # merge new sample and replicate weights 
# cycle3_with_diff <- list(cycle3a, cycle3d, cycle3e) %>% 
#   reduce(inner_join, by = "PersonID") %>% 
#   mutate(survey = 3) 
# 
# rm(cycle3a, cycle3b, cycle3c, cycle3d, cycle3e)
# 
# # concatenate all three cycles
# # no differences
# # error in building Cycle 2--missings need to be resolved
# hints5_svy_no_diff <- bind_rows(cycle1, cycle2, cycle3_no_diff) %>% 
#   # reorder columns to put new weights in front
#   select(survey, PersonID, num_range("Merged_NWGT", 0:150), everything()) %>% 
#   # variable to distinguish survey iterations
#   mutate_at("survey", factor, 1:3, paste("HINTS 5 Cycle", 1:3)) %>% 
#   as_survey_rep(weights = "Merged_NWGT0",
#                 repweights = , 
#                 type = "JK1", scale = 49/50, mse = TRUE)
# 
# # with differences
# hints5_svy_with_diff <- bind_rows(cycle1, cycle2, cycle3_with_diff) %>% 
#   # reorder columns to put new weights in front
#   select(survey, PersonID, num_range("Merged_NWGT", 0:250), everything()) %>% 
#   # variable to distinguish survey iterations
#   mutate_at("survey", factor, 1:3, paste("HINTS 5 Cycle", 1:3)) %>% 
#   as_survey_rep(weights = "Merged_NWGT0",
#                 repweights = paste0("Merged_NWGT", 1:250), 
#                 type = "JK1", scale = 49/50, mse = TRUE)
# 
# rm(cycle1, cycle2, cycle3_no_diff, cycle3_with_diff)
# 
# 
# 
# 
# hints5_svy_no_diff %>% 
#   mutate_at("SeekHealthInfo", factor, labels = c("NA", "Yes", "No")) %>% 
#   group_by(SeekHealthInfo) %>% 
#   summarize(n = unweighted(n()),
#             pct = survey_mean(na.rm = TRUE)) %>% 
#   mutate_at(vars(starts_with("pct")), percent)


