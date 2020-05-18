# merge HINTS 5, Cycles 1, 2, 3 into single table
#   both assuming no differences between survey modalities (groups) and
#   controlling for group differences
# also merging Cycles 1 and 2 as test since those are similar

# if differences found, would need 250 replicate weights: 50 each for Cycle 1
#   and Cycle 2, and 150 for the three modalities of Cycle 3; use 
#   nwgt0 through nwgt150 from Cycle 3

library(dplyr)
library(purrr)
library(haven)
library(srvyr)

percent <- function(x, decimals = 4) round(100 * x, decimals)

# merging HINTS 5, Cycle 1, 2 ---------------------------------------------
# building 100 replicate weights
# for Cycle 1, build three data tables cycle1a, cycle1b, cycle1c
# cycle1a is data as imported from SAS data file
# cycle1b creates replicate weights 1-50 by copying and renaming 
#   PERSON_FINWT1 : PERSON_FINWT50 as Merged_NWGT1 : Merged_NWGT50
#   sampling weight PERSON_FINWT0 copied as Merged_NWGT0
# cycle1c copies PERSON_FINWT0 as Merged_NWGT51 : Merged_NWGT100
# cycle1 joins all three tables
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

# for Cycle 2, build three data tables cycle2a, cycle2b, cycle2c
# cycle2a is data as imported from SAS data file
# cycle2b creates replicate weights 1-50 by copying and renaming 
#   PERSON_FINWT1 : PERSON_FINWT50 as Merged_NWGT51 : Merged_NWGT100
#   sampling weight PERSON_FINWT0 copied as Merged_NWGT0
# cycle1c copies PERSON_FINWT0 as Merged_NWGT1 : Merged_NWGT50
# join three tables to form cycle2
cycle2a <- 
  read_sas(unz("HINTS_5_Cycle_2_SAS_03192020.zip",
               "HINTS 5- Cycle 2-SAS-03192020/hints5_cycle2_public.sas7bdat"))
# final sampling weight and replicate weights 51-100
cycle2b <- cycle2a %>% 
  select(PersonID, c(paste0("PERSON_FINWT", 0:50))) %>% 
  rename_at(paste0("PERSON_FINWT", 0:50), ~ 
              paste0("Merged_NWGT", c(0, 51:100))) 
# replicate weights 1-50
# copy sampling weight 50 times as Merged_NWGT1 : Merged_NWGT50 
cycle2c <- map(1:50, ~ select(cycle2b, PersonID, Merged_NWGT0)) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  set_names(c("PersonID", paste0("Merged_NWGT", c(1:50)))) 
# merge new sample and replicate weights 
cycle2 <- list(cycle2a, cycle2b, cycle2c) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  mutate(survey = 2) 
rm(cycle2b, cycle2c)

# concatenate cycle 1 and cycle 2 and create survey object
hints5_12 <- bind_rows(cycle1, cycle2) %>% 
  select(survey, PersonID, num_range("Merged_NWGT", 0:100), everything()) %>% 
  # variable to distinguish survey iterations
  mutate_at("survey", factor, 1:2, paste("HINTS 5 Cycle", 1:2)) %>% 
  as_survey_rep(weights = "Merged_NWGT0",
                repweights = paste0("Merged_NWGT", 1:100), 
                type = "JK1", scale = 49/50, mse = TRUE)
rm(cycle1, cycle2)  

# check, looks good
hints5_12 %>% 
  mutate_at("SeekHealthInfo", factor, labels = c("NA", "Yes", "No")) %>% 
  group_by(survey, SeekHealthInfo) %>% 
  summarize(n = unweighted(n()),
            pct = survey_mean(na.rm = TRUE)) %>% 
  mutate_at(vars(starts_with("pct")), percent)

# Merge three cycles with no group differences ----------------------------
# assuming no differences between the three modalities of HINTS 5 Cycle 3, so
#   using TG_all_FINWT0 through TG_all_FINWT50 from Cycle 3
# creating 150 replicate weights
# Cycle 1
# final sampling weight and replicate weights 1-50
cycle1b <- cycle1a %>% 
  select(PersonID, c(paste0("PERSON_FINWT", 0:50))) %>% 
  rename_at(paste0("PERSON_FINWT", 0:50), ~ paste0("Merged_NWGT", 0:50)) 
# replicate weights 51-150
# copy sampling weight 100 times as Merged_NWGT51 : Merged_NWGT150
cycle1c <- map(1:100, ~ select(cycle1b, PersonID, Merged_NWGT0)) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  set_names(c("PersonID", paste0("Merged_NWGT", 51:150))) 
# merge three parts into one table and add survey identifier
cycle1 <- list(cycle1a, cycle1b, cycle1c) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  mutate(survey = 1) 
rm(cycle1b, cycle1c)

# Cycle 2
# sampling weights and replicate weights 51-100
cycle2b <- cycle2a %>% 
  select(PersonID, c(paste0("PERSON_FINWT", 0:50))) %>% 
  rename_at(paste0("PERSON_FINWT", 0:50), ~ 
              paste0("Merged_NWGT", c(0, 51:100))) 
# replicate weights 1:50, 101:!50
cycle2c <- map(1:100, ~ select(cycle2b, PersonID, Merged_NWGT0)) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  set_names(c("PersonID", paste0("Merged_NWGT", c(1:50, 101:150)))) 
# join into single table
cycle2 <- list(cycle2a, cycle2b, cycle2c) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  mutate(survey = 2) 
rm(cycle2b, cycle2c)

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
# join into single table 
cycle3_no_diff <- list(cycle3a, cycle3b, cycle3c) %>% 
  reduce(inner_join, by = "PersonID") %>% 
  mutate(survey = 3) 
rm(cycle3b, cycle3c)

# concatenate cycles 1, 2, 3 into single table and create survey object
hints5_svy_no_diff <- bind_rows(cycle1, cycle2, cycle3_no_diff) %>% 
  # reorder columns to put new weights in front
  select(survey, PersonID, num_range("Merged_NWGT", 0:150), everything()) %>% 
  # variable to distinguish survey iterations
  mutate_at("survey", factor, 1:3, paste("HINTS 5 Cycle", 1:3)) %>% 
  mutate_at("SeekHealthInfo", factor, labels = c("NA", "Yes", "No")) %>% 
  mutate_at("ChanceAskQuestions", factor,
            labels = c(paste0("NA", 1:5),
                       "Always", "Usually", "Sometimes", "Never")) %>%
  as_survey_rep(weights = "Merged_NWGT0",
                repweights = paste0("Merged_NWGT", 1:150), 
                type = "JK1", scale = 49/50, mse = TRUE)
rm(cycle1, cycle2, cycle3_no_diff)

# test on SeekHealthInfo (missings explicit)
hints5_svy_no_diff %>% 
  group_by(survey, SeekHealthInfo) %>% 
  summarize(n = unweighted(n()),
            pct = survey_mean(na.rm = TRUE)) %>% 
  mutate_at(vars(starts_with("pct")), percent)

# test with ChanceAskQuestions (missings explicit)
hints5_svy_no_diff %>%
  group_by(survey, ChanceAskQuestions) %>%
  summarize(n = unweighted(n()),
            pct = survey_mean(na.rm = TRUE)) %>%
  mutate_at(vars(starts_with("pct")), percent)

# Merge three cycles controlling for group differences --------------------
# Now need 250 replicate weights and to use nwgt1 : nwgt150

# Cycle 1
# sampling weight and replicate weights 1-50
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
# sampling weight and replicate weights 51-100
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
# sampling weight and replicate weights 101-250
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

treatment_lbl <- c("Paper only", "Web option", "Web bonus")
hints5_svy_with_diff <- bind_rows(cycle1, cycle2, cycle3) %>% 
  # add variable to distinguish surveys and groups
  # Cycles 1 and 2 were paper only; Cycle 3 was paper and online
  mutate(treatment = case_when(
    survey == 1 ~ 1,
    survey == 2 ~ 1,
    survey == 3 & Treatment_H5C3 == 1 ~ 1,
    survey == 3 & Treatment_H5C3 == 2 ~ 2,
    survey == 3 & Treatment_H5C3 == 3 ~ 3)) %>% 
  mutate_at("survey", factor, 1:3, paste("HINTS 5 Cycle", 1:3)) %>% 
  mutate_at("treatment", factor, 1:3, treatment_lbl) %>% 
  mutate_at("SeekHealthInfo", factor, labels = c("NA", "Yes", "No")) %>% 
  mutate_at("ChanceAskQuestions", factor,
            labels = c(paste0("NA", 1:5),
                       "Always", "Usually", "Sometimes", "Never")) %>%
  # reorder columns to put treatment and new weights in front
  select(survey, treatment, PersonID, num_range("Merged_NWGT", 0:250), 
         everything()) %>% 
  # create replicate weight survey object
  as_survey_rep(weights = "Merged_NWGT0",
                repweights = paste0("Merged_NWGT", 1:250), 
                type = "JK1", scale = 49/50, mse = TRUE)
rm(cycle1, cycle2, cycle3)

# test with SeekHealthInfo (missings explicit)
hints5_svy_with_diff %>% 
  group_by(survey, treatment, SeekHealthInfo) %>% 
  summarize(n = unweighted(n()),
            pct = survey_mean(na.rm = TRUE)) %>% 
  mutate_at(vars(starts_with("pct")), percent)

# test with ChanceAskQuestions (missings explicit)
hints5_svy_with_diff %>%
  group_by(survey, treatment, ChanceAskQuestions) %>%
  summarize(n = unweighted(n()),
            pct = survey_mean(na.rm = TRUE)) %>%
  mutate_at(vars(starts_with("pct")), percent)



# END ---------------------------------------------------------------------


