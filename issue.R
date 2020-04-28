# Issue: broom::tidy() ignores ddf argument
library(dplyr)
library(broom)
library(srvyr)
library(survey)
library(haven)
options(digits = 4)

# Reference
# "HINTS 5 Cycle 3 Survey Overview & Data Analysis Recommendations.pdf"
# contained in https://hints.cancer.gov/dataset/HINTS5_Cycle3_SAS_03112020.zip

download.file("https://hints.cancer.gov/dataset/HINTS5_Cycle3_SAS_03112020.zip",
              "HINTS5_Cycle3_SAS_03112020.zip")

edu_lbl <- c("Less than high school", "12 years or completed high school",
             "Some college", "College graduate or higher")
hints5_3_rep <- read_sas(unz("C:/Users/ML/Desktop/HINTS5_Cycle3_SAS_03112020.zip",
                             "hints5_cycle3_public.sas7bdat")) %>% 
  mutate(gender = factor(GenderC, 1:2, c("Male", "Female")),
         edu = case_when(Education %in% 1:2 ~ 1,
                         Education == 3     ~ 2,
                         Education %in% 4:5 ~ 3,
                         Education %in% 6:7 ~ 4,
                         TRUE ~ NA_real_)) %>% 
  mutate_at("edu", factor, 1:4, edu_lbl) %>% 
  mutate_at("SeekCancerInfo", factor, 2:1, c("No", "Yes")) %>% 
  as_survey_rep(weights = "TG_all_FINWT0",
                repweights = paste0("TG_all_FINWT", 1:50),
                type = "JK1", scale = 49/50, mse = TRUE)

model01 <- svyglm(SeekCancerInfo ~ gender + edu, hints5_3_rep, 
                  family = "quasibinomial")
model01$df.residual # 45, ddf for estimates should be adjusted to 49
exp(confint(model01, ddf = 49)) # these are the correct results
# see page 18 of recommendations document and survey::confint.svyglm

# results are same; ddf argument not passed to tidy()
tidy(model01, conf.int = TRUE, exponentiate = TRUE, ddf = 49)
tidy(model01, conf.int = TRUE, exponentiate = TRUE)

# Correct results                       Odds ratio   95% conf. limits
# ------------------------------------------------------------------
# genderFemale                              1.536       1.259  1.873
# edu12 years or completed high school      1.134       0.745  1.724
# eduSome college                           2.563       1.700  3.865
# eduCollege graduate or higher             3.275       2.151  4.986
