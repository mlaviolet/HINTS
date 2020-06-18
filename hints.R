# HINTS = Health Information National Trends Survey, conducted by the National
#   Cancer Institute.
# This code replicates selected results from the document
#   "HINTS 5 Cycle 3 Survey Overview & Data Analysis Recommendations.pdf"
# Page references are to that document

# https://hints.cancer.gov/
# Data from HINTS 5, cycles 1, 2, and 3 are found in
# https://hints.cancer.gov/dataset/HINTS5_Cycle3_SAS_03112020.zip
# https://hints.cancer.gov/dataset/HINTS_5_Cycle_2_SAS_03192020.zip
# https://hints.cancer.gov/dataset/HINTS-5_Cycle1_SAS.zip

# Michael Laviolette, PhD MPH
# statman54@gmail.com

library(dplyr)
library(tidyr)
library(srvyr)
library(survey)
library(broom)
library(haven)  

# pp. 14-15
# Recoding variables and converting to factors
edu_lbl <- c("Less than high school",
             "12 years or completed high school",
             "Some college",
             "College graduate or higher")
health_lbl <- c("Excellent", "Very good", "Good", "Fair", "Poor")
group_lbl <-  c("Paper only", "Web option", "Web bonus")
percent <- function(x, decimals = 4) round(100 * x, decimals)

# Variable names in document do not always have correct case
# Refer to survey instrument for correct names
# import SAS data
hints5_3 <- read_sas(unz("HINTS5_Cycle3_SAS_03112020.zip",
                         "hints5_cycle3_public.sas7bdat")) %>% 
  mutate(gender = factor(GenderC, 1:2, c("Male", "Female")),
         # make negative values of GeneralHealth missing
         GeneralHealth = if_else(GeneralHealth < 0, NA_real_, GeneralHealth),
         # collapse education to four levels
         edu = case_when(Education %in% 1:2 ~ 1,
                         Education == 3     ~ 2,
                         Education %in% 4:5 ~ 3,
                         Education %in% 6:7 ~ 4,
                         TRUE ~ NA_real_)) %>% 
  mutate_at("edu", factor, 1:4, edu_lbl) %>% 
  mutate_at("Treatment_H5C3", factor, labels = group_lbl) %>% 
  mutate_at("Treatment_H5C3", relevel, "Web option") %>% # to match SAS
  # "No" must be the referent level to model probability of "Yes". By default
  #    the first level of a factor is the reference level
  mutate_at("SeekCancerInfo", factor, 2:1, c("No", "Yes"))  

# Assessing for group differences with binary outcomes, with SeekCancerInfo  
#   as example
# page 8
hints5_3_grp <- as_survey_rep(hints5_3, weights = "nwgt0",
                repweights = paste0("nwgt", 1:150), 
                type = "JK1", scale = 49/50, mse = TRUE)
degf(hints5_3_grp)
model00 <- svyglm(SeekCancerInfo ~ Treatment_H5C3, hints5_3_grp, 
                  family = "quasibinomial") 
summary(model00, df.resid = 147)
exp(coef(model00))
exp(confint(model00))

# pp. 15-16
# construct replicate weights survey object
hints5_3_rep <- hints5_3 %>% 
  as_survey_rep(weights = "TG_all_FINWT0",
                repweights = paste0("TG_all_FINWT", 1:50),
                type = "JK1", scale = 49/50, mse = TRUE)

# Frequency table and chi-square test, pp. 15-16
# THIS MATCHES SAS--don't need df argument since using design df
tbl_1 <- hints5_3_rep %>% 
  group_by(edu, gender) %>% 
  summarize(n = unweighted(n()),
            pct = survey_mean(na.rm = TRUE, vartype = c("se", "ci"))) %>% 
  drop_na() %>% 
  mutate_at(vars(starts_with("pct")), percent)

chisq_test <- svychisq(~ edu + gender, hints5_3_rep, statistic = "Wald")
chisq_test$statistic
chisq_test$parameter

chisq_test2 <- svychisq(~ edu + gender, hints5_3_rep, statistic = "adjWald")
chisq_test2$statistic
chisq_test2$parameter

# Multivariable logistic regression of gender and education on SeekCancerInfo
# p. 17
model01 <- svyglm(SeekCancerInfo ~ gender + edu, hints5_3_rep, 
                  family = "quasibinomial")
# odds ratios
or_ci <- exp(confint(model01, ddf = 49)) %>% as_tibble(rownames = "name")
# THIS MATCHES SAS
or <- exp(coef(model01)) %>% 
  enframe() %>% 
  inner_join(or_ci, by = "name") %>% 
  filter(name != "(Intercept)") %>% 
  setNames(c("Effect", "Odds ratio", "OR lower 95%", "OR upper 95%"))

# odds ratios
exp(coef(model01))
exp(confint(model01, ddf = 49))

# Need tidy >= 0.7.0
tidy(model01, conf.int = TRUE, exponentiate = TRUE, ddf = 49)

# p. 18-19
model01a <- svyglm(GeneralHealth ~ gender + edu, hints5_3_rep)
model01a %>% 
  tidy(conf.int = TRUE)
anova(model01a) # Rao-Scott LRT
# tests similar to SAS available in R?

# p. 19
# survey object for Taylor series linearization
hints5_3_lin <- hints5_3 %>% 
  as_survey_design(ids = VAR_CLUSTER, strata = VAR_STRATUM, 
                   weight = TG_all_FINWT0, nest = TRUE)
degf(hints5_3_lin) # ok

# Frequency table and chi-square test, p, 20
tbl_2 <- hints5_3_lin %>% 
  group_by(edu, gender) %>% 
  summarize(n = unweighted(n()),
            pct = survey_mean(na.rm = TRUE, vartype = c("se", "ci"))) %>% 
  drop_na() %>% 
  mutate_at(vars(starts_with("pct")), function(x) 100 * x)
# number of missings in one or more variables
nrow(hints5_3_lin) - sum(tbl_2$n)

svychisq(~ gender + edu, hints5_3_lin, 
         statistic = "Wald")[c("statistic", "parameter")]
svychisq(~ gender + edu, hints5_3_lin, 
         statistic = "adjWald")[c("statistic", "parameter")]

# Multivariable logistic regression of gender and education on 
#   SeekCancerInfo, p. 21-22
model01b <- svyglm(SeekCancerInfo ~ gender + edu, hints5_3_lin, 
       family = "quasibinomial")
model10b_or <- model01b %>% 
  tidy(conf.int = TRUE, exponentiate = TRUE)

# Multivariable linear regression of gender and education on GeneralHealth
# p. 23
model02 <- svyglm(GeneralHealth ~ gender + edu, hints5_3_lin) 
anova(model02)
# degf(hints5_3_rep) # AGREES WITH SAS

# Test and control for group differences
# p. 69

chk1 <- hints5_3_grp %>% 
  group_by(Treatment_H5C3, edu, gender) %>% 
  summarize(n = unweighted(n()),
            pct = survey_mean(na.rm = TRUE)) %>% 
  drop_na() %>% 
  mutate_at(vars(starts_with("pct")), function(x) 100 * x)

# Logistic regression    
# Multivariable logistic regression of gender and education on SeekCancerInfo
# proc surveylogistic data=hints5cycle3 varmethod=jackknife;
#   weight NWGT0;
#   repweights NWGT1 - NWGT150 / df = 147 jkcoefs = 0.98;
#   class edu (ref = "Less than high school")
#   gender (ref = "Male")
#   treatment_h5c3 (ref = first) / param = REF;
#   model seekcancerinfo (descending) = treatment_h5c3 gender edu  
  
model03 <- svyglm(SeekCancerInfo ~ Treatment_H5C3 + gender + edu, hints5_3_grp, 
                  family = "quasibinomial")
summary(model03)
regTermTest(model03, "Treatment_H5C3")
model03a <- svyglm(SeekCancerInfo ~ gender + edu, hints5_3_grp, 
                  family = "quasibinomial")
anova(model03, model03a, method = "Wald") # same results as regTermTest

# Linear Regression
# Multivariable linear regression of gender and education on GeneralHealth

model04 <- svyglm(GeneralHealth ~ Treatment_H5C3 + gender + edu, hints5_3_grp)
summary(model04)
regTermTest(model04, "Treatment_H5C3")


