*---------|---------|---------|---------|---------|---------|---------|---------;
/* OVERVIEW OF THE HINTS 5 CYCLE 3 SURVEY AND DATA ANALYSIS RECOMMENDATIONS
January 2020 */

DM OUTPUT 'clear' continue;
DM LOG    'clear' continue;

libname library "R:\OCPH\CDP\1815\1815 Epidemiology\HINTS";
* libname library "F:\HINTS";
proc format cntlin = library.fmt_53;
run;

*%include "R:\OCPH\CDP\1815\1815 Epidemiology\HINTS\HINTS 5 Cycle 3 Public Format Assignments.sas";
/*data hints5_cycle3;*/
/*  set library.hints5_cycle3_public;*/
/*run;*/

/* page 8 */
data hints5_cycle3;
  set library.hints5_cycle3_public;
  *Set negative values to missing;
  if SeekCancerInfo < 0 then call missing(SeekCancerInfo);
  if GeneralHealth < 0 then call missing(GeneralHealth);
run;

proc freq data = hints5_cycle3;
  table HIGHSPANLI;
run;

title "Assessing for group differences with binary outcomes using SeekCancerInfo as example";
title2 "page 8";
proc surveylogistic data = hints5_cycle3 varmethod = jackknife;
  weight nwgt0;
  repweights nwgt1 - nwgt150 / df = 147 jkcoefs =0.98;
  class TREATMENT_H5C3;
  model SeekCancerInfo = TREATMENT_H5C3;
run;

title "Assessing for group differences with continuous data using GeneralHealth as example";
proc surveyreg data = hints5_cycle3 varmethod = jackknife;
  weight nwgt0;
  repweights nwgt1-nwgt150 / df = 147 jkcoefs = 0.98;
  class TREATMENT_H5C3;
  model GeneralHealth = TREATMENT_H5C3 / solution;
run;

/* page 14-15 */
proc format; *First create some temporary formats;
  value Genderf
    1 = "Male"
    2 = "Female";
  value Educationf
    1 = "Less than high school"
    2 = "12 years or completed high school"
    3 = "Some college"
    4 = "College graduate or higher";
  value seekcancerinfof
    1 = "Yes"
    0 = "No";
  value Generalf
    1 = "Excellent"
    2 = "Very good"
    3 = "Good"
    4 = "Fair"
    5 = "Poor";
  value survey
    1 = "HINTS 5 CYCLE 2"
    2 = "HINTS 5 CYCLE 3";
run;

data hints5_cycle3;
  set hints5_cycle3;
  /*Recode negative values to missing*/
  if genderc = 1 then gender = 1;
  if genderc = 2 then gender = 2;
  if genderc in (-9, -7) then gender = .;
  /*Recode education into four levels, and negative values to missing*/
  if education in (1, 2) then edu = 1;
  if education = 3 then edu = 2;
  if education in (4, 5) then edu = 3;
  if education in (6, 7) then edu = 4;
  if education in (-9, -7) then edu = .;
  /*Recode seekcancerinfo to 0- 1 format for proc rlogist procedure, and negative values to missing */
  if seekcancerinfo = 2 then seekcancerinfo = 0;
  if seekcancerinfo in (-9, -6, -2, -1) then seekcancerinfo = .;
  /*Recode negative values to missing for proc regress procedure*/
  if generalhealth in (-5, -9, -7) then generalhealth = .;
  /*Apply formats to recoded variables */
  format gender genderf. edu educationf. seekcancerinfo seekcancerinfof. 
    generalhealth generalf.;
run;

title "Frequency table and chi-square test";
title2 "page 15";
proc surveyfreq data = hints5_cycle3 varmethod = jackknife;
  weight TG_all_FINWT0;
  repweights TG_all_FINWT1 - TG_all_FINWT50 / df = 49 jkcoefs = 0.98;
  tables edu * gender / cl row col wchisq;
run;

/* page 17 */
title "Multivariable logistic regression of gender and education on SeekCancerInfo";
title2 "page 17";
proc surveylogistic data = hints5_cycle3 varmethod = jackknife;
  weight TG_all_FINWT0;
  repweights TG_all_FINWT1 - TG_all_FINWT50 / df = 49 jkcoefs = 0.98;
  class edu (ref = "Less than high school")
    gender (ref = "Male") / param=REF;
  model seekcancerinfo (descending) = gender edu / tech = newton 
    xconv = 1e-8 CLPARM EXPB;
run;

title "Multivariable linear regression of gender and education on GeneralHealth";
title2 "page 18";
title3 "Using replicate weights";
proc surveyreg data = hints5_cycle3 varmethod = jackknife;
  weight TG_all_FINWT0;
  repweights TG_all_FINWT1 - TG_all_FINWT50 / df = 49 jkcoefs = 0.98;
  class edu (ref = "Less than high school") gender (ref = "Male");
  model generalhealth = edu gender / solution;
run;

title2 "pages 19-20";
title3 "Using replicate weights";
proc surveyfreq data = hints5_cycle3 varmethod = TAYLOR;
  strata VAR_STRATUM;
  cluster VAR_CLUSTER;
  weight TG_all_finwt0;
  tables edu * gender / row col wchisq;
run;

/* page 21 */
title "Multivariable logistic regression of gender and education on SeekCancerInfo";
title2 "page 21";
title3;
proc surveylogistic data = hints5_cycle3 varmethod = TAYLOR;
  strata VAR_STRATUM;
  cluster VAR_CLUSTER;
  weight TG_all_FINWT0;
  class edu (ref = "Less than high school")
  gender (ref = "Male") / param = REF;
  model seekcancerinfo (descending) = gender edu / tech = newton xconv = 1e-8 
  CLPARM EXPB;
run;

title "Multivariable linear regression of gender and education on GeneralHealth";
title2 "page 23";
proc surveyreg data = hints5_cycle3 varmethod = TAYLOR;
  strata VAR_STRATUM;
  cluster VAR_CLUSTER;
  weight TG_all_FINWT0;
  class edu (ref = "Less than high school") gender (ref = "Male");
  model generalhealth = edu gender/solution;
run;

title "Merging HINTS survey iterations";
title2 "page 53";
/*CREATE TWO SEPARATE TEMPORARY DATA FILES THAT CONTAIN THE NEW ‘SURVEY’ 
  VARIABLE. */
data tempHINTS5CYCLE2;
  length tmpstrat $10;
  /*PUT NAME OF LIBRARY AND NAME OF EXISTING HINTS 5 CYCLE 3 DATA FILE*/
  set library.Hints5_cycle2_public;
  survey = 1;
  format survey survey.;
  *Recreate VAR_STRATUM variable with length of 10 to match H5C3;
  tmpstrat = var_stratum;
  drop var_stratum;
  rename tmpstrat = var_stratum;
run;

data tempHINTS5CYCLE3;
  set library.Hints5_cycle3_public;
  survey = 2;
  format survey survey.;
run;

/* THIS CODE MERGES THE TWO TEMPORARY DATA SETS CREATED ABOVE. IT ALSO CREATES
   ONE FINAL SAMPLE WEIGHT (Merged_NWGT0) AND 100 REPLICATE WEIGHTS 
   (Merged_NWGT1 THRU Merged_NWGT100) */
data mergeHINTS5C2_HINTS5C3;
  set tempHINTS5CYCLE2 tempHINTS5CYCLE3;
  /*Create Replicate Weights for trend tests*/
  **Replicate Weights;
  array hints52wgts [50] person_finwt1-person_finwt50;
  array hints53wgts [50] TG_all_finwt1-TG_all_finwt50;
  array Merged_NWgt [100] Merged_NWGT1-Merged_NWGT100;
  **Adjust Final And Replicate Weights;
  if survey eq 1 
  then do i=1 to 50; *HINTS 5 CYCLE 2;
         Merged_NWGT0=person_finwt0;
         Merged_NWgt[i]=hints52wgts[i];
         Merged_NWgt[50+i]=person_finwt0;
       end;
  else if survey eq 2 
       then do i=1 to 50; *HINTS 5 CYCLE 3;
              Merged_NWGT0 = TG_all_finwt0;
              Merged_NWgt[i] = TG_all_finwt0;
              Merged_NWgt[50+i] = hints53wgts[i];
            end;
run;

title "Running frequencies on two common variables";
title2 "page 54";
proc freq data = mergeHINTS5C2_HINTS5C3;
  tables seekhealthinfo chanceaskquestions / nocum nopercent;
run;

proc surveyfreq data = mergeHINTS5C2_HINTS5C3 varmethod = jackknife;
  weight Merged_NWGT0;
  repweights Merged_NWGT1-Merged_NWGT100 / df = 98 jkcoefs = 0.98;
  tables seekhealthinfo chanceaskquestions;
run;

title "Controlling for group differences";
title2 "page 66";
/* Creation of sample weights for the combined sample, controlling for group differences 
   p. 66 */
data hints5_cycle3_public (drop = i);
  set library.hints5_cycle3_public;
  /*Create Replicate Weights for mode tests involving Treatment*/
  **Replicate weights;
  array paperWgts [50]TG1_FINWT1 - TG1_FINWT50;
  array webWgts [50]TG2_FINWT1 - TG2_FINWT50;
  array webbonusWgts [50]TG3_FINWT1 - TG3_FINWT50;
  array nwgt [150]nwgt1 - nwgt150;
  **Adjust Final and Replicate Weights;
  if Treatment_H5C3 eq 1 
  then do i = 1 to 50; **paper;
         nwgt0 = TG1_FINWT0;
         nwgt [i] = paperWgts [i];
         nwgt [50+i] = TG1_FINWT0;
         nwgt [100+i] = TG1_FINWT0;
       end;
  if Treatment_H5C3 eq 2 
  then do i = 1 to 50; **web;
         nwgt0 = TG2_FINWT0;
         nwgt [i] = TG2_FINWT0;
         nwgt [50+i] = webWgts [i];
         nwgt [100+i] = TG2_FINWT0;
       end;
  if Treatment_H5C3 eq 3 
  then do i = 1 to 50; **WebBonus;
         nwgt0 = TG3_FINWT0;
         nwgt [i] = TG3_FINWT0;
         nwgt [50+i] = TG3_FINWT0;
         nwgt [100+i] = webbonusWgts [i];
       end;
run;

/* Frequency table and chi-square test, p. 69 */
title2 "Frequency table and chi-square test controlling for group differences";
title3 "page 69";
proc surveyfreq data = hints5_cycle3 varmethod = jackknife;
  weight NWGT0;
  repweights NWGT1-NWGT150 / df=147 jkcoefs = 0.98;
  tables treatment_h5c3 * edu * gender / row col wchisq;
run;

/* Multivariable logistic regression of gender and education on SeekCancerInfo */
title2 "Logistic regression controlling for group differences";
title3 "page 69";
proc surveylogistic data = hints5_cycle3 varmethod = jackknife;
  weight NWGT0;
  repweights NWGT1 - NWGT150 / df = 147 jkcoefs = 0.98;
  class edu (ref = "Less than high school")
  gender (ref = "Male")
  treatment_h5c3 (ref = first) / param = REF;
  model seekcancerinfo (descending) = treatment_h5c3 gender edu / 
    tech = newton xconv = 1e-8 CLPARM EXPB;
run;

/* Multivariable linear regression of gender and education on GeneralHealth */
title2 "Linear regression controlling for group differences";
title3 "page 69";
proc surveyreg data = hints5_cycle3 varmethod = jackknife;
  weight NWGT0;
  repweights NWGT1 - NWGT150 / df = 147 jkcoefs = 0.98;
  class edu (ref = "Less than high school") gender (ref = "Male") treatment_h5c3 (ref = first);
  model generalhealth = treatment_h5c3 edu gender / solution;
run;

title;
title2;
title3;

