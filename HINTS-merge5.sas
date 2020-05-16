*---------|---------|---------|---------|---------|---------|---------|---------;
/* Marge HINTS 5, Cycles 1, 2, 3
   May 2020 */

DM OUTPUT 'clear' continue;
DM LOG    'clear' continue;

libname library "R:\OCPH\CDP\1815\1815 Epidemiology\HINTS";
data hints5_cycle1;
  set library.hints5_cycle1_public;
run;
data hints5_cycle2;
  set library.hints5_cycle2_public;
run;
data hints5_cycle3;
  set library.hints5_cycle3_public;
run;

%include "R:\OCPH\CDP\1815\1815 Epidemiology\HINTS\HINTS 5 Cycle 1 Public Formats.sas";
%include "R:\OCPH\CDP\1815\1815 Epidemiology\HINTS\HINTS 5 Cycle 2 Public Formats.sas";
%include "R:\OCPH\CDP\1815\1815 Epidemiology\HINTS\HINTS 5 Cycle 3 Public Formats.sas";

%include "R:\OCPH\CDP\1815\1815 Epidemiology\HINTS\HINTS 5 Cycle 1 Public Format Assignments.sas";
%include "R:\OCPH\CDP\1815\1815 Epidemiology\HINTS\HINTS 5 Cycle 2 Public Format Assignments.sas";
%include "R:\OCPH\CDP\1815\1815 Epidemiology\HINTS\HINTS 5 Cycle 3 Public Format Assignments.sas";

proc format;
  value survey
    1 = "HINTS 5 Cycle 1"
    2 = "HINTS 5 Cycle 2"
	3 = "HINTS 5 Cycle 3"
    ;
run;

/* assuming no differences between groups */
data tempHINTS5CYCLE1;
  set hints5_cycle1;
  survey = 1;
  format survey survey.;
run;
data tempHINTS5CYCLE2 (drop = var_stratum);
  set hints5_cycle2;
  survey = 2;
  format survey survey.;
run;
data tempHINTS5CYCLE3 (drop = var_stratum);
  set hints5_cycle3;
  survey = 3;
  format survey survey.;
run;

data HINTS5_no_diff;
  set tempHINTS5CYCLE1 tempHINTS5CYCLE2 tempHINTS5CYCLE3;
  /*Create Replicate Weights for trend tests*/
  **Replicate Weights;
  array hints51wgts [50] person_finwt1-person_finwt50;
  array hints52wgts [50] person_finwt1-person_finwt50;
  array hints53wgts [50] TG_all_finwt1-TG_all_finwt50;
  array Merged_NWgt [150] Merged_NWGT1-Merged_NWGT150;
  **Adjust Final And Replicate Weights;
  if survey eq 1 
  then 
    do i = 1 to 50; *HINTS 5 Cycle 1;
      Merged_NWGT0 = person_finwt0;
      Merged_NWgt[i] = hints51wgts[i];
      Merged_NWgt[50+i] = person_finwt0;
      Merged_NWgt[100+i] = person_finwt0;
    end;
  else if survey eq 2 
       then 
         do i = 1 to 50; *HINTS 5 CYCLE 2;
	       Merged_NWGT0 = person_finwt0;
           Merged_NWgt[i] = person_finwt0;
           Merged_NWgt[50+i] = hints52wgts[i];
           Merged_NWgt[100+i] = person_finwt0;
		 end;
	   else if survey eq 3
            then 
              do i = 1 to 50; *HINTS 5 CYCLE 3;
	            Merged_NWGT0 = TG_all_finwt0;
                Merged_NWgt[i] = TG_all_finwt0;
                Merged_NWgt[50+i] = TG_all_finwt0;
                Merged_NWgt[100+i] = hints53wgts[i];
			  end;
run;

proc freq data = HINTS5_no_diff;
  table SeekCancerInfo GeneralHealth SeekHealthInfo ChanceAskQuestions / 
    nopercent nocum;
run;

data HINTS5_no_diff;
  set HINTS5_no_diff;
  if SeekCancerInfo < 0 then SeekCancerInfo = .;
  if GeneralHealth < 0 then GeneralHealth = .;
  if SeekHealthInfo < 0 then SeekHealthInfo = .;
  if ChanceAskQuestions < 0 then ChanceAskQuestions = .;
run;

title "No Group Differences";
proc surveyfreq data = HINTS5_no_diff varmethod = jackknife  missing;
  weight Merged_NWGT0;
  repweights Merged_NWGT1-Merged_NWGT150 / df = 98 jkcoefs = 0.98;
  tables survey * (SeekHealthInfo ChanceAskQuestions) / row;
run;

/* controlling for differences between groups */
proc format;
  value surveyx
    1 = "HINTS 5 Cycle 1"
    2 = "HINTS 5 Cycle 2"
	3 = "HINTS 5 Cycle 3, paper only"
    4 = "HINTS 5 Cycle 3, Web option"
    5 = "HINTS 5 Cycle 3, Web bonus"
    ;
run;

/*data tempHINTS5CYCLE3a (drop = var_stratum);*/
/*  set hints5_cycle3;*/
/*  if Treatment_H5C3 = 1*/
/*  then survey = 3;*/
/*  else if Treatment_H5C3 = 2*/
/*       then survey = 2;*/
/*       else if Treatment_H5C3 = 3*/
/*            then survey = 3;*/
/*  format survey surveyx.;*/
/*run;*/
/**/
/*proc freq data = tempHINTS5CYCLE3a;*/
/*  table survey /nocum nopercent;*/
/*run;*/

data HINTS5_with_diff;
  set tempHINTS5CYCLE1 tempHINTS5CYCLE2 tempHINTS5CYCLE3;
  /*Create Replicate Weights for trend tests*/
  **Replicate Weights;
  array hints51wgts [50] person_finwt1-person_finwt50;
  array hints52wgts [50] person_finwt1-person_finwt50;
  array hints53wgts [150] nwgt1-nwgt150;
  array Merged_NWgt [250] Merged_NWGT1-Merged_NWGT250;
  **Adjust Final And Replicate Weights;
  if survey eq 1 
  then do;
         do i = 1 to 50; *HINTS 5 Cycle 1;
           Merged_NWGT0 = person_finwt0;
           Merged_NWgt[i] = hints51wgts[i];
           Merged_NWgt[50+i] = person_finwt0;
	     end;
	     do i = 1 to 150; 
           Merged_NWgt[100+i] = person_finwt0;
         end;
       end;
  else if survey eq 2 
       then do;
              do i = 1 to 50; *HINTS 5 CYCLE 2;
	            Merged_NWGT0 = person_finwt0;
                Merged_NWgt[i] = person_finwt0;
                Merged_NWgt[50+i] = hints52wgts[i];
		      end;
              do i = 1 to 150;
                Merged_NWgt[100+i] = person_finwt0;
		      end;
		 end;
	   else if survey eq 3
            then do;
                   do i = 1 to 50; *HINTS 5 CYCLE 3;
	                 Merged_NWGT0 = nwgt0;
                     Merged_NWgt[i] = nwgt0;
                     Merged_NWgt[50+i] = nwgt0;
			       end;
			       do i = 1 to 150;
                     Merged_NWgt[100+i] = hints53wgts[i];
			       end;
			     end;
run;

/* RESUME HERE
Treatment_H5C3
*/

data HINTS5_with_diff;
  set HINTS5_with_diff;
  if survey = 3 and Treatment_H5C3 = 2
  then survey = 4;
  else if survey = 3 and Treatment_H5C3 = 3
       then survey = 5;
  format survey surveyx.;
run;

title "Controlling for Group Differences";
proc freq data = HINTS5_with_diff;
  table survey /nocum nopercent;
run;

proc surveyfreq data = HINTS5_with_diff varmethod = jackknife  missing;
  weight Merged_NWGT0;
  repweights Merged_NWGT1-Merged_NWGT250 / df = 98 jkcoefs = 0.98;
  tables survey * (SeekHealthInfo ChanceAskQuestions) / row;
run;
title;
