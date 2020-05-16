* proc format library=library cntlout = library.fmt_53;
proc format;
        Value $Stratum
            "CA" = "Appalachia Stratum"
            "HM" = "High Minority Areas"
            "LM" = "Low Minority Areas"
        ;
        Value $APP_REG
            "." = "Non-Appalachia"
            "C" = "Central Appalachia"
            "N" = "Northern Appalachia"
            "S" = "Southern Appalachia"
        ;
        Value DRA
           1 = "In the Mississippi Delta region"
           2 = "Not in the Mississippi Delta region"
        ;
        Value HIGHSPAN
            1 = "Yes"
            2 = "No"
        ;
       
        Value RUC2003F
            1 = "County in metro area with 1 million population or more"
            2 = "County in metro area of 250,000 to 1 million population"
            3 = "County in metro area of fewer than 250,000 population"
            4 = "Nonmetro county with urban population of 20,000 or more, adjacent to a metro area"
            5 = "Nonmetro county with urban population of 20,000 or more, not adjacent to a metro area"
            6 = "Nonmetro county with urban population of 2,500-19,999, adjacent to a metro area"
            7 = "Nonmetro county with urban population of 2,500-19,999, not adjacent to a metro area"
            8 = "Nonmetro county completely rural/<2,500 urban population/adjacent metro area"
            9 = "Nonmetro county completely rural/<2,500 urban population/not adjacent metro area"
        ;
        Value RUC2013F
            1 = "Metro - Counties in metro areas of 1 million population or more"
            2 = "Metro - Counties in metro areas of 250,000 to 1 million population"
            3 = "Metro - Counties in metro areas of fewer than 250,000 population"
            4 = "Nonmetro - Urban population of 20,000 or more, adjacent to a metro area"
            5 = "Nonmetro - Urban population of 20,000 or more, not adjacent to a metro area"
            6 = "Nonmetro - Urban population of 2,500 to 19,999, adjacent to a metro area"
            7 = "Nonmetro - Urban population of 2,500 to 19,999, not adjacent to a metro area"
            8 = "Nonmetro - Completely rural or less than 2,500 urban population, adjacent to a metro area"
            9 = "Nonmetro - Completely rural or less than 2,500 urban population, not adjacent to a metro area"
        ;
        Value PR_RUCA_
            1 = "Metropolitan area core: primary flow within an urbanized area (UA)"
            10 = "Rural areas: primary flow to a tract outside a UA or UC"
            2 = "Metropolitan area high commuting: primary flow 30% or more to a UA"
            3 = "Metropolitan area low commuting: primary flow 10% to 30% to a UA"
            4 = "Micropolitan area core: primary flow within an Urban Cluster of 10,000 to 49,999 (large UC)"
            5 = "Micropolitan high commuting: primary flow 30% or more to a large UC"
            6 = "Micropolitan low commuting: primary flow 10% to 30% to a large UC"
            7 = "Small town core: primary flow within an Urban Cluster of 2,500 to 9,999 (small UC)"
            8 = "Small town high commuting: primary flow 30% or more to a small UC"
            9 = "Small town low commuting: primary flow 10% to 30% to a small UC"
            99 = "Not coded: Census tract w/0 population & no rural-urban identifier info (at 2010 Census)"
        ;
        Value SEC_RUC
           1.0 = "Metro area core: primary flow w/in urbanized area (UA), No additional code"
           1.1 = "Metro area core: primary flow w/in an UA, Secondary flow 30%-%50=50 to larger UA"
           10.0 = "Rural areas: primary flow to a tract outside a UA or UC, No additional code"
           10.1 = "Rural areas: primary flow to tract outside a UA/UC, Secondary flow 30-50% to a UA"
           10.2 = "Rural areas: primary flow to tract outside a UA/UC, Secondary flow 30-50% to a large UC"
           10.3 = "Rural areas: primary flow to tract outside a UA/UC, Secondary flow 30-50% to a small UC"
           2.0 = "Metro area high commuting: primary flow 30%+ to a UA, No additional code"
           2.1 = "Metro area high commuting: primary flow 30%+ to a UA, Secondary flow 30-50% to larger UA"
           3.0 = "Metro area low commuting: primary flow 10-30% to a UA, No additional code"
           4.0 = "Micropol area: prim flow w/in Urban Cluster of 10,000-49,999, No additional code"
           4.1 = "Micropol area: prim flow w/in Urban Cluster of 10,000-49,999, Sec flow 30-50% to UA"
           5.0 = "Micropol high commuting: prim flow 30%+ to a large UC, No additional code"
           5.1 = "Micropol high commuting: prim flow 30%+ to large UC, Secondary flow 30-50% to UA"
           6.0 = "Micropol low commuting: primary flow 10%-30% to a large UC, No additional code"
           7.0 = "Small town: prim flow w/in Urban Cluster 2,500-9,999(small UC)/No additional code"
           7.1 = "Small town: prim flow w/in Urban Cluster 2,500-9,999(small UC)/Sec flow 30-50% to UA"
           7.2 = "Small town: prim flow w/in Urban Cluster 2,500-9,999(small UC)/Sec flow 30-50% to large UC"
           8.0 = "Small town/high commuting: prim flow 30%+ to small UC, No additional code"
           8.1 = "Small town/high commuting: prim flow 30%+ to small UC, Secondary flow 30-50% to UA"
           8.2 = "Small town/high commuting: prim flow 30%+ to small UC, Secondary flow 30-50% to large UC"
           9.0 = "Small town/low commuting: prim flow 10-30% to small UC, No additional code"
           99 = "Not coded: Census tract w/0 population & no rural-urban identifier info (at 2010 Census)"
        ;
        Value NCHSURCO
            1 = "Metropolitan: large metro"
            2 = "Metropolitan: large fringe metro"
            3 = "Metropolitan: medium metro"
            4 = "Metropolitan: small metro"
            5 = "Non-metropolitan: micropolitan"
            6 = "Non-metropolitan: noncore"
        ;
        Value CENSDIV
            01 = "New England"
            02 = "Middle Atlantic"
            03 = "East North Central"
            04 = "West North Central"
            05 = "South Atlantic"
            06 = "East South Central"
            07 = "West South Central"
            08 = "Mountain"
            09 = "Pacific"
        ;
        Value CENSREG
            01 = "Northeast"
            02 = "Midwest"
            03 = "South"
            04 = "West"
        ;
        Value $VAR_STR
            "5_3_HM" = "HINTS Cycle 5_3 high minority stratum"
            "5_3_LM" = "HINTS Cycle 5_3 low minority stratum"
            "5_3_W1_HM" = "HINTS Cycle 5_3 web pilot (no incentive) high minority stratum"
            "5_3_W1_LM" = "HINTS Cycle 5_3 web pilot (no incentive) low minority stratum"
            "5_3_W2_HM" = "HINTS Cycle 5_3 web pilot (with incentive) high minority stratum"
            "5_3_W2_LM" = "HINTS Cycle 5_3 web pilot (with incentive) low minority stratum"
        ;
        Value FormType
            2 = "Completed by paper"
            5 = "Completed on Web"
        ;
        Value Language
            1 = "English"
            2 = "Spanish"
        ;
        Value QDisp
            1 = "Complete"
            2 = "Partial complete"
        ;
        Value $UpdateD
            "DDMMMYYYY" = "Update date"
        ;
        Value AdultsI
          -9 = "Missing data"
          1 = "Yes"
          2 = "No"
        ;
        Value MailHHA
            -1 = "Inapplicable, coded 2 in AdultsInHH"
            -2 = "Question answered in error (Commission Error)"
            -4 = "Unreadable or Non-conforming numeric response"
            -6 = "Missing data (Filter Missing)"
            -7 = "Missing data (Web partial - Question Never Seen)"
            -9 = "Missing data (Not Ascertained)"
            01-74 = "Number of adults living in the household"
        ;
        Value SeekHea
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data"
           1 = "Yes"
           2 = "No"
        ;
        Value WhereSe
           -1 = "Inapplicable, coded 2 in SeekHealthInfo"
           -2 = "Question answered in error (Commission Error)"
           -5 = "Multiple responses selected in error"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Books"
           10 = "Newspapers"
           11 = "Telephone information number"
           12 = "Complementary, alternative, or unconventional practitioner"
           2 = "Brochures, pamphlets, etc."
           3 = "Cancer organization"
           4 = "Family"
           5 = "Friend/Co-worker"
           6 = "Doctor or health care provider"
           7 = "Internet"
           8 = "Library"
           9 = "Magazines"
        ;
        Value WhoLook
           -1 = "Inapplicable, coded 2 in SeekHealthInfo"
           -2 = "Question answered in error (Commission Error)"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Myself"
           2 = "Someone else"
           3 = "Both myself and someone else"
        ;
        Value LotOfEf
           -1 = "Inapplicable, coded 2 in SeekHealthInfo"
           -2 = "Question answered in error (Commission Error)"
           -5 = "Multiple responses selected in error"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Strongly agree"
           2 = "Somewhat agree"
           3 = "Somewhat disagree"
           4 = "Strongly disagree"
        ;
        Value Confide
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Completely confident"
           2 = "Very confident"
           3 = "Somewhat confident"
           4 = "A little confident"
           5 = "Not confident at all"
        ;
        Value TrustDo
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "A lot"
           2 = "Some"
           3 = "A little"
           4 = "Not At All"
        ;
        Value StrongF
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Books"
           10 = "Newspapers"
           11 = "Telephone information number"
           12 = "Complementary, alternative, or unconventional practitioner"
           2 = "Brochures, pamphlets, etc."
           3 = "Cancer organization"
           4 = "Family"
           5 = "Friend/Co-worker"
           6 = "Doctor or health care provider"
           7 = "Internet"
           8 = "Library"
           9 = "Magazines"
           91 = "Other - Specify:"
        ;
        Value $StrongN
            "-1" = "Inapplicable, coded -5 or 1 - 12 in StrongNeedHealthInfo"
            "-2" = "Question answered in error (Commission Error)"
            "-6" = "Missing data (Filter Missing)"
            "-7" = "Missing data (Web partial - Question Never Seen)"
            "-9" = "Missing data (Not Ascertained)"
            "A"-"Z" = "Verbatim response"
        ;
        Value StrongG
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Books"
           10 = "Newspapers"
           11 = "Telephone information number"
           12 = "Complementary, alternative, or unconventional practitioner"
           2 = "Brochures, pamphlets, etc."
           3 = "Cancer organization"
           4 = "Family"
           5 = "Friend/Co-worker"
           6 = "Doctor or health care provider"
           7 = "Internet"
           8 = "Library"
           9 = "Magazines"
           91 = "Other - Specify:"
        ;
        Value SeekCan
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Yes"
           2 = "No"
        ;
        Value UseInte
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Yes"
           2 = "No"
        ;
        Value Interne
           -1 = "Inapplicable, coded 2 in UseInternet"
           -2 = "Question answered in error (Commission Error)"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Yes"
           2 = "No"
        ;
        Value WhereUs
           -1 = "Inapplicable, coded 2 in UseInternet"
           -2 = "Question answered in error (Commission Error)"
           -5 = "Multiple responses selected in error"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Daily"
           2 = "Sometimes"
           3 = "Never"
           4 = "Not applicable"
        ;
        Value HaveDev
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           1 = "Selected"
           2 = "Not selected"
        ;
        Value HaveDeF
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           1 = "Selected"
           2 = "Not selected"
        ;
        Value HaveDeG
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           1 = "Tablet computer"
           2 = "Smartphone"
           3 = "Basic cell phone only"
           4 = "None"
           5 = "Multiple devices selected"
        ;
        Value TabletH
           -1 = "Inapplicable, coded 1 in HaveDevice_CellPh or coded 1 in HaveDevice_None"
           -2 = "Question answered in error (Commission Error)"
           -5 = "Multiple responses selected in error"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Yes"
           2 = "No"
           3 = "Don't know"
        ;
        Value Tablet_
           -1 = "Inapplicable, coded 1 in HaveDevice_CellPh or coded 1 in HaveDevice_None"
           -2 = "Question answered in error (Commission Error)"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Yes"
           2 = "No"
        ;
        Value Wearabl
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Yes"
           2 = "No"
        ;
        Value FreqWea
           -1 = "Inapplicable, coded 1 in WearableDevTrackHealth"
           -2 = "Question answered in error (Commission Error)"
           -5 = "Multiple responses selected in error"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Everyday"
           2 = "Almost everyday"
           3 = "1-2 times per week"
           4 = "Less than once per week"
           5 = "I did not use a wearable device in the past month"
        ;
        Value Willing
           -1 = "Inapplicable, coded 1 in WearableDevTrackHealth"
           -2 = "Question answered in error (Commission Error)"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Yes"
           2 = "No"
        ;
        Value SharedH
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Yes"
           2 = "No"
           3 = "Not Applicable"
        ;
        Value TextFro
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Yes"
           2 = "No"
           3 = "Don't know"
        ;
        Value FreqGoP
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           0 = "None"
           1 = "1 time"
           2 = "2 times"
           3 = "3 times"
           4 = "4 times"
           5 = "5-9 times"
           6 = "10 or more times"
        ;
        Value Quality
           -1 = "Inapplicable, coded 0 in FreqGoProvider"
           -2 = "Question answered in error (Commission Error)"
           -5 = "Multiple responses selected in error"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Excellent"
           2 = "Very good"
           3 = "Good"
           4 = "Fair"
           5 = "Poor"
        ;
        Value FreqGoU
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           0 = "I have not visited an urgent care, walk-in or retail clinic in the past 12 months"
           1 = "1 time"
           2 = "2-4 times"
           3 = "5-9 times"
           4 = "10 or more times"
        ;
        Value QualitF
           -1 = "Inapplicable, coded 0 in FreqGoUrgentCare"
           -2 = "Question answered in error (Commission Error)"
           -5 = "Multiple responses selected in error"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Excellent"
           2 = "Very good"
           3 = "Good"
           4 = "Fair"
           5 = "Poor"
        ;
        Value ChanceA
           -1 = "Inapplicable, coded 0 in both FreqGoProvider AND FreqGoUrgentCare"
           -2 = "Question answered in error (Commission Error)"
           -5 = "Multiple responses selected in error"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Always"
           2 = "Usually"
           3 = "Sometimes"
           4 = "Never"
        ;
        Value HealthF
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Yes"
           2 = "No"
        ;
        Value $HealthI
            "-1" = "Inapplicable, coded 2 in HealthIns_Other"
            "-2" = "Question answered in error (Commission Error)"
            "-6" = "Missing data (Filter Missing)"
            "-7" = "Missing data (Web partial - Question Never Seen)"
            "-9" = "Missing data (Not Ascertained)"
            "A"-"Z" = "Verbatim response"
        ;
        Value AccessO
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           0 = "None"
           1 = "1 to 2 times"
           2 = "3 to 5 times"
           3 = "6 to 9 times"
           4 = "10 or more times"
        ;
        Value NotAcce
           -1 = "Inapplicable, coded 1-4 in AccessOnlineRecord"
           -2 = "Question answered in error (Commission Error)"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Yes"
           2 = "No"
        ;
        Value Records
           -1 = "Inapplicable, coded 0 in AccessOnlineRecord"
           -2 = "Question answered in error (Commission Error)"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Yes"
           2 = "No"
        ;
        Value AccessU
           -1 = "Inapplicable, coded 0 in AccessOnlineRecord"
           -2 = "Question answered in error (Commission Error)"
           -5 = "Multiple responses selected in error"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Yes"
           2 = "No"
           3 = "Don't know"
        ;
        Value Underst
           -1 = "Inapplicable, coded 0 in AccessOnlineRecord"
           -2 = "Question answered in error (Commission Error)"
           -5 = "Multiple responses selected in error"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Very easy"
           2 = "Somewhat easy"
           3 = "Somewhat difficult"
           4 = "Very difficult"
        ;
        Value UsefulO
           -1 = "Inapplicable, coded 0 in AccessOnlineRecord"
           -2 = "Question answered in error (Commission Error)"
           -5 = "Multiple responses selected in error"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Very useful"
           2 = "Somewhat useful"
           3 = "Not very useful"
           4 = "Not at all useful"
           5 = "I do not use my online medical records to monitor my health"
        ;
        Value CaregiL
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data (Not Ascertained)"
           1 = "Selected"
           2 = "Not selected"
        ;
        Value CaregiM
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data (Not Ascertained)"
           1 = "Selected"
           2 = "Not selected"
        ;
        Value CaregiN
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Yes, a child/children"
           2 = "Yes, a spouse/partner"
           3 = "Yes, a parent/parents"
           5 = "Yes, a friend or other non-relative"
           6 = "No"
           7 = "Multiple caregiving relationships selected"
           8 = "Yes, another family member"
        ;
        Value CaregiF
           -1 = "Inapplicable, coded 1 Caregiving_No"
           -2 = "Question answered in error (Commission Error)"
           -5 = "Multiple responses selected in error"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data (Not Ascertained)"
           1 = "Yes"
           2 = "No"
        ;
        Value Caregiv
           -1 = "Inapplicable, coded 1 in Caregiving_No"
           -2 = "Question answered in error (Commission Error)"
           -4 = "Unreadable or Non-conforming numeric response"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data (Not Ascertained)"
           000-168 = "Hours per week spent providing care"
        ;
        Value CaregiH
           -1 = "Inapplicable, coded 1 Caregiving_No"
           -2 = "Question answered in error (Commission Error)"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data (Not Ascertained)"
           1 = "Selected"
           2 = "Not selected"
        ;
        Value CaregiI
           -1 = "Inapplicable, coded 1 Caregiving_No"
           -2 = "Question answered in error (Commission Error)"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data (Not Ascertained)"
           1 = "Selected"
           2 = "Not selected"
        ;
        Value $CaregiK
            "-1" = "Inapplicable, coded 1 Caregiving_No or coded 2 in Caregiving_Other"
            "-2" = "Question answered in error (Commission Error)"
            "-6" = "Missing data (Filter Missing)"
            "-7" = "Missing data (Web partial - Question Never Seen)"
            "-9" = "Missing data (Not Ascertained)"
            "A"-"Z" = "Verbatim response"
        ;
        Value CaregiJ
           -1 = "Inapplicable, coded 1 Caregiving_No"
           -2 = "Question answered in error (Commission Error)"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data (Not Ascertained)"
           1 = "Cancer"
           10 = "Multiple caregiving conditions selected"
           2 = "Alzheimer's, confusion, dementia, forgetfulness"
           3 = "Orthopedic/Musculoskeletal Issues"
           4 = "Mental health/Behavioral/Substance Abuse Issues"
           5 = "Chronic Conditions"
           6 = "Neurological/Developmental Issues"
           7 = "Acute Conditions"
           8 = "Aging/Aging related health issues"
           9 = "Not sure/Don't know"
           91 = "Other (Specify)"
        ;
        Value CaregiG
           -1 = "Inapplicable, coded 1 Caregiving_No"
           -2 = "Question answered in error (Commission Error)"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data (Not Ascertained)"
           0 = "None"
           1 = "1 to 2 times"
           2 = "3 to 5 times"
           3 = "6 to 9 times"
           4 = "10 or more times"
        ;
        Value General
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Excellent,"
           2 = "Very good,"
           3 = "Good,"
           4 = "Fair, or"
           5 = "Poor?"
        ;
        Value AvoidDo
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "True"
           2 = "Not true"
        ;
        Value HeightF
           -4 = "Unreadable or Non-conforming numeric response"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           4-7 = "Height, in feet"
        ;
        Value Height_
           -4 = "Unreadable or Non-conforming numeric response"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           00-11 = "Height, in inches"
        ;
        Value Weight
           -4 = "Unreadable or Non-conforming numeric response"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           040-500 = "Weight, in pounds"
        ;
        Value WeightP
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Overweight,"
           2 = "Slightly overweight,"
           3 = "Underweight,"
           4 = "Slightly underweight, or"
           5 = "Just about the right weight for you?"
        ;
        Value WeightI
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Lose weight,"
           2 = "Maintain your weight,"
           3 = "Gain weight, or"
           4 = "You haven't really paid attention to your weight?"
        ;
        Value LittleI
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Nearly every day"
           2 = "More than half the days"
           3 = "Several days"
           4 = "Not at all"
        ;
        Value ChangeT
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Strongly agree"
           2 = "Somewhat agree"
           3 = "Somewhat disagree"
           4 = "Strongly disagree"
        ;
        Value Fruit
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           0 = "None"
           1 = "1/2 cup or less"
           2 = "1/2 cup to 1 cup"
           3 = "1 to 2 cups"
           4 = "2 to 3 cups"
           5 = "3 to 4 cups"
           6 = "4 or more cups"
        ;
        Value Average
           -1 = "Inapplicable, coded 8 in AverageCaloriesPerDay_DK and correctly did not record a response"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           0000-9999 = "Number of calories"
        ;
        Value AveragF
           -1 = "Inapplicable, response recorded in AverageCaloriesPerDay and correctly did not check DK"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           8 = "Don't know"
        ;
        Value NoticeC
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Yes"
           2 = "No"
        ;
        Value UndersF
           -1 = "Inapplicable, coded 2 in NoticeCalorieInfoOnMenu"
           -2 = "Question answered in error (Commission Error)"
           -5 = "Multiple responses selected in error"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data (Not Ascertained)"
           1 = "Very easy"
           2 = "Somewhat easy"
           3 = "Somewhat difficult"
           4 = "Very difficult"
        ;
        Value Calorie
           -1 = "Inapplicable, coded 2 in NoticeCalorieInfoOnMenu"
           -2 = "Question answered in error (Commission Error)"
           -5 = "Multiple responses selected in error"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data (Not Ascertained)"
           1 = "Yes"
           2 = "No"
        ;
        Value DrinkDa
           -4 = "Unreadable or Non-conforming numeric response"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           0 = "None"
           1-7 = "Days per week"
        ;
        Value DrinksP
           -1 = "Inapplicable, coded 0 in DrinkDaysPerWeek"
           -2 = "Question answered in error (Commission Error)"
           -4 = "Unreadable or Non-conforming numeric response"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           00-99 = "Drinks per day"
        ;
        Value Alcohol
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Yes"
           2 = "No"
           3 = "Don't Know"
        ;
        Value HCPAlco
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "A lot"
           2 = "Some"
           3 = "A little"
           4 = "Nothing"
           5 = "I have not seen a doctor or health professional in the past 12 months"
        ;
        Value TimesMo
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           0 = "None"
           1 = "1 day per week"
           2 = "2 days per week"
           3 = "3 days per week"
           4 = "4 days per week"
           5 = "5 days per week"
           6 = "6 days per week"
           7 = "7 days per week"
        ;
        Value HowLong
           -1 = "Inapplicable, coded 0 in TimesModerateExercise"
           -2 = "Question answered in error (Commission Error)"
           -4 = "Unreadable or Non-conforming numeric response"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           000-999 = "Minutes of exercise time per day"
        ;
        Value TimesSt
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           0 = "None"
           1 = "1 day per week"
           2 = "2 days per week"
           3 = "3 days per week"
           4 = "4 days per week"
           5 = "5 days per week"
           6 = "6 days per week"
           7 = "7 days per week"
        ;
        Value AveragH
           -4 = "Unreadable or Non-conforming numeric response"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           00-24 = "Hours per day"
        ;
        Value EnjoyEx
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "A lot"
           2 = "Some"
           3 = "A little"
           4 = "Not at all"
        ;
        Value ExRec_C
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           1 = "Increased physical activity/exercise"
           2 = "Decreased physical activity/exercise"
           3 = "Changed type of physical activity"
           4 = "Looked for more information about recommendations"
           5 = "Did not make a change"
           6 = "Had not heard of any government recommendations for physical activity"
           7 = "Multiple response choices selected"
        ;
        Value PhysAct
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           1 = "Yes"
           2 = "No"
           3 = "Don't Know"
        ;
        Value AveragG
           -4 = "Unreadable or Non-conforming numeric response"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data (Not Ascertained)"
           0-24 = "Hours of sleep per night"
        ;
        Value AveragI
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           1 = "Very good"
           2 = "Fairly good"
           3 = "Fairly bad"
           4 = "Very bad"
        ;
        Value Morning
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           1 = "I'm definitely a morning-person"
           2 = "I'm more of a morning-person than a night-person"
           3 = "I'm neither a morning-person nor a night-person"
           4 = "I'm more of a night-person than a morning-person"
           5 = "I'm definitely a night-person"
        ;
        Value SpendTi
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Often"
           2 = "Sometimes"
           3 = "Rarely"
           4 = "Never"
           5 = "Don't go out on sunny days"
        ;
        Value TimesSu
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           000 = "No sunburns in last 12 months"
           001-999 = "Times"
        ;
        Value Sunburn
           -1 = "Inapplicable, coded 0 in TimesSunburned"
           -2 = "Question answered in error (Commission Error)"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data (Not Ascertained)"
           1 = "Selected"
           2 = "Not selected"
        ;
        Value SunburH
           -1 = "Inapplicable, coded 0 in TimesSunburned"
           -2 = "Question answered in error (Commission Error)"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           1 = "Working at your job"
           10 = "Multiple response choices selected"
           91 = "Other"
           2 = "Working outside at your home or a relative/friend's home"
           3 = "Sunbathing"
           4 = "Swimming"
           5 = "Exercise (running/hiking/sports - not swimming)"
           6 = "Watching a sporting event"
           7 = "Attending an outdoor event or venue (concert/zoo/fair, etc...)"
           8 = "Day-to-day activities"
           9 = "Don't know"
        ;
        Value SunburG
           -1 = "Inapplicable, coded 0 in TimesSunburned"
           -2 = "Question answered in error (Commission Error)"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           1 = "Wearing sunscreen with SPF of at least 15"
           2 = "Wearing protective clothing such as long pants or a shirt with sleeves"
           3 = "Staying in the shade or under an umbrella"
           4 = "None of the above"
           5 = "I don't know/I don't remember"
           6 = "Multiple response choices selected"
        ;
        Value SunburF
           -1 = "Inapplicable, coded 0 in TimesSunburned"
           -2 = "Question answered in error (Commission Error)"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data (Not Ascertained)"
           1 = "Yes"
           2 = "No"
        ;
        Value Smoke1F
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Yes"
           2 = "No"
        ;
        Value SmokeNo
           -1 = "Inapplicable, coded 2 in Smoke100"
           -2 = "Question answered in error (Commission Error)"
           -5 = "Multiple responses selected in error"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Everyday"
           2 = "Some days"
           3 = "Not at all"
        ;
        Value TriedQu
           -1 = "Inapplicable, coded 2 in Smoke100 or coded 3 in SmokeNow"
           -2 = "Question answered in error (Commission Error)"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Yes"
           2 = "No"
        ;
        Value ElectCi
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Much less harmful"
           2 = "Less harmful"
           3 = "Just as harmful"
           4 = "More harmful"
           5 = "Much more harmful, or"
           7 = "I don't know"
        ;
        Value UsedECi
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Yes"
           2 = "No"
        ;
        Value UseECig
           -1 = "Inapplicable, coded 2 in UsedEcCigEver"
           -2 = "Question answered in error (Commission Error)"
           -5 = "Multiple responses selected in error"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Every day"
           2 = "Some days"
           3 = "Not at all"
        ;
        Value SmokeDa
           -1 = "Inapplicable, coded 2 in UsedEcCigEver"
           -2 = "Question answered in error (Commission Error)"
           -5 = "Multiple responses selected in error"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           0 = "0 days"
           1 = "1 or 2 days"
           2 = "3 to 5 days"
           3 = "6 to 9 days"
           4 = "10 to 19 days"
           5 = "20 to 29 days"
           6 = "All 30 days"
        ;
        Value Nicotin
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Strongly agree"
           2 = "Agree"
           3 = "Disagree"
           4 = "Strongly disagree"
           5 = "Don't know"
        ;
        Value LowNicF
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data (Not Ascertained)"
           1 = "Much more harmful to your health than a typical cigarette?"
           2 = "Slightly more harmful to your health than a typical cigarette?"
           3 = "Equally harmful to your health as a typical cigarette?"
           4 = "Slightly less harmful to your health than a typical cigarette?"
           5 = "Much less harmful to your health than a typical cigarette?"
        ;
        Value LowNico
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data (Not Ascertained)"
           1 = "Much more addictive than a typical cigarette?"
           2 = "Slightly more addictive than a typical cigarette?"
           3 = "Equally addictive as a typical cigarette?"
           4 = "Slightly less addictive than a typical cigarette?"
           5 = "Much less addictive than a typical cigarette?"
        ;
        Value SeenFed
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Yes"
           2 = "No"
        ;
        Value Tobacco
           -1 = "Inapplicable, coded 2 in SeenFederalCourtTobaccoMessages"
           -2 = "Question answered in error (Commission Error)"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           1 = "Selected"
           2 = "Not selected"
        ;
        Value TobaccF
           -1 = "Inapplicable, coded 2 in SeenFederalCourtTobaccoMessages"
           -2 = "Question answered in error (Commission Error)"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           1 = "Federal Court ordered tobacco messages: health effects of smoking"
           2 = "Federal Court ordered tobacco messages: health effects secondhand smoke"
           3 = "Federal Court ordered tobacco messages: addictiveness"
           4 = "Federal Court ordered tobacco messages: enhance delivery"
           5 = "Federal Court ordered tobacco messages: low tar and light cigarettes"
           6 = "Multiple Federal Court ordered tobacco messages selected"
        ;
        Value GenderC
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Male"
           2 = "Female"
        ;
        Value EverHad
           -1 = "Inapplicable, coded 2 in GenderC"
           -2 = "Question answered in error (Commission Error)"
           -5 = "Multiple responses selected in error"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Yes"
           2 = "No"
        ;
        Value WhenPap
           -1 = "Inapplicable, coded 1 in GenderC"
           -2 = "Question answered in error (Commission Error)"
           -5 = "Multiple responses selected in error"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "A year ago or less"
           2 = "More than 1, up to 2 years ago"
           3 = "More than 2, up to 3 years ago"
           4 = "More than 3, up to 5 years ago"
           5 = "More than 5 years ago"
           6 = "I have never had a Pap test"
        ;
        Value WhenMam
           -1 = "Inapplicable, coded 1 in GenderC"
           -2 = "Question answered in error (Commission Error)"
           -5 = "Multiple responses selected in error"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "A year ago or less"
           2 = "More than 1, up to 2 years ago"
           3 = "More than 2, up to 3 years ago"
           4 = "More than 3, up to 5 years ago"
           5 = "More than 5 years ago"
           6 = "I have never had a mammogram"
        ;
        Value HeardHP
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Yes"
           2 = "No"
        ;
        Value HPVCaus
           -1 = "Inapplicable, coded 2 in HeardHPV"
           -2 = "Question answered in error (Commission Error)"
           -5 = "Multiple responses selected in error"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Yes"
           2 = "No"
           3 = "Not sure"
        ;
        Value EverHaF
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Yes"
           2 = "No"
        ;
        Value CaBladd
           -1 = "Inapplicable, coded 2 in EverHadCancer"
           -2 = "Question answered in error (Commission Error)"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Selected"
           2 = "Not selected"
        ;
        Value CaOther
           -1 = "Inapplicable, coded 2 in EverHadCancer"
           -2 = "Question answered in error (Commission Error)"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Selected - Specify"
           2 = "Not selected"
        ;
        Value $CaOtheF
            "-1" = "Inapplicable, coded 2 in EverHadCancer or coded 2 in CaOther"
            "-2" = "Question answered in error (Commission Error)"
            "-6" = "Missing data (Filter Missing)"
            "-7" = "Missing data (Web partial - Question Never Seen)"
            "-9" = "Missing data (Not Ascertained)"
            "A"-"Z" = "Verbatim response"
        ;
        Value Cancer_
           -1 = "Inapplicable, coded 2 in EverHadCancer"
           -2 = "Question answered in error (Commission Error)"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Bladder cancer only"
           10 = "Leukemia only"
           11 = "Liver cancer only"
           12 = "Lung cancer only"
           13 = "Melanoma only"
           14 = "Non-Hodgkins only"
           15 = "Oral cancer only"
           16 = "Ovarian cancer only"
           17 = "Pancreatic cancer only"
           18 = "Pharyngeal cancer only"
           19 = "Prostate cancer only"
           2 = "Bone cancer only"
           20 = "Rectal cancer only"
           22 = "Skin cancer only"
           23 = "Stomach cancer only"
           25 = "More than one cancer checked"
           3 = "Breast cancer only"
           4 = "Cervical cancer only"
           5 = "Colon cancer only"
           6 = "Endometrial cancer only"
           7 = "Head/Neck cancer only"
           8 = "Hodgkins only"
           9 = "Renal cancer only"
           91 = "Other cancer only"
        ;
        Value WhenDia
           -1 = "Inapplicable, coded 2 in EverHadCancer"
           -2 = "Question answered in error (Commission Error)"
           -4 = "Unreadable or Non-conforming numeric response"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           000-101 = "Age at diagnosis"
        ;
        Value ChanceG
           -1 = "Inapplicable, coded 1 in EverHadCancer"
           -2 = "Question answered in error (Commission Error)"
           -5 = "Multiple responses selected in error"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Very unlikely"
           2 = "Unlikely"
           3 = "Neither unlikely nor likely"
           4 = "Likely"
           5 = "Very likely"
        ;
        Value FreqWor
           -1 = "Inapplicable, coded 1 in EverHadCancer"
           -2 = "Question answered in error (Commission Error)"
           -5 = "Multiple responses selected in error"
           -6 = "Missing data (Filter Missing)"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Not at all"
           2 = "Slightly"
           3 = "Somewhat"
           4 = "Moderately"
           5 = "Extremely"
        ;
        Value FamilyE
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Yes"
           2 = "No"
           4 = "Not sure"
        ;
        Value Influen
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "A lot"
           2 = "A little"
           3 = "Not at all"
           4 = "Don't know"
        ;
        Value Age
           -4 = "Unreadable or Non-conforming numeric response"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           18-101 = "Current age, in years"
        ;
        Value Marital
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Married"
           2 = "Living as married or living with a romantic partner"
           3 = "Divorced"
           4 = "Widowed"
           5 = "Separated"
           6 = "Single, never been married";

        Value Educati
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Less than 8 years"
           2 = "8 through 11 years"
           3 = "12 years or completed high school"
           4 = "Post high school training other than college (vocational or technical)"
           5 = "Some college"
           6 = "College graduate"
           7 = "Postgraduate"
        ;
        Value SpeakEn
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Very well"
           2 = "Well"
           3 = "Not well"
           4 = "Not at all"
        ;
        Value NotHisp
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Selected"
           2 = "Not selected"
        ;
        Value Hisp_Ca
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           10 = "Not Hispanic"
           21 = "Mexican"
           22 = "Puerto Rican"
           23 = "Cuban"
           24 = "Other Hispanic"
           25 = "Multiple Hispanic ethnicities selected"
        ;
        Value White
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           1 = "Selected"
           2 = "Not Selected"
        ;
        Value Race_Ca
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           11 = "White"
           12 = "Black"
           14 = "American Indian or Alaska Native"
           16 = "Multiple races selected"
           31 = "Asian Indian"
           32 = "Chinese"
           33 = "Filipino"
           34 = "Japanese"
           35 = "Korean"
           36 = "Vietnamese"
           37 = "Other Asian"
           51 = "Native Hawaiian"
           52 = "Guamanian or Chamorro"
           53 = "Samoan"
           54 = "Other Pacific Islander"
        ;
        Value SexualF
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data (Not Ascertained)"
           1 = "Heterosexual, or straight"
           2 = "Homosexual, or gay or lesbian"
           3 = "Bisexual"
           91 = "Something else - Specify"
        ;
        Value $SexualO
            "-1" = "Inapplicable, coded 1, 2, or 3 in SexualOrientation"
            "-2" = "Question answered in error (Commission Error)"
            "-6" = "Missing data (Filter Missing)"
            "-7" = "Missing data (Web partial - Question Never Seen)"
            "-9" = "Missing data (Not Ascertained)"
            "A"-"Z" = "Verbatim response"
        ;
        Value TotalHo
           -4 = "Unreadable or Non-conforming numeric response"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing data (Not Ascertained)"
           00-60 = "Number of people"
        ;
        Value R_HHAdul
            . = "Missing value"
            01-23 = "Number of adults"
        ;
        Value HHAdults
            01-23 = "Number of adults"
        ;
        Value SelfGen
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           1 = "Male"
           2 = "Female"
        ;
        Value SelfAge
           -4 = "Unreadable or Non-conforming numeric response"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           018-100 = "Age, in years"
        ;
        Value SelfMOB
           -4 = "Unreadable or Non-conforming numeric response"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           1 = "January"
           10 = "October"
           11 = "November"
           12 = "December"
           2 = "February"
           3 = "March"
           4 = "April"
           5 = "May"
           6 = "June"
           7 = "July"
           8 = "August"
           9 = "September"
        ;
        Value HHAdulM
           -1 = "Inapplicable, Fewer than 2 adults in HH"
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           1 = "Male"
           2 = "Female"
        ;
        Value HHAdulH
           -1 = "Inapplicable, Fewer than 2 adults in HH"
           -4 = "Unreadable or Non-conforming numeric response"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           018-101 = "Age, in years"
        ;
        Value HHAdulI
           -1 = "Inapplicable, Fewer than 2 adults in HH"
           -4 = "Unreadable or Non-conforming numeric response"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           1 = "January"
           10 = "October"
           11 = "November"
           12 = "December"
           2 = "February"
           3 = "March"
           4 = "April"
           5 = "May"
           6 = "June"
           7 = "July"
           8 = "August"
           9 = "September"
        ;
        Value HHAdulN
           -1 = "Inapplicable, Fewer than 3 adults in HH"
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           1 = "Male"
           2 = "Female"
        ;
        Value HHAdult
           -1 = "Inapplicable, Fewer than 3 adults in HH"
           -4 = "Unreadable or Non-conforming numeric response"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           018-100 = "Age, in years"
        ;
        Value HHAdulJ
           -1 = "Inapplicable, Fewer than 3 adults in HH"
           -4 = "Unreadable or Non-conforming numeric response"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           1 = "January"
           10 = "October"
           11 = "November"
           12 = "December"
           2 = "February"
           3 = "March"
           4 = "April"
           5 = "May"
           6 = "June"
           7 = "July"
           8 = "August"
           9 = "September"
        ;
        Value HHAdulO
           -1 = "Inapplicable, Fewer than 4 adults in HH"
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           1 = "Male"
           2 = "Female"
        ;
        Value HHAdulF
           -1 = "Inapplicable, Fewer than 4 adults in HH"
           -4 = "Unreadable or Non-conforming numeric response"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           018-100 = "Age, in years"
        ;
        Value HHAdulK
           -1 = "Inapplicable, Fewer than 4 adults in HH"
           -4 = "Unreadable or Non-conforming numeric response"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           1 = "January"
           10 = "October"
           11 = "November"
           12 = "December"
           2 = "February"
           3 = "March"
           4 = "April"
           5 = "May"
           6 = "June"
           7 = "July"
           8 = "August"
           9 = "September"
        ;
        Value HHAdulP
           -1 = "Inapplicable, Fewer than 5 adults in HH"
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           1 = "Male"
           2 = "Female"
        ;
        Value HHAdulG
           -1 = "Inapplicable, Fewer than 5 adults in HH"
           -4 = "Unreadable or Non-conforming numeric response"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           018-100 = "Age, in years"
        ;
        Value HHAdulL
           -1 = "Inapplicable, Fewer than 5 adults in HH"
           -4 = "Unreadable or Non-conforming numeric response"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           1 = "January"
           10 = "October"
           11 = "November"
           12 = "December"
           2 = "February"
           3 = "March"
           4 = "April"
           5 = "May"
           6 = "June"
           7 = "July"
           8 = "August"
           9 = "September"
        ;
        Value HHAdulA
            -1 = "Inapplicable, Fewer than 6 adults in HH or paper complete"
            -5 = "Multiple responses selected in error"
            -7 = "Missing data (Web partial - Question Never Seen)"
            -9 = "Missing Data"
            1 = "Male"
            2 = "Female"
        ;
        Value HHAdulB
            -1 = "Inapplicable, Fewer than 6 adults in HH or paper complete"
            -4 = "Unreadable or Non-conforming numeric response"
            -7 = "Missing data (Web partial - Question Never Seen)"
            -9 = "Missing Data"
            018-100 = "Age, in years"
        ;
        Value HHAdulC
            -1 = "Inapplicable, Fewer than 6 adults in HH or paper complete"
            -4 = "Unreadable or Non-conforming numeric response"
            -7 = "Missing data (Web partial - Question Never Seen)"
            -9 = "Missing Data"
            1 = "January"
            10 = "October"
            11 = "November"
            12 = "December"
            2 = "February"
            3 = "March"
            4 = "April"
            5 = "May"
            6 = "June"
            7 = "July"
            8 = "August"
            9 = "September"
        ;
        Value Childre
           -4 = "Unreadable or Non-conforming numeric response"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           00-10 = "Number of children under 18"
        ;
        Value RentOrO
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           1 = "Own"
           2 = "Rent"
           3 = "Occupied without paying monetary rent"
        ;
        Value IncomeR
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           1 = "$0 to $9,999"
           2 = "$10,000 to $14,999"
           3 = "$15,000 to $19,999"
           4 = "$20,000 to $34,999"
           5 = "$35,000 to $49,999"
           6 = "$50,000 to $74,999"
           7 = "$75,000 to $99,999"
           8 = "$100,000 to $199,999"
           9 = "$200,000 or more"
        ;
        Value IncomeF
           -5 = "Multiple responses selected in error"
           -7 = "Missing data (Web partial - Question Never Seen)"
           -9 = "Missing Data"
           1 = "Living comfortably on present income"
           2 = "Getting by on present income"
           3 = "Finding it difficult on present income"
           4 = "Finding it very difficult on present income"
        ;
        Value PromptF
            . = "Non-web case"
            1 = "Yes"
            2 = "No"
        ;
		value AgeGrpA 
			1 = '18-34'
			2 = '35-39'
			3 = '40-44'
			4 = '45+'
			-4 = 'Unreadable or Nonconforming Numeric Response'
			-9 = 'Missing Data (Not Ascertained)';

		value AgeGrpB 
			1 = '18-34'
			2 = '35-49'
			3 = '50-64'
			4 = '65-74'
			5 = '75+'
			-4 = 'Unreadable or Nonconforming Numeric Response'
			-9 = 'Missing Data (Not Ascertained)'				;

		value EducA 
			1 = 'Less than High School'
			2 = 'High School Graduate'
			3 = 'Some College'
			4 = 'College Graduate or More'
			-7 = "Missing data (Web partial - Question Never Seen)"
			-9 = 'Missing Data (Not Ascertained)'				;

		value EducB 
			1 = 'Less than High School'
			2 = 'High School Graduate'
			3 = 'Some College'
			4 = "Bachelor's Degree"
			5 = 'Post-Baccalaureate Degree'
			-7 = "Missing data (Web partial - Question Never Seen)"
			-9 = 'Missing Data (Not Ascertained)'				;

		value RaceEthn 
			1 = 'Hispanic'
			2 = 'Non-Hispanic White'
			3 = 'Non-Hispanic Black or African American'
			4 = 'Non-Hispanic American Indian or Alaska Native'
			5 = 'Non-Hispanic Asian'
			6 = 'Non-Hispanic Native Hawaiian or other Pacific Islander'
			7 = 'Non-Hispanic Multiple Races Mentioned'
			-7 = "Missing data (Web partial - Question Never Seen)"
			-9 = 'Missing Data (Not Ascertained)'					;

		value RaceEthn5f 
			1 = "Non-Hispanic White"
			2= "Non-Hispanic Black or African American"
			3="Hispanic"
			4="Non-Hispanic Asian"
			5="Non-Hispanic Other"
			-7 = "Missing data (Web partial - Question Never Seen)"
			-9="Missing Data--Not Ascertained"
			;

		value HHInc 
			1 = 'Less than $20,000'
			2 = '$20,000 to < $35,000'
			3 = '$35,000 to < $50,000'
			4 = '$50,000 to < $75,000'
			5 = '$75,000 or More'
			-7 = "Missing data (Web partial - Question Never Seen)"
			-9 = 'Missing Data (Not Ascertained)'				;

		value BMI 
			-4 = 'Unreadable or Nonconforming Numeric Response'
			-7 = "Missing data (Web partial - Question Never Seen)"
			-9 = 'Missing Data (Not Ascertained)'			;

		value AgeDX 
			-1 = 'Inapplicable, coded 2 in EverHadCancer'
			-2 = 'Question Answered in Error (Commission Error)'
			-4 = 'Unreadable or Non-conforming numeric response'
			-6 = 'Missing Data (Filter Missing)'
			-7 = "Missing data (Web partial - Question Never Seen)"
			-9 = 'Missing Data (Not Ascertained)'				;

		value TimeSinceDX 
			1 = 'Less than 1 Yr Since DX'
			2 = '2-5 Yrs Since DX'
			3 = '6-10 Yrs Since DX'
			4 = '11+ Yrs Since DX'
			-1 = 'Inapplicable, coded 2 in EverHadCancer'
			-4 = 'Unreadable or Nonconforming Numeric Response'
			-6 = 'Missing Data (Filter Missing), coded -9 in EverHadCancer'
			-7 = "Missing data (Web partial - Question Never Seen)"
			-9 = 'Missing Data (Not Ascertained)'					;

		value smokeStat 
			1 = 'Current'
			2 = 'Former'
			3 = 'Never'
			-6 = 'Missing Data (Filter Missing), coded -9 in Smoke100'
			-7 = "Missing data (Web partial - Question Never Seen)"
			-9 = 'Missing Data (Not Ascertained)'					;

		value phq4f
			-5 = 'Multiple Responses Selected in Error'
			-7 = "Missing data (Web partial - Question Never Seen)"
			-9 = 'Missing Data (Not Ascertained)'
			;

		value eCigUse 
			1 = 'Current'
			2 = 'Former'
			3 = 'Never'
			-4 = 'Unreadable or Nonconforming Numeric Response'
			-6 = 'Missing Data (Filter Missing), coded -9 in UsedECigEver'
			-7 = "Missing data (Web partial - Question Never Seen)"
			-9 = 'Missing Data (Not Ascertained)'
			;

		value AvgDrinksPerWeek
			-4 = 'Unreadable or Nonconforming Numeric Response'
			-5 = 'Multiple Responses Selected in Error'
			-7 = "Missing data (Web partial - Question Never Seen)"
			-9 = 'Missing Data (Not Ascertained)'
			;

		value WeeklyMinutesModerateExercise
		-4 = 'Unreadable or Nonconforming Numeric Response'
		-7 = "Missing data (Web partial - Question Never Seen)"
		-9 = 'Missing Data (Not Ascertained)'
		;

		value Treatment_H5C3f
		1='Paper-Only'
		2='Web Option'
		3='Web Bonus'
		;
run;
