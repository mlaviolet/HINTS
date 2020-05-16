* proc format library=INSERT LIBRARY NAME HERE;
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
            8 = "Nonmetro county completely rural/less than 2,500 urban population, adjacent to metro area"
            9 = "Nonmetro county completely rural/less than 2,500 urban population, not adjacent to metro area"
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
        ;
        Value SEC_RUC
			1  = "Metropolitan area core: primary flow within an urbanized area (UA), No additional code"
			1.1 = "Metropolitan area core: primary flow within an urbanized area (UA), Secondary flow 30% to 50% to a larger UA"
			2   = "Metropolitan area high commuting: primary flow 30% or more to a UA, No additional code"
			2.1 = "Metropolitan area high commuting: primary flow 30% or more to a UA, Secondary flow 30% to 50% to a larger UA"
			3   = "Metropolitan area low commuting: primary flow 10% to 30% to a UA, No additional code"
			4   = "Micropolitan area core: primary flow within an Urban Cluster of 10,000 to 49,999 (large UC), No additional code"
			4.1 = "Micropolitan area core: primary flow within an Urban Cluster of 10,000 to 49,999 (large UC), Secondary flow 30% to 50% to a UA"
			5   = "Micropolitan high commuting: primary flow 30% or more to a large UC, No additional code"
			5.1 = "Micropolitan high commuting: primary flow 30% or more to a large UC, Secondary flow 30% to 50% to a UA"
			6   = "Micropolitan low commuting: primary flow 10% to 30% to a large UC, No additional code"
			7   = "Small town core: primary flow within an Urban Cluster of 2,500 to 9,999 (small UC), No additional code"
			7.1 = "Small town core: primary flow within an Urban Cluster of 2,500 to 9,999 (small UC), Secondary flow 30% to 50% to a UA"
			7.2 = "Small town core: primary flow within an Urban Cluster of 2,500 to 9,999 (small UC), Secondary flow 30% to 50% to a large UC"
			8   = "Small town high commuting: primary flow 30% or more to a small UC, No additional code"
			8.1 = "Small town high commuting: primary flow 30% or more to a small UC, Secondary flow 30% to 50% to a UA"
			8.2 = "Small town high commuting: primary flow 30% or more to a small UC, Secondary flow 30% to 50% to a large UC"
			9   = "Small town low commuting: primary flow 10% to 30% to a small UC, No additional code"
			10  = "Rural areas: primary flow to a tract outside a UA or UC, No additional code"
			10.1 = "Rural areas: primary flow to a tract outside a UA or UC, Secondary flow 30% to 50% to a UA"
			10.2 = "Rural areas: primary flow to a tract outside a UA or UC, Secondary flow 30% to 50% to a large UC"
			10.3 = "Rural areas: primary flow to a tract outside a UA or UC, Secondary flow 30% to 50% to a small UC"
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
            "5_2_HM" = "HINTS Cycle 5_2 high minority stratum"
            "5_2_LM" = "HINTS Cycle 5_2 low minority stratum"
        ;
        Value FormType
            2 = "Long Form, NB"
            4 = "Short Form, NB"
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
            -9 = "Missing data (Not Ascertained)"
            00-75 = "Number of adults living in the household"
        ;
        Value SeekHea
            -9 = "Missing data"
            1 = "Yes"
            2 = "No"
        ;
        Value WhereSe
            -1 = "Inapplicable, coded 2 in SeekHealthInfo"
            -2 = "Question answered in error (Commission Error)"
            -5 = "Multiple responses selected in error"
            -6 = "Missing data (Filter Missing)"
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
            -9 = "Missing data (Not Ascertained)"
            1 = "Myself"
            2 = "Someone else"
            3 = "Both myself and someone else"
        ;
        Value SeekCan
            -1 = "Inapplicable, coded 2 in SeekHealthInfo"
            -2 = "Question answered in error (Commission Error)"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            1 = "Yes"
            2 = "No"
        ;
        Value CancerL
            -1 = "Inapplicable, coded 2 in SeekHealthInfo"
            -2 = "Question answered in error (Commission Error)"
            -5 = "Multiple responses selected in error"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            1 = "Strongly agree"
            2 = "Somewhat agree"
            3 = "Somewhat disagree"
            4 = "Strongly disagree"
        ;
        Value CancerC
            -5 = "Multiple responses selected in error"
            -9 = "Missing data (Not Ascertained)"
            1 = "Completely confident"
            2 = "Very confident"
            3 = "Somewhat confident"
            4 = "A little confident"
            5 = "Not confident at all"
        ;
        Value CancerT
            -5 = "Multiple responses selected in error"
            -9 = "Missing data (Not Ascertained)"
            1 = "A lot"
            2 = "Some"
            3 = "A little"
            4 = "Not At All"
        ;
        Value StrongF
            -5 = "Multiple responses selected in error"
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
            "-1" = "Inapplicable, coded -5 or 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 or 12 in StrongNeedCancerInfo"
            "-2" = "Question answered in error (Commission Error)"
            "-6" = "Missing data (Filter Missing)"
            "-9" = "Missing data (Not Ascertained)"
            "A"-"Z" = "Verbatim response"
        ;
        Value UseInte
            -9 = "Missing data (Not Ascertained)"
            1 = "Yes"
            2 = "No"
        ;
        Value Interne
            -1 = "Inapplicable, coded 2 in UseInternet"
            -2 = "Question answered in error (Commission Error)"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            1 = "Yes"
            2 = "No"
        ;
        Value InternF
            -1 = "Inapplicable, coded 2 in UseInternet"
            -2 = "Question answered in error (Commission Error)"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            1 = "Yes"
            2 = "No"
        ;
        Value WhereUs
            -1 = "Inapplicable, coded 2 in UseInternet"
            -2 = "Question answered in error (Commission Error)"
            -5 = "Multiple responses selected in error"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            1 = "Daily"
            2 = "Sometimes"
            3 = "Never"
            4 = "Not applicable"
        ;
        Value HaveDev
            -9 = "Missing Data"
            1 = "Selected"
            2 = "Not selected"
        ;
        Value HaveDeF
            -9 = "Missing Data"
            1 = "Selected"
            2 = "Not selected"
        ;
        Value HaveDeG
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
            -9 = "Missing data (Not Ascertained)"
            1 = "Yes"
            2 = "No"
            3 = "Don't know"
        ;
        Value Tablet_
            -1 = "Inapplicable, coded 1 in HaveDevice_CellPh or coded 1 in HaveDevice_None"
            -2 = "Question answered in error (Commission Error)"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            1 = "Yes"
            2 = "No"
        ;
        Value SharedH
            -5 = "Multiple responses selected in error"
            -9 = "Missing data (Not Ascertained)"
            1 = "Yes"
            2 = "No"
            3 = "Not Applicable"
        ;
        Value TextFro
            -5 = "Multiple responses selected in error"
            -9 = "Missing data (Not Ascertained)"
            1 = "Yes"
            2 = "No"
            3 = "Don't know"
        ;
        Value MostRec
            -5 = "Multiple responses selected in error"
            -9 = "Missing data (Not Ascertained)"
            1 = "Within past year"
            2 = "1 - 2 years ago"
            3 = "3 - 5 years ago"
            4 = "More than 5 years ago"
            5 = "Never"
            6 = "Don't know"
        ;
        Value FreqGoP
            -5 = "Multiple responses selected in error"
            -9 = "Missing data (Not Ascertained)"
            0 = "None"
            1 = "1 time"
            2 = "2 times"
            3 = "3 times"
            4 = "4 times"
            5 = "5-9 times"
            6 = "10 or more times"
        ;
        Value ChanceA
            -1 = "Inapplicable, coded 0 in FreqGoProvider"
            -2 = "Question answered in error (Commission Error)"
            -5 = "Multiple responses selected in error"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            1 = "Always"
            2 = "Usually"
            3 = "Sometimes"
            4 = "Never"
        ;
        Value Quality
            -1 = "Inapplicable, coded 0 in FreqGoProvider"
            -2 = "Question answered in error (Commission Error)"
            -5 = "Multiple responses selected in error"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            1 = "Excellent"
            2 = "Very good"
            3 = "Good"
            4 = "Fair"
            5 = "Poor"
        ;
        Value ProbCar
            -1 = "Inapplicable, coded 0 in FreqGoProvider"
            -2 = "Question answered in error (Commission Error)"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            1 = "Yes"
            2 = "No"
        ;
        Value HealthF
            -9 = "Missing data (Not Ascertained)"
            1 = "Yes"
            2 = "No"
        ;
        Value $HealthI
            "-1" = "Inapplicable, coded 2 in HealthIns_Other"
            "-2" = "Question answered in error (Commission Error)"
            "-6" = "Missing data (Filter Missing)"
            "-9" = "Missing data (Not Ascertained)"
            "A"-"Z" = "Verbatim response"
        ;
        Value Confide
            -5 = "Multiple responses selected in error"
            -9 = "Missing data (Not Ascertained)"
            1 = "Very confident"
            2 = "Somewhat confident"
            3 = "Not confident"
        ;
        Value EverOff
            -5 = "Multiple responses selected in error"
            -9 = "Missing data (Not Ascertained)"
            1 = "Yes"
            2 = "No"
            3 = "Don't know"
        ;
        Value WhoOffe
            -1 = "Inapplicable, coded 2 or 3 in EverOfferedAccess"
            -2 = "Question answered in error (Commission Error)"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            1 = "Selected"
            2 = "Not selected"
        ;
        Value WhoOffF
            -1 = "Inapplicable, coded 2 or 3 in EverOfferedAccess"
            -2 = "Question answered in error (Commission Error)"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            1 = "Selected"
            2 = "Not selected"
        ;
        Value $WhoOffH
            "-1" = "Inapplicable, coded 2 or 3 in EverOfferedAccess or coded 2 in WhoOffered_Other"
            "-2" = "Question answered in error (Commission Error)"
            "-6" = "Missing data (Filter Missing)"
            "-9" = "Missing data (Not Ascertained)"
            "A"-"Z" = "Verbatim response"
        ;
        Value WhoOffG
            -1 = "Inapplicable, coded 2 or 3 in EverOfferedAccess"
            -2 = "Question answered in error (Commission Error)"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            1 = "Health care provider"
            2 = "Health insurer"
            3 = "Multiple entities selected"
            91 = "Something else (Specify)"
        ;
        Value AccessO
            -1 = "Inapplicable, coded 2 or 3 in EverOfferedAccess"
            -2 = "Question answered in error (Commission Error)"
            -5 = "Multiple responses selected in error"
            -9 = "Missing data (Not Ascertained)"
            0 = "None"
            1 = "1 to 2 times"
            2 = "3 to 5 times"
            3 = "6 to 9 times"
            4 = "10 or more times"
        ;
        Value NotAcce
            -1 = "Inapplicable, coded 2 or 3 in EverOfferedAccess or coded 0 in AccessOnlineRecord"
            -2 = "Question answered in error (Commission Error)"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            1 = "Yes"
            2 = "No"
        ;
        Value NotAccF
            -1 = "Inapplicable, coded 2 or 3 in EverOfferedAccess or coded 0 in AccessOnlineRecord"
            -2 = "Question answered in error (Commission Error)"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            1 = "Yes"
            2 = "No"
        ;
        Value $NotAccG
            "-1" = "Inapp, coded 2 or 3 in EverOfferedAccess, or 0 in AccessOnlineRecord, or 2 in NotAccessed_Other"
            "-2" = "Question answered in error (Commission Error)"
            "-6" = "Missing data (Filter Missing)"
            "-9" = "Missing data (Not Ascertained)"
            "A"-"Z" = "Verbatim response"
        ;
        Value RecordF
            -1 = "Inapplicable, coded 2 or 3 in EverOfferedAccess or coded 1 or 2 or 3 or 4 in AccessOnlineRecord"
            -2 = "Question answered in error (Commission Error)"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            1 = "Yes"
            2 = "No"
        ;
        Value Records
            -1 = "Inapplicable, coded 2 or 3 in EverOfferedAccess or coded 1 or 2 or 3 or 4 in AccessOnlineRecord"
            -2 = "Question answered in error (Commission Error)"
            -5 = "Multiple responses selected in error"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            1 = "Yes"
            2 = "No"
            3 = "Don't know"
        ;
        Value UsefulO
            -1 = "Inapplicable, coded 1 or 2 or 3 or 4 in AccessOnlineRecord"
            -2 = "Question answered in error (Commission Error)"
            -5 = "Multiple responses selected in error"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            1 = "Very useful"
            2 = "Somewhat useful"
            3 = "Not very useful"
            4 = "Not at all useful"
            5 = "I do not use my online medical record to monitor my health"
        ;
        Value CaregiQ
            -9 = "Missing Data (Not Ascertained)"
            1 = "Selected"
            2 = "Not selected"
        ;
        Value CaregiR
            -9 = "Missing Data (Not Ascertained)"
            1 = "Selected"
            2 = "Not selected"
        ;
        Value CaregiT
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
            -9 = "Missing Data (Not Ascertained)"
            1 = "Yes"
            2 = "No"
        ;
        Value CaregiK
            -1 = "Inapplicable, coded 1 Caregiving_No"
            -2 = "Question answered in error (Commission Error)"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing Data (Not Ascertained)"
            1 = "Selected"
            2 = "Not selected"
        ;
        Value CaregiL
            -1 = "Inapplicable, coded 1 Caregiving_No"
            -2 = "Question answered in error (Commission Error)"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing Data (Not Ascertained)"
            1 = "Selected"
            2 = "Not selected"
        ;
        Value $CaregiP
            "-1" = "Inapplicable, coded 1 Caregiving_No or coded 2 in Caregiving_Other"
            "-2" = "Question answered in error (Commission Error)"
            "-6" = "Missing data (Filter Missing)"
            "-9" = "Missing data (Not Ascertained)"
            "A"-"Z" = "Verbatim response"
        ;
        Value CaregiO
            -1 = "Inapplicable, coded 1 Caregiving_No"
            -2 = "Question answered in error (Commission Error)"
            -6 = "Missing data (Filter Missing)"
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
        Value CaregiJ
            -1 = "Inapplicable, coded 1 in Caregiving_No"
            -2 = "Question answered in error (Commission Error)"
            -4 = "Unreadable or Non-conforming numeric response"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing Data (Not Ascertained)"
            000 - 168 = "Hours per week spend providing care"
        ;
        Value CaregiI
            -1 = "Inapplicable, coded 1 in Caregiving_No"
            -2 = "Question answered in error (Commission Error)"
            -5 = "Multiple responses selected in error"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing Data (Not Ascertained)"
            1 = "Less than 30 days"
            2 = "1 to 6 months"
            3 = "7 months to 2 years"
            4 = "3 to 5 years"
            5 = "More than 5 years"
        ;
        Value CaregiH
            -1 = "Inapplicable, coded 1 in Caregiving_No"
            -2 = "Question answered in error (Commission Error)"
            -5 = "Multiple responses selected in error"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing Data (Not Ascertained)"
            1 = "In your household"
            2 = "Within 20 minutes of your home"
            3 = "20 to 59 minutes from your home"
            4 = "1 to 2 hours from your home"
            5 = "More than 2 hours away from your home"
        ;
        Value CaregiN
            -1 = "Inapplicable, coded 1 Caregiving_No"
            -2 = "Question answered in error (Commission Error)"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing Data (Not Ascertained)"
            1 = "Getting in and out of beds and chairs"
            10 = "Housework"
            11 = "Transportation (driving him/her or arranging transportation)"
            12 = "Multiple caregiving activities selected"
            2 = "Getting dressed"
            3 = "Getting to and from the toilet"
            4 = "Dealing with incontinence or diapers"
            5 = "Bathing/Personal care"
            6 = "Preparing meals"
            7 = "Feeding him/her"
            8 = "Managing finances such as paying bills or filling out insurance claims"
            9 = "Grocery shopping or other shopping"
        ;
        Value CaregiM
            -1 = "Inapplicable, coded 1 Caregiving_No"
            -2 = "Question answered in error (Commission Error)"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing Data (Not Ascertained)"
            1 = "Performing medical/nursing tasks"
            2 = "Communicating with health care professionals"
            3 = "Arranging for outside services"
            4 = "Spending time with him/her"
            5 = "Multiple caregiving activities selected"
        ;
        Value Caregiv
            -1 = "Inapplicable, coded 1 in Caregiving_No"
            -2 = "Question answered in error (Commission Error)"
            -5 = "Multiple responses selected in error"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing Data (Not Ascertained)"
            0 = "None"
            1 = "1 to 2 times"
            2 = "3 to 5 times"
            3 = "6 to 9 times"
            4 = "10 or more times"
        ;
        Value CaregiG
            -1 = "Inapplicable, coded 1 Caregiving_No"
            -2 = "Question answered in error (Commission Error)"
            -5 = "Multiple responses selected in error"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing Data (Not Ascertained)"
            1 = "Did not need"
            2 = "Received"
            3 = "Needed and not received"
        ;
        Value CaregiS
            -9 = "Missing Data (Not Ascertained)"
            1 = "In-person instruction"
            2 = "Hotline or 24-hr phone number"
            3 = "Reading material"
            4 = "Online video"
            5 = "Virtual"
            6 = "Multiple training methods selected"
        ;
        Value Knowled
            -9 = "Missing data (Not Ascertained)"
            1 = "I've never heard of it"
            2 = "I know a little bit about palliative care"
            3 = "I know what palliative care is and could explain it to someone else"
        ;
        Value PCGoal_
            -1 = "Inapplicable, coded 1 in KnowledgePalliativeCare"
            -2 = "Question answered in error (Commission Error)"
            -5 = "Multiple responses selected in error"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing Data (Not Ascertained)"
            1 = "Strongly agree"
            2 = "Somewhat agree"
            3 = "Somewhat disagree"
            4 = "Strongly disagree"
            5 = "Don't know"
        ;
        Value PCStron
            -1 = "Inapplicable, coded 1 in KnowledgePalliativeCare"
            -2 = "Question answered in error (Commission Error)"
            -5 = "Multiple responses selected in error"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing Data (Not Ascertained)"
            1 = "Printed materials (for example, newspapers, magazines)"
            2 = "Health care provider (doctor, nurse, social worker)"
            3 = "Conversations with people you trust (friends, relatives, or co-workers)"
            4 = "Internet (Google or another search engine, WebMD or another medical website)"
            5 = "Social Media (Facebook, Instagram, Twitter)"
        ;
        Value General
            -5 = "Multiple responses selected in error"
            -9 = "Missing data (Not Ascertained)"
            1 = "Excellent,"
            2 = "Very good,"
            3 = "Good,"
            4 = "Fair, or"
            5 = "Poor?"
        ;
        Value HeightF
            -4 = "Unreadable or Non-conforming numeric response"
            -9 = "Missing data (Not Ascertained)"
            4-7 = "Height, in feet"
        ;
        Value Height_
            -4 = "Unreadable or Non-conforming numeric response"
            -9 = "Missing data (Not Ascertained)"
            00-11 = "Height, in inches"
        ;
        Value Weight
            -4 = "Unreadable or Non-conforming numeric response"
            -9 = "Missing data (Not Ascertained)"
            040-500 = "Weight, in pounds"
        ;
        Value LittleI
            -5 = "Multiple responses selected in error"
            -9 = "Missing data (Not Ascertained)"
            1 = "Nearly every day"
            2 = "More than half the days"
            3 = "Several days"
            4 = "Not at all"
        ;
        Value Emotion
            -5 = "Multiple responses selected in error"
            -9 = "Missing data (Not Ascertained)"
            1 = "Never"
            2 = "Rarely"
            3 = "Sometimes"
            4 = "Often"
            5 = "Always"
        ;
        Value Influen
            -5 = "Multiple responses selected in error"
            -9 = "Missing data (Not Ascertained)"
            1 = "A lot"
            2 = "A little"
            3 = "Not at all"
            4 = "Don't know"
        ;
        Value NoticeC
            -9 = "Missing data (Not Ascertained)"
            1 = "Yes"
            2 = "No"
        ;
        Value Underst
            -1 = "Inapplicable, coded 2 in NoticeCalorieInfoOnMenu"
            -2 = "Question answered in error (Commission Error)"
            -5 = "Multiple responses selected in error"
            -6 = "Missing data (Filter Missing)"
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
            -9 = "Missing Data (Not Ascertained)"
            1 = "Yes"
            2 = "No"
        ;
        Value Average
            -1 = "Inapplicable, coded -8 in AverageCaloriesPerDay_DK"
            -4 = "Unreadable or Non-conforming numeric response"
            -8 = "Don't know"
            -9 = "Missing data (Not Ascertained)"
            0000-9000 = "Number of calories"
        ;
        Value AveraDK
           -1 = "Inapplicable, response recorded in AverageCaloriesPerDay"
           -9 = "Missing data (Not Ascertained)"
           8 = "Don't know"
        ;
        Value DrinkDa
            -4 = "Unreadable or Non-conforming numeric response"
            -9 = "Missing data (Not Ascertained)"
            0 = "None"
            1-7 = "Days per week"
        ;
        Value DrinksP
            -1 = "Inapplicable, coded 0 in DrinkDaysPerWeek"
            -2 = "Question answered in error (Commission Error)"
            -4 = "Unreadable or Non-conforming numeric response"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            00-99 = "Drinks per day"
        ;
        Value TimesMo
            -5 = "Multiple responses selected in error"
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
            -9 = "Missing data (Not Ascertained)"
            000-999 = "Minutes of exercise time"
        ;
        Value TimesSt
            -5 = "Multiple responses selected in error"
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
        Value AveragF
             -4 = "Unreadable or Non-conforming numeric response"
            -9 = "Missing data (Not Ascertained)"
            00 - 24 = "Hours per day"
        ;
        Value TimesUs
            -9 = "Missing data (Not Ascertained)"
            000 - 365 = "Times"
        ;
        Value SpendTi
            -5 = "Multiple responses selected in error"
            -9 = "Missing data (Not Ascertained)"
            1 = "Often"
            2 = "Sometimes"
            3 = "Rarely"
            4 = "Never"
            5 = "Don't go out on sunny days"
        ;
        Value SunEffe
            -5 = "Multiple responses selected in error"
            -9 = "Missing data (Not Ascertained)"
            1 = "Get a severe sunburn with blisters"
            2 = "Have a moderate sunburn with peeling"
            3 = "Burn mildly with some or no tanning"
            4 = "Turn darker without sunburn"
            5 = "Nothing would happen to my skin"
        ;
        Value Smoke1F
            -9 = "Missing data (Not Ascertained)"
            1 = "Yes"
            2 = "No"
        ;
        Value SmokeNo
            -1 = "Inapplicable, coded 2 in Smoke100"
            -2 = "Question answered in error (Commission Error)"
            -5 = "Multiple responses selected in error"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            1 = "Everyday"
            2 = "Some days"
            3 = "Not at all"
        ;
        Value ElectCi
            -5 = "Multiple responses selected in error"
            -9 = "Missing data (Not Ascertained)"
            1 = "Much less harmful"
            2 = "Less harmful"
            3 = "Just as harmful"
            4 = "More harmful"
            5 = "Much more harmful, or"
            7 = "I don't know"
        ;
        Value UsedECi
            -9 = "Missing data (Not Ascertained)"
            1 = "Yes"
            2 = "No"
        ;
        Value UseECig
            -1 = "Inapplicable, coded 2 in UsedEcCigEver"
            -2 = "Question answered in error (Commission Error)"
            -5 = "Multiple responses selected in error"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            1 = "Everyday"
            2 = "Some days"
            3 = "Not at all"
        ;
        Value SeenFed
            -9 = "Missing data (Not Ascertained)"
            1 = "Yes"
            2 = "No"
        ;
        Value Tobacco
            -1 = "Inapplicable, coded 2 in SeenFederalCourtTobaccoMessages"
            -2 = "Question answered in error (Commission Error)"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing Data"
            1 = "Selected"
            2 = "Not selected"
        ;
        Value TobaccF
            -1 = "Inapplicable, coded 2 in SeenFederalCourtTobaccoMessages"
            -2 = "Question answered in error (Commission Error)"
            -6 = "Missing data (Filter Missing)"
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
            -9 = "Missing data (Not Ascertained)"
            1 = "Male"
            2 = "Female"
        ;
        Value WhenPap
            -1 = "Inapplicable, coded 1 in GenderC"
            -2 = "Question answered in error (Commission Error)"
            -5 = "Multiple responses selected in error"
            -6 = "Missing data (Filter Missing)"
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
            -9 = "Missing data (Not Ascertained)"
            1 = "A year ago or less"
            2 = "More than 1, up to 2 years ago"
            3 = "More than 2, up to 3 years ago"
            4 = "More than 3, up to 5 years ago"
            5 = "More than 5 years ago"
            6 = "I have never had a mammogram"
        ;
        Value HeardHP
            -9 = "Missing data (Not Ascertained)"
            1 = "Yes"
            2 = "No"
        ;
        Value HPVCaus
            -1 = "Inapplicable, coded 2 in HeardHPV"
            -2 = "Question answered in error (Commission Error)"
            -5 = "Multiple responses selected in error"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            1 = "Yes"
            2 = "No"
            3 = "Not sure"
        ;
        Value FamBetw
            -9 = "Missing data (Not Ascertained)"
            1 = "Yes"
            2 = "No"
        ;
        Value Recomme
            -1 = "Inapplicable, coded 1 in FamBetween9and27"
            -2 = "Question answered in error (Commission Error)"
            -5 = "Multiple responses selected in error"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            1 = "Yes"
            2 = "No"
            3 = "Don't know"
        ;
        Value EverHad
            -9 = "Missing data (Not Ascertained)"
            1 = "Yes"
            2 = "No"
        ;
        Value CaBladd
            -1 = "Inapplicable, coded 2 in EverHadCancer"
            -2 = "Question answered in error (Commission Error)"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            1 = "Selected"
            2 = "Not selected"
        ;
        Value CaOther
            -1 = "Inapplicable, coded 2 in EverHadCancer"
            -2 = "Question answered in error (Commission Error)"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            1 = "Selected - Specify"
            2 = "Not selected"
        ;
        Value $CaOtheF
            "-1" = "Inapplicable, coded 2 in EverHadCancer or coded 2 in CaOther"
            "-2" = "Question answered in error (Commission Error)"
            "-6" = "Missing data (Filter Missing)"
            "-9" = "Missing data (Not Ascertained)"
            "A"-"Z" = "Verbatim response"
        ;
        Value Cancer_
            -1 = "Inapplicable, coded 2 in EverHadCancer"
            -2 = "Question answered in error (Commission Error)"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            1 = "Bladder cancer only"
            10 = "Leukemia only"
            11 = "Liver cancer only"
            12 = "Lung cancer only"
            13 = "Melanoma only"
            14 = "Non-Hodgkin only"
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
            -9 = "Missing data (Not Ascertained)"
            000-101 = "Age at diagnosis"
        ;
        Value FreqWoF
            -1 = "Inapplicable, coded 2 in EverHadCancer"
            -2 = "Question answered in error (Commission Error)"
            -5 = "Multiple responses selected in error"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            1 = "Not at all"
            2 = "Slightly"
            3 = "Somewhat"
            4 = "Moderately"
            5 = "Extremely"
        ;
        Value ImaginF
            -1 = "Inapplicable, coded 2 in EverHadCancer"
            -2 = "Question answered in error (Commission Error)"
            -6 = "Missing data (Filter Missing)"
            -5 = "Multiple responses selected in error"
            -9 = "Missing data (Not Ascertained)"
            1 = "Extremely difficult"
            2 = "Somewhat difficult"
            3 = "Neither difficult nor easy"
            4 = "Somewhat easy"
            5 = "Extremely easy"
        ;
        Value FreqWor
            -1 = "Inapplicable, coded 1 in EverHadCancer"
            -2 = "Question answered in error (Commission Error)"
            -5 = "Multiple responses selected in error"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            1 = "Not at all"
            2 = "Slightly"
            3 = "Somewhat"
            4 = "Moderately"
            5 = "Extremely"
        ;
        Value Imagine
            -1 = "Inapplicable, coded 1 in EverHadCancer"
            -2 = "Question answered in error (Commission Error)"
            -5 = "Multiple responses selected in error"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            1 = "Extremely difficult"
            2 = "Somewhat difficult"
            3 = "Neither difficult nor easy"
            4 = "Somewhat easy"
            5 = "Extremely easy"
        ;
        Value Everyth
            -5 = "Multiple responses selected in error"
            -9 = "Missing data (Not Ascertained)"
            1 = "Strongly agree"
            2 = "Somewhat agree"
            3 = "Somewhat disagree"
            4 = "Strongly disagree"
        ;
        Value Familia
            -5 = "Multiple responses selected in error"
            -9 = "Missing data (Not Ascertained)"
            1 = "Very well"
            2 = "Well"
            3 = "Somewhat"
            4 = "A little"
            5 = "Not at all"
        ;
        Value FamilyC
            -9 = "Missing data (Not Ascertained)"
            1 = "Selected"
            2 = "Not selected"
        ;
        Value FamilyF
            -9 = "Missing data (Not Ascertained)"
            1 = "Your biological mother"
            2 = "Your biological father"
            3 = "Your biological sister"
            4 = "Your biological brother"
            5 = "Your biological children"
            6 = "Other biological family members"
            7 = "A health care provider"
            8 = "I have not had discussions with any of these people"
            9 = "More than one person checked"
        ;
        Value FamilyE
            -5 = "Multiple responses selected in error"
            -9 = "Missing data (Not Ascertained)"
            1 = "Yes"
            2 = "No"
            4 = "Not sure"
        ;
        Value Age
            -4 = "Unreadable or Non-conforming numeric response"
            -9 = "Missing data (Not Ascertained)"
            18-101 = "Current age, in years"
        ;
        Value OccupaF
            -5 = "Multiple responses selected in error"
            -9 = "Missing data (Not Ascertained)"
            1 = "Employed"
            2 = "Unemployed"
            3 = "Homemaker"
            4 = "Student"
            5 = "Retired"
            6 = "Disabled"
            91 = "Other - Specify"
        ;
        Value $Occupat
            "-1" = "Inapplicable, coded 1 - 6 in OccupationStatus"
            "-2" = "Question answered in error (Commission Error)"
            "-6" = "Missing data (Filter Missing)"
            "-9" = "Missing data (Not Ascertained)"
            "A"-"Z" = "Verbatim response"
        ;
        Value Employe
            -9 = "Missing data (Not Ascertained)"
            1 = "Response checked on form"
            2 = "Response not checked on form"
        ;
        Value MultiOc
            0-7 = "total responses checked in question O2"
        ;
        Value ActiveD
            -5 = "Multiple responses selected in error"
            -9 = "Missing data (Not Ascertained)"
            1 = "Yes, now on active duty"
            2 = "Yes, on active duty in the last 12 months but not now"
            3 = "Yes, on active duty in the past, but not in the last 12 months"
            4 = "No, training for Reserves or National Guard only"
            5 = "No, never served in the military"
        ;
        Value Receive
            -1 = "Inapplicable, coded 4 or 5 in ActiveDutyArmedForces"
            -2 = "Question answered in error (Commission Error)"
            -5 = "Multiple responses selected in error"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            1 = "Yes, all my health care"
            2 = "Yes, some of my health care"
            3 = "No, no VA health care received"
        ;
        Value Marital
            -5 = "Multiple responses selected in error"
            -9 = "Missing data (Not Ascertained)"
            1 = "Married"
            2 = "Living as married"
            3 = "Divorced"
            4 = "Widowed"
            5 = "Separated"
            6 = "Single, never been married"
        ;
        Value Educati
            -9 = "Missing data (Not Ascertained)"
            1 = "Less than 8 years"
            2 = "8 through 11 years"
            3 = "12 years or completed high school"
            4 = "Post high school training other than college (vocational or technical)"
            5 = "Some college"
            6 = "College graduate"
            7 = "Postgraduate"
        ;
        Value BornInU
            -9 = "Missing data (Not Ascertained)"
            1 = "Yes"
            2 = "No"
        ;
        Value YearCam
            -1 = "Inapplicable, coded 1 in BornInUSA"
            -2 = "Question answered in error (Commission Error)"
            -4 = "Unreadable or Non-conforming numeric response"
            -6 = "Missing data (Filter Missing)"
            -9 = "Missing data (Not Ascertained)"
            0000-2018 = "Year"
        ;
        Value SpeakEn
            -5 = "Multiple responses selected in error"
            -9 = "Missing data (Not Ascertained)"
            1 = "Very well"
            2 = "Well"
            3 = "Not well"
            4 = "Not at all"
        ;
        Value Hisp_Ca
            -9 = "Missing data (Not Ascertained)"
            10 = "Not Hispanic"
            21 = "Mexican"
            22 = "Puerto Rican"
            23 = "Cuban"
            24 = "Other Hispanic"
            25 = "Multiple Hispanic ethnicities selected"
        ;
        Value White
            -9 = "Missing data (Not Ascertained)"
            1 = "Selected"
            2 = "Not Selected"
        ;
        Value Race_Ca
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
            "-9" = "Missing data (Not Ascertained)"
            "A"-"Z" = "Verbatim response"
        ;
        Value TotalHo
            -4 = "Unreadable or Non-conforming numeric response"
            -9 = "Missing data (Not Ascertained)"
            00-75 = "Number of people"
        ;
        Value R_HHAdul
            . = "Missing value"
            01-75 = "Number of adults"
        ;
        Value HHAdults
            01-75 = "Number of adults"
        ;
        Value SelfGen
            -5 = "Multiple responses selected in error"
            -9 = "Missing Data"
            1 = "Male"
            2 = "Female"
        ;
        Value SelfAge
            -4 = "Unreadable or Non-conforming numeric response"
            -9 = "Missing Data"
            018-101 = "Age, in years"
        ;
        Value SelfMOB
            -4 = "Unreadable or Non-conforming numeric response"
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
            -9 = "Missing Data"
            1 = "Male"
            2 = "Female"
        ;
        Value HHAdult
            -1 = "Inapplicable, Fewer than 2 adults in HH"
            -4 = "Unreadable or Non-conforming numeric response"
            -9 = "Missing Data"
            018-101 = "Age, in years"
        ;
        Value HHAdulI
            -1 = "Inapplicable, Fewer than 2 adults in HH"
            -4 = "Unreadable or Non-conforming numeric response"
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
            -9 = "Missing Data"
            1 = "Male"
            2 = "Female"
        ;
        Value HHAdulF
            -1 = "Inapplicable, Fewer than 3 adults in HH"
            -4 = "Unreadable or Non-conforming numeric response"
            -9 = "Missing Data"
            018-100 = "Age, in years"
        ;
        Value HHAdulJ
            -1 = "Inapplicable, Fewer than 3 adults in HH"
            -4 = "Unreadable or Non-conforming numeric response"
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
            -9 = "Missing Data"
            1 = "Male"
            2 = "Female"
        ;
        Value HHAdulG
            -1 = "Inapplicable, Fewer than 4 adults in HH"
            -4 = "Unreadable or Non-conforming numeric response"
            -9 = "Missing Data"
            018-100 = "Age, in years"
        ;
        Value HHAdulK
            -1 = "Inapplicable, Fewer than 4 adults in HH"
            -4 = "Unreadable or Non-conforming numeric response"
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
            -9 = "Missing Data"
            1 = "Male"
            2 = "Female"
        ;
        Value HHAdulH
            -1 = "Inapplicable, Fewer than 5 adults in HH"
            -4 = "Unreadable or Non-conforming numeric response"
            -9 = "Missing Data"
            018-100 = "Age, in years"
        ;
        Value HHAdulL
            -1 = "Inapplicable, Fewer than 5 adults in HH"
            -4 = "Unreadable or Non-conforming numeric response"
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
            -9 = "Missing Data"
            00-12 = "Number of children under 18"
        ;
        Value RentOrO
            -5 = "Multiple responses selected in error"
            -9 = "Missing Data"
            1 = "Own"
            2 = "Rent"
            3 = "Occupied without paying monetary rent"
        ;
        Value IncomeR
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
        Value MailSuF
            -1 = "Inapplicable, time recorded in ""Hours"" only"
            -4 = "Unreadable or Non-conforming numeric response"
            -9 = "Missing Data"
            00-90 = "Minutes"
        ;
        Value MailSur
            -1 = "Inapplicable, time recorded in ""Minutes"" only"
            -4 = "Unreadable or Non-conforming numeric response"
            -9 = "Missing Data"
            00-12 = "Hours"
        ;
        Value TypeOfA
            -9 = "Missing Data"
            1 = "Selected"
            2 = "Not Selected"
        ;

		value AgeGrpA 
			1 = '18-34'
			2 = '35-39'
			3 = '40-44'
			4 = '45+'
			-4 = 'Unreadable or Nonconforming Numeric Response'
			-9 = 'Missing Data (Not Ascertained)'				;

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
			-9 = 'Missing Data (Not Ascertained)'				;

			value EducB 
			1 = 'Less than High School'
			2 = 'High School Graduate'
			3 = 'Some College'
			4 = "Bachelor's Degree"
			5 = 'Post-Baccalaureate Degree'
			-9 = 'Missing Data (Not Ascertained)'				;

			value RaceEthn 
			1 = 'Hispanic'
			2 = 'Non-Hispanic White'
			3 = 'Non-Hispanic Black or African American'
			4 = 'Non-Hispanic American Indian or Alaska Native'
			5 = 'Non-Hispanic Asian'
			6 = 'Non-Hispanic Native Hawaiian or other Pacific Islander'
			7 = 'Non-Hispanic Multiple Races Mentioned'
			-4 = 'Unreadable or Nonconforming Numeric Response'
			-9 = 'Missing Data (Not Ascertained)'					;

			value RaceEthn5f 
			1 = "Non-Hispanic White"
			2= "Non-Hispanic Black or African American"
			3="Hispanic"
			4="Non-Hispanic Asian"
			5="Non-Hispanic Other"
			-9="Missing Data--Not Ascertained"
			;

			value HHInc 
			1 = 'Less than $20,000'
			2 = '$20,000 to < $35,000'
			3 = '$35,000 to < $50,000'
			4 = '$50,000 to < $75,000'
			5 = '$75,000 or More'
			-5 = 'Multiple Responses Selected in Error'
			-9 = 'Missing Data (Not Ascertained)'				;

			value BMI 
			-4 = 'Unreadable or Nonconforming Numeric Response'
			-9 = 'Missing Data (Not Ascertained)'			;

			value AgeDX 
			-1 = 'Inapplicable, coded 2 in EverHadCancer'
			-2 = 'Question Answered in Error (Commission Error)'
			-4 = 'Unreadable or Non-conforming numeric response'
			-6 = 'Missing Data (Filter Missing)'
			-9 = 'Missing Data (Not Ascertained)'				;

			value TimeSinceDX 
			1 = 'Less than 1 Yr Since DX'
			2 = '2-5 Yrs Since DX'
			3 = '6-10 Yrs Since DX'
			4 = '11+ Yrs Since DX'
			-1 = 'Inapplicable, coded 2 in EverHadCancer'
			-4 = 'Unreadable or Nonconforming Numeric Response'
			-6 = 'Missing Data (Filter Missing), coded -9 in EverHadCancer'
			-9 = 'Missing Data (Not Ascertained)'					;

			value smokeStat 
			1 = 'Current'
			2 = 'Former'
			3 = 'Never'
			-4 = 'Unreadable or Nonconforming Numeric Response'
			-6 = 'Missing Data (Filter Missing), coded -9 in Smoke100'
			-9 = 'Missing Data (Not Ascertained)'					;

			value phq4f
			-5 = 'Multiple Responses Selected in Error'
			-9 = 'Missing Data (Not Ascertained)'
			;

			value WeeklyMinutesModerateExercise
			-5 = 'Multiple Responses Selected in Error'
			-9 = 'Missing Data (Not Ascertained)'
			;

			value ecigStat 
			1 = 'Current'
			2 = 'Former'
			3 = 'Never'
			-4 = 'Unreadable or Nonconforming Numeric Response'
			-6 = 'Missing Data (Filter Missing), coded -9 in UsedECigEver'
			-9 = 'Missing Data (Not Ascertained)';

			value AvgDrinksPerWeek
			-4 = 'Unreadable or Nonconforming Numeric Response'
			-5 = 'Multiple Responses Selected in Error'
			-9 = 'Missing Data (Not Ascertained)'
			;
run;
