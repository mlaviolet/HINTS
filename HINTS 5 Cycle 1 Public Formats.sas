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
     8 = "Nonmetro county completely rural or less than 2,500 urban population, adjacent to"
     9 = "Nonmetro county completely rural or less than 2,500 urban population, not adjacent to"
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
 Value PR_RUCAF
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
 Value DMA
      .  = "Unknown"
     111 = "NON DMA AREA"
     500 = "PORTLAND-AUBURN"
     501 = "NEW YORK"
     502 = "BINGHAMTON"
     503 = "MACON"
     504 = "PHILADELPHIA"
     505 = "DETROIT"
     506 = "BOSTON (MANCHR)"
     507 = "SAVANNAH"
     508 = "PITTSBURGH"
     509 = "FT. WAYNE"
     510 = "CLEVELAND"
     511 = "WASH  DC (HAG)"
     512 = "BALTIMORE"
     513 = "FLINT-SAGNAW-BC"
     514 = "BUFFALO"
     515 = "CINCINNATI"
     516 = "ERIE"
     517 = "CHARLOTTE"
     518 = "GREENSBRO-HP-WS"
     519 = "CHARLESTON  SC"
     520 = "AUGUSTA"
     521 = "PROVIDNC-N. BED"
     522 = "COLUMBUS  GA"
     523 = "BURLNGTN-PLTBRG"
     524 = "ATLANTA"
     525 = "ALBANY  GA"
     526 = "UTICA"
     527 = "INDIANAPOLIS"
     528 = "MIAMI-FT. LAUDE"
     529 = "LOUISVILLE"
     530 = "TALLHSEE-THMSVL"
     531 = "TRI-CTIES TN-VA"
     532 = "ALBANY-SCH-TROY"
     533 = "HARTFRD&NW HAVN"
     534 = "ORL-DYTN B-MLBN"
     535 = "COLUMBUS  OH"
     536 = "YOUNGSTOWN"
     537 = "BANGOR"
     538 = "ROCHESTER  NY"
     539 = "TAMPA-ST P(SAR)"
     540 = "TRAVRS CTY-CDLC"
     541 = "LEXINGTON"
     542 = "DAYTON"
     543 = "SPRINGFLD-HLYOK"
     544 = "NRFLK-PRT-NP NW"
     545 = "GREENVL-NB-WASH"
     546 = "COLUMBIA  SC"
     547 = "TOLEDO"
     548 = "WEST PLM BCH-FP"
     549 = "WATERTOWN"
     550 = "WILMINGTON"
     551 = "LANSING"
     552 = "PRESQUE ISLE"
     553 = "MARQUETTE"
     554 = "WHEELING-STEUB."
     555 = "SYRACUSE"
     556 = "RICHMOND-PTRSBG"
     557 = "KNOXVILLE"
     558 = "LIMA"
     559 = "BLFLD-BECKLY-OH"
     560 = "RALEIGH-DUR(FY)"
     561 = "JACKSONVILLE"
     563 = "GR.RAPIDS-KL-BC"
     564 = "CHARLSTN-HUNTNG"
     565 = "ELMIRA"
     566 = "HARRSBG-LA-LB-Y"
     567 = "GRVL-SPA-ASH-AN"
     569 = "HARRISONBURG"
     570 = "FLRNCE-MRTL BCH"
     571 = "FT. MYERS-NAPLS"
     573 = "ROANOKE-LNCHBRG"
     574 = "JOHNSTOWN-ALTNA"
     575 = "CHATTANOOGA"
     576 = "SALISBURY"
     577 = "WILKES BAR-SCR"
     581 = "TERRE HAUTE"
     582 = "LAFAYETTE  IN"
     583 = "ALPENA"
     584 = "CHARLOTTESVILLE"
     588 = "SO. BEND-ELKHRT"
     592 = "GAINESVILLE"
     596 = "ZANESVILLE"
     597 = "PARKERSBURG"
     598 = "CLARKSBURG-WSTN"
     600 = "CORPUS CHRISTI"
     602 = "CHICAGO"
     603 = "JOPLIN-PITTSBRG"
     604 = "COLUMBIA-JF CTY"
     605 = "TOPEKA"
     606 = "DOTHAN"
     609 = "ST. LOUIS"
     610 = "ROCKFORD"
     611 = "RCH-MASN CY-AUS"
     612 = "SHREVEPORT"
     613 = "MINEAPLS-ST. PL"
     616 = "KANSAS CITY"
     617 = "MILWAUKEE"
     618 = "HOUSTON"
     619 = "SPRINGFIELD  MO"
     622 = "NEW ORLEANS"
     623 = "DALLAS-FT.WORTH"
     624 = "SIOUX CITY"
     625 = "WACO-TEMPL-BRYN"
     626 = "VICTORIA"
     627 = "WICHTA FLS&LWTN"
     628 = "MONRO-EL DORADO"
     630 = "BIRMINGHAM"
     631 = "OTTUMWA-KIRKSVL"
     632 = "PDCH-CG-HAR-MTV"
     633 = "ODESSA-MIDLAND"
     634 = "AMARILLO"
     635 = "AUSTIN"
     636 = "HRLG-WSL-BRN-MA"
     637 = "CDR RP-WA-IC&DB"
     638 = "ST. JOSEPH"
     639 = "JACKSON  TN"
     640 = "MEMPHIS"
     641 = "SAN ANTONIO"
     642 = "LAFAYETTE  LA"
     643 = "LAKE CHARLES"
     644 = "ALEXANDRIA  LA"
     647 = "GREENWD-GREENVL"
     648 = "CHAMPGN&SPR-DEC"
     649 = "EVANSVILLE"
     650 = "OKLAHOMA CITY"
     651 = "LUBBOCK"
     652 = "OMAHA"
     656 = "PANAMA CITY"
     657 = "SHERMAN-ADA"
     658 = "GREEN BAY-APLTN"
     659 = "NASHVILLE"
     661 = "SAN ANGELO"
     662 = "ABILENE-SWTWATR"
     669 = "MADISON"
     670 = "FT SMH-FY-SP-RG"
     671 = "TULSA"
     673 = "COLUMBUS-TUP-WP"
     675 = "PEORIA-BLMINGTN"
     676 = "DULUTH-SUPERIOR"
     678 = "WICHTA-HTCH PLS"
     679 = "DES MOINES-AMES"
     682 = "DAVNPRT-RI-MLNE"
     686 = "MOBILE-PNS(FWB)"
     687 = "MINOT-BSMRK-DKN"
     691 = "HNTVLE-DCTR(FL)"
     692 = "BEAUMNT-PRT ART"
     693 = "LITL RCK-PN BLF"
     698 = "MONTGOMERY(SEL)"
     702 = "LA CRSS-EAU CLR"
     705 = "WAUSAU-RHINELDR"
     709 = "TYLER-LONGVIEW"
     710 = "HATIESBRG-LAURL"
     711 = "MERIDIAN"
     716 = "BATON ROUGE"
     717 = "QUINCY-HBL-KEOK"
     718 = "JACKSON  MS"
     722 = "LIN&HST-KRNY"
     724 = "FARGO-VALLY CTY"
     725 = "SIOUX FLS(MCHL)"
     734 = "JONESBORO"
     736 = "BOWLING GREEN"
     737 = "MANKATO"
     740 = "NORTH PLATTE"
     743 = "ANCHORAGE"
     744 = "HONOLULU"
     745 = "FAIRBANKS"
     746 = "BILOXI-GULFPORT"
     747 = "JUNEAU"
     749 = "LAREDO"
     751 = "DENVER"
     752 = "COLO SPRNGS-PBL"
     753 = "PHOENIX"
     754 = "BUTTE-BOZEMAN"
     755 = "GREAT FALLS"
     756 = "BILLINGS"
     757 = "BOISE"
     758 = "IDAHO FALLS-POC"
     759 = "CHEY-SCTTS"
     760 = "TWIN FALLS"
     762 = "MISSOULA"
     764 = "RAPID CITY"
     765 = "EL PASO"
     766 = "HELENA"
     767 = "CASPER-RIVERTON"
     770 = "SALT LAKE CITY"
     771 = "YUMA-EL CENTRO"
     773 = "GR.JNCTN-MNTROS"
     789 = "TUCSON (S VSTA)"
     790 = "ALBUQ-SANTA FE"
     798 = "GLENDIVE"
     800 = "BAKERSFIELD"
     801 = "EUGENE"
     802 = "EUREKA"
     803 = "LOS ANGELES"
     804 = "PALM SPRINGS"
     807 = "SAN FRAN-OAK-SJ"
     810 = "YAKM-PSC-RCH-KN"
     811 = "RENO"
     813 = "MEDFORD-KLM FLS"
     819 = "SEATTLE-TACOMA"
     820 = "PORTLAND  OR"
     821 = "BEND  OR"
     825 = "SAN DIEGO"
     828 = "MONTEREY-SALNAS"
     839 = "LAS VEGAS"
     855 = "SANTABAR-SM-SLO"
     862 = "SACRMNTO-STK-MO"
     866 = "FRESNO-VISALIA"
     868 = "CHICO-REDDING"
     881 = "SPOKANE"
 ;
 Value CENSDIV
     1 = "New England"
     2 = "Middle Atlantic"
     3 = "East North Central"
     4 = "West North Central"
     5 = "South Atlantic"
     6 = "East South Central"
     7 = "West South Central"
     8 = "Mountain"
     9 = "Pacific"
 ;
 Value CENSREG
     01 = "Northeast"
     02 = "Midwest"
     03 = "South"
     04 = "West"
 ;
 Value FIPST
      1 = "Alabama"
      2 = "Alaska"
      4 = "Arizona"
      5 = "Arkansas"
      6 = "California"
      8 = "Colorado"
      9 = "Connecticut"
     10 = "Delaware"
     11 = "District of Columbia"
     12 = "Florida"
     13 = "Georgia"
     15 = "Hawaii"
     16 = "Idaho"
     17 = "Illinois"
     18 = "Indiana"
     19 = "Iowa"
     20 = "Kansas"
     21 = "Kentucky"
     22 = "Louisiana"
     23 = "Maine"
     24 = "Maryland"
     25 = "Massachusetts"
     26 = "Michigan"
     27 = "Minnesota"
     28 = "Mississippi"
     29 = "Missouri"
     30 = "Montana"
     31 = "Nebraska"
     32 = "Nevada"
     33 = "New Hampshire"
     34 = "New Jersey"
     35 = "New Mexico"
     36 = "New York"
     37 = "North Carolina"
     38 = "North Dakota"
     39 = "Ohio"
     40 = "Oklahoma"
     41 = "Oregon"
     42 = "Pennsylvania"
     44 = "Rhode Island"
     45 = "South Carolina"
     46 = "South Dakota"
     47 = "Tennessee"
     48 = "Texas"
     49 = "Utah"
     50 = "Vermont"
     51 = "Virginia"
     53 = "Washington"
     54 = "West Virginia"
     55 = "Wisconsin"
     56 = "Wyoming"
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
     00-12 = "Number of adults living in the household"
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
 Value LotOfEf
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
 Value ConfidF
     -5 = "Multiple responses selected in error"
     -9 = "Missing data (Not Ascertained)"
     1 = "Completely confident"
     2 = "Very confident"
     3 = "Somewhat confident"
     4 = "A little confident"
     5 = "Not confident at all"
 ;
 Value TrustDo
     -5 = "Multiple responses selected in error"
     -9 = "Missing data (Not Ascertained)"
     1 = "A lot"
     2 = "Some"
     3 = "A little"
     4 = "Not At All"
 ;
 Value StrongG
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
     "-1" = "Inapplicable, coded -5 or 1 or 2 or 3 or 4 or 5 or 6 or 7 or 8 or 9 or 10 or 11 or 12 in"
     "-2" = "Question answered in error (Commission Error)"
     "-6" = "Missing data (Filter Missing)"
     "-9" = "Missing data (Not Ascertained)"
     "A"-"Z" = "Verbatim response"
 ;
 Value SeekCan
     -9 = "Missing data (Not Ascertained)"
     1 = "Yes"
     2 = "No"
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
     1 = "Yes"
     2 = "No"
 ;
 Value TabletH
     -9 = "Missing data (Not Ascertained)"
     1 = "Yes"
     2 = "No"
     3 = "Don't know"
     4 = "Do not have a tablet or smartphone"
 ;
 Value Tablet_
     -1 = "Inapplicable, coded 4 in TabletHealthWellnessApps"
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
 Value AccessO
     -5 = "Multiple responses selected in error"
     -9 = "Missing data (Not Ascertained)"
     0 = "None"
     1 = "1 to 2 times"
     2 = "3 to 5 times"
     3 = "6 to 9 times"
     4 = "10 or more times"
 ;
 Value NotAcce
     -1 = "Inapplicable, coded 0 in AccessOnlineRecord"
     -2 = "Question answered in error (Commission Error)"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing data (Not Ascertained)"
     1 = "Yes"
     2 = "No"
 ;
 Value NotAccF
     -1 = "Inapplicable, coded 0 in AccessOnlineRecord"
     -2 = "Question answered in error (Commission Error)"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing data (Not Ascertained)"
     1 = "Yes"
     2 = "No"
 ;
 Value $NotAccG
     "-1" = "Inapplicable, coded 2 in NotAccessed_Other"
     "-2" = "Question answered in error (Commission Error)"
     "-6" = "Missing data (Filter Missing)"
     "-9" = "Missing data (Not Ascertained)"
     "A"-"Z" = "Verbatim response"
 ;
 Value RecordF
     -1 = "Inapplicable, coded 1 or 2 or 3 or 4 in AccessOnlineRecord"
     -2 = "Question answered in error (Commission Error)"
     -5 = "Multiple responses selected in error"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing data (Not Ascertained)"
     1 = "Yes"
     2 = "No"
     3 = "Don't know"
 ;
 Value Records
     -1 = "Inapplicable, coded 1 or 2 or 3 or 4 in AccessOnlineRecord"
     -2 = "Question answered in error (Commission Error)"
     -5 = "Multiple responses selected in error"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing data (Not Ascertained)"
     1 = "Yes"
     2 = "No"
 ;
 Value Underst
     -1 = "Inapplicable, coded 1 or 2 or 3 or 4 in AccessOnlineRecord"
     -2 = "Question answered in error (Commission Error)"
     -5 = "Multiple responses selected in error"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing data (Not Ascertained)"
     1 = "Very easy"
     2 = "Somewhat easy"
     3 = "Somewhat difficult"
     4 = "Very difficult"
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
 Value Confide
     -5 = "Multiple responses selected in error"
     -9 = "Missing data (Not Ascertained)"
     1 = "Very confident"
     2 = "Somewhat confident"
     3 = "Not confident"
 ;
 Value ElectIn
     -5 = "Multiple responses selected in error"
     -9 = "Missing data (Not Ascertained)"
     1 = "Very concerned"
     2 = "Somewhat concerned"
     3 = "Not concerned"
 ;
 Value AccessF
     -9 = "Missing data (Not Ascertained)"
     0 = "None"
     1 = "1 to 2 times"
     2 = "3 to 5 times"
     3 = "6 to 9 times"
     4 = "10 or more times"
 ;
 Value Accesse
     -1 = "Inapplicable, coded 0 AccessFamilyMedRec"
     -2 = "Question answered in error (Commission Error)"
     -5 = "Multiple responses selected in error"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing data (Not Ascertained)"
     1 = "Yes"
     2 = "No"
 ;
 Value CaregiJ
     -9 = "Missing Data (Not Ascertained)"
     1 = "Selected"
     2 = "Not selected"
 ;
 Value CaregiK
     -9 = "Missing Data (Not Ascertained)"
     1 = "Selected"
     2 = "Not selected"
 ;
 Value CaregiL
     -9 = "Missing data (Not Ascertained)"
     1 = "Yes, a child/children"
     2 = "Yes, a spouse/partner"
     3 = "Yes, a parent/parents"
     4 = "Yes, a close family member"
     5 = "Yes, a friend or other non-relative"
     6 = "No"
     7 = "Multiple caregiving relationships selected"
 ;
 Value Caregiv
     -1 = "Inapplicable, coded 1 Caregiving_No"
     -2 = "Question answered in error (Commission Error)"
     -5 = "Multiple responses selected in error"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing Data (Not Ascertained)"
     1 = "Selected"
     2 = "Not selected"
 ;
 Value CaregiF
     -1 = "Inapplicable, coded 1 Caregiving_No"
     -2 = "Question answered in error (Commission Error)"
     -5 = "Multiple responses selected in error"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing Data (Not Ascertained)"
     1 = "Selected"
     2 = "Not selected"
 ;
 Value $CaregiI
     "-1" = "Inapplicable, coded 1 Caregiving_No or coded 2 in Caregiving_Other"
     "-2" = "Question answered in error (Commission Error)"
     "-6" = "Missing data (Filter Missing)"
     "-9" = "Missing data (Not Ascertained)"
     "A"-"Z" = "Verbatim response"
 ;
 Value CaregiH
     -1 = "Inapplicable, coded 1 Caregiving_No"
     -2 = "Question answered in error (Commission Error)"
     -5 = "Multiple responses selected in error"
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
 Value CaregiG
     -1 = "Inapplicable, coded 1 in Caregiving_No"
     -2 = "Question answered in error (Commission Error)"
     -5 = "Multiple responses selected in error"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing Data (Not Ascertained)"
     1 = "Less than 5 hours per week"
     2 = "5-14 hours per week"
     3 = "15-20 hours per week"
     4 = "21-34 hours per week"
     5 = "35 or more hours per week"
 ;
 Value HeardDN
     -9 = "Missing data (Not Ascertained)"
     1 = "Yes"
     2 = "No"
 ;
 Value Genetic
     -1 = "Inapplicable, coded 2 in HeardDNATest"
     -2 = "Question answered in error (Commission Error)"
     -5 = "Multiple responses selected in error"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing Data (Not Ascertained)"
     1 = "Selected"
     2 = "Not selected"
 ;
 Value GenetiF
     -1 = "Inapplicable, coded 2 in HeardDNATest"
     -2 = "Question answered in error (Commission Error)"
     -5 = "Multiple responses selected in error"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing Data (Not Ascertained)"
     1 = "Determining risk or likelihood of getting a particular disease"
     2 = "Determining how a disease should be treated after diagnosis"
     3 = "Determining which drug(s) may or may not work for an individual"
     4 = "Determining the likelihood of passing an inherited disease to your children"
     5 = "Multiple genetic test uses selected"
 ;
 Value HadTest
     -1 = "Inapplicable, coded 2 in HeardDNATest"
     -2 = "Question answered in error (Commission Error)"
     -5 = "Multiple responses selected in error"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing Data (Not Ascertained)"
     1 = "Selected"
     2 = "Not selected"
 ;
 Value $HadTesG
     "-1" = "Inapplicable, coded 2 in HeardDNATest or coded 2 in HadTest_Other"
     "-2" = "Question answered in error (Commission Error)"
     "-6" = "Missing data (Filter Missing)"
     "-9" = "Missing data (Not Ascertained)"
     "A"-"Z" = "Verbatim response"
 ;
 Value HadTesF
     -1 = "Inapplicable, coded 2 in HeardDNATest"
     -2 = "Question answered in error (Commission Error)"
     -5 = "Multiple responses selected in error"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing Data (Not Ascertained)"
     1 = "Paternity testing"
     10 = "Multiple types of genetic tests selected"
     2 = "Ancestry testing"
     3 = "DNA Fingerprinting"
     4 = "Cystic Fibrosis (CF) carrier testing"
     5 = "BRCA 1/2 testing"
     6 = "Lynch syndrome testing"
     7 = "None of the above"
     8 = "Not sure"
     91 = "Other (Specify)"
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
     4-6 = "Height, in feet"
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
 Value UseMenu
     -5 = "Multiple responses selected in error"
     -9 = "Missing data (Not Ascertained)"
     1 = "Always"
     2 = "Often"
     3 = "Sometimes"
     4 = "Rarely"
     5 = "Never"
 ;
 Value Fruit
     -5 = "Multiple responses selected in error"
     -9 = "Missing data (Not Ascertained)"
     0 = "None"
     1 = "½ cup or less"
     2 = "½ cup to 1 cup"
     3 = "1 to 2 cups"
     4 = "2 to 3 cups"
     5 = "3 to 4 cups"
     6 = "4 or more cups"
 ;
 Value Alcohol
     -5 = "Multiple responses selected in error"
     -9 = "Missing data (Not Ascertained)"
     1 = "Yes"
     2 = "No"
     3 = "Don't know"
 ;
 Value AlcohoF
     -5 = "Multiple responses selected in error"
     -9 = "Missing data (Not Ascertained)"
     1 = "Strongly agree"
     2 = "Somewhat agree"
     3 = "Somewhat disagree"
     4 = "Strongly disagree"
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
 Value HowLonF
     -1 = "Inapplicable, coded 0 in TimesModerateExercise"
     -2 = "Question answered in error (Commission Error)"
     -4 = "Unreadable or Non-conforming numeric response"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing data (Not Ascertained)"
     00-90 = "Length of exercise time, in minutes"
 ;
 Value HowLong
     -1 = "Inapplicable, coded 0 in TimesModerateExercise"
     -2 = "Question answered in error (Commission Error)"
     -4 = "Unreadable or Non-conforming numeric response"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing data (Not Ascertained)"
     00-24 = "Length of exercise time, in hours"
 ;
 Value TimesSt
     -1 = "Inapplicable, coded 0 in TimesModerateExercise"
     -2 = "Question answered in error (Commission Error)"
     -5 = "Multiple responses selected in error"
     -6 = "Missing data (Filter Missing)"
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
 Value Tanning
     -5 = "Multiple responses selected in error"
     -9 = "Missing data (Not Ascertained)"
     0 = "0 times"
     1 = "1 to 2 times"
     2 = "3 to 10 times"
     3 = "11 to 24 times"
     4 = "25 or more times"
 ;
 Value SkinCaF
     -5 = "Multiple responses selected in error"
     -9 = "Missing data (Not Ascertained)"
     1 = "Yes, but not regularly"
     2 = "No"
     3 = "Yes, regularly"
     4 = "I don't know"
 ;
 Value SkinCan
     -5 = "Multiple responses selected in error"
     -9 = "Missing data (Not Ascertained)"
     1 = "Yes, but not regularly"
     2 = "No"
     3 = "Yes, regularly"
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
 Value TriedQu
     -1 = "Inapplicable, coded 2 in Smoke100, or coded 3 in SmokeNow"
     -2 = "Question answered in error (Commission Error)"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing data (Not Ascertained)"
     1 = "Yes"
     2 = "No"
 ;
 Value ElectCi
     -9 = "Missing data (Not Ascertained)"
     1 = "Much less harmful"
     2 = "Less harmful"
     3 = "Just as harmful"
     4 = "More harmful"
     5 = "Much more harmful"
     6 = "I've never heard of electronic cigarettes"
 ;
 Value UsedECi
     -1 = "Inapplicable, coded 6 in ElectCigLessHarm"
     -2 = "Question answered in error (Commission Error)"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing data (Not Ascertained)"
     1 = "Yes"
     2 = "No"
 ;
 Value UseECig
     -1 = "Inapplicable, coded 6 in ElectCigLessHarm or coded 2 in UsedEcCigEver"
     -2 = "Question answered in error (Commission Error)"
     -5 = "Multiple responses selected in error"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing data (Not Ascertained)"
     1 = "Everyday"
     2 = "Some days"
     3 = "Not at all"
 ;
 Value Smokele
     -1 = "Inapplicable, this is a Short Form"
     -5 = "Multiple responses selected in error"
     -9 = "Missing data (Not Ascertained)"
     1 = "Yes"
     2 = "No"
     3 = "Don't know"
 ;
 Value HookahL
     -5 = "Multiple responses selected in error"
     -9 = "Missing data (Not Ascertained)"
     1 = "Much less harmful"
     2 = "Less harmful"
     3 = "Just as harmful"
     4 = "More harmful"
     5 = "Much more harmful"
     6 = "I've never heard of a hookah"
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
 Value EverHad
     -1 = "Inapplicable, coded 2 in GenderC"
     -2 = "Question answered in error (Commission Error)"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing data (Not Ascertained)"
     1 = "Yes"
     2 = "No"
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
 Value HPVSTD
     -1 = "Inapplicable,  coded 2 in HeardHPV"
     -2 = "Question answered in error (Commission Error)"
     -5 = "Multiple responses selected in error"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing data (Not Ascertained)"
     1 = "Yes"
     2 = "No"
     3 = "Not sure"
 ;
 Value HPVMedi
     -1 = "Inapplicable, coded 2 in HeardHPV"
     -2 = "Question answered in error (Commission Error)"
     -5 = "Multiple responses selected in error"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing data (Not Ascertained)"
     1 = "Requires medical treatment"
     2 = "Will usually go away on its own"
 ;
 Value HPVShot
     -1 = "Inapplicable, this is a Short Form"
     -5 = "Multiple responses selected in error"
     -9 = "Missing data (Not Ascertained)"
     1 = "Not at all successful"
     2 = "A little successful"
     3 = "Pretty successful"
     4 = "Very successful"
     5 = "Don't know"
 ;
 Value FamBetw
     -9 = "Missing data (Not Ascertained)"
     1 = "Yes"
     2 = "No"
 ;
 Value Discuss
     -1 = "Inapplicable, coded 1 in FamBetween9and27"
     -2 = "Question answered in error (Commission Error)"
     -5 = "Multiple responses selected in error"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing data (Not Ascertained)"
     1 = "Yes"
     2 = "No"
     3 = "Don't know"
 ;
 Value EverHaF
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
 Value Undergo
     -1 = "Inapplicable, coded 2 in EverHadCancer"
     -2 = "Question answered in error (Commission Error)"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing data (Not Ascertained)"
     1 = "Yes"
     2 = "No"
 ;
 Value CancerT
     -1 = "Inapplicable, coded 2 in EverHadCancer, or coded 2 in UndergoCancerTreatment"
     -2 = "Question answered in error (Commission Error)"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing data (Not Ascertained)"
     1 = "Yes"
     2 = "No"
 ;
 Value HowLonH
     -1 = "Inapplicable, coded 2 in EverHadCancer or coded 2 in UndergoCancerTreatment"
     -2 = "Question answered in error (Commission Error)"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing data (Not Ascertained)"
     1 = "Less than 1 year ago"
     2 = "1 year ago to less than 5 years ago"
     3 = "5 years ago to less than 10 years ago"
     4 = "10 or more years ago"
     5 = "Still receiving treatment"
 ;
 Value CancerF
     -1 = "Inapplicable, coded 2 in EverHadCancer, or coded 2 in UndergoCancerTreatment, or"
     -2 = "Question answered in error (Commission Error)"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing data (Not Ascertained)"
     1 = "Yes"
     2 = "No"
 ;
 Value CancerD
     -1 = "Inapplicable, coded 2 in EverHadCancer,or coded 5 in HowLongFinishTreatment_Cat"
     -2 = "Question answered in error (Commission Error)"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing data (Not Ascertained)"
     1 = "Yes"
     2 = "No"
 ;
 Value CancerH
     -1 = "Inapplicable, coded 2 in EverHadCancer, or coded 5 in HowLongFinishTreatment_Cat"
     -2 = "Question answered in error (Commission Error)"
     -5 = "Multiple responses selected in error"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing data (Not Ascertained)"
     1 = "Not at all"
     2 = "A litle"
     3 = "Some"
     4 = "A lot"
 ;
 Value CancerA
     -1 = "Inapplicable, coded 2 in EverHadCancer"
     -2 = "Question answered in error (Commission Error)"
     -5 = "Multiple responses selected in error"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing data (Not Ascertained)"
     1 = "Discussed it with me in detail"
     2 = "Briefly discussed it with me"
     3 = "Did not discuss it at all"
     4 = "I don't remember"
     5 = "I was not working at the time of my diagnosis"
 ;
 Value Clinica
     -1 = "Inapplicable, coded 2 in EverHadCancer"
     -2 = "Question answered in error (Commission Error)"
     -5 = "Multiple responses selected in error"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing data (Not Ascertained)"
     1 = "Yes"
     2 = "No"
     3 = "Don't know"
 ;
 Value DiscusF
     -1 = "Inapplicable, coded 2 in EverHadCancer"
     -2 = "Question answered in error (Commission Error)"
     -6 = "Missing data (Filter Missing)"
     -9 = "Missing data (Not Ascertained)"
     1 = "Yes"
     2 = "No"
 ;
 Value ChanceG
     -5 = "Multiple responses selected in error"
     -9 = "Missing data (Not Ascertained)"
     1 = "Very unlikely"
     2 = "Unlikely"
     3 = "Neither unlikely nor likely"
     4 = "Likely"
     5 = "Very likely"
 ;
 Value FreqWor
     -5 = "Multiple responses selected in error"
     -9 = "Missing data (Not Ascertained)"
     1 = "Not at all"
     2 = "Slightly"
     3 = "Somewhat"
     4 = "Moderately"
     5 = "Extremely"
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
     0000-2017 = "Year"
 ;
 Value SpeakEn
     -5 = "Multiple responses selected in error"
     -9 = "Missing data (Not Ascertained)"
     1 = "Very well"
     2 = "Well"
     3 = "Not well"
     4 = "Not at all"
 ;
 Value NotHisp
     -9 = "Missing data (Not Ascertained)"
     1 = "Selected"
     2 = "Not selected"
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
     00-22 = "Number of people"
 ;
 Value R_HHAdul
     . = "Missing value"
     01-10 = "Number of adults"
 ;
 Value HHAdults
     01-10 = "Number of adults"
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
     018-100 = "Age, in years"
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

value RaceEthn5f 
1 = "Non-Hispanic White"
2= "Non-Hispanic Black or African American"
3="Hispanic"
4="Non-Hispanic Asian"
5="Non-Hispanic Other"
-9="Missing Data--Not Ascertained"
;

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



 run;
