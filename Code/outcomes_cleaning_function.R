create_outcomes_data <- function(){
  
  if (!require("pacman")) install.packages("pacman")
  
  pacman::p_load(devtools, dplyr, tidyverse, tidyr, stringr,  curl)
  

  ####start####
  
  
  
  
  ####Children Outcomes####
  
  #2010
  #read in raw data with all cols as character so they can pivot together
  schl_absence <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2010_underlying/Absence2009_10.csv"),
                           colClasses = "character")%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name = X.1,
                  LA.Number = X,
                  pupils.n = Number.of.children.looked.after.continuously.for.12.months.at.31.March.2010.who.are.aged.5.to.15..attending.primary..secondary.or.special.schools,
                  sess_possible.n = Number.of.possible.sessions,
                  sess_authorised.n = Number.of.sessions.of.authorised.absence,
                  sess_unauthorised.n = Number.of.sessions.of.unauthorised.absence,
                  sess_overall.n = Number.of.sessions.of.overall.absence,
                  pupils_pa_10_exact.n = Number.of.children.looked.after.continuously.for.12.months.at.31.March.2010.who.are.aged.5.to.15..attending.primary..secondary.or.special.schools.classed.as.persistent.absentees)%>% # rename the id columns
    dplyr::select(LA_Name, LA.Number, pupils.n, sess_possible.n, sess_authorised.n,
                  sess_unauthorised.n, sess_overall.n, pupils_pa_10_exact.n)%>%
    dplyr::filter(LA.Number!="")%>%
    dplyr::mutate(pupils.pt = as.character((as.numeric(pupils.n)/as.numeric(pupils.n))*100),
                  sess_possible.pt = as.character((as.numeric(sess_possible.n)/as.numeric(sess_possible.n))*100),
                  sess_overall.pt = as.character((as.numeric(sess_overall.n)/as.numeric(sess_possible.n))*100),
                  sess_authorised.pt = as.character((as.numeric(sess_authorised.n)/as.numeric(sess_possible.n))*100),
                  sess_unauthorised.pt = as.character((as.numeric(sess_unauthorised.n)/as.numeric(sess_possible.n))*100),
                  pupils_pa_10_exact.pt = as.character((as.numeric(pupils_pa_10_exact.n)/as.numeric(pupils.n))*100))%>%
    tidyr::pivot_longer(cols = !c(LA_Name,LA.Number), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(\\w+)\\.(\\w+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "school absence")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  #schl_absence_by_school <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2010_underlying/Absence_Final_V2.csv"),
  #                                   colClasses = "character")%>%
  #  dplyr::filter(!is.na(LA.Number))%>%
  #  dplyr::rename(LA_Name = X)%>%
  #  tidyr::pivot_longer(cols = !c(LA.Number, LA_Name), names_to = "variable", 
  #                      values_to = "value")%>%
  #  dplyr::mutate(category = "child outcomes",
  #                subcategory = "school absence")
  
  
  schl_exclusion <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2012_underlying/SFR32_2012_TableLA10_Exclusions_UD.csv"),
                             colClasses = "character")[c(5,7,14:16)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name = la_name,
                  LA.Number = la_code,
                  pupils.n = Num_elig_2010,
                  perm_excl.n = Num_perm_ex_2010,
                  one_plus_sus.n = Num_fix_ex_2010)%>%
    dplyr::filter(LA_Name!="")%>%
    dplyr::mutate(pupils.pt = as.character((as.numeric(pupils.n)/as.numeric(pupils.n))*100),
                  perm_excl.pt = as.character((as.numeric(perm_excl.n)/as.numeric(pupils.n))*100),
                  one_plus_sus.pt = as.character((as.numeric(one_plus_sus.n)/as.numeric(pupils.n))*100))%>%
    tidyr::pivot_longer(cols = !c(LA_Name,LA.Number), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(\\w+)\\.(\\w+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "school exclusion")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  
  #ks1 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2010_underlying/KS1_Final_V2.csv"),
  #                colClasses = "character")%>%
  #  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  #  dplyr::filter(!is.na(LA.Number))%>%
  #  dplyr::rename(LA_Name = X)%>%
  #  tidyr::pivot_longer(cols = !c(LA.Number, LA_Name), names_to = "variable", 
  #                      values_to = "value")%>%
  #  dplyr::mutate(category = "child outcomes",
  #                subcategory = "key stage 1")
  
  ks2 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2012_underlying/SFR32_2012_TableLA2_KS2_UD.csv"),
                  colClasses = "character")[c(7,25:26)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name = la_name,
                  mat_eligible_pupils.n = Elig_maths_2010,
                  mat_met_expected_standard.n = Lev4_maths_2010)%>%
    dplyr::filter(LA_Name!="")%>%
    dplyr::mutate(#read_eligible_pupils.n = NA,
      #               read_eligible_pupils.pt = NA,
      #               writta_eligible_pupils.n = NA,
      #               writta_eligible_pupils.pt = NA,
      #               rwm_eligible_pupils.n = NA,
      #               rwm_eligible_pupils.pt = NA,
      #               gps_eligible_pupils.n = NA,
      #               gps_eligible_pupils.pt = NA,
      #               scita_eligible_pupils.n = NA,
      #               scita_eligible_pupils.pt = NA,
      #               read_met_expected_standard.n = NA,
      #               read_met_expected_standard.pt = NA,
      #               writta_met_expected_standard.n = NA,
      #               writta_met_expected_standard.pt = NA,
      #               rwm_met_expected_standard.n = NA,
      #               rwm_met_expected_standard.pt = NA,
      #               gps_met_expected_standard.n = NA,
      #               gps_met_expected_standard.pt = NA,
      #               scita_met_expected_standard.n = NA,
      #               scita_met_expected_standard.pt = NA,
      #               read_progress_eligible_pupils.n = NA,
      #               read_progress_eligible_pupils.pt = NA,
      #               writte_progress_eligible_pupils.n = NA,
      #               writte_progress_eligible_pupils.pt = NA,
      #               mat_progress_eligible_pupils.n = NA,
      #               mat_progress_eligible_pupils.pt = NA,
      #               read_progress_score.n = NA,
      #               read_progress_score.pt = NA,
      #               writta_progress_score.n = NA,
      #               writta_progress_score.pt = NA,
      #               mat_progress_score.n = NA,
      #               mat_progress_score.pt = NA,
      LA.Number=NA,
      mat_eligible_pupils.pt = as.character((as.numeric(mat_eligible_pupils.n)/as.numeric(mat_eligible_pupils.n))*100),
      mat_met_expected_standard.pt = as.character((as.numeric(mat_met_expected_standard.n)/as.numeric(mat_eligible_pupils.n))*100))%>%
    tidyr::pivot_longer(cols = !c(LA_Name, LA.Number), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(\\w+)\\.(\\w+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "key stage 2")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  
  
  # ks4 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2010_underlying/KS4_Final_V2.csv"),
  #                 colClasses = "character")%>%
  #   dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  #   dplyr::filter(!is.na(LA.Number))%>%
  #   dplyr::rename(LA_Name = X)%>%
  #   tidyr::pivot_longer(cols = !c(LA.Number, LA_Name), names_to = "variable", 
  #                       values_to = "value")%>%
  #   dplyr::mutate(category = "child outcomes",
  #                 subcategory = "key stage 4")
  
  
  oc2 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2010_underlying/OC2_Final_V2.csv"),
                  colClasses = "character")[c(1,2,39:49)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::filter(LA.Number!="")%>%
    dplyr::rename(LA_Name = X,
                  `Total all ages.n` = Number.of.children.looked.after.continuously.for.12.months.at.31.March.2010,
                  `Total ages 10 to 17 years.n` = Number.of.children.looked.after.continuously.for.12.months.aged.10.or.over.at.31.March,
                  `Total ages 0 to 4 years.n` = Number.of.children.looked.after.continuously.for.12.months.aged.5.or.under.at.31.March.2010,
                  `Convicted or subject to youth cautions or youth conditional cautions during the year.n` = Number.of.children.with.cautions.or.conviction ,
                  `Identified as having a substance misuse problem.n` = Number.of.children.identified.as.having.a.substance.misuse.problem,
                  `Received an intervention for their substance misuse problem.n` = Number.of.children.who.received.an.intervention.for.a.substance.misuse.problem,
                  `Offered intervention but refused it.n` = Number.of.children.offered.an.intervention.for.a.substance.problem.but.who.refused.it,
                  `Development assessments up to date.n`=Number.of.children.aged.5.or.under.who.development.assessment.were.up.to.date,
                  `Had their immunisations up to date.n` = Number.of.children.who.immunisations.were.up.to.date,
                  `Had their teeth checked by a dentist.n` = Number.of.children.whose.teeth.were.checked.by.a.dentist,
                  `Had their annual health assessment.n` = Number.of.children.whose.health.assessments.were.up.to.date)%>%
    dplyr::mutate(`Total all ages.pt` = "100",
                  `Total ages 10 to 17 years.pt` ="100", 
                  `Total ages 0 to 4 years.pt` = "100",
                  `Convicted or subject to youth cautions or youth conditional cautions during the year.pt` = as.character((as.numeric(`Convicted or subject to youth cautions or youth conditional cautions during the year.n`)/as.numeric(`Total ages 10 to 17 years.n`))*100),
                  `Identified as having a substance misuse problem.pt` = as.character((as.numeric(`Identified as having a substance misuse problem.n`)/as.numeric(`Total all ages.n`))*100),
                  `Received an intervention for their substance misuse problem.pt` = as.character((as.numeric(`Received an intervention for their substance misuse problem.n`)/as.numeric(`Identified as having a substance misuse problem.n`))*100),
                  `Offered intervention but refused it.pt` = as.character((as.numeric(`Offered intervention but refused it.n`)/as.numeric(`Identified as having a substance misuse problem.n`))*100),
                  `Development assessments up to date.pt`= as.character((as.numeric(`Development assessments up to date.n`)/as.numeric(`Total ages 0 to 4 years.n`))*100),
                  `Had their immunisations up to date.pt` = as.character((as.numeric(`Had their immunisations up to date.n`)/as.numeric(`Total all ages.n`))*100),
                  `Had their teeth checked by a dentist.pt` = as.character((as.numeric(`Had their teeth checked by a dentist.n`)/as.numeric(`Total all ages.n`))*100),
                  `Had their annual health assessment.pt` = as.character((as.numeric(`Had their annual health assessment.n`)/as.numeric(`Total all ages.n`))*100))%>%
    tidyr::pivot_longer(cols = !c(LA_Name, LA.Number), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(.+)\\.(.+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "health and criminalisation")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  
  #sen <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2010_underlying/SEN_Final_V2.csv"),
  #                colClasses = "character")%>%
  #  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  #  dplyr::filter(LA.Number!="")%>%
  #  dplyr::rename(LA_Name = X)%>%
  #  tidyr::pivot_longer(cols = !c(LA.Number, LA_Name), names_to = "variable", 
  #                      values_to = "value")%>%
  #  dplyr::mutate(category = "child outcomes",
  #                subcategory = "special educational needs")
  
  
  
  #bind together each outcome group
  outcomes_2010 <- rbind(schl_absence,  schl_exclusion,
                         ks2,  oc2)
  
  #allocate year variable
  outcomes_2010$year <- 2010
  
  #remove all unnecessary dataframes
  rm(list=setdiff(ls(), c("outcomes_2010")))
  
  #2011
  
  schl_absence <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2012_underlying/SFR32_2012_TableLA11_Absence_UD.csv"),
                           colClasses = "character")[c(5,7, 30:35)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name = la_name,
                  LA.Number = la_code,
                  pupils.n = FT_Schooling_2011,
                  sess_possible.n = Poss_sess_2011,
                  sess_authorised.n = Auth_ab_2011,
                  sess_unauthorised.n = Unauth_ab_2011,
                  sess_overall.n = Overall_ab_2011,
                  pupils_pa_10_exact.n = Persist_ab_2011)%>%
    dplyr::filter(LA.Number!="")%>%
    dplyr::mutate(pupils.pt = as.character((as.numeric(pupils.n)/as.numeric(pupils.n))*100),
                  sess_possible.pt = as.character((as.numeric(sess_possible.n)/as.numeric(sess_possible.n))*100),
                  sess_overall.pt = as.character((as.numeric(sess_overall.n)/as.numeric(sess_possible.n))*100),
                  sess_authorised.pt = as.character((as.numeric(sess_authorised.n)/as.numeric(sess_possible.n))*100),
                  sess_unauthorised.pt = as.character((as.numeric(sess_unauthorised.n)/as.numeric(sess_possible.n))*100),
                  pupils_pa_10_exact.pt = as.character((as.numeric(pupils_pa_10_exact.n)/as.numeric(pupils.n))*100))%>%
    pivot_longer(cols = !c(LA_Name,LA.Number), 
                 names_to = c("variable", ".value"),
                 names_pattern = "(\\w+)\\.(\\w+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "school absence")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  
  schl_exclusion <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2012_underlying/SFR32_2012_TableLA10_Exclusions_UD.csv"),
                             colClasses = "character")[c(5,7,17:19)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name = la_name,
                  LA.Number = la_code,
                  pupils.n = Num_elig_2011,
                  perm_excl.n = Num_perm_ex_2011,
                  one_plus_sus.n = Num_fix_ex_2011)%>%
    dplyr::filter(LA_Name!="")%>%
    dplyr::mutate(pupils.pt = as.character((as.numeric(pupils.n)/as.numeric(pupils.n))*100),
                  perm_excl.pt = as.character((as.numeric(perm_excl.n)/as.numeric(pupils.n))*100),
                  one_plus_sus.pt = as.character((as.numeric(one_plus_sus.n)/as.numeric(pupils.n))*100))%>%
    tidyr::pivot_longer(cols = !c(LA_Name,LA.Number), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(\\w+)\\.(\\w+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "school exclusion")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  
  #ks1 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2011_underlying/KS1_Attainment_2011_LA.csv"),
  #                colClasses = "character")%>%
  #  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  #  dplyr::rename(LA_Name = X,
  #                LA.Number = LA.Number)%>%
  #  dplyr::filter(!is.na(LA.Number))%>%
  #  tidyr::pivot_longer(cols = !c(LA.Number, LA_Name), names_to = "variable", 
  #                      values_to = "value")%>%
  #  dplyr::mutate(category = "child outcomes",
  #                subcategory = "ks1")
  
  
  ks2 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2012_underlying/SFR32_2012_TableLA2_KS2_UD.csv"),
                  colClasses = "character")[c(5,7,32:33)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name = la_name,
                  LA.Number = la_code,
                  mat_eligible_pupils.n = Elig_maths_2011,
                  mat_met_expected_standard.n = Lev4_maths_2011)%>%
    dplyr::filter(LA_Name!="")%>%
    dplyr::mutate(#read_eligible_pupils.n = NA,
      #               read_eligible_pupils.pt = NA,
      #               writta_eligible_pupils.n = NA,
      #               writta_eligible_pupils.pt = NA,
      #               rwm_eligible_pupils.n = NA,
      #               rwm_eligible_pupils.pt = NA,
      #               gps_eligible_pupils.n = NA,
      #               gps_eligible_pupils.pt = NA,
      #               scita_eligible_pupils.n = NA,
      #               scita_eligible_pupils.pt = NA,
      #               read_met_expected_standard.n = NA,
      #               read_met_expected_standard.pt = NA,
      #               writta_met_expected_standard.n = NA,
      #               writta_met_expected_standard.pt = NA,
      #               rwm_met_expected_standard.n = NA,
      #               rwm_met_expected_standard.pt = NA,
      #               gps_met_expected_standard.n = NA,
      #               gps_met_expected_standard.pt = NA,
      #               scita_met_expected_standard.n = NA,
      #               scita_met_expected_standard.pt = NA,
      #               read_progress_eligible_pupils.n = NA,
      #               read_progress_eligible_pupils.pt = NA,
      #               writte_progress_eligible_pupils.n = NA,
      #               writte_progress_eligible_pupils.pt = NA,
      #               mat_progress_eligible_pupils.n = NA,
      #               mat_progress_eligible_pupils.pt = NA,
      #               read_progress_score.n = NA,
      #               read_progress_score.pt = NA,
      #               writta_progress_score.n = NA,
      #               writta_progress_score.pt = NA,
      #               mat_progress_score.n = NA,
      #               mat_progress_score.pt = NA,
      mat_eligible_pupils.pt = as.character((as.numeric(mat_eligible_pupils.n)/as.numeric(mat_eligible_pupils.n))*100),
      mat_met_expected_standard.pt = as.character((as.numeric(mat_met_expected_standard.n)/as.numeric(mat_eligible_pupils.n))*100))%>%
    tidyr::pivot_longer(cols = !c(LA_Name, LA.Number), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(\\w+)\\.(\\w+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "key stage 2")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  
  # ks4 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2011_underlying/KS4_Attainment_2011_LA.csv"),
  #                          colClasses = "character")%>%
  #   dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  #   dplyr::rename(LA_Name = X,
  #                 LA.Number = LA.Number)%>%
  #   dplyr::filter(!is.na(LA.Number))%>%
  #   tidyr::pivot_longer(cols = !c(LA.Number, LA_Name), names_to = "variable", 
  #                       values_to = "value")%>%
  #   dplyr::mutate(category = "child outcomes",
  #                 subcategory = "ks4")
  
  
  oc2 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2011_underlying/OC2.csv"),
                  colClasses = "character")[-c(14:20)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name = X.1,
                  LA.Number = X,
                  `Total all ages.n` = Number.of.children.looked.after.continuously.for.12.months.at.31.March.2011,
                  `Total ages 10 to 17 years.n` = Number.of.children.looked.after.continuously.for.12.months.aged.10.or.over.at.31.March,
                  `Total ages 0 to 4 years.n` = Number.of.children.looked.after.continuously.for.12.months.aged.5.or.under.at.31.March.2011,
                  `Convicted or subject to youth cautions or youth conditional cautions during the year.n` = Number.of.children.looked.after.with.cautions.or.conviction,
                  `Identified as having a substance misuse problem.n` = Number.of.children.looked.after.identified.as.having.a.substance.misuse.problem,
                  `Received an intervention for their substance misuse problem.n` = Number.who.received.an.intervention.for.a.substance.misuse.problem,
                  `Offered intervention but refused it.n` = Number.who.were.offered.an.intervention.for.a.substance.problem.but.who.refused.it,
                  `Development assessments up to date.n`=Number.of.children.aged.5.or.under.who.development.assessment.were.up.to.date,
                  `Had their immunisations up to date.n` = Number.whose.immunisations.were.up.to.date,
                  `Had their teeth checked by a dentist.n` = Number.whose.teeth.were.checked.by.a.dentist,
                  `Had their annual health assessment.n` = Number.whose.health.assessments.were.up.to.date,
                  `SDQ average score.n` = Total.SDQ.score.received,
                  `Total ages 5 to 16 years.n` = Total.number.of.eligible.children.for.an.SDQ.score,
                  `SDQ score was received.n` = Number.of.eligible.children.with.an.SDQ.score)%>%
    dplyr::filter(LA.Number!="")%>%
    dplyr::mutate(`Total all ages.pt` = "100",
                  `Total ages 10 to 17 years.pt` ="100", 
                  `Total ages 0 to 4 years.pt` = "100",
                  `Total ages 5 to 16 years.pt` = "100",
                  `Convicted or subject to youth cautions or youth conditional cautions during the year.pt` = as.character((as.numeric(`Convicted or subject to youth cautions or youth conditional cautions during the year.n`)/as.numeric(`Total ages 10 to 17 years.n`))*100),
                  `Identified as having a substance misuse problem.pt` = as.character((as.numeric(`Identified as having a substance misuse problem.n`)/as.numeric(`Total all ages.n`))*100),
                  `Received an intervention for their substance misuse problem.pt` = as.character((as.numeric(`Received an intervention for their substance misuse problem.n`)/as.numeric(`Identified as having a substance misuse problem.n`))*100),
                  `Offered intervention but refused it.pt` = as.character((as.numeric(`Offered intervention but refused it.n`)/as.numeric(`Identified as having a substance misuse problem.n`))*100),
                  `Development assessments up to date.pt`= as.character((as.numeric(`Development assessments up to date.n`)/as.numeric(`Total ages 0 to 4 years.n`))*100),
                  `Had their immunisations up to date.pt` = as.character((as.numeric(`Had their immunisations up to date.n`)/as.numeric(`Total all ages.n`))*100),
                  `Had their teeth checked by a dentist.pt` = as.character((as.numeric(`Had their teeth checked by a dentist.n`)/as.numeric(`Total all ages.n`))*100),
                  `Had their annual health assessment.pt` = as.character((as.numeric(`Had their annual health assessment.n`)/as.numeric(`Total all ages.n`))*100),
                  `SDQ average score.pt` = as.character((as.numeric(`SDQ average score.n`)/as.numeric(`SDQ score was received.n`))*100),
                  `SDQ score was received.pt` = as.character((as.numeric(`SDQ score was received.n`)/as.numeric(`Total ages 5 to 16 years.n`))*100))%>%
    tidyr::pivot_longer(cols = !c(LA_Name, LA.Number), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(.+)\\.(.+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "health and criminalisation")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  oc22 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2013_LA/sheet7_oc24.csv"),
                   colClasses = "character", skip=5)[c(1,3,6,7,8)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name = X,
                  `SDQ score is a cause for concern.pt` = X.2,
                  `SDQ score is borderline.pt` = X.1,
                  `SDQ score is normal.pt` = Banded.SDQ.Score4,
                  `SDQ score was received.n` = Number.of.children.looked.after.for.at.least.12.months.aged.5.to.16.with.an.SDQ.score2)%>%
    dplyr::filter(LA_Name !="")%>%
    dplyr::mutate(LA.Number = NA,
                  `SDQ score is a cause for concern.n` = as.character((as.numeric(`SDQ score is a cause for concern.pt`)/100)*as.numeric(`SDQ score was received.n`)),
                  `SDQ score is normal.n` = as.character((as.numeric(`SDQ score is normal.pt`)/100)*as.numeric(`SDQ score was received.n`)),
                  `SDQ score is borderline.n` = as.character((as.numeric(`SDQ score is borderline.pt`)/100)*as.numeric(`SDQ score was received.n`))
    )%>%
    dplyr::select(-`SDQ score was received.n`)%>%
    tidyr::pivot_longer(cols = !c(LA_Name, LA.Number), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(.+)\\.(.+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "health and criminalisation")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  
  
  sen <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2011_underlying/SEN_2011_LA.csv"),
                  colClasses = "character")%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name = X,
                  LA.Number = LA.Number,
                  all_SEN.n = Number.with.SEN,
                  statement_or_EHC_plan.n = Number.with.statement)%>%
    dplyr::filter(!is.na(as.numeric(LA.Number)))%>%
    dplyr::mutate(pupils.n = as.character(as.numeric(all_SEN.n)+as.numeric(Number.with.No.SEN)),
                  pupils.pt = "100",
                  all_SEN.pt = as.character((as.numeric(all_SEN.n)/(as.numeric(all_SEN.n)+as.numeric(Number.with.No.SEN)))),
                  statement_or_EHC_plan.pt = as.character((as.numeric(statement_or_EHC_plan.n)/(as.numeric(all_SEN.n)+as.numeric(Number.with.No.SEN)))))%>%
    dplyr::select(-Number.with.No.SEN, -Number.without.statement )%>%
    tidyr::pivot_longer(cols = !c(LA_Name, LA.Number), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(.+)\\.(.+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "special educational needs")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  
  
  #bind together each outcome group
  outcomes_2011 <- rbind(schl_absence, schl_exclusion,
                         ks2,  oc2, oc22, sen)
  
  #allocate year variable
  outcomes_2011$year <- 2011
  
  #bind together previous years
  outcomes <- rbind(outcomes_2010, outcomes_2011)
  
  #remove all unnecessary dataframes
  rm(list=setdiff(ls(), c("outcomes")))
  
  
  #2012
  #read in raw data with all cols as character so they can pivot together
  schl_absence <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2012_underlying/SFR32_2012_TableLA11_Absence_UD.csv"),
                           colClasses = "character")[c(1:7, 37:42)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::select(-country_code, -country_name, -gor_code, -gor_name)%>% #remove unnecessary vars
    dplyr::rename(LA_Name = la_name,
                  LA.Number = la_code,
                  LA_Code = la_9digit,
                  pupils.n = FT_Schooling_2012,
                  sess_possible.n = Poss_sess_2012,
                  sess_authorised.n = Auth_ab_2012,
                  sess_unauthorised.n = Unauth_ab_2012,
                  sess_overall.n = Overall_ab_2012,
                  pupils_pa_10_exact.n = Persist_ab_2012)%>%#rename id variables
    dplyr::filter(LA.Number!="")%>%
    dplyr::mutate(pupils.pt = as.character((as.numeric(pupils.n)/as.numeric(pupils.n))*100),
                  sess_possible.pt = as.character((as.numeric(sess_possible.n)/as.numeric(sess_possible.n))*100),
                  sess_overall.pt = as.character((as.numeric(sess_overall.n)/as.numeric(sess_possible.n))*100),
                  sess_authorised.pt = as.character((as.numeric(sess_authorised.n)/as.numeric(sess_possible.n))*100),
                  sess_unauthorised.pt = as.character((as.numeric(sess_unauthorised.n)/as.numeric(sess_possible.n))*100),
                  pupils_pa_10_exact.pt = as.character((as.numeric(pupils_pa_10_exact.n)/as.numeric(pupils.n))*100))%>%
    pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                 names_to = c("variable", ".value"),
                 names_pattern = "(\\w+)\\.(\\w+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "school absence")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  schl_exclusion <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2013_LA/sheet9_excluded.csv"),
                             colClasses = "character", skip=5)[c(1,25,27,28)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name = X,
                  pupils.n = Number.of.children.matched.to.the.school.census4.3,
                  perm_excl.pt = Percentage.of.children.permanently.excluded.3,
                  one_plus_sus.pt = Percentage.of.children.with.at.least.one.fixed.term.exclusion.3)%>%
    dplyr::filter(LA_Name!="")%>%
    dplyr::mutate(LA.Number = NA,
                  LA_Code = NA,
                  pupils.pt = as.character((as.numeric(pupils.n)/as.numeric(pupils.n))*100),
                  perm_excl.n = as.character((as.numeric(perm_excl.pt)/100)*as.numeric(pupils.n)),
                  one_plus_sus.n = as.character((as.numeric(one_plus_sus.pt)/100)*as.numeric(pupils.n)))%>%
    tidyr::pivot_longer(cols = !c(LA_Name, LA.Number, LA_Code), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(\\w+)\\.(\\w+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "school exclusion")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  oc2 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2012_underlying/SFR32_2012_OC2_LA_Tables_UD.csv"),
                  colClasses = "character")[c(5:18,24:28)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name = la_name,
                  LA.Number = la_code,
                  LA_Code = la_9digit,
                  `Total all ages.n` = CLA_12_months_2012,
                  `Total ages 10 to 17 years.n` = CLA_12_months_2012_10plus,
                  `Total ages 0 to 4 years.n` = CLA_12_months_2012_5under,
                  `Convicted or subject to youth cautions or youth conditional cautions during the year.n` = caution_conviction,
                  `Identified as having a substance misuse problem.n` = substance_misuse,
                  `Received an intervention for their substance misuse problem.n` = substance_intervention,
                  `Offered intervention but refused it.n` = refused_intervention,
                  `Development assessments up to date.n`=dev_assess,
                  `Had their immunisations up to date.n` = immunisations,
                  `Had their teeth checked by a dentist.n` = teeth_check,
                  `Had their annual health assessment.n` = health_assess,
                  `SDQ score is a cause for concern.n` = SDQ_concern,
                  `SDQ score is borderline.n` = SDQ_borderline,
                  `SDQ score is normal.n` = SDQ_normal,
                  `Total ages 5 to 16 years.n` = elig_SDQ,
                  `SDQ score was received.n` = SDQ_score_rec)%>%
    dplyr::filter(LA_Code!="")%>%
    dplyr::mutate(`Total all ages.pt` = "100",
                  `Total ages 10 to 17 years.pt` ="100", 
                  `Total ages 0 to 4 years.pt` = "100",
                  `Total ages 5 to 16 years.pt` = "100",
                  `Convicted or subject to youth cautions or youth conditional cautions during the year.pt` = as.character((as.numeric(`Convicted or subject to youth cautions or youth conditional cautions during the year.n`)/as.numeric(`Total ages 10 to 17 years.n`))*100),
                  `Identified as having a substance misuse problem.pt` = as.character((as.numeric(`Identified as having a substance misuse problem.n`)/as.numeric(`Total all ages.n`))*100),
                  `Received an intervention for their substance misuse problem.pt` = as.character((as.numeric(`Received an intervention for their substance misuse problem.n`)/as.numeric(`Identified as having a substance misuse problem.n`))*100),
                  `Offered intervention but refused it.pt` = as.character((as.numeric(`Offered intervention but refused it.n`)/as.numeric(`Identified as having a substance misuse problem.n`))*100),
                  `Development assessments up to date.pt`= as.character((as.numeric(`Development assessments up to date.n`)/as.numeric(`Total ages 0 to 4 years.n`))*100),
                  `Had their immunisations up to date.pt` = as.character((as.numeric(`Had their immunisations up to date.n`)/as.numeric(`Total all ages.n`))*100),
                  `Had their teeth checked by a dentist.pt` = as.character((as.numeric(`Had their teeth checked by a dentist.n`)/as.numeric(`Total all ages.n`))*100),
                  `Had their annual health assessment.pt` = as.character((as.numeric(`Had their annual health assessment.n`)/as.numeric(`Total all ages.n`))*100),
                  `SDQ score was received.pt` = as.character((as.numeric(`SDQ score was received.n`)/as.numeric(`Total ages 5 to 16 years.n`))*100),
                  `SDQ score is normal.pt` = as.character((as.numeric(`SDQ score is normal.n`)/as.numeric(`Total ages 5 to 16 years.n`))*100),
                  `SDQ score is a cause for concern.pt` = as.character((as.numeric(`SDQ score is a cause for concern.n`)/as.numeric(`Total ages 5 to 16 years.n`))*100),
                  `SDQ score is borderline.pt` = as.character((as.numeric(`SDQ score is borderline.n`)/as.numeric(`Total ages 5 to 16 years.n`))*100),
    )%>%
    tidyr::pivot_longer(cols = !c(LA_Name, LA.Number, LA_Code), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(.+)\\.(.+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "health and criminalisation")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  oc22 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2013_LA/sheet7_oc24.csv"),
                   colClasses = "character", skip=5)[c(1,11,13)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name = X,
                  yes = Number.of.children.looked.after.for.at.least.12.months.aged.5.to.16.with.an.SDQ.score2.1,
                  percent = Average.score.per.child3.1)%>%
    dplyr::filter(LA_Name!="")%>%
    dplyr::mutate(LA_Code = NA,
                  LA.Number = NA,
                  number = as.character((as.numeric(percent))*as.numeric(yes)),
                  variable = "SDQ average score",
                  category = "child outcomes",
                  subcategory = "health and criminalisation")%>%
    dplyr::select(-yes)
  
  
  #ks1 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2012_underlying/SFR32_2012_TableLA1_KS1_UD.csv"),
  #                colClasses = "character")%>%
  #  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  #  dplyr::select(-country_code, country_name, gor_code, gor_name)%>% #remove unnecessary vars
  #  dplyr::rename(LA_Name = la_name,
  #                LA.Number = la_code,
  #                LA_Code = la_9digit)%>% #rename id variables
  #  dplyr::filter(!is.na(LA_Name))%>% #keep only LAs
  #  tidyr::pivot_longer(cols = !c(LA.Number, LA_Name, LA_Code), names_to = "variable", 
  #                      values_to = "value")%>% #pivot so variables go in one column
  #  dplyr::mutate(category = "child outcomes",
  #                subcategory = "ks1") #create categories and subcategories
  
  
  ks2 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2012_underlying/SFR32_2012_TableLA2_KS2_UD.csv"),
                  colClasses = "character")[c(5:7,39:40)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name = la_name,
                  LA.Number = la_code,
                  LA_Code = la_9digit,
                  mat_eligible_pupils.n = Elig_maths_2012,
                  mat_met_expected_standard.n = Lev4_maths_2012)%>%
    dplyr::filter(LA_Name!="")%>%
    dplyr::mutate(#read_eligible_pupils.n = NA,
      #               read_eligible_pupils.pt = NA,
      #               writta_eligible_pupils.n = NA,
      #               writta_eligible_pupils.pt = NA,
      #               rwm_eligible_pupils.n = NA,
      #               rwm_eligible_pupils.pt = NA,
      #               gps_eligible_pupils.n = NA,
      #               gps_eligible_pupils.pt = NA,
      #               scita_eligible_pupils.n = NA,
      #               scita_eligible_pupils.pt = NA,
      #               read_met_expected_standard.n = NA,
      #               read_met_expected_standard.pt = NA,
      #               writta_met_expected_standard.n = NA,
      #               writta_met_expected_standard.pt = NA,
      #               rwm_met_expected_standard.n = NA,
      #               rwm_met_expected_standard.pt = NA,
      #               gps_met_expected_standard.n = NA,
      #               gps_met_expected_standard.pt = NA,
      #               scita_met_expected_standard.n = NA,
      #               scita_met_expected_standard.pt = NA,
      #               read_progress_eligible_pupils.n = NA,
      #               read_progress_eligible_pupils.pt = NA,
      #               writte_progress_eligible_pupils.n = NA,
      #               writte_progress_eligible_pupils.pt = NA,
      #               mat_progress_eligible_pupils.n = NA,
      #               mat_progress_eligible_pupils.pt = NA,
      #               read_progress_score.n = NA,
      #               read_progress_score.pt = NA,
      #               writta_progress_score.n = NA,
      #               writta_progress_score.pt = NA,
      #               mat_progress_score.n = NA,
      #               mat_progress_score.pt = NA,
      mat_eligible_pupils.pt = as.character((as.numeric(mat_eligible_pupils.n)/as.numeric(mat_eligible_pupils.n))*100),
      mat_met_expected_standard.pt = as.character((as.numeric(mat_met_expected_standard.n)/as.numeric(mat_eligible_pupils.n))*100))%>%
    tidyr::pivot_longer(cols = !c(LA_Name, LA.Number, LA_Code), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(\\w+)\\.(\\w+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "key stage 2")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  # ks4 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2012_underlying/SFR32_2012_TableLA3_KS4_UD.csv"),
  #                 colClasses = "character")%>%
  #   dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  #   dplyr::select(-country_code, country_name, gor_code, gor_name)%>% #remove unnecessary vars
  #   dplyr::rename(LA_Name = la_name,
  #                 LA.Number = la_code,
  #                 LA_Code = la_9digit)%>% #rename id variables
  #   dplyr::filter(!is.na(LA_Name))%>% #keep only LAs
  #   tidyr::pivot_longer(cols = !c(LA.Number, LA_Name, LA_Code), names_to = "variable", 
  #                       values_to = "value")%>% #pivot so variables go in one column
  #   dplyr::mutate(category = "child outcomes",
  #                 subcategory = "ks4") #create categories and subcategories
  
  
  sen <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2012_underlying/SFR32_2012_TableLA9_SEN_UD.csv"),
                  colClasses = "character")%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::select(-country_code, -country_name, -gor_code, -gor_name)%>% #remove unnecessary vars
    dplyr::rename(LA_Name = la_name,
                  LA.Number = la_code,
                  LA_Code = la_9digit,
                  all_SEN.n = SEN,
                  statement_or_EHC_plan.n = with_statement,
                  pupils.n = num_school_age_child)%>%
    dplyr::filter(!is.na(as.numeric(LA.Number)))%>%
    dplyr::mutate(pupils.pt = "100",
                  all_SEN.pt = as.character((as.numeric(all_SEN.n)/(as.numeric(pupils.n)))),
                  statement_or_EHC_plan.pt = as.character((as.numeric(statement_or_EHC_plan.n)/(as.numeric(pupils.n)))))%>%
    dplyr::select(-Without_statement, -No_SEN )%>%
    tidyr::pivot_longer(cols = !c(LA_Name, LA.Number,LA_Code), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(.+)\\.(.+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "special educational needs")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  
  #bind together each outcome group
  outcomes_2012 <- rbind(schl_absence, schl_exclusion,
                         ks2,  oc2,oc22, sen)
  
  #allocate year variable
  outcomes_2012$year <- 2012
  
  #create missing variable for la codes in old data
  outcomes$LA_Code <- NA
  
  #bind together previous years
  outcomes <- rbind(outcomes, outcomes_2012)
  
  #remove all unnecessary dataframes
  rm(list=setdiff(ls(), c("outcomes")))
  
  
  
  #2013 
  #read in raw data with all cols as character so they can pivot together, skip empty rows
  # ks1 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2013_LA/sheet1_ks1.csv"),
  #                 colClasses = "character", skip=4)%>%
  #   dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  #   dplyr::rename(LA_Name= X,
  #                 percent_level_24_reading = Percentage.achieving.at.least.Level.24.in.the.following.,
  #                 percent_level_24_writing = X.2,
  #                 percent_level_24_maths = X.3)%>% #rename variables
  #   dplyr::select(LA_Name,Percentage.with.UPN2, Number.eligible.for.Key.Stage.1.assessments3,percent_level_24_reading, percent_level_24_writing, percent_level_24_maths)%>% #remove empty column
  #   tidyr::pivot_longer(cols=!LA_Name, names_to = "variable", 
  #                       values_to = "value")%>% #pivot so variables go in one column
  #   dplyr::mutate(category = "child outcomes",
  #                 subcategory = "ks1") #create categories and subcategories)
  
  
  ks2 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2013_LA/sheet2_ks2.csv"),
                  colClasses = "character", skip=5)[c(1,28:33)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name= X,
                  read_eligible_pupils.n = Number.eligible.to.sit.Key.Stage.2.tasks.and.tests4.4,
                  mat_met_expected_standard.pt = Percentage.who.achieved.at.least.Level.45.in.the.following..4,
                  writta_met_expected_standard.pt = X.16,
                  read_met_expected_standard.pt = X.15,
                  gps_met_expected_standard.pt = X.17)%>% #rename variables
    dplyr::select(-X.14)%>% #remove empty column
    dplyr::filter(LA_Name!="")%>%
    dplyr::mutate(read_eligible_pupils.pt = "100",
                  writta_eligible_pupils.n = read_eligible_pupils.n,
                  writta_eligible_pupils.pt = "100",
                  mat_eligible_pupils.n = read_eligible_pupils.n,
                  mat_eligible_pupils.pt = "100",
                  #rwm_eligible_pupils.n = NA,
                  #rwm_eligible_pupils.pt = NA,
                  gps_eligible_pupils.n = read_eligible_pupils.n,
                  gps_eligible_pupils.pt = "100",
                  #scita_eligible_pupils.n = NA,
                  #scita_eligible_pupils.pt = NA,
                  read_met_expected_standard.n = as.character((as.numeric(read_met_expected_standard.pt)/100)*as.numeric(read_eligible_pupils.n)),
                  writta_met_expected_standard.n =  as.character((as.numeric(writta_met_expected_standard.pt)/100)*as.numeric(writta_eligible_pupils.n)),
                  #rwm_met_expected_standard.n = NA,
                  #rwm_met_expected_standard.pt = NA,
                  gps_met_expected_standard.n = as.character((as.numeric(gps_met_expected_standard.pt)/100)*as.numeric(gps_eligible_pupils.n)),
                  # scita_met_expected_standard.n = NA,
                  # scita_met_expected_standard.pt = NA,
                  # read_progress_eligible_pupils.n = NA,
                  # read_progress_eligible_pupils.pt = NA,
                  # writte_progress_eligible_pupils.n = NA,
                  # writte_progress_eligible_pupils.pt = NA,
                  # mat_progress_eligible_pupils.n = NA,
                  # mat_progress_eligible_pupils.pt = NA,
                  # read_progress_score.n = NA,
                  # read_progress_score.pt = NA,
                  # writta_progress_score.n = NA,
                  # writta_progress_score.pt = NA,
                  # mat_progress_score.n = NA,
                  # mat_progress_score.pt = NA,
                  mat_met_expected_standard.n = as.character((as.numeric(mat_met_expected_standard.pt)/100)*as.numeric(mat_eligible_pupils.n)))%>%
    tidyr::pivot_longer(cols = !c(LA_Name), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(\\w+)\\.(\\w+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "key stage 2")%>%
    dplyr::rename(number=n,
                  percent=pt)
  # 
  # ks4 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2013_LA/sheet3_ks4.csv"),
  #                colClasses = "character", skip=5)[c(1,30:35)]%>%
  #   dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  #   dplyr::rename(LA_Name= X,
  #                 Five_GCSEs = X.19,
  #                 Five_GCSEs_inc_Eng_Mat = X.20,
  #                 GCSES_Eng_Mat = X.21)%>% #rename variables
  #   dplyr::select(-X.18)%>% #remove empty column
  #   tidyr::pivot_longer(cols=!LA_Name, names_to = "variable", 
  #                       values_to = "value")%>% #pivot so variables go in one column
  #   dplyr::mutate(category = "child outcomes",
  #                 subcategory = "ks4") #create categories and subcategories)
  
  oc21 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2013_LA/sheet4_oc21.csv"),
                   colClasses = "character", skip=4)[c(1,6,7)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name= X,
                  number = Looked.after.children.aged.10.and.above,
                  percent = X.3  )%>% #rename variables
    dplyr::filter(LA_Name!="")%>%
    dplyr::mutate(variable = "Convicted or subject to youth cautions or youth conditional cautions during the year",
                  category = "child outcomes",
                  subcategory = "health and criminalisation") #create categories and subcategories)
  
  oc22 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2013_LA/sheet4_oc21.csv"),
                   colClasses = "character", skip=4)[c(1,4)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name= X,
                  number = Number.of.looked.after.children.aged.10.or.older.at.31.March3)%>% #rename variables
    dplyr::filter(LA_Name!="")%>%
    dplyr::mutate(percent = "100",
                  variable = "Total ages 10 to 17 years",
                  category = "child outcomes",
                  subcategory = "health and criminalisation") #create categories and subcategories
  
  oc23 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2013_LA/sheet5_oc22.csv"),
                   colClasses = "character", skip=4)[c(1,2,4,5,6,8)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name= X, 
                  `Total all ages.n` = Number.of.children.looked.after.at.31.March.who.had.been.looked.after.for.at.least.12.months1,
                  `Identified as having a substance misuse problem.n` = Number.identified.as.having.a.substance.misuse.problem.during.the.year2,
                  `Received an intervention for their substance misuse problem.n` = Number.who.received.an.intervention.for.their.substance.misuse.problem.during.the.year3,
                  `Offered intervention but refused it.n` = Number.who.were.offered.an.intervention.but.who.refused.it.during.the.year4,
                  `Identified as having a substance misuse problem.pt` = Percentage.identified.as.having.a.substance.misuse.problem.during.the.year
    )%>% #rename variables
    dplyr::filter(LA_Name != "")%>% #remove empty rows
    dplyr::mutate(`Total all ages.pt` = "100",
                  `Received an intervention for their substance misuse problem.pt` = as.character((as.numeric(`Received an intervention for their substance misuse problem.n`)/as.numeric(`Identified as having a substance misuse problem.n`))*100),
                  `Offered intervention but refused it.pt` = as.character((as.numeric(`Offered intervention but refused it.n`)/as.numeric(`Identified as having a substance misuse problem.n`))*100)
    )%>%
    tidyr::pivot_longer(cols = !c(LA_Name), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(.+)\\.(.+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "health and criminalisation")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  oc24 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2013_LA/sheet6_oc23.csv"),
                   colClasses = "character", skip=4)[c(1,2,4,5,6,8,9)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name= X,
                  `Total all ages.n` = Number.of.children.looked.after.at.31.March.who.had.been.looked.after.for.at.least.12.months1,
                  `Total ages 0 to 4 years.n` = Number.of.children.who.had.been.looked.after.for.at.least.12.months..and.aged.5.or.younger.at.31.March.2013,
                  `Development assessments up to date.n`=Number.of.these.children.whose.development.assessments.were.up.to.date5,
                  `Had their immunisations up to date.n` = Number.of.children.whose.immunisations.were.up.to.date2,
                  `Had their teeth checked by a dentist.n` = Number.of.children.who.had.their.teeth.checked.by.a.dentist3,
                  `Had their annual health assessment.n` = Number.of.children.who.had.their.annual.health.assessment4)%>% #rename variables
    dplyr::filter(LA_Name!="")%>% #remove empty rows
    dplyr::mutate(`Total all ages.pt` = "100",
                  `Total ages 0 to 4 years.pt` = "100",
                  `Development assessments up to date.pt`= as.character((as.numeric(`Development assessments up to date.n`)/as.numeric(`Total ages 0 to 4 years.n`))*100),
                  `Had their immunisations up to date.pt` = as.character((as.numeric(`Had their immunisations up to date.n`)/as.numeric(`Total all ages.n`))*100),
                  `Had their teeth checked by a dentist.pt` = as.character((as.numeric(`Had their teeth checked by a dentist.n`)/as.numeric(`Total all ages.n`))*100),
                  `Had their annual health assessment.pt` = as.character((as.numeric(`Had their annual health assessment.n`)/as.numeric(`Total all ages.n`))*100)
    )%>%
    tidyr::pivot_longer(cols = !c(LA_Name), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(.+)\\.(.+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "health and criminalisation")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  oc25 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2013_LA/sheet7_oc24.csv"),
                   colClasses = "character", skip=5)[c(1, 18:24)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name= X,
                  `SDQ score is a cause for concern.pt` = X.8,
                  `SDQ score is borderline.pt` = X.7,
                  `SDQ score is normal.pt` = Banded.SDQ.Score4.2,
                  `Total ages 5 to 16 years.n` = Number.of.children.looked.after.for.at.least.12.months.aged.5.to.161.2,
                  `SDQ score was received.n` = Number.of.children.looked.after.for.at.least.12.months.aged.5.to.16.with.an.SDQ.score2.2,
                  `SDQ score was received.pt` = Percentage.of.children.for.whom.an.SDQ.score.was.submitted.2,
                  `SDQ average score.pt`= Average.score.per.child3.2)%>% #rename variables
    dplyr::filter(LA_Name!="")%>% #remove empty rows
    dplyr::mutate(`Total ages 5 to 16 years.pt` = "100",
                  `SDQ score is a cause for concern.n` = as.character((as.numeric(`SDQ score is a cause for concern.pt`)/100)*as.numeric(`SDQ score was received.n`)) ,
                  `SDQ score is borderline.n` = as.character((as.numeric(`SDQ score is borderline.pt`)/100)*as.numeric(`SDQ score was received.n`)) ,
                  `SDQ score is normal.n` = as.character((as.numeric(`SDQ score is normal.pt`)/100)*as.numeric(`SDQ score was received.n`)) ,
                  `SDQ average score.n` = as.character((as.numeric(`SDQ average score.pt`)/100)*as.numeric(`SDQ score was received.n`)))%>%
    tidyr::pivot_longer(cols = !c(LA_Name), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(.+)\\.(.+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "health and criminalisation")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  sen <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2013_LA/sheet8_sen.csv"),
                  colClasses = "character", skip=4)[c(1,3,11,12,14,15)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name= X,
                  all_SEN.n = All.children.with.SEN6,
                  all_SEN.pt = X.10,
                  statement_or_EHC_plan.n = X.7,
                  statement_or_EHC_plan.pt = X.8,
                  pupils.n = Number.of.children.looked.after.at.31.March.who.had.been.continuously.looked.after.for.at.least.12.months.and.matched.to.census.data3)%>%
    dplyr::filter(LA_Name!="")%>%
    dplyr::mutate(pupils.pt = "100")%>%
    tidyr::pivot_longer(cols = !c(LA_Name), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(.+)\\.(.+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "special educational needs")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  schl_exclusion <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2014_LA/sheet9_excluded.csv"),
                             colClasses = "character", skip=5)[c(1,25:28)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name = X,
                  pupils.n = of.these..3,
                  perm_excl.pt = Percentage.of.children.permanently.excluded.3,
                  one_plus_sus.pt = Percentage.of.children.with.at.least.one.fixed.exclusion.3)%>%
    dplyr::filter(LA_Name!="")%>%
    dplyr::select(-X.11)%>%
    dplyr::mutate(pupils.pt = as.character((as.numeric(pupils.n)/as.numeric(pupils.n))*100),
                  perm_excl.n = as.character((as.numeric(perm_excl.pt)/100)*as.numeric(pupils.n)),
                  one_plus_sus.n = as.character((as.numeric(one_plus_sus.pt)/100)*as.numeric(pupils.n)))%>%
    tidyr::pivot_longer(cols = !c(LA_Name), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(\\w+)\\.(\\w+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "school exclusion")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  
  schl_absence <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2013_LA/sheet10_absence.csv"),
                           colClasses = "character", skip=5)[c(1,45:51)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name= X,
                  pupils.n = Number.of.looked.after.children.matched.to.absence.data5.4,
                  sess_authorised.pt = Percentage.of.sessions.lost.due.to6..4,
                  sess_unauthorised.pt = X.28,
                  sess_overall.pt = X.29,
                  pupils_pa_10_exact.pt = Percentage.of.looked.after.children.classed.as.persistent.absentees7.4)%>% #rename variables
    dplyr::select(-X.27, -X.30)%>% #remove empty column
    dplyr::filter(LA_Name!="")%>%
    dplyr::mutate(pupils.pt = as.character((as.numeric(pupils.n)/as.numeric(pupils.n))*100),
                  sess_possible.pt = NA,
                  sess_possible.n = NA,
                  sess_overall.n = NA,
                  sess_authorised.n = NA,
                  sess_unauthorised.n = NA,
                  pupils_pa_10_exact.n = as.character((as.numeric(pupils_pa_10_exact.pt)/100)*as.numeric(pupils.n)))%>%
    pivot_longer(cols = !c(LA_Name), 
                 names_to = c("variable", ".value"),
                 names_pattern = "(\\w+)\\.(\\w+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "school absence")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  
  
  
  
  #bind together each outcome group
  outcomes_2013 <- rbind(schl_absence,schl_exclusion ,oc21,oc22,oc23,oc24, oc25,
                         ks2,   sen)
  
  #allocate year variable
  outcomes_2013$year <- 2013
  
  #create missing variable for la codes in old data
  outcomes_2013$LA_Code <- NA
  outcomes_2013$LA.Number <- NA
  
  #bind together previous years
  outcomes <- rbind(outcomes, outcomes_2013)
  
  #remove all unnecessary dataframes
  rm(list=setdiff(ls(), c("outcomes")))
  
  
  #2014
  #read in raw data with all cols as character so they can pivot together, skip empty rows
  #ks1 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2014_LA/sheet1_ks1.csv"),
  #                colClasses = "character", skip=4)%>%
  #  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  #  dplyr::rename(LA_Name= X,
  #                percent_level_24_reading = Percentage.who.achieved.at.least.Level.24.in.the.following.,
  #                percent_level_24_writing = X.3,
  #                percent_level_24_maths = X.4)%>% #rename variables
  #  dplyr::select(LA_Name,Percentage.with.UPN2, Number.eligible.to.sit.Key.Stage.1.tasks.and.tests3,percent_level_24_reading, percent_level_24_writing, percent_level_24_maths)%>% #remove empty column
  #  tidyr::pivot_longer(cols=!LA_Name, names_to = "variable", 
  #                      values_to = "value")%>% #pivot so variables go in one column
  #  dplyr::mutate(category = "child outcomes",
  #                subcategory = "ks1") #create categories and subcategories)
  
  
  ks2 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2014_LA/sheet2_ks2.csv"),
                  colClasses = "character", skip=5)[c(1,32:38)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name= X,
                  read_eligible_pupils.n = Number.eligible.to.sit.Key.Stage.2.tasks.and.tests4.3,
                  mat_met_expected_standard.pt = Percentage.who.achieved.at.least.Level.46.in.the.following..4,
                  writta_met_expected_standard.pt = X.20,
                  read_met_expected_standard.pt = X.19,
                  gps_met_expected_standard.pt = X.21,
                  rwm_met_expected_standard.pt = X.22)%>% #rename variables
    dplyr::select(-X.18)%>% #remove empty column
    dplyr::filter(LA_Name!="")%>%
    dplyr::mutate(read_eligible_pupils.pt = "100",
                  writta_eligible_pupils.n = read_eligible_pupils.n,
                  writta_eligible_pupils.pt = "100",
                  mat_eligible_pupils.n = read_eligible_pupils.n,
                  mat_eligible_pupils.pt = "100",
                  rwm_eligible_pupils.n = read_eligible_pupils.n,
                  rwm_eligible_pupils.pt = "100",
                  gps_eligible_pupils.n = read_eligible_pupils.n,
                  gps_eligible_pupils.pt = "100",
                  #scita_eligible_pupils.n = NA,
                  #scita_eligible_pupils.pt = NA,
                  read_met_expected_standard.n = as.character((as.numeric(read_met_expected_standard.pt)/100)*as.numeric(read_eligible_pupils.n)),
                  writta_met_expected_standard.n =  as.character((as.numeric(writta_met_expected_standard.pt)/100)*as.numeric(writta_eligible_pupils.n)),
                  rwm_met_expected_standard.n = as.character((as.numeric(rwm_met_expected_standard.pt)/100)*as.numeric(rwm_eligible_pupils.n)),
                  gps_met_expected_standard.n = as.character((as.numeric(gps_met_expected_standard.pt)/100)*as.numeric(gps_eligible_pupils.n)),
                  # scita_met_expected_standard.n = NA,
                  # scita_met_expected_standard.pt = NA,
                  # read_progress_eligible_pupils.n = NA,
                  # read_progress_eligible_pupils.pt = NA,
                  # writte_progress_eligible_pupils.n = NA,
                  # writte_progress_eligible_pupils.pt = NA,
                  # mat_progress_eligible_pupils.n = NA,
                  # mat_progress_eligible_pupils.pt = NA,
                  # read_progress_score.n = NA,
                  # read_progress_score.pt = NA,
                  # writta_progress_score.n = NA,
                  # writta_progress_score.pt = NA,
                  # mat_progress_score.n = NA,
                  # mat_progress_score.pt = NA,
                  mat_met_expected_standard.n = as.character((as.numeric(mat_met_expected_standard.pt)/100)*as.numeric(mat_eligible_pupils.n)))%>%
    tidyr::pivot_longer(cols = !c(LA_Name), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(\\w+)\\.(\\w+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "key stage 2")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  # ks4 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2014_LA/sheet3_ks4.csv"),
  #                 colClasses = "character", skip=5)[c(1,37:42)]%>%
  #   dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  #   dplyr::rename(LA_Name= X,
  #                 Five_GCSEs = Percentage.achieving..5,
  #                 Five_GCSEs_inc_Eng_Mat = X.22,
  #                 GCSES_Eng_Mat = X.23)%>% #rename variables
  #   dplyr::select(-X.21)%>% #remove empty column
  #   tidyr::pivot_longer(cols=!LA_Name, names_to = "variable", 
  #                       values_to = "value")%>% #pivot so variables go in one column
  #   dplyr::mutate(category = "child outcomes",
  #                 subcategory = "ks4") #create categories and subcategories)
  # 
  
  oc21 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2014_LA/sheet4_oc21.csv"),
                   colClasses = "character", skip=4)[c(1,6,7)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name= X,
                  number = Looked.after.children.aged.10.and.above,
                  percent = X.3)%>% #rename variables
    dplyr::filter(LA_Name!="")%>%
    dplyr::mutate(variable = "Convicted or subject to youth cautions or youth conditional cautions during the year",
                  category = "child outcomes",
                  subcategory = "health and criminalisation") #create categories and subcategories)
  
  
  oc22 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2014_LA/sheet4_oc21.csv"),
                   colClasses = "character", skip=4)[c(1,4)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name= X,
                  number = Number.of.looked.after.children.aged.10.or.older.at.31.March3)%>% #rename variables
    dplyr::filter(LA_Name!="")%>%
    dplyr::mutate(percent = "100",
                  variable = "Total ages 10 to 17 years",
                  category = "child outcomes",
                  subcategory = "health and criminalisation") #create categories and subcategories
  
  
  oc23 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2014_LA/sheet5_oc22.csv"),
                   colClasses = "character", skip=4)[c(1,2,4,5,6,8)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name= X, 
                  `Total all ages.n` = Number.of.children.looked.after.at.31.March.who.had.been.looked.after.for.at.least.12.months1,
                  `Identified as having a substance misuse problem.n` = Number.identified.as.having.a.substance.misuse.problem.during.the.year2,
                  `Received an intervention for their substance misuse problem.n` = Number.who.received.an.intervention.for.their.substance.misuse.problem.during.the.year3,
                  `Offered intervention but refused it.n` = Number.who.were.offered.an.intervention.but.who.refused.it.during.the.year4,
                  `Identified as having a substance misuse problem.pt` = Percentage.identified.as.having.a.substance.misuse.problem.during.the.year2
    )%>% #rename variables
    dplyr::filter(LA_Name != "")%>% #remove empty rows
    dplyr::mutate(`Total all ages.pt` = "100",
                  `Received an intervention for their substance misuse problem.pt` = as.character((as.numeric(`Received an intervention for their substance misuse problem.n`)/as.numeric(`Identified as having a substance misuse problem.n`))*100),
                  `Offered intervention but refused it.pt` = as.character((as.numeric(`Offered intervention but refused it.n`)/as.numeric(`Identified as having a substance misuse problem.n`))*100)
    )%>%
    tidyr::pivot_longer(cols = !c(LA_Name), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(.+)\\.(.+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "health and criminalisation")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  oc24 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2014_LA/sheet6_oc23.csv"),
                   colClasses = "character", skip=4)[c(1,2,4,5,6,8,9)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name= X,
                  `Total all ages.n` = Number.of.children.looked.after.at.31.March.who.had.been.looked.after.for.at.least.12.months1,
                  `Total ages 0 to 4 years.n` = Number.of.children.who.had.been.looked.after.for.at.least.12.months..and.aged.5.or.younger.at.31.March.2014,
                  `Development assessments up to date.n`=Number.of.these.children.whose.development.assessments.were.up.to.date5,
                  `Had their immunisations up to date.n` = Number.of.children.whose.immunisations.were.up.to.date2,
                  `Had their teeth checked by a dentist.n` = Number.of.children.who.had.their.teeth.checked.by.a.dentist3,
                  `Had their annual health assessment.n` = Number.of.children.who.had.their.annual.health.assessment4)%>% #rename variables
    dplyr::filter(LA_Name!="")%>% #remove empty rows
    dplyr::mutate(`Total all ages.pt` = "100",
                  `Total ages 0 to 4 years.pt` = "100",
                  `Development assessments up to date.pt`= as.character((as.numeric(`Development assessments up to date.n`)/as.numeric(`Total ages 0 to 4 years.n`))*100),
                  `Had their immunisations up to date.pt` = as.character((as.numeric(`Had their immunisations up to date.n`)/as.numeric(`Total all ages.n`))*100),
                  `Had their teeth checked by a dentist.pt` = as.character((as.numeric(`Had their teeth checked by a dentist.n`)/as.numeric(`Total all ages.n`))*100),
                  `Had their annual health assessment.pt` = as.character((as.numeric(`Had their annual health assessment.n`)/as.numeric(`Total all ages.n`))*100)
    )%>%
    tidyr::pivot_longer(cols = !c(LA_Name), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(.+)\\.(.+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "health and criminalisation")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  
  oc25 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2013_LA/sheet7_oc24.csv"),
                   colClasses = "character", skip=5)[c(1, 18:24)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name= X,
                  `SDQ score is a cause for concern.pt` = X.8,
                  `SDQ score is borderline.pt` = X.7,
                  `SDQ score is normal.pt` = Banded.SDQ.Score4.2,
                  `Total ages 5 to 16 years.n` = Number.of.children.looked.after.for.at.least.12.months.aged.5.to.161.2,
                  `SDQ score was received.n` = Number.of.children.looked.after.for.at.least.12.months.aged.5.to.16.with.an.SDQ.score2.2,
                  `SDQ score was received.pt` = Percentage.of.children.for.whom.an.SDQ.score.was.submitted.2,
                  `SDQ average score.pt`= Average.score.per.child3.2)%>% #rename variables
    dplyr::filter(LA_Name!="")%>% #remove empty rows
    dplyr::mutate(`Total ages 5 to 16 years.pt` = "100",
                  `SDQ score is a cause for concern.n` = as.character((as.numeric(`SDQ score is a cause for concern.pt`)/100)*as.numeric(`SDQ score was received.n`)) ,
                  `SDQ score is borderline.n` = as.character((as.numeric(`SDQ score is borderline.pt`)/100)*as.numeric(`SDQ score was received.n`)) ,
                  `SDQ score is normal.n` = as.character((as.numeric(`SDQ score is normal.pt`)/100)*as.numeric(`SDQ score was received.n`)) ,
                  `SDQ average score.n` = as.character((as.numeric(`SDQ average score.pt`)/100)*as.numeric(`SDQ score was received.n`)))%>%
    tidyr::pivot_longer(cols = !c(LA_Name), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(.+)\\.(.+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "health and criminalisation")%>%
    dplyr::rename(number=n,
                  percent=pt)
  sen <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2014_LA/sheet8_sen.csv"),
                  colClasses = "character", skip=4)[c(1,3,11,12,14,15)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name= X,
                  all_SEN.n = All.children.with.SEN6,
                  all_SEN.pt = X.10,
                  statement_or_EHC_plan.n = X.7,
                  statement_or_EHC_plan.pt = X.8,
                  pupils.n = Number.of.children.looked.after.at.31.March.who.had.been.continuously.looked.after.for.at.least.12.months.and.matched.to.census.data3)%>%
    dplyr::filter(LA_Name!="")%>%
    dplyr::mutate(pupils.pt = "100")%>%
    tidyr::pivot_longer(cols = !c(LA_Name), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(.+)\\.(.+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "special educational needs")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  sch_exclusion <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2015_LA/sheet5_excluded.csv"),
                            colClasses = "character", skip=6)[c(3,34:37)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name = X.2,
                  pupils.n = of.these..4,
                  perm_excl.pt = Percentage.of.children.permanently.excluded.4,
                  one_plus_sus.pt = Percentage.of.children.with.at.least.one.fixed.period.exclusion.4)%>%
    dplyr::filter(LA_Name!="")%>%
    dplyr::select(-X.16)%>%
    dplyr::mutate(pupils.pt = as.character((as.numeric(pupils.n)/as.numeric(pupils.n))*100),
                  perm_excl.n = as.character((as.numeric(perm_excl.pt)/100)*as.numeric(pupils.n)),
                  one_plus_sus.n = as.character((as.numeric(one_plus_sus.pt)/100)*as.numeric(pupils.n)))%>%
    tidyr::pivot_longer(cols = !c(LA_Name), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(\\w+)\\.(\\w+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "school exclusion")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  
  schl_absence <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2014_LA/sheet10_absence.csv"),
                           colClasses = "character", skip=5)[c(1,41:47)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name= X,
                  pupils.n = Number.of.looked.after.children.matched.to.absence.data5.4,
                  sess_authorised.pt = Percentage.of.sessions.missed.due.to6..4,
                  sess_unauthorised.pt = X.26,
                  sess_overall.pt = X.27,
                  pupils_pa_10_exact.pt = Percentage.of.looked.after.children.classed.as.persistent.absentees8.1)%>% #rename variables
    dplyr::select(-X.25, -X.28)%>% #remove empty column
    dplyr::filter(LA_Name!="")%>%
    dplyr::mutate(pupils.pt = as.character((as.numeric(pupils.n)/as.numeric(pupils.n))*100),
                  sess_possible.pt = NA,
                  sess_possible.n = NA,
                  sess_overall.n = NA,
                  sess_authorised.n = NA,
                  sess_unauthorised.n = NA,
                  pupils_pa_10_exact.n = as.character((as.numeric(pupils_pa_10_exact.pt)/100)*as.numeric(pupils.n)))%>%
    pivot_longer(cols = !c(LA_Name), 
                 names_to = c("variable", ".value"),
                 names_pattern = "(\\w+)\\.(\\w+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "school absence")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  #bind together each outcome group
  outcomes_2014 <- rbind(schl_absence,sch_exclusion,oc21,oc22,oc23,oc24, oc25,
                         ks2,   sen)
  
  #allocate year variable
  outcomes_2014$year <- 2014
  
  #create missing variable for la codes in old data
  outcomes_2014$LA_Code <- NA
  outcomes_2014$LA.Number <- NA
  
  #bind together previous years
  outcomes <- rbind(outcomes, outcomes_2014)
  
  #remove all unnecessary dataframes
  rm(list=setdiff(ls(), c("outcomes")))
  
  
  
  
  
  #2015
  #read in raw data with all cols as character so they can pivot together, skip empty rows
  # ks1 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2015_LA/sheet1_ks1.csv"),
  #                 colClasses = "character", skip=5)%>%
  #   dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  #   dplyr::rename(LA_Name= X,
  #                 LA.Number=X.1,
  #                 LA_Code = LA.Code,
  #                 percent_level_24_reading = Percentage.who.achieved.at.least.level.24.in.the.following.,
  #                 percent_level_24_writing = X.4,
  #                 percent_level_24_maths = X.5)%>% #rename variables
  #   dplyr::select(LA_Name,LA_Code,LA.Number,Percentage.with.UPN2, Number.eligible.to.sit..key.stage.1.tasks.and.tests3,percent_level_24_reading, percent_level_24_writing, percent_level_24_maths)%>% #remove empty column
  #   tidyr::pivot_longer(cols=!c(LA_Name, LA_Code,LA.Number), names_to = "variable", 
  #                       values_to = "value")%>% #pivot so variables go in one column
  #   dplyr::mutate(category = "child outcomes",
  #                 subcategory = "ks1") #create categories and subcategories)
  
  
  ks2 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2015_LA/sheet2_ks2.csv"),
                  colClasses = "character", skip=6)[c(1:3,37:43)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name= X.2,
                  LA.Number = X.1,
                  LA_Code = X,
                  read_eligible_pupils.n = Number.eligible.to.sit.key.stage.2.tasks.and.tests4.4,
                  mat_met_expected_standard.pt = Percentage.who.achieved.at.least.level.45.in.the.following..4,
                  writta_met_expected_standard.pt = X.25,
                  read_met_expected_standard.pt = X.24,
                  gps_met_expected_standard.pt = X.26,
                  rwm_met_expected_standard.pt = X.27)%>% #rename variables
    dplyr::select(-X.23)%>% #remove empty column
    dplyr::filter(LA_Name!="")%>%
    dplyr::mutate(read_eligible_pupils.pt = "100",
                  writta_eligible_pupils.n = read_eligible_pupils.n,
                  writta_eligible_pupils.pt = "100",
                  mat_eligible_pupils.n = read_eligible_pupils.n,
                  mat_eligible_pupils.pt = "100",
                  rwm_eligible_pupils.n = read_eligible_pupils.n,
                  rwm_eligible_pupils.pt = "100",
                  gps_eligible_pupils.n = read_eligible_pupils.n,
                  gps_eligible_pupils.pt = "100",
                  #scita_eligible_pupils.n = NA,
                  #scita_eligible_pupils.pt = NA,
                  read_met_expected_standard.n = as.character((as.numeric(read_met_expected_standard.pt)/100)*as.numeric(read_eligible_pupils.n)),
                  writta_met_expected_standard.n =  as.character((as.numeric(writta_met_expected_standard.pt)/100)*as.numeric(writta_eligible_pupils.n)),
                  rwm_met_expected_standard.n = as.character((as.numeric(rwm_met_expected_standard.pt)/100)*as.numeric(rwm_eligible_pupils.n)),
                  gps_met_expected_standard.n = as.character((as.numeric(gps_met_expected_standard.pt)/100)*as.numeric(gps_eligible_pupils.n)),
                  # scita_met_expected_standard.n = NA,
                  # scita_met_expected_standard.pt = NA,
                  # read_progress_eligible_pupils.n = NA,
                  # read_progress_eligible_pupils.pt = NA,
                  # writte_progress_eligible_pupils.n = NA,
                  # writte_progress_eligible_pupils.pt = NA,
                  # mat_progress_eligible_pupils.n = NA,
                  # mat_progress_eligible_pupils.pt = NA,
                  # read_progress_score.n = NA,
                  # read_progress_score.pt = NA,
                  # writta_progress_score.n = NA,
                  # writta_progress_score.pt = NA,
                  # mat_progress_score.n = NA,
                  # mat_progress_score.pt = NA,
                  mat_met_expected_standard.n = as.character((as.numeric(mat_met_expected_standard.pt)/100)*as.numeric(mat_eligible_pupils.n)))%>%
    tidyr::pivot_longer(cols = !c(LA_Name, LA.Number, LA_Code), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(\\w+)\\.(\\w+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "key stage 2")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  
  
  
  # ks4 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2015_LA/sheet3_ks4.csv"),
  #                 colClasses = "character", skip=6)[c(1:3,32:37)]%>%
  #   dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  #   dplyr::rename(LA_Name= X.2,
  #                 LA.Number = X.1,
  #                 LA_Code = X,
  #                 Five_GCSEs = Percentage.achieving..4,
  #                 Five_GCSEs_inc_Eng_Mat = X.20,
  #                 GCSES_Eng_Mat = X.21)%>% #rename variables
  #   dplyr::select(-X.19)%>% #remove empty column
  #   tidyr::pivot_longer(cols=!c(LA_Name, LA_Code,LA.Number), names_to = "variable", 
  #                       values_to = "value")%>% #pivot so variables go in one column
  #   dplyr::mutate(category = "child outcomes",
  #                 subcategory = "ks4") #create categories and subcategories)
  
  
  
  oc2 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2015_LA/SFR34_OC22015.csv"),
                  colClasses = "character")%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::filter(geog_l=="LA")%>%
    dplyr::select(-geog_l)%>% #remove empty column
    dplyr::rename(LA_Name= geog_n,
                  LA.Number=geog_c,
                  LA_Code=New_geog_code,
                  `Total all ages.n` = CLA_12mths,
                  `Total ages 10 to 17 years.n` = CLA_10over,
                  `Total ages 0 to 4 years.n` = CLA_5under,
                  `Convicted or subject to youth cautions or youth conditional cautions during the year.n` = CLA_convicted,
                  `Identified as having a substance misuse problem.n` = CLA_submisuse,
                  `Received an intervention for their substance misuse problem.n` = CLA_subint,
                  `Offered intervention but refused it.n` = CLA_suboffint,
                  `Development assessments up to date.n`=CLA_devassmt,
                  `Had their immunisations up to date.n` = CLA_immunisation,
                  `Had their teeth checked by a dentist.n` = CLA_teethcheck,
                  `Had their annual health assessment.n` = CLA_healthassmt,
                  `SDQ score is a cause for concern.n` = CLA_SDQconcern,
                  `SDQ score is borderline.n` = CLA_SDQborderline,
                  `SDQ score is normal.n` = CLA_SDQnormal,
                  `Total ages 5 to 16 years.n` = CLA_5to16,
                  `SDQ score was received.n` = CLA_SDQ) %>%
    dplyr::filter(LA_Code!="")%>%
    dplyr::mutate(`Total all ages.pt` = "100",
                  `Total ages 10 to 17 years.pt` ="100", 
                  `Total ages 0 to 4 years.pt` = "100",
                  `Total ages 5 to 16 years.pt` = "100",
                  `Convicted or subject to youth cautions or youth conditional cautions during the year.pt` = as.character((as.numeric(`Convicted or subject to youth cautions or youth conditional cautions during the year.n`)/as.numeric(`Total ages 10 to 17 years.n`))*100),
                  `Identified as having a substance misuse problem.pt` = as.character((as.numeric(`Identified as having a substance misuse problem.n`)/as.numeric(`Total all ages.n`))*100),
                  `Received an intervention for their substance misuse problem.pt` = as.character((as.numeric(`Received an intervention for their substance misuse problem.n`)/as.numeric(`Identified as having a substance misuse problem.n`))*100),
                  `Offered intervention but refused it.pt` = as.character((as.numeric(`Offered intervention but refused it.n`)/as.numeric(`Identified as having a substance misuse problem.n`))*100),
                  `Development assessments up to date.pt`= as.character((as.numeric(`Development assessments up to date.n`)/as.numeric(`Total ages 0 to 4 years.n`))*100),
                  `Had their immunisations up to date.pt` = as.character((as.numeric(`Had their immunisations up to date.n`)/as.numeric(`Total all ages.n`))*100),
                  `Had their teeth checked by a dentist.pt` = as.character((as.numeric(`Had their teeth checked by a dentist.n`)/as.numeric(`Total all ages.n`))*100),
                  `Had their annual health assessment.pt` = as.character((as.numeric(`Had their annual health assessment.n`)/as.numeric(`Total all ages.n`))*100),
                  `SDQ score was received.pt` = as.character((as.numeric(`SDQ score was received.n`)/as.numeric(`Total ages 5 to 16 years.n`))*100),
                  `SDQ score is normal.pt` = as.character((as.numeric(`SDQ score is normal.n`)/as.numeric(`Total ages 5 to 16 years.n`))*100),
                  `SDQ score is a cause for concern.pt` = as.character((as.numeric(`SDQ score is a cause for concern.n`)/as.numeric(`Total ages 5 to 16 years.n`))*100),
                  `SDQ score is borderline.pt` = as.character((as.numeric(`SDQ score is borderline.n`)/as.numeric(`Total ages 5 to 16 years.n`))*100),
    )%>%
    tidyr::pivot_longer(cols = !c(LA_Name, LA.Number, LA_Code), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(.+)\\.(.+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "health and criminalisation")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  oc22 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2015_LA/sheet_extra_sdq.csv"),
                   colClasses = "character", skip=6)[c(1,2,20,22)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name = X.1,
                  LA.Number = X,
                  yes = Number.of.children.looked.after.for.at.least.12.months.aged.5.to.16.with.an.SDQ.score2.2,
                  percent = Average.score.per.child3.2)%>%
    dplyr::filter(LA.Number!="")%>%
    dplyr::mutate(LA_Code = NA,
                  number = as.character((as.numeric(percent))*as.numeric(yes)),
                  variable = "SDQ average score",
                  category = "child outcomes",
                  subcategory = "health and criminalisation")%>%
    dplyr::select(-yes)
  
  
  
  sen <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2015_LA/sheet4_sen.csv"),
                  colClasses = "character", skip=5)[c(1:3,5,13,14,16,17)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name= X.1,
                  LA_Code=LA.Code,
                  LA.Number=X,
                  all_SEN.n = Total.with.SEN6,
                  all_SEN.pt = X.11,
                  statement_or_EHC_plan.n = X.8,
                  statement_or_EHC_plan.pt = X.9,
                  pupils.n = Number.of.children.looked.after.at.31.March.who.had.been.continuously.looked.after.for.at.least.twelve.months.and.matched.to.census.data3)%>%
    dplyr::filter(LA_Code!="")%>%
    dplyr::mutate(pupils.pt = "100")%>%
    tidyr::pivot_longer(cols = !c(LA_Name, LA.Number, LA_Code), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(.+)\\.(.+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "special educational needs")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  sch_exclusion <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2016_LA/sheet9_excluded.csv"),
                            colClasses = "character", skip=6)[c(1:3,24:27)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Code = X,
                  LA.Number = X.1,
                  LA_Name= X.2,
                  pupils.n = Number.matched.to.the.school.census3.4,
                  perm_excl.pt = Percentage.of.children.permanently.excluded.4,
                  one_plus_sus.pt = Percentage.of.children.with.at.least.one.fixed.period.exclusion.4)%>%
    dplyr::filter(LA_Name!="")%>%
    dplyr::select(-X.11)%>%
    dplyr::mutate(pupils.pt = as.character((as.numeric(pupils.n)/as.numeric(pupils.n))*100),
                  perm_excl.n = as.character((as.numeric(perm_excl.pt)/100)*as.numeric(pupils.n)),
                  one_plus_sus.n = as.character((as.numeric(one_plus_sus.pt)/100)*as.numeric(pupils.n)))%>%
    tidyr::pivot_longer(cols = !c(LA_Name, LA_Code, LA.Number), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(\\w+)\\.(\\w+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "school exclusion")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  
  schl_absence <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2015_LA/sheet6_absence.csv"),
                           colClasses = "character", skip=6)[c(1:3,27:33)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name= X.2,
                  LA_Code=X.1,
                  LA.Number=X,
                  pupils.n = Number.of.looked.after.children.matched.to.absence.data5.2,
                  sess_overall.pt = Percentage.of.sessions.missed.due.to6..2,
                  sess_authorised.pt = X.18,
                  sess_unauthorised.pt = X.19,
                  pupils_pa_10_exact.pt = Percentage.of.looked.after.children.classed.as.persistent.absentees7.2)%>% #rename variables
    dplyr::select( -X.17, -X.20)%>% #remove empty column
    dplyr::filter(LA_Name!="")%>%
    dplyr::mutate(pupils.pt = as.character((as.numeric(pupils.n)/as.numeric(pupils.n))*100),
                  sess_possible.pt = NA,
                  sess_possible.n = NA,
                  sess_overall.n = NA,
                  sess_authorised.n = NA,
                  sess_unauthorised.n = NA,
                  pupils_pa_10_exact.n = as.character((as.numeric(pupils_pa_10_exact.pt)/100)*as.numeric(pupils.n)))%>%
    pivot_longer(cols = !c(LA_Name, LA_Code, LA.Number), 
                 names_to = c("variable", ".value"),
                 names_pattern = "(\\w+)\\.(\\w+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "school absence")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  
  
  #bind together each outcome group
  outcomes_2015 <- rbind(schl_absence,sch_exclusion,oc2, oc22,
                         ks2,   sen)
  
  #allocate year variable
  outcomes_2015$year <- 2015
  
  #create missing variable for la codes in old data
  # outcomes_2014$LA_Code <- NA
  # outcomes_2014$LA.Number <- NA
  
  #bind together previous years
  outcomes <- rbind(outcomes, outcomes_2015)
  
  #remove all unnecessary dataframes
  rm(list=setdiff(ls(), c("outcomes")))
  
  
  
  #2016
  
  #read in raw data with all cols as character so they can pivot together, skip empty rows
  # ks1 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2016_LA/sheet1_ks1.csv"),
  #                 colClasses = "character", skip=5)%>%
  #   dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  #   dplyr::rename(LA_Name= X.1,
  #                 LA.Number=X,
  #                 LA_Code = LA.Code,
  #                 percent_level_24_reading = Reaching.the.expected.standard4....,
  #                 percent_level_24_writing = X.3,
  #                 percent_level_24_maths = X.4,
  #                 percent_level_24_science = X.5)%>% #rename variables
  #   dplyr::select(-X.2, -X.6)%>% #remove empty column
  #   tidyr::pivot_longer(cols=!c(LA_Name, LA_Code,LA.Number), names_to = "variable", 
  #                       values_to = "value")%>% #pivot so variables go in one column
  #   dplyr::mutate(category = "child outcomes",
  #                 subcategory = "ks1") #create categories and subcategories)
  
  
  ks2 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2016_LA/sheet2_ks2.csv"),
                  colClasses = "character", skip=5)[c(1:10)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name= X.1,
                  LA.Number = X,
                  LA_Code = LA.Code,
                  read_eligible_pupils.n = Number.eligible.for.key.stage.2.tests.and.assessments3.4.5,
                  mat_met_expected_standard.pt = Reaching.the.expected.standard....,
                  writta_met_expected_standard.pt = X.4,
                  read_met_expected_standard.pt = X.3,
                  gps_met_expected_standard.pt = X.5,
                  rwm_met_expected_standard.pt = X.6)%>% #rename variables
    dplyr::select(-X.2)%>% #remove empty column
    dplyr::filter(LA_Code!="")%>%
    dplyr::mutate(read_eligible_pupils.pt = "100",
                  writta_eligible_pupils.n = read_eligible_pupils.n,
                  writta_eligible_pupils.pt = "100",
                  mat_eligible_pupils.n = read_eligible_pupils.n,
                  mat_eligible_pupils.pt = "100",
                  rwm_eligible_pupils.n = read_eligible_pupils.n,
                  rwm_eligible_pupils.pt = "100",
                  gps_eligible_pupils.n = read_eligible_pupils.n,
                  gps_eligible_pupils.pt = "100",
                  #scita_eligible_pupils.n = NA,
                  #scita_eligible_pupils.pt = NA,
                  read_met_expected_standard.n = as.character((as.numeric(read_met_expected_standard.pt)/100)*as.numeric(read_eligible_pupils.n)),
                  writta_met_expected_standard.n =  as.character((as.numeric(writta_met_expected_standard.pt)/100)*as.numeric(writta_eligible_pupils.n)),
                  rwm_met_expected_standard.n = as.character((as.numeric(rwm_met_expected_standard.pt)/100)*as.numeric(rwm_eligible_pupils.n)),
                  gps_met_expected_standard.n = as.character((as.numeric(gps_met_expected_standard.pt)/100)*as.numeric(gps_eligible_pupils.n)),
                  #scita_met_expected_standard.n = NA,
                  #scita_met_expected_standard.pt = NA,
                  mat_met_expected_standard.n = as.character((as.numeric(mat_met_expected_standard.pt)/100)*as.numeric(mat_eligible_pupils.n)))%>%
    tidyr::pivot_longer(cols = !c(LA_Name, LA.Number, LA_Code), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(\\w+)\\.(\\w+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "key stage 2")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  
  ks22 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2016_LA/sheet3_ks22.csv"),
                   colClasses = "character", skip=6)%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name= X.1,
                  LA.Number = X,
                  LA_Code = LA.Code,
                  read_progress_eligible_pupils.n = Reading.Progress.Score4,
                  writta_progress_eligible_pupils.n = Writing.Progress.Score4,
                  mat_progress_eligible_pupils.n = Mathematics.Progress.Score4,
                  read_progress_score.pt = X.2,
                  writta_progress_score.pt = X.6,
                  mat_progress_score.pt = X.10)%>% #rename variables
    dplyr::select(-X.3,-X.4,-X.5,-X.7,-X.8,-X.9, -X.11, -X.12, -X.13, -X.14,
                  -X.15, -X.16, -X.17, -X.18, -X.19)%>% #remove empty column
    dplyr::filter(LA.Number!="")%>%
    dplyr::mutate(read_progress_eligible_pupils.pt = "100",
                  writte_progress_eligible_pupils.pt = "100",
                  mat_progress_eligible_pupils.pt = "100",
                  read_progress_score.n = as.character((as.numeric(read_progress_score.pt))*as.numeric(read_progress_eligible_pupils.n)),
                  writta_progress_score.n = as.character((as.numeric(writta_progress_score.pt))*as.numeric(writta_progress_eligible_pupils.n)),
                  mat_progress_score.n = as.character((as.numeric(mat_progress_score.pt))*as.numeric(mat_progress_eligible_pupils.n)))%>%
    tidyr::pivot_longer(cols = !c(LA_Name, LA.Number, LA_Code), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(\\w+)\\.(\\w+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "key stage 2")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  
  
  # ks4 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2016_LA/sheet4_ks41.csv"),
  #                 colClasses = "character", skip=6)[c(1:3,26:29)]%>%
  #   dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  #   dplyr::rename(LA_Name= X.1,
  #                 LA.Number = X,
  #                 LA_Code = LA.Code,
  #                 pupils.n = Number.at.the.end.of.Key.Stage.4.4,
  #                 GCSES_Eng_Mat = Percentage.of.pupils..4,
  #                 Eng_bacc_percent_enter = X.16,
  #                 Eng_bacc_percent_achieve = X.17)%>% #rename variables
  #   dplyr::filter(LA_Code!="")%>%
  #   #dplyr::select(-X.19)%>% #remove empty column
  #   tidyr::pivot_longer(cols=!c(LA_Name, LA_Code,LA.Number), names_to = "variable", 
  #                       values_to = "value")%>% #pivot so variables go in one column
  #   dplyr::mutate(category = "child outcomes",
  #                 subcategory = "ks4") #create categories and subcategories)
  
  
  ks42 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2016_LA/sheet5_ks42.csv"),
                   colClasses = "character", skip=5)[c(1:5)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name= X.1,
                  LA.Number = X,
                  LA_Code = LA.Code,
                  pupils.n = Number.at.the.end.of.Key.Stage.4,
                  att8.pt = Average.Attainment.8.score.per.pupil3)%>% #rename variables
    dplyr::filter(LA_Code!="")%>%
    dplyr::mutate(pupils.pt="100",
                  att8.n = as.character(as.numeric(att8.pt)*as.numeric(pupils.n)))%>%
    tidyr::pivot_longer(cols = !c(LA_Name, LA.Number, LA_Code), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(\\w+)\\.(\\w+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "key stage 4")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  
  ks43 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2016_LA/sheet6_ks43.csv"),
                   colClasses = "character", skip=5)[c(1:5)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name= X.1,
                  LA.Number = X,
                  LA_Code = LA.Code,
                  inp8calc.n = Number.of.pupils.included,
                  p8score.pt = Average.Progress.8.score4.5)%>% #rename variables
    dplyr::filter(LA_Code!="")%>%
    dplyr::mutate(inp8calc.pt="100",
                  p8score.n = as.character(as.numeric(p8score.pt)*as.numeric(inp8calc.n)))%>%
    tidyr::pivot_longer(cols = !c(LA_Name, LA.Number, LA_Code), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(\\w+)\\.(\\w+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "key stage 4")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  
  
  oc2 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2016_LA/SFR41_OC22016.csv"),
                  colClasses = "character")%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::filter(geog_l=="LA")%>%
    dplyr::select(-geog_l)%>% #remove empty column
    dplyr::rename(LA_Name= geog_n,
                  LA.Number=geog_c,
                  LA_Code=New_geog_code,
                  `Total all ages.n` = CLA_12mths,
                  `Total ages 10 to 17 years.n` = CLA_10over,
                  `Total ages 0 to 4 years.n` = CLA_under5,
                  `Convicted or subject to youth cautions or youth conditional cautions during the year.n` = CLA_convicted,
                  `Identified as having a substance misuse problem.n` = CLA_submisuse,
                  `Received an intervention for their substance misuse problem.n` = CLA_subint,
                  `Offered intervention but refused it.n` = CLA_suboffint,
                  `Development assessments up to date.n`=CLA_devassmt,
                  `Had their immunisations up to date.n` = CLA_immunisation,
                  `Had their teeth checked by a dentist.n` = CLA_teethcheck,
                  `Had their annual health assessment.n` = CLA_healthassmt,
                  `SDQ score is a cause for concern.n` = CLA_SDQconcern,
                  `SDQ score is borderline.n` = CLA_SDQborderline,
                  `SDQ score is normal.n` = CLA_SDQnormal,
                  `Total ages 5 to 16 years.n` = CLA_5to16,
                  `SDQ score was received.n` = CLA_SDQ) %>%
    dplyr::filter(LA_Code!="")%>%
    dplyr::mutate(`Total all ages.pt` = "100",
                  `Total ages 10 to 17 years.pt` ="100", 
                  `Total ages 0 to 4 years.pt` = "100",
                  `Total ages 5 to 16 years.pt` = "100",
                  `Convicted or subject to youth cautions or youth conditional cautions during the year.pt` = as.character((as.numeric(`Convicted or subject to youth cautions or youth conditional cautions during the year.n`)/as.numeric(`Total ages 10 to 17 years.n`))*100),
                  `Identified as having a substance misuse problem.pt` = as.character((as.numeric(`Identified as having a substance misuse problem.n`)/as.numeric(`Total all ages.n`))*100),
                  `Received an intervention for their substance misuse problem.pt` = as.character((as.numeric(`Received an intervention for their substance misuse problem.n`)/as.numeric(`Identified as having a substance misuse problem.n`))*100),
                  `Offered intervention but refused it.pt` = as.character((as.numeric(`Offered intervention but refused it.n`)/as.numeric(`Identified as having a substance misuse problem.n`))*100),
                  `Development assessments up to date.pt`= as.character((as.numeric(`Development assessments up to date.n`)/as.numeric(`Total ages 0 to 4 years.n`))*100),
                  `Had their immunisations up to date.pt` = as.character((as.numeric(`Had their immunisations up to date.n`)/as.numeric(`Total all ages.n`))*100),
                  `Had their teeth checked by a dentist.pt` = as.character((as.numeric(`Had their teeth checked by a dentist.n`)/as.numeric(`Total all ages.n`))*100),
                  `Had their annual health assessment.pt` = as.character((as.numeric(`Had their annual health assessment.n`)/as.numeric(`Total all ages.n`))*100),
                  `SDQ score was received.pt` = as.character((as.numeric(`SDQ score was received.n`)/as.numeric(`Total ages 5 to 16 years.n`))*100),
                  `SDQ score is normal.pt` = as.character((as.numeric(`SDQ score is normal.n`)/as.numeric(`Total ages 5 to 16 years.n`))*100),
                  `SDQ score is a cause for concern.pt` = as.character((as.numeric(`SDQ score is a cause for concern.n`)/as.numeric(`Total ages 5 to 16 years.n`))*100),
                  `SDQ score is borderline.pt` = as.character((as.numeric(`SDQ score is borderline.n`)/as.numeric(`Total ages 5 to 16 years.n`))*100),
    )%>%
    tidyr::pivot_longer(cols = !c(LA_Name, LA.Number, LA_Code), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(.+)\\.(.+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "health and criminalisation")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  oc22 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2016_LA/sheet_extra_sdq.csv"),
                   colClasses = "character", skip=6)[c(1,2,3,21,23)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name = X.2,
                  LA.Number = X.1,
                  LA_Code = X,
                  yes = Number.of.children.looked.after.for.at.least.12.months.aged.5.to.16.with.an.SDQ.score4.2,
                  percent = Average.score.per.child.2)%>%
    dplyr::filter(LA.Number!="")%>%
    dplyr::mutate(number = as.character((as.numeric(percent))*as.numeric(yes)),
                  variable = "SDQ average score",
                  category = "child outcomes",
                  subcategory = "health and criminalisation")%>%
    dplyr::select(-yes)
  
  
  sen <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2016_LA/sheet7_sen.csv"),
                  colClasses = "character", skip=5)[c(1:4,12,13,15,16)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name= X.1,
                  LA_Code=LA.Code,
                  LA.Number=X,
                  all_SEN.n = Total.with.SEN5,
                  all_SEN.pt = X.11,
                  statement_or_EHC_plan.n = X.8,
                  statement_or_EHC_plan.pt = X.9,
                  pupils.n = Number.of.children.looked.after.at.31.March.who.had.been.continuously.looked.after.for.at.least.twelve.months.and.matched.to.census.data2)%>%
    dplyr::filter(LA_Code!="")%>%
    dplyr::mutate(pupils.pt = "100")%>%
    tidyr::pivot_longer(cols = !c(LA_Name, LA.Number, LA_Code), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(.+)\\.(.+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "special educational needs")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  
  
  schl_absence <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/2016_LA/sheet8_absence.csv"),
                           colClasses = "character", skip=6)[c(1:3,29:35)]%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::rename(LA_Name= X.2,
                  LA_Code=X,
                  LA.Number=X.1,
                  pupils.n=Number.of.looked.after.children.matched.to.absence.data4.3,
                  sess_overall.pt = Percentage.of.sessions.missed.due.to5..3,
                  sess_authorised.pt = X.20,
                  sess_unauthorised.pt = X.21,
                  pupils_pa_10_exact.pt = Percentage.of.looked.after.children.classed.as.persistent.absentees6.3)%>% #rename variables
    dplyr::select(-X.19, -X.22)%>% #remove empty column
    dplyr::filter(LA_Code!="")%>%
    dplyr::mutate(pupils.pt = as.character((as.numeric(pupils.n)/as.numeric(pupils.n))*100),
                  sess_possible.pt = NA,
                  sess_possible.n = NA,
                  sess_overall.n = NA,
                  sess_authorised.n = NA,
                  sess_unauthorised.n = NA,
                  pupils_pa_10_exact.n = as.character((as.numeric(pupils_pa_10_exact.pt)/100)*as.numeric(pupils.n)))%>%
    pivot_longer(cols = !c(LA_Name, LA_Code,LA.Number), 
                 names_to = c("variable", ".value"),
                 names_pattern = "(\\w+)\\.(\\w+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "school absence")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  
  
  #bind together each outcome group
  outcomes_2016 <- rbind(schl_absence,oc2, oc22, 
                         ks2,ks22, ks42, ks43,  sen)
  
  #allocate year variable
  outcomes_2016$year <- 2016
  
  #create missing variable for la codes in old data
  # outcomes_2014$LA_Code <- NA
  # outcomes_2014$LA.Number <- NA
  
  #bind together previous years
  outcomes <- rbind(outcomes, outcomes_2016)
  
  #remove all unnecessary dataframes
  rm(list=setdiff(ls(), c("outcomes")))
  
  
  
  #Post2016
  #read in raw data
  exclusions <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/Post_2016_files/permanent_exclusions_and_suspensions_la.csv"),
                         colClasses = "character")%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::filter(geographic_level=="Local authority",
                  social_care_group=="CLA 12 months at 31 March")%>% # keep only LAs and LACs
    dplyr::mutate(year = paste("20", str_sub(time_period, start= -2), sep=""))%>% # turn 202021 to 2021
    dplyr::rename(LA_Name= la_name,
                  LA_Code=new_la_code,
                  LA.Number=old_la_code)%>% #rename variables
    dplyr::select(-time_period, -time_identifier, -geographic_level, -country_code, -country_name, -region_code, -region_name ,-social_care_group)%>% #remove empty column
    tidyr::pivot_longer(cols=!c(LA_Name, LA_Code,LA.Number, year), names_to = "variable", 
                        values_to = "value")%>% #pivot so variables go in one column
    dplyr::mutate(category = "child outcomes",
                  subcategory = "school absence")%>%
    tidyr::pivot_wider(names_from = variable, values_from = value)%>%
    pivot_longer(cols = !c(LA_Name, LA_Code,LA.Number,category, subcategory, year), 
                 names_to = c(".value", "variable"),
                 names_pattern = "(t|pt)_(.*)")%>%
    dplyr::rename(number=t,
                  percent=pt)
  
  absence <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/Post_2016_files/absence_six_half_terms_la.csv"),
                      colClasses = "character")%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::filter(geographic_level=="Local authority",
                  social_care_group=="CLA 12 months at 31 March",
                  school_type=="Total")%>% # keep only LAs and LACs
    dplyr::mutate(year = paste("20", str_sub(time_period, start= -2), sep=""),
                  pt_pupils="100",
                  pt_sess_possible="100")%>% # turn 202021 to 2021)
    dplyr::rename(LA_Name= la_name,
                  LA_Code=new_la_code,
                  LA.Number=old_la_code,
                  pt_sess_overall = pt_overall)%>% #rename variables
    dplyr::select(-time_period, -time_identifier, -geographic_level, -country_code, -country_name, -region_code, -region_name ,-social_care_group, -school_type, -year_breakdown)%>% #remove empty column
    tidyr::pivot_longer(cols=!c(LA_Name, LA_Code,LA.Number, year), names_to = "variable", 
                        values_to = "value")%>%   #pivot so variables go in one column
    dplyr::mutate(category = "child outcomes",
                  subcategory = "school absence")%>%
    tidyr::pivot_wider(names_from = variable, values_from = value)%>%
    pivot_longer(cols = !c(LA_Name, LA_Code,LA.Number,category, subcategory, year), 
                 names_to = c(".value", "variable"),
                 names_pattern = "(t|pt)_(.*)")%>%
    dplyr::rename(number=t,
                  percent=pt)
  
  
  ks2 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/Post_2016_files/ks2_la.csv"),
                  colClasses = "character")%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::filter(geographic_level=="Local authority",
                  social_care_group=="CLA 12 months at 31 March")%>% # keep only LAs and LACs
    dplyr::mutate(year = paste("20", str_sub(time_period, start= -2), sep=""))%>% # turn 202021 to 2021)
    dplyr::rename(LA_Name= la_name,
                  LA_Code=new_la_code,
                  LA.Number=old_la_code)%>% #rename variables
    dplyr::select(-time_period, -time_identifier, -geographic_level, -country_code, -country_name, -region_code, -region_name ,-social_care_group, -version)%>% #remove empty column
    tidyr::pivot_longer(cols=!c(LA_Name, LA_Code,LA.Number, year), names_to = "variable", 
                        values_to = "value")%>%   #pivot so variables go in one column
    dplyr::mutate(category = "child outcomes",
                  subcategory = "ks2")%>%
    tidyr::pivot_wider(names_from = variable, values_from = value)%>%
    dplyr::rename(pt_mat_progress_score = avg_mat_progress_score,
                  pt_writta_progress_score = avg_writta_progress_score,
                  pt_read_progress_score = avg_read_progress_score)%>%
    dplyr::select(-ends_with("CI"))%>%
    pivot_longer(cols = !c(LA_Name, LA_Code,LA.Number,category, subcategory, year), 
                 names_to = c(".value", "variable"),
                 names_pattern = "(t|pt)_(.*)")%>%
    dplyr::rename(number=t,
                  percent=pt)
  
  
  
  ks4 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/Post_2016_files/ks4_la.csv"),
                  colClasses = "character")%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::filter(geographic_level=="Local authority",
                  social_care_group=="CLA 12 months at 31 March")%>% # keep only LAs and LACs
    dplyr::mutate(year = paste("20", str_sub(time_period, start= -2), sep=""))%>% # turn 202021 to 2021)
    dplyr::rename(LA_Name= la_name,
                  LA_Code=new_la_code,
                  LA.Number=old_la_code)%>% #rename variables
    dplyr::select(-time_period, -time_identifier, -geographic_level, -country_code, -country_name, -region_code, -region_name ,-social_care_group, -version)%>% #remove empty column
    tidyr::pivot_longer(cols=!c(LA_Name, LA_Code,LA.Number, year), names_to = "variable", 
                        values_to = "value")%>%   #pivot so variables go in one column
    dplyr::mutate(category = "child outcomes",
                  subcategory = "ks4")%>%
    tidyr::pivot_wider(names_from = variable, values_from = value)%>%
    dplyr::rename(pt_att8 = avg_att8,
                  pt_ebaccaps = avg_ebaccaps,
                  pt_p8score = avg_p8score)%>%
    dplyr::mutate(pt_pupils="100")%>%
    dplyr::select(-p8score_CI_low,  -p8score_CI_upp)%>%
    pivot_longer(cols = !c(LA_Name, LA_Code,LA.Number,category, subcategory, year), 
                 names_to = c(".value", "variable"),
                 names_pattern = "(t|pt)_(.*)")%>%
    dplyr::rename(number=t,
                  percent=pt)
  
  
  
  sen <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/Post_2016_files/sen_la.csv"),
                  colClasses = "character")%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::filter(geographic_level=="Local authority",
                  social_care_group=="CLA 12 months at 31 March")%>% # keep only LAs and LACs
    dplyr::mutate(year = paste("20", str_sub(time_period, start= -2), sep=""))%>% # turn 202021 to 2021)
    dplyr::rename(LA_Name= la_name,
                  LA_Code=new_la_code,
                  LA.Number=old_la_code)%>% #rename variables
    dplyr::select(-time_period, -time_identifier, -geographic_level, -country_code, -country_name, -region_code, -region_name ,-social_care_group)%>% #remove empty column
    tidyr::pivot_longer(cols=!c(LA_Name, LA_Code,LA.Number, year), names_to = "variable", 
                        values_to = "value")%>%   #pivot so variables go in one column
    dplyr::mutate(category = "child outcomes",
                  subcategory = "special educational needs")%>%
    tidyr::pivot_wider(names_from = variable, values_from = value)%>%
    pivot_longer(cols = !c(LA_Name, LA_Code,LA.Number,category, subcategory, year), 
                 names_to = c(".value", "variable"),
                 names_pattern = "(t|pt)_(.*)")%>%
    dplyr::rename(number=t,
                  percent=pt)
  
  oc2 <-  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/Post_2016_files/la_conviction_health_outcome_cla.csv"),
                   colClasses = "character")%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::filter(geographic_level=="Local authority")%>% # keep only LAs and LACs
    dplyr::mutate(year = time_period)%>% # turn 202021 to 2021)
    dplyr::rename(LA_Name= la_name,
                  LA_Code=new_la_code,
                  LA.Number=old_la_code,
                  variable = characteristic,
                  percent = percentage)%>% #rename variables
    dplyr::select(-time_period, -time_identifier, -geographic_level, -country_code, 
                  -country_name, -region_code, -region_name, -cla_group)%>% #remove empty column
    dplyr::mutate(category = "child outcomes",
                  subcategory = "health and criminalisation",
                  percent = ifelse(variable == "SDQ average score", number, percent),
                  number = ifelse(variable == "SDQ average score", NA, number))
  
  
  
  oc2_17 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Outcomes/Post_2016_files/SFR50_OC22017.csv"),
                     colClasses = "character")%>%
    dplyr::mutate_all(~ str_replace(., ",", ""))%>%
    dplyr::filter(geog_l=="LA")%>%
    dplyr::select(-geog_l, -LA_order)%>% #remove empty column
    dplyr::rename(LA_Name= geog_n,
                  LA.Number=geog_c,
                  LA_Code=New_geog_code,
                  `Total all ages.n` = OC2_12mths,
                  `Total ages 10 to 17 years.n` = OC2_10to17,
                  `Total ages 0 to 4 years.n` = OC2_U5,
                  `Convicted or subject to youth cautions or youth conditional cautions during the year.n` = OC2_convicted,
                  `Identified as having a substance misuse problem.n` = OC2_submisuse,
                  `Received an intervention for their substance misuse problem.n` = OC2_subint,
                  `Offered intervention but refused it.n` = OC2_suboffint,
                  `Development assessments up to date.n`=OC2_devassmt,
                  `Had their immunisations up to date.n` = OC2_immunisation,
                  `Had their teeth checked by a dentist.n` = OC2_teethcheck,
                  `Had their annual health assessment.n` = OC2_healthassmt,
                  `SDQ score is a cause for concern.n` = OC2_SDQconcern,
                  `SDQ score is borderline.n` = OC2_SDQborderline,
                  `SDQ score is normal.n` = OC2_SDQnormal,
                  `Total ages 5 to 16 years.n` = OC2_5to16,
                  `SDQ score was received.n` = OC2_SDQ,
                  `SDQ average score.pt` = OC2_SDQaverage) %>%
    dplyr::filter(LA_Code!="")%>%
    dplyr::mutate(`Total all ages.pt` = "100",
                  `Total ages 10 to 17 years.pt` ="100", 
                  `Total ages 0 to 4 years.pt` = "100",
                  `Total ages 5 to 16 years.pt` = "100",
                  `Convicted or subject to youth cautions or youth conditional cautions during the year.pt` = as.character((as.numeric(`Convicted or subject to youth cautions or youth conditional cautions during the year.n`)/as.numeric(`Total ages 10 to 17 years.n`))*100),
                  `Identified as having a substance misuse problem.pt` = as.character((as.numeric(`Identified as having a substance misuse problem.n`)/as.numeric(`Total all ages.n`))*100),
                  `Received an intervention for their substance misuse problem.pt` = as.character((as.numeric(`Received an intervention for their substance misuse problem.n`)/as.numeric(`Identified as having a substance misuse problem.n`))*100),
                  `Offered intervention but refused it.pt` = as.character((as.numeric(`Offered intervention but refused it.n`)/as.numeric(`Identified as having a substance misuse problem.n`))*100),
                  `Development assessments up to date.pt`= as.character((as.numeric(`Development assessments up to date.n`)/as.numeric(`Total ages 0 to 4 years.n`))*100),
                  `Had their immunisations up to date.pt` = as.character((as.numeric(`Had their immunisations up to date.n`)/as.numeric(`Total all ages.n`))*100),
                  `Had their teeth checked by a dentist.pt` = as.character((as.numeric(`Had their teeth checked by a dentist.n`)/as.numeric(`Total all ages.n`))*100),
                  `Had their annual health assessment.pt` = as.character((as.numeric(`Had their annual health assessment.n`)/as.numeric(`Total all ages.n`))*100),
                  `SDQ score was received.pt` = as.character((as.numeric(`SDQ score was received.n`)/as.numeric(`Total ages 5 to 16 years.n`))*100),
                  `SDQ score is normal.pt` = as.character((as.numeric(`SDQ score is normal.n`)/as.numeric(`Total ages 5 to 16 years.n`))*100),
                  `SDQ score is a cause for concern.pt` = as.character((as.numeric(`SDQ score is a cause for concern.n`)/as.numeric(`Total ages 5 to 16 years.n`))*100),
                  `SDQ score is borderline.pt` = as.character((as.numeric(`SDQ score is borderline.n`)/as.numeric(`Total ages 5 to 16 years.n`))*100),
                  `SDQ average score.n` = as.character((as.numeric(`SDQ average score.pt`)*as.numeric(`SDQ score was received.n`))),
                  year="2017"
    )%>%
    tidyr::pivot_longer(cols = !c(LA_Name, LA.Number, LA_Code, year), 
                        names_to = c("variable", ".value"),
                        names_pattern = "(.+)\\.(.+)")%>%
    dplyr::mutate(category = "child outcomes",
                  subcategory = "health and criminalisation")%>%
    dplyr::rename(number=n,
                  percent=pt)
  
  outcomespost <- rbind( sen, ks2, ks4, absence, exclusions, oc2, oc2_17)
  outcomes <- rbind(outcomespost, outcomes)
  
  outcomes <- outcomes %>%
    dplyr::filter(!variable=="Received an intervention for their substance misuse problem"&year<2018,
                  !variable=="Offered intervention but refused it"&year<2018)
  #rm(list=setdiff(ls(), c("outcomes")))
  
  
  
  
}