####packages####

if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools, dplyr, tidyverse, tidyr, stringr,  curl)


setwd("C:/Users/bengo/OneDrive - Nexus365/Documents/Children's Care Homes Project")
options(scipen=999)


rm(list=setdiff(ls(), c("")))
####start####

####2022####

leave_acc <-  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/post_2017/data/la_care_leavers_accommodation_suitability.csv"),
                 colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geographic_level=="Local authority")%>% # keep only LAs
  dplyr::mutate(year = time_period)%>%
  dplyr::rename(LA_Name= la_name,
                LA_Code=new_la_code,
                LA.Number=old_la_code,
                percent = percentage,
                subcategory = age,
                variable = accommodation_suitability)%>% #rename variables
  dplyr::select(-time_period, -time_identifier, -geographic_level, -country_code, 
                -country_name, -region_code, -region_name)%>% #remove empty column
  dplyr::mutate(category = "care leavers")


leave_acctype <-  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/post_2017/data/la_care_leavers_accommodation_type.csv"),
                       colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geographic_level=="Local authority")%>% # keep only LAs
  dplyr::mutate(year = time_period)%>%
  dplyr::rename(LA_Name= la_name,
                LA_Code=new_la_code,
                LA.Number=old_la_code,
                percent = percentage,
                subcategory = age,
                variable = accommodation_type)%>% #rename variables
  dplyr::select(-time_period, -time_identifier, -geographic_level, -country_code, 
                -country_name, -region_code, -region_name)%>% #remove empty column
  dplyr::mutate(category = "care leavers")



leave_activity <-  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/post_2017/data/la_care_leavers_activity.csv"),
                           colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geographic_level=="Local authority")%>% # keep only LAs
  dplyr::mutate(year = time_period)%>%
  dplyr::rename(LA_Name= la_name,
                LA_Code=new_la_code,
                LA.Number=old_la_code,
                percent = percentage,
                subcategory = age,
                variable = activity)%>% #rename variables
  dplyr::select(-time_period, -time_identifier, -geographic_level, -country_code, 
                -country_name, -region_code, -region_name)%>% #remove empty column
  dplyr::mutate(category = "care leavers")

leave_intouch <-  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/post_2017/data/la_care_leavers_intouch.csv"),
                            colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geographic_level=="Local authority")%>% # keep only LAs
  dplyr::mutate(year = time_period)%>%
  dplyr::rename(LA_Name= la_name,
                LA_Code=new_la_code,
                LA.Number=old_la_code,
                percent = percentage,
                subcategory = age,
                variable = in_touch)%>% #rename variables
  dplyr::select(-time_period, -time_identifier, -geographic_level, -country_code, 
                -country_name, -region_code, -region_name)%>% #remove empty column
  dplyr::mutate(category = "care leavers")


leave_stayput <-  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/post_2017/data/la_care_leavers_stayput.csv"),
                           colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geographic_level=="Local authority")%>% # keep only LAs
  dplyr::mutate(year = time_period)%>%
  dplyr::rename(LA_Name= la_name,
                LA_Code=new_la_code,
                LA.Number=old_la_code,
                percent = percentage,
                subcategory = age,
                variable = stay_put)%>% #rename variables
  dplyr::select(-time_period, -time_identifier, -geographic_level, -country_code, 
                -country_name, -region_code, -region_name)%>% #remove empty column
  dplyr::mutate(category = "care leavers")


shortterm <-  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/post_2017/data/la_children_looked_after_exclusively_under_a_series_of_short_term_placements.csv"),
                           colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geographic_level=="Local authority")%>% # keep only LAs
  dplyr::mutate(year = time_period,
                percent=NA)%>%
  dplyr::rename(LA_Name= la_name,
                LA_Code=new_la_code,
                LA.Number=old_la_code,
                variable = cla_group)%>% #rename variables
  dplyr::select(-time_period, -time_identifier, -geographic_level, -country_code, 
                -country_name, -region_code, -region_name)%>% #remove empty column
  dplyr::mutate(category = "child characteristics",
                subcategory = "short term placements")


adopted_during <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/post_2017/data/la_children_who_ceased_during_the_year_sgo_adoptions.csv"),
                        colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geographic_level=="Local authority")%>% # keep only LAs
  dplyr::mutate(year = time_period,
                variable =  ifelse(characteristic == "Total", paste(cla_group, "_", characteristic, sep = ""), characteristic))%>%
  dplyr::rename(LA_Name= la_name,
                LA_Code=new_la_code,
                LA.Number=old_la_code,
                percent = percentage)%>% #rename variables
  dplyr::select(-time_period, -time_identifier, -geographic_level, -country_code, 
                -country_name, -region_code, -region_name, -cla_group, -characteristic,)%>% #remove empty column
  dplyr::mutate(category = "child characteristics",
                subcategory = "adopted during")



ceased_during <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/post_2017/data/la_children_who_ceased_during_year.csv"),
                           colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geographic_level=="Local authority")%>% # keep only LAs
  dplyr::mutate(year = time_period)%>%
  dplyr::rename(LA_Name= la_name,
                LA_Code=new_la_code,
                LA.Number=old_la_code,
                percent = percentage,
                subcategory = cla_group,
                variable = characteristic)%>% #rename variables
  dplyr::select(-time_period, -time_identifier, -geographic_level, -country_code, 
                -country_name, -region_code, -region_name)%>% #remove empty column
  dplyr::mutate(category = "ceased during")



missing <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/post_2017/data/la_children_who_were_missing_or_away_from_placement_without_authorisation.csv"),
                          colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geographic_level=="Local authority")%>% # keep only LAs
  dplyr::mutate(year = X.U.FEFF.time_period,
                percent = NA)%>%
  dplyr::rename(LA_Name= la_name,
                LA_Code=new_la_code,
                LA.Number=old_la_code,
                variable = cla_group)%>% #rename variables
  dplyr::select(-X.U.FEFF.time_period, -time_identifier, -geographic_level, -country_code, 
                -country_name, -region_code, -region_name)%>% #remove empty column
  dplyr::mutate(category = "child characteristics",
                subcategory = "missing incidents")



stability <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/post_2017/data/la_cla_at_31_march_placement_stability_indicator.csv"),
                    colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geographic_level=="Local authority")%>% # keep only LAs
  dplyr::mutate(year = X.U.FEFF.time_period)%>%
  dplyr::rename(LA_Name= la_name,
                LA_Code=new_la_code,
                LA.Number=old_la_code,
                percent = percentage,
                variable = characteristic,
                subcategory = cla_group)%>% #rename variables
  dplyr::select(-X.U.FEFF.time_period, -time_identifier, -geographic_level, -country_code, 
                -country_name, -region_code, -region_name)%>% #remove empty column
  dplyr::mutate(category = "placement stability")


new_placements <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/post_2017/data/la_cla_during_year_new_placements.csv"),
                         colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geographic_level=="Local authority")%>% # keep only LAs
  dplyr::mutate(year = time_period)%>%
  dplyr::rename(LA_Name= la_name,
                LA_Code=new_la_code,
                LA.Number=old_la_code,
                percent = percentage,
                subcategory = cla_group,
                variable = characteristic)%>% #rename variables
  dplyr::select(-time_period, -time_identifier, -geographic_level, -country_code, 
                -country_name, -region_code, -region_name)%>% #remove empty column
  dplyr::mutate(category = "new placements")


char <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/post_2017/data/la_cla_on_31_march_by_characteristics.csv"),
                 colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geographic_level=="Local authority")%>% # keep only LAs
  dplyr::mutate(year = time_period)%>%
  dplyr::rename(LA_Name= la_name,
                LA_Code=new_la_code,
                LA.Number=old_la_code,
                percent = percentage,
                subcategory = cla_group,
                variable = characteristic)%>% #rename variables
  dplyr::select(-time_period, -time_identifier, -geographic_level, -country_code, 
                -country_name, -region_code, -region_name)%>% #remove empty column
  dplyr::mutate(category = "child characteristic at 31st March")



started_during <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/post_2017/data/la_cla_who_started_to_be_looked_after.csv"),
                 colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geographic_level=="Local authority")%>% # keep only LAs
  dplyr::mutate(year = time_period)%>%
  dplyr::rename(LA_Name= la_name,
                LA_Code=new_la_code,
                LA.Number=old_la_code,
                percent = percentage,
                subcategory = cla_group,
                variable = characteristic)%>% #rename variables
  dplyr::select(-time_period, -time_identifier, -geographic_level, -country_code, 
                -country_name, -region_code, -region_name)%>% #remove empty column
  dplyr::mutate(category = "started during")

net_gain <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/post_2017/data/la_cla_on_31_march_la_of_placement_net_gain.csv"),
                     colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geographic_level=="Local authority")%>% # keep only LAs
  dplyr::mutate(year = X.U.FEFF.time_period)%>%
  dplyr::rename(LA_Name= la_name,
                LA_Code=new_la_code,
                LA.Number=old_la_code,
                variable = characteristic)%>% #rename variables
  dplyr::select(-X.U.FEFF.time_period, -time_identifier, -geographic_level, -country_code, 
                -country_name, -region_code, -region_name)%>% #remove empty column
  dplyr::mutate(category = "net gain",
                percent = NA,
                subcategory = "net gain")


school_moves <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/post_2017/data/official_statistics_la_cla_school_moves.csv"),
                           colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geographic_level=="Local authority")%>% # keep only LAs
  dplyr::mutate(year = X.U.FEFF.time_period)%>%
  dplyr::rename(LA_Name= la_name,
                LA_Code=new_la_code,
                LA.Number=old_la_code,
                percent = percentage,
                subcategory = move_measure,
                variable = school_stability)%>% #rename variables
  dplyr::select(-X.U.FEFF.time_period, -time_identifier, -geographic_level, -country_code, 
                -country_name, -region_code, -region_name, -cla_group)%>% #remove empty column
  dplyr::mutate(category = "school moves")


outcomes <- rbind(school_moves, net_gain, started_during, char,
                         new_placements, stability, missing, ceased_during,
                         adopted_during, shortterm, leave_acc, leave_acctype,
                         leave_activity, leave_stayput, leave_intouch)

rm(list=setdiff(ls(), c("outcomes")))

####2017#### 


####2016####


####2015####


####2014####


####2013####


####2012####


####2011####


####2010####


