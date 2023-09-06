####packages####

if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools, dplyr, tidyverse, tidyr, stringr,  curl)


#setwd("C:/Users/bengo/OneDrive - Nexus365/Documents/Children's Care Homes Project")
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
  dplyr::mutate(year = time_period,
                percent = NA)%>%
  dplyr::rename(LA_Name= la_name,
                LA_Code=new_la_code,
                LA.Number=old_la_code,
                variable = cla_group)%>% #rename variables
  dplyr::select(-time_period, -time_identifier, -geographic_level, -country_code, 
                -country_name, -region_code, -region_name)%>% #remove empty column
  dplyr::mutate(category = "child characteristics",
                subcategory = "missing incidents")



stability <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/post_2017/data/la_cla_at_31_march_placement_stability_indicator.csv"),
                    colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geographic_level=="Local authority")%>% # keep only LAs
  dplyr::mutate(year = time_period)%>%
  dplyr::rename(LA_Name= la_name,
                LA_Code=new_la_code,
                LA.Number=old_la_code,
                percent = percentage,
                variable = characteristic,
                subcategory = cla_group)%>% #rename variables
  dplyr::select(-time_period, -time_identifier, -geographic_level, -country_code, 
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
  dplyr::mutate(year = time_period)%>%
  dplyr::rename(LA_Name= la_name,
                LA_Code=new_la_code,
                LA.Number=old_la_code,
                variable = characteristic)%>% #rename variables
  dplyr::select(-time_period, -time_identifier, -geographic_level, -country_code, 
                -country_name, -region_code, -region_name)%>% #remove empty column
  dplyr::mutate(category = "net gain",
                percent = NA,
                subcategory = "net gain")


school_moves <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/post_2017/data/official_statistics_la_cla_school_moves.csv"),
                           colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geographic_level=="Local authority")%>% # keep only LAs
  dplyr::mutate(year = time_period)%>%
  dplyr::rename(LA_Name= la_name,
                LA_Code=new_la_code,
                LA.Number=old_la_code,
                percent = percentage,
                subcategory = move_measure,
                variable = school_stability)%>% #rename variables
  dplyr::select(-time_period, -time_identifier, -geographic_level, -country_code, 
                -country_name, -region_code, -region_name, -cla_group)%>% #remove empty column
  dplyr::mutate(category = "school moves")


characteristics <- rbind(school_moves, net_gain, started_during, char,
                         new_placements, stability, missing, ceased_during,
                         adopted_during, shortterm, leave_acc, leave_acctype,
                         leave_activity, leave_stayput, leave_intouch)

rm(list=setdiff(ls(), c("characteristics")))

####2017#### 

admitted <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2017/SFR50_ADM2017.csv"),
                     colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::rename(LA_Code = New_geog_code,
                LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::select(-geog_l, -LA_order)%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                    names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "CLA_started2017"])*100)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "started during",
                year="2017",
    subcategory = ifelse(variable=="CLA_started2017", "Taken into care",
                                  ifelse(variable=="CLA_taken2017", "Taken into care",
                                         ifelse(variable=="SCLA_10to15", "Age group",
                                                ifelse(variable=="SCLA_16over", "Age group",
                                                       ifelse(variable=="SCLA_1to4", "Age group",
                                                              ifelse(variable=="SCLA_5to9", "Age group",
                                                                     ifelse(variable=="SCLA_AbNeg", "Category of need",
                                                                            ifelse(variable=="SCLA_AbsPar", "Category of need",
                                                                                   ifelse(variable=="SCLA_Cdisab", "Category of need",
                                                                                          ifelse(variable=="SCLA_FAcSt", "Category of need",
                                                                                                 ifelse(variable=="SCLA_FCO", "Legal status",
                                                                                                        ifelse(variable=="SCLA_FD", "Category of need",
                                                                                                               ifelse(variable=="SCLA_female", "Gender",
                                                                                                                      ifelse(variable=="SCLA_ICO", "Legal status",
                                                                                                                             ifelse(variable=="SCLA_LI", "Category of need",
                                                                                                                                    ifelse(variable=="SCLA_male", "Gender",
                                                                                                                                           ifelse(variable=="SCLA_ONCT", "Legal status",
                                                                                                                                                  ifelse(variable=="SCLA_PACE", "Legal status",
                                                                                                                                                         ifelse(variable=="SCLA_ParIll", "Category of need",
                                                                                                                                                                ifelse(variable=="SCLA_PlaceO", "Legal status",
                                                                                                                                                                       ifelse(variable=="SCLA_S20", "Legal status",
                                                                                                                                                                              ifelse(variable=="SCLA_SEPO", "Legal status",
                                                                                                                                                                                     ifelse(variable=="SCLA_SORR", NA,
                                                                                                                                                                                            ifelse(variable=="SCLA_SUB", "Category of need",
                                                                                                                                                                                                   ifelse(variable=="SCLA_U1", "Age group",
                                                                                                                                                                                                          ifelse(variable=="SCLA_UCAO", "Legal status",
                                                                                                                                                                                                                 ifelse(variable=="SCLA_UPP",NA, NA))))))))))))))))))))))))))),
                variable = ifelse(variable=="CLA_started2017", "Total children",
                                  ifelse(variable=="CLA_taken2017", "All children taken into care",
                                         ifelse(variable=="SCLA_10to15", "10 to 15 years",
                                                ifelse(variable=="SCLA_16over", "16 years and over",
                                                       ifelse(variable=="SCLA_1to4", "1 to 4 years",
                                                              ifelse(variable=="SCLA_5to9", "5 to 9 years",
                                                                     ifelse(variable=="SCLA_AbNeg", "N1. Abuse or neglect",
                                                                            ifelse(variable=="SCLA_AbsPar", "N8. Absent parenting",
                                                                                   ifelse(variable=="SCLA_Cdisab", "N2. Child's disability",
                                                                                          ifelse(variable=="SCLA_FAcSt", "N.4 Family acute stress",
                                                                                                 ifelse(variable=="SCLA_FCO", "Full care order",
                                                                                                        ifelse(variable=="SCLA_FD", "N5. Family dysfunction",
                                                                                                               ifelse(variable=="SCLA_female", "Female",
                                                                                                                      ifelse(variable=="SCLA_ICO", "Interm care order",
                                                                                                                             ifelse(variable=="SCLA_LI", "N7. Low income",
                                                                                                                                    ifelse(variable=="SCLA_male", "Male",
                                                                                                                                           ifelse(variable=="SCLA_ONCT", "Remand",
                                                                                                                                                  ifelse(variable=="SCLA_PACE", "LA accommodation under PACE 1989",
                                                                                                                                                         ifelse(variable=="SCLA_ParIll", "N3. Parental illness or disability",
                                                                                                                                                                ifelse(variable=="SCLA_PlaceO", "Placement order granted",
                                                                                                                                                                       ifelse(variable=="SCLA_S20", "Voluntary agreement under S20",
                                                                                                                                                                              ifelse(variable=="SCLA_SEPO", "Emergency protction order",
                                                                                                                                                                                     ifelse(variable=="SCLA_SORR", NA,
                                                                                                                                                                                            ifelse(variable=="SCLA_SUB", "N6. Socially unacceptable behaviour",
                                                                                                                                                                                                   ifelse(variable=="SCLA_U1", "Under 1 year",
                                                                                                                                                                                                          ifelse(variable=="SCLA_UCAO", "Child assessment order and in LA accommodation",
                                                                                                                                                                                                                 ifelse(variable=="SCLA_UPP",NA, NA))))))))))))))))))))))))))))






march <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2017/SFR50_CLA2017.csv"),
                  colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::rename(LA_Code = New_geog_code,
                LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::select(-geog_l, -LA_order)%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "CLA_Mar2017"])*100))%>%
  dplyr::mutate(percent = ifelse(variable=="CLA_2017", 100, percent))%>%
  dplyr::mutate(percent = ifelse(variable=="CLA_stp2017", as.numeric(number)/as.numeric(number[variable=="CLA_2017"])*100, percent))%>%
  dplyr::mutate(percent = ifelse(grepl("RPC", variable), as.numeric(number)/as.numeric(number[variable=="CLA_RPC_All_Place"])*100, percent))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "child characteristic at 31st March",
                year="2017",
                subcategory = ifelse(variable=="CLA_Mar2017", "Age group",
                                  ifelse(variable=="CLA_2017", "Age group",
                                         ifelse(variable=="CLA_stp2017", "Age group",
                                            ifelse(variable=="CLA_10to15", "Age group",
                                              ifelse(variable=="CLA_16over", "Age group",
                                                ifelse(variable=="CLA_1to4", "Age group",NA)))))),
                subcategory = ifelse(variable=="CLA_5to9", "Age group",
                                    ifelse(variable=="CLA_Adopt", "Placement",
                                           ifelse(variable=="CLA_Asian", "Ethnicity",
                                                  ifelse(variable=="CLA_Black", "Ethnicity",
                                                         ifelse(variable=="CLA_CPG", "Legal status",
                                                                ifelse(variable=="CLA_EOTH", "Ethnicity", subcategory)))))),
                subcategory = ifelse(variable=="CLA_ExtPl", "LA of placement",
                                    ifelse(variable=="CLA_FCO", "Legal status",
                                           ifelse(variable=="CLA_female", "Gender",
                                                  ifelse(variable=="CLA_Fost", "Placement",
                                                         ifelse(variable=="CLA_FrAd", NA,                                                                                                                                    
                                                                ifelse(variable=="CLA_ICO", "Legal status", subcategory)))))),
                subcategory = ifelse(variable=="CLA_InBound", "Locality of placement",
                                    ifelse(variable=="CLA_InLA_GT20", "Distance between home and placement and locality of placement",
                                       ifelse(variable=="CLA_InLA_LTE20", "Distance between home and placement and locality of placement",
                                              ifelse(variable=="CLA_InLA_NoInfo", "Distance between home and placement and locality of placement",
                                                     ifelse(variable=="CLA_IntPl", "LA of placement",
                                                            ifelse(variable=="CLA_LAPl", "Placed inside the local authority boundary",subcategory)))))),
                subcategory = ifelse(variable=="CLA_male", "Gender",
                                     ifelse(variable=="CLA_Mixed", "Ethnicity",
                                            ifelse(variable=="CLA_NetGain", "Net gain of children by responsible LA",
                                                 ifelse(variable=="CLA_Nrep", "Place providers",
                                                        ifelse(variable=="CLA_Ocom", "Placement",
                                                               ifelse(variable=="CLA_Ores", "Placement",
                                                                      ifelse(variable=="CLA_Oth", "Ethnicity",
                                                                             ifelse(variable=="CLA_OthLA", "Place providers",
                                                                                    ifelse(variable=="CLA_OthPl", "Placement", subcategory))))))))),
                subcategory =  ifelse(variable=="CLA_OthPP", "Place providers",
                                     ifelse(variable=="CLA_Outbound", "Locality of placement",
                                            ifelse(variable=="CLA_OutLA_GT20", "Distance between home and placement and locality of placement",
                                                   ifelse(variable=="CLA_OutLA_LTE20", "Distance between home and placement and locality of placement",
                                                          ifelse(variable=="CLA_OutLA_NoInfo", "Distance between home and placement and locality of placement",
                                                                 ifelse(variable=="CLA_OwnP", "Place providers",
                                                                        ifelse(variable=="CLA_Par", "Place providers",
                                                                               ifelse(variable=="CLA_Parent", "Placement",
                                                                                      ifelse(variable=="CLA_PlaceO", "Legal status", subcategory))))))))),
                subcategory =  ifelse(variable=="CLA_Priv", "Place providers",
                                      ifelse(variable=="CLA_RPC_All_Place", "Reason for placement change during the year",
                                             ifelse(variable=="CLA_RPC_ALLEG", "Reason for placement change during the year",
                                                    ifelse(variable=="CLA_RPC_APPRR", "Reason for placement change during the year",
                                                           ifelse(variable=="CLA_RPC_CARPL", "Reason for placement change during the year",
                                                                  ifelse(variable=="CLA_RPC_CHILD", "Reason for placement change during the year",
                                                                         ifelse(variable=="CLA_RPC_CLOSE", "Reason for placement change during the year",
                                                                                ifelse(variable=="CLA_RPC_CREQB", "Reason for placement change during the year",
                                                                                       ifelse(variable=="CLA_RPC_CREQO", "Reason for placement change during the year",subcategory))))))))),
                subcategory = ifelse(variable=="CLA_RPC_LAREQ", "Reason for placement change during the year",
                                     ifelse(variable=="CLA_RPC_OTHER", "Reason for placement change during the year",
                                            ifelse(variable=="CLA_RPC_PLACE", "Reason for placement change during the year",
                                                   ifelse(variable=="CLA_RPC_STAND", "Reason for placement change during the year",
                                                          ifelse(variable=="CLA_RSch", "Placement",
                                                                 ifelse(variable=="CLA_S20", "Legal status",
                                                                        ifelse(variable=="CLA_Secure", "Placement", subcategory))))))),
                subcategory =  ifelse(variable=="CLA_U1", "Age group",
                                      ifelse(variable=="CLA_UASC", "Unaccompanied asylum-seeking children",
                                             ifelse(variable=="CLA_Vol", "Place providers",
                                                    ifelse(variable=="CLA_White", "Ethnicity",
                                                          ifelse(variable=="CLA_YJLS", "Legal status",subcategory))))),
                variable = ifelse(variable=="CLA_Mar2017", "Total",
                                  ifelse(variable=="CLA_2017", "Total_during",
                                         ifelse(variable=="CLA_stp2017", "Children who were only looked after exclusively under a series of short term placements",
                                                ifelse(variable=="CLA_10to15", "10 to 15 years",
                                                       ifelse(variable=="CLA_16over", "16 years and over",
                                                              ifelse(variable=="CLA_1to4", "1 to 4 years", variable)))))),
                variable = ifelse(variable=="CLA_5to9", "5 to 9 years",
                             ifelse(variable=="CLA_Adopt", "Placed for adoption",
                                    ifelse(variable=="CLA_Asian", "Asian or Asian British",
                                           ifelse(variable=="CLA_Black", "Black African, Caribbean or Black British",
                                                  ifelse(variable=="CLA_CPG", "Detained for child protection",
                                                         ifelse(variable=="CLA_EOTH", "Other ethnic group",
                                                                ifelse(variable=="CLA_ExtPl", "2. Other LA children externally placed within the local authority boundary", variable))))))),
                variable = ifelse(variable=="CLA_FCO", "Full care order",
                             ifelse(variable=="CLA_female", "Female",
                                    ifelse(variable=="CLA_Fost", "Foster placements",
                                           ifelse(variable=="CLA_FrAd", NA,                                                                                                                                    
                                                  ifelse(variable=="CLA_ICO", "Interim care order",
                                                         ifelse(variable=="CLA_InBound", "Placed inside the local authority boundary",
                                                                ifelse(variable=="CLA_InLA_GT20", "3. Placed inside the LA boundary more than 20 miles from home",
                                                                       ifelse(variable=="CLA_InLA_LTE20", "1. Placed inside the LA boundary 20 miles or less from home", variable )))))))),
                variable = ifelse(variable=="CLA_InLA_NoInfo", "5. Placed inside the LA boundary distance not known or not recorded",
                                 ifelse(variable=="CLA_IntPl", "1. Own LA children placed internally within the local authority boundary",
                                        ifelse(variable=="CLA_LAPl", "Placed inside the local authority boundary",
                                               ifelse(variable=="CLA_male", "Male",
                                                      ifelse(variable=="CLA_Mixed", "Mixed or Multiple ethnic groups",
                                                             ifelse(variable=="CLA_NetGain", "Net gain of children by responsible LA",
                                                                    ifelse(variable=="CLA_Nrep", "Placement provider not reported",
                                                                           ifelse(variable=="CLA_Ocom", "Other placements in the community", variable)))))))),
                variable = ifelse(variable=="CLA_Ores", "Other residential settings",
                                 ifelse(variable=="CLA_Oth", "Refused or information not yet available",
                                        ifelse(variable=="CLA_OthLA", "Other LA provision",
                                               ifelse(variable=="CLA_OthPl", "Other placements",
                                                      ifelse(variable=="CLA_OthPP", "Other public provision (e.g. by a PCT etc)",
                                                             ifelse(variable=="CLA_Outbound", "Placed outside the local authority boundary",
                                                                    ifelse(variable=="CLA_OutLA_GT20", "4. Placed outside the LA boundary more than 20 miles from home",
                                                                           ifelse(variable=="CLA_OutLA_LTE20", "2. Placed outside the LA boundary 20 miles or less from home",
                                                                                  ifelse(variable=="CLA_OutLA_NoInfo", "6. Placed outside the LA boundary distance not known or not recorded", variable))))))))),
                variable = ifelse(variable=="CLA_OwnP", "Own provision (by the LA)",
                                  ifelse(variable=="CLA_Par", "Placed with parents or other person with parental responsibility",
                                         ifelse(variable=="CLA_Parent", "Parents or other person with parental responsibility",
                                                ifelse(variable=="CLA_PlaceO", "Placement order granted",
                                                       ifelse(variable=="CLA_Priv", "Private provision",
                                                              ifelse(variable=="CLA_RPC_All_Place", "Total placements changing",
                                                                     ifelse(variable=="CLA_RPC_ALLEG", "Allegation (s47)",
                                                                            ifelse(variable=="CLA_RPC_APPRR", "Approval removed", variable)))))))),
                variable = ifelse(variable=="CLA_RPC_CARPL", "Change to/implementation of care plan",
                                 ifelse(variable=="CLA_RPC_CHILD", "Child requests placement to end",
                                        ifelse(variable=="CLA_RPC_CLOSE", "Resignation or closure of provision",
                                               ifelse(variable=="CLA_RPC_CREQB", "Carer requests placement ends due to childs behaviour",
                                                      ifelse(variable=="CLA_RPC_CREQO", "Carer requests placement end other than due to childs behaviour",
                                                             ifelse(variable=="CLA_RPC_LAREQ", "Responsible/area authority requests placement ends",
                                                                    ifelse(variable=="CLA_RPC_OTHER", "Other",
                                                                           ifelse(variable=="CLA_RPC_PLACE", "Change in the status of a placement only", variable)))))))),
                variable = ifelse(variable=="CLA_RPC_STAND", "Standards of care concern",
                                 ifelse(variable=="CLA_RSch", "Residential schools",
                                        ifelse(variable=="CLA_S20", "Voluntary agreements under S20 CA 1989",
                                               ifelse(variable=="CLA_Secure", "Secure units children's homes and semi-independent living accommodation",
                                                      ifelse(variable=="CLA_U1", "Under 1 year",
                                                             ifelse(variable=="CLA_UASC", "Non-unaccompanied asylum-seeking children",
                                                                    ifelse(variable=="CLA_Vol", "Voluntary/third sector provision",
                                                                           ifelse(variable=="CLA_White", "White",
                                                                                  ifelse(variable=="CLA_YJLS", "Youth justice legal statuses",variable))))))))))

                                                
                                                
  
  


ceased <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2017/SFR50_CEA2017.csv"),
                   colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::rename(LA_Code = New_geog_code,
                LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::select(-geog_l, -LA_order)%>%
  dplyr::mutate(`Special guardianship orders` = as.character(as.numeric(CEA_SGO1)+as.numeric(CEA_SGO2), na.rm=F),
                `Adopted` = as.character(as.numeric(CEA_Adop1)+as.numeric(CEA_Adop2), na.rm=F))%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "CLA_cease2017"])*100)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "ceased during",
                year="2017",
                subcategory = ifelse(variable == "CLA_cease2017", "Total",
                                     ifelse(variable == "Special guardianship orders", "Reason episode ceased",
                                            ifelse(variable=="Adopted", "Reason episode ceased",
                                  ifelse(variable=="CEA_10to15","Age on ceasing",
                                         ifelse(variable=="CEA_16","Age on ceasing",
                                                ifelse(variable=="CEA_17","Age on ceasing",
                                                       ifelse(variable=="CEA_18over","Age on ceasing",
                                                              ifelse(variable=="CEA_1to4","Age on ceasing",
                                                                     ifelse(variable=="CEA_5to9","Age on ceasing",
                                                                            ifelse(variable=="CEA_Abroad","Reason episode ceased",
                                                                                   # ifelse(variable=="CEA_Adop1","",
                                                                                   #   ifelse(variable=="CEA_Adop2","",
                                                                                   ifelse(variable=="CEA_AgeAssmt","Reason episode ceased",
                                                                                          ifelse(variable=="CEA_CAO","Reason episode ceased",
                                                                                                 NA)))))))))))),
                subcategory = ifelse(variable == "CEA_Custody", "Sentenced to custody",
                                  ifelse(variable=="CEA_Died", "Died",
                                         ifelse(variable=="CEA_female", "Gender",
                                                ifelse(variable=="CEA_IndLiv1", "Reason episode ceased",
                                                       ifelse(variable=="CEA_IndLiv2", "Reason episode ceased",
                                                              ifelse(variable=="CEA_male", "Gender",
                                                                     ifelse(variable=="CEA_NoPar", "Reason episode ceased",
                                                                            ifelse(variable=="CEA_Other", "Reason episode ceased",
                                                                                   ifelse(variable=="CEA_ParNPlan", "Reason episode ceased",
                                                                                          ifelse(variable=="CEA_ParPlan", "Reason episode ceased",
                                                                                                 ifelse(variable=="CEA_RemEnd", "Reason episode ceased",
                                                                                                        ifelse(variable=="CEA_Residential", "Reason episode ceased",
                                                                                                               subcategory)))))))))))),
                subcategory = #ifelse(variable=="CEA_SGO1","",
                  #      ifelse(variable=="CEA_SGO2","",
                  ifelse(variable=="CEA_Taken","Reason episode ceased",
                         ifelse(variable=="CEA_U1","Age on ceasing",
                                subcategory)),
                variable = ifelse(variable == "CLA_cease2017", "Total",
                                  ifelse(variable=="CEA_10to15","10 to 15 years",
                                         ifelse(variable=="CEA_16","16 years",
                                                ifelse(variable=="CEA_17","17 years",
                                                       ifelse(variable=="CEA_18over","18 years and over",
                                                              ifelse(variable=="CEA_1to4","1 to 4 years",
                                                                     ifelse(variable=="CEA_5to9","5 to 9 years",
                                                                            ifelse(variable=="CEA_Abroad","Child moved abroad",
                                                                                  # ifelse(variable=="CEA_Adop1","",
                                                                                       #   ifelse(variable=="CEA_Adop2","",
                                                                                                 ifelse(variable=="CEA_AgeAssmt","Age assessment determined child aged 18 or over",
                                                                                                        ifelse(variable=="CEA_CAO","Residence order or child arrangement order granted",
                                                                                                        variable)))))))))),
                variable = ifelse(variable == "CEA_Custody", "Sentenced to custody",
                                  ifelse(variable=="CEA_Died", "Died",
                                         ifelse(variable=="CEA_female", "Female",
                                                ifelse(variable=="CEA_IndLiv1", "Moved into independent living (with supportive accommodation)",
                                                       ifelse(variable=="CEA_IndLiv2", "Moved into independent living (with no formalised support)",
                                                              ifelse(variable=="CEA_male", "Male",
                                                                     ifelse(variable=="CEA_NoPar", "Left care to live with parents relatives or other person with no parental responsibility",
                                                                            ifelse(variable=="CEA_Other", "Care ceased for any other reason",
                                                                                   ifelse(variable=="CEA_ParNPlan", "Returned home to live with parents or other person with parental responsibility which was not part of the care planning process",
                                                                                          ifelse(variable=="CEA_ParPlan", "Returned home to live with parents or other person with parental responsibility which was part of the care planning process",
                                                                                                 ifelse(variable=="CEA_RemEnd", "Accommodation on remand ended",
                                                                                                        ifelse(variable=="CEA_Residential", "Transferred to residential care funded by adult social services",
                                                                                                        variable)))))))))))),
                variable = #ifelse(variable=="CEA_SGO1","",
                            #      ifelse(variable=="CEA_SGO2","",
                                         ifelse(variable=="CEA_Taken","Care taken by another local authority",
                                                ifelse(variable=="CEA_U1","Under 1 year",
                                                variable)))%>%
  dplyr::filter(variable!="CEA_SGO1",
                variable!="CEA_SGO2",
                variable!="CEA_Adop1",
                variable!="CEA_Adop2")






leaver17 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2017/SFR50_CareLeavers17182017.csv"),
                     colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::rename(LA_Code = New_geog_code,
                LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::select(LA_Code, LA.Number, LA_Name, tidyr::ends_with("17.18"))%>%#
  dplyr::mutate(`Total in education employment or training` = as.character(as.numeric(CL_Act_HE17.18)+ as.numeric(CL_Act_OE17.18)+as.numeric(CL_Act_TE17.18)))%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "CL_All_17.18"])*100)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "care leavers",
                year="2017",
                subcategory = "17 to 18 years",
                variable = ifelse(variable=="CL_Acc_BB17.18", "Bed and breakfast",
                                  ifelse(variable=="CL_Acc_CH17.18", "Community home",
                                         ifelse(variable=="CL_Acc_Cust17.18", "In custody",
                                                ifelse(variable=="CL_Acc_Dep17.18", "Deported",
                                                       ifelse(variable=="CL_Acc_EA17.18", "Emergency accommodation",
                                                              ifelse(variable=="CL_Acc_F17.18", "Foyers",
                                                                     ifelse(variable=="CL_Acc_FFC17.18", "With former foster carers",
                                                                            ifelse(variable=="CL_Acc_GA17.18", "Gone abroad",
                                                                                   variable)))))))),
                variable = ifelse(variable=="CL_Acc_IL17.18", "Independent living",
                                  ifelse(variable=="CL_Acc_NFA17.18","No fixed abode/homeless",
                                         ifelse(variable=="CL_Acc_NK17.18","Residence not known",
                                                ifelse(variable=="CL_Acc_NoInf17.18","Total information not known",
                                                       ifelse(variable=="CL_Acc_NoSUITInfo17.18","No information",
                                                              ifelse(variable=="CL_Acc_NotSUIT17.18","Accommodation considered not suitable",
                                                                     ifelse(variable=="CL_Acc_OL17.18","Ordinary lodgings",
                                                                            ifelse(variable=="CL_Acc_OTH17.18","Other accommodation",
                                                                            variable)))))))),
                variable = ifelse(variable=="CL_Acc_P17.18", "With parents or relatives",
                                  ifelse(variable=="CL_Acc_SITA17.18","Semi-independent transitional accommodation",
                                         ifelse(variable=="CL_Acc_SL17.18","Supported lodgings",
                                                ifelse(variable=="CL_Acc_SUIT17.18","Accommodation considered suitable",
                                                       ifelse(variable=="CL_Act_HE17.18","Higher education i.e. studies beyond A level",
                                                              ifelse(variable=="CL_Act_NEET_ill17.18","Not in education training or employment, owing to illness or disability",
                                                                     ifelse(variable=="CL_Act_NEET_oth17.18","Not in education training or employment, owing to other reasons",
                                                                            ifelse(variable=="CL_Act_NEET_preg17.18","Not in education training or employment, owing to pregnancy or parenting",
                                                                                   variable)))))))),
                variable = ifelse(variable=="CL_Act_NoInf17.18", "Total information not known",
                                  ifelse(variable=="CL_Act_OE17.18","Education other than higher education",
                                         ifelse(variable=="CL_All_17.18","Total",
                                                ifelse(variable=="CL_Acc_SUIT17.18","Accommodation considered suitable",
                                                       ifelse(variable=="CL_InTouch_IT17.18","Local authority in touch with care leaver",
                                                              ifelse(variable=="CL_InTouch_NoServ17.18","Young person no longer requires services",
                                                                     ifelse(variable=="CL_InTouch_Not17.18","Local authority not in touch with care leaver",
                                                                            ifelse(variable=="CL_InTouch_Refu17.18","Young person refuses contact",
                                                                                   ifelse(variable=="CL_Act_TE17.18","In training or employment",
                                                                                    variable))))))))),
                )
  




leaver19 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2017/SFR50_CareLeavers19to212017.csv"),
                     colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::rename(LA_Code = New_geog_code,
                LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::select(LA_Code, LA.Number, LA_Name, tidyr::ends_with("19to21"))%>%#
  dplyr::mutate(`Total in education employment or training` = as.character(as.numeric(CL_Act_HE19to21)+ as.numeric(CL_Act_OE19to21)+as.numeric(CL_Act_TE19to21)))%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "CL_All_19to21"])*100)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "care leavers",
                year="2017",
                subcategory = "19 to 21 years",
                variable = ifelse(variable=="CL_Acc_BB19to21", "Bed and breakfast",
                                  ifelse(variable=="CL_Acc_CH19to21", "Community home",
                                         ifelse(variable=="CL_Acc_Cust19to21", "In custody",
                                                ifelse(variable=="CL_Acc_Dep19to21", "Deported",
                                                       ifelse(variable=="CL_Acc_EA19to21", "Emergency accommodation",
                                                              ifelse(variable=="CL_Acc_F19to21", "Foyers",
                                                                     ifelse(variable=="CL_Acc_FFC19to21", "With former foster carers",
                                                                            ifelse(variable=="CL_Acc_GA19to21", "Gone abroad",
                                                                                   variable)))))))),
                variable = ifelse(variable=="CL_Acc_IL19to21", "Independent living",
                                  ifelse(variable=="CL_Acc_NFA19to21","No fixed abode/homeless",
                                         ifelse(variable=="CL_Acc_NK19to21","Residence not known",
                                                ifelse(variable=="CL_Acc_NoInf19to21","Total information not known",
                                                       ifelse(variable=="CL_Acc_NoSUITInfo19to21","No information",
                                                              ifelse(variable=="CL_Acc_NotSUIT19to21","Accommodation considered not suitable",
                                                                     ifelse(variable=="CL_Acc_OL19to21","Ordinary lodgings",
                                                                            ifelse(variable=="CL_Acc_OTH19to21","Other accommodation",
                                                                                   variable)))))))),
                variable = ifelse(variable=="CL_Acc_P19to21", "With parents or relatives",
                                  ifelse(variable=="CL_Acc_SITA19to21","Semi-independent transitional accommodation",
                                         ifelse(variable=="CL_Acc_SL19to21","Supported lodgings",
                                                ifelse(variable=="CL_Acc_SUIT19to21","Accommodation considered suitable",
                                                       ifelse(variable=="CL_Act_HE19to21","Higher education i.e. studies beyond A level",
                                                              ifelse(variable=="CL_Act_NEET_ill19to21","Not in education training or employment, owing to illness or disability",
                                                                     ifelse(variable=="CL_Act_NEET_oth19to21","Not in education training or employment, owing to other reasons",
                                                                            ifelse(variable=="CL_ACT_NEET_preg19to21","Not in education training or employment, owing to pregnancy or parenting",
                                                                                   variable)))))))),
                variable = ifelse(variable=="CL_Act_NoInf19to21", "Total information not known",
                                  ifelse(variable=="CL_Act_OE19to21","Education other than higher education",
                                         ifelse(variable=="CL_All_19to21","Total",
                                                ifelse(variable=="CL_Acc_SUIT19to21","Accommodation considered suitable",
                                                       ifelse(variable=="CL_InTouch_IT19to21","Local authority in touch with care leaver",
                                                              ifelse(variable=="CL_InTouch_NoServ19to21","Young person no longer requires services",
                                                                     ifelse(variable=="CL_InTouch_Not19to21","Local authority not in touch with care leaver",
                                                                            ifelse(variable=="CL_InTouch_Refu19to21","Young person refuses contact",
                                                                                   ifelse(variable=="CL_Act_TE19to21","In training or employment",
                                                                                          variable))))))))),
  )










characteristics <- rbind(characteristics, admitted, march, ceased, leaver17, leaver19)
  
####2016####
admitted <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2016/SFR41_ADM2016.csv"),
                     colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::rename(LA_Code = New_geog_code,
                LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::select(-geog_l)%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "CLA_started2016"])*100)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "started during",
                year="2016",
                subcategory = ifelse(variable=="CLA_started2016", "Taken into care",
                                     ifelse(variable=="CLA_taken2016", "Taken into care",
                                            ifelse(variable=="SCLA_10to15", "Age group",
                                                   ifelse(variable=="SCLA_16over", "Age group",
                                                          ifelse(variable=="SCLA_1to4", "Age group",
                                                                 ifelse(variable=="SCLA_5to9", "Age group",
                                                                        ifelse(variable=="SCLA_AbNeg", "Category of need",
                                                                               ifelse(variable=="SCLA_AbsPar", "Category of need",
                                                                                      ifelse(variable=="SCLA_Cdisab", "Category of need",
                                                                                             ifelse(variable=="SCLA_FAcSt", "Category of need",
                                                                                                    ifelse(variable=="SCLA_FCO", "Legal status",
                                                                                                           ifelse(variable=="SCLA_FD", "Category of need",
                                                                                                                  ifelse(variable=="SCLA_female", "Gender",
                                                                                                                         ifelse(variable=="SCLA_ICO", "Legal status",
                                                                                                                                ifelse(variable=="SCLA_LI", "Category of need",
                                                                                                                                       ifelse(variable=="SCLA_male", "Gender",
                                                                                                                                              ifelse(variable=="SCLA_ONCT", "Legal status",
                                                                                                                                                     ifelse(variable=="SCLA_PACE", "Legal status",
                                                                                                                                                            ifelse(variable=="SCLA_ParIll", "Category of need",
                                                                                                                                                                   ifelse(variable=="SCLA_POG", "Legal status",
                                                                                                                                                                          ifelse(variable=="SCLA_S20", "Legal status",
                                                                                                                                                                                 ifelse(variable=="SCLA_SEPO", "Legal status",
                                                                                                                                                                                        ifelse(variable=="SCLA_SORR", NA,
                                                                                                                                                                                               ifelse(variable=="SCLA_SUB", "Category of need",
                                                                                                                                                                                                      ifelse(variable=="SCLA_U1", "Age group",
                                                                                                                                                                                                             ifelse(variable=="SCLA_UCAO", "Legal status",
                                                                                                                                                                                                                    ifelse(variable=="SCLA_UPP",NA, NA))))))))))))))))))))))))))),
                variable = ifelse(variable=="CLA_started2016", "Total children",
                                  ifelse(variable=="CLA_taken2016", "All children taken into care",
                                         ifelse(variable=="SCLA_10to15", "10 to 15 years",
                                                ifelse(variable=="SCLA_16over", "16 years and over",
                                                       ifelse(variable=="SCLA_1to4", "1 to 4 years",
                                                              ifelse(variable=="SCLA_5to9", "5 to 9 years",
                                                                     ifelse(variable=="SCLA_AbNeg", "N1. Abuse or neglect",
                                                                            ifelse(variable=="SCLA_AbsPar", "N8. Absent parenting",
                                                                                   ifelse(variable=="SCLA_Cdisab", "N2. Child's disability",
                                                                                          ifelse(variable=="SCLA_FAcSt", "N.4 Family acute stress",
                                                                                                 ifelse(variable=="SCLA_FCO", "Full care order",
                                                                                                        ifelse(variable=="SCLA_FD", "N5. Family dysfunction",
                                                                                                               ifelse(variable=="SCLA_female", "Female",
                                                                                                                      ifelse(variable=="SCLA_ICO", "Interm care order",
                                                                                                                             ifelse(variable=="SCLA_LI", "N7. Low income",
                                                                                                                                    ifelse(variable=="SCLA_male", "Male",
                                                                                                                                           ifelse(variable=="SCLA_ONCT", "Remand",
                                                                                                                                                  ifelse(variable=="SCLA_PACE", "LA accommodation under PACE 1989",
                                                                                                                                                         ifelse(variable=="SCLA_ParIll", "N3. Parental illness or disability",
                                                                                                                                                                ifelse(variable=="SCLA_POG", "Placement order granted",
                                                                                                                                                                       ifelse(variable=="SCLA_S20", "Voluntary agreement under S20",
                                                                                                                                                                              ifelse(variable=="SCLA_SEPO", "Emergency protction order",
                                                                                                                                                                                     ifelse(variable=="SCLA_SORR", NA,
                                                                                                                                                                                            ifelse(variable=="SCLA_SUB", "N6. Socially unacceptable behaviour",
                                                                                                                                                                                                   ifelse(variable=="SCLA_U1", "Under 1 year",
                                                                                                                                                                                                          ifelse(variable=="SCLA_UCAO", "Child assessment order and in LA accommodation",
                                                                                                                                                                                                                 ifelse(variable=="SCLA_UPP",NA, NA))))))))))))))))))))))))))))








march <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2016/SFR41_CLA2016.csv"),
                  colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::rename(LA_Code = New_geog_code,
                LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::select(-geog_l)%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "CLA_Mar2016"])*100))%>%
  dplyr::mutate(percent = ifelse(variable=="CLA_2016", 100, percent))%>%
  dplyr::mutate(percent = ifelse(variable=="CLA_stp2016", as.numeric(number)/as.numeric(number[variable=="CLA_2016"])*100, percent))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "child characteristic at 31st March",
                year="2016",
                subcategory = ifelse(variable=="CLA_Mar2016", "Age group",
                                     ifelse(variable=="CLA_2016", "Age group",
                                            ifelse(variable=="CLA_stp2016", "Age group",
                                                   ifelse(variable=="CLA_10to15", "Age group",
                                                          ifelse(variable=="CLA_16over", "Age group",
                                                                 ifelse(variable=="CLA_1to4", "Age group",NA)))))),
                subcategory = ifelse(variable=="CLA_5to9", "Age group",
                                     ifelse(variable=="CLA_Adopt", "Placement",
                                            ifelse(variable=="CLA_Asian", "Ethnicity",
                                                   ifelse(variable=="CLA_Black", "Ethnicity",
                                                          ifelse(variable=="CLA_CPG", "Legal status",
                                                                 ifelse(variable=="CLA_EOTH", "Ethnicity", subcategory)))))),
                subcategory = ifelse(variable=="CLA_ExtPl", "LA of placement",
                                     ifelse(variable=="CLA_FCO", "Legal status",
                                            ifelse(variable=="CLA_female", "Gender",
                                                   ifelse(variable=="CLA_Fost", "Placement",
                                                          ifelse(variable=="CLA_FrAd", NA,                                                                                                                                    
                                                                 ifelse(variable=="CLA_ICO", "Legal status", subcategory)))))),
                subcategory = ifelse(variable=="CLA_InBound", "Locality of placement",
                                                          ifelse(variable=="CLA_IntPl", "LA of placement",
                                                                 ifelse(variable=="CLA_LAPl", "Placed inside the local authority boundary",subcategory))),
                subcategory = ifelse(variable=="CLA_male", "Gender",
                                     ifelse(variable=="CLA_Mixed", "Ethnicity",
                                            ifelse(variable=="CLA_NetGain", "Net gain of children by responsible LA",
                                                   ifelse(variable=="CLA_Nrep", "Place providers",
                                                          ifelse(variable=="CLA_Ocom", "Placement",
                                                                 ifelse(variable=="CLA_Ores", "Placement",
                                                                        ifelse(variable=="CLA_Oth", "Ethnicity",
                                                                               ifelse(variable=="CLA_OthLA", "Place providers",
                                                                                      ifelse(variable=="CLA_OthPl", "Placement", subcategory))))))))),
                subcategory =  ifelse(variable=="CLA_OthPP", "Place providers",
                                      ifelse(variable=="CLA_Outbound", "Locality of placement",
                                             ifelse(variable=="CLA_OwnP", "Place providers",
                                                                         ifelse(variable=="CLA_Par", "Place providers",
                                                                                ifelse(variable=="CLA_Parent", "Placement",
                                                                                       ifelse(variable=="CLA_PlaceO", "Legal status", subcategory)))))),
                subcategory =  ifelse(variable=="CLA_Priv", "Place providers", subcategory),
                subcategory = ifelse(variable=="CLA_RSch", "Placement",
                                                                 ifelse(variable=="CLA_S20", "Legal status",
                                                                        ifelse(variable=="CLA_Secure", "Placement", subcategory))),
                subcategory =  ifelse(variable=="CLA_U1", "Age group",
                                      ifelse(variable=="CLA_UASC", "Unaccompanied asylum-seeking children",
                                             ifelse(variable=="CLA_Vol", "Place providers",
                                                    ifelse(variable=="CLA_White", "Ethnicity",
                                                           ifelse(variable=="CLA_YJLS", "Legal status",subcategory))))),
                variable = ifelse(variable=="CLA_Mar2016", "Total",
                                  ifelse(variable=="CLA_2016", "Total_during",
                                         ifelse(variable=="CLA_stp2016", "Children who were only looked after exclusively under a series of short term placements",
                                                ifelse(variable=="CLA_10to15", "10 to 15 years",
                                                       ifelse(variable=="CLA_16over", "16 years and over",
                                                              ifelse(variable=="CLA_1to4", "1 to 4 years", variable)))))),
                variable = ifelse(variable=="CLA_5to9", "5 to 9 years",
                                  ifelse(variable=="CLA_Adopt", "Placed for adoption",
                                         ifelse(variable=="CLA_Asian", "Asian or Asian British",
                                                ifelse(variable=="CLA_Black", "Black African, Caribbean or Black British",
                                                       ifelse(variable=="CLA_CPG", "Detained for child protection",
                                                              ifelse(variable=="CLA_EOTH", "Other ethnic group",
                                                                     ifelse(variable=="CLA_ExtPl", "2. Other LA children externally placed within the local authority boundary", variable))))))),
                variable = ifelse(variable=="CLA_FCO", "Full care order",
                                  ifelse(variable=="CLA_female", "Female",
                                         ifelse(variable=="CLA_Fost", "Foster placements",
                                                ifelse(variable=="CLA_FrAd", NA,                                                                                                                                    
                                                       ifelse(variable=="CLA_ICO", "Interim care order",
                                                              ifelse(variable=="CLA_InBound", "Placed inside the local authority boundary",variable)))))),
                variable =               ifelse(variable=="CLA_LAPl", "Placed inside the local authority boundary",
                                                ifelse(variable=="CLA_male", "Male",
                                                       ifelse(variable=="CLA_Mixed", "Mixed or Multiple ethnic groups",
                                                              ifelse(variable=="CLA_NetGain", "Net gain of children by responsible LA",
                                                                     ifelse(variable=="CLA_Nrep", "Placement provider not reported",
                                                                            ifelse(variable=="CLA_Ocom", "Other placements in the community", variable)))))),
                variable = ifelse(variable=="CLA_Ores", "Other residential settings",
                                  ifelse(variable=="CLA_Oth", "Refused or information not yet available",
                                         ifelse(variable=="CLA_OthLA", "Other LA provision",
                                                ifelse(variable=="CLA_OthPl", "Other placements",
                                                       ifelse(variable=="CLA_OthPP", "Other public provision (e.g. by a PCT etc)",
                                                              ifelse(variable=="CLA_Outbound", "Placed outside the local authority boundary", variable)))))),
                variable = ifelse(variable=="CLA_OwnP", "Own provision (by the LA)",
                                  ifelse(variable=="CLA_Par", "Placed with parents or other person with parental responsibility",
                                         ifelse(variable=="CLA_Parent", "Parents or other person with parental responsibility",
                                                ifelse(variable=="CLA_PlaceO", "Placement order granted",
                                                       ifelse(variable=="CLA_Priv", "Private provision", variable))))),
                variable =       ifelse(variable=="CLA_RSch", "Residential schools",
                                         ifelse(variable=="CLA_S20", "Voluntary agreements under S20 CA 1989",
                                                ifelse(variable=="CLA_Secure", "Secure units children's homes and semi-independent living accommodation",
                                                       ifelse(variable=="CLA_U1", "Under 1 year",
                                                              ifelse(variable=="CLA_UASC", "Non-unaccompanied asylum-seeking children",
                                                                     ifelse(variable=="CLA_Vol", "Voluntary/third sector provision",
                                                                            ifelse(variable=="CLA_White", "White",
                                                                                   ifelse(variable=="CLA_YJLS", "Youth justice legal statuses",variable)))))))))




ceased <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2016/SFR41_CEA2016.csv"),
                   colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::rename(LA_Code = New_geog_code,
                LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::select(-geog_l)%>%
  dplyr::mutate(`Special guardianship orders` = as.character(as.numeric(CLA_ceaSpecG1)+as.numeric(CLA_ceaSpecG2), na.rm=F),
                `Adopted` = as.character(as.numeric(CLA_ceaAdop1)+as.numeric(CLA_ceaAdop2), na.rm=F))%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "CLA_cease"])*100)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "ceased during",
                year="2016",
                subcategory = ifelse(variable == "CLA_cease", "Age on ceasing",
                                     ifelse(variable=="Special guardianship orders","Reason episode ceased",
                                            ifelse(variable== "Adopted", "Reason episode ceased",
                                  ifelse(variable=="CLA_cea1015","Age on ceasing",
                                         ifelse(variable=="CLA_cea16","Age on ceasing",
                                                ifelse(variable=="CLA_cea17","Age on ceasing",
                                                       ifelse(variable=="CLA_cea18","Age on ceasing",
                                                              ifelse(variable=="CLA_cea14","Age on ceasing",
                                                                     ifelse(variable=="CLA_cea59","Age on ceasing",
                                                                            ifelse(variable=="CLA_ceaAbroad","Reason episode ceased",
                                                                                   # ifelse(variable=="CEA_Adop1","",
                                                                                   #   ifelse(variable=="CEA_Adop2","",
                                                                                   ifelse(variable=="CLA_ceaAgeAssmt","Reason episode ceased",
                                                                                          ifelse(variable=="CLA_ceaROG","Reason episode ceased",
                                                                                                 NA)))))))))))),
                subcategory = ifelse(variable == "CLA_cea_sen_cust", "Reason episode ceased",
                                  ifelse(variable=="CLA_ceaDied", "Reason episode ceased",
                                         ifelse(variable=="CLA_ceafe", "Gender",
                                                ifelse(variable=="CLA_ceaIndLiv1", "Reason episode ceased",
                                                       ifelse(variable=="CLA_ceaIndLiv2", "Reason episode ceased",
                                                              ifelse(variable=="CLA_ceamal", "Gender",
                                                                     ifelse(variable=="CLA_ceaNoPar", "Reason episode ceased",
                                                                            ifelse(variable=="CLA_cea_OthRea", "Reason episode ceased",
                                                                                   ifelse(variable=="CLA_ceaParNPlan", "Reason episode ceased",
                                                                                          ifelse(variable=="CLA_ceaParPlan", "Reason episode ceased",
                                                                                                 ifelse(variable=="CLA_ceaRemEnd", "Reason episode ceased",
                                                                                                        ifelse(variable=="CLA_cea_tran_res", "Reason episode ceased",
                                                                                                               subcategory)))))))))))),
                subcategory = #ifelse(variable=="CEA_SGO1","",
                  #      ifelse(variable=="CEA_SGO2","",
                  ifelse(variable=="CLA_ceataken","Reason episode ceased",
                         ifelse(variable=="CLA_cea1","Age on ceasing",
                                subcategory)),
                variable = ifelse(variable == "CLA_cease", "Total",
                                  ifelse(variable=="CLA_cea1015","10 to 15 years",
                                         ifelse(variable=="CLA_cea16","16 years",
                                                ifelse(variable=="CLA_cea17","17 years",
                                                       ifelse(variable=="CLA_cea18","18 years and over",
                                                              ifelse(variable=="CLA_cea14","1 to 4 years",
                                                                     ifelse(variable=="CLA_cea59","5 to 9 years",
                                                                            ifelse(variable=="CLA_ceaAbroad","Child moved abroad",
                                                                                   # ifelse(variable=="CEA_Adop1","",
                                                                                   #   ifelse(variable=="CEA_Adop2","",
                                                                                   ifelse(variable=="CLA_ceaAgeAssmt","Age assessment determined child aged 18 or over",
                                                                                          ifelse(variable=="CLA_ceaROG","Residence order or child arrangement order granted",
                                                                                                 variable)))))))))),
                variable = ifelse(variable == "CLA_cea_sen_cust", "Sentenced to custody",
                                  ifelse(variable=="CLA_ceaDied", "Died",
                                         ifelse(variable=="CLA_ceafe", "Female",
                                                ifelse(variable=="CLA_ceaIndLiv1", "Moved into independent living (with supportive accommodation)",
                                                       ifelse(variable=="CLA_ceaIndLiv2", "Moved into independent living (with no formalised support)",
                                                              ifelse(variable=="CLA_ceamal", "Male",
                                                                     ifelse(variable=="CLA_ceaNoPar", "Left care to live with parents relatives or other person with no parental responsibility",
                                                                            ifelse(variable=="CLA_cea_OthRea", "Care ceased for any other reason",
                                                                                   ifelse(variable=="CLA_ceaParNPlan", "Returned home to live with parents or other person with parental responsibility which was not part of the care planning process",
                                                                                          ifelse(variable=="CLA_ceaParPlan", "Returned home to live with parents or other person with parental responsibility which was part of the care planning process",
                                                                                                 ifelse(variable=="CLA_ceaRemEnd", "Accommodation on remand ended",
                                                                                                        ifelse(variable=="CLA_cea_tran_res", "Transferred to residential care funded by adult social services",
                                                                                                               variable)))))))))))),
                variable = #ifelse(variable=="CEA_SGO1","",
                  #      ifelse(variable=="CEA_SGO2","",
                  ifelse(variable=="CLA_ceataken","Care taken by another local authority",
                         ifelse(variable=="CLA_cea1","Under 1 year",
                                variable)))%>%
  dplyr::filter(variable!="CLA_ceaAdop1",
                variable!="CLA_ceaAdop2",
                variable!="CLA_ceaSpecG1",
                variable!="CLA_ceaSpecG2")







leavers <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2016/SFR41_CareLeavers2016.csv"),
                     colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::rename(LA_Code = New_geog_code,
                LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::select(LA_Code, LA.Number, LA_Name, (tidyr::ends_with("17.18")|tidyr::ends_with("1920.21")))%>%#
  #dplyr::mutate(`Total in education employment or training` = as.character(as.numeric(CL_Act_HE17.18)+ as.numeric(CL_Act_OE17.18)+as.numeric(CL_Act_TE17.18)))%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = ifelse(grepl('17.18$', variable), (as.character(as.numeric(number) / as.numeric(number[variable == "All_aged17.18"])*100)),
                                 ifelse(grepl('1920.21$', variable), as.character(as.numeric(number) / as.numeric(number[variable == "All_aged1920.21"])*100),NA))) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "care leavers",
                year="2016",
                subcategory = ifelse(grepl('17.18$', variable), "17 to 18 years",
                                  ifelse(grepl('1920.21$', variable), "19 to 21 years", NA)),
                variable = gsub("17.18", "", variable),
                variable = gsub("1920.21", "", variable),
                variable = ifelse(variable=="Acc_BB", "Bed and breakfast",
                                  ifelse(variable=="Acc_CH", "Community home",
                                         ifelse(variable=="Acc_Cust", "In custody",
                                                ifelse(variable=="Acc_Dep", "Deported",
                                                       ifelse(variable=="Acc_EA", "Emergency accommodation",
                                                              ifelse(variable=="Acc_F", "Foyers",
                                                                     ifelse(variable=="Acc_FFC", "With former foster carers",
                                                                            ifelse(variable=="Acc_GA", "Gone abroad",
                                                                                   variable)))))))),
                variable = ifelse(variable=="Acc_IL", "Independent living",
                                  ifelse(variable=="Acc_NFA","No fixed abode/homeless",
                                         ifelse(variable=="Acc_ResNk","Residence not known",
                                                ifelse(variable=="Acc_NoInf","Total information not known",
                                                       ifelse(variable=="Acc_NoSUITInfo","No information",
                                                              ifelse(variable=="Acc_NotSUIT","Accommodation considered not suitable",
                                                                     ifelse(variable=="Acc_OL","Ordinary lodgings",
                                                                            ifelse(variable=="Acc_OTH","Other accommodation",
                                                                                   variable)))))))),
                variable = ifelse(variable=="Acc_P", "With parents or relatives",
                                  ifelse(variable=="Acc_SITA","Semi-independent transitional accommodation",
                                         ifelse(variable=="Acc_SL","Supported lodgings",
                                                ifelse(variable=="Acc_SUIT","Accommodation considered suitable",
                                                       ifelse(variable=="Act_HE","Higher education i.e. studies beyond A level",
                                                              ifelse(variable=="Act_NEET_ill","Not in education training or employment, owing to illness or disability",
                                                                     ifelse(variable=="Act_NEET_oth","Not in education training or employment, owing to other reasons",
                                                                            ifelse(variable=="Act_NEET_preg","Not in education training or employment, owing to pregnancy or parenting",
                                                                                   variable)))))))),
                variable = ifelse(variable=="LA_NoActinf", "Total information not known",
                                  ifelse(variable=="Act_OE","Education other than higher education",
                                         ifelse(variable=="All_aged","Total",
                                                ifelse(variable=="Acc_SUIT","Accommodation considered suitable",
                                                       ifelse(variable=="InTouch_IT","Local authority in touch with care leaver",
                                                              ifelse(variable=="InTouch_NoServ","Young person no longer requires services",
                                                                     ifelse(variable=="InTouch_Not","Local authority not in touch with care leaver",
                                                                            ifelse(variable=="InTouch_Refu","Young person refuses contact",
                                                                                   ifelse(variable=="Act_TE","In training or employment",
                                                                                          variable))))))))))
  




characteristics <- rbind(characteristics, admitted, march, ceased, leavers)


####2015####
admitted <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2015/SFR34_ADM2015.csv"),
                     colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::rename(LA_Code = New_geog_code,
                LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::select(-geog_l)%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "CLA_started2014"])*100)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "started during",
                year="2015",
                subcategory = ifelse(variable=="CLA_started2014", "Taken into care",
                                     ifelse(variable=="CLA_taken2014", "Taken into care",
                                            ifelse(variable=="SCLA_10to15", "Age group",
                                                   ifelse(variable=="SCLA_16over", "Age group",
                                                          ifelse(variable=="SCLA_1to4", "Age group",
                                                                 ifelse(variable=="SCLA_5to9", "Age group",
                                                                        ifelse(variable=="SCLA_AbNeg", "Category of need",
                                                                               ifelse(variable=="SCLA_AbsPar", "Category of need",
                                                                                      ifelse(variable=="SCLA_Cdisab", "Category of need",
                                                                                             ifelse(variable=="SCLA_FAcSt", "Category of need",
                                                                                                    ifelse(variable=="SCLA_FCO", "Legal status",
                                                                                                           ifelse(variable=="SCLA_FD", "Category of need",
                                                                                                                  ifelse(variable=="SCLA_female", "Gender",
                                                                                                                         ifelse(variable=="SCLA_ICO", "Legal status",
                                                                                                                                ifelse(variable=="SCLA_LI", "Category of need",
                                                                                                                                       ifelse(variable=="SCLA_male", "Gender",
                                                                                                                                              ifelse(variable=="SCLA_ONCT", "Legal status",
                                                                                                                                                     ifelse(variable=="SCLA_PACE", "Legal status",
                                                                                                                                                            ifelse(variable=="SCLA_ParIll", "Category of need",
                                                                                                                                                                   ifelse(variable=="SCLA_POG", "Legal status",
                                                                                                                                                                          ifelse(variable=="SCLA_S20", "Legal status",
                                                                                                                                                                                 ifelse(variable=="SCLA_SEPO", "Legal status",
                                                                                                                                                                                        ifelse(variable=="SCLA_SORR", NA,
                                                                                                                                                                                               ifelse(variable=="SCLA_SUB", "Category of need",
                                                                                                                                                                                                      ifelse(variable=="SCLA_U1", "Age group",
                                                                                                                                                                                                             ifelse(variable=="SCLA_UCAO", "Legal status",
                                                                                                                                                                                                                    ifelse(variable=="SCLA_UPP",NA, NA))))))))))))))))))))))))))),
                variable = ifelse(variable=="CLA_started2014", "Total children",
                                  ifelse(variable=="CLA_taken2014", "All children taken into care",
                                         ifelse(variable=="SCLA_10to15", "10 to 15 years",
                                                ifelse(variable=="SCLA_16over", "16 years and over",
                                                       ifelse(variable=="SCLA_1to4", "1 to 4 years",
                                                              ifelse(variable=="SCLA_5to9", "5 to 9 years",
                                                                     ifelse(variable=="SCLA_AbNeg", "N1. Abuse or neglect",
                                                                            ifelse(variable=="SCLA_AbsPar", "N8. Absent parenting",
                                                                                   ifelse(variable=="SCLA_Cdisab", "N2. Child's disability",
                                                                                          ifelse(variable=="SCLA_FAcSt", "N.4 Family acute stress",
                                                                                                 ifelse(variable=="SCLA_FCO", "Full care order",
                                                                                                        ifelse(variable=="SCLA_FD", "N5. Family dysfunction",
                                                                                                               ifelse(variable=="SCLA_female", "Female",
                                                                                                                      ifelse(variable=="SCLA_ICO", "Interm care order",
                                                                                                                             ifelse(variable=="SCLA_LI", "N7. Low income",
                                                                                                                                    ifelse(variable=="SCLA_male", "Male",
                                                                                                                                           ifelse(variable=="SCLA_ONCT", "Remand",
                                                                                                                                                  ifelse(variable=="SCLA_PACE", "LA accommodation under PACE 1989",
                                                                                                                                                         ifelse(variable=="SCLA_ParIll", "N3. Parental illness or disability",
                                                                                                                                                                ifelse(variable=="SCLA_POG", "Placement order granted",
                                                                                                                                                                       ifelse(variable=="SCLA_S20", "Voluntary agreement under S20",
                                                                                                                                                                              ifelse(variable=="SCLA_SEPO", "Emergency protction order",
                                                                                                                                                                                     ifelse(variable=="SCLA_SORR", NA,
                                                                                                                                                                                            ifelse(variable=="SCLA_SUB", "N6. Socially unacceptable behaviour",
                                                                                                                                                                                                   ifelse(variable=="SCLA_U1", "Under 1 year",
                                                                                                                                                                                                          ifelse(variable=="SCLA_UCAO", "Child assessment order and in LA accommodation",
                                                                                                                                                                                                                 ifelse(variable=="SCLA_UPP",NA, NA))))))))))))))))))))))))))))


march <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2015/SFR34_CLA2015.csv"),
                  colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::rename(LA_Code = New_geog_code,
                LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::select(-geog_l)%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "CLA_Mar2015"])*100))%>%
  dplyr::mutate(percent = ifelse(variable=="CLA_2015", 100, percent))%>%
  dplyr::mutate(percent = ifelse(variable=="CLA_stp2015", as.numeric(number)/as.numeric(number[variable=="CLA_2015"])*100, percent))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "child characteristic at 31st March",
                year="2015",
                subcategory = ifelse(variable=="CLA_Mar2015", "Age group",
                                     ifelse(variable=="CLA_2015", "Age group",
                                            ifelse(variable=="CLA_stp2015", "Age group",
                                                   ifelse(variable=="CLA_10to15", "Age group",
                                                          ifelse(variable=="CLA_16over", "Age group",
                                                                 ifelse(variable=="CLA_1to4", "Age group",NA)))))),
                subcategory = ifelse(variable=="CLA_5to9", "Age group",
                                     ifelse(variable=="CLA_Adopt", "Placement",
                                            ifelse(variable=="CLA_Asian", "Ethnicity",
                                                   ifelse(variable=="CLA_Black", "Ethnicity",
                                                          ifelse(variable=="CLA_CPG", "Legal status",
                                                                 ifelse(variable=="CLA_EOTH", "Ethnicity", subcategory)))))),
                subcategory = ifelse(variable=="CLA_ExtPl", "LA of placement",
                                     ifelse(variable=="CLA_FCO", "Legal status",
                                            ifelse(variable=="CLA_female", "Gender",
                                                   ifelse(variable=="CLA_Fost", "Placement",
                                                          ifelse(variable=="CLA_FrAd", NA,                                                                                                                                    
                                                                 ifelse(variable=="CLA_ICO", "Legal status", subcategory)))))),
                subcategory = ifelse(variable=="CLA_InBound", "Locality of placement",
                                     ifelse(variable=="CLA_IntPl", "LA of placement",
                                            ifelse(variable=="CLA_LAPl", "Placed inside the local authority boundary",subcategory))),
                subcategory = ifelse(variable=="CLA_male", "Gender",
                                     ifelse(variable=="CLA_Mixed", "Ethnicity",
                                            ifelse(variable=="CLA_NetGain", "Net gain of children by responsible LA",
                                                   ifelse(variable=="CLA_Nrep", "Place providers",
                                                          ifelse(variable=="CLA_Ocom", "Placement",
                                                                 ifelse(variable=="CLA_Ores", "Placement",
                                                                        ifelse(variable=="CLA_Oth", "Ethnicity",
                                                                               ifelse(variable=="CLA_OthLA", "Place providers",
                                                                                      ifelse(variable=="CLA_OthPl", "Placement", subcategory))))))))),
                subcategory =  ifelse(variable=="CLA_OthPP", "Place providers",
                                      ifelse(variable=="CLA_Outbound", "Locality of placement",
                                             ifelse(variable=="CLA_OwnP", "Place providers",
                                                    ifelse(variable=="CLA_Par", "Place providers",
                                                           ifelse(variable=="CLA_Parent", "Placement",
                                                                  ifelse(variable=="CLA_PlaceO", "Legal status", subcategory)))))),
                subcategory =  ifelse(variable=="CLA_Priv", "Place providers", subcategory),
                subcategory = ifelse(variable=="CLA_RSch", "Placement",
                                     ifelse(variable=="CLA_S20", "Legal status",
                                            ifelse(variable=="CLA_Secure", "Placement", subcategory))),
                subcategory =  ifelse(variable=="CLA_U1", "Age group",
                                      ifelse(variable=="CLA_UASC", "Unaccompanied asylum-seeking children",
                                             ifelse(variable=="CLA_Vol", "Place providers",
                                                    ifelse(variable=="CLA_White", "Ethnicity",
                                                           ifelse(variable=="CLA_YJLS", "Legal status",subcategory))))),
                variable = ifelse(variable=="CLA_Mar2015", "Total",
                                  ifelse(variable=="CLA_2015", "Total_during",
                                         ifelse(variable=="CLA_stp2015", "Children who were only looked after exclusively under a series of short term placements",
                                                ifelse(variable=="CLA_10to15", "10 to 15 years",
                                                       ifelse(variable=="CLA_16over", "16 years and over",
                                                              ifelse(variable=="CLA_1to4", "1 to 4 years", variable)))))),
                variable = ifelse(variable=="CLA_5to9", "5 to 9 years",
                                  ifelse(variable=="CLA_Adopt", "Placed for adoption",
                                         ifelse(variable=="CLA_Asian", "Asian or Asian British",
                                                ifelse(variable=="CLA_Black", "Black African, Caribbean or Black British",
                                                       ifelse(variable=="CLA_CPG", "Detained for child protection",
                                                              ifelse(variable=="CLA_EOTH", "Other ethnic group",
                                                                     ifelse(variable=="CLA_ExtPl", "2. Other LA children externally placed within the local authority boundary", variable))))))),
                variable = ifelse(variable=="CLA_FCO", "Full care order",
                                  ifelse(variable=="CLA_female", "Female",
                                         ifelse(variable=="CLA_Fost", "Foster placements",
                                                ifelse(variable=="CLA_FrAd", NA,                                                                                                                                    
                                                       ifelse(variable=="CLA_ICO", "Interim care order",
                                                              ifelse(variable=="CLA_InBound", "Placed inside the local authority boundary",variable)))))),
                variable =               ifelse(variable=="CLA_LAPl", "Placed inside the local authority boundary",
                                                ifelse(variable=="CLA_male", "Male",
                                                       ifelse(variable=="CLA_Mixed", "Mixed or Multiple ethnic groups",
                                                              ifelse(variable=="CLA_NetGain", "Net gain of children by responsible LA",
                                                                     ifelse(variable=="CLA_Nrep", "Placement provider not reported",
                                                                            ifelse(variable=="CLA_Ocom", "Other placements in the community", variable)))))),
                variable = ifelse(variable=="CLA_Ores", "Other residential settings",
                                  ifelse(variable=="CLA_Oth", "Refused or information not yet available",
                                         ifelse(variable=="CLA_OthLA", "Other LA provision",
                                                ifelse(variable=="CLA_OthPl", "Other placements",
                                                       ifelse(variable=="CLA_OthPP", "Other public provision (e.g. by a PCT etc)",
                                                              ifelse(variable=="CLA_Outbound", "Placed outside the local authority boundary", variable)))))),
                variable = ifelse(variable=="CLA_OwnP", "Own provision (by the LA)",
                                  ifelse(variable=="CLA_Par", "Placed with parents or other person with parental responsibility",
                                         ifelse(variable=="CLA_Parent", "Parents or other person with parental responsibility",
                                                ifelse(variable=="CLA_PlaceO", "Placement order granted",
                                                       ifelse(variable=="CLA_Priv", "Private provision", variable))))),
                variable =       ifelse(variable=="CLA_RSch", "Residential schools",
                                        ifelse(variable=="CLA_S20", "Voluntary agreements under S20 CA 1989",
                                               ifelse(variable=="CLA_Secure", "Secure units children's homes and semi-independent living accommodation",
                                                      ifelse(variable=="CLA_U1", "Under 1 year",
                                                             ifelse(variable=="CLA_UASC", "Non-unaccompanied asylum-seeking children",
                                                                    ifelse(variable=="CLA_Vol", "Voluntary/third sector provision",
                                                                           ifelse(variable=="CLA_White", "White",
                                                                                  ifelse(variable=="CLA_YJLS", "Youth justice legal statuses",variable)))))))))











ceased <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2015/SFR34_CEA2015.csv"),
                   colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::rename(LA_Code = New_geog_code,
                LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::select(-geog_l)%>%
  dplyr::mutate(`Special guardianship orders` = as.character(as.numeric(CLA_ceaSpecG)+as.numeric(CLA_ceaSpecG2), na.rm=F),
                `Adopted` = as.character(as.numeric(CLA_ceaAdop)+as.numeric(CLA_ceaAdop2), na.rm=F))%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "CLA_cease"])*100)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "ceased during",
                year="2015",
                subcategory = ifelse(variable == "CLA_cease", "Age on ceasing",
                                     ifelse(variable=="Special guardianship orders","Reason episode ceased",
                                            ifelse(variable== "Adopted", "Reason episode ceased",
                                     ifelse(variable=="CLA_cea1015","Age on ceasing",
                                            ifelse(variable=="CLA_cea16","Age on ceasing",
                                                   ifelse(variable=="CLA_cea17","Age on ceasing",
                                                          ifelse(variable=="CLA_cea18","Age on ceasing",
                                                                 ifelse(variable=="CLA_cea14","Age on ceasing",
                                                                        ifelse(variable=="CLA_cea59","Age on ceasing",
                                                                               ifelse(variable=="CLA_ceaAbroad","Reason episode ceased",
                                                                                      # ifelse(variable=="CEA_Adop1","",
                                                                                      #   ifelse(variable=="CEA_Adop2","",
                                                                                      ifelse(variable=="CLA_ceaAgeAssmt","Reason episode ceased",
                                                                                             ifelse(variable=="CLA_ceaROG","Reason episode ceased",
                                                                                                    NA)))))))))))),
                subcategory = ifelse(variable == "CLA_cea_sen_cust", "Reason episode ceased",
                                     ifelse(variable=="CLA_ceaDied", "Reason episode ceased",
                                            ifelse(variable=="CLA_ceafe", "Gender",
                                                   ifelse(variable=="CLA_ceaIndLiv", "Reason episode ceased",
                                                          ifelse(variable=="CLA_ceaIndLiv2", "Reason episode ceased",
                                                                 ifelse(variable=="CLA_ceamal", "Gender",
                                                                        ifelse(variable=="CLA_ceaNoPar", "Reason episode ceased",
                                                                               ifelse(variable=="CLA_cea_OthRea", "Reason episode ceased",
                                                                                      ifelse(variable=="CLA_ceaParNPlan", "Reason episode ceased",
                                                                                             ifelse(variable=="CLA_ceaParPlan", "Reason episode ceased",
                                                                                                    ifelse(variable=="CLA_ceaRemEnd", "Reason episode ceased",
                                                                                                           ifelse(variable=="CLA_cea_tran_res", "Reason episode ceased",
                                                                                                                  subcategory)))))))))))),
                subcategory = #ifelse(variable=="CEA_SGO1","",
                  #      ifelse(variable=="CEA_SGO2","",
                  ifelse(variable=="CLA_ceataken","Reason episode ceased",
                         ifelse(variable=="CLA_cea1","Age on ceasing",
                                subcategory)),
                variable = ifelse(variable == "CLA_cease", "Total",
                                  ifelse(variable=="CLA_cea1015","10 to 15 years",
                                         ifelse(variable=="CLA_cea16","16 years",
                                                ifelse(variable=="CLA_cea17","17 years",
                                                       ifelse(variable=="CLA_cea18","18 years and over",
                                                              ifelse(variable=="CLA_cea14","1 to 4 years",
                                                                     ifelse(variable=="CLA_cea59","5 to 9 years",
                                                                            ifelse(variable=="CLA_ceaAbroad","Child moved abroad",
                                                                                   # ifelse(variable=="CEA_Adop1","",
                                                                                   #   ifelse(variable=="CEA_Adop2","",
                                                                                   ifelse(variable=="CLA_ceaAgeAssmt","Age assessment determined child aged 18 or over",
                                                                                          ifelse(variable=="CLA_ceaROG","Residence order or child arrangement order granted",
                                                                                                 variable)))))))))),
                variable = ifelse(variable == "CLA_cea_sen_cust", "Sentenced to custody",
                                  ifelse(variable=="CLA_ceaDied", "Died",
                                         ifelse(variable=="CLA_ceafe", "Female",
                                                ifelse(variable=="CLA_ceaIndLiv", "Moved into independent living (with supportive accommodation)",
                                                       ifelse(variable=="CLA_ceaIndLiv2", "Moved into independent living (with no formalised support)",
                                                              ifelse(variable=="CLA_ceamal", "Male",
                                                                     ifelse(variable=="CLA_ceaNoPar", "Left care to live with parents relatives or other person with no parental responsibility",
                                                                            ifelse(variable=="CLA_cea_OthRea", "Care ceased for any other reason",
                                                                                   ifelse(variable=="CLA_ceaParNPlan", "Returned home to live with parents or other person with parental responsibility which was not part of the care planning process",
                                                                                          ifelse(variable=="CLA_ceaParPlan", "Returned home to live with parents or other person with parental responsibility which was part of the care planning process",
                                                                                                 ifelse(variable=="CLA_ceaRemEnd", "Accommodation on remand ended",
                                                                                                        ifelse(variable=="CLA_cea_tran_res", "Transferred to residential care funded by adult social services",
                                                                                                               variable)))))))))))),
                variable = #ifelse(variable=="CEA_SGO1","",
                  #      ifelse(variable=="CEA_SGO2","",
                  ifelse(variable=="CLA_ceataken","Care taken by another local authority",
                         ifelse(variable=="CLA_cea1","Under 1 year",
                                variable)))%>%
  dplyr::filter(variable!="CLA_ceaAdop",
                variable!="CLA_ceaAdop2",
                variable!="CLA_ceaSpecG",
                variable!="CLA_ceaSpecG2")







leavers <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2015/SFR34_FormerCareLeavers2015.csv"),
                    colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::rename(LA_Code = New_geog_code,
                LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::select(-geog_l)%>%#
  #dplyr::mutate(`Total in education employment or training` = as.character(as.numeric(CL_Act_HE17.18)+ as.numeric(CL_Act_OE17.18)+as.numeric(CL_Act_TE17.18)))%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "All_aged1920.21"])*100)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "care leavers",
                year="2015",
                subcategory = "19 to 21 years",
                #variable = gsub("17.18", "", variable),
                #variable = gsub("1920.21", "", variable),
                variable = ifelse(variable=="Acc_BB", "Bed and breakfast",
                                  ifelse(variable=="Acc_CH", "Community home",
                                         ifelse(variable=="Acc_Cust", "In custody",
                                                ifelse(variable=="Acc_Dep", "Deported",
                                                       ifelse(variable=="Acc_EA", "Emergency accommodation",
                                                              ifelse(variable=="Acc_F", "Foyers",
                                                                     ifelse(variable=="Acc_FFC", "With former foster carers",
                                                                            ifelse(variable=="Acc_GA", "Gone abroad",
                                                                                   variable)))))))),
                variable = ifelse(variable=="Acc_IL", "Independent living",
                                  ifelse(variable=="Acc_NFA","No fixed abode/homeless",
                                         ifelse(variable=="Acc_ResNk","Residence not known",
                                                ifelse(variable=="LA_NoAccinf","Total information not known",
                                                       ifelse(variable=="Acc_NoSUITInfo","No information",
                                                              ifelse(variable=="Acc_NotSUIT","Accommodation considered not suitable",
                                                                     ifelse(variable=="Acc_OL","Ordinary lodgings",
                                                                            ifelse(variable=="Acc_OTH","Other accommodation",
                                                                                   variable)))))))),
                variable = ifelse(variable=="Acc_P", "With parents or relatives",
                                  ifelse(variable=="Acc_SITA","Semi-independent transitional accommodation",
                                         ifelse(variable=="Acc_SL","Supported lodgings",
                                                ifelse(variable=="Acc_SUIT","Accommodation considered suitable",
                                                       ifelse(variable=="Act_HE","Higher education i.e. studies beyond A level",
                                                              ifelse(variable=="Act_NEET_ill","Not in education training or employment, owing to illness or disability",
                                                                     ifelse(variable=="Act_NEET_oth","Not in education training or employment, owing to other reasons",
                                                                            ifelse(variable=="Act_NEET_preg","Not in education training or employment, owing to pregnancy or parenting",
                                                                                   variable)))))))),
                variable = ifelse(variable=="LA_NoActinf", "Total information not known",
                                  ifelse(variable=="Act_OE","Education other than higher education",
                                         ifelse(variable=="All_aged1920.21","Total",
                                                ifelse(variable=="Acc_SUIT","Accommodation considered suitable",
                                                       ifelse(variable=="InTouch_IT","Local authority in touch with care leaver",
                                                              ifelse(variable=="InTouch_NoServ","Young person no longer requires services",
                                                                     ifelse(variable=="InTouch_Not","Local authority not in touch with care leaver",
                                                                            ifelse(variable=="InTouch_Refu","Young person refuses contact",
                                                                                   ifelse(variable=="Act_TE","In training or employment",
                                                                                          variable))))))))))










characteristics <- rbind(characteristics, admitted, march, ceased, leavers)
####2014####

admitted <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2014/SFR36_ADM2014.csv"),
                     colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::rename(LA_Code = New_geog_code,
                LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::select(-geog_l)%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "CLA_started2014"])*100)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "started during",
                year="2014",
                subcategory = ifelse(variable=="CLA_started2014", "Taken into care",
                                     ifelse(variable=="CLA_taken2014", "Taken into care",
                                            ifelse(variable=="SCLA_10to15", "Age group",
                                                   ifelse(variable=="SCLA_16over", "Age group",
                                                          ifelse(variable=="SCLA_1to4", "Age group",
                                                                 ifelse(variable=="SCLA_5to9", "Age group",
                                                                        ifelse(variable=="SCLA_AbNeg", "Category of need",
                                                                               ifelse(variable=="SCLA_AbsPar", "Category of need",
                                                                                      ifelse(variable=="SCLA_Cdisab", "Category of need",
                                                                                             ifelse(variable=="SCLA_FAcSt", "Category of need",
                                                                                                    ifelse(variable=="SCLA_FCO", "Legal status",
                                                                                                           ifelse(variable=="SCLA_FD", "Category of need",
                                                                                                                  ifelse(variable=="SCLA_female", "Gender",
                                                                                                                         ifelse(variable=="SCLA_ICO", "Legal status",
                                                                                                                                ifelse(variable=="SCLA_LI", "Category of need",
                                                                                                                                       ifelse(variable=="SCLA_male", "Gender",
                                                                                                                                              ifelse(variable=="SCLA_ONCT", "Legal status",
                                                                                                                                                     ifelse(variable=="SCLA_PACE", "Legal status",
                                                                                                                                                            ifelse(variable=="SCLA_ParIll", "Category of need",
                                                                                                                                                                   ifelse(variable=="SCLA_POG", "Legal status",
                                                                                                                                                                          ifelse(variable=="SCLA_S20", "Legal status",
                                                                                                                                                                                 ifelse(variable=="SCLA_SEPO", "Legal status",
                                                                                                                                                                                        ifelse(variable=="SCLA_SORR", NA,
                                                                                                                                                                                               ifelse(variable=="SCLA_SUB", "Category of need",
                                                                                                                                                                                                      ifelse(variable=="SCLA_U1", "Age group",
                                                                                                                                                                                                             ifelse(variable=="SCLA_UCAO", "Legal status",
                                                                                                                                                                                                                    ifelse(variable=="SCLA_UPP",NA, NA))))))))))))))))))))))))))),
                variable = ifelse(variable=="CLA_started2014", "Total children",
                                  ifelse(variable=="CLA_taken2014", "All children taken into care",
                                         ifelse(variable=="SCLA_10to15", "10 to 15 years",
                                                ifelse(variable=="SCLA_16over", "16 years and over",
                                                       ifelse(variable=="SCLA_1to4", "1 to 4 years",
                                                              ifelse(variable=="SCLA_5to9", "5 to 9 years",
                                                                     ifelse(variable=="SCLA_AbNeg", "N1. Abuse or neglect",
                                                                            ifelse(variable=="SCLA_AbsPar", "N8. Absent parenting",
                                                                                   ifelse(variable=="SCLA_Cdisab", "N2. Child's disability",
                                                                                          ifelse(variable=="SCLA_FAcSt", "N.4 Family acute stress",
                                                                                                 ifelse(variable=="SCLA_FCO", "Full care order",
                                                                                                        ifelse(variable=="SCLA_FD", "N5. Family dysfunction",
                                                                                                               ifelse(variable=="SCLA_female", "Female",
                                                                                                                      ifelse(variable=="SCLA_ICO", "Interm care order",
                                                                                                                             ifelse(variable=="SCLA_LI", "N7. Low income",
                                                                                                                                    ifelse(variable=="SCLA_male", "Male",
                                                                                                                                           ifelse(variable=="SCLA_ONCT", "Remand",
                                                                                                                                                  ifelse(variable=="SCLA_PACE", "LA accommodation under PACE 1989",
                                                                                                                                                         ifelse(variable=="SCLA_ParIll", "N3. Parental illness or disability",
                                                                                                                                                                ifelse(variable=="SCLA_POG", "Placement order granted",
                                                                                                                                                                       ifelse(variable=="SCLA_S20", "Voluntary agreement under S20",
                                                                                                                                                                              ifelse(variable=="SCLA_SEPO", "Emergency protction order",
                                                                                                                                                                                     ifelse(variable=="SCLA_SORR", NA,
                                                                                                                                                                                            ifelse(variable=="SCLA_SUB", "N6. Socially unacceptable behaviour",
                                                                                                                                                                                                   ifelse(variable=="SCLA_U1", "Under 1 year",
                                                                                                                                                                                                          ifelse(variable=="SCLA_UCAO", "Child assessment order and in LA accommodation",
                                                                                                                                                                                                                 ifelse(variable=="SCLA_UPP",NA, NA))))))))))))))))))))))))))))





march <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2014/SFR36_CLA2014.csv"),
                  colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::rename(LA_Code = New_geog_code,
                LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::select(-geog_l, -tidyr::starts_with("X"))%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "CLA_Mar2014"])*100))%>%
  dplyr::mutate(percent = ifelse(variable=="CLA_2014", 100, percent))%>%
  dplyr::mutate(percent = ifelse(variable=="CLA_P2yrs", NA, percent))%>%
  dplyr::mutate(percent = ifelse(variable=="CLA_stp2014", as.numeric(number)/as.numeric(number[variable=="CLA_2014"])*100, percent))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "child characteristic at 31st March",
                year="2014",
                subcategory = ifelse(variable=="CLA_Mar2014", "Age group",
                                     ifelse(variable=="CLA_2014", "Age group",
                                            ifelse(variable=="CLA_stp2014", "Age group",
                                                   ifelse(variable=="CLA_10to15", "Age group",
                                                          ifelse(variable=="CLA_16over", "Age group",
                                                                 ifelse(variable=="CLA_1to4", "Age group",NA)))))),
                subcategory = ifelse(variable=="CLA_Miss", NA,
                                     ifelse(variable=="CLA_Moth", NA,
                                            ifelse(variable=="CLA_1Pla", NA,
                                            ifelse(variable=="CLA_2PLa", NA,
                                                   ifelse(variable=="CLA_3Pla", NA,
                                                          ifelse(variable=="CLA_P2yrs", "placement stability", subcategory)))))),
                subcategory = ifelse(variable=="CLA_5to9", "Age group",
                                     ifelse(variable=="CLA_Adopt", "Placement",
                                            ifelse(variable=="CLA_Asian", "Ethnicity",
                                                   ifelse(variable=="CLA_Black", "Ethnicity",
                                                          ifelse(variable=="CLA_CPG", "Legal status",
                                                                 ifelse(variable=="CLA_EOTH", "Ethnicity", subcategory)))))),
                subcategory = ifelse(variable=="CLA_ExtPl", "LA of placement",
                                     ifelse(variable=="CLA_FCO", "Legal status",
                                            ifelse(variable=="CLA_female", "Gender",
                                                   ifelse(variable=="CLA_Fost", "Placement",
                                                          ifelse(variable=="CLA_FrAd", NA,                                                                                                                                    
                                                                 ifelse(variable=="CLA_ICO", "Legal status", subcategory)))))),
                subcategory = ifelse(variable=="CLA_InBound", "Locality of placement",
                                     ifelse(variable=="CLA_IntPl", "LA of placement",
                                            ifelse(variable=="CLA_LAPl", "Placed inside the local authority boundary",subcategory))),
                subcategory = ifelse(variable=="CLA_male", "Gender",
                                     ifelse(variable=="CLA_Mixed", "Ethnicity",
                                            ifelse(variable=="CLA_NetGain", "Net gain of children by responsible LA",
                                                   ifelse(variable=="CLA_Nrep", "Place providers",
                                                          ifelse(variable=="CLA_Ocom", "Placement",
                                                                 ifelse(variable=="CLA_Ores", "Placement",
                                                                        ifelse(variable=="CLA_Oth", "Ethnicity",
                                                                               ifelse(variable=="CLA_OthLA", "Place providers",
                                                                                      ifelse(variable=="CLA_OthPl", "Placement", subcategory))))))))),
                subcategory =  ifelse(variable=="CLA_OthPP", "Place providers",
                                      ifelse(variable=="CLA_Outbound", "Locality of placement",
                                             ifelse(variable=="CLA_OwnP", "Place providers",
                                                    ifelse(variable=="CLA_Par", "Place providers",
                                                           ifelse(variable=="CLA_Parent", "Placement",
                                                                  ifelse(variable=="CLA_PlaceO", "Legal status", subcategory)))))),
                subcategory =  ifelse(variable=="CLA_Priv", "Place providers", subcategory),
                subcategory = ifelse(variable=="CLA_RSch", "Placement",
                                     ifelse(variable=="CLA_S20", "Legal status",
                                            ifelse(variable=="CLA_Secure", "Placement", subcategory))),
                subcategory =  ifelse(variable=="CLA_U1", "Age group",
                                      ifelse(variable=="CLA_UASC", "Unaccompanied asylum-seeking children",
                                             ifelse(variable=="CLA_Vol", "Place providers",
                                                    ifelse(variable=="CLA_White", "Ethnicity",
                                                           ifelse(variable=="CLA_YJLS", "Legal status",subcategory))))),
                variable = ifelse(variable=="CLA_Mar2014", "Total",
                                  ifelse(variable=="CLA_2014", "Total_during",
                                         ifelse(variable=="CLA_stp2014", "Children who were only looked after exclusively under a series of short term placements",
                                                ifelse(variable=="CLA_10to15", "10 to 15 years",
                                                       ifelse(variable=="CLA_16over", "16 years and over",
                                                              ifelse(variable=="CLA_1to4", "1 to 4 years", variable)))))),
                variable = ifelse(variable=="CLA_Miss", NA,
                                  ifelse(variable=="CLA_Moth", NA,
                                     ifelse(variable=="CLA_1Pla", NA,
                                            ifelse(variable=="CLA_2PLa", NA,
                                                   ifelse(variable=="CLA_3Pla", NA,
                                                          ifelse(variable=="CLA_P2yrs", "Living in the same placement for at least 2 years or are placed for adoption and their adoption and their adoptive placement together with their previous placement, last for at least 2 years", variable)))))),
                variable = ifelse(variable=="CLA_5to9", "5 to 9 years",
                                  ifelse(variable=="CLA_Adopt", "Placed for adoption",
                                         ifelse(variable=="CLA_Asian", "Asian or Asian British",
                                                ifelse(variable=="CLA_Black", "Black African, Caribbean or Black British",
                                                       ifelse(variable=="CLA_CPG", "Detained for child protection",
                                                              ifelse(variable=="CLA_EOTH", "Other ethnic group",
                                                                     ifelse(variable=="CLA_ExtPl", "2. Other LA children externally placed within the local authority boundary", variable))))))),
                variable = ifelse(variable=="CLA_FCO", "Full care order",
                                  ifelse(variable=="CLA_female", "Female",
                                         ifelse(variable=="CLA_Fost", "Foster placements",
                                                ifelse(variable=="CLA_FrAd", NA,                                                                                                                                    
                                                       ifelse(variable=="CLA_ICO", "Interim care order",
                                                              ifelse(variable=="CLA_InBound", "Placed inside the local authority boundary",variable)))))),
                variable =               ifelse(variable=="CLA_LAPl", "Placed inside the local authority boundary",
                                                ifelse(variable=="CLA_male", "Male",
                                                       ifelse(variable=="CLA_Mixed", "Mixed or Multiple ethnic groups",
                                                              ifelse(variable=="CLA_NetGain", "Net gain of children by responsible LA",
                                                                     ifelse(variable=="CLA_Nrep", "Placement provider not reported",
                                                                            ifelse(variable=="CLA_Ocom", "Other placements in the community", variable)))))),
                variable = ifelse(variable=="CLA_Ores", "Other residential settings",
                                  ifelse(variable=="CLA_Oth", "Refused or information not yet available",
                                         ifelse(variable=="CLA_OthLA", "Other LA provision",
                                                ifelse(variable=="CLA_OthPl", "Other placements",
                                                       ifelse(variable=="CLA_OthPP", "Other public provision (e.g. by a PCT etc)",
                                                              ifelse(variable=="CLA_Outbound", "Placed outside the local authority boundary", variable)))))),
                variable = ifelse(variable=="CLA_OwnP", "Own provision (by the LA)",
                                  ifelse(variable=="CLA_Par", "Placed with parents or other person with parental responsibility",
                                         ifelse(variable=="CLA_Parent", "Parents or other person with parental responsibility",
                                                ifelse(variable=="CLA_PlaceO", "Placement order granted",
                                                       ifelse(variable=="CLA_Priv", "Private provision", variable))))),
                variable =       ifelse(variable=="CLA_RSch", "Residential schools",
                                        ifelse(variable=="CLA_S20", "Voluntary agreements under S20 CA 1989",
                                               ifelse(variable=="CLA_Secure", "Secure units children's homes and semi-independent living accommodation",
                                                      ifelse(variable=="CLA_U1", "Under 1 year",
                                                             ifelse(variable=="CLA_UASC", "Non-unaccompanied asylum-seeking children",
                                                                    ifelse(variable=="CLA_Vol", "Voluntary/third sector provision",
                                                                           ifelse(variable=="CLA_White", "White",
                                                                                  ifelse(variable=="CLA_YJLS", "Youth justice legal statuses",variable)))))))))





ceased <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2014/SFR36_CEA2014.csv"),
                   colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::rename(LA_Code = New_geog_code,
                LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::select(-geog_l)%>%
  dplyr::mutate(`Special guardianship orders` = as.character(as.numeric(CLA_ceaSpecG)+as.numeric(CLA_ceaSpecG2), na.rm=F),
                `Adopted` = as.character(as.numeric(CLA_ceaAdop)+as.numeric(CLA_ceaAdop2), na.rm=F))%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "CLA_cease"])*100)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "ceased during",
                year="2014",
                subcategory = ifelse(variable == "CLA_cease", "Age on ceasing",
                                     ifelse(variable=="Special guardianship orders","Reason episode ceased",
                                            ifelse(variable== "Adopted", "Reason episode ceased",
                                     ifelse(variable=="CLA_cea1015","Age on ceasing",
                                            ifelse(variable=="CLA_cease16","Age on ceasing",
                                                   ifelse(variable=="CLA_cea17","Age on ceasing",
                                                          ifelse(variable=="CLA_cea18","Age on ceasing",
                                                                 ifelse(variable=="CLA_cea14","Age on ceasing",
                                                                        ifelse(variable=="CLA_cea59","Age on ceasing",
                                                                               ifelse(variable=="CLA_ceaAbroad","Reason episode ceased",
                                                                                      # ifelse(variable=="CEA_Adop1","",
                                                                                      #   ifelse(variable=="CEA_Adop2","",
                                                                                      ifelse(variable=="CLA_ceaAgeAssmt","Reason episode ceased",
                                                                                             ifelse(variable=="CLA_ceaROG","Reason episode ceased",
                                                                                                    NA)))))))))))),
                subcategory = ifelse(variable == "CLA_cea_sen_cust", "Reason episode ceased",
                                     ifelse(variable=="CLA_ceaDied", "Reason episode ceased",
                                            ifelse(variable=="CLA_ceafe", "Gender",
                                                   ifelse(variable=="CLA_ceaIndLiv", "Reason episode ceased",
                                                          ifelse(variable=="CLA_ceaIndLiv2", "Reason episode ceased",
                                                                 ifelse(variable=="CLA_ceamal", "Gender",
                                                                        ifelse(variable=="CLA_ceaNoPar", "Reason episode ceased",
                                                                               ifelse(variable=="CLA_cea_OthRea", "Reason episode ceased",
                                                                                      ifelse(variable=="CLA_ceaParNPlan", "Reason episode ceased",
                                                                                             ifelse(variable=="CLA_ceaParPlan", "Reason episode ceased",
                                                                                                    ifelse(variable=="CLA_ceaRemEnd", "Reason episode ceased",
                                                                                                           ifelse(variable=="CLA_cea_tran_res", "Reason episode ceased",
                                                                                                                  subcategory)))))))))))),
                subcategory = #ifelse(variable=="CEA_SGO1","",
                  #      ifelse(variable=="CEA_SGO2","",
                  ifelse(variable=="CLA_ceataken","Reason episode ceased",
                         ifelse(variable=="CLA_cea1","Age on ceasing",
                                subcategory)),
                variable = ifelse(variable == "CLA_cease", "Total",
                                  ifelse(variable=="CLA_cea1015","10 to 15 years",
                                         ifelse(variable=="CLA_cease16","16 years",
                                                ifelse(variable=="CLA_cea17","17 years",
                                                       ifelse(variable=="CLA_cea18","18 years and over",
                                                              ifelse(variable=="CLA_cea14","1 to 4 years",
                                                                     ifelse(variable=="CLA_cea59","5 to 9 years",
                                                                            ifelse(variable=="CLA_ceaAbroad","Child moved abroad",
                                                                                   # ifelse(variable=="CEA_Adop1","",
                                                                                   #   ifelse(variable=="CEA_Adop2","",
                                                                                   ifelse(variable=="CLA_ceaAgeAssmt","Age assessment determined child aged 18 or over",
                                                                                          ifelse(variable=="CLA_ceaROG","Residence order or child arrangement order granted",
                                                                                                 variable)))))))))),
                variable = ifelse(variable == "CLA_cea_sen_cust", "Sentenced to custody",
                                  ifelse(variable=="CLA_ceaDied", "Died",
                                         ifelse(variable=="CLA_ceafe", "Female",
                                                ifelse(variable=="CLA_ceaIndLiv", "Moved into independent living (with supportive accommodation)",
                                                       ifelse(variable=="CLA_ceaIndLiv2", "Moved into independent living (with no formalised support)",
                                                              ifelse(variable=="CLA_ceamal", "Male",
                                                                     ifelse(variable=="CLA_ceaNoPar", "Left care to live with parents relatives or other person with no parental responsibility",
                                                                            ifelse(variable=="CLA_cea_OthRea", "Care ceased for any other reason",
                                                                                   ifelse(variable=="CLA_ceaParNPlan", "Returned home to live with parents or other person with parental responsibility which was not part of the care planning process",
                                                                                          ifelse(variable=="CLA_ceaParPlan", "Returned home to live with parents or other person with parental responsibility which was part of the care planning process",
                                                                                                 ifelse(variable=="CLA_ceaRemEnd", "Accommodation on remand ended",
                                                                                                        ifelse(variable=="CLA_cea_tran_res", "Transferred to residential care funded by adult social services",
                                                                                                               variable)))))))))))),
                variable = #ifelse(variable=="CEA_SGO1","",
                  #      ifelse(variable=="CEA_SGO2","",
                  ifelse(variable=="CLA_ceataken","Care taken by another local authority",
                         ifelse(variable=="CLA_cea1","Under 1 year",
                                variable)))%>%
  dplyr::filter(variable!="CLA_ceaAdop",
                variable!="CLA_ceaAdop2",
                variable!="CLA_ceaSpecG",
                variable!="CLA_ceaSpecG2",
                variable!="CLA_cea16",
                variable!="CLA_ceaPar")





leavers <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2014/SFR36_FormerCareLeavers2014.csv"),
                    colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::rename(LA_Code = New_geog_code,
                LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::select(-geog_l)%>%#
  #dplyr::mutate(`Total in education employment or training` = as.character(as.numeric(CL_Act_HE17.18)+ as.numeric(CL_Act_OE17.18)+as.numeric(CL_Act_TE17.18)))%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "All_aged1920.21"])*100)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "care leavers",
                year="2014",
                subcategory = "19 to 21 years",
                #variable = gsub("17.18", "", variable),
                #variable = gsub("1920.21", "", variable),
                variable = ifelse(variable=="Acc_BB", "Bed and breakfast",
                                  ifelse(variable=="Acc_CH", "Community home",
                                         ifelse(variable=="Acc_Cust", "In custody",
                                                ifelse(variable=="Acc_Dep", "Deported",
                                                       ifelse(variable=="Acc_EA", "Emergency accommodation",
                                                              ifelse(variable=="Acc_F", "Foyers",
                                                                     ifelse(variable=="Acc_FFC", "With former foster carers",
                                                                            ifelse(variable=="Acc_GA", "Gone abroad",
                                                                                   variable)))))))),
                variable = ifelse(variable=="Acc_IL", "Independent living",
                                  ifelse(variable=="Acc_NFA","No fixed abode/homeless",
                                         ifelse(variable=="Acc_ResNk","Residence not known",
                                                ifelse(variable=="LA_Noinf.1","Total information not known",
                                                       ifelse(variable=="Acc_NoSUITInfo","No information",
                                                              ifelse(variable=="Acc_NotSUIT","Accommodation considered not suitable",
                                                                     ifelse(variable=="Acc_OL","Ordinary lodgings",
                                                                            ifelse(variable=="Acc_OTH","Other accommodation",
                                                                                   variable)))))))),
                variable = ifelse(variable=="Acc_P", "With parents or relatives",
                                  ifelse(variable=="Acc_SITA","Semi-independent transitional accommodation",
                                         ifelse(variable=="Acc_SL","Supported lodgings",
                                                ifelse(variable=="Acc_SUIT","Accommodation considered suitable",
                                                       ifelse(variable=="Act_HE","Higher education i.e. studies beyond A level",
                                                              ifelse(variable=="Act_NEET_ill","Not in education training or employment, owing to illness or disability",
                                                                     ifelse(variable=="Act_NEET_oth","Not in education training or employment, owing to other reasons",
                                                                            ifelse(variable=="Act_NEET_preg","Not in education training or employment, owing to pregnancy or parenting",
                                                                                   variable)))))))),
                variable = ifelse(variable=="LA_Noinf", "Total information not known",
                                  ifelse(variable=="Act_OE","Education other than higher education",
                                         ifelse(variable=="All_aged1920.21","Total",
                                                ifelse(variable=="Acc_SUIT","Accommodation considered suitable",
                                                       ifelse(variable=="InTouch_IT","Local authority in touch with care leaver",
                                                              ifelse(variable=="InTouch_NoServ","Young person no longer requires services",
                                                                     ifelse(variable=="InTouch_Not","Local authority not in touch with care leaver",
                                                                            ifelse(variable=="InTouch_Refu","Young person refuses contact",
                                                                                   ifelse(variable=="Act_TE","In training or employment",
                                                                                          ifelse(variable == "ChildDied", NA,
                                                                                                 ifelse(variable == "ChildDied.1",NA, variable))))))))))))






characteristics <- rbind(characteristics, admitted, march, ceased, leavers)

####2013####



admitted <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2013/SFR36_ADM2013.csv"),
                     colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::rename(LA_Code = New_geog_code,
                LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::select(-geog_l, -tidyr::starts_with("X"))%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "CLA_started2013"])*100)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "started during",
                year="2013",
                subcategory = ifelse(variable=="CLA_started2013", "Taken into care",
                                     ifelse(variable=="CLA_taken2013", "Taken into care",
                                            ifelse(variable=="SCLA_10to15", "Age group",
                                                   ifelse(variable=="SCLA_16over", "Age group",
                                                          ifelse(variable=="SCLA_1to4", "Age group",
                                                                 ifelse(variable=="SCLA_5to9", "Age group",
                                                                        ifelse(variable=="SCLA_AbNeg", "Category of need",
                                                                               ifelse(variable=="SCLA_AbsPar", "Category of need",
                                                                                      ifelse(variable=="SCLA_Cdisab", "Category of need",
                                                                                             ifelse(variable=="SCLA_FAcSt", "Category of need",
                                                                                                    ifelse(variable=="SCLA_FCO", "Legal status",
                                                                                                           ifelse(variable=="SCLA_FD", "Category of need",
                                                                                                                  ifelse(variable=="SCLA_female", "Gender",
                                                                                                                         ifelse(variable=="SCLA_ICO", "Legal status",
                                                                                                                                ifelse(variable=="SCLA_LI", "Category of need",
                                                                                                                                       ifelse(variable=="SCLA_male", "Gender",
                                                                                                                                              ifelse(variable=="SCLA_ONCT", "Legal status",
                                                                                                                                                     ifelse(variable=="SCLA_PACE", "Legal status",
                                                                                                                                                            ifelse(variable=="SCLA_ParIll", "Category of need",
                                                                                                                                                                   ifelse(variable=="SCLA_POG", "Legal status",
                                                                                                                                                                          ifelse(variable=="SCLA_S20", "Legal status",
                                                                                                                                                                                 ifelse(variable=="SCLA_SEPO", "Legal status",
                                                                                                                                                                                        ifelse(variable=="SCLA_SORR", NA,
                                                                                                                                                                                               ifelse(variable=="SCLA_SUB", "Category of need",
                                                                                                                                                                                                      ifelse(variable=="SCLA_U1", "Age group",
                                                                                                                                                                                                             ifelse(variable=="SCLA_UCAO", "Legal status",
                                                                                                                                                                                                                    ifelse(variable=="SCLA_UPP",NA, NA))))))))))))))))))))))))))),
                variable = ifelse(variable=="CLA_started2013", "Total children",
                                  ifelse(variable=="CLA_taken2013", "All children taken into care",
                                         ifelse(variable=="SCLA_10to15", "10 to 15 years",
                                                ifelse(variable=="SCLA_16over", "16 years and over",
                                                       ifelse(variable=="SCLA_1to4", "1 to 4 years",
                                                              ifelse(variable=="SCLA_5to9", "5 to 9 years",
                                                                     ifelse(variable=="SCLA_AbNeg", "N1. Abuse or neglect",
                                                                            ifelse(variable=="SCLA_AbsPar", "N8. Absent parenting",
                                                                                   ifelse(variable=="SCLA_Cdisab", "N2. Child's disability",
                                                                                          ifelse(variable=="SCLA_FAcSt", "N.4 Family acute stress",
                                                                                                 ifelse(variable=="SCLA_FCO", "Full care order",
                                                                                                        ifelse(variable=="SCLA_FD", "N5. Family dysfunction",
                                                                                                               ifelse(variable=="SCLA_female", "Female",
                                                                                                                      ifelse(variable=="SCLA_ICO", "Interm care order",
                                                                                                                             ifelse(variable=="SCLA_LI", "N7. Low income",
                                                                                                                                    ifelse(variable=="SCLA_male", "Male",
                                                                                                                                           ifelse(variable=="SCLA_ONCT", "Remand",
                                                                                                                                                  ifelse(variable=="SCLA_PACE", "LA accommodation under PACE 1989",
                                                                                                                                                         ifelse(variable=="SCLA_ParIll", "N3. Parental illness or disability",
                                                                                                                                                                ifelse(variable=="SCLA_POG", "Placement order granted",
                                                                                                                                                                       ifelse(variable=="SCLA_S20", "Voluntary agreement under S20",
                                                                                                                                                                              ifelse(variable=="SCLA_SEPO", "Emergency protction order",
                                                                                                                                                                                     ifelse(variable=="SCLA_SORR", NA,
                                                                                                                                                                                            ifelse(variable=="SCLA_SUB", "N6. Socially unacceptable behaviour",
                                                                                                                                                                                                   ifelse(variable=="SCLA_U1", "Under 1 year",
                                                                                                                                                                                                          ifelse(variable=="SCLA_UCAO", "Child assessment order and in LA accommodation",
                                                                                                                                                                                                                 ifelse(variable=="SCLA_UPP",NA, NA))))))))))))))))))))))))))))






march <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2013/SFR36_CLA2013.csv"),
                  colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::rename(LA_Code = New_geog_code,
                LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::select(-geog_l, -tidyr::starts_with("X"))%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "CLA_Mar2013"])*100))%>%
  dplyr::mutate(percent = ifelse(variable=="CLA_2013", 100, percent))%>%
  dplyr::mutate(percent = ifelse(variable=="CLA_P2yrs", NA, percent))%>%
  dplyr::mutate(percent = ifelse(variable=="CLA_stp2013", as.numeric(number)/as.numeric(number[variable=="CLA_2013"])*100, percent))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "child characteristic at 31st March",
                year="2013",
                subcategory = ifelse(variable=="CLA_Mar2013", "Age group",
                                     ifelse(variable=="CLA_2013", "Age group",
                                            ifelse(variable=="CLA_stp2013", "Age group",
                                                   ifelse(variable=="CLA_10to15", "Age group",
                                                          ifelse(variable=="CLA_16over", "Age group",
                                                                 ifelse(variable=="CLA_1to4", "Age group",NA)))))),
                subcategory = ifelse(variable=="CLA_Miss", NA,
                                     ifelse(variable=="CLA_Moth", NA,
                                            ifelse(variable=="CLA_1Pla", NA,
                                                   ifelse(variable=="CLA_2PLa", NA,
                                                          ifelse(variable=="CLA_3Pla", NA,
                                                                 ifelse(variable=="CLA_P2yrs", "placement stability", subcategory)))))),
                subcategory = ifelse(variable=="CLA_5to9", "Age group",
                                     ifelse(variable=="CLA_Adopt", "Placement",
                                            ifelse(variable=="CLA_Asian", "Ethnicity",
                                                   ifelse(variable=="CLA_Black", "Ethnicity",
                                                          ifelse(variable=="CLA_CPG", "Legal status",
                                                                 ifelse(variable=="CLA_EOTH", "Ethnicity", subcategory)))))),
                subcategory = ifelse(variable=="CLA_ExtPl", "LA of placement",
                                     ifelse(variable=="CLA_FCO", "Legal status",
                                            ifelse(variable=="CLA_female", "Gender",
                                                   ifelse(variable=="CLA_Fost", "Placement",
                                                          ifelse(variable=="CLA_FrAd", NA,                                                                                                                                    
                                                                 ifelse(variable=="CLA_ICO", "Legal status", subcategory)))))),
                subcategory = ifelse(variable=="CLA_InBound", "Locality of placement",
                                     ifelse(variable=="CLA_IntPl", "LA of placement",
                                            ifelse(variable=="CLA_LAPl", "Placed inside the local authority boundary",subcategory))),
                subcategory = ifelse(variable=="CLA_male", "Gender",
                                     ifelse(variable=="CLA_Mixed", "Ethnicity",
                                            ifelse(variable=="CLA_NetGain", "Net gain of children by responsible LA",
                                                   ifelse(variable=="CLA_Nrep", "Place providers",
                                                          ifelse(variable=="CLA_Ocom", "Placement",
                                                                 ifelse(variable=="CLA_Ores", "Placement",
                                                                        ifelse(variable=="CLA_Oth", "Ethnicity",
                                                                               ifelse(variable=="CLA_OthLA", "Place providers",
                                                                                      ifelse(variable=="CLA_OthPl", "Placement", subcategory))))))))),
                subcategory =  ifelse(variable=="CLA_OthPP", "Place providers",
                                      ifelse(variable=="CLA_Outbound", "Locality of placement",
                                             ifelse(variable=="CLA_OwnP", "Place providers",
                                                    ifelse(variable=="CLA_Par", "Place providers",
                                                           ifelse(variable=="CLA_Parent", "Placement",
                                                                  ifelse(variable=="CLA_PlaceO", "Legal status", subcategory)))))),
                subcategory =  ifelse(variable=="CLA_Priv", "Place providers", subcategory),
                subcategory = ifelse(variable=="CLA_RSch", "Placement",
                                     ifelse(variable=="CLA_S20", "Legal status",
                                            ifelse(variable=="CLA_Secure", "Placement", subcategory))),
                subcategory =  ifelse(variable=="CLA_U1", "Age group",
                                      ifelse(variable=="CLA_UASC", "Unaccompanied asylum-seeking children",
                                             ifelse(variable=="CLA_Vol", "Place providers",
                                                    ifelse(variable=="CLA_White", "Ethnicity",
                                                           ifelse(variable=="CLA_YJLS", "Legal status",subcategory))))),
                variable = ifelse(variable=="CLA_Mar2013", "Total",
                                  ifelse(variable=="CLA_2013", "Total_during",
                                         ifelse(variable=="CLA_stp2013", "Children who were only looked after exclusively under a series of short term placements",
                                                ifelse(variable=="CLA_10to15", "10 to 15 years",
                                                       ifelse(variable=="CLA_16over", "16 years and over",
                                                              ifelse(variable=="CLA_1to4", "1 to 4 years", variable)))))),
                variable = ifelse(variable=="CLA_Miss", NA,
                                  ifelse(variable=="CLA_Moth", NA,
                                         ifelse(variable=="CLA_1Pla", NA,
                                                ifelse(variable=="CLA_2PLa", NA,
                                                       ifelse(variable=="CLA_3Pla", NA,
                                                              ifelse(variable=="CLA_P2yrs", "Living in the same placement for at least 2 years or are placed for adoption and their adoption and their adoptive placement together with their previous placement, last for at least 2 years", variable)))))),
                variable = ifelse(variable=="CLA_5to9", "5 to 9 years",
                                  ifelse(variable=="CLA_Adopt", "Placed for adoption",
                                         ifelse(variable=="CLA_Asian", "Asian or Asian British",
                                                ifelse(variable=="CLA_Black", "Black African, Caribbean or Black British",
                                                       ifelse(variable=="CLA_CPG", "Detained for child protection",
                                                              ifelse(variable=="CLA_EOTH", "Other ethnic group",
                                                                     ifelse(variable=="CLA_ExtPl", "2. Other LA children externally placed within the local authority boundary", variable))))))),
                variable = ifelse(variable=="CLA_FCO", "Full care order",
                                  ifelse(variable=="CLA_female", "Female",
                                         ifelse(variable=="CLA_Fost", "Foster placements",
                                                ifelse(variable=="CLA_FrAd", NA,                                                                                                                                    
                                                       ifelse(variable=="CLA_ICO", "Interim care order",
                                                              ifelse(variable=="CLA_InBound", "Placed inside the local authority boundary",variable)))))),
                variable =               ifelse(variable=="CLA_LAPl", "Placed inside the local authority boundary",
                                                ifelse(variable=="CLA_male", "Male",
                                                       ifelse(variable=="CLA_Mixed", "Mixed or Multiple ethnic groups",
                                                              ifelse(variable=="CLA_NetGain", "Net gain of children by responsible LA",
                                                                     ifelse(variable=="CLA_Nrep", "Placement provider not reported",
                                                                            ifelse(variable=="CLA_Ocom", "Other placements in the community", variable)))))),
                variable = ifelse(variable=="CLA_Ores", "Other residential settings",
                                  ifelse(variable=="CLA_Oth", "Refused or information not yet available",
                                         ifelse(variable=="CLA_OthLA", "Other LA provision",
                                                ifelse(variable=="CLA_OthPl", "Other placements",
                                                       ifelse(variable=="CLA_OthPP", "Other public provision (e.g. by a PCT etc)",
                                                              ifelse(variable=="CLA_Outbound", "Placed outside the local authority boundary", variable)))))),
                variable = ifelse(variable=="CLA_OwnP", "Own provision (by the LA)",
                                  ifelse(variable=="CLA_Par", "Placed with parents or other person with parental responsibility",
                                         ifelse(variable=="CLA_Parent", "Parents or other person with parental responsibility",
                                                ifelse(variable=="CLA_PlaceO", "Placement order granted",
                                                       ifelse(variable=="CLA_Priv", "Private provision", variable))))),
                variable =       ifelse(variable=="CLA_RSch", "Residential schools",
                                        ifelse(variable=="CLA_S20", "Voluntary agreements under S20 CA 1989",
                                               ifelse(variable=="CLA_Secure", "Secure units children's homes and semi-independent living accommodation",
                                                      ifelse(variable=="CLA_U1", "Under 1 year",
                                                             ifelse(variable=="CLA_UASC", "Non-unaccompanied asylum-seeking children",
                                                                    ifelse(variable=="CLA_Vol", "Voluntary/third sector provision",
                                                                           ifelse(variable=="CLA_White", "White",
                                                                                  ifelse(variable=="CLA_YJLS", "Youth justice legal statuses",variable)))))))))




ceased <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2013/SFR36_CEA2013.csv"),
                   colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::rename(LA_Code = New_geog_code,
                LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::select(-geog_l)%>%
  dplyr::mutate(`Special guardianship orders` = as.character(as.numeric(CLA_ceaSpecG)+as.numeric(CLA_ceaSpecG2), na.rm=F),
                `Adopted` = as.character(as.numeric(CLA_ceaAdop)+as.numeric(CLA_ceaAdop2), na.rm=F))%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "CLA_cease"])*100)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "ceased during",
                year="2013",
                subcategory = ifelse(variable == "CLA_cease", "Age on ceasing",
                                     ifelse(variable=="Special guardianship orders","Reason episode ceased",
                                            ifelse(variable== "Adopted", "Reason episode ceased",
                                                   ifelse(variable=="CLA_cea1015","Age on ceasing",
                                                          ifelse(variable=="CLA_cease16","Age on ceasing",
                                                                 ifelse(variable=="CLA_cea17","Age on ceasing",
                                                                        ifelse(variable=="CLA_cea18","Age on ceasing",
                                                                               ifelse(variable=="CLA_cea14","Age on ceasing",
                                                                                      ifelse(variable=="CLA_cea59","Age on ceasing",
                                                                                             ifelse(variable=="CLA_ceaAbroad","Reason episode ceased",
                                                                                                    # ifelse(variable=="CEA_Adop1","",
                                                                                                    #   ifelse(variable=="CEA_Adop2","",
                                                                                                    ifelse(variable=="CLA_ceaAgeAssmt","Reason episode ceased",
                                                                                                           ifelse(variable=="CLA_ceaROG","Reason episode ceased",
                                                                                                                  NA)))))))))))),
                subcategory = ifelse(variable == "CLA_cea_sen_cust", "Reason episode ceased",
                                     ifelse(variable=="CLA.ceaDied", "Reason episode ceased",
                                            ifelse(variable=="CLA_ceafe", "Gender",
                                                   ifelse(variable=="CLA_ceaIndLiv", "Reason episode ceased",
                                                          ifelse(variable=="CLA_ceaIndLiv2", "Reason episode ceased",
                                                                 ifelse(variable=="CLA_ceamal", "Gender",
                                                                        ifelse(variable=="CLA_ceaNoPar", "Reason episode ceased",
                                                                               ifelse(variable=="CLA_cea_OthRea", "Reason episode ceased",
                                                                                      ifelse(variable=="CLA_ceaParNPlan", "Reason episode ceased",
                                                                                             ifelse(variable=="CLA_ceaParPlan", "Reason episode ceased",
                                                                                                    ifelse(variable=="CLA_ceaRemEnd", "Reason episode ceased",
                                                                                                           ifelse(variable=="CLA_cea_tran_res", "Reason episode ceased",
                                                                                                                  subcategory)))))))))))),
                subcategory = #ifelse(variable=="CEA_SGO1","",
                  #      ifelse(variable=="CEA_SGO2","",
                  ifelse(variable=="CLA_ceataken","Reason episode ceased",
                         ifelse(variable=="CLA_cea1","Age on ceasing",
                                subcategory)),
                variable = ifelse(variable == "CLA_cease", "Total",
                                  ifelse(variable=="CLA_cea1015","10 to 15 years",
                                         ifelse(variable=="CLA_cease16","16 years",
                                                ifelse(variable=="CLA_cea17","17 years",
                                                       ifelse(variable=="CLA_cea18","18 years and over",
                                                              ifelse(variable=="CLA_cea14","1 to 4 years",
                                                                     ifelse(variable=="CLA_cea59","5 to 9 years",
                                                                            ifelse(variable=="CLA_ceaAbroad","Child moved abroad",
                                                                                   # ifelse(variable=="CEA_Adop1","",
                                                                                   #   ifelse(variable=="CEA_Adop2","",
                                                                                   ifelse(variable=="CLA_ceaAgeAssmt","Age assessment determined child aged 18 or over",
                                                                                          ifelse(variable=="CLA_ceaROG","Residence order or child arrangement order granted",
                                                                                                 variable)))))))))),
                variable = ifelse(variable == "CLA_cea_sen_cust", "Sentenced to custody",
                                  ifelse(variable=="CLA.ceaDied", "Died",
                                         ifelse(variable=="CLA_ceafe", "Female",
                                                ifelse(variable=="CLA_ceaIndLiv", "Moved into independent living (with supportive accommodation)",
                                                       ifelse(variable=="CLA_ceaIndLiv2", "Moved into independent living (with no formalised support)",
                                                              ifelse(variable=="CLA_ceamal", "Male",
                                                                     ifelse(variable=="CLA_ceaNoPar", "Left care to live with parents relatives or other person with no parental responsibility",
                                                                            ifelse(variable=="CLA_cea_OthRea", "Care ceased for any other reason",
                                                                                   ifelse(variable=="CLA_ceaParNPlan", "Returned home to live with parents or other person with parental responsibility which was not part of the care planning process",
                                                                                          ifelse(variable=="CLA_ceaParPlan", "Returned home to live with parents or other person with parental responsibility which was part of the care planning process",
                                                                                                 ifelse(variable=="CLA_ceaRemEnd", "Accommodation on remand ended",
                                                                                                        ifelse(variable=="CLA_cea_tran_res", "Transferred to residential care funded by adult social services",
                                                                                                               variable)))))))))))),
                variable = #ifelse(variable=="CEA_SGO1","",
                  #      ifelse(variable=="CEA_SGO2","",
                  ifelse(variable=="CLA_ceataken","Care taken by another local authority",
                         ifelse(variable=="CLA_cea1","Under 1 year",
                                variable)))%>%
  dplyr::filter(variable!="CLA_ceaAdop",
                variable!="CLA_ceaAdop2",
                variable!="CLA_ceaSpecG",
                variable!="CLA_ceaSpecG2",
                variable!="CLA_cea16",
                variable!="CLA_ceaPar")

characteristics <- rbind(characteristics, admitted, march, ceased)


####2012####

admitted <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2012/UnderlyingData/SFR20_ADM2012.csv"),
                     colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::rename(LA_Code = New_geog_code,
                LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::select(-geog_l, -tidyr::starts_with("X"))%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "CLA_started2012"])*100)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "started during",
                year="2012",
                subcategory = ifelse(variable=="CLA_started2012", "Taken into care",
                                     ifelse(variable=="CLA_taken2012", "Taken into care",
                                            ifelse(variable=="SCLA_10to15", "Age group",
                                                   ifelse(variable=="SCLA_16over", "Age group",
                                                          ifelse(variable=="SCLA_1to4", "Age group",
                                                                 ifelse(variable=="SCLA_5to9", "Age group",
                                                                        ifelse(variable=="SCLA_AbNeg", "Category of need",
                                                                               ifelse(variable=="SCLA_AbsPar", "Category of need",
                                                                                      ifelse(variable=="SCLA_Cdisab", "Category of need",
                                                                                             ifelse(variable=="SCLA_FAcSt", "Category of need",
                                                                                                    ifelse(variable=="SCLA_FCO", "Legal status",
                                                                                                           ifelse(variable=="SCLA_FD", "Category of need",
                                                                                                                  ifelse(variable=="SCLA_female", "Gender",
                                                                                                                         ifelse(variable=="SCLA_ICO", "Legal status",
                                                                                                                                ifelse(variable=="SCLA_LI", "Category of need",
                                                                                                                                       ifelse(variable=="SCLA_male", "Gender",
                                                                                                                                              ifelse(variable=="SCLA_ONCT", "Legal status",
                                                                                                                                                     ifelse(variable=="SCLA_PACE", "Legal status",
                                                                                                                                                            ifelse(variable=="SCLA_ParIll", "Category of need",
                                                                                                                                                                   ifelse(variable=="SCLA_POG", "Legal status",
                                                                                                                                                                          ifelse(variable=="SCLA_S20", "Legal status",
                                                                                                                                                                                 ifelse(variable=="SCLA_SEPO", "Legal status",
                                                                                                                                                                                        ifelse(variable=="SCLA_SORR", NA,
                                                                                                                                                                                               ifelse(variable=="SCLA_SUB", "Category of need",
                                                                                                                                                                                                      ifelse(variable=="SCLA_U1", "Age group",
                                                                                                                                                                                                             ifelse(variable=="SCLA_UCAO", "Legal status",
                                                                                                                                                                                                                    ifelse(variable=="SCLA_UPP",NA, NA))))))))))))))))))))))))))),
                variable = ifelse(variable=="CLA_started2012", "Total children",
                                  ifelse(variable=="CLA_taken2012", "All children taken into care",
                                         ifelse(variable=="SCLA_10to15", "10 to 15 years",
                                                ifelse(variable=="SCLA_16over", "16 years and over",
                                                       ifelse(variable=="SCLA_1to4", "1 to 4 years",
                                                              ifelse(variable=="SCLA_5to9", "5 to 9 years",
                                                                     ifelse(variable=="SCLA_AbNeg", "N1. Abuse or neglect",
                                                                            ifelse(variable=="SCLA_AbsPar", "N8. Absent parenting",
                                                                                   ifelse(variable=="SCLA_Cdisab", "N2. Child's disability",
                                                                                          ifelse(variable=="SCLA_FAcSt", "N.4 Family acute stress",
                                                                                                 ifelse(variable=="SCLA_FCO", "Full care order",
                                                                                                        ifelse(variable=="SCLA_FD", "N5. Family dysfunction",
                                                                                                               ifelse(variable=="SCLA_female", "Female",
                                                                                                                      ifelse(variable=="SCLA_ICO", "Interm care order",
                                                                                                                             ifelse(variable=="SCLA_LI", "N7. Low income",
                                                                                                                                    ifelse(variable=="SCLA_male", "Male",
                                                                                                                                           ifelse(variable=="SCLA_ONCT", "Remand",
                                                                                                                                                  ifelse(variable=="SCLA_PACE", "LA accommodation under PACE 1989",
                                                                                                                                                         ifelse(variable=="SCLA_ParIll", "N3. Parental illness or disability",
                                                                                                                                                                ifelse(variable=="SCLA_POG", "Placement order granted",
                                                                                                                                                                       ifelse(variable=="SCLA_S20", "Voluntary agreement under S20",
                                                                                                                                                                              ifelse(variable=="SCLA_SEPO", "Emergency protction order",
                                                                                                                                                                                     ifelse(variable=="SCLA_SORR", NA,
                                                                                                                                                                                            ifelse(variable=="SCLA_SUB", "N6. Socially unacceptable behaviour",
                                                                                                                                                                                                   ifelse(variable=="SCLA_U1", "Under 1 year",
                                                                                                                                                                                                          ifelse(variable=="SCLA_UCAO", "Child assessment order and in LA accommodation",
                                                                                                                                                                                                                 ifelse(variable=="SCLA_UPP",NA, NA))))))))))))))))))))))))))))
















march <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2012/UnderlyingData/SFR20_CLA2012.csv"),
                  colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::rename(LA_Code = New_geog_code,
                LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::select(-geog_l, -tidyr::starts_with("X"))%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "CLA_Mar2012"])*100))%>%
  dplyr::mutate(percent = ifelse(variable=="CLA_2012", 100, percent))%>%
  dplyr::mutate(percent = ifelse(variable=="CLA_P2yrs", NA, percent))%>%
  dplyr::mutate(percent = ifelse(variable=="CLA_stp2012", as.numeric(number)/as.numeric(number[variable=="CLA_2012"])*100, percent))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "child characteristic at 31st March",
                year="2012",
                subcategory = ifelse(variable=="CLA_Mar2012", "Age group",
                                     ifelse(variable=="CLA_2012", "Age group",
                                            ifelse(variable=="CLA_stp2012", "Age group",
                                                   ifelse(variable=="CLA_10to15", "Age group",
                                                          ifelse(variable=="CLA_16over", "Age group",
                                                                 ifelse(variable=="CLA_1to4", "Age group",NA)))))),
                subcategory = ifelse(variable=="CLA_Miss", NA,
                                     ifelse(variable=="CLA_Moth", NA,
                                            ifelse(variable=="CLA_1Pla", NA,
                                                   ifelse(variable=="CLA_2PLa", NA,
                                                          ifelse(variable=="CLA_3Pla", NA,
                                                                 ifelse(variable=="CLA_P2yrs", "placement stability", subcategory)))))),
                subcategory = ifelse(variable=="CLA_5to9", "Age group",
                                     ifelse(variable=="CLA_Adopt", "Placement",
                                            ifelse(variable=="CLA_Asian", "Ethnicity",
                                                   ifelse(variable=="CLA_Black", "Ethnicity",
                                                          ifelse(variable=="CLA_CPG", "Legal status",
                                                                 ifelse(variable=="CLA_EOTH", "Ethnicity", subcategory)))))),
                subcategory = ifelse(variable=="CLA_ExtPl", "LA of placement",
                                     ifelse(variable=="CLA_FCO", "Legal status",
                                            ifelse(variable=="CLA_female", "Gender",
                                                   ifelse(variable=="CLA_Fost", "Placement",
                                                          ifelse(variable=="CLA_FrAd", NA,                                                                                                                                    
                                                                 ifelse(variable=="CLA_ICO", "Legal status", subcategory)))))),
                subcategory = ifelse(variable=="CLAA_InBound", "Locality of placement",
                                     ifelse(variable=="CLA_IntPl", "LA of placement",
                                            ifelse(variable=="CLA_LAPl", "Placed inside the local authority boundary",subcategory))),
                subcategory = ifelse(variable=="CLA_male", "Gender",
                                     ifelse(variable=="CLA_Mixed", "Ethnicity",
                                            ifelse(variable=="CLA_NetGain", "Net gain of children by responsible LA",
                                                   ifelse(variable=="CLA_Nrep", "Place providers",
                                                          ifelse(variable=="CLA_Ocom", "Placement",
                                                                 ifelse(variable=="CLA_Ores", "Placement",
                                                                        ifelse(variable=="CLA_Oth", "Ethnicity",
                                                                               ifelse(variable=="CLA_OthLA", "Place providers",
                                                                                      ifelse(variable=="CLA_OthPl", "Placement", subcategory))))))))),
                subcategory =  ifelse(variable=="CLA_OthPP", "Place providers",
                                      ifelse(variable=="CLA_Outbound", "Locality of placement",
                                             ifelse(variable=="CLA_OwnP", "Place providers",
                                                    ifelse(variable=="CLA_Par", "Place providers",
                                                           ifelse(variable=="CLA_Parent", "Placement",
                                                                  ifelse(variable=="CLA_PlaceO", "Legal status", subcategory)))))),
                subcategory =  ifelse(variable=="CLA_Priv", "Place providers", subcategory),
                subcategory = ifelse(variable=="CLA_RSch", "Placement",
                                     ifelse(variable=="CLA_S20", "Legal status",
                                            ifelse(variable=="CLA_Secure", "Placement", subcategory))),
                subcategory =  ifelse(variable=="CLA_U1", "Age group",
                                      ifelse(variable=="CLA_UASC", "Unaccompanied asylum-seeking children",
                                             ifelse(variable=="CLA_Vol", "Place providers",
                                                    ifelse(variable=="CLA_White", "Ethnicity",
                                                           ifelse(variable=="CLA_YJLS", "Legal status",subcategory))))),
                variable = ifelse(variable=="CLA_Mar2012", "Total",
                                  ifelse(variable=="CLA_2012", "Total_during",
                                         ifelse(variable=="CLA_stp2012", "Children who were only looked after exclusively under a series of short term placements",
                                                ifelse(variable=="CLA_10to15", "10 to 15 years",
                                                       ifelse(variable=="CLA_16over", "16 years and over",
                                                              ifelse(variable=="CLA_1to4", "1 to 4 years", variable)))))),
                variable = ifelse(variable=="CLA_Miss", NA,
                                  ifelse(variable=="CLA_Moth", NA,
                                         ifelse(variable=="CLA_1Pla", NA,
                                                ifelse(variable=="CLA_2PLa", NA,
                                                       ifelse(variable=="CLA_3Pla", NA,
                                                              ifelse(variable=="CLA_P2yrs", "Living in the same placement for at least 2 years or are placed for adoption and their adoption and their adoptive placement together with their previous placement, last for at least 2 years", variable)))))),
                variable = ifelse(variable=="CLA_5to9", "5 to 9 years",
                                  ifelse(variable=="CLA_Adopt", "Placed for adoption",
                                         ifelse(variable=="CLA_Asian", "Asian or Asian British",
                                                ifelse(variable=="CLA_Black", "Black African, Caribbean or Black British",
                                                       ifelse(variable=="CLA_CPG", "Detained for child protection",
                                                              ifelse(variable=="CLA_EOTH", "Other ethnic group",
                                                                     ifelse(variable=="CLA_ExtPl", "2. Other LA children externally placed within the local authority boundary", variable))))))),
                variable = ifelse(variable=="CLA_FCO", "Full care order",
                                  ifelse(variable=="CLA_female", "Female",
                                         ifelse(variable=="CLA_Fost", "Foster placements",
                                                ifelse(variable=="CLA_FrAd", NA,                                                                                                                                    
                                                       ifelse(variable=="CLA_ICO", "Interim care order",
                                                              ifelse(variable=="CLAA_InBound", "Placed inside the local authority boundary",variable)))))),
                variable =               ifelse(variable=="CLA_LAPl", "Placed inside the local authority boundary",
                                                ifelse(variable=="CLA_male", "Male",
                                                       ifelse(variable=="CLA_Mixed", "Mixed or Multiple ethnic groups",
                                                              ifelse(variable=="CLA_NetGain", "Net gain of children by responsible LA",
                                                                     ifelse(variable=="CLA_Nrep", "Placement provider not reported",
                                                                            ifelse(variable=="CLA_Ocom", "Other placements in the community", variable)))))),
                variable = ifelse(variable=="CLA_Ores", "Other residential settings",
                                  ifelse(variable=="CLA_Oth", "Refused or information not yet available",
                                         ifelse(variable=="CLA_OthLA", "Other LA provision",
                                                ifelse(variable=="CLA_OthPl", "Other placements",
                                                       ifelse(variable=="CLA_OthPP", "Other public provision (e.g. by a PCT etc)",
                                                              ifelse(variable=="CLA_Outbound", "Placed outside the local authority boundary", variable)))))),
                variable = ifelse(variable=="CLA_OwnP", "Own provision (by the LA)",
                                  ifelse(variable=="CLA_Par", "Placed with parents or other person with parental responsibility",
                                         ifelse(variable=="CLA_Parent", "Parents or other person with parental responsibility",
                                                ifelse(variable=="CLA_PlaceO", "Placement order granted",
                                                       ifelse(variable=="CLA_Priv", "Private provision", variable))))),
                variable =       ifelse(variable=="CLA_RSch", "Residential schools",
                                        ifelse(variable=="CLA_S20", "Voluntary agreements under S20 CA 1989",
                                               ifelse(variable=="CLA_Secure", "Secure units children's homes and semi-independent living accommodation",
                                                      ifelse(variable=="CLA_U1", "Under 1 year",
                                                             ifelse(variable=="CLA_UASC", "Non-unaccompanied asylum-seeking children",
                                                                    ifelse(variable=="CLA_Vol", "Voluntary/third sector provision",
                                                                           ifelse(variable=="CLA_White", "White",
                                                                                  ifelse(variable=="CLA_YJLS", "Youth justice legal statuses",variable)))))))))






ceased <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2012/UnderlyingData/SFR_CEA2012.csv"),
                   colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::rename(LA_Code = New_geog_code,
                LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::select(-geog_l)%>%
  dplyr::mutate(`Special guardianship orders` = as.character(as.numeric(CLA_ceaSpecG)+as.numeric(CLA_ceaSpecG2), na.rm=F),
                `Adopted` = as.character(as.numeric(CLA_ceaAdop)+as.numeric(CLA_ceaAdop2), na.rm=F))%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "CLA_cease"])*100)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "ceased during",
                year="2012",
                subcategory = ifelse(variable == "CLA_cease", "Age on ceasing",
                                     ifelse(variable=="Special guardianship orders","Reason episode ceased",
                                            ifelse(variable== "Adopted", "Reason episode ceased",
                                                   ifelse(variable=="CLA_cea1015","Age on ceasing",
                                                          ifelse(variable=="CLA_cease16","Age on ceasing",
                                                                 ifelse(variable=="CLA_cea17","Age on ceasing",
                                                                        ifelse(variable=="CLA_cea18","Age on ceasing",
                                                                               ifelse(variable=="CLA_cea14","Age on ceasing",
                                                                                      ifelse(variable=="CLA_cea59","Age on ceasing",
                                                                                             ifelse(variable=="CLA_ceaAbroad","Reason episode ceased",
                                                                                                    # ifelse(variable=="CEA_Adop1","",
                                                                                                    #   ifelse(variable=="CEA_Adop2","",
                                                                                                    ifelse(variable=="CLA_ceaAgeAssmt","Reason episode ceased",
                                                                                                           ifelse(variable=="CLA_ceaROG","Reason episode ceased",
                                                                                                                  NA)))))))))))),
                subcategory = ifelse(variable == "CLA_cea_sen_cust", "Reason episode ceased",
                                     ifelse(variable=="CLA.ceaDied", "Reason episode ceased",
                                            ifelse(variable=="CLA_ceafe", "Gender",
                                                   ifelse(variable=="CLA_ceaIndLiv", "Reason episode ceased",
                                                          ifelse(variable=="CLA_ceaIndLiv2", "Reason episode ceased",
                                                                 ifelse(variable=="CLA_ceamal", "Gender",
                                                                        ifelse(variable=="CLA_ceaNoPar", "Reason episode ceased",
                                                                               ifelse(variable=="CLA_cea_OthRea", "Reason episode ceased",
                                                                                      ifelse(variable=="CLA_ceaParNPlan", "Reason episode ceased",
                                                                                             ifelse(variable=="CLA_ceaParPlan", "Reason episode ceased",
                                                                                                    ifelse(variable=="CLA_ceaRemEnd", "Reason episode ceased",
                                                                                                           ifelse(variable=="CLA_cea_tran_res", "Reason episode ceased",
                                                                                                                  subcategory)))))))))))),
                subcategory = #ifelse(variable=="CEA_SGO1","",
                  #      ifelse(variable=="CEA_SGO2","",
                  ifelse(variable=="CLA_ceataken","Reason episode ceased",
                         ifelse(variable=="CLA_cea1","Age on ceasing",
                                subcategory)),
                variable = ifelse(variable == "CLA_cease", "Total",
                                  ifelse(variable=="CLA_cea1015","10 to 15 years",
                                         ifelse(variable=="CLA_cease16","16 years",
                                                ifelse(variable=="CLA_cea17","17 years",
                                                       ifelse(variable=="CLA_cea18","18 years and over",
                                                              ifelse(variable=="CLA_cea14","1 to 4 years",
                                                                     ifelse(variable=="CLA_cea59","5 to 9 years",
                                                                            ifelse(variable=="CLA_ceaAbroad","Child moved abroad",
                                                                                   # ifelse(variable=="CEA_Adop1","",
                                                                                   #   ifelse(variable=="CEA_Adop2","",
                                                                                   ifelse(variable=="CLA_ceaAgeAssmt","Age assessment determined child aged 18 or over",
                                                                                          ifelse(variable=="CLA_ceaROG","Residence order or child arrangement order granted",
                                                                                                 variable)))))))))),
                variable = ifelse(variable == "CLA_cea_sen_cust", "Sentenced to custody",
                                  ifelse(variable=="CLA.ceaDied", "Died",
                                         ifelse(variable=="CLA_ceafe", "Female",
                                                ifelse(variable=="CLA_ceaIndLiv", "Moved into independent living (with supportive accommodation)",
                                                       ifelse(variable=="CLA_ceaIndLiv2", "Moved into independent living (with no formalised support)",
                                                              ifelse(variable=="CLA_ceamal", "Male",
                                                                     ifelse(variable=="CLA_ceaNoPar", "Left care to live with parents relatives or other person with no parental responsibility",
                                                                            ifelse(variable=="CLA_cea_OthRea", "Care ceased for any other reason",
                                                                                   ifelse(variable=="CLA_ceaParNPlan", "Returned home to live with parents or other person with parental responsibility which was not part of the care planning process",
                                                                                          ifelse(variable=="CLA_ceaParPlan", "Returned home to live with parents or other person with parental responsibility which was part of the care planning process",
                                                                                                 ifelse(variable=="CLA_ceaRemEnd", "Accommodation on remand ended",
                                                                                                        ifelse(variable=="CLA_cea_tran_res", "Transferred to residential care funded by adult social services",
                                                                                                               variable)))))))))))),
                variable = #ifelse(variable=="CEA_SGO1","",
                  #      ifelse(variable=="CEA_SGO2","",
                  ifelse(variable=="CLA_ceataken","Care taken by another local authority",
                         ifelse(variable=="CLA_cea1","Under 1 year",
                                variable)))%>%
  dplyr::filter(variable!="CLA_ceaAdop",
                variable!="CLA_ceaAdop2",
                variable!="CLA_ceaSpecG",
                variable!="CLA_ceaSpecG2",
                variable!="CLA_cea16",
                variable!="CLA_ceaPar")

characteristics <- rbind(characteristics, admitted, march, ceased)





####2011####

admitted <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2011/SFE21_ADM.csv"),
                     colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::rename(LA_Code = New_geog_code,
                LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::select(-geog_l, -tidyr::starts_with("X"))%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "CLA_started2011"])*100)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "started during",
                year="2011",
                subcategory = ifelse(variable=="CLA_started2011", "Taken into care",
                                     ifelse(variable=="CLA_taken2011", "Taken into care",
                                            ifelse(variable=="SCLA_10to15", "Age group",
                                                   ifelse(variable=="SCLA_16over", "Age group",
                                                          ifelse(variable=="SCLA_1to4", "Age group",
                                                                 ifelse(variable=="SCLA_5to9", "Age group",
                                                                        ifelse(variable=="SCLA_AbNeg", "Category of need",
                                                                               ifelse(variable=="SCLA_AbsPar", "Category of need",
                                                                                      ifelse(variable=="SCLA_Cdisab", "Category of need",
                                                                                             ifelse(variable=="SCLA_FAcSt", "Category of need",
                                                                                                    ifelse(variable=="SCLA_FCO", "Legal status",
                                                                                                           ifelse(variable=="SCLA_FD", "Category of need",
                                                                                                                  ifelse(variable=="SCLA_female", "Gender",
                                                                                                                         ifelse(variable=="SCLA_ICO", "Legal status",
                                                                                                                                ifelse(variable=="SCLA_LI", "Category of need",
                                                                                                                                       ifelse(variable=="SCLA_male", "Gender",
                                                                                                                                              ifelse(variable=="SCLA_ONCT", "Legal status",
                                                                                                                                                     ifelse(variable=="SCLA_PACE", "Legal status",
                                                                                                                                                            ifelse(variable=="SCLA_ParIll", "Category of need",
                                                                                                                                                                   ifelse(variable=="SCLA_POG", "Legal status",
                                                                                                                                                                          ifelse(variable=="SCLA_S20", "Legal status",
                                                                                                                                                                                 ifelse(variable=="SCLA_SEPO", "Legal status",
                                                                                                                                                                                        ifelse(variable=="SCLA_SORR", NA,
                                                                                                                                                                                               ifelse(variable=="SCLA_SUB", "Category of need",
                                                                                                                                                                                                      ifelse(variable=="SCLA_U1", "Age group",
                                                                                                                                                                                                             ifelse(variable=="SCLA_UCAO", "Legal status",
                                                                                                                                                                                                                    ifelse(variable=="SCLA_UPP",NA, NA))))))))))))))))))))))))))),
                variable = ifelse(variable=="CLA_started2011", "Total children",
                                  ifelse(variable=="CLA_taken2011", "All children taken into care",
                                         ifelse(variable=="SCLA_10to15", "10 to 15 years",
                                                ifelse(variable=="SCLA_16over", "16 years and over",
                                                       ifelse(variable=="SCLA_1to4", "1 to 4 years",
                                                              ifelse(variable=="SCLA_5to9", "5 to 9 years",
                                                                     ifelse(variable=="SCLA_AbNeg", "N1. Abuse or neglect",
                                                                            ifelse(variable=="SCLA_AbsPar", "N8. Absent parenting",
                                                                                   ifelse(variable=="SCLA_Cdisab", "N2. Child's disability",
                                                                                          ifelse(variable=="SCLA_FAcSt", "N.4 Family acute stress",
                                                                                                 ifelse(variable=="SCLA_FCO", "Full care order",
                                                                                                        ifelse(variable=="SCLA_FD", "N5. Family dysfunction",
                                                                                                               ifelse(variable=="SCLA_female", "Female",
                                                                                                                      ifelse(variable=="SCLA_ICO", "Interm care order",
                                                                                                                             ifelse(variable=="SCLA_LI", "N7. Low income",
                                                                                                                                    ifelse(variable=="SCLA_male", "Male",
                                                                                                                                           ifelse(variable=="SCLA_ONCT", "Remand",
                                                                                                                                                  ifelse(variable=="SCLA_PACE", "LA accommodation under PACE 1989",
                                                                                                                                                         ifelse(variable=="SCLA_ParIll", "N3. Parental illness or disability",
                                                                                                                                                                ifelse(variable=="SCLA_POG", "Placement order granted",
                                                                                                                                                                       ifelse(variable=="SCLA_S20", "Voluntary agreement under S20",
                                                                                                                                                                              ifelse(variable=="SCLA_SEPO", "Emergency protction order",
                                                                                                                                                                                     ifelse(variable=="SCLA_SORR", NA,
                                                                                                                                                                                            ifelse(variable=="SCLA_SUB", "N6. Socially unacceptable behaviour",
                                                                                                                                                                                                   ifelse(variable=="SCLA_U1", "Under 1 year",
                                                                                                                                                                                                          ifelse(variable=="SCLA_UCAO", "Child assessment order and in LA accommodation",
                                                                                                                                                                                                                 ifelse(variable=="SCLA_UPP",NA, NA))))))))))))))))))))))))))))


















march <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2011/SFR21_CLA.csv"),
                  colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::rename(LA_Code = New_geog_code,
                LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::select(-geog_l, -tidyr::starts_with("X"))%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "CLA_Mar2011"])*100))%>%
  dplyr::mutate(percent = ifelse(variable=="CLA_2011", 100, percent))%>%
  dplyr::mutate(percent = ifelse(variable=="CLA_P2yrs", NA, percent))%>%
  dplyr::mutate(percent = ifelse(variable=="CLA_stp2011", as.numeric(number)/as.numeric(number[variable=="CLA_2011"])*100, percent))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "child characteristic at 31st March",
                year="2011",
                subcategory = ifelse(variable=="CLA_Mar2011", "Age group",
                                     ifelse(variable=="CLA_2011", "Age group",
                                            ifelse(variable=="CLA_stp2011", "Age group",
                                                   ifelse(variable=="CLA_10to15", "Age group",
                                                          ifelse(variable=="CLA_16over", "Age group",
                                                                 ifelse(variable=="CLA_1to4", "Age group",NA)))))),
                subcategory = ifelse(variable=="CLA_Miss", NA,
                                     ifelse(variable=="CLA_Moth", NA,
                                            ifelse(variable=="CLA_1Pla", NA,
                                                   ifelse(variable=="CLA_2PLa", NA,
                                                          ifelse(variable=="CLA_3Pla", NA,
                                                                 ifelse(variable=="CLA_P2yrs", "placement stability", subcategory)))))),
                subcategory = ifelse(variable=="CLA_5to9", "Age group",
                                     ifelse(variable=="CLA_Adopt", "Placement",
                                            ifelse(variable=="CLA_Asian", "Ethnicity",
                                                   ifelse(variable=="CLA_Black", "Ethnicity",
                                                          ifelse(variable=="CLA_CPG", "Legal status",
                                                                 ifelse(variable=="CLA_EOTH", "Ethnicity", subcategory)))))),
                subcategory = ifelse(variable=="CLA_ExtPl", "LA of placement",
                                     ifelse(variable=="CLA_FCO", "Legal status",
                                            ifelse(variable=="CLA_female", "Gender",
                                                   ifelse(variable=="CLA_Fost", "Placement",
                                                          ifelse(variable=="CLA_FrAd", NA,                                                                                                                                    
                                                                 ifelse(variable=="CLA_ICO", "Legal status", subcategory)))))),
                subcategory = ifelse(variable=="CLAA_InBound", "Locality of placement",
                                     ifelse(variable=="CLA_IntPl", "LA of placement",
                                            ifelse(variable=="CLA_LAPl", "Placed inside the local authority boundary",subcategory))),
                subcategory = ifelse(variable=="CLA_male", "Gender",
                                     ifelse(variable=="CLA_Mixed", "Ethnicity",
                                            ifelse(variable=="CLA_NetGain", "Net gain of children by responsible LA",
                                                   ifelse(variable=="CLA_Nrep", "Place providers",
                                                          ifelse(variable=="CLA_Ocom", "Placement",
                                                                 ifelse(variable=="CLA_Ores", "Placement",
                                                                        ifelse(variable=="CLA_Oth", "Ethnicity",
                                                                               ifelse(variable=="CLA_OthLA", "Place providers",
                                                                                      ifelse(variable=="CLA_OthPl", "Placement", subcategory))))))))),
                subcategory =  ifelse(variable=="CLA_OthPP", "Place providers",
                                      ifelse(variable=="CLA_Outbound", "Locality of placement",
                                             ifelse(variable=="CLA_OwnP", "Place providers",
                                                    ifelse(variable=="CLA_Par", "Place providers",
                                                           ifelse(variable=="CLA_Parent", "Placement",
                                                                  ifelse(variable=="CLA_PlaceO", "Legal status", subcategory)))))),
                subcategory =  ifelse(variable=="CLA_Priv", "Place providers", subcategory),
                subcategory = ifelse(variable=="CLA_RSch", "Placement",
                                     ifelse(variable=="CLA_S20", "Legal status",
                                            ifelse(variable=="CLA_Secure", "Placement", subcategory))),
                subcategory =  ifelse(variable=="CLA_U1", "Age group",
                                      ifelse(variable=="CLA_UASC", "Unaccompanied asylum-seeking children",
                                             ifelse(variable=="CLA_Vol", "Place providers",
                                                    ifelse(variable=="CLA_White", "Ethnicity",
                                                           ifelse(variable=="CLA_YJLS", "Legal status",subcategory))))),
                variable = ifelse(variable=="CLA_Mar2011", "Total",
                                  ifelse(variable=="CLA_2011", "Total_during",
                                         ifelse(variable=="CLA_stp2011", "Children who were only looked after exclusively under a series of short term placements",
                                                ifelse(variable=="CLA_10to15", "10 to 15 years",
                                                       ifelse(variable=="CLA_16over", "16 years and over",
                                                              ifelse(variable=="CLA_1to4", "1 to 4 years", variable)))))),
                variable = ifelse(variable=="CLA_Miss", NA,
                                  ifelse(variable=="CLA_Moth", NA,
                                         ifelse(variable=="CLA_1Pla", NA,
                                                ifelse(variable=="CLA_2PLa", NA,
                                                       ifelse(variable=="CLA_3Pla", NA,
                                                              ifelse(variable=="CLA_P2yrs", "Living in the same placement for at least 2 years or are placed for adoption and their adoption and their adoptive placement together with their previous placement, last for at least 2 years", variable)))))),
                variable = ifelse(variable=="CLA_5to9", "5 to 9 years",
                                  ifelse(variable=="CLA_Adopt", "Placed for adoption",
                                         ifelse(variable=="CLA_Asian", "Asian or Asian British",
                                                ifelse(variable=="CLA_Black", "Black African, Caribbean or Black British",
                                                       ifelse(variable=="CLA_CPG", "Detained for child protection",
                                                              ifelse(variable=="CLA_EOTH", "Other ethnic group",
                                                                     ifelse(variable=="CLA_ExtPl", "2. Other LA children externally placed within the local authority boundary", variable))))))),
                variable = ifelse(variable=="CLA_FCO", "Full care order",
                                  ifelse(variable=="CLA_female", "Female",
                                         ifelse(variable=="CLA_Fost", "Foster placements",
                                                ifelse(variable=="CLA_FrAd", NA,                                                                                                                                    
                                                       ifelse(variable=="CLA_ICO", "Interim care order",
                                                              ifelse(variable=="CLAA_InBound", "Placed inside the local authority boundary",variable)))))),
                variable =               ifelse(variable=="CLA_LAPl", "Placed inside the local authority boundary",
                                                ifelse(variable=="CLA_male", "Male",
                                                       ifelse(variable=="CLA_Mixed", "Mixed or Multiple ethnic groups",
                                                              ifelse(variable=="CLA_NetGain", "Net gain of children by responsible LA",
                                                                     ifelse(variable=="CLA_Nrep", "Placement provider not reported",
                                                                            ifelse(variable=="CLA_Ocom", "Other placements in the community", variable)))))),
                variable = ifelse(variable=="CLA_Ores", "Other residential settings",
                                  ifelse(variable=="CLA_Oth", "Refused or information not yet available",
                                         ifelse(variable=="CLA_OthLA", "Other LA provision",
                                                ifelse(variable=="CLA_OthPl", "Other placements",
                                                       ifelse(variable=="CLA_OthPP", "Other public provision (e.g. by a PCT etc)",
                                                              ifelse(variable=="CLA_Outbound", "Placed outside the local authority boundary", variable)))))),
                variable = ifelse(variable=="CLA_OwnP", "Own provision (by the LA)",
                                  ifelse(variable=="CLA_Par", "Placed with parents or other person with parental responsibility",
                                         ifelse(variable=="CLA_Parent", "Parents or other person with parental responsibility",
                                                ifelse(variable=="CLA_PlaceO", "Placement order granted",
                                                       ifelse(variable=="CLA_Priv", "Private provision", variable))))),
                variable =       ifelse(variable=="CLA_RSch", "Residential schools",
                                        ifelse(variable=="CLA_S20", "Voluntary agreements under S20 CA 1989",
                                               ifelse(variable=="CLA_Secure", "Secure units children's homes and semi-independent living accommodation",
                                                      ifelse(variable=="CLA_U1", "Under 1 year",
                                                             ifelse(variable=="CLA_UASC", "Non-unaccompanied asylum-seeking children",
                                                                    ifelse(variable=="CLA_Vol", "Voluntary/third sector provision",
                                                                           ifelse(variable=="CLA_White", "White",
                                                                                  ifelse(variable=="CLA_YJLS", "Youth justice legal statuses",variable)))))))))







ceased <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2011/SFR21_CEA.csv"),
                   colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::rename(LA_Code = New_geog_code,
                LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::select(-geog_l)%>%
  dplyr::mutate(`Special guardianship orders` = as.character(as.numeric(CLA_ceaSpecG)+as.numeric(CLA_ceaSpecG2), na.rm=F),
                `Adopted` = as.character(as.numeric(CLA_ceaAdop)+as.numeric(CLA_ceaAdop2), na.rm=F))%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "CLA_cease"])*100)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "ceased during",
                year="2011",
                subcategory = ifelse(variable == "CLA_cease", "Age on ceasing",
                                     ifelse(variable=="Special guardianship orders","Reason episode ceased",
                                            ifelse(variable== "Adopted", "Reason episode ceased",
                                                   ifelse(variable=="CLA_cea1015","Age on ceasing",
                                                          ifelse(variable=="CLA_cease16","Age on ceasing",
                                                                 ifelse(variable=="CLA_cea17","Age on ceasing",
                                                                        ifelse(variable=="CLA_cea18","Age on ceasing",
                                                                               ifelse(variable=="CLA_cea14","Age on ceasing",
                                                                                      ifelse(variable=="CLA_cea59","Age on ceasing",
                                                                                             ifelse(variable=="CLA_ceaAbroad","Reason episode ceased",
                                                                                                    # ifelse(variable=="CEA_Adop1","",
                                                                                                    #   ifelse(variable=="CEA_Adop2","",
                                                                                                    ifelse(variable=="CLA_ceaAgeAssmt","Reason episode ceased",
                                                                                                           ifelse(variable=="CLA_ceaROG","Reason episode ceased",
                                                                                                                  NA)))))))))))),
                subcategory = ifelse(variable == "CLA_cea_sen_cust", "Reason episode ceased",
                                     ifelse(variable=="CLA.ceaDied", "Reason episode ceased",
                                            ifelse(variable=="CLA_ceafe", "Gender",
                                                   ifelse(variable=="CLA_ceaIndLiv", "Reason episode ceased",
                                                          ifelse(variable=="CLA_ceaIndLiv2", "Reason episode ceased",
                                                                 ifelse(variable=="CLA_ceamal", "Gender",
                                                                        ifelse(variable=="CLA_ceaNoPar", "Reason episode ceased",
                                                                               ifelse(variable=="CLA_cea_OthRea", "Reason episode ceased",
                                                                                      ifelse(variable=="CLA_ceaParNPlan", "Reason episode ceased",
                                                                                             ifelse(variable=="CLA_ceaParPlan", "Reason episode ceased",
                                                                                                    ifelse(variable=="CLA_ceaRemEnd", "Reason episode ceased",
                                                                                                           ifelse(variable=="CLA_cea_tran_res", "Reason episode ceased",
                                                                                                                  subcategory)))))))))))),
                subcategory = #ifelse(variable=="CEA_SGO1","",
                  #      ifelse(variable=="CEA_SGO2","",
                  ifelse(variable=="CLA_ceataken","Reason episode ceased",
                         ifelse(variable=="CLA_cea1","Age on ceasing",
                                subcategory)),
                variable = ifelse(variable == "CLA_cease", "Total",
                                  ifelse(variable=="CLA_cea1015","10 to 15 years",
                                         ifelse(variable=="CLA_cease16","16 years",
                                                ifelse(variable=="CLA_cea17","17 years",
                                                       ifelse(variable=="CLA_cea18","18 years and over",
                                                              ifelse(variable=="CLA_cea14","1 to 4 years",
                                                                     ifelse(variable=="CLA_cea59","5 to 9 years",
                                                                            ifelse(variable=="CLA_ceaAbroad","Child moved abroad",
                                                                                   # ifelse(variable=="CEA_Adop1","",
                                                                                   #   ifelse(variable=="CEA_Adop2","",
                                                                                   ifelse(variable=="CLA_ceaAgeAssmt","Age assessment determined child aged 18 or over",
                                                                                          ifelse(variable=="CLA_ceaROG","Residence order or child arrangement order granted",
                                                                                                 variable)))))))))),
                variable = ifelse(variable == "CLA_cea_sen_cust", "Sentenced to custody",
                                  ifelse(variable=="CLA.ceaDied", "Died",
                                         ifelse(variable=="CLA_ceafe", "Female",
                                                ifelse(variable=="CLA_ceaIndLiv", "Moved into independent living (with supportive accommodation)",
                                                       ifelse(variable=="CLA_ceaIndLiv2", "Moved into independent living (with no formalised support)",
                                                              ifelse(variable=="CLA_ceamal", "Male",
                                                                     ifelse(variable=="CLA_ceaNoPar", "Left care to live with parents relatives or other person with no parental responsibility",
                                                                            ifelse(variable=="CLA_cea_OthRea", "Care ceased for any other reason",
                                                                                   ifelse(variable=="CLA_ceaParNPlan", "Returned home to live with parents or other person with parental responsibility which was not part of the care planning process",
                                                                                          ifelse(variable=="CLA_ceaParPlan", "Returned home to live with parents or other person with parental responsibility which was part of the care planning process",
                                                                                                 ifelse(variable=="CLA_ceaRemEnd", "Accommodation on remand ended",
                                                                                                        ifelse(variable=="CLA_cea_tran_res", "Transferred to residential care funded by adult social services",
                                                                                                               variable)))))))))))),
                variable = #ifelse(variable=="CEA_SGO1","",
                  #      ifelse(variable=="CEA_SGO2","",
                  ifelse(variable=="CLA_ceataken","Care taken by another local authority",
                         ifelse(variable=="CLA_cea1","Under 1 year",
                                variable)))%>%
  dplyr::filter(variable!="CLA_ceaAdop",
                variable!="CLA_ceaAdop2",
                variable!="CLA_ceaSpecG",
                variable!="CLA_ceaSpecG2",
                variable!="CLA_cea16",
                variable!="CLA_ceaPar")


characteristics <- rbind(characteristics, admitted, march, ceased)










####2010####


admitted <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2010/SFR27_2010_Started.csv"),
                     colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::mutate(LA_Code = NA)%>%
  dplyr::rename(LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::select(LA_Code, LA.Number, LA_Name, StartTotal_2010)%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "StartTotal_2010"])*100)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "started during",
                year="2010",
                subcategory = ifelse(variable=="StartTotal_2010", "Taken into care",NA),
                variable = ifelse(variable=="StartTotal_2010", "Total children", NA))








ceased <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2010/SFR27_2010_Ceased.csv"),
                   colClasses = "character")%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::rename(LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::select(-geog_l)%>%
  dplyr::mutate(LA_Code = NA,
                `Special guardianship orders` = as.character(as.numeric(CeasedSpeGuaForm_2010)+as.numeric(CeasedSpecGuaOth_2010), na.rm=F),
                `Adopted` = as.character(as.numeric(CeasedAdoptUnop_2010)+as.numeric(CeasedAdoptDisp_2010), na.rm=F))%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "CeasedTotal_2010"])*100)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "ceased during",
                year="2010",
                subcategory = ifelse(variable == "CeasedTotal_2010", "Age on ceasing",
                                     ifelse(variable=="Special guardianship orders","Reason episode ceased",
                                            ifelse(variable== "Adopted", "Reason episode ceased",
                                                   ifelse(variable=="Ceased1015","Age on ceasing",
                                                          ifelse(variable=="Ceasedse16","Age on ceasing",
                                                                 ifelse(variable=="Ceased17","Age on ceasing",
                                                                        ifelse(variable=="Ceased18","Age on ceasing",
                                                                               ifelse(variable=="Ceased14","Age on ceasing",
                                                                                      ifelse(variable=="Ceased59","Age on ceasing",
                                                                                             ifelse(variable=="CeasedAbroad","Reason episode ceased",
                                                                                                    # ifelse(variable=="CEA_Adop1","",
                                                                                                    #   ifelse(variable=="CEA_Adop2","",
                                                                                                    ifelse(variable=="CeasedAgeAssmt","Reason episode ceased",
                                                                                                           ifelse(variable=="CeasedResOrd_2010","Reason episode ceased",
                                                                                                                  NA)))))))))))),
                subcategory = ifelse(variable == "CeasedCust_2010", "Reason episode ceased",
                                     ifelse(variable=="CeasedDied_2010", "Reason episode ceased",
                                            ifelse(variable=="Ceasedfe", "Gender",
                                                   ifelse(variable=="CeasedIndSup_2010", "Reason episode ceased",
                                                          ifelse(variable=="CeasedIndNoSup_2010", "Reason episode ceased",
                                                                 ifelse(variable=="Ceasedmal", "Gender",
                                                                        ifelse(variable=="CeasedNoPar", "Reason episode ceased",
                                                                               ifelse(variable=="CeasedOther_2010", "Reason episode ceased",
                                                                                      ifelse(variable=="CeasedParNPlan", "Reason episode ceased",
                                                                                             ifelse(variable=="CeasedParPlan", "Reason episode ceased",
                                                                                                    ifelse(variable=="CeasedRemEnd", "Reason episode ceased",
                                                                                                           ifelse(variable=="CeasedAduCare_2010", "Reason episode ceased",
                                                                                                                  subcategory)))))))))))),
                subcategory = #ifelse(variable=="CEA_SGO1","",
                  #      ifelse(variable=="CEA_SGO2","",
                  ifelse(variable=="CeasedAnotherLA_2010","Reason episode ceased",
                         ifelse(variable=="Ceased1","Age on ceasing",
                                subcategory)),
                variable = ifelse(variable == "CeasedTotal_2010", "Total",
                                  ifelse(variable=="Ceased1015","10 to 15 years",
                                         ifelse(variable=="Ceasedse16","16 years",
                                                ifelse(variable=="Ceased17","17 years",
                                                       ifelse(variable=="Ceased18","18 years and over",
                                                              ifelse(variable=="Ceased14","1 to 4 years",
                                                                     ifelse(variable=="Ceased59","5 to 9 years",
                                                                            ifelse(variable=="CeasedAbroad","Child moved abroad",
                                                                                   # ifelse(variable=="CEA_Adop1","",
                                                                                   #   ifelse(variable=="CEA_Adop2","",
                                                                                   ifelse(variable=="CeasedAgeAssmt","Age assessment determined child aged 18 or over",
                                                                                          ifelse(variable=="CeasedResOrd_2010","Residence order or child arrangement order granted",
                                                                                                 variable)))))))))),
                variable = ifelse(variable == "CeasedCust_2010", "Sentenced to custody",
                                  ifelse(variable=="CeasedDied_2010", "Died",
                                         ifelse(variable=="Ceasedfe", "Female",
                                                ifelse(variable=="CeasedIndSup_2010", "Moved into independent living (with supportive accommodation)",
                                                       ifelse(variable=="CeasedIndNoSup_2010", "Moved into independent living (with no formalised support)",
                                                              ifelse(variable=="Ceasedmal", "Male",
                                                                     ifelse(variable=="CeasedNoPar", "Left care to live with parents relatives or other person with no parental responsibility",
                                                                            ifelse(variable=="CeasedOther_2010", "Care ceased for any other reason",
                                                                                   ifelse(variable=="CeasedParNPlan", "Returned home to live with parents or other person with parental responsibility which was not part of the care planning process",
                                                                                          ifelse(variable=="CeasedParPlan", "Returned home to live with parents or other person with parental responsibility which was part of the care planning process",
                                                                                                 ifelse(variable=="CeasedRemEnd", "Accommodation on remand ended",
                                                                                                        ifelse(variable=="CeasedAduCare_2010", "Transferred to residential care funded by adult social services",
                                                                                                               variable)))))))))))),
                variable = #ifelse(variable=="CEA_SGO1","",
                  #      ifelse(variable=="CEA_SGO2","",
                  ifelse(variable=="CeasedAnotherLA_2010","Care taken by another local authority",
                         ifelse(variable=="Ceased1","Under 1 year",
                                variable)))%>%
  dplyr::filter(variable!="CeasedAdoptDisp_2010",
                variable!="CeasedAdoptUnop_2010",
                variable!="CeasedSpeGuaForm_2010",
                variable!="CeasedSpecGuaOth_2010",
                variable!="Ceased16",
                variable!="CeasedHome_2010")






march <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Children_Placement_Characteristics/2010/SFR27_2010_March.csv"),
                  colClasses = "character")[c(1:4)]%>%
  dplyr::mutate_all(~ str_replace(., ",", ""))%>%
  dplyr::filter(geog_l=="LA")%>%
  dplyr::rename(LA.Number = geog_c,
                LA_Name = geog_n)%>%
  dplyr::mutate(LA_Code=NA)%>%
  dplyr::select(-geog_l, -tidyr::starts_with("X"))%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "MarTotal_2010"])*100))%>%
  #dplyr::mutate(percent = ifelse(variable=="CLA_2010", 100, percent))%>%
  #dplyr::mutate(percent = ifelse(variable=="CLA_P2yrs", NA, percent))%>%
  #dplyr::mutate(percent = ifelse(variable=="CLA_stp2010", as.numeric(number)/as.numeric(number[variable=="CLA_2010"])*100, percent))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "child characteristic at 31st March",
                year="2010",
                subcategory = "Age group",
                variable = "Total")












characteristics <- rbind(characteristics, admitted, march, ceased)



#write.csv(characteristics, "C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/childrens_social_care_data/Final_Data/categories/placement_characteristics.csv")








