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
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code), 
                      names_to = "variable", values_to = "number")%>%
  dplyr::group_by(LA_Name, LA_Code, LA.Number) %>%
  dplyr::mutate(percent = as.character(as.numeric(number) / as.numeric(number[variable == "CLA_cease2017"])*100)) %>%
  dplyr::ungroup()%>%
  dplyr::mutate(category = "started during",
                year="2017",
                variable = ifelse(variable == "CLA_cease2017", "",
                                  ifelse(variable=="CEA_10to15","",
                                         ifelse(variable=="CEA_16","",
                                                ifelse(variable=="CEA_17","",
                                                       ifelse(variable=="CEA_18over","",
                                                              ifelse(variable=="CEA_1to4","",
                                                                     ifelse(variable=="CEA_5to9","",
                                                                            ifelse(variable=="CEA_Abroad","",
                                                                                   ifelse(variable=="CEA_Adop1","",
                                                                                          ifelse(variable=="CEA_Adop2","",
                                                                                                 ifelse(variable=="CEA_AgeAssmt","",
                                                                                                        ifelse(variable=="CEA_CAO","",
                                                                                                        NA)))))))))))),
                variable = ifelse(variable == "CEA_Custody", "",
                                  ifelse(variable=="CEA_Died", "",
                                         ifelse(variable=="CEA_female", "",
                                                ifelse(variable=="", "",
                                                       ifelse(variable=="", "",
                                                              ifelse(variable=="", "",
                                                                     ifelse(variable=="", "",
                                                                            ifelse(variable=="", "",
                                                                                   ifelse(variable=="", "",
                                                                                          ifelse(variable=="", "",
                                                                                                 ifelse(variable=="", "",
                                                                                                        ifelse(variable=="", "",
                                                                                                        variable)))))))))))))



characteristics <- rbind(characteristics, admitted, march)
  
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



characteristics <- rbind(characteristics, admitted, march)


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





















characteristics <- rbind(characteristics, admitted)
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














characteristics <- rbind(characteristics, admitted, march)

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







characteristics <- rbind(characteristics, admitted)


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







characteristics <- rbind(characteristics, admitted)





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







characteristics <- rbind(characteristics, admitted)










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







characteristics <- rbind(characteristics, admitted)












