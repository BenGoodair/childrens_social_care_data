library(tidyverse, curl)
source("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Code/outcomes_cleaning_function.R")
outcomes <- create_outcomes_data()
source("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Code/characteristics_cleaning_function.R")
characteristics <- create_characteristics_data()
source("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Code/expenditure_cleaning_function.R")
expenditure <- create_expenditure_data()

dashboard_data <- rbind(outcomes, characteristics, expenditure)%>%
  dplyr::mutate(LA_Name = LA_Name %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim())

source("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Code/provider_cleaning_function.R")
ProviderData <- create_provider_data()

provider_at_march <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/provider_at_march.csv"), skip=3)%>%
  dplyr::filter(Provision.type=="Children's home",
                Registration.status=="Active")%>%
  dplyr::select(Local.authority, Sector, Places)%>%
  dplyr::mutate(childrens_homes_n=1,
                LA_Name = Local.authority %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim(),
                Sector = Sector %>% gsub( "Health Authority", "Local Authority", .))%>%
  dplyr::group_by(LA_Name, Sector)%>%
  dplyr::summarise(childrens_homes_n = sum(childrens_homes_n),
                   Places = sum(Places))%>%
  dplyr::ungroup()%>%
  tidyr::pivot_longer(cols = !c(LA_Name, Sector), names_to = "variable", values_to="number")%>%
  dplyr::mutate(category="Childrens Homes",
                year=2023,
                LA_Code=NA,
                LA.Number=NA)%>%
  dplyr::rename(subcategory=Sector)%>%
  dplyr::group_by(LA_Name, variable)%>%
  dplyr::mutate(percent = as.numeric(number) / (as.numeric(number[subcategory == "Private"])+as.numeric(number[subcategory == "Voluntary"])+as.numeric(number[subcategory == "Local Authority"]))*100)%>%
  dplyr::ungroup()


dashboard_data <- merge(dashboard_data, 
                        provider_at_march%>%
                          dplyr::select(Local.authority, Sector, Places)%>%
                          dplyr::mutate(childrens_homes_n=1,
                                        LA_Name = Local.authority %>%
                                          gsub('&', 'and', .) %>%
                                          gsub('[[:punct:] ]+', ' ', .) %>%
                                          toupper() %>%
                                          gsub("CITY OF", "",.)%>%
                                          gsub("UA", "",.)%>%
                                          gsub("COUNTY OF", "",.)%>%
                                          gsub("ROYAL BOROUGH OF", "",.)%>%
                                          gsub("LEICESTER CITY", "LEICESTER",.)%>%
                                          gsub("UA", "",.)%>%
                                          gsub("DARWIN", "DARWEN", .)%>%
                                          gsub("AND DARWEN", "WITH DARWEN", .)%>%
                                          gsub("NE SOM", "NORTH EAST SOM", .)%>%
                                          gsub("N E SOM", "NORTH EAST SOM", .)%>%
                                          str_trim())%>%
                        dplyr::group_by(LA_Name, Sector)%>%
                        dplyr::summarise(childrens_homes_n = sum(childrens_homes_n),
                                         Places = sum(Places))%>%
                          dplyr::ungroup()%>%
                          tidyr::pivot_wider(names_from = Sector, values_from = c(childrens_homes_n, Places)),
                        by="LA_Name")

write.csv(dashboard_data, "C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/childrens_social_care_data/Final_Data/outputs/dashboard_data.csv")

