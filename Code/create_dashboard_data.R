library(tidyverse, curl)
options(scipen=999)
source("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Code/outcomes_cleaning_function.R")
outcomes <- create_outcomes_data()
source("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Code/characteristics_cleaning_function.R")
characteristics <- create_characteristics_data()
source("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Code/expenditure_cleaning_function.R")
expenditure <- create_expenditure_data()
source("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Code/provider_cleaning_function.R")
ProviderData <- create_provider_data()

dashboard_data <- rbind(outcomes, characteristics, expenditure)%>%
  dplyr::mutate(LA_Name = LA_Name %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
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


provider_at_march <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/provider_at_march.csv"), skip=3)%>%
  dplyr::filter(Provision.type=="Children's home",
                Registration.status=="Active")%>%
  dplyr::select(Local.authority, Sector, Places)%>%
  dplyr::mutate(childrens_homes_n=1,
                LA_Name = Local.authority %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
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
                year=2022)%>%#year is actually 2023 just for ease
  dplyr::rename(subcategory=Sector)%>%
  dplyr::group_by(LA_Name, variable, year)%>%
  dplyr::summarise(total = sum(number, na.rm=T),
                   subcategory=subcategory,
                   number=number)%>%
  dplyr::ungroup()%>%
  dplyr::mutate(percent = as.numeric(number) / as.numeric(total)*100)%>%
  dplyr::select(-total)%>%
  dplyr::left_join(., dashboard_data %>% select(LA_Name, LA_Code)%>% dplyr::distinct() %>% dplyr::filter(!is.na(LA_Code),
                                                                                                         LA_Code!="",
                                                                                                         stringr::str_starts(LA_Code, 'E')),
                   by="LA_Name")%>%
  dplyr::mutate(LA.Number=NA,
                category = "Childrens homes")


dashboard_data <- rbind(dashboard_data, provider_at_march)

provider_at_march <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/provider_at_march.csv"), skip=3)%>%
  dplyr::filter(Provision.type=="Children's home",
                Registration.status=="Active")%>%
  dplyr::mutate(Sector = ifelse(Sector=="Private", "For profit",
                                ifelse(Sector=="Health Authority", "Local Authority",
                                       ifelse(Sector=="Voluntary", "Third Sector",
                                              ifelse(Sector=="Local Authority", "Local Authority", NA)))))%>%
  tidyr::pivot_longer(cols = c(Overall.experiences.and.progress.of.children.and.young.people, How.well.children.and.young.people.are.helped.and.protected, The.effectiveness.of.leaders.and.managers), names_to = "Domain", values_to = "Rating")%>%
  dplyr::filter(Rating!="")



source("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Code/leavers_joiners_cleaning_function.R")
enter_exit <- create_market_exits_entries()


write.csv(dashboard_data, "C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/childrens_social_care_data/Final_Data/outputs/dashboard_data.csv")
write.csv(enter_exit, "C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/childrens_social_care_data/Final_Data/outputs/enter_exit.csv")
write.csv(provider_at_march, "C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/childrens_social_care_data/Final_Data/outputs/active_chomes_2023.csv")



####plots for pres####


plot1 <- dashboard_data %>%dplyr::filter(variable=="Own provision (by the LA)"|
                                  (category == "Expenditure" & 
                                    subcategory == "Own_provision" &
                                     variable=="Total Children Looked After"
                                  ))%>%
  dplyr::mutate(dataset = ifelse(category=="Expenditure","Expenditure", "Placements"),
                percent=as.numeric(percent),
                year = as.numeric(year))%>%
  ggplot(., aes(x = year, y = percent)) +
  geom_point(size = 2, color = "#B4CFEE", alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE, colour = "#2A6EBB") +
  labs(
    x = "Year",
    y = "In House (%)",
    title = "Looked after children's services",
    color = ""
  )+
  theme_bw()+
  facet_wrap(~dataset,nrow = 1)+
  theme(text = element_text(size=20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.length=unit(.28, "cm"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size=24),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=20),
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", size = 1),
        legend.text = element_text(size=20),
        legend.position = "top",
        strip.background = element_rect(fill="gray90", colour="black", size=1),
        strip.text = element_text(face="bold", size=16),
        title=element_text(face="bold")) +
  theme(panel.spacing.x = unit(4, "mm"))
