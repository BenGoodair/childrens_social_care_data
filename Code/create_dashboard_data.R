source("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Code/outcomes_cleaning_function.R")
outcomes <- create_outcomes_data()
source("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Code/characteristics_cleaning_function.R")
characteristics <- create_characteristics_data()
source("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Code/expenditure_cleaning_function.R")
expenditure <- create_expenditure_data()

dashboard_data <- rbind(outcomes, characteristics, expenditure)

source("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Code/provider_cleaning_function.R")
ProviderData <- create_provider_data()


library(tidyverse)

ProviderData <- ProviderData %>%
  mutate(date = as.Date(Registration.date, format = "%d/%m/%Y"),
         month = format(date, "%m/%y"),
         time = as.integer(time_length(difftime(date, as.Date("2022-12-01")), "months")))

Providernobs <- ProviderData %>%
  dplyr::filter(Provision.type == "Children's home",
                !is.na(Local.authority)) %>%
  dplyr::select(time, Sector, URN, Local.authority, Places) %>%
  dplyr::distinct()

nobsByIdih <- Providernobs %>%
  dplyr::group_by(time, Local.authority, Sector) %>%
  dplyr::summarize(Places = sum(Places))

nobsprive <- nobsByIdih %>%
  dplyr::filter(Sector == "Private")

nobsvol <- nobsByIdih %>%
  dplyr::filter(Sector == "Voluntary")

nobsla <- nobsByIdih %>%
  dplyr::filter(Sector == "Local Authority")

all_data <- tibble(Sector = "Private",
                   Local.authority = rep(unique(nobsprive$Local.authority), each = 596),
                   time = rep(-595:0, times = 145),
                   er = 1)

nobsprive <- nobsprive %>%
  right_join(all_data, by = c("Sector", "time", "Local.authority")) %>%
  dplyr::arrange(time)%>%
  dplyr::group_by(Local.authority)%>%
  mutate(nobs = ifelse(is.na(nobs), 0, nobs),
         cumulative = cumsum(nobs))%>%
  dplyr::ungroup()


all_data <- tibble(Sector = "Local Authority",
                   Local.authority = rep(unique(nobsprive$Local.authority), each = 596),
                   time = rep(-595:0, times = 145),
                   er = 1)

nobsla <- nobsla %>%
  right_join(all_data, by = c("Sector", "time", "Local.authority")) %>%
  dplyr::arrange(time)%>%
  dplyr::group_by(Local.authority)%>%
  mutate(nobs = ifelse(is.na(nobs), 0, nobs),
         cumulative = cumsum(nobs))%>%
  dplyr::ungroup()

all_data <- tibble(Sector = "Voluntary",
                   Local.authority = rep(unique(nobsprive$Local.authority), each = 596),
                   time = rep(-595:0, times = 145),
                   er = 1)

nobsvol <- nobsvol %>%
  right_join(all_data, by = c("Sector", "time", "Local.authority")) %>%
  dplyr::arrange(time)%>%
  dplyr::group_by(Local.authority)%>%
  mutate(nobs = ifelse(is.na(nobs), 0, nobs),
         cumulative = cumsum(nobs))%>%
  dplyr::ungroup()

nobs <- bind_rows(nobsla, nobsvol, nobsprive)

nobs$Sector <- factor(nobs$Sector, levels = c("Private", "Local Authority", "Third Sector"))
levels(nobs$Sector) <- c("For-profit", "Local Authority", "Third Sector")

Providernobs$Sector <- factor(Providernobs$Sector, levels = c("Private", "Local Authority", "Third Sector"))

write.csv(dashboard_data, "C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/childrens_social_care_data/Final_Data/outputs/dashboard_data.csv")

