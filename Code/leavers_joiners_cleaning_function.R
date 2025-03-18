####
create_market_exits_entries <- function(){
  
  if (!require("pacman")) install.packages("pacman")
  
  pacman::p_load(devtools, dplyr, tidyverse, tidyr, stringr,  curl)

  
  joiners17 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_17.csv"))  
  joiners18 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_18.csv"))  
  joiners19 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_19.csv"))  
  joiners20 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_20.csv"))  
  joiners21 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_21.csv"))  
  joiners22 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_22.csv"), skip=3)  
  joiners23 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_23.csv"), skip=3)  
  
  joiners <- rbind( #joiners17 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, First.effective.date..that.the.provider.became.active.) %>% dplyr::rename(Registration.date = First.effective.date..that.the.provider.became.active.),
    #joiners18 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places,First.effective.date..that.the.provider.became.active.) %>% dplyr::rename(Registration.date = First.effective.date..that.the.provider.became.active.),
    joiners19 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Registration.date),
    joiners20 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Registration.date),
    joiners21 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Registration.date),
    joiners22 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Registration.date),
    joiners23 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, First.effective.date..that.the.provider.became.active.) %>% dplyr::rename(Registration.date = First.effective.date..that.the.provider.became.active.)
  )%>%
    dplyr::rename(Date = Registration.date)%>%
    dplyr::mutate(leave_join = "Join",
                  provider_status = NA)%>%
    dplyr::filter(Provision.type!="Adoption Support Agency",
                  Provision.type!="Further Education College with Residential Accommodation",
                  Provision.type!="Boarding School",
                  Provision.type!="Residential Family Centre",
                  Provision.type!="Residential Special School",
                  Provision.type!="Voluntary Adoption Agency",
                  Provision.type!="Residential Holiday Scheme for Disabled Children",
                  Provision.type!="Independent Fostering Agency",
                  Provision.type!="Voluntary Adoption Agency")
  
  
  Leavers <- rbind( #joiners17 %>% dplyr::filter(Leaver.status=="Leaver") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, First.effective.date..that.the.provider.became.active.) %>% dplyr::rename(Registration.date = First.effective.date..that.the.provider.became.active.),
    #joiners18 %>% dplyr::filter(Leaver.status=="Leaver") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places,First.effective.date..that.the.provider.became.active.) %>% dplyr::rename(Registration.date = First.effective.date..that.the.provider.became.active.),
    joiners19 %>% dplyr::filter(Leaver.status=="Leaver") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Date.closed)%>%
      dplyr::rename(Cancelled.or.resigned.date = Date.closed),
    joiners20 %>% dplyr::filter(Leaver.status=="Leaver") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Cancelled.or.resigned.date),
    joiners21 %>% dplyr::filter(Leaver.status=="Leaver") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Resigned.closed.date)%>%
      dplyr::rename(Cancelled.or.resigned.date=Resigned.closed.date),
    joiners22 %>% dplyr::filter(Leaver.status=="Leaver") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Cancelled.or.resigned.date),
    joiners23 %>% dplyr::filter(Leaver.status=="Leaver") %>% 
      dplyr::mutate(Cancelled.or.resigned.date = ifelse(First.effective.date..that.the.provider.became.resigned.!="",First.effective.date..that.the.provider.became.resigned.,
                                                        ifelse(First.effective.date..that.the.provider.became.resigned.==""&First.effective.date..that.the.provider.closed.=="", First.effective.date..that.the.provider.became.cancelled.,
                                                               First.effective.date..that.the.provider.closed.)))%>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places,Cancelled.or.resigned.date) 
  )%>%
    dplyr::rename(Date = Cancelled.or.resigned.date)%>%
    dplyr::mutate(leave_join = "Leave",
                  provider_status = NA)%>%
    dplyr::filter(Provision.type!="Adoption Support Agency",
                  Provision.type!="Further Education College with Residential Accommodation",
                  Provision.type!="Boarding School",
                  Provision.type!="Residential Family Centre",
                  Provision.type!="Residential Special School",
                  Provision.type!="Voluntary Adoption Agency",
                  Provision.type!="Residential Holiday Scheme for Disabled Children",
                  Provision.type!="Independent Fostering Agency",
                  Provision.type!="Voluntary Adoption Agency")
  
  
  exits <- rbind(joiners, Leavers)
  
  pre <- rbind(
    read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 3, skip=3) %>%
      dplyr::mutate(leave_join = "Join")%>%
      dplyr::rename(Date = `Registration date`),
    read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 4, skip=3) %>%
      dplyr::mutate(leave_join = "Leave")%>%
      dplyr::rename(Date = `Date closed`),
    read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 5, skip=3) %>%
      dplyr::mutate(leave_join = "Join")%>%
      dplyr::rename(Date = `Registration date`),
    read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 6, skip=3) %>%
      dplyr::mutate(leave_join = "Leave")%>%
      dplyr::rename(Date = `Date closed`),
    read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 7, skip=3) %>%
      dplyr::mutate(leave_join = "Join")%>%
      dplyr::rename(Date = `Registration date`),
    read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 8, skip=3) %>%
      dplyr::mutate(leave_join = "Leave")%>%
      dplyr::rename(Date = `Date closed`),
    read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 9, skip=3) %>%
      dplyr::mutate(leave_join = "Join")%>%
      dplyr::rename(Date = `Registration date`),
    read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/Care Markets/Data/Raw/Children_s_homes_registrations_transparency_dataset (1).xls", sheet = 10, skip=3) %>%
      dplyr::mutate(leave_join = "Leave")%>%
      dplyr::rename(Date = `Date closed`)
  )
  
  all <- rbind(pre%>%
                 dplyr::rename(Provision.type = `Provider type`,
                               Local.authority=`Local authority`)%>%
                 dplyr::mutate(Local.authority = Local.authority %>%
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
                                 gsub("COUNTY DURHAM", "DURHAM", .)%>%
                                 gsub("AND DARWEN", "WITH DARWEN", .)%>%
                                 gsub("NE SOM", "NORTH EAST SOM", .)%>%
                                 gsub("N E SOM", "NORTH EAST SOM", .)%>%
                                 str_trim())%>% dplyr::select(URN,Local.authority,Sector,Places,Date,leave_join), 
               exits%>% dplyr::select(URN,Local.authority,Sector,Places,Date, leave_join)%>%
                 dplyr::mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%  # Convert Date to Date format
                 dplyr::filter(Date >= as.Date("2018-04-01"))  # Filter for dates after 1st April 2018
  )%>%  
    dplyr::distinct(URN,leave_join, .keep_all = T)%>%
    tidyr::pivot_wider(id_cols = c("Local.authority", "Sector", "URN", "Places"), names_from = "leave_join", values_from = as.character("Date"))%>%
    dplyr::mutate(Join = substr(Join, 1, 10),
                  Leave = substr(Leave, 1, 10),
                  Sector= ifelse(Sector=="Health Authority", "Local Authority", Sector))
  
  
  leaves <- all %>%
    dplyr::select(URN,Leave)
  
  all <- all %>%
    dplyr::select(-Leave)%>%
    full_join(., leaves, by= "URN")  %>%
    group_by(URN) %>%                      # Group by URN
    fill(Leave, .direction = "downup") %>% 
    fill(Join, .direction = "downup")%>%
    ungroup()%>%
    dplyr::distinct(URN, .keep_all = T)%>%
    dplyr::mutate(Local.authority = Local.authority %>%
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
                    gsub("COUNTY DURHAM", "DURHAM", .)%>%
                    gsub("AND DARWEN", "WITH DARWEN", .)%>%
                    gsub("NE SOM", "NORTH EAST SOM", .)%>%
                    gsub("N E SOM", "NORTH EAST SOM", .)%>%
                    str_trim())
  
  
  
  
  
  
  joiners17 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_17.csv"))  
  joiners18 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_18.csv"))  
  joiners19 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_19.csv"))  
  joiners20 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_20.csv"))  
  joiners21 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_21.csv"))  
  joiners22 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_22.csv"), skip=3)  
  joiners23 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_23.csv"), skip=3)  
  joiners24 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/joiners_leavers_24.csv"), skip=3)  
  
  joiners <- rbind( joiners17 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, First.effective.date..that.the.provider.became.active.) %>% dplyr::rename(Registration.date = First.effective.date..that.the.provider.became.active.),
                     joiners18 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places,First.effective.date..that.the.provider.became.active.) %>% dplyr::rename(Registration.date = First.effective.date..that.the.provider.became.active.),
                     joiners19 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Registration.date),
                     joiners20 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Registration.date),
                     joiners21 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Registration.date),
                     joiners22 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, Registration.date),
                     joiners23 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, First.effective.date..that.the.provider.became.active.) %>% dplyr::rename(Registration.date = First.effective.date..that.the.provider.became.active.),
                     joiners24 %>% dplyr::filter(Joiner.status=="Joiner") %>% dplyr::select(URN, Provision.type, Local.authority, Sector, Places, First.effective.date..that.the.provider.became.active.) %>% dplyr::rename(Registration.date = First.effective.date..that.the.provider.became.active.)
                    
  )%>%
    dplyr::rename(Date = Registration.date)%>%
    dplyr::mutate(leave_join = "Join",
                  provider_status = NA)%>%
    dplyr::filter(Provision.type!="Adoption Support Agency",
                  Provision.type!="Further Education College with Residential Accommodation",
                  Provision.type!="Boarding School",
                  Provision.type!="Residential Family Centre",
                  Provision.type!="Residential Special School",
                  Provision.type!="Voluntary Adoption Agency",
                  Provision.type!="Residential Holiday Scheme for Disabled Children",
                  Provision.type!="Independent Fostering Agency",
                  Provision.type!="Voluntary Adoption Agency")
  
  
  
  # #pre2016joiners
  # homesat14 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/providers_at_sept_14.csv"))
  # homesat15 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/providers_at_15.csv"))%>% 
  # dplyr::select(URN, Provision.type, Local.authority, Sector) 
  # homesat16 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/Provider_level/providers_at_16.csv"), skip=1) %>%
  #   dplyr::select(URN, Provision.type, Local.authority, Sector) 
  # 
  # yes1 <- merge(homesat15, homesat14, by="URN", all.x=T) %>%
  #   dplyr::filter(is.na(GOR))%>%
  #   dplyr::mutate(Registration.date = "30/04/2015")
  #   
  # 
  # yes2 <- merge(homesat16, homesat15, by="URN", all.x=T) %>%
  #   dplyr::filter(is.na(Local.authority.y))%>%
  #   dplyr::mutate(Registration.date = "30/04/2016")
  # 
    
  
  
  source("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Code/provider_cleaning_function_nonactive.R")
  ProviderData <- create_provider_data()
  
  leavers <- merge(leavers, ProviderData, by="URN", all.x=T) %>%
    dplyr::select(URN, Provision.type, Local.authority, Sector, Places, provider_status ,close_date)%>%
    dplyr::filter(!is.na(Provision.type))%>%
    dplyr::rename(Date = close_date)%>%
    dplyr::mutate(leave_join = "Leave")
  
  
  enter_exit <- unique(rbind(leavers, joiners%>%dplyr::mutate(Date=as.Date(Date, format =  "%d/%m/%Y"))))
  
  # imd <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Economic_Political_Contextual/imd19.csv"))  %>%
  #   dplyr::rename(Local.authority = Upper.Tier.Local.Authority.District.name..2019.)%>%
  #   dplyr::mutate(Local.authority = Local.authority %>%
  #                   gsub('&', 'and', .) %>%
  #                   gsub('[[:punct:] ]+', ' ', .) %>%
  #                   gsub('[0-9]', '', .)%>%
  #                   toupper() %>%
  #                   gsub("CITY OF", "",.)%>%
  #                   gsub("UA", "",.)%>%
  #                   gsub("COUNTY OF", "",.)%>%
  #                   gsub("ROYAL BOROUGH OF", "",.)%>%
  #                   gsub("LEICESTER CITY", "LEICESTER",.)%>%
  #                   gsub("UA", "",.)%>%
  #                   gsub("DARWIN", "DARWEN", .)%>%
  #                   gsub("COUNTY DURHAM", "DURHAM", .)%>%
  #                   gsub("AND DARWEN", "WITH DARWEN", .)%>%
  #                   gsub("NE SOM", "NORTH EAST SOM", .)%>%
  #                   gsub("N E SOM", "NORTH EAST SOM", .)%>%
  #                   str_trim())%>%
  #   dplyr::select(Local.authority,IMD...Rank.of.average.score, IMD.2019...Extent )%>%
  #   dplyr::mutate(imd_decile = ntile(IMD...Rank.of.average.score, 10),
  #                 imd_extent_decile = ntile(IMD.2019...Extent, 10))
  # 
  # house_price <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/Economic_Political_Contextual/Average-prices-2024-02.csv"))  %>%
  #   dplyr::rename(Local.authority = Region_Name,
  #                 Average_house_price = Average_Price)%>%
  #   dplyr::filter(Date=="01/12/2023")%>%
  #   dplyr::select(Local.authority,Average_house_price )%>%
  #   dplyr::mutate(Local.authority = Local.authority %>%
  #                   gsub('&', 'and', .) %>%
  #                   gsub('[[:punct:] ]+', ' ', .) %>%
  #                   gsub('[0-9]', '', .)%>%
  #                   toupper() %>%
  #                   gsub("CITY OF", "",.)%>%
  #                   gsub("UA", "",.)%>%
  #                   gsub("COUNTY OF", "",.)%>%
  #                   gsub("ROYAL BOROUGH OF", "",.)%>%
  #                   gsub("LEICESTER CITY", "LEICESTER",.)%>%
  #                   gsub("UA", "",.)%>%
  #                   gsub("DARWIN", "DARWEN", .)%>%
  #                   gsub("COUNTY DURHAM", "DURHAM", .)%>%
  #                   gsub("AND DARWEN", "WITH DARWEN", .)%>%
  #                   gsub("NE SOM", "NORTH EAST SOM", .)%>%
  #                   gsub("N E SOM", "NORTH EAST SOM", .)%>%
  #                   str_trim())
  # 
  # enter_exit <- enter_exit %>%
  #   dplyr::mutate(Local.authority = Local.authority %>%
  #                   gsub('&', 'and', .) %>%
  #                   gsub('[[:punct:] ]+', ' ', .) %>%
  #                   gsub('[0-9]', '', .)%>%
  #                   toupper() %>%
  #                   gsub("CITY OF", "",.)%>%
  #                   gsub("UA", "",.)%>%
  #                   gsub("COUNTY OF", "",.)%>%
  #                   gsub("ROYAL BOROUGH OF", "",.)%>%
  #                   gsub("LEICESTER CITY", "LEICESTER",.)%>%
  #                   gsub("UA", "",.)%>%
  #                   gsub("DARWIN", "DARWEN", .)%>%
  #                   gsub("AND DARWEN", "WITH DARWEN", .)%>%
  #                   gsub("NE SOM", "NORTH EAST SOM", .)%>%
  #                   gsub("N E SOM", "NORTH EAST SOM", .)%>%
  #                   str_trim())%>%
  #   dplyr::full_join(., imd, by="Local.authority")%>%
  #   dplyr::mutate(imd_decile = ifelse(Local.authority %in% c("BOURNEMOUTH", "POOLE", "CHRISTCHURCH"), 7,
  #                                     ifelse(Local.authority %in% c("WEST NORTHAMPTONSHIRE", "NORTH NORTHAMPTONSHIRE"), 7,
  #                                            imd_decile)),
  #                 IMD...Rank.of.average.score = ifelse(Local.authority %in% c("BOURNEMOUTH", "POOLE", "CHRISTCHURCH"), 104,
  #                                                      ifelse(Local.authority %in% c("WEST NORTHAMPTONSHIRE", "NORTH NORTHAMPTONSHIRE"), 99,
  #                                                             IMD...Rank.of.average.score)),
  #                 imd_extent_decile = ifelse(Local.authority %in% c("BOURNEMOUTH", "POOLE", "CHRISTCHURCH"), 4,
  #                                                      ifelse(Local.authority %in% c("WEST NORTHAMPTONSHIRE", "NORTH NORTHAMPTONSHIRE"), 5,
  #                                                             imd_extent_decile)),
  #                 IMD.2019...Extent = ifelse(Local.authority %in% c("BOURNEMOUTH", "POOLE", "CHRISTCHURCH"), 0.1126,
  #                                                      ifelse(Local.authority %in% c("WEST NORTHAMPTONSHIRE", "NORTH NORTHAMPTONSHIRE"), 0.1511,
  #                                                             IMD.2019...Extent)))%>%
  #   dplyr::left_join(., house_price, by="Local.authority")
  #   
  # 
  # 
  
  
  
# plz <-enter_exit%>% 
#   dplyr::mutate(yes = 1,
#                 Places = as.numeric(Places),
#   date = as.Date(Date, format =  "%d/%m/%Y"),
#   year = format(date,"%Y"))%>%
#                               dplyr::group_by(Sector, year, leave_join)%>%
#                               dplyr::summarise(yes = sum(yes, na.rm=T),
#                                         Places = sum(Places, na.rm=T))
                    

    
}
  
    