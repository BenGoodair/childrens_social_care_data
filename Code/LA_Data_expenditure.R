####packages####

if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools, dplyr, tidyverse, tidyr, stringr,  curl)


#setwd("C:/Users/bengo/OneDrive - Nexus365/Documents/Children's Care Homes Project")
options(scipen=999)


rm(list=setdiff(ls(), c("")))
####Expenditure data pre2014####

outturn0809 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/LA_Spend/outturn_0809.csv"),
                       colClasses = "character")
outturn0910 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/LA_Spend/outturn_0910.csv"),
                        colClasses = "character")
outturn1011 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/LA_Spend/outturn_1011.csv"),
                        colClasses = "character")
outturn1112 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/LA_Spend/outturn_1112.csv"),
                        colClasses = "character")
outturn1213 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/LA_Spend/outturn_1213.csv"),
                        colClasses = "character")
outturn1314 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/LA_Spend/outturn_1314.csv"),
                        colClasses = "character")
ExpenditureData <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/LA_Spend/LA_Care_Expenditure_By_Ownership_OverTime.csv"),
                        colClasses = "character")
ExpenditureData22 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Raw_Data/LA_level/LA_Spend/LA_Care_Expenditure_By_Ownership_OverTime_22.csv"),
                            colClasses = "character")

outturn0809 <- outturn0809 %>% dplyr::mutate(S52.Line.Reference. = str_replace(S52.Line.Reference., "\\s", "|")) %>% 
  tidyr::separate(S52.Line.Reference., into = c("LineNumber", "CYPServiceDescription"), sep = "\\|")%>%
  dplyr::rename(LA = LEA.Number.,
                LA.Name = LA.Name.,
                OwnProvision = PUBLIC..z.,
                PrivateProvision = PRIVATE..x.,
                Voluntary = VOLUNTARY..y.,
                TotalExpenditure = TOTAL.EXPENDITURE..k.,
                Income = INCOME..l.,
                NetCurrentExpenditure = NET.Current.Expenditure..m.)%>%
  dplyr::mutate(ReportGroup = NA, OtherPublic = NA, GovGrantsInsideAEF = NA, GovGrantsOutsideAEF = NA,  LEANetRevenueExpenditure = NA, year=2009)

outturn0910 <- outturn0910 %>%  dplyr::rename(LA = LEA,
                                              LA.Name = LEA.Name,
                                              LineNumber = Line,
                                              CYPServiceDescription = S52.Line.Reference,
                                              OwnProvision = Own.Provision..y.,
                                              PrivateProvision = Private..z.i..,
                                              Voluntary = Voluntary..z.iii..,
                                              OtherPublic = Other.Public..z.ii..,
                                              TotalExpenditure = Total.Expenditure..k.,
                                              Income = Income..l.,
                                              NetCurrentExpenditure = NET.Current.Expenditure..m.,
                                              GovGrantsInsideAEF = Govt..Grants.Inside.AEF..n., 
                                              GovGrantsOutsideAEF = Govt..Grants.Outside.AEF..o.,
                                              LEANetRevenueExpenditure = LEA.NET.Revenue.Expenditure..p.)%>%
  dplyr::mutate(ReportGroup = NA, year=2010)

outturn1011 <- outturn1011 %>%  dplyr::rename(LineNumber = Line,
                                              CYPServiceDescription = S52.Line.Reference,
                                              OwnProvision = Own.Provision..y.,
                                              PrivateProvision = Private..z.i..,
                                              Voluntary = Voluntary..z.iii..,
                                              OtherPublic = Other.Public..z.ii..,
                                              TotalExpenditure = Total.Expenditure..k.,
                                              Income = Income..l.,
                                              NetCurrentExpenditure = NET.Current.Expenditure..m.,
                                              GovGrantsInsideAEF = Govt..Grants.Inside.AEF..n., 
                                              GovGrantsOutsideAEF = Govt..Grants.Outside.AEF..o.,
                                              LEANetRevenueExpenditure = LEA.NET.Revenue.Expenditure..p.)%>%
  dplyr::mutate(ReportGroup = NA, year=2011)%>% dplyr::select(-ReturnStatus)

outturn1112 <- outturn1112 %>%  dplyr::mutate(ReportGroup = NA, year=2012)

outturn1213 <- outturn1213 %>%  dplyr::mutate(year=2013,
                                              CYPServiceDescription = str_replace(CYPServiceDescription, "^[^ ]* ", ""))

outturn1314 <- outturn1314 %>%  dplyr::mutate(year=2014, 
                                              CYPServiceDescription = str_replace(CYPServiceDescription, "^[^ ]* ", ""))


pre2014 <- rbind(outturn0809, outturn0910, outturn1011, outturn1112, outturn1213, outturn1314)

pre2014 <-  pre2014 %>%
  dplyr::rename(LA_Name = LA.Name, LA.Number = LA,
                Description = CYPServiceDescription)%>%
  dplyr::mutate(LAD19CD=NA)%>%
  dplyr::mutate(LA_Name = gsub('&','and',LA_Name),
                LA_Name = gsub('[[:punct:] ]+',' ',LA_Name),
                LA_Name = toupper(LA_Name),
                LA_Name = str_trim(LA_Name),
                ReportGroup = toupper(ReportGroup))%>%
  dplyr::rename(variable=Description,
                category=ReportGroup,
                LA_Code = LAD19CD)%>%
  dplyr::mutate(category = "Expenditure")%>%
  dplyr::select(-Income, -NetCurrentExpenditure, -GovGrantsInsideAEF, -GovGrantsOutsideAEF,
                -LEANetRevenueExpenditure, -LineNumber)%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code, year, variable, category),
                      names_to = "subcategory", values_to = "number")





#oldlalookup <- read.csv("Data/oldlalookup.csv")
#pre2014 <- merge(oldlalookup, pre2014, by="LA", all=T)


ExpenditureData$year <- str_sub(ExpenditureData$time_period, start= -2)
ExpenditureData$year <-  paste("20", ExpenditureData$year, sep="")

ExpenditureData <- ExpenditureData %>% dplyr::filter(geographic_level == "Local authority")%>%
  dplyr::select(LAD19NM, LAD19CD,year, ReportGroup, Laold, Description ,            
                OwnProvision            , PrivateProvision       ,  OtherPublic        ,    
                Voluntary               , TotalExpenditure       ,  Income             ,    
                NetCurrentExpenditure   , GovGrantsInsideAEF     ,  GovGrantsOutsideAEF,    
                LEANetRevenueExpenditure)%>%
  dplyr::mutate(OwnProvision = as.numeric(OwnProvision)*1000000           ,
                PrivateProvision  = as.numeric(PrivateProvision)*1000000     , 
                OtherPublic = as.numeric(OtherPublic)*1000000         ,   
                Voluntary = as.numeric(Voluntary)*1000000        , 
                TotalExpenditure  = as.numeric(TotalExpenditure)*1000000     ,  
                Income    = as.numeric(Income)*1000000         ,    
                NetCurrentExpenditure = as.numeric(NetCurrentExpenditure)*1000000  , 
                GovGrantsInsideAEF  = as.numeric(GovGrantsInsideAEF)*1000000   ,  
                GovGrantsOutsideAEF= as.numeric(GovGrantsOutsideAEF)*1000000,    
                LEANetRevenueExpenditure= as.numeric(LEANetRevenueExpenditure)*1000000)%>%
  dplyr::rename(LA_Name = LAD19NM,
                LA.Number = Laold)%>%
  dplyr::filter(year=="2015")
totals <- ExpenditureData %>% dplyr::filter(ReportGroup=="Total")%>%dplyr::mutate(LineNumber=NA)
nontotals  <- ExpenditureData %>% dplyr::filter(ReportGroup!="Total")%>%
  dplyr::mutate(Description = str_replace(Description, "\\s", "|")) %>% 
  tidyr::separate(Description, into = c("LineNumber", "Description"), sep = "\\|")



fulldata <- rbind(totals, nontotals)

#Try and clean text to comparable (remove punct and capitalise)


fulldata <- fulldata %>%
  dplyr::mutate(LA_Name = gsub('&','and',LA_Name),
                LA_Name = gsub('[[:punct:] ]+',' ',LA_Name),
                LA_Name = toupper(LA_Name),
                LA_Name = str_trim(LA_Name),
                ReportGroup = toupper(ReportGroup))%>%
  dplyr::rename(variable=Description,
                category=ReportGroup,
                LA_Code = LAD19CD)%>%
  dplyr::filter(category=="CHILDREN LOOKED AFTER")%>%
  dplyr::mutate(category = "Expenditure")%>%
  dplyr::select(-Income, -NetCurrentExpenditure, -GovGrantsInsideAEF, -GovGrantsOutsideAEF,
                -LEANetRevenueExpenditure, -LineNumber)%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code, year, variable, category),
                      names_to = "subcategory", values_to = "number")





ExpenditureData22$year <- str_sub(ExpenditureData22$time_period, start= -2)
ExpenditureData22$year <-  paste("20", ExpenditureData22$year, sep="")

ExpenditureData22 <- ExpenditureData22 %>% dplyr::filter(geographic_level == "Local authority")%>%
  dplyr::select(la_name, new_la_code,year, category_of_expenditure, old_la_code, description_of_expenditure ,            
                own_provision            , private_provision       ,  other_public_sector_provision        ,    
                voluntary_provision               , total_expenditure )%>%
  dplyr::rename(LA_Name = la_name,
                LA.Number = old_la_code,
                variable=description_of_expenditure,
                category=category_of_expenditure,
                LA_Code = new_la_code,
                OwnProvision = own_provision,
                PrivateProvision = private_provision,
                OtherPublic = other_public_sector_provision,
                Voluntary = voluntary_provision,
                TotalExpenditure = total_expenditure)%>%
  dplyr::mutate(LA_Name = gsub('&','and',LA_Name),
                LA_Name = gsub('[[:punct:] ]+',' ',LA_Name),
                LA_Name = toupper(LA_Name),
                LA_Name = str_trim(LA_Name),
                category = toupper(category))%>%
  dplyr::filter(category=="CHILDREN LOOKED AFTER")%>%
  dplyr::mutate(category = "Expenditure")%>%
  tidyr::pivot_longer(cols = !c(LA_Name,LA.Number, LA_Code, year, variable, category),
                      names_to = "subcategory", values_to = "number")%>%
  dplyr::mutate(variable = str_replace(variable, "\\s", "|")) %>% 
  tidyr::separate(variable, into = c("LineNumber", "variable"), sep = "\\|")%>%
  dplyr::select(-LineNumber)
  
  
  
fulldata <- rbind(fulldata, ExpenditureData22)







write.csv(fulldata, "Data/LA_Expenditure_Childrens_Services.csv")

