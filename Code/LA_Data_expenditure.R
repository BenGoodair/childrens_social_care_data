####packages####

if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools, dplyr, tidyverse, tidyr, stringr,  curl)


#setwd("C:/Users/bengo/OneDrive - Nexus365/Documents/Children's Care Homes Project")
options(scipen=999)


rm(list=setdiff(ls(), c("")))
####Expenditure data pre2014####

outturn0809 <- read.csv("Data/outturn_0809.csv")
outturn0910 <- read.csv("Data/outturn_0910.csv")
outturn1011 <- read.csv("Data/outturn_1011.csv")
outturn1112 <- read.csv("Data/outturn_1112.csv")
outturn1213 <- read.csv("Data/outturn_1213.csv")
outturn1314 <- read.csv("Data/outturn_1314.csv")
ExpenditureData <- read.csv("Data/LA_Care_Expenditure_By_Ownership_OverTime.csv")

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
outturn1213 <- outturn1213 %>%  dplyr::mutate(year=2013, CYPServiceDescription = sub(".*? ", "", outturn1213$CYPServiceDescription)
)
outturn1314 <- outturn1314 %>%  dplyr::mutate(year=2014, CYPServiceDescription = sub(".*? ", "", outturn1314$CYPServiceDescription)
)

pre2014 <- rbind(outturn0809, outturn0910, outturn1011, outturn1112, outturn1213, outturn1314)

oldlalookup <- read.csv("Data/oldlalookup.csv")
pre2014 <- merge(oldlalookup, pre2014, by="LA", all=T)

pre2014 <- pre2014 %>%dplyr::rename(LAD19NM = LA.Name, Laold = LA,
                                    Description = CYPServiceDescription )

ExpenditureData$year <- str_sub(ExpenditureData$?..time_period, start= -2)
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
                LEANetRevenueExpenditure= as.numeric(LEANetRevenueExpenditure)*1000000)
totals <- ExpenditureData %>% dplyr::filter(ReportGroup=="Total")%>%dplyr::mutate(LineNumber=NA)
nontotals  <- ExpenditureData %>% dplyr::filter(ReportGroup!="Total")%>%
  dplyr::mutate(Description = str_replace(Description, "\\s", "|")) %>% 
  tidyr::separate(Description, into = c("LineNumber", "Description"), sep = "\\|")



fulldata <- rbind(totals, nontotals, pre2014)


#Try and clean text to comparable (remove punct and capitalise)
fulldata$LAD19NM <-  gsub('&','and',fulldata$LAD19NM)

fulldata$LAD19NM <-  gsub('[[:punct:] ]+',' ',fulldata$LAD19NM)


fulldata$LAD19NM <-  toupper(fulldata$LAD19NM)

fulldata$LAD19NM <-  str_trim(fulldata$LAD19NM)

write.csv(fulldata, "Data/LA_Expenditure_Childrens_Services.csv")

