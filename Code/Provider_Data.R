####Load Packages####

library(dplyr)
library(tidyverse)
library(stringr)
library(rebus)
library(ggiraph)
library(ggiraphExtra)
library(tidyverse)
library(plyr)
library(ggpubr)
library(gridExtra)
library(lubridate)
library(curl)

options(scipen=999)


rm(list=setdiff(ls(), c("")))




####Read in Data from Github Repo####

ProviderData2022 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Identifying_organisational_childrens_homes_ratings/main/Raw_data/All_SocCare_inspec_2122_fin.csv"))
ProviderData2020 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Identifying_organisational_childrens_homes_ratings/main/Raw_data/All_SocCare_inspec_1920.csv"))
ProviderData2019 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Identifying_organisational_childrens_homes_ratings/main/Raw_data/All_SocCare_inspec_1819.csv"))
ProviderData2018 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Identifying_organisational_childrens_homes_ratings/main/Raw_data/All_SocCare_inspec_1718.csv"))
ProviderData2017 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Identifying_organisational_childrens_homes_ratings/main/Raw_data/All_SocCare_inspec_1617.csv"))
ProviderData2016 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Identifying_organisational_childrens_homes_ratings/main/Raw_data/All_SocCare_inspec_1516.csv"))
ProviderData2015 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Identifying_organisational_childrens_homes_ratings/main/Raw_data/All_SocCare_inspec_1415.csv"))
ProviderData2014excch <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Identifying_organisational_childrens_homes_ratings/main/Raw_data/All_SocCare_exccarehomes_inspec_1314.csv"))
ProviderData2014c1 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Identifying_organisational_childrens_homes_ratings/main/Raw_data/Carehomes_inspec_14_1.csv"))
ProviderData2014c2 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/Identifying_organisational_childrens_homes_ratings/main/Raw_data/Carehomes_inspec_14_2.csv"))

ProviderData2014ch <- rbind(ProviderData2014c1, ProviderData2014c2)





####Clean data to merge annual datasets at site level####

#Column titles differ in each year's raw data and some columns only exist in some years' of data
#This code makes column titles and numbers the same

#2014 excluding care homes
ProviderData2014excch$Registration.date <- NA
ProviderData2014excch$Sector <- NA

ProviderData2014excch$Web.link <- NA
ProviderData2014excch$Linked.Education.URN <- NA
names(ProviderData2014excch)[names(ProviderData2014excch)=="Provider.URN"] <- "URN"
names(ProviderData2014excch)[names(ProviderData2014excch)=="Provider.name"] <- "Name"
names(ProviderData2014excch)[names(ProviderData2014excch)=="Provider.address.1"] <- "Address.1"
names(ProviderData2014excch)[names(ProviderData2014excch)=="Provider.address.2"] <- "Address.2"
names(ProviderData2014excch)[names(ProviderData2014excch)=="Provider.town"] <- "Town"
ProviderData2014excch$County <- NA
names(ProviderData2014excch)[names(ProviderData2014excch)=="Provider.postcode"] <- "Postcode"
ProviderData2014excch$Ofsted.region <- NA
ProviderData2014excch$Parliamentary.constituency <- NA
ProviderData2014excch$Places <- NA
names(ProviderData2014excch)[names(ProviderData2014excch)=="Organisation"] <- "Organisation.which.owns.the.provider"
names(ProviderData2014excch)[names(ProviderData2014excch)=="Event.Type.Group"] <- "Event.type"
ProviderData2014excch$Government.Office.Region <- NA

names(ProviderData2014excch)[names(ProviderData2014excch)=="Event.date"] <- "Inspection.date"
ProviderData2014excch$Publication.date <- NA
ProviderData2014excch$Outcomes.in.education.and.related.learning.activities <- NA
ProviderData2014excch$Health.services <- NA
names(ProviderData2014excch)[names(ProviderData2014excch)=="Leadership.and.management"] <- "The.effectiveness.of.leaders.and.managers"
ProviderData2014excch$Short.Breaks.Only <- NA

names(ProviderData2014excch)[names(ProviderData2014excch)=="Overall.effectiveness"] <- "Overall.experiences.and.progress.of.children.and.young.people"
names(ProviderData2014excch)[names(ProviderData2014excch)=="Safeguarding.children.and.young.people"] <- "How.well.children.and.young.people.are.helped.and.protected"
ProviderData2014excch <- ProviderData2014excch[-c(18:22, 24:35)]

names(ProviderData2014excch)[names(ProviderData2014excch)=="Quality.of.service"] <- "Quality.of.care"
names(ProviderData2014excch)[names(ProviderData2014excch)=="Experiences.and.progress.of..and.outcomes.for..children.and.young.people"] <- "Outcomes.for.children.and.young.people"


#2014

ProviderData2014ch$Web.link <- NA
names(ProviderData2014ch)[names(ProviderData2014ch)=="Provider.URN"] <- "URN"
ProviderData2014ch$Linked.Education.URN <- NA
names(ProviderData2014ch)[names(ProviderData2014ch)=="Provider.type"] <- "Provision.type"
ProviderData2014ch$Registration.date <- NA
names(ProviderData2014ch)[names(ProviderData2014ch)=="Provider.status"] <- "Registration.status"
names(ProviderData2014ch)[names(ProviderData2014ch)=="Provider.name"] <- "Name"
names(ProviderData2014ch)[names(ProviderData2014ch)=="Provider.address.1"] <- "Address.1"
names(ProviderData2014ch)[names(ProviderData2014ch)=="Provider.address.2"] <- "Address.2"
names(ProviderData2014ch)[names(ProviderData2014ch)=="Provider.town"] <- "Town"
ProviderData2014ch$County <- NA
names(ProviderData2014ch)[names(ProviderData2014ch)=="Provider.postcode"] <- "Postcode"
ProviderData2014ch$Ofsted.region <- NA
names(ProviderData2014ch)[names(ProviderData2014ch)=="Local.Authority"] <- "Local.authority"
ProviderData2014ch$Parliamentary.constituency <- NA
ProviderData2014ch$Places <- NA
ProviderData2014ch$Organisation.which.owns.the.provider <- NA
names(ProviderData2014ch)[names(ProviderData2014ch)=="Event.type.group"] <- "Event.type"
names(ProviderData2014ch)[names(ProviderData2014ch)=="government_office_region_name"] <- "Government.Office.Region"

names(ProviderData2014ch)[names(ProviderData2014ch)=="Event.date"] <- "Inspection.date"
ProviderData2014ch$Publication.date <- NA
ProviderData2014ch$Outcomes.in.education.and.related.learning.activities <- NA
ProviderData2014ch$Health.services <- NA
names(ProviderData2014ch)[names(ProviderData2014ch)=="Leadership.and.management"] <- "The.effectiveness.of.leaders.and.managers"
ProviderData2014ch$Short.Breaks.Only <- NA

names(ProviderData2014ch)[names(ProviderData2014ch)=="Overall.effectiveness"] <- "Overall.experiences.and.progress.of.children.and.young.people"
names(ProviderData2014ch)[names(ProviderData2014ch)=="Safeguarding.children.and.young.people"] <- "How.well.children.and.young.people.are.helped.and.protected"

ProviderData2020$Outcomes.for.children.and.young.people <- NA
ProviderData2020$Quality.of.care <- NA

#setdiff(ProviderData2020, ProviderData2014ch)

#2015
names(ProviderData2015)[names(ProviderData2015)=="Linked.ED.URN"] <- "Linked.Education.URN"
names(ProviderData2015)[names(ProviderData2015)=="Reg.Status"] <- "Registration.status"
ProviderData2015$Registration.date <- NA
names(ProviderData2015)[names(ProviderData2015)=="Setting.Name"] <- "Name"
names(ProviderData2015)[names(ProviderData2015)=="Inspection.type"] <- "Event.type"
names(ProviderData2015)[names(ProviderData2015)=="Short.breaks.only"] <- "Short.Breaks.Only"

ProviderData2015$Ofsted.region <- NA
ProviderData2015$Places <- NA
ProviderData2015$Organisation.which.owns.the.provider <- NA
ProviderData2015$Publication.date <- NA


names(ProviderData2015)[names(ProviderData2015)=="Region"] <- "Government.Office.Region"
names(ProviderData2015)[names(ProviderData2015)=="Inspection.Type"] <- "Event.type"
names(ProviderData2015)[names(ProviderData2015)=="Inspection.Date"] <- "Inspection.date"
names(ProviderData2015)[names(ProviderData2015)=="Inspection.ID"] <- "Event.number"
ProviderData2014$Short.Breaks.Only <- NA
ProviderData2020$Short.Breaks.Only <- NA
names(ProviderData2015)[names(ProviderData2015)=="Leadership.and.management"] <- "The.effectiveness.of.leaders.and.managers"
ProviderData2015$Health.services <- NA
ProviderData2015$Parliamentary.constituency <- NA

names(ProviderData2015)[names(ProviderData2015)=="Overall.effectiveness"] <- "Overall.experiences.and.progress.of.children.and.young.people"
names(ProviderData2015)[names(ProviderData2015)=="X.U.FEFF.Web.link"] <- "Web.link"
names(ProviderData2015)[names(ProviderData2015)=="The.safety.of.children.and.young.people"] <- "How.well.children.and.young.people.are.helped.and.protected"


#setdiff(ProviderData2020, ProviderData2015)

#2016
names(ProviderData2016)[names(ProviderData2016)=="Linked.education.URN"] <- "Linked.Education.URN"
names(ProviderData2016)[names(ProviderData2016)=="Registration.Status"] <- "Registration.status"

ProviderData2016$Registration.date <- NA
ProviderData2016$Ofsted.region <- NA
ProviderData2016$Parliamentary.constituency <- NA
ProviderData2016$Organisation.which.owns.the.provider <- NA
ProviderData2016$Publication.date <- NA
ProviderData2016$Short.Breaks.Only <- NA
names(ProviderData2016)[names(ProviderData2016)=="Leadership.and.management"] <- "The.effectiveness.of.leaders.and.managers"
ProviderData2016$Outcomes.in.education.and.related.learning.activities <- NA
ProviderData2016$Health.services <- NA

names(ProviderData2016)[names(ProviderData2016)=="Overall.effectiveness"] <- "Overall.experiences.and.progress.of.children.and.young.people"
names(ProviderData2016)[names(ProviderData2016)=="Safeguarding.children.and.young.people"] <- "How.well.children.and.young.people.are.helped.and.protected"


#2017
names(ProviderData2017)[names(ProviderData2017)=="Short.breaks.only"] <- "Short.Breaks.Only"
names(ProviderData2017)[names(ProviderData2017)=="Ofsted.Region"] <- "Ofsted.region"

ProviderData2017$Name <- NA
ProviderData2017$Parliamentary.constituency <- NA
ProviderData2017$Organisation.which.owns.the.provider <- NA
ProviderData2017$Publication.date <- NA
ProviderData2017$Outcomes.in.education.and.related.learning.activities <- NA


ProviderData2017$Health.services <- NA
ProviderData2017$Registration.date <- NA
names(ProviderData2017)[names(ProviderData2017)=="Organisation.which.owns.the.children.s.home"] <- "Organisation.which.owns.the.provider"
names(ProviderData2017)[names(ProviderData2017)=="Registration.Status"] <- "Registration.status"

names(ProviderData2017)[names(ProviderData2017)=="Overall.effectiveness"] <- "Overall.experiences.and.progress.of.children.and.young.people"
names(ProviderData2017)[names(ProviderData2017)=="Safeguarding.children.and.young.people"] <- "How.well.children.and.young.people.are.helped.and.protected"
names(ProviderData2017)[names(ProviderData2017)=="Leadership.and.management"] <- "The.effectiveness.of.leaders.and.managers"

#2018
names(ProviderData2018)[names(ProviderData2018)=="Registration.Status"] <- "Registration.status"
ProviderData2018$Outcomes.in.education.and.related.learning.activities <- NA

names(ProviderData2018)[names(ProviderData2018)=="Overall.effectiveness"] <- "Overall.experiences.and.progress.of.children.and.young.people"
names(ProviderData2018)[names(ProviderData2018)=="Safeguarding.children.and.young.people"] <- "How.well.children.and.young.people.are.helped.and.protected"
names(ProviderData2018)[names(ProviderData2018)=="Leadership.and.management"] <- "The.effectiveness.of.leaders.and.managers"
names(ProviderData2018)[names(ProviderData2018)=="Ofsted.Region"] <- "Ofsted.region"


ProviderData2018$Short.Breaks.Only <- NA
ProviderData2018$Health.services <- NA
ProviderData2018$Registration.date <- NA
names(ProviderData2018)[names(ProviderData2018)=="Organisation"] <- "Organisation.which.owns.the.provider"


#2019

names(ProviderData2019)[names(ProviderData2019)=="X.U.FEFF.Web.link"] <- "Web.link"

ProviderData2019$Short.Breaks.Only <- NA
ProviderData2019$Outcomes.for.children.and.young.people <- NA
ProviderData2019$Health.services <- NA
ProviderData2019$Quality.of.care <- NA

#2022



ProviderData2022$Outcomes.for.children.and.young.people <- NA
ProviderData2022$Health.services <- NA
ProviderData2022$Quality.of.care <- NA



names(ProviderData2022)[names(ProviderData2022)=="Short.break.only.children.s.home"] <- "Short.Breaks.Only"
names(ProviderData2022)[names(ProviderData2022)=="Inspection.event.number"] <- "Event.number"
names(ProviderData2022)[names(ProviderData2022)=="Inspection.event.type"] <- "Event.type"
names(ProviderData2022)[names(ProviderData2022)=="Inspection.publication.date"] <- "Publication.date"
names(ProviderData2022)[names(ProviderData2022)=="Inspection.overall.experiences.and.progress.of.children.and.young.people"] <- "Overall.experiences.and.progress.of.children.and.young.people"
names(ProviderData2022)[names(ProviderData2022)=="Inspection.outcomes.in.education.and.related.learning.activities"] <- "Outcomes.in.education.and.related.learning.activities"
names(ProviderData2022)[names(ProviderData2022)=="Inspection.how.well.children.and.young.people.are.helped.and.protected"] <- "How.well.children.and.young.people.are.helped.and.protected"
names(ProviderData2022)[names(ProviderData2022)=="Inspection.the.effectiveness.of.leaders.and.managers"] <- "The.effectiveness.of.leaders.and.managers"

#check for any different column names
setdiff(names(ProviderData2022), names(ProviderData2020))



#The data has spelling differences across the years, here is the first attempt to normalise them
##Spelling differences corrected##


ProviderData2018$Organisation.which.owns.the.provider[ProviderData2018$Organisation.which.owns.the.provider== "Acorn Homes (uk) Ltd"] <- "Acorn Homes (UK) Limited"
ProviderData2018$Organisation.which.owns.the.provider[ProviderData2018$Organisation.which.owns.the.provider== "Broadwood Residential Ltd"] <- "Broadwood Residential Limited"
ProviderData2018$Organisation.which.owns.the.provider[ProviderData2018$Organisation.which.owns.the.provider== "Headway Adolescent Resources Ltd"] <- "Headway Adolescent Resources Limited"
ProviderData2018$Organisation.which.owns.the.provider[ProviderData2018$Organisation.which.owns.the.provider== "Hillcrest Children's Services Ltd"] <- "Hillcrest Childrens Services Limited"
ProviderData2018$Organisation.which.owns.the.provider[ProviderData2018$Organisation.which.owns.the.provider== "Kisimul Group Ltd"] <- "Kisimul Group Limited"
ProviderData2018$Organisation.which.owns.the.provider[ProviderData2018$Organisation.which.owns.the.provider== "Priory Education Services Limited 06244880"] <- "Priory Education Services Limited"
ProviderData2018$Organisation.which.owns.the.provider[ProviderData2018$Organisation.which.owns.the.provider== "The Adolescent And Children's Trust (tact)"] <- "The Adolescent and Children's Trust"
ProviderData2018$Organisation.which.owns.the.provider[ProviderData2018$Organisation.which.owns.the.provider== "The SENAD Group Limited"] <- "The Senad Group Limited"
ProviderData2018$Organisation.which.owns.the.provider[ProviderData2018$Organisation.which.owns.the.provider== "Time-Out Children's Homes Limited"] <- "Time-Out Childrens Homes Limited"
ProviderData2018$Organisation.which.owns.the.provider[ProviderData2018$Organisation.which.owns.the.provider== "Cambian Childcare Ltd"] <- "Cambian Childcare Limited"

ProviderData2019$Organisation.which.owns.the.provider[ProviderData2019$Organisation.which.owns.the.provider== "Acorn Children's Homes (Branston) Ltd"] <- "Acorn Children's Home (Branston) Ltd"
ProviderData2019$Organisation.which.owns.the.provider[ProviderData2019$Organisation.which.owns.the.provider== "Action For Children"] <- "Action for Children"
ProviderData2019$Organisation.which.owns.the.provider[ProviderData2019$Organisation.which.owns.the.provider== "Inspire Social Care Services Ltd"] <- "Inspire Social Care Services Limited"
ProviderData2019$Organisation.which.owns.the.provider[ProviderData2019$Organisation.which.owns.the.provider== "Pathway Care Solutions Ltd 04004053"] <- "Pathway Care Solutions Ltd"
ProviderData2019$Organisation.which.owns.the.provider[ProviderData2019$Organisation.which.owns.the.provider== "The Partnership Of Care Today Children's Services"] <- "The Partnership of Care Today Children's Services"
ProviderData2019$Organisation.which.owns.the.provider[ProviderData2019$Organisation.which.owns.the.provider== "The SENAD Group Limited"] <- "The Senad Group Limited"
ProviderData2019$Organisation.which.owns.the.provider[ProviderData2019$Organisation.which.owns.the.provider== "Cambian Childcare Ltd"] <- "Cambian Childcare Limited"

ProviderData2020$Organisation.which.owns.the.provider[ProviderData2020$Organisation.which.owns.the.provider== "Pathway Care Solutions Ltd 04004053"] <- "Pathway Care Solutions Ltd"
ProviderData2020$Organisation.which.owns.the.provider[ProviderData2020$Organisation.which.owns.the.provider== "Social Care Services Ltd"] <- "Social Care Services Limited"
ProviderData2020$Organisation.which.owns.the.provider[ProviderData2020$Organisation.which.owns.the.provider== "The SENAD Group Limited"] <- "The Senad Group Limited"
ProviderData2020$Organisation.which.owns.the.provider[ProviderData2020$Organisation.which.owns.the.provider== "Cambian Childcare Ltd"] <- "Cambian Childcare Limited"

ProviderData2022$Organisation.which.owns.the.provider[ProviderData2022$Organisation.which.owns.the.provider== "Pathway Care Solutions Ltd 04004053"] <- "Pathway Care Solutions Ltd"
ProviderData2022$Organisation.which.owns.the.provider[ProviderData2022$Organisation.which.owns.the.provider== "Social Care Services Ltd"] <- "Social Care Services Limited"
ProviderData2022$Organisation.which.owns.the.provider[ProviderData2022$Organisation.which.owns.the.provider== "The SENAD Group Limited"] <- "The Senad Group Limited"
ProviderData2022$Organisation.which.owns.the.provider[ProviderData2022$Organisation.which.owns.the.provider== "Cambian Childcare Ltd"] <- "Cambian Childcare Limited"
ProviderData2022 <- ProviderData2022 %>% dplyr::select(-Inspection.health.services)

#bind together the datasets and remove all nonsense dataframes
ProviderData <- rbind(ProviderData2020,ProviderData2022, ProviderData2014ch,ProviderData2014excch, ProviderData2015, ProviderData2016, ProviderData2017, ProviderData2018, ProviderData2019)
rm(list=setdiff(ls(), c("ProviderData")))

#More correcting of differential spelling across years#
ProviderData$Overall.experiences.and.progress.of.children.and.young.people[ProviderData$Overall.experiences.and.progress.of.children.and.young.people == "Requires improvement"] <- "Requires improvement to be good"
ProviderData$Outcomes.in.education.and.related.learning.activities[ProviderData$Outcomes.in.education.and.related.learning.activities == "Requires improvement"] <- "Requires improvement to be good"
ProviderData$How.well.children.and.young.people.are.helped.and.protected[ProviderData$How.well.children.and.young.people.are.helped.and.protected == "Requires improvement"] <- "Requires improvement to be good"
ProviderData$The.effectiveness.of.leaders.and.managers[ProviderData$The.effectiveness.of.leaders.and.managers == "Requires improvement"] <- "Requires improvement to be good"

ProviderData$Overall.experiences.and.progress.of.children.and.young.people[ProviderData$Overall.experiences.and.progress.of.children.and.young.people == "Adequate"] <- "Requires improvement to be good"
ProviderData$Outcomes.in.education.and.related.learning.activities[ProviderData$Outcomes.in.education.and.related.learning.activities == "Adequate"] <- "Requires improvement to be good"
ProviderData$How.well.children.and.young.people.are.helped.and.protected[ProviderData$How.well.children.and.young.people.are.helped.and.protected == "Adequate"] <- "Requires improvement to be good"
ProviderData$The.effectiveness.of.leaders.and.managers[ProviderData$The.effectiveness.of.leaders.and.managers == "Adequate"] <- "Requires improvement to be good"


ProviderData$Sector[ProviderData$Sector == "Voluntary "] <- "Voluntary"
ProviderData$Sector[ProviderData$Sector == "Local authority"] <- "Local Authority"
ProviderData$Sector[ProviderData$Sector == "Health authority"] <- "Health Authority"

ProviderData$Provision.type[ProviderData$Provision.type == "Independent Fostering Provider"] <- "Independent Fostering Agency"
ProviderData$Provision.type[ProviderData$Provision.type == "Children's Home"] <- "Children's home"


#Identify inspections that are included in several annual data releases
n_occur <- data.frame(table(ProviderData$Event.number))

yep <- n_occur[n_occur$Freq > 1,]

#Keep only inspections when home was active
ProviderData <- ProviderData[which(ProviderData$Registration.status=="Active"),]


#Remove repeated inspections
ProviderData <- ProviderData[!duplicated(ProviderData$Event.number), ]

#Keep only full inspections
ProviderData <- ProviderData[which(ProviderData$Event.type=="Full inspection"),]

#Organise rating variable in orders we want
ProviderData$Overall.experiences.and.progress.of.children.and.young.people <- factor(ProviderData$Overall.experiences.and.progress.of.children.and.young.people, levels = c("Inadequate","Requires improvement to be good", "Good","Outstanding" ), ordered = T)
ProviderData$Quality.of.care <- factor(ProviderData$Quality.of.care, levels = c("Inadequate","Requires improvement to be good", "Good","Outstanding" ), ordered = T)
ProviderData$The.effectiveness.of.leaders.and.managers <- factor(ProviderData$The.effectiveness.of.leaders.and.managers, levels = c("Inadequate","Requires improvement to be good", "Good","Outstanding" ), ordered = T)
ProviderData$How.well.children.and.young.people.are.helped.and.protected <- factor(ProviderData$How.well.children.and.young.people.are.helped.and.protected, levels = c("Inadequate","Requires improvement to be good", "Good","Outstanding" ), ordered = T)
ProviderData$Outcomes.for.children.and.young.people <- factor(ProviderData$Outcomes.for.children.and.young.people, levels = c("Inadequate","Requires improvement to be good", "Good","Outstanding" ), ordered = T)

#Remove non children's homes (done long way to show what is getting excluded)
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Secure Training Centre"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Residential special school (>295 days/year)"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Residential Special School"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Boarding School"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Boarding school"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Secure training centre"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Further Education College with Residential Accommodation"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Voluntary Adoption Agency"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Secure children's home"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Residential special school (registered as a children's home)"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Residential Holiday Scheme for Disabled Children"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Residential Family Centre"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Further Education College with Residential Accommodation"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Adoption Support Agency"),]
ProviderData <- ProviderData[which(ProviderData$Provision.type!="Independent Fostering Agency"),]

#recode CCG owned children's homes as LA for purposes of representing public ownership
ProviderData[which(ProviderData$Sector == "Health Authority"),]$Sector <- "Local Authority"

#Correct erroneous input
ProviderData[which(ProviderData$URN == "SC474543"),]$Sector <- "Voluntary"

#Create date variables
ProviderData$date <- as.Date(ProviderData$Inspection.date, format =  "%d/%m/%Y")
ProviderData$year <- format(ProviderData$date,"%Y")
rm(list=setdiff(ls(), c("ProviderData")))

write.csv(ProviderData,"C:/Users/bengo/OneDrive - Nexus365/GitHub/Outsourcing_Impact_Dashboard/Data/dashboard_provider_data.csv")

