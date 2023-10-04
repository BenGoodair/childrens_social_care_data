source("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Code/outcomes_cleaning_function.R")
outcomes <- create_outcomes_data()
source("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Code/characteristics_cleaning_function.R")
characteristics <- create_characteristics_data()
source("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Code/expenditure_cleaning_function.R")
expenditure <- create_expenditure_data()

dashboard_data <- rbind(outcomes, characteristics, expenditure)

write.csv(dashboard_data, "C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/childrens_social_care_data/Final_Data/outputs/dashboard_data.csv")

