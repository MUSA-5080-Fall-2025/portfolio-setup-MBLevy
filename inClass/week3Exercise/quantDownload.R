library(tidyverse)
library(tidycensus)

census_api_key(Sys.getenv("f6ac8f4f33d6431a4a6a8713369bc824e47dc1e4"), install = TRUE)
#Get ACS vars each year
acs2023 <- load_variables(2023, "acs5", cache = TRUE)
acs2013 <- load_variables(2013, "acs5", cache = TRUE)

#Get income vars
income2023 <- acs2023 %>%
  filter(str_detect(name, "B19001"))


#Get Desired Race/Ethnicities
incomeBlack <- acs2023 %>%
  filter(str_detect(name, "B19001B"))
incomeAsian <- acs2023 %>%
  filter(str_detect(name, "B19001D"))
incomeWhite <- acs2023 %>%
  filter(str_detect(name, "B19001A"))
incomeHisp <- acs2023 %>%
  filter(str_detect(name, "B19001I"))

#Adding adjusted labels
incomeBlack <- incomeBlack %>% mutate(
  label2 = paste(label, " Black", sep = "")
)
incomeWhite <- incomeWhite %>% mutate(
  label2 = paste(label, " White", sep = "")
)
incomeAsian <- incomeAsian %>% mutate(
  label2 = paste(label, " Asian", sep = "")
)
incomeHisp <- incomeHisp %>% mutate(
  label2 = paste(label, " Hisp", sep = "")
)

incomeBlack

print("All B19 (Income) table variables:")
head(income2023, 10)


allLabels <- c(incomeAsian$name, incomeBlack$name, incomeHisp$name, incomeWhite$name)

allLabels


# Get data for just one state to test
data2013CT <- get_acs(
  geography = "tract",
  variables = allLabels,
  state = "PA",
  county = "Philadelphia",
  year = 2013,
  survey = "acs5"
)
data2013City <- get_acs(
  geography = "county",
  variables = allLabels,
  state = "PA",
  county = "Philadelphia",
  year = 2013,
  survey = "acs5"
)

data2023CT <- get_acs(
  geography = "tract",
  variables = allLabels,
  state = "PA",
  county = "Philadelphia",
  year = 2023,
  survey = "acs5"
)
data2023City <- get_acs(
  geography = "county",
  variables = allLabels,
  state = "PA",
  county = "Philadelphia",
  year = 2023,
  survey = "acs5"
)


#Limit to wanted census tracts, 20, 32, 33, 36
  #Format "Census Tract 33;"

data2013CT <- data2013CT %>%
  filter(str_detect(NAME, "Census Tract (20|32|33|36),"))
data2023CT <- data2023CT %>%
  filter(str_detect(NAME, "Census Tract (20|32|33|36);"))


nameLabelAsian <- incomeAsian %>% select(variable = name, label = label2)
nameLabelBlack <- incomeBlack %>% select(variable = name, label = label2)
nameLabelWhite <- incomeWhite %>% select(variable = name, label = label2)
nameLabelHisp <- incomeHisp %>% select(variable = name, label = label2)


#add labels
data2013CT <- left_join(data2013CT, nameLabelAsian,
                        by = "variable")
data2013CT <- left_join(data2013CT, nameLabelBlack,
                        by = "variable")
data2013CT <- left_join(data2013CT, nameLabelWhite,
                        by = "variable")
data2013CT <- left_join(data2013CT, nameLabelHisp,
                        by = "variable")

data2023CT <- left_join(data2023CT, nameLabelAsian,
                        by = "variable")
data2023CT <- left_join(data2023CT, nameLabelBlack,
                        by = "variable")
data2023CT <- left_join(data2023CT, nameLabelWhite,
                        by = "variable")
data2023CT <- left_join(data2023CT, nameLabelHisp,
                        by = "variable")

data2013City <- left_join(data2013City, nameLabelAsian,
                        by = "variable")
data2013City <- left_join(data2013City, nameLabelBlack,
                        by = "variable")
data2013City <- left_join(data2013City, nameLabelWhite,
                        by = "variable")
data2013City <- left_join(data2013City, nameLabelHisp,
                        by = "variable")

data2023City <- left_join(data2023City, nameLabelAsian,
                        by = "variable")
data2023City <- left_join(data2023City, nameLabelBlack,
                        by = "variable")
data2023City <- left_join(data2023City, nameLabelWhite,
                        by = "variable")
data2023City <- left_join(data2023City, nameLabelHisp,
                        by = "variable")


#install.packages('writexl')
library(writexl)

#Write Excel Files
write_xlsx(data2013CT, 'C:/Users/matth/Documents/pennFall25/Quant/assignment1/B19001-2013CT.xlsx')
write_xlsx(data2023CT, 'C:/Users/matth/Documents/pennFall25/Quant/assignment1/B19001-2023CT.xlsx')
write_xlsx(data2013City, 'C:/Users/matth/Documents/pennFall25/Quant/assignment1/B19001-2013City.xlsx')
write_xlsx(data2023City, 'C:/Users/matth/Documents/pennFall25/Quant/assignment1/B19001-2023City.xlsx')
