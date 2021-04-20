library(readxl)
library(tidyverse)


data_boing_candy_2015 <- read_xlsx("raw_data/boing-boing-candy-2015.xlsx")
data_boing_candy_2016 <- read_xlsx("raw_data/boing-boing-candy-2016.xlsx")
data_boing_candy_2017 <- read_xlsx("raw_data/boing-boing-candy-2017.xlsx")


view(data_boing_candy_2015)
view(data_boing_candy_2016)
view(data_boing_candy_2017)


glimpse(data_boing_candy_2015)
glimpse(data_boing_candy_2016)
glimpse(data_boing_candy_2017)


names(data_boing_candy_2015)
names(data_boing_candy_2016)
names(data_boing_candy_2017)

class(data_boing_candy_2015)

######################CLEANING DATA_BOING_CANDY_2015 TABLE######################


#load library

library(janitor)

#clean names
#data_boing_candy_2015v1 <- data_boing_candy_2015 %>% 
#clean_names()
  
#column separate for timestamp
data_boing_candy_2015v1 <- data_boing_candy_2015 %>% 
  separate(Timestamp, c("date", "time"), sep = " " )
class(data_boing_candy_2015v1)
view(data_boing_candy_2015v1)


#changed the column name to age
names(data_boing_candy_2015v1)[names(data_boing_candy_2015v1) == "How old are you?"] <- "age"
names(data_boing_candy_2015v1)[names(data_boing_candy_2015v1) == "Are you going actually going trick or treating yourself?"] <- "going_trick_treat"
view(data_boing_candy_2015v1)

#replacing unwanted values with NA
options(scipen = 9999)
data_boing_candy_2015v1$age <- as.integer(as.character(data_boing_candy_2015v1$age))
view(data_boing_candy_2015v1)


#counting na's in the 'age' column
sum(is.na(data_boing_candy_2015v1$age))
ncol(data_boing_candy_2015v1)

#making the table longer using pivot function
data_boing_candy_2015v2 <- data_boing_candy_2015v1 %>%
  pivot_longer(cols = `[Butterfinger]`:`Please estimate the degrees of separation you have from the following folks [Beyoncé Knowles]`,
               names_to = "candy_category", 
               values_to = "reaction") 
view(data_boing_candy_2015v2)

#replacing 0 with na in age column
na_if(data_boing_candy_2015v2$age,0)

#remove square brackets from the candy_category
view(data_boing_candy_2015v2)


#remove the square brackets from the candy_category
library(stringr)
data_boing_candy_2015v3 <- data_boing_candy_2015v2 %>% 
  mutate(candy_category = str_replace_all(candy_category, "[[:punct:]]", ""))

view(data_boing_candy_2015v3)


#####################Cleaning dat_boing_candy_2016#################

view(data_boing_candy_2016)

data_boing_candy_2016v1 <- data_boing_candy_2016 %>% 
  separate(Timestamp, c("date", "time"), sep = " " )
view(data_boing_candy_2016v1)

names(data_boing_candy_2016v1)[names(data_boing_candy_2016v1) == "How old are you?"] <- "age"
names(data_boing_candy_2016v1)[names(data_boing_candy_2016v1) == "Are you going actually going trick or treating yourself?"] <- "going_trick_treat"
names(data_boing_candy_2016v1)[names(data_boing_candy_2016v1) == "Your gender:"] <- "gender"
names(data_boing_candy_2016v1)[names(data_boing_candy_2016v1) == "Which country do you live in?"] <- "country"
names(data_boing_candy_2016v1)[names(data_boing_candy_2016v1) == "Which state, province, county do you live in?"] <- "state_county_province"

view(data_boing_candy_2016v1)


#replacing unwanted values with NA
options(scipen = 9999)
data_boing_candy_2016v1$age <- as.integer(as.character(data_boing_candy_2016v1$age))
view(data_boing_candy_2016v1)


#making the table longer using pivot function
data_boing_candy_2016v2 <- data_boing_candy_2016v1 %>%
  pivot_longer(cols = `[100 Grand Bar]`:`[York Peppermint Patties] Ignore`,
               names_to = "candy_category", 
               values_to = "reaction") 
view(data_boing_candy_2016v2)

#cleaning the column country
data_boing_candy_2016v3 <- data_boing_candy_2016v2 %>% 
  mutate(country = recode(country, "usa" = "USA", "us" = "USA", "US" =  "USA", "Usa" = "USA",
                                    "America" = "USA", "United States of America" = "USA",
                                    "United States" = "USA", "USA!!!!!!" = "USA", 
                                    "uSA" = "USA", "united states" = "USA", "United Sates" = "USA",
                                    "USA! USA!" = "USA", "U.s." = "USA", "United  States of America" = "USA",
                                    "united states of america" = "USA", "u.s." = "USA",
                                    "canada" = "Canada", "france" = "France", "U.S.A." = "USA",
                                    "england" = "England", "Us" = "USA", "uk" = "UK", 
                                    "United State" = "USA", "United Stetes" = "USA",
                                    "USA USA USA USA" = "USA", "america" = "USA",
                                    "netherlands" = "Netherlands", "kenya" = "Kenya", "UNited States" = "USA",
                                    "USA (I think but it's an election year so who can really tell)" = "USA",
                                    "USA!" = "USA", "Units States" = "Units States", "U.S." = "USA",
                                    "United states" = "USA", "USA USA USA" = "USA", "USA! USA! USA!" = "USA",
                                    "United Kingdom" = "UK", "United Kindom" = "UK", "Units States" = "USA", 
                                    "the best one - usa" = "USA",
                                    "hungary" = "Hungary", "sweden" = "Sweden", "germany" = "Germany", 
                                    "See above" = "NA", "The Netherlands" = "Netherlands",
                                    "51.0" = "NA", "47.0" = "NA", "54.0" = "NA", "44.0" = "NA", "45.0" = "NA",
                                    "30.0" = "NA", "Somewhere" = "NA", "god's country" = "NA",
                                    "there isn't one for old men" = "NA", "The Yoo Ess of Aaayyyyyy" = "NA",
                                    "one of the best ones" = "NA", "croatia" = "Croatia", "belgium" = "Belgium",
                                    "españa" = "Spain", "this one" = "NA", "Murica" = "USA", "Merica" = "USA",
                                    "Units States" = "USA"
                                    ))

data_boing_candy_2016v3 %>% 
  mutate(country = replace_na("NA"))


view(data_boing_candy_2016v3)

#making the table longer using pivot function
data_boing_candy_2016v3 <- data_boing_candy_2016v3 %>% 
  mutate(candy_category = str_replace_all(candy_category, "[[:punct:]]", ""))


xyz <- data_boing_candy_2016v3 %>% 
distinct(country)

view(xyz)

#xyz <- data_boing_candy_2016v3 %>% 
#distinct(gender)  
#view(xyz)

#cleaning the column state_county_province
data_boing_candy_2016v3 <- data_boing_candy_2016v3 %>% 
  mutate(state_county_province = recode(state_county_province, "7940.0" = "NA")
