library(readxl)
library(tidyverse)

data_boing_candy_2017 <- read_xlsx("raw_data/boing-boing-candy-2017.xlsx")

view(data_boing_candy_2017)
glimpse(data_boing_candy_2017)
names(data_boing_candy_2017)



names(data_boing_candy_2017)[names(data_boing_candy_2017) == "Q1: GOING OUT?"] <- "going_trick_treat"
names(data_boing_candy_2017)[names(data_boing_candy_2017) == "Q2: GENDER"] <- "gender"
names(data_boing_candy_2017)[names(data_boing_candy_2017) == "Q3: AGE"] <- "age"
names(data_boing_candy_2017)[names(data_boing_candy_2017) == "Q4: COUNTRY"] <- "country"
names(data_boing_candy_2017)[names(data_boing_candy_2017) == "Q5: STATE, PROVINCE, COUNTY, ETC"] <- "state_province_county"

#view(data_boing_candy_2017)

#REMOVING UNWANTED COLUMNS
data_boing_candy_2017$...114 <- NULL
data_boing_candy_2017$`Click Coordinates (x, y)`<- NULL
data_boing_candy_2017$`Q12: MEDIA [Yahoo]`<- NULL
data_boing_candy_2017$`Q12: MEDIA [ESPN]`<- NULL
data_boing_candy_2017$`Q12: MEDIA [Science]`<- NULL
data_boing_candy_2017$`Q12: MEDIA [Daily Dish]`<- NULL
data_boing_candy_2017$`Q11: DAY`<- NULL
data_boing_candy_2017$`Q10: DRESS`<- NULL
data_boing_candy_2017$`Q9: OTHER COMMENTS`<- NULL
data_boing_candy_2017$`Q7: JOY OTHER`<- NULL
data_boing_candy_2017$`Q8: DESPAIR OTHER`<- NULL

#view(data_boing_candy_2017)

#making the table longer using pivot function
data_boing_candy_2017v1 <- data_boing_candy_2017 %>%
  pivot_longer(cols = `Q6 | 100 Grand Bar`:`Q6 | York Peppermint Patties`,
               names_to = "candy_category", 
               values_to = "reaction") 

#view(data_boing_candy_2017v1)

#string replace

data_boing_candy_2017v2 <- data_boing_candy_2017v1 %>% 
  mutate(candy_category = str_replace(candy_category,"Q[0-9] \\| ", ""))

options(scipen = 9999)
data_boing_candy_2017v2$age <- as.integer(as.character(data_boing_candy_2017v2$age))
view(data_boing_candy_2017v2)

#replacing unwanted values with NA
data_boing_candy_2017v2$age[data_boing_candy_2017v2$age == 1000] <- NA
data_boing_candy_2017v2$age[data_boing_candy_2017v2$age == 312] <- NA
  
#view(data_boing_candy_2017v2)

data_boing_candy_2017v2 %>% 
group_by(age) %>% 
  summarise(n()) %>% 
  arrange(desc(age))

view(data_boing_candy_2015v2)


data_boing_candy_2017v3 <- data_boing_candy_2017v3 %>% 
  mutate(candy_category = str_replace_all(candy_category, "[[:punct:]]", ""))
view(data_boing_candy_2017v3)

xyz <- data_boing_candy_2017v3 %>% 
  distinct(country)

view(xyz)


#cleaning the column country
data_boing_candy_2017v4 <- data_boing_candy_2017v3 %>% 
  mutate(country = recode(country, "'merica" = "USA", "Ahem....Amerca" = "USA",
                          "america" =  "USA", "America" = "USA",
                          "australia" = "Australia", "Can" = "Canada",
                          "canada" = "Canada", "Canada`" = "Canada", "cascadia" = "Cascadia",
                          "CANADA" = "Canada",  "endland" = "England", "finland"= "Finland",
                          "france" = "France", "germany" = "Germany", "hong kong" = "Hong Kong",
                          "spain" = "Spain", "The United States"= "USA",
                          "The United States of America" = "USA", "U S" = "USA", "u s a" = "USA", 
                          "U.K." = "UK", "u.s." = "USA", "U.S." = "USA", "u.s.a." = "USA", "U.S.A." = "USA",
                          "uk" = "UK", "Uk" = "UK", "unhinged states" = "USA", "Unied States" = "USA",
                          "unite states" = "USA", "United kingdom" = "UK", "United Kingdom" = "UK",
                          "United Sates" = "USA", "United staes" = "USA", "United State" = "USA",
                          "United Statea" = "USA", "United Stated" = "USA", "united states" = "USA",
                          "united States" = "USA", "United states" = "USA", "United States" = "USA",
                          "united states of america" = "USA", "United States of America" = "USA",
                          "United Statss" = "USA", "united ststes" = "USA", "United ststes" = "USA",
                          "Unites States" = "USA", "us" = "USA", "Us"= "USA", "US" = "USA",
                          "US of A" = "USA", "usa" = "USA", "Usa" = "USA", "USa" = "USA", "USA" = "USA",
                          "USA USA USA!!!!" = "USA", "USA! USA! USA!" = "USA", "USA? Hard to tell anymore.." = "USA",
                          "USAA" = "USA", "usas" = "USA", "USAUSAUSA" = "USA",
                          "1" = "NA", "32" = "NA", "35" = "NA", "45" = "NA", "46" = "NA",
                          "A" = "NA","Fear and Loathing" = "NA", "I don't know anymore" = "NA",
                          "I pretend to be from Canada, but I am really from the United States." = "NA",
                          "insanity lately" = "NA", "subscribe to dm4uz3 on youtube" = "NA", "UD" = "NA"
                          ))

view(data_boing_candy_2017v4)

xyz <- data_boing_candy_2017v4 %>% 
  distinct(state_province_county)

view(xyz)

###########################################################################
nrow(data_boing_candy_2017v4)
names(data_boing_candy_2017v4)


