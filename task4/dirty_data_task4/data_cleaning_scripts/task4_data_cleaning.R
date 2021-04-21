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
#view(data_boing_candy_2015v1)


#changed the column name to age
names(data_boing_candy_2015v1)[names(data_boing_candy_2015v1) == "How old are you?"] <- "age"
names(data_boing_candy_2015v1)[names(data_boing_candy_2015v1) == "Are you going actually going trick or treating yourself?"] <- "going_trick_treat"
names(data_boing_candy_2015v1)[names(data_boing_candy_2015v1) == "Please leave any remarks or comments regarding your choices."] <- "comments"
#view(data_boing_candy_2015v1)


#removing unwanted columns from the dataset
data_boing_candy_2015v1$`Please list any items not included above that give you JOY.` <- NULL
data_boing_candy_2015v1$`Please list any items not included above that give you DESPAIR.` <- NULL
data_boing_candy_2015v1$`Guess the number of mints in my hand.` <- NULL
data_boing_candy_2015v1$`Betty or Veronica?` <- NULL
data_boing_candy_2015v1$`Check all that apply: "I cried tears of sadness at the end of  ____________"`<- NULL
data_boing_candy_2015v1$`Fill in the blank: "Taylor Swift is a force for ___________"` <- NULL
data_boing_candy_2015v1$`What is your favourite font?` <- NULL
data_boing_candy_2015v1$`If you squint really hard, the words "Intelligent Design" would look like.` <- NULL
data_boing_candy_2015v1$`Fill in the blank: "Imitation is a form of ____________"` <- NULL
data_boing_candy_2015v1$`Please estimate the degree(s) of separation you have from the following celebrities [JK Rowling]` <- NULL
data_boing_candy_2015v1$`Please estimate the degree(s) of separation you have from the following celebrities [JJ Abrams]` <- NULL
data_boing_candy_2015v1$`Please estimate the degree(s) of separation you have from the following celebrities [Beyoncé]` <- NULL
data_boing_candy_2015v1$`Please estimate the degree(s) of separation you have from the following celebrities [Bieber]` <- NULL
data_boing_candy_2015v1$`Please estimate the degree(s) of separation you have from the following celebrities [Kevin Bacon]` <- NULL
data_boing_candy_2015v1$`Please estimate the degree(s) of separation you have from the following celebrities [Francis Bacon (1561 - 1626)]` <- NULL
data_boing_candy_2015v1$`Which day do you prefer, Friday or Sunday?` <- NULL
data_boing_candy_2015v1$`Please estimate the degrees of separation you have from the following folks [Bruce Lee]` <- NULL
data_boing_candy_2015v1$`Please estimate the degrees of separation you have from the following folks [JK Rowling]` <- NULL
data_boing_candy_2015v1$`Please estimate the degrees of separation you have from the following folks [Malala Yousafzai]` <- NULL
data_boing_candy_2015v1$`Please estimate the degrees of separation you have from the following folks [Thom Yorke]` <- NULL
data_boing_candy_2015v1$`Please estimate the degrees of separation you have from the following folks [JJ Abrams]` <- NULL
data_boing_candy_2015v1$`Please estimate the degrees of separation you have from the following folks [Hillary Clinton]` <- NULL
data_boing_candy_2015v1$`Please estimate the degrees of separation you have from the following folks [Donald Trump]` <- NULL
data_boing_candy_2015v1$`Please estimate the degrees of separation you have from the following folks [Beyoncé Knowles]` <- NULL
data_boing_candy_2015v1$`"That dress* that went viral early this year - when I first saw it, it was ________"` <- NULL


#view(data_boing_candy_2015v1)



#replacing unwanted values with NA
options(scipen = 9999)
data_boing_candy_2015v1$age <- as.integer(as.character(data_boing_candy_2015v1$age))
#view(data_boing_candy_2015v1)


#counting na's in the 'age' column
sum(is.na(data_boing_candy_2015v1$age))
ncol(data_boing_candy_2015v1)

#making the table longer using pivot function
data_boing_candy_2015v2 <- data_boing_candy_2015v1 %>%
  pivot_longer(cols = `[Butterfinger]`:`[Necco Wafers]` ,
               names_to = "candy_category", 
               values_to = "reaction") 
#view(data_boing_candy_2015v2)

###################removing everything except JOY, DESPAIR##################
data_boing_candy_2015v3 <-subset(data_boing_candy_2015v2, 
                                 data_boing_candy_2015v2$reaction == "JOY" | 
                                 data_boing_candy_2015v2$reaction == "DESPAIR")
#view(data_boing_candy_2015v3)



#replacing 0 with na in age column
na_if(data_boing_candy_2015v3$age,0)


#remove the square brackets from the candy_category
library(stringr)
data_boing_candy_2015v4 <- data_boing_candy_2015v3 %>% 
  mutate(candy_category = str_replace_all(candy_category, "[[:punct:]]", ""))

#view(data_boing_candy_2015v3)


#####################Cleaning dat_boing_candy_2016#################

#view(data_boing_candy_2016)

data_boing_candy_2016v1 <- data_boing_candy_2016 %>% 
  separate(Timestamp, c("date", "time"), sep = " " )
#view(data_boing_candy_2016v1)

names(data_boing_candy_2016v1)[names(data_boing_candy_2016v1) == "How old are you?"] <- "age"
names(data_boing_candy_2016v1)[names(data_boing_candy_2016v1) == "Are you going actually going trick or treating yourself?"] <- "going_trick_treat"
names(data_boing_candy_2016v1)[names(data_boing_candy_2016v1) == "Your gender:"] <- "gender"
names(data_boing_candy_2016v1)[names(data_boing_candy_2016v1) == "Which country do you live in?"] <- "country"
names(data_boing_candy_2016v1)[names(data_boing_candy_2016v1) == "Which state, province, county do you live in?"] <- "state_county_province"

#view(data_boing_candy_2016v1)


data_boing_candy_2016v1$`Please list any items not included above that give you JOY.` <- NULL
data_boing_candy_2016v1$`[York Peppermint Patties] Ignore` <- NULL
data_boing_candy_2016v1$`Please leave any witty, snarky or thoughtful remarks or comments regarding your choices.` <- NULL
data_boing_candy_2016v1$`Guess the number of mints in my hand.` <- NULL
data_boing_candy_2016v1$`Betty or Veronica?` <- NULL
data_boing_candy_2016v1$`What is your favourite font?` <- NULL
data_boing_candy_2016v1$`Please estimate the degree(s) of separation you have from the following celebrities [JK Rowling]` <- NULL
data_boing_candy_2016v1$`Please estimate the degree(s) of separation you have from the following celebrities [JJ Abrams]` <- NULL
data_boing_candy_2016v1$`Please estimate the degree(s) of separation you have from the following celebrities [Beyoncé]` <- NULL
data_boing_candy_2016v1$`Please estimate the degree(s) of separation you have from the following celebrities [Bieber]` <- NULL
data_boing_candy_2016v1$`Please estimate the degree(s) of separation you have from the following celebrities [Kevin Bacon]` <- NULL
data_boing_candy_2016v1$`Please estimate the degree(s) of separation you have from the following celebrities [Francis Bacon (1561 - 1626)]` <- NULL
data_boing_candy_2016v1$`Which day do you prefer, Friday or Sunday?` <- NULL
data_boing_candy_2016v1$`Do you eat apples the correct way, East to West (side to side) or do you eat them like a freak of nature, South to North (bottom to top)?` <- NULL
data_boing_candy_2016v1$`When you see the above image of the 4 different websites, which one would you most likely check out (please be honest).` <- NULL
data_boing_candy_2016v1$`"That dress* that went viral a few years back - when I first saw it, it was ________"` <- NULL
data_boing_candy_2016v1$`Please list any items not included above that give you DESPAIR.` <- NULL
data_boing_candy_2016v1$`[Person of Interest Season 3 DVD Box Set (not including Disc 4 with hilarious outtakes)]` <- NULL


#view(data_boing_candy_2016v1)


#replacing unwanted values with NA
options(scipen = 9999)
data_boing_candy_2016v1$age <- as.integer(as.character(data_boing_candy_2016v1$age))
#view(data_boing_candy_2016v1)


#making the table longer using pivot function
data_boing_candy_2016v2 <- data_boing_candy_2016v1 %>%
  pivot_longer(cols = `[100 Grand Bar]`:`[York Peppermint Patties]`,
               names_to = "candy_category", 
               values_to = "reaction") 
#view(data_boing_candy_2016v2)

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

data_boing_candy_2016v3 <- data_boing_candy_2016v3 %>% 
  mutate(state_county_province = recode(state_county_province, "il" = "Illinois",
                                        "PA" = "Pennsylvania", "IN" = "Indiana",
                                        "the Mitten" = "Michigan"))
#separating state/county/province column 
#data_boing_candy_2016v3 <- data_boing_candy_2016v3 %>% 
  #separate(state_county_province, c("state", "county", "city", sep = "," ))
#view(data_boing_candy_2016v3)



data_boing_candy_2016v3 %>% 
  mutate(country = replace_na("NA"))


#view(data_boing_candy_2016v3)

#making the table longer using pivot function
data_boing_candy_2016v3 <- data_boing_candy_2016v3 %>% 
  mutate(candy_category = str_replace_all(candy_category, "[[:punct:]]", ""))
view(data_boing_candy_2016v3)

xyz <- data_boing_candy_2016v3 %>% 
distinct(country)
view(data_boing_candy_2016v3)

#data_boing_candy_2016v3 %>% 
#distinct(reaction)  

sss<-data_boing_candy_2016v3 %>% 
distinct(state_county_province) 
view(sss)

#cleaning the column state_county_province
data_boing_candy_2016v3 <- data_boing_candy_2016v3 %>% 
  mutate(state_county_province = recode(state_county_province, "7940.0" = "NA"))
view(data_boing_candy_2016v3)
