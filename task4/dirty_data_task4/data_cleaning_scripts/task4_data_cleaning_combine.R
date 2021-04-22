#LOADING LIBRARY####

library(readxl)
library(readr)
library(tidyverse)
library(janitor)
library(stringr)


#############################BOING_BOING_CANDY_2015 DATA CLEANING###############

###READING THE DATA####
data_boing_candy_2015 <- read_xlsx("raw_data/boing-boing-candy-2015.xlsx")

###FUNCTIONS TO UNDERSTAND THE DATASET###
#view(data_boing_candy_2015)
glimpse(data_boing_candy_2015)
names(data_boing_candy_2015)
class(data_boing_candy_2015)

###SEPARATING COLUMN TIMESTAMP INTO DATE AND TIME###

data_boing_candy_2015v1 <- data_boing_candy_2015 %>% 
  separate(Timestamp, c("date", "time"), sep = " " )
class(data_boing_candy_2015v1)



###CHANGING COLUMN NAMES###

names(data_boing_candy_2015v1)[names(data_boing_candy_2015v1) == "How old are you?"] <- "age"
names(data_boing_candy_2015v1)[names(data_boing_candy_2015v1) == "Are you going actually going trick or treating yourself?"] <- "going_trick_treat"
names(data_boing_candy_2015v1)[names(data_boing_candy_2015v1) == "Please leave any remarks or comments regarding your choices."] <- "comments"
#view(data_boing_candy_2015v1)


###REMOVING IRRELEVANT COLUMNS FROM THE DATASET###

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



###REPLACING UNWANTED VALUES WITH NA###

options(scipen = 9999)
data_boing_candy_2015v1$age <- as.integer(as.character(data_boing_candy_2015v1$age))
#view(data_boing_candy_2015v1)


###COUNTING NA'S IN THE AGE COLUMN###

sum(is.na(data_boing_candy_2015v1$age))
ncol(data_boing_candy_2015v1)


###MAKING THE TABLE LONGER USING PIVOT LONGER###

data_boing_candy_2015v2 <- data_boing_candy_2015v1 %>%
  pivot_longer(cols = `[Butterfinger]`:`[Necco Wafers]` ,
               names_to = "candy_category", 
               values_to = "reaction") 
#view(data_boing_candy_2015v2)

###REMOVING VALUES WHICH ARE NOT JOY OR DESPAIR###

data_boing_candy_2015v3 <-subset(data_boing_candy_2015v2, 
                                 data_boing_candy_2015v2$reaction == "JOY" | 
                                   data_boing_candy_2015v2$reaction == "DESPAIR")
#view(data_boing_candy_2015v3)



###REPLACING 0 WITH NA IN THE AGE COLUMN ###

na_if(data_boing_candy_2015v3$age,0)


###REMOVING THE SQUARE BRACKETS FROM THE CANDY_CATEGORY COLUMN###

data_boing_candy_2015v4 <- data_boing_candy_2015v3 %>% 
  mutate(candy_category = str_replace_all(candy_category, "[:punct:]", ""))
#view(data_boing_candy_2015v4)


### TIDYING THE AGE COLUMN###


data_boing_candy_2015v5 <- data_boing_candy_2015v4 %>% 
mutate(age = if_else(age < 0 | age > 100, NA_integer_, age)) 
 #%>% 
  #filter(age>100)

view(data_boing_candy_2015v5)

#data_boing_candy_2015v4 %>% 
 # filter(age>100)
################################################################################


#############################BOING_BOING_CANDY_2016 DATA CLEANING###############

###READING THE DATA####

data_boing_candy_2016 <- read_xlsx("raw_data/boing-boing-candy-2016.xlsx")


###FUNCTIONS TO UNDERSTAND THE DATASET###

#view(data_boing_candy_2016)
glimpse(data_boing_candy_2016)
names(data_boing_candy_2016)


###SEPARATING COLUMN TIMESTAMP INTO DATE AND TIME###

data_boing_candy_2016v1 <- data_boing_candy_2016 %>% 
  separate(Timestamp, c("date", "time"), sep = " " )
#view(data_boing_candy_2016v1)


###CHANGING COLUMN NAMES###

names(data_boing_candy_2016v1)[names(data_boing_candy_2016v1) == "How old are you?"] <- "age"
names(data_boing_candy_2016v1)[names(data_boing_candy_2016v1) == "Are you going actually going trick or treating yourself?"] <- "going_trick_treat"
names(data_boing_candy_2016v1)[names(data_boing_candy_2016v1) == "Your gender:"] <- "gender"
names(data_boing_candy_2016v1)[names(data_boing_candy_2016v1) == "Which country do you live in?"] <- "country"
names(data_boing_candy_2016v1)[names(data_boing_candy_2016v1) == "Which state, province, county do you live in?"] <- "state_county_province"
#view(data_boing_candy_2016v1)


###REMOVING IRRELEVANT COLUMNS FROM THE DATASET###

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


###REPLACING IRRELEVANT VALUES WITH NA###

options(scipen = 9999)
data_boing_candy_2016v1$age <- as.integer(as.character(data_boing_candy_2016v1$age))
#view(data_boing_candy_2016v1)


###MAKING TABLE LONGER USING THE PIVOT FUNCTION###

data_boing_candy_2016v2 <- data_boing_candy_2016v1 %>%
  pivot_longer(cols = `[100 Grand Bar]`:`[York Peppermint Patties]`,
               names_to = "candy_category", 
               values_to = "reaction") 
#view(data_boing_candy_2016v2)


####CLEANING THE COLUMN COUNTRY###

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


###TIDYING THE COLUMN NAME BY GIVING THE FULL NAME###

data_boing_candy_2016v4 <- data_boing_candy_2016v3 %>% 
  mutate(state_county_province = recode(state_county_province, "il" = "Illinois",
                                        "PA" = "Pennsylvania", "IN" = "Indiana",
                                        "the Mitten" = "Michigan"))
#view(data_boing_candy_2016v3)


###REPLACING "NA" WITH NA###

data_boing_candy_2016v5  <- data_boing_candy_2016v4 %>% 
  mutate(country = replace_na("NA"))
#view(data_boing_candy_2016v3)


###REMOVING THE SQUARE BRACKETS FROM THE CANDY_CATEGORY COLUMN###

data_boing_candy_2016v6 <- data_boing_candy_2016v5 %>% 
  mutate(candy_category = str_replace_all(candy_category, "[[:punct:]]", ""))
#view(data_boing_candy_2016v3)



###TIDYING THE COLUMN STATE_COUNTY_PROVINCE###

data_boing_candy_2016v7 <- data_boing_candy_2016v6 %>% 
  mutate(state_county_province = recode(state_county_province, "7940.0" = "NA"))
#view(data_boing_candy_2016v3)


### TIDYING THE AGE COLUMN###


data_boing_candy_2016v7 %>% 
  mutate(age = if_else(age < 0 | age > 100, NA_integer_, age)) %>% 
  filter(age>100)

#view(data_boing_candy_2016v3)

################################################################################

#############################BOING_BOING_CANDY_2017 DATA CLEANING###############

###READING THE DATA####

data_boing_candy_2017 <- read_xlsx("raw_data/boing-boing-candy-2017.xlsx")

###FUNCTIONS TO UNDERSTAND THE DATASET###

#view(data_boing_candy_2017)
glimpse(data_boing_candy_2017)
names(data_boing_candy_2017)


###CHANGING COLUMN NAMES###

names(data_boing_candy_2017)[names(data_boing_candy_2017) == "Q1: GOING OUT?"] <- "going_trick_treat"
names(data_boing_candy_2017)[names(data_boing_candy_2017) == "Q2: GENDER"] <- "gender"
names(data_boing_candy_2017)[names(data_boing_candy_2017) == "Q3: AGE"] <- "age"
names(data_boing_candy_2017)[names(data_boing_candy_2017) == "Q4: COUNTRY"] <- "country"
names(data_boing_candy_2017)[names(data_boing_candy_2017) == "Q5: STATE, PROVINCE, COUNTY, ETC"] <- "state_county_province"
#view(data_boing_candy_2017)


###REMOVING IRRELEVANT COLUMNS###

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


###MAKING TABLE LONGER USING PIVOT FUNCTIONS###

data_boing_candy_2017v1 <- data_boing_candy_2017 %>%
  pivot_longer(cols = `Q6 | 100 Grand Bar`:`Q6 | York Peppermint Patties`,
               names_to = "candy_category", 
               values_to = "reaction") 
#view(data_boing_candy_2017v1)


###REPLACING STRING VALUE###

data_boing_candy_2017v2 <- data_boing_candy_2017v1 %>% 
  mutate(candy_category = str_replace(candy_category,"Q[0-9] \\| ", ""))


### PUTTING NA###

options(scipen = 9999)
data_boing_candy_2017v2$age <- as.integer(as.character(data_boing_candy_2017v2$age))
#view(data_boing_candy_2017v2)


###REPLACING UNWANTED VALUES WITH NA###

data_boing_candy_2017v2$age[data_boing_candy_2017v2$age == 1000] <- NA
data_boing_candy_2017v2$age[data_boing_candy_2017v2$age == 312] <- NA
#view(data_boing_candy_2017v2)


###REMOVING BRACKETS FROM THE STRING###

data_boing_candy_2017v3 <- data_boing_candy_2017v2 %>% 
  mutate(candy_category = str_replace_all(candy_category, "[[:punct:]]", ""))
#view(data_boing_candy_2017v3)



###TIDYING THE COLUMN COUNTRY###
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

data_boing_candy_2017v5 <- data_boing_candy_2017v4 %>% 
  mutate(date = "2017-01-01")

view(data_boing_candy_2017v5)
################################################################################


###########################MERGING DATASET######################################

data_2015_2017 <- bind_rows(data_boing_candy_2015v5,data_boing_candy_2017v5)

#view(data_2015_2017)

data_2015_2017_2016 <- bind_rows(data_2015_2017,data_boing_candy_2016v7)

#view(data_2015_2017_2016)

###write the csv file###

final_candy_ratings <- write_csv(data_2015_2017_2016,"final_candy_ratings.csv")


################################################################################