library(readxl)
library(readr)
library(tidyverse)


data_boing_candy_2015 <- read_xlsx("raw_data/boing-boing-candy-2015.xlsx")

view(data_boing_candy_2015)
glimpse(data_boing_candy_2015)
names(data_boing_candy_2015)


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

###########################################################################

nrow(data_boing_candy_2015v4)
names(data_boing_candy_2015v4)

view(data_boing_candy_2016v3)
