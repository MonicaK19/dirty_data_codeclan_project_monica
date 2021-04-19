
#1 MVP
#1.1 Task 1 - Decathlon Data

#Loading the library necessary to begin with data reading and data cleaning

library(tidyverse)
library(readr)


#data reading from decathalon.rds file

data_clean_decathalon_v1 <- read_rds("raw_data/decathlon.rds")

#viewing the read dataset

view(data_clean_decathalon_v1)

#number of rows
nrow(data_clean_decathalon_v1)

#number of columns
ncol(data_clean_decathalon_v1)

#row and column 
dim(data_clean_decathalon_v1)

#names of the column
names(data_clean_decathalon_v1)

#view of the data type
glimpse(data_clean_decathalon_v1)

#view summary
summary(data_clean_decathalon_v1)

#viewing the class
class(data_clean_decathalon_v1)


#########################Cleaning data begins###########################

#load library

library (janitor)

#cleaning dataset
data_clean_decathalon_v2 <- data_clean_decathalon_v1 %>% 
clean_names() %>% 
rownames_to_column("names") %>% 
mutate(names = str_to_sentence(names)) %>% 
view(data_clean_decathalon_v2)


glimpse(data_clean_decathalon_v2)

#renaming some column names
names(data_clean_decathalon_v2)[names(data_clean_decathalon_v2) == "x100m"] <- "100_m"
names(data_clean_decathalon_v2)[names(data_clean_decathalon_v2) == "x110m_hurdle"] <- "110_m_hurdle"
names(data_clean_decathalon_v2)[names(data_clean_decathalon_v2) == "x400m"] <- "400_m"
names(data_clean_decathalon_v2)[names(data_clean_decathalon_v2) == "x1500m"] <- "1500_m"

names(data_clean_decathalon_v2)


#count na's as per column
colSums(is.na(data_clean_decathalon_v2))


#count total na's 
sum(is.na(data_clean_decathalon_v2))


view(data_clean_decathalon_v2)

#writing data to csv file

write_csv(data_clean_decathalon_v2, "decathalon_cleaned_data.csv")

