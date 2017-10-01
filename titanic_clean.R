library(dplyr)
library(tidyr)
library(readr)
titanic = read_csv("titanic3_original.csv")

head(titanic)
glimpse(titanic)

# 1: ------ Port of Embarkation ------
# in the embarked column, replace the missing values with 'S'
# --- FIND MISSING VALUES ---
# DPLYR method
# in data frame 'titanic', filter through the NA values and count them
titanic %>% 
  group_by(embarked) %>%
  filter(is.na(embarked)) %>% 
  summarise(missing_val = n())

# --- REPLACE THE MISSING VALUES WITH 'S' ---
# in the data frame 'titanic', replace the missing values in the embarked column, with 'S'
# this will create the modified data frame 'titanic_embark' which will be passed down and modified further
titanic_embark = titanic %>% 
  replace_na(list(embarked = 'S'))
class(titanic_embark)

# check for missing values again
titanic_embark %>% 
  filter(is.na(embarked)) %>% 
  summarise(missing_val = n())


# 2: ------ AGE ------
# 1. Calculate the mean of Age and use it to populate the missing values
# 2. Think about other ways you could have populated the missing values in the age column. 
#     Why would you pick any of those over the mean (or not)? 

# 2. Other ways to fill in the missing Age:
# --- Given number of siblings and parents in the data, you can make a better estimate of age group based on parents' and siblings' age. 
# Although it requires more work, it would represent a more accurate age. For example, if it is known that a set of parents have one child and
# their ages are 35 and 30, common sense will tell us that it is unlikely that any of their children will be over the age of 10 or so. 

# find the number of missing values in 'age' column
titanic_embark %>%
  filter(is.na(age)) %>%
  summarise(missing_count = n())

# calculate the MEAN
# do not include the missing values in the calculation
mean_age = titanic_embark %>% 
  summarise(mean_age = mean(age, na.rm = TRUE), total = n())
mean_age
class(mean_age)
# calculate the MEDIAN
median_age = titanic_embark %>% 
  summarise(median_age = median(age, na.rm = TRUE), total = n())
median_age

# --- REPLACE THE MISSING VALUES WITH THE MEAN ---
# using 'replace_na', combine into a single formula from equation above
titanic_age = titanic %>% 
  replace_na(list(age = mean(age, na.rm = TRUE)))

class(titanic_age)

# check for missing values again to see if it worked
titanic_age%>%
  filter(is.na(age)) %>%
  summarise(missing_count = n())

# 3: ------ LIFEBOAT ------
# You’re interested in looking at the distribution of passengers in different lifeboats, but as we know, 
# many passengers did not make it to a boat :-( This means that there are a lot of missing values in the boat column. 
# Fill these empty slots with a dummy value e.g. the string 'None' or 'NA'

# number of missing values in boat column
titanic_age %>% 
  filter(is.na(boat)) %>% 
  summarise(boat_miss = n())

# --- REPLACE MISSING VALUE WITH 'None' ---
# DPLYR method
# in data frame titanic, replace the missing values of 'boat' column with 'None'
titanic_boat= titanic %>% 
  replace_na(list(boat = 'None'))

class(titanic_boat)

# check for missing values again. There should be zero
titanic_boat %>%
  filter(is.na(age), is.na(embarked), is.na(boat)) %>%
  summarise(missing_age = n(), missing_embark = n(), missing_boat = n())

# 4: ------ CABIN ------
# You notice that many passengers don’t have a cabin number associated with them.
# --- Does it make sense to fill missing cabin numbers with a value?
# --- What does a missing value here mean?
# You have a hunch that the fact that the cabin number is missing might be a useful indicator of survival. 
# Create a new column has_cabin_number which has 1 if there is a cabin number, and 0 otherwise.

head(titanic)
titanic_clean = 
  titanic_boat %>%
  mutate(has_cabin_number = ifelse(is.na(cabin), 0, 1))
class(titanic_clean)

# look over data
glimpse(titanic_clean)

# write the new file
write_csv(titanic_clean, "titanic_clean.csv")

# ------ ALTERNATIVELY ------
# to replace all the missing values, and to add new column, condense the code with DPLYR pipeline:
titanic_clean2 = 
  titanic %>%
  replace_na(replace = list(age = mean(age, na.rm = TRUE), 
                            boat = 'None',
                            embarked = 'S')) %>%
  mutate(has_cabin_number = ifelse(is.na(cabin), 0, 1))

titanic_clean2 %>%
  filter(is.na(age), is.na(embarked), is.na(boat)) %>%
  summarise(age_count = n(), embarked_count = n(), boat_count = n())
