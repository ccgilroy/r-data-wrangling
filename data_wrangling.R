# load packages ----
library(tidyverse)
library(haven)
library(gssr)

# load gss data objects from gssr package ----
data("gss_panel06_long")
data("gss_doc")
data("gss_panel_doc")

# let's find out some stuff
dim(gss_panel06_long) 
names(gss_panel06_long)

# this data set is so wide that summary functions like glimpse() are impractical

# subset columns with select() ----
gss_panel06_sub <- 
  gss_panel06_long %>%
  select(firstid, wave, year, id,
         age, sex, race, 
         divorce, 
         income, income06, rincome, rincom06,
         vote00, vote04, vote08, 
         earthsun)

# okay, *now* let's look at the data ----
glimpse(gss_panel06_sub)
gss_panel06_sub

# most variables are this weird haven::labelled data type---let's fix that

# what's the problem with just doing this?
as_factor(gss_panel06_sub)

# let's see why... 
gss_panel06_sub %>%
  count(age) %>%
  tail()

# sorry to all the 90-year-olds
gss_panel06 <- 
  gss_panel06_sub %>%
  mutate(age = as.numeric(age)) %>%
  as_factor()

# as_factor() works on specific variables too: 
gss_panel06_sub %>%
  mutate(age = as.numeric(age), 
         sex = as_factor(sex), 
         race = as_factor(race)) 

# grouping and summarizing ----
# summarize() can be used on its own
gss_panel06 %>%
  summarize(age = mean(age, na.rm = TRUE))

# but it usually makes more sense on grouped data
gss_panel06 %>%
  group_by(wave) %>%
  summarize(age = mean(age, na.rm = TRUE))

# you can summarize
gss_panel06 %>%
  group_by(wave) %>%
  summarize(mean_age = mean(age, na.rm = TRUE), 
            median_age = median(age, na.rm = TRUE), 
            min_age = min(age, na.rm = TRUE),
            max_age = max(age, na.rm = TRUE))

# you can group by multiple variables
gss_panel06 %>%
  filter(!is.na(id)) %>%
  group_by(sex, race) %>%
  summarize(n = n())

# count() is a useful shortcut
gss_panel06 %>%
  filter(!is.na(id)) %>%
  count(sex, race)

# what about proportions? group_by() + mutate()
gss_panel06 %>%
  filter(!is.na(id)) %>%
  count(sex, race) %>%
  group_by(sex) %>%
  mutate(p = n/sum(n))

# let's try calculating the proportion of respondents who say they believe 
# that the sun revolves around the earth, excluding NAs from the calculation
gss_panel06 %>% 
  filter(!is.na(earthsun)) %>%
  count(wave, earthsun) %>%
  group_by(wave) %>%
  mutate(p = n/sum(n))

# bonus: what does the proportion look like in each wave?

# working with factors using forcats ----
gss_panel06 %>%
  filter(!is.na(id)) %>%
  count(divorce)

levels(gss_panel06$divorce)

# drop unused levels
fct_example <- 
  gss_panel06 %>% 
  mutate(divorce = fct_drop(divorce))
  
levels(fct_example$divorce)

fct_example <- 
  fct_example %>%
  mutate(divorce = fct_relevel(divorce, "NO", "YES"))

levels(fct_example$divorce)

# recoding rincome 
gss_panel06 %>%
  count(rincome)

levels(gss_panel06$rincome)

fct_example2 <- 
  gss_panel06 %>%
  mutate(rincome_num = fct_recode(rincome,  
                                  `500` = "LT $1000",
                                  `2000` = "$1000 TO 2999",
                                  `3500` = "$3000 TO 3999",
                                  `4500` = "$4000 TO 4999",
                                  `5500` = "$5000 TO 5999",
                                  `6500` = "$6000 TO 6999",
                                  `7500` = "$7000 TO 7999",
                                  `9000` = "$8000 TO 9999",
                                  `12500` = "$10000 - 14999",
                                  `17500` = "$15000 - 19999",
                                  `22500` = "$20000 - 24999",
                                  `25000` = "$25000 OR MORE"), 
         rincome_num = as.numeric(as.character(rincome_num)))
# why'd I change the data type twice?
# what happens if you leave out `as.character()`?

fct_example2 %>%
  count(rincome, rincome_num)

# another way - case_when() 
# case_when() is a multi-condition ifelse()
gss_panel06 %>%
  mutate(rincome_num = case_when(
    rincome == "LT $1000" ~ 500,
    rincome == "$1000 TO 2999" ~ 2000,
    rincome == "$3000 TO 3999" ~ 3500,
    rincome == "$4000 TO 4999" ~ 4500,
    rincome == "$5000 TO 5999" ~ 5500,
    rincome == "$6000 TO 6999" ~ 6500,
    rincome == "$7000 TO 7999" ~ 7500,
    rincome == "$8000 TO 9999" ~ 9000,
    rincome == "$10000 - 14999" ~ 12500,
    rincome == "$15000 - 19999" ~ 17500,
    rincome == "$20000 - 24999" ~ 22500,
    rincome == "$25000 OR MORE" ~ 25000, 
    TRUE ~ NA_real_))

# working with text using stringr ----
str_example <- 
  gss_panel06 %>% 
  select(rincome)
  
# pull out text matching a pattern
str_example <- 
  str_example %>%
  mutate(rincome_lower = str_extract(rincome, pattern = "^\\$[0-9]+")) %>%
  mutate(rincome_upper = str_extract(rincome, pattern = "[0-9]+$")) 

# clean up the strings a bit more
# then convert to numeric
str_example <- 
  str_example %>%
  mutate(rincome_lower = str_remove(rincome_lower, "\\$")) %>%
  mutate(rincome_lower = as.numeric(rincome_lower), 
         rincome_upper = as.numeric(rincome_upper)) 

# check our work!
str_example %>%
  count(rincome, rincome_lower, rincome_upper)
  
# some other possible data wrangling tasks
# - calculate midpoints with mutate
# - replace NA values for rincome_lower with 0 if rincome_upper == 1000
# - figure out which of the above approaches might work for rincom06
