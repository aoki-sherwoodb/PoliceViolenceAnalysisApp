# final-project
# Ben Aoki-Sherwood and Avery Watts
# June 2022, Intro to Data Science

library(tidyverse)

#load datasets
residence <- read_csv('data/police_residence.csv', 
                      col_types = cols(black = col_double(),
                                       hispanic = col_double(),
                                       asian = col_double()))
cities <- read_csv('data/cities.csv')
demographics <- read_delim('data/us-cities-demographics.csv', delim = ';')
killings <- read_csv('data/police_killings.csv')
hate_crimes <- read_csv('data/hate_crimes.csv')

#standardize variables across geographical data sets for joining
residence <- residence %>%
  mutate(city = str_to_lower(str_extract(residence$city, '[^,]+(?=(,|$))')),
         across(.cols = c('black', 'hispanic', 'asian'), .fns = coalesce, 0.0))

cities <- cities %>%
  mutate(city = str_to_lower(CITY), lat = LATITUDE, lon = LONGITUDE, state_code = STATE_CODE) %>%
  select(c('city', 'lat', 'lon', state_code))

dups <- duplicated(cities %>% select(c('city', 'state_code')))

cities <- cities %>%
  filter(!dups)

demographics <- demographics %>%
  rename_with(str_to_lower) %>%
  rename_with(.cols = everything(), .fn = str_replace_all, ' ', '_') %>%
  mutate(across(.cols = c('city', 'race'), .fns = str_to_lower)) 

#extract number of police killings in each city 
killings_by_city <- killings %>%
  mutate(city = str_to_lower(city)) %>%
  group_by(city, state) %>%
  summarize(killings = n())

#join location and demographic data to police residence data for all cities in residence
joined_cities <- residence %>%
  left_join(cities, by = c('city', 'state_code')) %>%
  left_join(demographics, by = c('city', 'state_code')) %>%
  left_join(killings_by_city, by = c('city', 'state_code' = 'state')) %>%
  mutate(killings = coalesce(killings, 0),
         race_prop = count / total_population, 
         police_force_prop = police_force_size / total_population) %>%
  pivot_wider(names_from = race, values_from = c('count', 'race_prop')) %>%
  rename_with(.cols = everything(), .fn = str_replace_all, '( |-)', '_') %>%
  select(-c('count_NA', 'race_prop_NA'))
  
write_csv(joined_cities, 'data/joined_cities.csv')

#find which variables are most important for predicting killings
library(caret)
library(randomForest)
set.seed(2347904)

rf_data <- joined_cities %>%
  mutate(had_killing = as.factor(ifelse(killings > 0, 'yes', 'no')),
         had_killing = fct_relevel(had_killing, 'yes')) %>%
  select(-c('city', 'state_code', 'state', 'killings')) %>%
  drop_na()

train_control <- trainControl(method = "cv", number = 10)

killings_rf <- randomForest(had_killing ~ . , data = rf_data, mtry = 20)
varImpPlot(killings_rf, n.var = 20) #it looks like police force size, average household size, and the proportion of all police that live in the city are important




