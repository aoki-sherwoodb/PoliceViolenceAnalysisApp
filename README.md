# final-project-ben-avery

## https://aokisherwoodb.shinyapps.io/demographics_police_violence/

## Files in our Repo:
- `app.R`: contains all code for our shiny app.
- `data_preprocessing.R`: contains all code for our data manipulation and preparation. We joined several datasets to get our data on U.S. cities.
- `data/`: contains the datasets used in our R files.
- `www/`: contains an image for our homepage

## Technical Report

We started with 5 datasets: city coordinates, city demographics, hate crimes, police residence, and police killings. To process this data into a single big table, we:
- loaded them from csv using readr tools
- tidied and joined them into our master joined_cities dataset using the dplyr and tidyr tools we learned in class (mutating, joining, pivoting, etc) as well as a few that we didn't learn in class
- in this process, we also used tools we didn't learn in class. We used rename() to standardize the column names of the data, since there was a lot of variation across the data sources. We also used coalesce() to convert NAs into default values.

For the prediction portion of our app, we used the randomForest package to fit a random forest model to our data and determine which variables were most influential in determining whether or not there was a police killing. With that knowledge in hand, we selected 3 variables (police residence rate, total population, and police force size) and fit a smaller RF model to only those variables, allowing the user to input values for each of them and predict whether a city with those statistics would have had a police killing.

For each category of visualization that we included (police residency vs police killings, demographics vs police killings), we used a combination of the standard ggplot graphs that we learned in class and maps generated using the Leaflet package. 

