# Load packages
library(tidyverse)
library(interactions)

# File Paths
data_path <- "D:/OneDrive - University of Warwick/data/nielsen_extracts/HMS"
products_path <- "Master_Files/Latest/products.tsv"
purchases_path <- "2011/Annual_Files/purchases_2011.tsv"
trips_path <- "2011/Annual_Files/trips_2011.tsv"
panelists_path <- "2011/Annual_Files/panelists_2011.tsv"
ratings_path <- "D:/PROJECT DATA/ratings_for_msc_students.csv"


####### Products and Ratings Exploration #############

# Load data
products <- read_tsv(file.path(data_path, products_path))
product_ratings <- read_csv(ratings_path)

# Get 10 distinct product_group_descr with highest rating_avg
product_ratings %>% 
  group_by(product_group_descr) %>% 
  summarise(rating_avg = mean(rating_avg, na.rm = TRUE)) %>% 
  arrange(desc(rating_avg)) %>% 
  head(11)

# Get 10 distinct product_group_descr with least rating_avg
product_ratings %>% 
  group_by(product_group_descr) %>% 
  summarise(rating_avg = mean(rating_avg, na.rm = TRUE)) %>% 
  arrange(rating_avg) %>% 
  head(11)

# Get 10 products with highest rating_avg
product_ratings %>% 
  arrange(desc(rating_avg)) %>% 
  head(10)

# Check if upc (product identifier) is unique
products %>% 
  count(upc) %>% 
  filter(n > 1)
## NOTE: 222,357 products have duplicate upc values

# Explore products with duplicate upc values
duplicate_upc <- products %>% 
  filter(upc %in% c("000001075687", "000002000190", "000001075687",
                    "000002000339", "000002000557", "000002001117",
                    "000002001447", "000002001400", "000002001276")) %>%
  arrange(upc)
## NOTE: Products with same upc are basically the same product. They differ
## in their upc_ver_uc value though, which was used to retain earlier versions
## of the same product. Hence we can safely remove duplicates based on upc.

# Count number of unique product_group_code
products %>% 
  count(product_group_code)

# Remove duplicates based on upc
products <- products %>% 
  distinct(upc, .keep_all = TRUE)

# Join products with product_ratings where product_module_descr matches
products <- products %>% 
  left_join(product_ratings, by = c("product_module_descr" = "product_module_descr")) %>% 
  select(upc, upc_ver_uc, upc_descr, product_module_descr, product_group_descr.x,
         product_group_descr.y, department_descr.x, department_descr.y, brand_descr,
         rating_avg, rating_normalized)

# Check if for all instances, product_group_descr.x and product_group_descr.y are the same
products %>% 
  filter(product_group_descr.x != product_group_descr.y) %>%
  count()

# Check if for all instances, department_descr.x and department_descr.y are the same
products %>% 
  filter(department_descr.x != department_descr.y) %>%
  count()
## NOTE: 427 instances where department_descr.x and department_descr.y are different

# Get all instances where department_descr.x and department_descr.y are different
diff <- products %>% 
  filter(department_descr.x != department_descr.y) %>%
  select(department_descr.x, department_descr.y)
## NOTE: The difference comes from a renaming of "DEPT-DRY GROCERY" to "DRY GROCERY"
## Hence we can treat them as the same

# Remove redundant columns and clean up column names
products <- products %>% 
  select(-department_descr.y, -product_group_descr.y) %>% 
  rename(department_descr = department_descr.x, product_group_descr = product_group_descr.x)

# Check if there are products with missing rating_avg
products %>% 
  count(is.na(rating_avg))
## There are 63,642 products with missing rating

# Get products with missing rating_avg
missing_rating <- products %>% 
  filter(is.na(rating_avg)) %>% 
  select(upc, upc_ver_uc, upc_descr, product_module_descr, department_descr, brand_descr)

# Save missing rating products to csv
write_csv(missing_rating, "D:/PROJECT DATA/missing_rating_products.csv")

# Remove products with missing rating_avg
products <- products %>% 
  filter(!is.na(rating_avg))


####### Purchases Exploration #############

purchases <- read_tsv(file.path(data_path, purchases_path))

# Sort purchases by trip_code_uc and upc
purchases <- purchases %>% 
  arrange(trip_code_uc, upc)

# Find instances where a combination trip_code_uc and upc is not unique
purchases %>% 
  count(trip_code_uc, upc) %>% 
  filter(n > 1)


# Join purchases with products where upc matches
purchases <- purchases %>% 
  left_join(products, by = c("upc" = "upc")) %>% 
  select(trip_code_uc, upc, quantity, total_price_paid, product_module_descr,
         product_group_descr, brand_descr, rating_avg, rating_normalized)


# Check if there are purchases with missing rating_avg
purchases %>% 
  count(is.na(rating_avg))
## NOTE: There are 7,736,874 records with missing rating_avg

# Remove purchases with missing rating_avg
purchases <- purchases %>% 
  filter(!is.na(rating_avg))


####### Trips Dataset ############

trips <- read_tsv(file.path(data_path, trips_path))

# Check if there trips with duplicate trip_code_uc
trips %>% 
  count(trip_code_uc) %>% 
  filter(n > 1)

# Join purchases with trips where trip_code_uc matches
purchases <- purchases %>% 
  left_join(trips, by = c("trip_code_uc" = "trip_code_uc")) %>%
  select(trip_code_uc, upc, quantity, total_price_paid, product_module_descr,
         product_group_descr, brand_descr, rating_avg, rating_normalized, household_code,
         purchase_date, total_spent)

# Sort purchases by household_code and trip_code_uc
purchases <- purchases %>% 
  arrange(household_code, trip_code_uc)

####### Household-Category Expenses Calculation #############

# Group purchases by household_code and product_group_descr
# Get total spent on each product group, rating of category and total spent by household
household_category_expenses <- purchases %>% 
  group_by(household_code, product_group_descr) %>% 
  summarise(category_spend = sum(total_price_paid),
            rating_avg = mean(rating_avg)) %>% 
  group_by(household_code) %>% 
  mutate(total_annual_spend = sum(category_spend))

# Save household_category_expenses to csv
write_csv(household_category_expenses, "D:/PROJECT DATA/household_category_expenses_2011.csv")

# Test: Get annual spend for household using purchases
purchases %>% 
  filter(household_code == 8538550) %>% 
  summarise(total_annual_spend = sum(total_price_paid))

####### Conspicuous Consumption Calculation #############

# Calculate conspicuous consumption for each household
conspicuous_consumption <- household_category_expenses %>%
  group_by(household_code) %>%
  summarise(
    conspicuous_consumption = sum((category_spend / total_annual_spend) * rating_avg)
  )

# Save conspicuous_consumption to csv
write_csv(conspicuous_consumption, "D:/PROJECT DATA/household_conspicuous_consumption_2011.csv")

conspicuous_consumption <- read_csv("D:/PROJECT DATA/household_conspicuous_consumption_2011.csv")

####### Panelists Dataset #############

panelists <- read_tsv(file.path(data_path, panelists_path))

# Join conspicuous_purchases_total with panelists
conspicuous_consumption <- conspicuous_consumption %>% 
  left_join(panelists, by = c("household_code" = "Household_Cd")) %>%
  select(household_code, conspicuous_consumption, Household_Income, 
         Household_Size, Household_Composition,
         Male_Head_Age, Female_Head_Age,
         Male_Head_Education, Female_Head_Education,
         Male_Head_Employment, Female_Head_Employment, 
         Marital_Status, Race, Panelist_ZipCd, Fips_State_Cd, Fips_County_Cd)

# Create a new column FIPS_Code that combines Fips_State_Cd and Fips_County_Cd
# while ensuring Fips_State_Cd is 2 digits and Fips_County_Cd is 3 digits
conspicuous_consumption <- conspicuous_consumption %>% 
  mutate(FIPS_Code = str_pad(Fips_State_Cd, 2, pad = "0") %>% 
           paste0(str_pad(Fips_County_Cd, 3, pad = "0")))


####### Gini Coefficient Dataset #############

# Load gini data
gini <- read_csv("D:/PROJECT DATA/gini.csv")
gini_zip <- read_csv("D:/PROJECT DATA/gini_zip_2011.csv")

# E.g: Get Gini data where FIPS Code is 36029
gini %>% 
  filter(`FIPS Code` == 36029) %>% 
  select(`FIPS Code`, `State`, `Area`, "2011")
## There are 3 records. Two point to the same location with same Gini value.
## One points to a different location with a different Gini value.

# Remove instances where FIPS code is more than 5 characters
gini <- gini %>% 
  filter(nchar(`FIPS Code`) <= 5)

# Find instances where 2011 Gini value is missing
gini %>% 
  count(is.na(`2011`))
## NOTE: 3,934 records have missing Gini value for 2011

# Remove records with missing Gini value
gini <- gini %>% 
  filter(!is.na(`2011`))

# Check if the FIPS Code is unique
gini %>% 
  count(`FIPS Code`) %>% 
  filter(n > 1)

# For records with duplicate FIPS Code, only keep the first record
gini <- gini %>% 
  distinct(`FIPS Code`, .keep_all = TRUE)


# Join conspicuous_purchases_total with gini data
conspicuous_consumption <- conspicuous_consumption %>% 
  left_join(gini, by = c("FIPS_Code" = "FIPS Code")) %>%
  select(household_code, conspicuous_consumption, Household_Income, 
         Household_Size, Household_Composition,
         Male_Head_Age, Female_Head_Age,
         Male_Head_Education, Female_Head_Education,
         Male_Head_Employment, Female_Head_Employment, Race, Marital_Status, 
         Fips_State_Cd, Fips_County_Cd, FIPS_Code, Panelist_ZipCd, "2011", State, Area)

# Rename 2011 column to Gini
conspicuous_consumption <- conspicuous_consumption %>% 
  rename(Gini = `2011`)

# Save conspicuous_consumption to csv
write_csv(conspicuous_consumption, "D:/PROJECT DATA/conspicuous_consumption_household_gini_2011.csv")

# Get records with missing Gini value
conspicuous_consumption %>% 
  filter(is.na(Gini))

####### Gini Coefficient Dataset 2 Test #############

# Create new column Zip for gini_zip that removes the first 6 chr from Area Name
gini_zip <- gini_zip %>% 
  mutate(Zip = str_sub(`Area Name`, 7))

# Check if there are any missing values in Zip
gini_zip %>% 
  count(is.na(Zip))

# Check if there are any duplicate Zip values
gini_zip %>% 
  count(Zip) %>% 
  filter(n > 1)

# Rename Gini column to Gini2 in gini_zip
gini_zip <- gini_zip %>% 
  rename(Gini2 = Gini)

# Join conspicuous_consumption with gini_zip where Zip matches Panelist_ZipCd
conspicuous_consumption <- conspicuous_consumption %>% 
  left_join(gini_zip, by = c("Panelist_ZipCd" = "Zip")) %>%
  select(household_code, conspicuous_consumption, Household_Income, 
         Household_Size, Household_Composition,
         Male_Head_Age, Female_Head_Age,
         Male_Head_Education, Female_Head_Education,
         Male_Head_Employment, Female_Head_Employment, Race, Marital_Status, 
         Fips_State_Cd, Fips_County_Cd, FIPS_Code, Panelist_ZipCd, Gini, State, 
         Area, Gini2)

# Check if there are any missing values in Gini2
conspicuous_consumption %>% 
  count(is.na(Gini2))

########## County Income Dataset #############

# Load county income data
county_income <- read_csv("D:/PROJECT DATA/2011_census_income_data.csv")

# Join conspicuous_consumption with county_income by state and county FIPS code
conspicuous_consumption <- conspicuous_consumption %>% 
  left_join(county_income, by = c("Fips_State_Cd" = "State FIPS", "Fips_County_Cd" = "County FIPS")) %>%
  select(household_code, conspicuous_consumption, Household_Income, 
         Household_Size, Household_Composition,
         Male_Head_Age, Female_Head_Age,
         Male_Head_Education, Female_Head_Education,
         Male_Head_Employment, Female_Head_Employment, Race, Marital_Status, 
         Fips_State_Cd, Fips_County_Cd, FIPS_Code, Gini, State, Area, 
         "Median Household Income")

# Rename Median Household Income to County_Median_Income
conspicuous_consumption <- conspicuous_consumption %>% 
  rename(County_Median_Income = `Median Household Income`)

# Create a new column Household_Income_Estimate that estimates the household income
# based on the upper limit of the Household_Income category
conspicuous_consumption <- conspicuous_consumption %>% 
  mutate(Household_Income_Estimate = case_when(
    Household_Income == 3 ~ 4999,
    Household_Income == 4 ~ 7999,
    Household_Income == 6 ~ 9999,
    Household_Income == 8 ~ 11999,
    Household_Income == 10 ~ 14999,
    Household_Income == 11 ~ 19999,
    Household_Income == 13 ~ 24999,
    Household_Income == 15 ~ 29999,
    Household_Income == 16 ~ 34999,
    Household_Income == 17 ~ 39999,
    Household_Income == 18 ~ 44999,
    Household_Income == 19 ~ 49999,
    Household_Income == 21 ~ 59999,
    Household_Income == 23 ~ 69999,
    Household_Income == 26 ~ 99999,
    Household_Income == 27 ~ 100000,
  ))

# Convert County_Median_Income to numeric by removing commas
conspicuous_consumption <- conspicuous_consumption %>% 
  mutate(County_Median_Income = as.numeric(gsub(",", "", County_Median_Income)))

# Create a new column Household_Income_Rank that ranks the household as 1 (low)
# if the estimated income is less than the median income, and 2 (high) otherwise
conspicuous_consumption <- conspicuous_consumption %>% 
  mutate(Household_Income_Rank = case_when(
    Household_Income_Estimate < County_Median_Income ~ "Low-Income",
    TRUE ~ "High-Income"
  ))

# Transform Household_Income_Rank into a factor
conspicuous_consumption <- conspicuous_consumption %>% 
  mutate(Household_Income_Rank = factor(Household_Income_Rank, 
                                        levels = c(1, 2), 
                                        labels = c("Low-Income", "High-Income")))

# Check if there are any missing Household_Income_Rank values
conspicuous_consumption %>% 
  count(is.na(Household_Income_Rank))

# Save conspicuous_consumption to csv
write_csv(conspicuous_consumption, "D:/PROJECT DATA/conspicuous_consumption_household_gini_income_rank_2011.csv")



# Create a new column Household_Age that averages Male_Head_Age and Female_Head_Age.
# If either is 0 (missing), the other is used.
conspicuous_consumption <- conspicuous_consumption %>% 
  mutate(Household_Age = ifelse(Male_Head_Age == 0, Female_Head_Age,
                                ifelse(Female_Head_Age == 0, Male_Head_Age,
                                       (Male_Head_Age + Female_Head_Age) / 2)))

# Check if there are any missing Household_Age values
conspicuous_consumption %>% 
  count(is.na(Household_Age))

# Create a new column Household_College that checks if either head has a college degree
# Note: 4-6 indicates some college
conspicuous_consumption <- conspicuous_consumption %>% 
  mutate(Household_College = ifelse(Male_Head_Education >= 4 | 
                                      Female_Head_Education >= 4, 1, 0))

# Check if there are any missing Household_College values
conspicuous_consumption %>% 
  count(is.na(Household_College))

# Create a new column Household_Employed that checks if either head is employed
# Note: 1-3 indicates some level of employment
conspicuous_consumption <- conspicuous_consumption %>% 
  mutate(Household_Employed = ifelse(Male_Head_Employment >= 1 & Male_Head_Employment <= 3 |
                                       Female_Head_Employment >= 1 & Female_Head_Employment <= 3,
                                     1, 0))

# Check if there are any missing Household_Employed values
conspicuous_consumption %>% 
  count(is.na(Household_Employed))

# Create a new column County_Median_Income_Standardized that standardizes 
# the County_Median_Income variable
conspicuous_consumption <- conspicuous_consumption %>% 
  mutate(County_Median_Income_Std = (
    County_Median_Income - mean(County_Median_Income, na.rm = TRUE)) / 
      sd(County_Median_Income, na.rm = TRUE))

# Save conspicuous_consumption to csv
write_csv(conspicuous_consumption, "D:/PROJECT DATA/conspicuous_consumption_household_gini_income_rank_2011_2.csv")

conspicuous_consumption <- read_csv("D:/PROJECT DATA/conspicuous_consumption_household_gini_income_rank_2011_2.csv")


################ MODELING ################

# Linear model to predict conspicuous consumption using gini, household income, 
# and other household characteristics
model1 <- lm(conspicuous_consumption ~ Gini + Household_Income + Household_Age + 
               Household_College + Household_Employed + Race + Household_Size +
               County_Median_Income_Std, 
             data = conspicuous_consumption)
summary(model1)

# Linear model to predict conspicuous consumption using gini, income rank,
# and other household characteristics
model2 <- lm(conspicuous_consumption ~ Gini * Household_Income_Rank + Household_Age + 
               Household_College + Household_Employed + Race + Household_Size +
               County_Median_Income_Std,
             data = conspicuous_consumption)
summary(model2)

## Follow-up analysis for the interaction term

# Visualize the interaction effect
interact_plot(model2, pred = Gini, modx = Household_Income_Rank)

# Run simple slope analysis
sim_slopes(model2, pred = Gini, modx = Household_Income_Rank)


# Improve the interaction plot
plot <- interact_plot(model2, pred = Gini, modx = Household_Income_Rank, 
                   int.width = 0.95, # Confidence interval width
                   line.size = 1.2)    # Width of lines

# Customize the plot
plot + 
  theme_minimal(base_size = 13) + 
  labs(x = "County-level income inequality (Gini coefficient)",
       y = "Conspicuous Consumption") + 
  scale_color_brewer(palette = "Set1") +  # Color palette
  theme(legend.position = "top",
        panel.grid.major = element_line(color = "grey80"),  # Major grid lines
        panel.grid.minor = element_blank()) # Minor grid lines

