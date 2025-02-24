#PSYC 259 Homework 4 - Writing functions
#Optional, for extra credit if all questions are answered

#List names of students collaborating with: 

### SETUP: RUN THIS BEFORE STARTING ----------
library(tidyverse)
set.seed(1)
id <- rep("name", 30)
x <- runif(30, 0, 10)
y <- runif(30, 0, 10)
z <- runif(30, 0, 10)
ds <- tibble(id, x, y, z)

### Question 1 ---------- 

#Vectors x, y, and z contain random numbers between 1 and 10. 
#Write a function called "limit_replace" that will replace values less than 2 or greater than 8 with NA
#Then, run the function on x and save the results to a new vector "x_replace" to show it worked

# Define function
limit_replace <- function(vec) {
  vec <- ifelse(vec < 2 | vec > 8, NA, vec)
  return(vec)
}

ds <- ds %>%
  mutate(across(c(x, y, z), limit_replace, .names = "{.col}_replace"))

print(ds)

### Question 2 ---------- 

#Make a new version of limit_replace that asks for arguments for a lower bounary and an upper boundary
  #so that they can be customized (instead of being hard coded as 2 and 8)
#Run the function on vector y with boundaries 4 and 6, saving the results to a new vector "y_replace"

# Define function
limit_replace <- function(vec, lower, upper) {
  vec <- ifelse(vec < lower | vec > upper, NA, vec)
  return(vec)
}

# Apply function
y_replace <- limit_replace(ds$y, lower = 4, upper = 6)
print(ds$y)
print(y_replace)

### Question 3 ----------

#Write a function called "plus_minus_SD" that can take one of the vectors (x/y/z) as input
  #and "num_of_SDs" as an input and returns the boundaries +/- num_of_SDs around the mean. 
#plus_minus_SD(x, 1) would give +/- 1SD around the mean of x, plus_minus_SD(y, 2) would give +/- 2SDs around the mean 
#Make num_of_SDs default to 1
#run the new function on x, y, and z with 1 SD

# Define function
plus_minus_SD <- function(vec, num_of_SDs = 1) {
  mean_val <- mean(vec, na.rm = T)
  sd_val <- sd(vec, na.rm = T)
  lower_bound <- mean_val - (num_of_SDs * sd_val)
  upper_bound <- mean_val + (num_of_SDs * sd_val)
  return(c(lower_bound, upper_bound))
}

# Run the function
x_bounds <- plus_minus_SD(ds$x, 1)
y_bounds <- plus_minus_SD(ds$y, 1)
z_bounds <- plus_minus_SD(ds$z, 1)
print(x_bounds)
print(y_bounds)
print(z_bounds)

### Question 4 ----------

#Write an another new version of limit_replace
#This time, make the upper and lower boundaries optional arguments
#If they are not given, use +/- 1 SD as the boundaries (from your plus_minus_SD function)
#Apply the function to each column in ds, and save the results to a new tibble called "ds_replace"

# Define the new version of limit_replace
limit_replace <- function(vec, lower = NULL, upper = NULL) {
  if (is.null(lower) | is.null(upper)) {
    bounds <- plus_minus_SD(vec, 1)  
    lower <- ifelse(is.null(lower), bounds[1], lower)
    upper <- ifelse(is.null(upper), bounds[2], upper)
  }
  
  vec <- ifelse(vec < lower | vec > upper, NA, vec)
  return(vec)
}

# Apply function
ds_replace <- ds %>%
  mutate(across(where(is.numeric), limit_replace))
print(ds_replace)

### Question 5 ----------

#Add a "stopifnot" command to your limit_replace function to make sure it only runs on numeric variables
#Try running it on a non-numeric input (like "id") to make sure it gives you an error

# Add stopifnot
limit_replace <- function(vec, lower = NULL, upper = NULL) {
  stopifnot(is.numeric(vec))
  if (is.null(lower) | is.null(upper)) {
    bounds <- plus_minus_SD(vec, 1)  # Default to 1 SD
    lower <- ifelse(is.null(lower), bounds[1], lower)
    upper <- ifelse(is.null(upper), bounds[2], upper)
  }
  vec <- ifelse(vec < lower | vec > upper, NA, vec)
  return(vec)
}

ds_replace <- ds %>%
  mutate(across(where(is.numeric), limit_replace))

# Test case
try(limit_replace(ds$id)) 
print(ds_replace)

### Question 6 ----------

#What other requirements on the input do you need to make the function work correctly?
#Add another stopifnot to enforce one more requirement

# Make sure there is at least one non-missing value
# stopifnot(sum(!is.na(vec)) > 0)
limit_replace <- function(vec, lower = NULL, upper = NULL) {
  stopifnot(is.numeric(vec))
  stopifnot(sum(!is.na(vec)) > 0)
  if (is.null(lower) | is.null(upper)) {
    bounds <- plus_minus_SD(vec, 1)  
    lower <- ifelse(is.null(lower), bounds[1], lower)
    upper <- ifelse(is.null(upper), bounds[2], upper)
  }
  vec <- ifelse(vec < lower | vec > upper, NA, vec)
  return(vec)
}

# Apply the function 
ds_replace <- ds %>%
  mutate(across(where(is.numeric), limit_replace))

# Test cases
try(limit_replace(ds$id))  
empty_vec <- rep(NA, 10)
try(limit_replace(empty_vec)) 
print(ds_replace)

### Question 7 ----------

#Clear out your workspace and load the built-in diamonds dataset by running the lines below
#RUN THIS CODE
rm(list = ls())
library(tidyverse)
ds_diamonds <- diamonds

#Save your two functions to an external file (or files) 
#Then, load your functions from the external files(s)
#Next, run your limit_replace function on all of the numeric columns in the new data set
#and drop any rows with NA, saving it to a new tibble named "ds_trimmed"

# Save plus_minus_SD to an external file
writeLines(c(
  "plus_minus_SD <- function(vec, num_of_SDs = 1) {",
  "  mean_val <- mean(vec, na.rm = TRUE)",
  "  sd_val <- sd(vec, na.rm = TRUE)",
  "  lower_bound <- mean_val - (num_of_SDs * sd_val)",
  "  upper_bound <- mean_val + (num_of_SDs * sd_val)",
  "  return(c(lower_bound, upper_bound))",
  "}"
), "plus_minus_SD.R")

# Save limit_replace to an external file
writeLines(c(
  "limit_replace <- function(vec, lower = NULL, upper = NULL) {",
  "  stopifnot(is.numeric(vec))",  
  "  stopifnot(sum(!is.na(vec)) > 0)",  
  "  if (is.null(lower) | is.null(upper)) {",
  "    bounds <- plus_minus_SD(vec, 1)",
  "    lower <- ifelse(is.null(lower), bounds[1], lower)",
  "    upper <- ifelse(is.null(upper), bounds[2], upper)",
  "  }",
  "  vec <- ifelse(vec < lower | vec > upper, NA, vec)",
  "  return(vec)",
  "}"
), "limit_replace.R")

source("plus_minus_SD.R")
source("limit_replace.r")

# Apply limit_replace
ds_replace <- ds_diamonds %>%
  mutate(across(where(is.numeric), limit_replace))

# Drop NAs
ds_trimmed <- ds_replace %>%
  drop_na()
print(ds_trimmed)

### Question 8 ----------

#The code below makes graphs of diamond price grouped by different variables
#Refactor it to make it more efficient using functions and/or iteration
#Don't worry about the order of the plots, just find a more efficient way to make all 6 plots
#Each cut (Premium/Ideal/Good) should have a plot with trimmed and untrimmed data
#The title of each plot should indicate which cut and whether it's all vs. trimmed data

ds_diamonds %>% filter(cut == "Premium") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Premium, all") + 
  theme_minimal()

ds_diamonds %>% filter(cut == "Ideal") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Ideal, all") +
  theme_minimal()

ds_diamonds %>% filter(cut == "Good") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Good, all") +
  theme_minimal()

ds_trimmed %>% filter(cut == "Premium") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Premium, trimmed") + 
  theme_minimal()

ds_trimmed %>% filter(cut == "Ideal") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Ideal, trimmed") +
  theme_minimal()

ds_trimmed %>% filter(cut == "Good") %>% 
  ggplot(aes(x = clarity, y = price)) + 
  geom_boxplot() + 
  ggtitle("Good, trimmed") +
  theme_minimal()



# Function for the boxplot
plot_diamond_price <- function(data, cut_type, trimmed = FALSE) {
  title_suffix <- ifelse(trimmed, "trimmed", "all")
  ggplot(data %>% filter(cut == cut_type), aes(x = clarity, y = price)) +
    geom_boxplot() +
    ggtitle(paste(cut_type, title_suffix, sep = ", ")) +
    theme_minimal()
}

# Cuts
cut_types <- c("Premium", "Ideal", "Good")

# Plots
plots <- map(cut_types, ~ plot_diamond_price(ds_diamonds, .x, FALSE)) 
plots_trimmed <- map(cut_types, ~ plot_diamond_price(ds_trimmed, .x, TRUE))  

# Combine both lists of plots
all_plots <- c(plots, plots_trimmed)
all_plots

