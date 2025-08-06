####### R/RStudio Statistical Summaries #################

#####################################################################################
Specific statistics: built-in functions
For more specific descriptive statistics, R offers individual functions like 
mean(), median(), sd() (standard deviation), var() (variance), min(), max(), range(), 
and quantile(). 

########################################################################################

# Load a sample dataset (e.g., mtcars)
data("mtcars")

# Calculate the mean of a variable
mean(mtcars$mpg, na.rm = TRUE) # na.rm = TRUE handles missing values

# Calculate the median of a variable
median(mtcars$mpg, na.rm = TRUE)

# Calculate the standard deviation of a variable
sd(mtcars$mpg, na.rm = TRUE)

# Calculate the variance of a variable
var(mtcars$mpg, na.rm = TRUE)

# Calculate the minimum value of a variable
min(mtcars$mpg, na.rm = TRUE)

# Calculate the maximum value of a variable
max(mtcars$mpg, na.rm = TRUE)

# Calculate the range of a variable
range(mtcars$mpg, na.rm = TRUE)

# Calculate quantiles (e.g., 25th, 50th, 75th percentiles)
quantile(mtcars$mpg, na.rm = TRUE)

# Calculate a specific percentile (e.g., 95th percentile)
quantile(mtcars$mpg, 0.95, na.rm = TRUE)


#####################################################################################
Summarizing by groups
You can calculate summary statistics for subsets of your data based on one or more 
grouping variables using the group_by() and summarise() functions from the dplyr package. 
########################################################################################

# Load the dplyr package
library(dplyr)

# Group by 'cyl' (number of cylinders) and calculate the mean and median of 'mpg'
mtcars %>% 
  group_by(cyl) %>% 
  summarise(
    mean_mpg = mean(mpg), 
    median_mpg = median(mpg),
    sd_mpg = sd(mpg)
  )


##############################################################################################

Enhanced summaries with packages
External R packages offer more specialized and customizable statistical summaries:
  
  psych package: The describe() function provides a comprehensive summary 
including skewness, kurtosis, and interquartile range (IQR).

skimr package: The skim() function offers a customizable and concise summary 
tailored for different variable types.

fBasics package: The basicStats() function provides mean, standard deviation,
median, minimum, maximum, and sample size. 

##############################################################################################

# Install and load the psych package
install.packages("psych")
library(psych)

# Use the describe() function for a comprehensive summary
describe(mtcars)

# Install and load the skimr package
install.packages("skimr")
library(skimr)

# Use the skim() function for a customizable summary
skim(mtcars)

# Use skim() with grouped data
mtcars %>% 
  group_by(cyl) %>% 
  skim()

# Install and load the fBasics package
install.packages("fBasics")
library(fBasics)

# Use basicStats() for a summary table
basicStats(mtcars)



