################ Data Wrangling #################################################

setwd("Your/directory/path") # Set working directory 

iris_data <- read.csv("iris.csv", header = TRUE) # Load the dataset

#######################--------Selecting Date------#########################

###using square brackets []

cleaned_iris_data[1:3,] # selects the first three rows

cleaned_iris_data[,2] # selects all rows of the second

cleaned_iris_data[1:3,2:4] # selects rows 1, 2, and 3 and columns 2, 3, and 4

#selects rows where the column named "column_name" has the value name "setosa"
cleaned_iris_data[cleaned_iris_data$Species == "setosa",]

cleaned_iris_data[, "Species"] #select column by name Species

##select multiple columns by name Petal.Length and Species
cleaned_iris_data[, c("Petal.Length", "Species")]

cleaned_iris_data$Sepal.Width # select a single column by name


########## using the subset() function

setwd("Your/directory/path")

HINTSData <- read.csv("HINTSData.csv", header = TRUE) # Load the dataset

## OR
HINTSData<-read.csv(choose.files(),header = T) # import the dataset

names(HINTSData) # get the names of all columns of the data

# Create a dataframe and 
# select rows where age is less than 30 and only keeps the "Age" and "BirthGender" columns. 

HINTSData_subset <- subset(HINTSData,Age < 30, select = c("Age", "BirthGender"))

head(HINTSData_subset) # printout the first six rows of the subset data


############# ---Using dplyr from tidyverse package

install.packages("tidyverse") #installing the tidyverse package
library(dplyr) # loading the package

# Select "Age" and "BirthGender" columns.
HINTSData_subset <- HINTSData %>% select("Age", "BirthGender")

# select columns 2 and 4 (i.e., "PersonID" and  "HealthInsurance")
HINTSData_subset <- HINTSData %>% select(2, 4)

#Exclude columns 1 to 3
HINTSData_subset <- HINTSData %>% select(-c(1:3))

# selects rows where age is greater than 25 and gender is "1"
HINTSData_subset <- HINTSData %>% filter(Age > 25 & BirthGender == "1")


##############---- Cleaning the HINTS Data ------##########################################

HINTSData<-read.csv(choose.files(), header = T)  #####Use HINTS Data
names(HINTSData)
head(HINTSData)
summary(HINTSData)

attach(HINTSData) # attaching the data allows you to use the variable in the data without using $ each time you need the variable

HINTSData_new<-HINTSData[BirthGender > 0,] # select data of all gender values, excluding the negatives

summary(HINTSData_new)

# excluding the negatives or unwanted values of multiple variables in the same data frame

HINTSData_new <- HINTSData[c(QualityCare>0 & HealthInsurance>0 & Age>0 & BirthGender>0 & FullTimeOcc_Cat>0 
                             & BMI>=0 & smokeStat>0 & WeeklyMinutesModerateExercise>=0 & AvgDrinksPerWeek>=0),]

summary(HINTSData_new)
attach(HINTSData_new)


################ ----Recoding QaulityCare into new variable------##########

HINTSData_new$QualityCare_New[HINTSData_new$QualityCare == 1] <- "Excellent"
HINTSData_new$QualityCare_New[HINTSData_new$QualityCare == 2] <- "Very Good"
HINTSData_new$QualityCare_New[HINTSData_new$QualityCare == 3] <- "Good"
HINTSData_new$QualityCare_New[HINTSData_new$QualityCare == 4] <- "Fair"
HINTSData_new$QualityCare_New[HINTSData_new$QualityCare == 5] <- "Poor"

summary(factor(HINTSData_new$QualityCare_New))


####################################---NOTE----###############################################
#Choosing the right method:
#  --Base R brackets are versatile and fundamental, offering fine-grained control, 
      #particularly useful for custom logic or non-standard operations.
#--The subset() function is more intuitive for beginners or when dealing with 
    #smaller datasets and simple conditions.
#--The dplyr package is excellent for creating readable and scalable workflows, 
#especially for larger datasets and complex data manipulation tasks, offering 
#powerful functions and the pipe operator (%>%) for chaining operations efficiently. 

##############################################################################################


#########################----Merging data--##################################################

########## Merging columns using the cbind() function
#NOTE: To merge columns, ensure that the data rows match or have the same ID

HINTSData_1 <- HINTSData[,2:4] #select data columns 2 to 4

HINTSData_2 <- HINTSData[,15:17] # select data columns 15 to 17

# merge the data columns 2 to 4 and columns 15 to 17
mergeCol_HINTSData12 <- cbind(HINTSData_1, HINTSData_2)

head(mergeCol_HINTSData12)

############## Merging rows using the rbind() function
#NOTE: To merge rows, ensure that the data columns match the same name

#select first 50 rows of columns 2 to 4
HINTSData_3 <- HINTSData[1:50,2:4] 

#select first 500 to 650 rows of columns 2 to 4
HINTSData_4 <- HINTSData[500:650, 2:4] 

# merge the data from the rows of dataframe HINTSData_3 and HINTSData_4
mergeRow_HINTSData34 <- rbind(HINTSData_3, HINTSData_4)

dim(mergeRow_HINTSData34) #print the number of row by columns

names(mergeRow_HINTSData34) #print the columns names


############### Using the merge() function

HINTSData_5 <- HINTSData[,2:4] #select data columns 2 to 4
head(HINTSData_5) #print the firs 6 rows

HINTSData_6 <- HINTSData[,c(2,15:17)] # select data columns 2 and 15 to 17
head(HINTSData_6) #print the firs 6 rows

# marge data HINTSData_5 and HINTSData_6 by = "PersonID"
mergeCol_byID_HINTSData56 <- merge(HINTSData_5, HINTSData_6, by = "PersonID")
head(mergeCol_byID_HINTSData56) #print the first 6 rows



############### Using the dplyr package from tidyverse package

HINTSData_1 <- HINTSData %>% select("PersonID", "Age") # select "PersonID" and "Age"

HINTSData_2 <- HINTSData %>% select("PersonID", "BirthGender") # "PersonID" and "BirthGender"

# merge HINTSData_1 and HINTSData_2 by PersonID
merge_HINTSData12 <- left_join(HINTSData_1, HINTSData_2, by = "PersonID") 

head(merge_HINTSData12)

dim(merge_HINTSData12)

####################################---Sorting data----#################################################

#Create dataframe with 5 rows and 3 columns

df <- data.frame(id=c(2,1,3,4,5), name=c('sravan','jau','chrisa','shivgami','ram'), gender=c('f','m','m','f','m'))
print(df)

#Example 1 - Sort the dataframe by gender and id columns
df2 <- df[order(df$gender, df$id), ]

#Example 2 - Sort the dataframe by gender and id columns
df2 <- df[with(df, order(gender, id)), ]

#Example 3 -  Use the dplyr package
library("dplyr") 
df2 <- arrange(df, gender, id)

#Example 4 - Sort by descending order
library("dplyr") 
df2 <- arrange(df, desc(gender), desc(id) )

#Example 5 - Use the data.table package
library("data.table") 
df2 <- setorder(df, gender, id)


####################################---Reshaping data----#################################################

### Example (using pivot_longer)

# Imagine you have a dataset showing the scores of students on three tests: 

student_data_wide <- data.frame(
  StudentID = c(1, 2, 3),
  Test1 = c(85, 92, 78),
  Test2 = c(90, 88, 85),
  Test3 = c(75, 95, 80)
)

# To reshape this into a long format for easier analysis with test scores in a single column: 

library(tidyr) # loading tidyr package

student_data_long <- student_data_wide %>%
  pivot_longer(cols = starts_with("Test"), # Selects columns starting with "Test"
               names_to = "TestName",     # New column for test names
               values_to = "Score")       # New column for scores

print(student_data_long)

#####---Import My_Pract_data text data and reshaping into long using the "day"

txt.filedata<-read.table("H:/My Drive/Teaching Courses/PUBH 422/My_Pract_data.txt",header=T)

names(txt.filedata)

txt.filedata_long <- txt.filedata %>%
  pivot_longer(cols = starts_with("day"), # Selects columns starting with "Test"
               names_to = "DayName",     # New column for test names
               values_to = "Score")       # New column for scores

print(txt.filedata_long)


##############################################---NOTE---#########################################################
#Other functions and approaches
#---Base R reshape() function: While pivot_longer() and pivot_wider() are generally preferred for their ease of use, 
#The University of Virginia Library notes that base R also provides the reshape() function. This function offers 
#flexibility but can have a steeper learning curve.
#---reshape2 package: The melt() function from the reshape2 package can be used to convert wide data to long format.
#---cbind() and rbind(): These functions are used for joining vectors, matrices, or data frames by columns or rows, 
    #which can also be a form of data reshaping.
#---merge(): The merge() function can be used to combine two data frames based on common column names. 

################################################################################################################


####################################---Handling Missing and Messy data----#################################################


#############---Detecting Missing Data---#####

### is.na(): Use is.na() to identify missing values. 

x <- c(1, 2, NA, 4)
is.na(x) 
# Output: [1] FALSE FALSE TRUE FALSE 

### sum(is.na()): Count the total number of missing values.
sum(is.na(df))

### colSums(is.na()): Count missing values in each column.
colSums(is.na(df))

### complete.cases(): Identify rows with complete observations (no missing values).

complete.cases(df) 
# Output: [1] TRUE FALSE FALSE FALSE (if df has missing values)


########## Using the iris data 

setwd("H:/My Drive/Teaching Courses/PUBH 422/Datasets") # Set working directory

iris_data <- read.csv("iris.csv", header = TRUE) # Load the iris dataset

is.na(iris_data) # identifying the missing values, NA’s

sum(is.na(iris_data)) # sum the NA’s



##############----Handling Missing Data---############

####---METHOD 1: Removing missing data using the na.omit() function 

# Syntax: na.omit(dataframe_with_na)

cleaned_iris_data <- na.omit(iris_data) # Remove rows with NA values


### Filtering with complete.cases(): You can explicitly filter rows to include only complete cases.

#Syntax: df_complete <- df[complete.cases(df),]

iris_complete <- iris_data[complete.cases(iris_data),]

summary(iris_complete)

dim(iris_complete)



#### drop_na() (from tidyr package): This tidyverse function provides a convenient way to remove rows with 
#### missing values, either from all columns or specific ones.

# Remove rows with NA from all columns
# Syntax: df %>% drop_na()

cleaned_iris_data1 <- iris_data %>% drop_na() # remove all NA's from the data

summary(cleaned_iris_data1)

# Remove rows with NA from specific columns
# Syntax: df %>% drop_na(column1, column2)

cleaned_iris_data2 <- iris_data %>% drop_na(1,2) # remove the NA's of only column 1 and 2

summary(cleaned_iris_data2)


####---METHOD 2:  Imputing missing data

### Mean/Median Imputation: Replace missing values with the mean or median of the respective column.

# Syntax: vector_with_na[is.na(vector_with_na)] <- mean(vector_with_na, na.rm = TRUE)

# Replace the missing data of Sepal.Length with the mean of Sepal.Leangth in the iris data

iris_data$Sepal.Length[is.na(iris_data$Sepal.Length)] <- mean(iris_data$Sepal.Length, na.rm = TRUE)

### Multiple Imputation

install.packages("mice") # install the package mice
library(mice) # load the package

imputed_iris_data <- mice(iris_data, m=5, method='pmm')

new_iris <- complete(imputed_iris_data, 1) # obtaining the imputed data

summary(new_iris) # print statistical summary

dim(new_iris) # print data dimension 

# Distribution of oberserved/imputed values

xyplot(imputed_iris_data, Sepal.Length ~ Sepal.Width | .imp, pch = 20, cex=1.4)


#### The na.rm argument ignores the NA's in the variable and computes the function

sum(iris_data$Petal.Length, na.rm = TRUE) # calculate the sum

mean(iris_data$Petal.Length, na.rm = TRUE) # calculate the mean

sd(iris_data$Petal.Length, na.rm = TRUE) # calculate the standard deviation


#######################################---NOTE---####################################################################

##### ---Common data cleaning techniques--#####
        #Cleaning column names: Use clean_names() from janitor to standardize column names.
### ---Handling missing values:
#--Identify missing values with is.na().
#--Count missing values using sum(is.na(df)) or colSums(is.na(df)).
#--Remove rows or columns with missing values using na.omit() or dplyr functions.
#--Impute missing values (e.g., replace with mean, median, or mode).
#--Interpolate missing values for time series data using na.approx() from the zoo package.
### --Reshaping data: Use pivot_longer() and pivot_wider() from tidyr to restructure data from wide 
      #to long and vice versa.
### --Removing duplicates: Use distinct() from dplyr to keep only unique rows. The get_dupes() function 
      #from janitor is also useful for isolating and removing duplicate records.
### --Standardizing data formats: Ensure consistent data types, capitalization, and removing unnecessary 
      #formatting or special characters.
### --Addressing outliers: Use summary statistics and visualizations (e.g., boxplots) to identify and 
      #handle outliers, either by removing or transforming them.
### --Combining data: Use functions like bind_rows() from dplyr to combine multiple datasets or files. 

### ---Reproducibility and best practices --####
### --Reproducible Workflows: Documenting your data cleaning steps (e.g., using R Markdown) is crucial 
      #for reproducibility and ensuring that others (or your future self) can replicate your analysis.
### --Version Control: Utilize Git and GitHub to track changes to your code and data cleaning scripts, 	
      #facilitating collaboration and version control.
### --Project Organization: Maintain a structured project directory with meaningful subdirectories 
      #for raw data, cleaned data, scripts, and output.
### --Data Dictionary: Create a data dictionary or metadata to understand the variables and their 
      #meaning, which is crucial for effective data cleaning. 

################################################################################################################################


#######---Using Tidyvers package for data wrangling---#####

# Load the necessary library
library(dplyr)

# Load the mtcars dataset
data <- mtcars

# Display the first few rows of the dataset
head(data)

###--- Selecting Columns

# Select specific columns: mpg, cyl, and hp
selected_data <- data %>%
  select(mpg, cyl, hp)

# Display the first few rows of the selected data
head(selected_data)

###---  Filtering Rows

# Filter rows where the number of cylinders (cyl) is 6
filtered_data <- data %>%
  filter(cyl == 6)

# Display the first few rows of the filtered data
head(filtered_data)

###--- Creating New Columns with mutate


# Create a new column 'hp_per_cyl' that is the horsepower divided by the number
#of cylinders
mutated_data <- data %>%
  mutate(hp_per_cyl = hp / cyl)

# Display the first few rows of the mutated data
head(mutated_data)


###--- Summarizing Data with summarize

# Summarize data to find the mean mpg and total horsepower
summary_data <- data %>%
  summarize(mean_mpg = mean(mpg), total_hp = sum(hp))

# Display the summary data
summary_data

###--- Arranging Rows
# Arrange rows by horsepower in descending order
arranged_data <- data %>%
  arrange(desc(hp))

# Display the first few rows of the arranged data
head(arranged_data)


##########################################----NOTE--- ###########################################################

######--- Other key packages from tidyverse

###--- tidyr: Focuses on tidying and reshaping data. Key functions include:
###--- pivot_longer(): Reshaping data from a wide format to a long format (increasing rows, decreasing columns).
###--- pivot_wider(): Reshaping data from a long format to a wide format (decreasing rows, increasing columns).
###--- readr: Provides fast and friendly functions for reading various data formats into R, such as CSV and delimited files.
###--- tibble: Offers a modernized version of data frames, with improved printing and subsetting behaviors

##################################################################################################################




