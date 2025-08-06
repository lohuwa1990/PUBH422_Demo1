############ IMPORTING & EXPORTING DATA ########################

### Import the iris data from your computer

iris_data <- read_csv("H:/My Drive/Teaching Courses/PUBH 422/Datasets/iris.csv") # importing the iris data from the data location directory

str(iris_data) # Displays the structure of the data (types of columns, data frame summary) 

### Alternatively

setwd("H:/My Drive/Teaching Courses/PUBH 422/Datasets") # Set working directory

iris_data <- read.csv("iris.csv", header = TRUE) # Load the iris dataset

is.na(iris_data) # identifying the missing values, NA’s

sum(is.na(iris_data)) # sum the NA’s

cleaned_iris_data <- na.omit(iris_data) # Remove rows with NA values

summary(clean_iris_data) # Check the statistical summary of the iris data 

write_csv(cleaned_iris_data, "cleaned_iris.csv") # Replace with the desired file path

export(cleaned_iris_data, "cleaned_iris.csv") # exporting the data, giving it the name cleaned_iris


##### --------------NOTE--------------- ###########
*** read_csv(): This function, from the readr package, is used to import data from a CSV file. It automatically
detects column types and loads the data into an R data frame.

*** str(): This function provides an overview of the structure of the imported data, showing details such as
column names, data types (numeric, character, factor), and the number of rows and columns.

*** na.omit(): A built-in function in R that removes rows with NA (missing) values from the dataset. This is a
simple data cleaning operation often performed on raw data.

*** write_csv(): This function writes a data frame back into a CSV file. In this example, the cleaned data (with
                                                                                                            missing values removed) is exported to a new file named cleaned_iris.csv.

*** export(): This function exports the data, giving it the name cleaned_iris. Exported data is often stored in the Document of your computer.

*** summary(): This function gives a quick statistical summary of each variable in the dataset (mean, median, min, max, etc.).


############ UNIVERIATE PLOTS ########################


setwd("Your/directory/path") # Set working directory = H:/My Drive/Teaching Courses/PUBH/Datasets

Clean_iris_data <- read.csv(“Cleaned_iris.csv", header = TRUE) # Load the dataset

# Use $ to select species from the iris data and make it a categorical variable using the factor function

cleaned_iris_data$Species <- factor(cleaned_iris_data$Species) 

summary(cleaned_iris_data) # statistical summary of the iris data

plot(cleaned_iris_data$Sepal.Length, main='scatter plot’) # creating a scatterplot of sepal length

hist(cleaned_iris_data$Sepal.Length, main="frequency") # creating a histogram of sepal length

boxplot(cleaned_iris_data$Sepal.Length, main='boxplot’) # creating a boxplot of sepal length

dotchart(cleaned_iris_data$Sepal.Length, main='dot chart’) # creating a dotplot of sepal length

##### Creating a pie chart##########

pie_species<-cleaned_iris_data$Species #select and plot the variable QualityCare

pie_species_table<-table(pie_species) # make a frequency table of the selected variable

labels <- c('setosa', 'vericolor', 'virginica') #add label for each category of QualityCare

# Plot the chart and add labels with rainbow colors

pie(pie_species_table, labels, main = "iris data species pie chart", 
    col = rainbow(length(pie_species_table)))

#add percentage labels
piepercent<- round(100*pie_species_table/sum(pie_species_table), 1)

# Plot the chart.
pie(pie_species_table, labels = piepercent, main = "iris species pie chart",
    col = rainbow(length(pie_species_table)))

#add legend
legend("topright", c('setosa', 'vericolor', 'virginica'), cex = 1,
       fill = rainbow(length(pie_species_table)))

# Saving the image, giving the name iris_species_pie
png(file = "iris_species_pie.jpg")

######## Creating 3D Pie chart#######

install.packages("plotrix") # installing necessary package

library(plotrix) # making use of the package

pie3D(pie_species_table,labels = piepercent, explode = 0.05, main = "Pie Chart of iris species")

legend("topright", c('setosa', 'vericolor', 'virginica'), cex = 1,
       fill = rainbow(length(pie_species_table)))

#####---Creating a Bar Chart---##########

barplot(pie_species_table, names.arg=labels, xlab="Species", ylab="Count", col="blue",
        main="iris species Barchart", border="red")

##### Creating a density curve

x.density <- density(cleaned_iris_data$Sepal.Length) # Creating the density points

plot(x.density, main="density") # Create the density plot

polygon(x.density, col="lightblue", border="black") # Create the density polygon plot

# Use help() to learn more about the syntax and description of a particular function

help("boxplot") # learn about boxplot function in R

#######-----Using ggplot2 R package----####

install.packages("ggplot2")
library(ggplot2)

data("mtcars")

# Histogram of Miles per Gallon (mpg)

ggplot(mtcars, aes(x=mpg)) + 
  geom_histogram(binwidth=2, fill="steelblue", color="black") +
  ggtitle("Histogram of MPG") +
  xlab("Miles per Gallon") +
  ylab("Count")


# Bar plot of Number of Species in the iris data

ggplot(cleaned_iris_data, aes(x=factor(Species))) +
  geom_bar(fill="tomato", color="black") +
  ggtitle("Bar Plot of Species Count") +
  xlab("Number of Species") +
  ylab("Count")

# Line plot of MPG over Index (sequential order of rows) from the mtcars R data

ggplot(mtcars, aes(x=1:nrow(mtcars), y=mpg)) +
  geom_line(color="red") +
  ggtitle("Line Plot of MPG") +
  xlab("Index") +
  ylab("Miles per Gallon")

# Scatter plot of Sepal Length by Species in the iris data

ggplot(cleaned_iris_data, aes(x = Species, y = Sepal.Length, fill = Species)) + 
  geom_boxplot() +
  theme(legend.position = "top")

# Scatter plot of MPG vs Horsepower from the mtcars R data

ggplot(mtcars, aes(x=hp, y=mpg)) +
  geom_point(color="purple") +
  ggtitle("Scatter Plot of MPG vs Horsepower") +
  xlab("Horsepower") +
  ylab("Miles per Gallon")


