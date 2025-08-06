
##############------Reading and importing text file data----###########

## use the code below if the data is without column names and we have to add the names

txt.filedata<-read.table("Q://Desktop/My_Pract_data.txt",header=F, col.names=c("id", "gender", "race", "trt", "day1", "day2", "day3"))

## use this code below if the text data already has column names 

txt.filedata<-read.table("Q://Desktop/My_Pract_data.txt", header=T)

attach(txt.filedata)
names(txt.filedata)



##############------Reading and importing CSV data file----###########

iris_data <- read_csv("H:/My Drive/Teaching Courses/PUBH 422/Datasets/iris.csv") # importing the iris data from the data location directory

str(iris_data) # Displays the structure of the data (types of columns, data frame summary) 

names(iris_data) # print the names of the columns
dim(iris_data) # print the dimension of the data frame

### Alternatively

setwd("H:/My Drive/Teaching Courses/PUBH 422/Datasets") # Set working directory

iris_data <- read.csv("iris.csv", header = TRUE) # Load the iris dataset

### Alternatively

HINTSdata <- read.csv(choose.files(),header = T) # opens and browse to select data from your computer

names(HINTSdata) # print the names of the columns
dim(HINTSdata) # print the dimension of the data frame


#####----NOTE: When reading the data, some computer access forward slash (/), while others accept back slash (\)



############## Reading or importing online file data ######################

online<-read.table("http://faculty.etsu.edu/seier/data/AGE.txt",header=F,col.names=c("Age"))

attach(online)
names(online)

online
Age


##############------Reading data file----############################

### Example 1: Copy and paste

my.datafile <- tempfile()
cat(file=my.datafile, " 
 1      F    0   0 day1 19.81310
 1      F    0   0 day2 18.05777
 1      F    0   0 day3 14.84996
 2      M    0   0 day1 17.91846
 2      M    0   0 day2 18.75825
 2      M    0   0 day3 15.30547
 3      M    0   0 day1 17.22526
 3      M    0   0 day2 19.79218
 3      M    0   0 day3 15.10622
", sep=" ")

options(scipen=999) # suppressing scientific notation

# Here we are creating a data frame and attaching it to R

mydata_file<-read.table(my.datafile,header=FALSE, col.names=c("id", "gender", "race", "trt", "day", "amt"))

attach(mydata_file)
names(mydata_file)

### Example 2: copy and paste

my.datafile <- tempfile()
cat(file=my.datafile, " 
3497900 0.623
2485475 0.593
1782875 0.512
1725450 0.500
1645575 0.463
1469800 0.395
", sep=" ")

options(scipen=999) # suppressing scientific notation

# Here we are creating a data frame and attaching it to R

paper<-read.table(my.datafile,header=FALSE, col.names=c("X1", "X2"))

attach(paper)
names(paper)

x<-c(1,1,2,3,4,5,5,4)

xmen<-c("Storm", "Iceman", "Cyclops", "Mystique")


######---Use the function detach() to detach the data after using



