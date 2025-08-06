################ Data Description & Visualization #################################################

################--------------------- Data Description----------------- #################################################


setwd("H:/My Drive/Teaching Courses/PUBH 422/Datasets") # Set working directory

iris_data <- read.csv("iris.csv", header = TRUE) # Load the iris dataset

cleaned_iris_data <- na.omit(iris_data) # Remove rows with NA values


summary(cleaned_iris_data) # return the statistical summaries of the entire data frame

mean(cleaned_iris_data$Sepal.Width) # Calculates the average 

median(cleaned_iris_data$Sepal.Width) # Finds the middle value

### Calculate the mode from the modeest package
install.packages("modeest")
library(modeest)
mfv(cleaned_iris_data$Sepal.Width) # Finds the mode (most frequent value)

sd(cleaned_iris_data$Sepal.Width) # Calculates the standard deviation 

var(cleaned_iris_data$Sepal.Width) # Computes the variance 

range(cleaned_iris_data$Sepal.Width) # Returns the minimum and maximum values

IQR(cleaned_iris_data$Sepal.Width) # Calculates the interquartile range 

quantile(cleaned_iris_data$Sepal.Width) # calculate specific quantiles 


####--- Using the sapply() function: use the function sapply() to apply a particular function 
over a list or vector.

# Compute the mean of each column
sapply(cleaned_iris_data[, -5], mean)

# Compute quartiles
sapply(cleaned_iris_data[, -5], quantile)


####--- Descriptive statistics (sd, mean, median, etc.) by groups using --tapply()--

sd_Sepal.Length_by_Species <- tapply(cleaned_iris_data$Sepal.Length,cleaned_iris_data$Species, sd)
sd_Sepal.Length_by_Species

mean_Sepal.Length_by_Species <- tapply(cleaned_iris_data$Sepal.Length,cleaned_iris_data$Species, mean)
mean_Sepal.Length_by_Species

######### Descriptive statistics (mean, sd, median, etc.) by groups using --aggregate ()—##########

aggregate(cleaned_iris_data[c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")],
          by=list(cleaned_iris_data$Species), mean, na.rm=TRUE)



####--- Using stat.desc() function: provides other useful statistics including: the median, the mean
the standard error on the mean (SE.mean), the confidence interval of the mean (CI.mean) at the p level 
(default is 0.95), the variance (var), the standard deviation (std.dev)
and the variation coefficient (coef.var) defined as the standard deviation divided by the mean
Install pastecs package

install.packages("pastecs")

# Compute descriptive statistics

library(pastecs) # load package

res <- stat.desc(cleaned_iris_data[, -5])
round(res, 2) # round to 2 decimal places



#################------Categorical data: Frequencies/Crosstabs-------######################

HINTSData<-read.csv(choose.files(), header = T)  #####Use HINTS Data
names(HINTSData)
attach(HINTSData)

# Cleaning the data
HINTSData_new <- HINTSData[c(QualityCare>0 & HealthInsurance>0 & Age>0 & BirthGender>0 & FullTimeOcc_Cat>0 
                             & BMI>=0 & smokeStat>0 & WeeklyMinutesModerateExercise>=0 & AvgDrinksPerWeek>=0),]

names(HINTSData_new)
attach(HINTSData_new)
summary(HINTSData_new)

##### ----Frequency tables with freq() -----####

install.packages("summarytools")
library(summarytools)

freq(QualityCare) # print the frequency of QualityCare

### --remove NA and cumulative frequency information
freq(QualityCare, report.nas = FALSE, cumul = FALSE) 


### --Create One-way table of QualityCare#####

one_way_table<-table(QualityCare)
one_way_table

### --Create two-way table of QualityCare and BirthGender######

two_way_table<-table(QualityCare, BirthGender)
two_way_table

### --Adding row/column totals

addmargins(two_way_table) # Add row and column sum totals

prop.table(two_way_table,1) # Row proportions

prop.table(two_way_table,2) # Column proportions

round(prop.table(two_way_table,1), 2) # Round row prop to 2 digits

round(100*prop.table(two_way_table,1), 2) # Round row prop to 2 digits (percents)

addmargins(round(prop.table(two_way_table,1), 2), 2) # Round row prop to 2 digits and add only column sum

prop.table(two_way_table) # Total proportions using table total
addmargins(prop.table(two_way_table)) # add the row and column sum


## In percentages of total proportions, rounded to 2 decimal places, and add row/column sum
addmargins(round(100*prop.table(two_way_table),2)) 


###### ---3-way crosstabs ---#####

three_way_table<-xtabs(~QualityCare + HealthInsurance + BirthGender, data = HINTSData_new)
three_way_table
ftable(three_way_table)


####### ---Creating Crosstabs and Chi-square test using the function ctable() ---###################

#The ctable() function produces cross-tabulations with row percentages

ctable(x = factor(QualityCare), y = factor(HealthInsurance))

###Obtain proportion from table total

ctable(x = factor(QualityCare), y = factor(HealthInsurance),
       prop = T)

### --- Perform Chi-square test

ctable(x = factor(QualityCare), y = factor(HealthInsurance),
       #prop = "n", #remove proportions
       #totals = T, #remove totals
       #headings = T, #remove headings
       chisq = T # Chi-square test of independence
)


#######################################################################################################
################ --------------Data Visualization------------- #################################################
#########################################################################################################


############### -------Bar Graphs------- #############

barplot(table(smokeStat))


# Produces a bar graph
# Make sure to include the table function

# OR

tab<-table(smokeStat)


labels <- c('current', 'former', 'never') # defining the labels
barplot(tab, names.arg=labels)

## -------A Prettier Bar Graph---------- ##

barplot(tab, names.arg=labels, main="Bar Graph of Smoking Status", col="orchid", xlab="Smoking Status", ylab="Frequency") 
barplot(tab,names.arg=labels, main="Bar Graph of Smoking Status", col=c("orchid","deeppink","aquamarine"), xlab="Smoking Status", ylab="Frequency")

barplot(tab, names.arg=labels, main="Bar Graph of Smoking Status", col=rainbow(3), xlab="Smoking Status", ylab="Frequency")
barplot(tab, names.arg=labels, main="Bar Graph of Smoking Status", col=heat.colors(3), xlab="Smoking Status", ylab="Frequency")
barplot(tab, names.arg=labels, main="Bar Graph of Smoking Status",col=terrain.colors(3), xlab="Smoking Status", ylab="Frequency")

## ---Multiple panel plots: One row by two column plot
par(mfrow = c(1, 2)) 

barplot(tab, names.arg=labels, main="Bar Graph of Smoking Status",col=topo.colors(3),xlab="Smoking Status",ylab="Frequency")

barplot(tab, names.arg=labels, main="Bar Graph of Smoking Status", col=cm.colors(3), xlab="Smoking Status", ylab="Frequency")
legend("topleft", labels, cex = 1.3, fill = cm.colors(3)) # adding legend

### ---Revert back to single panel plot
par(mfrow = c(1, 1))


############# -------Pie Graphs------- ######################


pie(table(smokeStat), col=topo.colors(3)) 
pie(table(smokeStat), col=c("orchid","aquamarine","deeppink"))

# Produces a pie chart
# Make sure to include the table function
# Since there are three categories of smoking status
# we need three colors, one for each slice of the pie
# Rainbow(3) will choose 3 colors
# The second command allows you to choose the 3 colors you wanted

# OR

tab<-table(smokeStat) # make a talbe of smoking status

pie(tab, col=topo.colors(3))
pie(tab,col=c("orchid","aquamarine","deeppink"))

## ------A Prettier Pie Chart------ ##

tab<-table(smokeStat) 
lbls<-c("Current", "Former","Never")  # Definding labels
percent<-round(tab/sum(tab)*100) # Calculate table percentages

lbls <- paste(lbls, percent) # add percents to labels
lbls <- paste(lbls,"%",sep="") # add % to labels

pie(tab, labels=lbls, main="Pie Chart of Smoking Status", col=topo.colors(3))

###--Add legend
labels <- c('current', 'former', 'never') # defining the labels
legend("topleft", labels, cex = 1.3, fill = topo.colors(3)) # adding legend


############## -----Plotting 3D Pie Char --------------#############

# Get the library.
install.packages("plotrix") # installing package
library(plotrix) # making use of the package

labels <- c('current', 'former', 'never') # defining the labels

# Plot the chart.
pie3D(tab,labels = labels,explode = 0.05, main = "Pie Chart of Quality of Care ")

##add legend and percent
piepercent<- round(100*ddf1tab/sum(ddf1tab), 1)
pie3D(tab,labels = lbls,explode = 0.05, main = "Pie Chart of Quality of Care ")
legend("topright", labels, cex = 0.8, fill = rainbow(length(tab)))

# Give the chart file a name called Hints_Quality_of_Care
png(file = "Hints_3DSmokingStatus.jpg")



##########################################################
#############    Two Categorical Variable    #############
##########################################################

## Two-way Table — Counts ##

table(smokeStat, BirthGender)  # Provides a count for each combination of the two categorical variables

tab<-table(smokeStat, BirthGender) # Stores two-way table

addmargins(tab) ## Also includes the Sum

## --Two-way Table — Proportion --##

prop.table(table(smokeStat, BirthGender))	

# --Provides a percent for each combination of the two categorical variables
# --Make sure to include the table function

# --OR

prop.table(tab)

addmargins(prop.table(tab)) ## Also includes the Sum

##### Cluster and Stacked Bar Graphs #####

# --The next TWO commands will construct a Stacked Bar Graph

tab<-table(smokeStat, BirthGender) 
barplot(tab, legend=rownames(tab))

## --OR 

barplot(tab, legend=rownames(tab),col=rainbow(3))
barplot(tab, legend=rownames(tab),col=c("orchid","aquamarine","deeppink"),
        main = "Stacked Bar Plot of Gender by Smoking Status",
        xlab="Gender", ylab="Frequency")

# --The next two commands will construct a Clustered Bar Graph

tab<-table(smokeStat, BirthGender)
barplot(tab, beside=TRUE, legend=rownames(tab))

## --OR 

barplot(tab, beside=TRUE, legend=rownames(tab),col=rainbow(3))
barplot(tab, beside=TRUE, main="Bar Graph of Smoking Status vs. Gender", col=rainbow(3),
        xlab="Gender", legend=rownames(tab),ylab="Frequency") 

## --A Prettier Cluster (Stacked) Bar Graph --##

tab<-table(smokeStat, BirthGender)

barplot(tab,beside=TRUE,main="Bar Graph of Smoking Status vs. Gender",col=rainbow(3),
        xlab="Gender", legend=rownames(tab),ylab="Frequency",ylim=c(0,200)) 

###################################################
#############    Discrete Variables    #############
####################################################

## Frequency, Relative Frequencies, & Cumulative Relative Frequencies ##

# --Sample discrete data
data <- c(1, 2, 2, 3, 3, 3, 4, 4, 5, 4, 5, 2, 1, 2, 1, 3)

n <- length(data) # sample size

freq<-table(data)

rel.freq<-prop.table(freq)

cumu.rel.freq<-cumsum(rel.freq)

freq.table<-cbind(freq,rel.freq,cumu.rel.freq)

freq.table

## -----Dotplot----- ##

# --The dotchart() function in base R can create a simple dot plot.

dotchart(data,
         main = "Dot Plot of Discrete Variable",
         xlab = "Value",
         ylab = "Frequency (Implicit)")

# --For a stacked dot plot where tied values are stacked vertically, 
# you can use stripchart():

stripchart(data, method="stack", offset=.5, pch=19, 
           main="Stacked Dot Plot of Discrete Variable")


######################################################################
#############    Plots for Any Quantitative Variables    #############
######################################################################

## using the HINTSData_New

## --Stem-and-leaf display-- ##

stem(Age)

## --Histogram-- ##

hist(Age)
hist(Age, main="Histogram of Age", col="deeppink")

###Get the probabilities
hist(BMI, xlab = 'BMI level', main = 'Histogram of BMI', col = 4, border = 'green',
     probability = T)
grid(10,10)

## ---Density Plot--- ###

# Estimate kernel density
density_estimate <- density(Age)

# Plot the density
plot(density_estimate, main = "Density Plot of Age",
     col = 4, xlab = "Value", ylab = "Density")

## --Boxplot-- ##

boxplot(Age, horizontal = T, main = "Boxplot Distribution of Age") 


## ----HISTOGRAM AND BOXPLOT IN ONE FIGURE--- ##

par(mfcol=c(1,2)) ## 1 row and 2 columns of plots

hist(Age, col='forestgreen', main='a. Histogram of Age')  

boxplot(Age, col='deeppink',main='b. Boxplot of Age')  


############################################################
#############    Any Quantitative Variables    #############
#############       Side-by-Side Boxplots      #############
#############         Grouping Variable        #############
############################################################

par(mfcol=c(1,1)) ## 1 row and 2 columns of plots

## Example 1 ##

boxplot(Age ~ BirthGender) # boxplot of age by gender

#### Boxplot of Age by QualityCare

boxplot(Age ~ QualityCare, data = HINTSData_new, xlab = "Quality of Care",
        ylab = "Age in years", main = "Distribution of Age by Quality of Care",
        col = rainbow(5),
        names=c('Excellent', 'very good', 'good','fair','poor'))

# Plot the chart of different style

par(mfrow=c(1,2))

## Display Vertical boxplot 

boxplot(BMI ~ QualityCare, data = HINTSData_new, xlab = "Quality of Care",
        ylab = "BMI", main = "Distribution of Age by Quality of Care",
        notch=TRUE, varwidth=TRUE,col = c('red','yellow','green','blue','pink'),
        names=c('Excellent', 'very good', 'good','fair','poor'))

## Display Horizontal boxplot

boxplot(BMI ~ QualityCare, data = HINTSData_new, xlab = "BMI",
        ylab = "Quality of Care", main = "Distribution of Age by Quality of Care",
        notch=TRUE, varwidth=TRUE,col = c(1:5),
        names=c('Excellent', 'very good', 'good','fair','poor'), horizontal = T)

################ ----Recoding QaulityCare into new variable variable------##########

HINTSData_new$QualityCare_New[HINTSData_new$QualityCare == 1] <- "Excellent"
HINTSData_new$QualityCare_New[HINTSData_new$QualityCare == 2] <- "Very Good"
HINTSData_new$QualityCare_New[HINTSData_new$QualityCare == 3] <- "Good"
HINTSData_new$QualityCare_New[HINTSData_new$QualityCare == 4] <- "Fair"
HINTSData_new$QualityCare_New[HINTSData_new$QualityCare == 5] <- "Poor"

summary(factor(HINTSData_new$QualityCare_New))

boxplot(Age ~ QualityCare_New, data = df5, xlab = "Quality of Care",
        ylab = "Age in years", main = "Distribution of Age by Quality of Care")


# Provides a boxplot for each category of a categorical variable
# In general: boxplot(quantitative variable ~ categorical variable)

## Example 2 ##

length <- c(11.2,13.0,12.5,10.0,11.3, 7.0,10.0,10.2,10.6,6.7,9.3,
            11.0,12.8,9.5,12.2,13.5,13.1,11.3,12.2,14.2,7.6,5.8,7.7, 9.0,
            8.1,8.6,6.3,7.5,8.6,8.7,7.8,6.5,6.1,8.0,7.5,8.0,7.0,6.2,7.9,8.2)

type <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,
          2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)

boxplot(length ~ type, main ='Sycamore and Sugar Maple leaves ',
        ylab='length (cm)', col='lightgreen')


########----Plotting line plot---########################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##--- Data from Biomass

biomass_data<-read.csv(choose.files(),header = T) ##Biomass data

attach(biomass_data)
names(biomass_data)
head(biomass_data)


plot(STBM)

plot(STBM, type = "o",col = 'red', ylab = 'Estimate', main = 'Line Plot of Data', pch =1)

####---Create two line plots on the same panel

plot(STBM, type = "o",col = 'red', ylab = 'Estimate', main = 'Line Plot of Data', pch =1)
lines(RTBM, type = 'o', col = 'blue', pch=2)
legend('bottomright', legend = c('STBM','RTBM'), cex = 1.3, col = c('red', 'blue'), pch = c(1,2))


##############################################
#############    Scatterplots    #############
#############     Correlation    #############
##############################################

# ---Scatter plot
plot(STBM, RTBM, pch = 19, col = factor(C))

# ---Legend
legend("topleft",legend = levels(factor(C)),pch = 19,col = factor(levels(factor(C))))

## ---Advance scatterplot with confidence interval
install.packages("car")
library(car)

scatterplot(STBM, RTBM)

#--OR
scatterplot(STBM, RTBM,            # Data
            pch = 19,        # Symbol of the points
            col = 1,         # Color of the points
            smooth = FALSE,  # Remove smooth estimate
            regLine = FALSE) # Remove linear estimate

## ---Using the HINTS Dataset---####

par(mfrow=c(1, 1)) # creating one row two columns side-by-side plots

# Single scatterplot
plot(Age, BMI, main = 'Age vs BMI', xlab = 'Age', ylab = 'BMI', col='green',pch=19)

# -----Two or more scatterplot on the same panel

plot(Age, BMI, main = 'BMI ~ Age and AvgAlcoholPerWk', xlab = 'Age', ylab = 'BMI', col='blue',pch=1)
points(AvgDrinksPerWeek, BMI, col='red', pch=2)
points(WeeklyMinutesModerateExercise, BMI, col='black', pch=3)
legend('topright', legend=c("Age", "Alcohol","PhysicalEx"), cex=1.3, col = c("blue","red","black"), pch=c(1,2,3))
grid(10,10) # add grid to the plot

### ---Creating two data variables of Altitude and RBC---###

Altitude<-c(0,1840,2200,2200,5000,5200,5750,7400,8650,10740,12000,
            12200,12300,14200,14800,14900,17500)

RBC<-c(4.93,4.75,5.40,4.65,5.42,6.55,5.99,5.39,5.44,5.82,7.50,5.67,  
       6.31,7.05,6.46,6.66,7.37)

plot(Altitude, RBC)  #Produces a scatterplot
cor(Altitude, RBC)   # Computes Pearson's correlation r

# In General:
# plot(x, y)
# cor(x, y)

## A Prettier Scatterplot ##

plot(Altitude, RBC, main="Scatterplot of X and Y", xlab="X Variable", ylab="Y Variable", pch=19, col="deeppink")


##############----Scatter Plot Matrix------#######################
##################################################################


plot(biomass_data[,4:8], pch =20, cex = 1.5, col = 'steelblue')

## Using pairs() function
pairs(HINTSData_new[,c(5,14,16,17)])

#Show only upper panel:
pairs(HINTSData_new[,c(5,14,16,17)], pch=19, lower.panel = NULL)

#add color of gender
pairs(HINTSData_new[,c(5,14,16,17)], pch=19, col = rainbow(2)[BirthGender], lower.panel = NULL)

#### ---Create Scatterplot Matrix Using pairs.panels() function--- #### 
### --- Gives both scatterplot and correlations

install.packages("psych")
library(psych)

pairs.panels(biomass_data[,4:8]) # from biomass data

pairs.panels(HINTSData_new[,c(5,14,16,17)], 
             main = "Distributions, Scatterplot, & Correlations") # from HINTS data



### ----Correlation from heatmap--- ###########

install.packages("corrplot")
library(corrplot)

CorEED <- cor(biomass_data[,5:8]) ### Obtain the correlations

corrplot(CorEED, method = 'number') # Corellation matrix with colorful number

corrplot(CorEED, method = 'color', order = 'alphabet')

corrplot(CorEED) # by default, method = 'circle'

corrplot.mixed(CorEED, order = 'AOE')

corrplot(CorEED, method = 'ellipse', order = 'AOE', type = 'upper')



#### ---From data file----#####

my.datafile <- tempfile()
cat(file=my.datafile, " 
  5.526  59.0  113.5 f
  10.401  75.0  142.0 m
  9.213  69.0  124.0 f
  8.953  67.5  125.0 f
  7.063  62.0  129.5 m
  6.610  62.0  123.0 f
  11.273  74.0  140.0 m
  2.447  47.0  97.0 f 
  15.493  86.5  162.0 m
  9.004  69.0  126.5 f
  8.199  70.5  136.0 m
  6.601  64.5  116.0 f
  7.622  67.5  135.0 m
  10.067  73.0  136.5 m
  10.091  73.0  135.5 m
  10.888  77.0  139.0 m
  7.610  61.5  118.0 f
  7.733  66.5  133.5 m
  12.015  79.5  150.0 m
  10.049  74.0  137.0 m
  5.149  59.5  116.0 f 
  9.158  68.0  123.0 f
  12.132 75.0  141.0 m
  6.978  66.5  117.0 f
  6.890  63.0  117.0 f
", sep=" ")
options(scipen=999) # suppressing scientific notation

lizard<-read.table(my.datafile,header=FALSE, col.names=c("Mass", "SVL", "HLS", "Gender"))

attach(lizard)
names(lizard)
head(lizard)

pairs(lizard[,1:3])

pairs(lizard[,1:3], pch=19, col = rainbow(2)[factor(Gender)], 
      main = "Scatterplot Matrix of Mass vs SVL vs HLS by Gender")
install.packages("scatterplot3d")
library(scatterplot3d)

par(mfrow=c(1,1))

scatterplot3d(SVL, HLS, Mass)

# ----Producing a 3d scatterplot and 3d scatterpl ot based on Gender--- #

### --Recoding Gender into red and blue, with variable name pcolor

lizard$pcolor[lizard$Gender=="f"]<-"red"
lizard$pcolor[lizard$Gender=="m"]<-"blue"

scatterplot3d(SVL, HLS, Mass, color=lizard$pcolor, pch=19, 
              main = "3D Plot of SVL vs HLS vs Mass")

install.packages("rgl")
library(rgl)

plot3d(SVL, HLS, Mass, col=lizard$pcolor, size=3) ## Interactive 3D plot


