##########################################################################################
########################---- Data Visualization using ggplot2 package---##############

###---Importing and loading data----####

HINTSData<-read.csv(choose.files(), header = T)  #####Use HINTS Data
names(HINTSData)
attach(HINTSData)
summary(HINTSData)

df_new<-HINTSData[BirthGender>0,]

summary(HINTSData_new)

HINTSData_new <- HINTSData[c(QualityCare>0 & HealthInsurance>0 & Age>0 & BirthGender>0 & FullTimeOcc_Cat>0 
                             & BMI>=0 & smokeStat>0 & WeeklyMinutesModerateExercise>=0 & AvgDrinksPerWeek>=0),]

names(HINTSData_new)
attach(HINTSData_new)
summary(HINTSData_new)

#################################################################################################
####################----Univariate Data Visualization using ggplot2 package----##################
#################################################################################################

names(HINTSData_new)

# installing tidyverse package
install.packages("tidyverse")
library(ggplot2)

# ---Histogram of BMI

ggplot(HINTSData_new, aes(x=BMI)) +
  geom_histogram(binwidth=2, fill="steelblue", color="black") +
  ggtitle("Histogram of BMI") +
  xlab("BMI Level") +
  ylab("Count")

#--- Boxplot

ggplot(HINTSData_new, aes(y = BMI)) + 
  geom_boxplot(fill = "pink") + 
  scale_x_discrete() +
  labs(title = "Boxplot of BMI",
       y = "BMI Level")

#### ----Dot plot

data("mtcars") ### loading built-in R data
head(mtcars)

# plot the mpg distribution using a dotplot
# Plot mpg as a dot plot using 
# gold dots with black borders

ggplot(mtcars, aes(x = mpg)) +
  geom_dotplot(fill = "gold", 
               color="black") + 
  labs(title = "Dotplot of mpg",
       y = "Proportion",
       x = "mpg")

# ---Bar plot of Smoking Status

ggplot(HINTSData_new, aes(x=factor(smokeStat))) +
  geom_bar(fill="tomato", color="black") +
  ggtitle("Bar Plot of Smoking Status") +
  xlab("Smoking Status") +
  ylab("frequency") 

###################---Advanced Bar Chart################################################

# Creating a new variable QualityCare_New with the names of the level of QualityCare

HINTSData_new$QualityCare_New[HINTSData_new$QualityCare==1] <- "Excellent"
HINTSData_new$QualityCare_New[HINTSData_new$QualityCare==2] <- "Very Good"
HINTSData_new$QualityCare_New[HINTSData_new$QualityCare==3] <- "Good"
HINTSData_new$QualityCare_New[HINTSData_new$QualityCare==4] <- "Fair"
HINTSData_new$QualityCare_New[HINTSData_new$QualityCare==5] <- "Poor"

head(HINTSData_new)

library(dplyr)
library(scales)
plotdata <- HINTSData_new %>%
  count(QualityCare) %>%
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%"))

# ---plot the bars as percentages in descending order with bar labels

ggplot(plotdata, 
       aes(x = reorder(QualityCare, -pct), y = pct)) + 
  geom_bar(stat="identity", fill="indianred3", color="black") +
  geom_text(aes(label = pctlabel), vjust=-0.25) +
  scale_y_continuous(labels = percent) +
  labs(x = "Quality of Care", 
       y = "Percent", 
       title  = "Participants by Quality of Care") + 
  coord_flip()

#######################################################################

# --- Pie chart of Quality of care

# create a basic ggplot2 pie chart

QualityCare_Data <- data.frame(
  group = c("Excellent", "V.Good", "Good", "Fair", "Poor"),
  count= c(863, 1180, 531, 134, 30) 
)

QualityCare_Data

# Pie Chart
ggplot(QualityCare_Data, aes(x="", y=Frequency, fill=Status))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  ggtitle("Pie Chart of Quality of Care")


# ------Tree map: An alternative to a pie chart is a tree map.

install.packages("treemapify")
library(treemapify)
library(dplyr)

# create a treemap of Quality of Care

plotdata <- HINTSData_new %>%
  count(QualityCare_New)

ggplot(plotdata, 
       aes(fill = QualityCare_New, area = n)) +
  geom_treemap() + 
  labs(title = "Treemap of Quality of Care")

# Alternatively

ggplot(plotdata, 
       aes(fill = QualityCare_New, 
           area = n, 
           label = QualityCare_New)) +
  geom_treemap() + 
  geom_treemap_text(colour = "white", 
                    place = "centre") +
  labs(title = "Treemap of Status of Quality of Care") +
  theme(legend.position = "none")


# --Line plot of BMI Level over Index (sequential order of rows)

ggplot(HINTSData_new, aes(x=1:nrow(HINTSData_new), y=BMI)) +
  geom_line(color="green") +
  ggtitle("Line Plot of BMI") +
  xlab("Index") +
  ylab("BMI Level")

# Another example using mtcars data in R

data("mtcars")

# ---Line plot of MPG over Index (sequential order of rows)

ggplot(mtcars, aes(x=1:nrow(mtcars), y=mpg)) +
  geom_line(color="red") +
  ggtitle("Line Plot of MPG") +
  xlab("Index") +
  ylab("Miles per Gallon")


###########################################################################################
###################################----Bivariate Visualization------#######################
###########################################################################################

# --------Change barplot fill colors by groups
#---------Barplot of Quality of Care base of BMI

ggplot(HINTSData_new, aes(x=QualityCare_New, y=BMI, fill=QualityCare_New)) +
  geom_bar(stat="identity") + 
  theme_minimal()


# --Scatter plot of WeeklyMinutesModerateExercise vs BMI

ggplot(HINTSData_new, aes(x=WeeklyMinutesModerateExercise, y=BMI)) +
  geom_point(color="purple") +
  geom_smooth(method = "lm") +
  ggtitle("Scatter Plot of WeeklyMinutesModerateExercise vs BMI") +
  xlab("WeeklyMinutesModerateExercise") 
ylab("BMI Level")


## --Boxplot of BMI by Quality of Care Status


ggplot(HINTSData_new, aes(x = QualityCare_New, y = BMI)) + 
  geom_boxplot(aes(fill = factor(QualityCare_New))) +
  labs(title = "Boxplot of BMI by Quality of Care Status",
       x = "Quality of Care", 
       y = "BMI Level")

#---Area plot of the distribution of Age by Gender

ggplot(HINTSData_new, aes(x = Age)) +
  geom_area(aes(fill = factor(BirthGender)), stat ="bin", alpha=0.6) + 
  labs(title = "Area plot of the distribution of Age by Gender",
       y = "Count")

#---Density Histogram distribution of Age by Gender

ggplot(HINTSData_new, aes(x = Age)) +
  geom_density(aes(fill = factor(BirthGender))) + 
  labs(title = "Density Histogram distribution of Age by Gender",
       y = "density")

# ----Ridgeline plots: A ridgeline plot (also called a joyplot) 
# displays the distribution of a quantitative variable for several groups.

# create ridgeline graph
library(ggplot2)

install.packages("ggridges")
library(ggridges)

ggplot(HINTSData_new, 
       aes(x = BMI, y = QualityCare_New, fill = QualityCare_New)) +
  geom_density_ridges() + 
  theme_ridges() +
  labs(title = "Distribution of BMI by Quality of Care",
       x = "BMI", y = "Quality of Care") +
  theme(legend.position = "none")


#### ----Time Serie plot

data(economics, package="ggplot2")  # load buit-in R data called economics
head(economics)
dim(economics)
#economics <- data.frame(economics)  # convert to dataframe
ggplot(economics) + geom_line(aes(x=date, y=pce, color="pcs")) +
  labs(title="Economics Time Series of Prices")


##############################################################################################
####################### --- Multivariate Visualization using ggplot2---------##################
##############################################################################################


##----Barplots

ggplot(HINTSData_new, aes(x=QualityCare_New, y=BMI, fill=factor(BirthGender))) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired") +
  labs(title="Cluster Barplot of BMI by Quality of Care and Gender",
       x="Quality of Care",
       y="BMI",
       color="Gender")

## -----Another example

# Create a dataframe
df2 <- data.frame(Supplement_type=rep(c("VC", "OJ"), each=3),
                  Dose=rep(c("D0.5", "D1", "D2"),2),
                  Tooth_length=c(6.8, 15, 33, 4.2, 10, 29.5))
head(df2)

# -----Stacked barplot with multiple groups
ggplot(data=df2, aes(x=Dose, y=Tooth_length, fill=Supplement_type)) +
  geom_bar(stat="identity") +
  labs(title="Stacked Barplot of Tooth Length by Dose and Supplement Type",
       x="Dose",
       y="Tooth Length",
       color="Supplement Type")

# ---Use position=position_dodge(): Cluster barplot

ggplot(data=df2, aes(x=Dose, y=Tooth_length, fill=Supplement_type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(title="Cluster Barplot of Tooth Length by Dose and Supplement Type",
       x="Dose",
       y="Tooth Length",
       color="Supplement Type")



# -----Faceted scatter plot of Age vs BMI, separated by number of Smoking Status

ggplot(HINTSData_new, aes(x=Age, y=BMI)) +
  geom_point(aes(color=factor(QualityCare_New)), size=3) +
  facet_wrap(~QualityCare_New) +
  labs(title="Scatter Plot of Age vs BMI by Quality of Care",
       x="Age",
       y="BMI",
       color="Quality of Care") +
  theme_minimal()

# -----Another example using mtcars data in R

data("mtcars") 

#Faceted scatter plot of mpg vs hp, separated by number of cylinders
ggplot(mtcars, aes(x=hp, y=mpg)) +
  geom_point(aes(color=factor(cyl)), size=3) +
  facet_wrap(~cyl) +
  labs(title="Scatter Plot of MPG vs HP by Cylinder",
       x="Horsepower",
       y="Miles Per Gallon",
       color="Number of Cylinders") +
  theme_minimal()


# -----Customizing the aesthetics of a scatter plot from the HINTS Data

ggplot(HINTSData_new, aes(x=Age, y=BMI)) +
  geom_point(aes(color=factor(BirthGender), shape=factor(smokeStat)), size=4) +
  scale_color_manual(values=c("blue", "red"), labels=c("Male", "Female")) +
  scale_shape_manual(values=c(16, 17, 18)) +
  labs(title="Scatter Plot of Age vs BM by Gender and Smoking Status",
       x="Age",
       y="BMI Level",
       color="Gender",
       shape="Number of Smoking Status") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5))


# -----Another example using mtcars data in R

data("mtcars") 

# ---Customizing the aesthetics of a scatter plot

ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point(aes(color=factor(am), shape=factor(cyl)), size=4) +
  scale_color_manual(values=c("blue", "red"), labels=c("Automatic", "Manual")) +
  scale_shape_manual(values=c(16, 17, 18)) +
  labs(title="Scatter Plot of MPG vs Weight",
       x="Weight",
       y="Miles Per Gallon",
       color="Transmission",
       shape="Number of Cylinders") +
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5))


# ----Scatter plot with annotations using the HINTS data

ggplot(HINTSData_new, aes(x=WeeklyMinutesModerateExercise, y=BMI)) +
  geom_point(aes(color=factor(QualityCare_New)), size=3) +
  geom_text(aes(label=row.names(HINTSData_new)), vjust=-0.5, hjust=0.5) +
  labs(title="Scatter Plot of WeeklyMinutesModerateExercise vs BMI by Quality of Care",
       x="WeeklyMinutesModerateExercise",
       y="BMI",
       color="Quality of Care Status") +
  theme_bw()


# ---Another example using mtcars data in R

data("mtcars") 

ggplot(mtcars, aes(x=mpg, y=hp)) +
  geom_point(aes(color=factor(cyl)), size=3) +
  geom_text(aes(label=row.names(mtcars)), vjust=-0.5, hjust=0.5) +
  labs(title="Scatter Plot of HP vs MPG with Annotations",
       x="Miles Per Gallon",
       y="Horsepower",
       color="Number of Cylinders") +
  theme_bw()


#------ plot multiple time series using 'geom_line's

data(economics, package="ggplot2")  # load built-in R data called economics
head(economics)
dim(economics)

ggplot(economics) + geom_line(aes(x=date, y=pce, color="pcs")) + 
  geom_line(aes(x=date, y=unemploy, col="unemploy")) + 
  scale_color_discrete(name="Legend") + 
  labs(title="Economics Time Series of Prices and Unemployment") 

### ---Advanced comparison scatterplots---- ###

data("diamonds") # load built-in R data called diamonds
head(diamonds)
dim(diamonds)

# ---Scatterplot, specifying the aesthetics inside the geoms.

ggplot(diamonds) + geom_point(aes(x=carat, y=price, color=cut)) + 
  geom_smooth(aes(x=carat, y=price, color=cut)) +
  labs(title="Diamond data scatterplot of price vs carat by cut with smoothing") 

##### ---- creating a plot function and printout

gg <- ggplot(diamonds, aes(x=carat, y=price, color=cut)) + geom_point() + 
  labs(title="Scatterplot of Diamond data scatterplot of price vs carat by cut", 
       x="Carat", y="Price") +
  theme(plot.title=element_text(size=30, face="bold"), 
        axis.text.x=element_text(size=15), 
        axis.text.y=element_text(size=15),
        axis.title.x=element_text(size=25),
        axis.title.y=element_text(size=25)) + 
  scale_color_discrete(name="Cut of diamonds") 
print(gg)

### ---- For comparison purposes:
# ---you can put all the plots in a grid as well using facet_grid(formula).
# ---using the gg function created above, add the facet_grid() function

gg + facet_grid(color ~ cut)   # In a grid


