#########################----Basic Inference----########################
##########################################################################

### ----Loading Data

HINTSData<-read.csv(choose.files(), header = T)  #####Use HINTS Data
names(HINTSData)
attach(HINTSData)
summary(HINTSData)

df_new<-HINTSData[BirthGender>0,]

summary(HINTSData_new)

HINTSData_new <- HINTSData[c(QualityCare>0 & HealthInsurance>0 & Age>0 & BirthGender>0 & FullTimeOcc_Cat>0 
                             & AgeGrpB>0 & EducA>0  & BMI>=0 & smokeStat>0 & WeeklyMinutesModerateExercise>=0 
                             & AvgDrinksPerWeek>=0 & HHInc>0 & MaritalStatus>0 & SexualOrientation>0),]

names(HINTSData_new)
attach(HINTSData_new)
summary(HINTSData_new)

#The t.test( ) function performs one-sample and two-sample t-tests. 
#In performing a one-sample t-test, this function also gives a confidence interval for the population mean.

#########-------Test Mean of One-sample t-test--------########################

t.test(BMI) # Default null hypothesis(mu) = 0 with 95% CI

# test if the mean BMI is less than 20 at 90% confidence interval
t.test(BMI, mu=20, alternative = "less", conf.level = 0.90)

t.test(BMI ~ factor(BirthGender))$conf.int  ## print the confidence interval of the t.test

#########--Test means of Two Samples / Two-sample t-test / Independent sample t-test---###################

#null hypothesis H0: mu_M = mu_F
#The outcome variable and grouping variable are identified using the 'outcome ~ group' syntax

t.test(BMI ~ factor(BirthGender))

t.test(BMI ~ factor(BirthGender), var.equal = TRUE) #Assumption of equal variance

t.test(BMI ~ factor(BirthGender), var.equal = FALSE) #Assumption of unequal variance

### ----Recategorize BMI into 'not overweight = 0 (BMI<=30)' and 'overweight =1 (BMI>30)'

HINTSData_new$bmiCat <- ifelse(BMI <= 30,0,1)

attach(HINTSData_new) 
summary(factor(bmiCat))

### ---Test whether there is a difference in exercise between overweight BMI and not overweight BMI

t.test(WeeklyMinutesModerateExercise ~ factor(bmiCat))


#############################---Paired-sample t-test---###############################

#The following example compares the means of a pre-physical activity BMI score (score1) and 
#a post-physical activity BMI score (score2) from a sample of 6 subjects.

score1<-c(20.7, 35.4, 23.0, 35.1, 29.2, 18.4)

score2<-c(23.6, 27.2, 16.6, 23.0, 28.7, 30.3)

mean(score1)
mean(score2)

#null hypothesis H0: mu_score1 = mu_score2
t.test(score1,score2,paired=TRUE)

# Add the type of test
t.test(score1,score2,paired=TRUE, alternative = "greater")

#################---Chisquare Test----####################################
###############################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ---Chi-Square Test of Independence or Relationship between 2 Categorical Variables##

#### Do chi-squared test Ho: no relationship/independent 

# NOTE: Chi-sqr = sum (obs-exp)^2/exp. Degrees of freedom for Chi-sqr are (r-1)*(c-1)
# NOTE: Chi-sqr contribution = (obs-exp)^2/exp
# Cramer's V = sqrt(Chi-sqr/N*min). Where N is the sample size and min is the minimum of (r-1) or (c-1)

## Create a table with QualityCare as row and BirthGender as column

tab<-table(QualityCare, BirthGender) 
tab

# perform the Chi-square test
chisq.test(tab) 

### Do fisher'exact test Ho: no relationship
fisher.test(tab) 


#########---Test means of More than Two-sample/ ANOVA Test---###################
#############################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

##Test the difference in BMI across 4 different educational level

BMI_anova <- aov(BMI ~ factor(EducA)) #ANOVA model


summary(BMI_anova) #print the ANOVA results

model.tables(BMI_anova, "means", digits=3) #print the mean BMI of the education levels


#Given that the overall ANOVA shows significance, we can request pairwise comparisons 
#using Tukey's multiple comparison procedure:

TukeyHSD(BMI_anova)

# ---Visualization of the means of the BMI across educational level

# Box plot
boxplot(BMI ~ EducA, data = HINTSData_new,
        xlab = "Education Level", ylab = "BMI",
        main = "Boxplot distribution of BMI by level of education",
        frame = FALSE, col = c("blue","#00AFBB", "#E7B800", "#FC4E07"))


# -----plotmeans across groups

install.packages("gplots") # install required package
library("gplots") # loading package

plotmeans(BMI ~ EducA, data = HINTSData_new, frame = FALSE,
          xlab = "Education Level", ylab = "BMI",
          main="Mean Plot of BMI by level of education with 95% CI") 

####-----Assessing ANOVA model assumptions----####################

# ---1. Plot of Homogeneity of variances of the ANOVA model
plot(BMI_anova, 1)

# --Levene’s test to check the homogeneity of variances.
# H0: variance across groups are similar or not different

library(car)
leveneTest(BMI ~ factor(EducA), data = HINTSData_new)


# ---2. Normality plot of the ANOVA residuals
plot(BMI_anova, 2)

# Extract the ANIOVA residuals
aov_residuals <- residuals(object = BMI_anova )

# Run Shapiro-Wilk test: Test the normal distribution of the ANOVA residuals
# H0: ANOVA residuals are normally distributed
shapiro.test(x = aov_residuals )


############################################################
######## ---correlation test-----#########################
############################################################

cor(Age, BMI) # Compute the correlation value between Age and BMI

cor.test(Age, BMI) # Test the correlation between Age and BMI

### ----Using the biomass data

biomass_data<-read.csv(choose.files(),header = T) ##loading biomass data

attach(biomass_data)
names(biomass_data)
head(biomass_data)

#---Creating a correlation plot with a statistical test

# Install and load the required R package
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

# print correlation plot with statistical test for the 5th-8th variables

chart.Correlation(biomass_data[,5:8]) 

#######################################################################################
#####################----Linear Regression Analysis----################################
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#---Regression analysis is performed through the 'lm( )' function

#####---Simple linear regression

model_bioLFBM <- lm(LFBM ~ STBM, data = biomass_data)
summary(model_bioLFBM)

model_BIM<-lm(BMI ~ WeeklyMinutesModerateExercise, data = HINTSData_new)
summary(model_BIM)

#####-------Multiple linear regression

model2_bioLFBM <- lm(LFBM ~ STBM + RTBM, data = biomass_data)
summary(model2_bioLFBM)

model2_BIM<-lm(BMI ~ WeeklyMinutesModerateExercise + Age, data = HINTSData_new)
summary(model2_BIM)



