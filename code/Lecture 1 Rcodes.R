data() # provides list of all built in datasets

data(state) # Loads in built in data set

# The pound symbol # allows you to comment in R

state.name # Dataset in R

is.numeric(state.name) # Asking R if this is a numeric mode

is.character(state.name) # Asking R if this is a character mode

# is.logical() 
# Asking R if this is a lodical mode

is.vector(state.name) # Asking R if this is a vector object

is.data.frame(state.name)  # Asking R if this is a data frame object

# Also: is.matrix(), is.factor(), is.list()

state.name[13] # Picks out the 13th element of the vector

data(precip)

precip

help(precip) # Gives description of the dataset 

is.numeric(precip)

length(precip) # Gives the length of the vector or factor (including lists) 

mean(precip) # Provides the mean of the data

var(precip) # Provides the variance of the data

sd(precip) # Provides the standard deviation of the data

summary(precip) # A generic function used to produce result summaries of the results of various model fitting functions 

as.vector(precip) # Changes the type of object # Dropped off the character attribute or one object

# Also: as.matrix(), as.character(), as.numeric(), as.data.frame(), as.logical(), as.factor()

s<-as.vector(precip) # Creating a new variable s where precip is now a vector not a factor

names(s) # A function to get or set the names of an object.

blastoff<-c(5,4,3,2,1)
blastoff

ct<-1:100 # Creating a sequence from 1 to 100

10:(-100) # Creating a sequence from 10 to -100

5:1 # Creating a sequence from 5 to 1

seq(5,1,length=5) # Creating a sequence from 5 to 1 with 5 elements using the sequence function

seq(1,5,by=1) # Creating a sequence from 1 to 5 by increments of 1 using the sequence function

seq(0,10,length.out=12) # Creating a sequence from 0 to 10 with 12 elements using the sequence function

set.length<-length(seq(-10,10,.1)) # Nested functions. In this case, it will compute the length of the sequence

state.region

is.factor(sate.region) # Asking R if this is a factor or categorical variable

attributes(state.region) # These functions access an object's attributes and returns the object's attribute list.

attributes(state.region)$class # The $ combined list name will allow you to pull a particular attribute

names(attributes(state.region))

region.char<-as.character(state.region)

is.character(region.char)

hp.char<-c("Snape","Harry","Dumbledore")  # When creating a character vector, use quotes around each element

state.x77

is.data.frame(state.x77)

is.matrix(state.x77)


length(state.x77) # Provides the number of elements in a matrix

dim(state.x77) # Gives you the dimension of the matrix (r x c) 

dim(state.x77)[1] # Gives you the number of rows in the matrix

dim(state.x77)[2] # Gives you the number of columns in the matrix

state.x77[3,5] # Gives the element in the 3rd row and 5th column

state.x77[3,] # Gives the entire 3rd row

state.x77[,5] # Gives the entire 5th column

state.x77[3,c(1,5,7)] # Gives the elements in the 3rd row and the 1st, 5th, and 7th column

state.x77[c(3,5),5] # Gives the elements in the 3rd and 5th row and 5th column

rownames(state.x77) # Gives the row names of a matrix like object

colnames(state.x77) # Gives the column names of a matrix like object

state.x77[,"Murder"] # Gives the entire column of Murder

state.dfr<-data.frame(state.name, state.region, state.abb, state.x77) # Creates a data frame

# Coerces all variables as factors. Creates a matrix with different types of objects

state.dfr

attributes(state.dfr)

names(state.dfr) # Provides the column names

row.names(state.dfr) # Same as rownames

Population # Will get an error saying object not found

state.dfr$Population

is.character(state.dfr$state.name)

is.factor(state.dfr$state.name)


state2.dfr<-data.frame(I(state.name), state.region, I(state.abb), state.x77) # Creates a data frame

# Change the class of an object to indicate that it should be treated ‘as is’. 
# In the data.frame() function, it inhibits the conversion of character vectors to factors

is.character(state.dfr$state.name)

is.factor(state.dfr$state.name)

A<-state.x77[1:3,1:3] # Creating a 3 by 3 matrix