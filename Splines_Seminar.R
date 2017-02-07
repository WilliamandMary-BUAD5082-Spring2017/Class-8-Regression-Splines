

# Regression Splines
# -Lab 7.8.2 in the textbook
# -Code from titanium example
# -Code for cool example using equidistant knots
# -Old Faithful example and practice










#############   From Textbook 7.8.2 Lab   #################################################

# library for splines
library(splines)

library(ISLR) # Wage data set is in the ISLR library

attach(Wage) # predict Wage for different ages



# NOTE:  bs() function generates the entire matrix of basis functions for splines with the specified set of knots or df.
# cubic splines are the default, but change this by specifying 'degree = '


# plot age, wage in gray
plot(age, wage, col = 'gray', main = 'Wages for People of Different Ages')

# fit wage to age using a regression spline with knots at age = 25, 40, and 60
model <- lm(wage~bs(age, knots = c(25,40,60)), data = Wage)

#summary output corresponds to the betas for the intercept, x, x^2, x^3, and then the 3 knots (in order)
summary(model)
####### Note from Prof Murray #########  
# There are always 1 + p + k rows in the Coefficients report.
# The first is the intercept, the next p are the p polynomial powers, and the final k are the truncated power basis functions, one for each knot
######################  


# add the model and prediction interval to the plot
agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
pred=predict(model,newdata =list(age=age.grid),se=T) 
lines(age.grid ,pred$fit ,lwd=2) 
lines(age.grid ,pred$fit +2*pred$se ,lty="dashed") 
lines(age.grid ,pred$fit -2*pred$se ,lty="dashed")

# can also place knots uniformly by specifying df# 
dim(bs(age ,knots=c(25,40,60))) 
#[1] 3000 6
#######Murray#########  The bs() function creates a matrix that will act as the X's in the linear regression. There's one row for each observation
######################  in the original data (3000) and one column for each p + k, as above (3 degrees for the polynomial + 3 knots). If you specify
######################  intercept=T, you also get an extra column.

dim(bs(age ,df=6)) # b/c 3 knots means 6 df (K+4 = 3+4 = 7, minus one for the intercept = 6)
#[1] 3000 6 
attr(bs(age ,df=6) ,"knots") # get the knots attribute from the basis function matrix
#25% 50% 75% 
#33.8 42.0 51.0



# natural spline:
# fit a natural spline with 4 degrees of freedom
model2 <- lm(wage~ns(age, df=4), data = Wage)

# add the model to the plot
pred2=predict (model2 ,newdata=list(age=age.grid),se=T) 
lines(age.grid , pred2$fit ,col="red",lwd=2)




library(boot)

### using glm for cross-validation:
set.seed(1)
degree_freedom=4:14
cv.error=rep(0,11)
for(d in degree_freedom){
  glm.fit = glm(wage~bs(age, df = d), data=Wage)
  cv.error[d-3] = cv.glm(Wage,glm.fit,K=10)$delta[1] # delta[1] is raw cross-validation etimate of prediction error, [2] is adjusted
}

min(cv.error) # gives the min cv error
match(min(cv.error), cv.error) # gives the location of the min cv error in the cv.error list
match(min(cv.error),cv.error) + 3 # gives the df based on that location (from using d-3 before)
# in this case looks like 5 df is best, which is 2 knots (K + 4 - 1_for_intercept = 5, so K = 2) (could also check with summary)

# model the spline using the chosen degrees of freedom
model <- lm(wage~bs(age, df = 5), data = Wage)
plot(age, wage, col = 'gray', main = 'Wages for People of Different Ages')

agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2], length.out = 100)
pred=predict (model,newdata =list(age=age.grid),se=T) 
lines(age.grid ,pred$fit ,lwd=2) 











########################## Really awesome titanium data example from the powerpoint ###########################

rm(list=ls())
library(splines)
t.data<- read.table("titanium.txt", header = TRUE)
str(t.data)
head(t.data)
plot(x = t.data$x, y=t.data$y, xlab = "Temperature", ylab = "Phys.Prop")

titanium.lm2<- lm(y~poly(x,2),data = t.data)
summary(titanium.lm2)
plot(t.data$x, t.data$y, col="black", xlab = "Temperature", ylab = "Phys.Prop")
lines(t.data$x, predict(titanium.lm2), col="blue") 


titanium.lm5<- lm(y~poly(x,5),data = t.data)
plot(t.data$x, t.data$y, col="black", xlab = "Temperature", ylab = "Phys.Prop")
lines(t.data$x, predict(titanium.lm5), col="blue") 


titanium.lm21<- lm(y~poly(x,21),data = t.data)
plot(t.data$x, t.data$y, col="black", xlab = "Temperature", ylab = "Phys.Prop")
lines(t.data$x, predict(titanium.lm21), col="blue") 
summary(titanium.lm21)


model<- glm(y~ns(x, knots=c(825, 885, 895, 905, 990)),data = t.data )
plot(t.data$x, t.data$y, col="black", xlab = "Temperature", ylab = "Phys.Prop")
lines(t.data$x, predict(model), col="blue")




################### SO COOL ################################################################




















### Very cool example, shows how to make equally spaced knots by distance rather than by distribution:

###########  From http://people.stat.sc.edu/Hitchcock/  #############################


# R code to analyze the simulated (X,Y) data
# using spline methods

# Save the data file into a directory and 
# use the full path name:
xy_data <- read.table(file = "simulated101datapairs.txt", 
                      header=FALSE, col.names = c('x', 'y'))

# attaching the data frame:
attach(xy_data) 

# First look at a scatter plot of Y against X:
plot(xy_data$x, xy_data$y)

# There appears to be a relationship between Y and X, but it is
# not a conventional functional relationship

############### (Cubic) Regression Splines ######################

# Load the "splines" package:
library(splines)

# The bs() function in R produces B-splines, a computationally efficient 
# way to compute cubic regression splines

######################################
# Specifying 4 equally spaced knots:

number.knots <- 4
spacings <- seq(from=min(x),to=max(x),length=number.knots+2)[2:(number.knots+1)]
regr.spline <- lm(y ~ bs(x, df = NULL, knots=spacings, degree = 3, intercept=T)) # why would you use intercept=T vs F?????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
#######Murray#########  The bs() function is used internally by many other splice-related functions, some of which need a 'first' column for
######################  the intercept and others that don't. In the case of regression splines, you shouldn't use it as it confuses the predict() function (see 
######################  answer below about error messages). the bs() call could have just been bs(x, knots=spacings)

# plotting the data with the regression spline overlain:
x.values <- seq(from=min(x), to=max(x), length=200)
plot(x, y)
lines(x.values, predict(regr.spline, newdata=data.frame(x=x.values))) # what's the deal with the warning message??????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
#######Murray#########  Caused by the intercept=T parameter in the bs() function above

######################################
# Specifying 10 equally spaced knots:

number.knots <- 10
spacings <- seq(from=min(x),to=max(x),length=number.knots+2)[2:(number.knots+1)]
regr.spline <- lm(y ~ bs(x, df = NULL, knots=spacings, degree = 3, intercept=T))
# plotting the data with the regression spline overlain:
x.values <- seq(from=min(x), to=max(x), length=200)
plot(x, y); lines(x.values, predict(regr.spline, newdata=data.frame(x=x.values)))
######################################
# We can specify unequally spaced knots.  For example, we could put more 
# knots in the "wiggly" region of the data (from x=0.5 to x=1.0) and 
# fewer knots in the "flat" region of the data:

regr.spline <- lm(y ~ bs(x, df = NULL, knots=c(0.25, 0.5, 0.6, 0.7, 0.8, 0.9), degree = 3, intercept=T))
x.values <- seq(from=min(x), to=max(x), length=200)
plot(x, y)
lines(x.values, predict(regr.spline, data.frame(x=x.values)))


# Seems to work well for this function!!



















######### Spline-based Regression with the Old Faithful Data Set #######
#########  More information at same resource as above; http://people.stat.sc.edu/Hitchcock/ ##########


# The X variable here is the length of time (in minutes) it takes for the geyser to erupt.
# The Y variable is the waiting time until the next eruption.

# attaching the data frame from ISLR package:
attach(faithful)

# look at variables

# plot--why use a spline here?
plot(eruptions, waiting, main = "Wait Time and Length of Last Eruption")


##################################
# A regression spline with relatively more knots placed where there is denser data:

regr.spline.OF <- lm(waiting ~ bs(eruptions, 
                                  knots=c(1.9, 2.2, 2.5, 3.0, 3.6, 4.1, 4.6), degree = 3))
x.values <- seq(from=min(eruptions), to=max(eruptions), length=200)
lines(x.values, predict(regr.spline.OF, data.frame(eruptions=x.values)))







#?#?#?# Questions with Old Faithful data set #?#?#?#

# plot eruptions on the x-axis and waiting on the y-axis



# fit a cubic spline with knots at 2 and 4, with waiting depending on eruptions

# add the model line to the plot



# fit a natural cubic spline with 6 df, with waiting depending on eruptions

# add the model line to the plot in blue







