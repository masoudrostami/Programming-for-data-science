
# Bike Share Project

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

View(chi)
dim(ny)
dim(wash)
dim(chi)

library(ggplot2)
library(pastecs)
library(tidyverse)
library(gridExtra)

##Discriptive summary

str(ny)
str(wash)
str(chi)

#

summary(ny)
summary(wash)
summary(chi)


## Unfortunetly, in the washington data we are missing some variables.


#### #Question 1: Is there any different in trip duration between female and male in New York and Chicago? and
#how we can compare them together? 

##  #xploring the data



objects(ny) 
objects(chi) 

#

ny1=na.omit(ny)
chi1=na.omit(chi)
#

by(ny1$Trip.Duration, ny1$Gender, summary) 
by(chi1$Trip.Duration, chi1$Gender, summary) 




#### Usin  Histogram to find the relationshib between the Trip duration and gender in New York and Chicago

Ny.f=subset(ny1, Gender=="Female")
Ny.m=subset(ny1, Gender=="Male")
chi.f=subset(chi1, Gender=="Female")
chi.m=subset(chi1, Gender=="Male")



par(mfrow = c(2, 2))  # Set up a 2 x 2 plotting space

# Create the loop.vector (all the columns)



hist(Ny.f$Trip.Duration,border="brown", breaks=5000, xlim=c(0,5000),  main="Trip duration in New York in Females", xlab="Trip duration")
hist(Ny.m$Trip.Duration, border="gray", breaks=50000, xlim=c(0,5000),  main="Trip duration in New York in Males", xlab="Trip duration")
hist(chi.f$Trip.Duration,border="blue", breaks=5000, xlim=c(0,5000),  main="Trip duration in Chicago in Females", xlab="Trip duration")
hist(chi.m$Trip.Duration, border="orange", breaks=5000, xlim=c(0,5000),  main="Trip duration in Chicago  in Male", xlab="Trip duration")


### 
# First start with trip duration by different genders in New York


ggplot(data=subset(ny, !is.na(Gender)), aes(x=Gender, y=Trip.Duration))+geom_boxplot(aes(color=Gender))+ 
        coord_cartesian(ylim = c(0,2000))+ggtitle("Trip duraion by Gender in New york")+labs(y="Trip duration", x = "Gender")

# Second start with trip duration by different genders in Chicago
ggplot(data=subset(chi, !is.na(Gender)), aes(x=Gender, y=Trip.Duration))+geom_boxplot(aes(color=Gender))+ 
        coord_cartesian(ylim = c(0,2000))+ggtitle("Trip duraion by Gender in Chicago")+labs(y="Trip duration", x = "Gender")



# Conclusion:
# It seems in New York and Chicago, female are more interested to biking in Bike share company, 
#and in the New York it is more common than Chicago. Female has the highest mean and median compare to the males.
#Also, our histgram and boxplot sshow in both states the bike sharing is most popular in Female than men. 
# it seems females are enjoying biking and having more fun.









#Questions 2:
# Which ages has the highest trip duration by using bike sharing? compare in New York and Chicago




# Create a function called phist to unserstand the age of customer using Bike share
phist <- function(x, ...) {
        hist(x,
             col = gray(.5, .2),
             border = "red",
             yaxt = "n",
             ylab = " ",
             ...)
        ci <- t.test(x)$conf.int
        top.text <- paste(
                "Mean = ", round(mean(x), 1),
                " (95% CI [", round(ci[1], 1),
                ", ", round(ci[2], 1), 
                "]), SD = ", round(sd(x), 1), 
                sep = " ")
        mtext(top.text, side = 3)
}
par(mfrow=c(2,1))
 
phist(ny1$Birth.Year, xlab = "Birth of the Year", main = "The Age of customers in New York", breaks=100)
phist(chi1$Birth.Year, xlab = "Birth of the year", main = "The Age of customers in Chicago", breaks=100)



####### Using ggplot

ggplot(data=ny1, aes(x=Birth.Year, y=Trip.Duration))+geom_point(alpha=0.05, position = position_jitter(h=0)) +
        coord_trans(y='sqrt') +geom_jitter(stat = 'summary',color="red", fun = "median")+ylim(0,5000)+xlim(1939, 2000)+
        ggtitle("Trip duraion by Ages in New York")+labs(y="Trip duration", x = "Birth of the Year")



ggplot(data=chi1, aes(x=Birth.Year, y=Trip.Duration))+geom_point(alpha=0.05, position = position_jitter(h=0)) +
        coord_trans(y='sqrt') +geom_jitter(stat = 'summary',color="red", fun = "median")+ylim(0,5000)+xlim(1939, 2000)+
        ggtitle("Trip duraion by Ages in New York")+labs(y="Trip duration", x = "Birth of the Year")




#### Models


Model1=lm(Trip.Duration~Birth.Year, data=chi)
summary(Model1)


Model2=lm(Trip.Duration~Birth.Year, data=ny)
summary(Model2)


####  Conclusion:
#It seems thae average age of birth of individuals using Bike share in New York is 1978 (Average 22 Years old) and in Chicago
# is 1981 (Average 19 years old) that show more younger people in Chicago are interested inn using Bike.
#Also, I did Linear model by using lm function and based on that both have 
#high p-value that show we should ratin our null hypothesis mean there is not significant difference
#between trip duration in different years. However, the p-value in chicaco is lower compare to
#New york that may show in chicago may age have higher effect on trip duraion compare to New York.




#Question3: 
#Is there any different between user type and their trip duration between New York and Chicago?

customer_ny=subset(ny1, User.Type=="Customer") #Subset the data for customer user type in New York
Subscriber_ny=subset(ny1, User.Type=="Subscriber") #Subset the data for Subscriber user type in New York
customer_chi=subset(chi1, User.Type=="Customer", na.rm=TRUE ) #Subset the data for customer user type in Chicago
Subscriber_chi=subset(chi1, User.Type=="Subscriber") #Subset the data for Subscriber  user type in Chicago

## # Create the function mean_my()

mean_my <- function(x) {
        output<- sum(x) / length(x) 
        return(output) 
        
}


mean_my(customer_ny$Trip.Duration)
mean_my(Subscriber_ny$Trip.Duration)
mean_my(customer_chi$Trip.Duration)
mean_my(Subscriber_chi$Trip.Duration)

# Trip duraion by User type in New York


a=ggplot(data=ny, aes(x=Trip.Duration))+geom_histogram(binwidth=10, alpha=0.2, col="blue")+facet_wrap(~User.Type)+xlim(0, 4000)+
        ggtitle("Trip duraion by User type in New York")+labs(y="Trip duration", x = "User Type")

#Trip duraion by User type in Chicago
b=ggplot(data=chi, aes(x=Trip.Duration))+geom_histogram(binwidth=10, alpha=0.2, col="red")+facet_wrap(~User.Type)+xlim(0, 4000)+
        ggtitle("Trip duraion by User type in Chicago")+labs(y="Trip duration", x = "User Type")



grid.arrange(a,b , ncol = 1, nrow = 2)


## Models
model.ny=(lm(Trip.Duration~User.Type, data=ny))
summary(model.ny)

model.chi=(lm(Trip.Duration~User.Type, data=chi))
summary(model.chi)

# Conclusion
#It seems people that subscribe with Bikeshare company in New Yorka and Chicago are 
#using more bikes compare to people with no subscirption, and  have the highest trip dutaion time.
#Also linear model show the huge significant differnt between subscribe and no subscribe
# trip duration due to very small p-value (< 2.2e-16) and high F-value. Also, our graph show that individuals with subscription in New York
# are more than Chicago.





