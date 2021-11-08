# Question 1 

# individual numbers
for(i in 10:99) {
  print(i^2+i^3)
}

#sum
total=0
for (i in 10:99) {
  total<-total+(i^2 + i^3)
}
total

#different method
i <- 10:99
sum(i^2+i^3)

# Question 2 
#simulate a single fair coin toss
ifelse(runif(1) < 0.5, "heads", "tails")

#100 times, count the total number of heads

n <- 100
total <- 0
coin_flip <- sample(c(0,1), n, replace = TRUE)
for(flip_i in 1:n)
{
  if(coin_flip[flip_i] == 1)
  {total = total + 1}
}
print(total)

#Question 3
#Simulate data for a class of 25
rnorm(25, 35, 10)

#Any problems? Yes. test scores can be higher than 50 by using this method

#Bonus: Round to the nearest 0.5
round(rnorm(25, 35, 10), 0)

#Question 4

#Part 1:
number <- 1:30
l <- "Label"
paste(l, number)

#Part 2:
number <- 1:30
f <- "fn"
paste0(f, number)

#Question 5

library(ggplot2)

X <- runif(20000)
df <- data.frame(rowSums(matrix(X, narrow =1000, ncol=20, byrow=F)))
M <- apply(df,2,mean)

ggplot(df, aes(x))+geom_histogram(aes(y=..density..), binwidth = 1, fill="blue") + geom_density() + stat_function(fun = dnorm,color="red", args = list(mean=M)) +ggtitle("Simulations")+ ylab("Density")
#Difference: At X=0, the curve reaches its maximum point however the histogram tends to show values that are lower than that.

#Question 6

1:10 > 5

# R is going through the numbers 1 to 10, one by one and assessing if its bigger than  5. If true, it returns TRUE, if false, it returns FALSE.

#Question 7

#Part 1: Load that data:
mydata <- read.csv("~/Downloads/churn.arff", header= FALSE, skip = 15)
names(mydata) <- c("COLLEGE", "INCOME", "OVERAGE", "LEFTOVER", "HOUSE", "HANDSET_PRICE", "OVER_15MINS_CALLS_PER_MONTH", "AVERAGE_CALL_DURATION", "REPORTED_SATISFACTION", "REPORTED_USAGE_LEVEL", "CONSIDERING_CHANGE_OF_PLAN", "LEAVE" )

#Part 2: 12 columns, 2000 rows

#Part3: Create a new variable IncomeGroup that characterizes users with as <$35k; $35k-$45k; $45k-$65k; $65k-$100k; $100k. Use the function cut() to divide the data into intervals.

mydata$IncomeGroup <- cut(mydata$INCOME, breaks = c(0,35000,45000,65000,100000,1000000), labels = c("<$35k", "$35k-$45k", "$45k-$65k", "$65k-$100k", "$100k<"))

#Part 4: Plot the distribution of OVERAGE for each of the income groups.

plot(mydata$IncomeGroup, mydata$OVERAGE, xlab= "Income Group", ylab= "Overage")

#Part 5: Explore the data with regard to LEAVE decisions and make at least one visual and quantitative comparisons across income groups and other variables to find
#out who is most likely to leave.

ggplot(data = mydata) +geom_bar(aes(IncomeGroup, fill=LEAVE))
ggplot(data = mydata) +geom_bar(aes(LEAVE, fill=IncomeGroup))


install.packages("plyr")
library(plyr)
count(mydata$LEAVE)
count(mydata$IncomeGroup)

mydata$LEAVE_SCORE <- ifelse(mydata$LEAVE == "LEAVE", "1", "0")
mydata$LEAVE_SCORE <- as.numeric(mydata$LEAVE_SCORE)

summary(mydata$LEAVE_SCORE)

#Comment: Customers within the "$100<" income group are more likely to leave than stay. All other income groups are more likely to stay than leave.

#Part 6: Summary
mydata$CONSIDERING_CHANGE_OF_PLAN_SCORE <- ifelse(mydata$CONSIDERING_CHANGE_OF_PLAN == "no", "1", ifelse(mydata$CONSIDERING_CHANGE_OF_PLAN == "never_thought", "2", ifelse(mydata$CONSIDERING_CHANGE_OF_PLAN == "perhaps", "3", ifelse(mydata$CONSIDERING_CHANGE_OF_PLAN == "considering", "4", "5"))))
mydata$REPORTED_USAGE_LEVEL_SCORE <- ifelse(mydata$REPORTED_USAGE_LEVEL == "very_little", "1", ifelse(mydata$REPORTED_USAGE_LEVEL == "little", "2", ifelse(mydata$REPORTED_USAGE_LEVEL == "avg", "3", ifelse(mydata$REPORTED_USAGE_LEVEL == "high", "4", "5"))))
mydata$REPORTED_SATISFACTION_SCORE <- ifelse(mydata$REPORTED_SATISFACTION == "very_unsat", "1", ifelse(mydata$REPORTED_SATISFACTION == "unsat", "2", ifelse(mydata$REPORTED_SATISFACTION == "avg", "3", ifelse(mydata$REPORTED_SATISFACTION == "sat", "4", "5"))))

mydata$CONSIDERING_CHANGE_OF_PLAN_SCORE <- as.numeric(mydata$CONSIDERING_CHANGE_OF_PLAN_SCORE)
mydata$REPORTED_USAGE_LEVEL_SCORE <- as.numeric(mydata$REPORTED_USAGE_LEVEL_SCORE)
mydata$REPORTED_SATISFACTION_SCORE <- as.numeric(mydata$REPORTED_SATISFACTION_SCORE)

summary(mydata)

#Findings based on the data provided:
#1: Majority of their customers reported an income of $100k or more, however they are also more likely to Leave than Stay.
#2: 49.26% of customers are likely to leave than stay.
#3: The average "Considering Change of Plan Score" is 3.392 which also indicates that customers are actively considering changing their plans.
#4 Their mean satisfaction score is 2.566 out of 5which translates to "avg".
#5: The mean of usage level score is 2.815 out of 5 which indicates that customers are reporting an average use of services.
#In conclusion they are receiving average ratings in terms of satisfaction and usages which in return has resulted in 49.26% of customers leaving and most of them considering to leave.
