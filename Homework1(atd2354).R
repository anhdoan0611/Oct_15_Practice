data <- read.csv(file.choose())

#QUESTION 1:

#Calculating the number of approved bonds, and the number of defeated ones:
table(data$Result)

#Creating a subset that contains only the approved bonds:
approved <- data[data$Result == "Carried",]

#Calculating the number of approved bonds across the four government types:
table(approved$Type)

#Calculating the rates of approved bonds across the four government types:
nrow(approved[approved$Type =="CITY",])/nrow(data[data$Type == "CITY",])
nrow(approved[approved$Type =="ISD",])/nrow(data[data$Type == "ISD",])
nrow(approved[approved$Type =="WD",])/nrow(data[data$Type == "WD",])
nrow(approved[approved$Type =="COUNTY",])/nrow(data[data$Type == "COUNTY",])

#QUESTION 2:

#Calculating the new variable called "Votes_Total"
data$Votes_Total <- data$VotesFor + data$VotesAgainst

#Finding the row number of the bond with the highest turnover:
which(grepl(max(data$Votes_Total), data$Votes_Total)) 

#Accessing the data associated with that bond's presidential election:
data[1072,]

#QUESTION 3:

#Creating the subset that contains the approved measure bonds with at least 100 total votes:
subset <- data[data$Result == "Carried" & data$Votes_Total>=100,]

#Creating a new variable that gives the percentage of total votes that were for the bond measure (in %)
subset$Percentage <- (subset$VotesFor/subset$Votes_Total)*100

#Creating a graph of the distribution
boxplot(subset$Percentage, main = "Percentage of total votes that were for the bond", col = "#2c7fb8", xlab = "Percentage (%)", horizontal = TRUE)

#Summary of the percentage of total votes that were for the bond:
#Five-number summary (min, Q1, median, Q3, max):
fivenum(subset$Percentage)

#Inner-quartile range (Q3-Q1)
IQR(subset$Percentage)

#QUESTION 4:

#Converting into millions unit:
subset$Amount <- subset$Amount/1000000

#Graphing the relationship
plot(subset$Percentage, subset$Amount, ylab = "Cost (in millions)", xlab = "Percentage of approved votes", ylim = c(0,3500), pch=20, main = "The relationship between margin and cost", col = "orange")

#Find the correlation
cor(subset$Percentage, subset$Amount)

print("Hello World")



