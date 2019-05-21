# Packages
install.packages("xts")
install.packages("psych")
install.packages("mlogit")
install.packages("dplyr")
install.packages("psych")

library(psych)
library(mlogit)
library(dplyr)
library(xts)
library(psych)

require(psych)
require(mlogit)
require(dplyr)
require(xts)
require(psych)



# Input data

july = read.csv("D:/July.csv")
nrow(july)
nrow(july)
names(july)
str(july)


# Subset for specific station

PSN = subset(july, start.station.name=="Pershing Square North" | end.station.name=="Pershing Square North",
             select = c("tripduration", "starttime", "stoptime", "usertype", "birth.year", "gender"))

# Sanity checks

nrow(PSN)
names(PSN)
str(PSN)
head(PSN)
describe(PSN)

levels(PSN$usertype)
table(PSN$gender)
table(PSN$birth.year)

PSN = subset(PSN, birth.year > 1930 )    # Born after 1930
PSN = subset(PSN, tripduration < 7201 )  # Trips lasting less than 2 hours


# Append fare

PSN$fare <- (3+12+24)/3

PSN <- within(PSN, fare[usertype=="Subscriber"] <- 169)

names(PSN) = c("TT","start", "end","choice", "age",
                 "gender","fare")

head(PSN)


# Clarify choice set
PSN <- PSN %>%
  filter(choice %in% c("Subscriber","Customer"))  %>%
  mutate(choice = factor(choice, labels = c("Subscriber","Customer")))

PSN <- subset(PSN, select = c("TT", "choice", "age", "gender", "fare"))

# MNL

#PSN.data <-c()
PSN.data  = mlogit.data(PSN, choice = "choice", shape = "wide")# , alt.levels = c("Subscriber","Customer"))

head(PSN.data)
str(PSN.data)

PSN.mnl <- mlogit(choice ~ 1 | age + gender + TT, 
                 data = PSN.data, reflevel = "Customer")

summary(PSN.mnl)




# Take 2: Travel Time as proxy for cost

# fare calculations

PSN$CO.Subscriber <- 0         # PSN$TT
PSN$CO.Customer   <- 0         # PSN$TT

PSN <- within(PSN, CO.Subscriber[choice=="Subscriber"] <-  3 + 4*(TT[choice=="Subscriber"]-2700)/900)
PSN <- within(PSN, CO.Subscriber[choice=="Customer"]   <-  3 + 4*(TT[choice=="Customer"]-2700)/900)

PSN <- within(PSN, CO.Customer[choice=="Customer"]   <-  3 + 4*(TT[choice=="Customer"]-1800)/900)
PSN <- within(PSN, CO.Customer[choice=="Subscriber"] <-  3 + 4*(TT[choice=="Subscriber"]-1800)/900)


names(PSN) = c("TT","choice", "age","gender","fare","CO.Subscriber", "CO.Customer")

head(PSN)


PSN <- within(PSN, CO.Subscriber[CO.Subscriber< 3] <-  3 )
PSN <- within(PSN, CO.Customer[CO.Customer< 3]     <-  3 )


# MNL

#PSN2.data <-c()
PSN2.data  = mlogit.data(PSN, choice = "choice", shape = "wide",
                         varying = 6:7)# , alt.levels = c("Subscriber","Customer"))

head(PSN2.data)
str(PSN2.data)


PSN2.mnl <- mlogit(choice ~ CO | age + gender + TT, 
                  data = PSN2.data)

summary(PSN2.mnl)




