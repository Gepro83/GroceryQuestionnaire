#load the data (manually transformed to .csv before)
groc.orig.df <- read.csv(file.choose(),header=TRUE, sep=";")

#first instpection
str(groc.orig.df)
summary(groc.orig.df)

#################
##preprocessing##
#################
#create a new dataframe with processed data
#eliminate unncessary or redundant columns 

groc.df <- data.frame(lapply(groc.orig.df[, c(4, 6, 8, 10, 12, 14)], factor)) #one column for a yes/no question is sufficient
levels(groc.df[, 1]) <- c("No", "Yes")
levels(groc.df[, 2]) <- c("No", "Yes")
levels(groc.df[, 3]) <- c("No", "Yes")
levels(groc.df[, 4]) <- c("No", "Yes")
levels(groc.df[, 5]) <- c("No", "Yes")
levels(groc.df[, 6]) <- c("No", "Yes")

groc.df$Lebensmitt_00016 <- as.factor(#recode to a single factor column
  groc.orig.df$Lebensmitt_00016.I.never.purchase.groceries +
  2 * groc.orig.df$Lebensmitt_00016.Less.than.once.a.month +
  3 * groc.orig.df$Lebensmitt_00016.Once.a.month +
  4 * groc.orig.df$Lebensmitt_00016.Twice.a.month +
  5 * groc.orig.df$Lebensmitt_00016.Three.times.a.month +
  6 * groc.orig.df$Lebensmitt_00016.Once.in.a.week +
  7 * groc.orig.df$Lebensmitt_00016.More.than.once.in.a.week)
levels(groc.df$Lebensmitt_00016)[1] <- "never"
levels(groc.df$Lebensmitt_00016)[2] <- "less than once a month"
levels(groc.df$Lebensmitt_00016)[3] <- "once a month"
levels(groc.df$Lebensmitt_00016)[4] <- "twice a month"
levels(groc.df$Lebensmitt_00016)[5] <- "three times a month"
levels(groc.df$Lebensmitt_00016)[6] <- "once a week"
levels(groc.df$Lebensmitt_00016)[7] <- "more than once a week"

groc.df$Lebensmitt_00017 <- #recode to a single column
  groc.orig.df$Lebensmitt_00017.1 +
  2 * groc.orig.df$Lebensmitt_00017.2 +
  3 * groc.orig.df$Lebensmitt_00017.3 +
  4 * groc.orig.df$Lebensmitt_00017.4 +
  5 * groc.orig.df$Lebensmitt_00017.5 +
  6 * groc.orig.df$Lebensmitt_00017.6 +
  7 * groc.orig.df$Lebensmitt_00017.7

groc.df[, 9:20] <- groc.orig.df[, 30:41] #these columns are fine

groc.df$Lebensmitt_00025 <- as.factor(#recode to a single factor column
  groc.orig.df$Lebensmitt_00025.less.than.5.Minutes +
  2 * groc.orig.df$Lebensmitt_00025.6.10.Minutes +
  3 * groc.orig.df$Lebensmitt_00025.11.20.Minutes +    
  4 * groc.orig.df$Lebensmitt_00025.21.30.Minutes +
  5 * groc.orig.df$Lebensmitt_00025.more.than.30.Minutes)
levels(groc.df$Lebensmitt_00025)[1] <- "less than 5 minutes"
levels(groc.df$Lebensmitt_00025)[2] <- "6 to 10 minutes"
levels(groc.df$Lebensmitt_00025)[3] <- "11 to 20 minutes"
levels(groc.df$Lebensmitt_00025)[4] <- "21 to 30 minutes"
levels(groc.df$Lebensmitt_00025)[5] <- "more than 30 minutes"

groc.df[, 22:36] <- lapply(groc.orig.df[, 47:61], factor) #convert to factor
#inverse the ranking questions so that a higher value stands for a higher ranking
groc.df[, 37:45] <- 1/groc.orig.df[, 62:70]
groc.df[, 46:51] <- 1/groc.orig.df[, 73:78] 

groc.df[, 52:53] <- groc.orig.df[, 79:80] #these columns are fine

groc.df[, 54:72] <- lapply(groc.orig.df[, 81:99], factor) #convert to factor    

groc.df$Online_00049 <- as.factor(#recode to a single factor column
  groc.orig.df$Online_00049.Several.times.a.week +
  2 * groc.orig.df$Online_00049.Weekly +
  3 * groc.orig.df$Online_00049.Several.times.a.month +
  4 * groc.orig.df$Online_00049.Monthly +
  5 * groc.orig.df$Online_00049.Every.2.3.months +
  6 * groc.orig.df$Online_00049.2.3.times.a.year +
  7 * groc.orig.df$Online_00049.Less.often +
  8 * groc.orig.df$Online_00049.Never)
levels(groc.df$Online_00049)[1] <- "several times a week"
levels(groc.df$Online_00049)[2] <- "weekly"
levels(groc.df$Online_00049)[3] <- "several times a month"
levels(groc.df$Online_00049)[4] <- "monthly"
levels(groc.df$Online_00049)[5] <- "every 2-3 months"
levels(groc.df$Online_00049)[6] <- "2-3 times a year"
levels(groc.df$Online_00049)[7] <- "less often"
levels(groc.df$Online_00049)[8] <- "never"

groc.df[, 74:85] <- groc.orig.df[, 108:119] #these columns are fine    

groc.df$Gender <- as.factor(groc.orig.df$Demographi_00036.female) #convert to factor
levels(groc.df$Gender)[1] <- "male"
levels(groc.df$Gender)[2] <- "female"

groc.df$Demographi_00037 <- groc.orig.df$Demographi_00037 #this column is fine

groc.df$Demographi_00039 <- as.factor(#recode to a single factor column
  groc.orig.df$Demographi_00039.living.very.comfortably +
  2 * groc.orig.df$Demographi_00039.living.comfortably +
  3 * groc.orig.df$Demographi_00039.getting.along +
  4 * groc.orig.df$Demographi_00039.getting.along.hardly +
  5 * groc.orig.df$Demographi_00039.not.getting.along)
levels(groc.df$Demographi_00039)[1] <- "living very comfortably"
levels(groc.df$Demographi_00039)[2] <- "living comfortably"
levels(groc.df$Demographi_00039)[3] <- "getting along"
levels(groc.df$Demographi_00039)[4] <- "getting along hardly"
levels(groc.df$Demographi_00039)[5] <- "not getting along"

groc.df$Demographi_00053 <- as.factor(#recode to a single factor column
  groc.orig.df$Demographi_00053.I.have.very.much.time.for.myself +
  2 * groc.orig.df$Demographi_00053.I.have.much.time.for.myself +
  3 * groc.orig.df$Demographi_00053.I.have.enough.time.for.myself +
  4 * groc.orig.df$Demographi_00053.I.have.hardly.any.time.for.myself +
  5 * groc.orig.df$Demographi_00053.I.have.no.time.for.myself)
levels(groc.df$Demographi_00053)[1] <- "I have very much time for myself"
levels(groc.df$Demographi_00053)[2] <- "I have much time for myself"
levels(groc.df$Demographi_00053)[3] <- "I have enough time for myself"
levels(groc.df$Demographi_00053)[4] <- "I have hardly any time for myself"
levels(groc.df$Demographi_00053)[5] <- "I have no time for myself"

groc.df$Demographi_00050 <- as.factor(#recode to a single factor column
  groc.orig.df$Demographi_00050.1010 +
  2 * groc.orig.df$Demographi_00050.1020 +
  3 * groc.orig.df$Demographi_00050.1030 +
  4 * groc.orig.df$Demographi_00050.1040 +
  5 * groc.orig.df$Demographi_00050.1050 +
  6 * groc.orig.df$Demographi_00050.1060 +
  7 * groc.orig.df$Demographi_00050.1070 +
  8 * groc.orig.df$Demographi_00050.1080 +
  9 *  groc.orig.df$Demographi_00050.1090 +
  10 * groc.orig.df$Demographi_00050.1100 +
  11 * groc.orig.df$Demographi_00050.1110 +
  12 * groc.orig.df$Demographi_00050.1120 +
  13 * groc.orig.df$Demographi_00050.1130 +
  14 * groc.orig.df$Demographi_00050.1140 +
  15 * groc.orig.df$Demographi_00050.1150 +
  16 * groc.orig.df$Demographi_00050.1160 +
  17 * groc.orig.df$Demographi_00050.1170 +
  18 * groc.orig.df$Demographi_00050.1180 +
  19 * groc.orig.df$Demographi_00050.1190 +
  20 * groc.orig.df$Demographi_00050.1200 +
  21 * groc.orig.df$Demographi_00050.1210 +
  22 * groc.orig.df$Demographi_00050.1220 +
  23 * groc.orig.df$Demographi_00050.1230 +
  24 * groc.orig.df$Demographi_00050.not.in.Vienna)

groc.df$Demographi_00051 <- as.factor(#recode to a single factor column
  groc.orig.df$Demographi_00051.full.time.employed +
  2 * groc.orig.df$Demographi_00051.part.time.employed +
  3 * groc.orig.df$Demographi_00051.self.employed +
  4 * groc.orig.df$Demographi_00051.not.working +
  5 * groc.orig.df$Demographi_00051.homekeeper +
  6 * groc.orig.df$Demographi_00051.student +
  7 * groc.orig.df$Demographi_00051.retired)
levels(groc.df$Demographi_00051)[1] <- "full time employed"
levels(groc.df$Demographi_00051)[2] <- "part time employed"
levels(groc.df$Demographi_00051)[3] <- "self employed"
levels(groc.df$Demographi_00051)[4] <- "not working"
levels(groc.df$Demographi_00051)[5] <- "homekeeper"
levels(groc.df$Demographi_00051)[6] <- "student"
levels(groc.df$Demographi_00051)[7] <- "retired"

groc.df$Demographi_00052 <- as.factor(#recode to a single factor column
  groc.orig.df$Demographi_00052.none +
  2 * groc.orig.df$Demographi_00052.compulsory.school +
  3 * groc.orig.df$Demographi_00052.apprenticeship +
  4 * groc.orig.df$Demographi_00052.matriculation +
  5 * groc.orig.df$Demographi_00052.graduate)
levels(groc.df$Demographi_00052)[1] <- "none"
levels(groc.df$Demographi_00052)[2] <- "compulsory school"
levels(groc.df$Demographi_00052)[3] <- "apprenticeship"
levels(groc.df$Demographi_00052)[4] <- "matriculation"
levels(groc.df$Demographi_00052)[5] <- "graduate"



########################
##descriptive analysis##
########################

##univariate##

par(mfrow=c(2,2))
#plot(groc.df[, 1], main="I have learned before this survay about the \n possibility of shopping grocery products online")
plot(groc.df[, 2], main="I am interested in purchasing groceries online")
plot(groc.df[, 3], main="I already informed myself about shopping \n groceries online")
plot(groc.df[, 4], main="I am planning to shop groceries online \n within the next two months")
plot(groc.df[, 5], main="I already purchased groceries online.")
#plot(groc.df[, 6], main="I am satisfied with my online purchases of groceries")          
par(mfrow=c(1,1))
# (=> 1 and 6 probably useless)

par(mfrow=c(2,2))
plot(groc.df[, 7], cex.names = 0.8, las=2, main="How often do buy groceries personally")
plot(as.factor(groc.df[, 8]), main="On how many days of a week do you buy groceries personally")
hist(groc.df[, 9], main="On average, how much do you spend per week on groceries (in EURO)?", xlab="Euro")
plot(as.factor(groc.df[, 10]), main="For how many persons do you typically purchase groceries (including you)?") #outliers!
par(mfrow=c(1,1))

#Imagine a typical purchase of groceries. How often are the following products part of your purchase?(e.g., 1 = 10% of purchases)
par(mfrow=c(3,4))
hist(groc.df[, 11], xlab = "0 to 100% (10)", main = "Meat")
hist(groc.df[, 12], xlab = "0 to 100% (10)", main = "Dairy products")
hist(groc.df[, 13], xlab = "0 to 100% (10)", main = "Frozen food")
hist(groc.df[, 14], xlab = "0 to 100% (10)", main = "Fresh fruit")
hist(groc.df[, 15], xlab = "0 to 100% (10)", main = "Fresh vegetables")
hist(groc.df[, 16], xlab = "0 to 100% (10)", main = "Bread and pastries")
hist(groc.df[, 17], xlab = "0 to 100% (10)", main = "Beverages")
hist(groc.df[, 18], xlab = "0 to 100% (10)", main = "Convenience food")
hist(groc.df[, 19], xlab = "0 to 100% (10)", main = "Sausage and ham")
hist(groc.df[, 20], xlab = "0 to 100% (10)", main = "Tinned food")
par(mfrow=c(1,1))

plot(groc.df[, 21], main= "How much time do you require to your next possibility to purchase groceries?")

lbls <- c("by foot", "bicycle", "public transport", "car", "taxi", "motorcycle")
values <- c(table(groc.df[, 22])[2],
            table(groc.df[, 23])[2],
            table(groc.df[, 24])[2],
            table(groc.df[, 25])[2],
            0, #no one uses a taxi
            table(groc.df[, 27])[2])
barplot(values/531, names.arg = lbls, ylab = "Percentage of participants",
        main="How do you typically reach this location?") # some participants use multiple means

lbls <- c("Spar/Eurospar", "Hofer", "Billa", "Merkur", "Interspar", "Lidl", "Penny", "Adeg", "Denns Biomarkt")
values <- c(table(groc.df[, 28])[2],
            table(groc.df[, 29])[2],
            table(groc.df[, 30])[2],
            table(groc.df[, 31])[2],
            table(groc.df[, 32])[2],
            table(groc.df[, 33])[2],
            table(groc.df[, 34])[2],
            table(groc.df[, 35])[2],
            table(groc.df[, 36])[2])
barplot(values/531, names.arg = lbls, ylab = "Percentage of participants",
        main="At which of these grocery stores did you purchase \n at least once within the last year?") # some participants use multiple means

#Which shops do you prefer? Please rank.
#(1 is best)
par(mfrow=c(3,3))
hist(groc.df$Lebensmitt_00021.Spar...Eurospar, breaks=20, main="Spar/Eurospar", xlab="Ranking")
hist(groc.df$Lebensmitt_00021.Hofer, breaks=20, main="Hofer", xlab="Ranking")
hist(groc.df$Lebensmitt_00021.Billa, breaks=20, main="Billa", xlab="Ranking")
hist(groc.df$Lebensmitt_00021.Merkur, breaks=20, main="Merkur", xlab="Ranking")
hist(groc.df$Lebensmitt_00021.Interspar, breaks=20, main="Interspar", xlab="Ranking")
hist(groc.df$Lebensmitt_00021.Lidl, breaks=20, main="Lidl", xlab="Ranking")
hist(groc.df$Lebensmitt_00021.Penny, breaks=20, main="Penny", xlab="Ranking")
hist(groc.df$Lebensmitt_00021.Adeg, breaks=20, main="Adeg", xlab="Ranking")
hist(groc.df$Lebensmitt_00021.denns.Biomarkt, breaks=20, main="Dennis Biomarkt", xlab="Ranking")
par(mfrow=c(1,1))

#Please rank the following attributes describing an online purchase according to your personal preference
#(1 is best)
par(mfrow=c(2,3))
hist(groc.df$Attributreihung.Remaining.shelf.life, breaks=20, main="Remaining shelf life", xlab="Ranking")
hist(groc.df$Attributreihung.Delivery.day, breaks=20, main="Delivery day", xlab="Ranking")
hist(groc.df$Attributreihung.Time.window.width, breaks=20, main="Time window width", xlab="Ranking")
hist(groc.df$Attributreihung.Discount, breaks=20, main="Discount", xlab="Ranking")
hist(groc.df$Attributreihung.Delay.of.delivery, breaks=20, main="Delay of delivery", xlab="Ranking")
hist(groc.df$Attributreihung.Delivery.fee, breaks=20, main="Delivery fee", xlab="Ranking")
par(mfrow=c(1,1))

#Imagine you could delegate your purchase of groceries to a third person. This person will purchase 
#the items according to you shopping list at your favorite store and delivers the product to your home. 
#Your attendance is not required. What are you willing to pay for such a service (in Euro)?
par(mfrow=c(2,1))
hist(groc.df$Online_00064, breaks=50, main="How much would you pay for a delivery service from a third person?", xlab="Euro", xaxt="n")
axis(1, at=0:20*10)
hist(groc.df$Online_00065.Inâ....of.my.purchases, xlab="Frequency of serviceuse", breaks=20,
     main="How often would you use such a service \n (1=10% of your purchases 10=100% of your purchases)")
par(mfrow=c(1,1))

lbls <- c("00:00-02:00",
          "02:00-04:00",
          "04:00-06:00",
          "06:00-08:00",
          "08:00-10:00",
          "10:00-12:00",
          "12:00-14:00",
          "14:00-16:00",
          "16:00-18:00",
          "18:00-20:00",
          "20:00-22:00",
          "22:00-24:00")
values <- c(table(groc.df$Online_00047.00.00.02.00)[2],
            table(groc.df$Online_00047.02.00.04.00)[2],
            table(groc.df$Online_00047.04.00.06.00)[2],
            table(groc.df$Online_00047.06.00.08.00)[2],
            table(groc.df$Online_00047.08.00.10.00)[2],
            table(groc.df$Online_00047.10.00.12.00)[2],
            table(groc.df$Online_00047.12.00.14.00)[2],
            table(groc.df$Online_00047.14.00.16.00)[2],
            table(groc.df$Online_00047.16.00.18.00)[2],
            table(groc.df$Online_00047.18.00.20.00)[2],
            table(groc.df$Online_00047.20.00.22.00)[2],
            table(groc.df$Online_00047.22.00.24.00)[2])
par(mfrow=c(2,2), cex.axis=0.7)
barplot(values/531, names.arg = lbls, ylab = "Percentage of participants", las=2,
        main="For which time-windows are you able \n to receive your goods at home?")

lbls <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
values <- c(table(groc.df$Online_00066.Monday)[2],
            table(groc.df$Online_00066.Tuesday)[2],
            table(groc.df$Online_00066.Wednesday)[2],
            table(groc.df$Online_00066.Thursday)[2],
            table(groc.df$Online_00066.Friday)[2],
            table(groc.df$Online_00066.Saturday)[2],
            table(groc.df$Online_00066.Sunday)[2])
barplot(values/531, names.arg = lbls, ylab = "Percentage of participants", las=2,
        main="For which days are you able \n to receive your goods at home?")

plot(groc.df$Online_00049, las=2, main="How often do you purchase online in general \n (not only groceries)?", ylab="number of participants")
par(mfrow=c(1,1), cex.axis=1)

par(mfrow=c(4,3))
par(mar = rep(2, 4))
#Do you do those purchases/actions more often online (=1) or offline (=5)?
hist(groc.df$Online_00048.Bank.transactions, main="Bank transactions", xlab="Online(=1) to Offline(=5)")
hist(groc.df$Online_00048.Textiles, main="Textiles", xlab="Online(=1) to Offline(=5)")
hist(groc.df$Online_00048.Gambling...Betting, main="Gambling", xlab="Online(=1) to Offline(=5)")
hist(groc.df$Online_00048.Groceries, main="Groceries", xlab="Online(=1) to Offline(=5)")
hist(groc.df$Online_00048.Hotels...Flights, main="Hotels", xlab="Online(=1) to Offline(=5)")
hist(groc.df$Online_00048.Books...DVDs...eGames, main="Books, Dvds, eGames", xlab="Online(=1) to Offline(=5)")
hist(groc.df$Online_00048.Software...Hardware, main="Software, Hardware", xlab="Online(=1) to Offline(=5)")
hist(groc.df$Online_00048.Sporting.goods, main="Sporting goods", xlab="Online(=1) to Offline(=5)")
hist(groc.df$Online_00048.Food..Delivery.service...Take.away., main="Food, Delivery service", xlab="Online(=1) to Offline(=5)")
hist(groc.df$Online_00048.Tickets.for.events, main="Tickets for events", xlab="Online(=1) to Offline(=5)")
hist(groc.df$Online_00048.Electronics..TV...PC......., main="Electronics, TV, PC", xlab="Online(=1) to Offline(=5)")
par(mfrow=c(1,1))

#Demographics
par(mfrow=c(2,2))
boxplot(groc.df$Demographi_00035, main="Age of participants")
plot(groc.df$Gender, main="Gender of participants")
barplot(table(groc.df$Demographi_00037), main="Number of people in your household")
plot(groc.df$Demographi_00039, main="What describes your financial situation best?")

par(cex.axis=0.7)
plot(groc.df$Demographi_00053, main="What describes your available spare time best?")
par(cex.axis=1)
plot(groc.df$Demographi_00050, main="District of residence")
par(cex.axis=0.8)
plot(groc.df$Demographi_00051, main="Employment status")
plot(groc.df$Demographi_00052, main="Highest education")
par(mfrow=c(1,1), cex.axis=1)

##bivariate##

library(corrplot)
corrplot.mixed(cor(groc.df[,37:45]), upper="ellipse") #Hofer and Merkur different customers?
corrplot.mixed(cor(groc.df[,46:51]), upper="ellipse") #some weak correlations

removeNA <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
corrplot.mixed(cor(removeNA(groc.df, c("Lebensmitt_00020.Meat..incl..fish.and.poultry.",
                                       "Lebensmitt_00020.Dairy.products..incl..Eggs.and.cheese.",
                                       "Lebensmitt_00020.frozen.food",
                                       "Lebensmitt_00020.fresh.fruit",                           
                                       "Lebensmitt_00020.fresh.vegetables",
                                       "Lebensmitt_00020.bread.and.pastries",                    
                                       "Lebensmitt_00020.beverages",
                                       "Lebensmitt_00020.convenience.food",                      
                                       "Lebensmitt_00020.sausage.and.ham",
                                       "Lebensmitt_00020.tinned.food"))[, 11:20]),  upper="ellipse") #some strong correlations


corrplot.mixed(cor(removeNA(groc.df, c("Online_00048.Bank.transactions", 
                       "Online_00048.Textiles",
                       "Online_00048.Gambling...Betting",
                       "Online_00048.Groceries",
                       "Online_00048.Hotels...Flights",
                       "Online_00048.Books...DVDs...eGames",
                       "Online_00048.Software...Hardware",                       
                       "Online_00048.Sporting.goods",
                       "Online_00048.Food..Delivery.service...Take.away.",
                       "Online_00048.Tickets.for.events",
                       "Online_00048.Electronics..TV...PC......."))[,74:84]), upper="ellipse") #some stronger correlations


##################
###segmentation###
##################

library(cluster)                  

groc.dist <- daisy(groc.df[,7:84]) #first 6 question seem meaningless (?), leave demographics as descriptors


#hierarchical clustering
groc.hc <- hclust(groc.dist,method="ward.D")
plot(groc.hc, xlab="", sub="")
plot(groc.hc$height)
plot(groc.hc$height[515:530])
#=> 4 clusters
plot(groc.hc, xlab="", sub="")
rect.hclust(groc.hc, k=2, border="red")

groc.hc.segment <- cutree(groc.hc, k=2)
groc.hc.segment       # membership vector for 6 groups

##  amount of groupmembers 
table(groc.hc.segment)

boxplot(groc.df$Demographi_00035~groc.hc.segment) #Age of participants

groc.gender.num <- ifelse(groc.df$Gender=="female", 0, 1) #Gender
boxplot(groc.gender.num~groc.hc.segment) #does not say anything

boxplot(groc.df$Demographi_00037~groc.hc.segment, outline=FALSE) # Number of people in household

plot(groc.df$Demographi_00039, main="What describes your financial situation best?")
groc.finsit.num <- groc.df$Demographi_00039
levels(groc.finsit.num)[1] <- 1
levels(groc.finsit.num)[2] <- 2
levels(groc.finsit.num)[3] <- 3
levels(groc.finsit.num)[4] <- 4
levels(groc.finsit.num)[5] <- 5
groc.finsit.num <- as.numeric(groc.finsit.num)
boxplot(groc.finsit.num~groc.hc.segment) # does not say anything


#transform factors to numericals (?)
groc.num <- groc.df

levels(groc.num$Lebensmitt_00016)[1] <- 1
levels(groc.num$Lebensmitt_00016)[2] <- 2
levels(groc.num$Lebensmitt_00016)[3] <- 3
levels(groc.num$Lebensmitt_00016)[4] <- 4
levels(groc.num$Lebensmitt_00016)[5] <- 5
levels(groc.num$Lebensmitt_00016)[6] <- 6
levels(groc.num$Lebensmitt_00016)[7] <- 7
groc.num$Lebensmitt_00016 <- as.numeric(groc.num$Lebensmitt_00016)
boxplot(groc.num$Lebensmitt_00016~groc.hc.segment)
#no

levels(groc.num$Lebensmitt_00025)[1] <- 1
levels(groc.num$Lebensmitt_00025)[2] <- 2
levels(groc.num$Lebensmitt_00025)[3] <- 3
levels(groc.num$Lebensmitt_00025)[4] <- 4
levels(groc.num$Lebensmitt_00025)[5] <- 5
groc.num$Lebensmitt_00025 <- as.numeric(groc.num$Lebensmitt_00025)
boxplot(groc.num$Lebensmitt_00025~groc.hc.segment)
# eher nein

groc.num$Lebensmitt_00031.by.foot <- ifelse(groc.num$Lebensmitt_00031.by.foot=="0", 0, 1)
boxplot(groc.num$Lebensmitt_00031.by.foot~groc.hc.segment)
#no
groc.num$Lebensmitt_00031.bicylce <- ifelse(groc.num$Lebensmitt_00031.bicylce=="0", 0, 1)
boxplot(groc.num$Lebensmitt_00031.bicylce~groc.hc.segment)
#no
groc.num$Lebensmitt_00031.public.transport <- ifelse(groc.num$Lebensmitt_00031.public.transport=="0", 0, 1)
boxplot(groc.num$Lebensmitt_00031.public.transport~groc.hc.segment)
#no
groc.num$Lebensmitt_00031.car <- ifelse(groc.num$Lebensmitt_00031.car=="0", 0, 1)
boxplot(groc.num$Lebensmitt_00031.car~groc.hc.segment)
#no
groc.num$Lebensmitt_00031.taxi <- ifelse(groc.num$Lebensmitt_00031.taxi=="0", 0, 1)
boxplot(groc.num$Lebensmitt_00031.taxi~groc.hc.segment)
#no
groc.num$Lebensmitt_00031.motorcycle <- ifelse(groc.num$Lebensmitt_00031.motorcycle=="0", 0, 1)
boxplot(groc.num$Lebensmitt_00031.motorcycle~groc.hc.segment)
#no
groc.num$Lebensmitt_00063.Spar...Eurospar <- ifelse(groc.num$Lebensmitt_00063.Spar...Eurospar=="0", 0, 1)
boxplot(groc.num$Lebensmitt_00063.Spar...Eurospar~groc.hc.segment)
#nach supermärkte???

groc.num$Lebensmitt_00063.Hofer <- ifelse(groc.num$Lebensmitt_00063.Hofer=="0", 0, 1)
boxplot(groc.num$Lebensmitt_00063.Hofer~groc.hc.segment)
groc.num$Lebensmitt_00063.Billa <- ifelse(groc.num$Lebensmitt_00063.Billa=="0", 0, 1)
boxplot(groc.num$Lebensmitt_00063.Billa~groc.hc.segment)
groc.num$Lebensmitt_00063.Merkur <- ifelse(groc.num$Lebensmitt_00063.Merkur=="0", 0, 1)
boxplot(groc.num$Lebensmitt_00063.Merkur~groc.hc.segment)
groc.num$Lebensmitt_00063.Interspar <- ifelse(groc.num$Lebensmitt_00063.Interspar=="0", 0, 1)
boxplot(groc.num$Lebensmitt_00063.Interspar~groc.hc.segment)
groc.num$Lebensmitt_00063.Lidl <- ifelse(groc.num$Lebensmitt_00063.Lidl=="0", 0, 1)
boxplot(groc.num$Lebensmitt_00063.Lidl~groc.hc.segment)
groc.num$Lebensmitt_00063.Penny <- ifelse(groc.num$Lebensmitt_00063.Penny=="0", 0, 1)
boxplot(groc.num$Lebensmitt_00063.Penny~groc.hc.segment)
groc.num$Lebensmitt_00063.Adeg <- ifelse(groc.num$Lebensmitt_00063.Adeg=="0", 0, 1)
boxplot(groc.num$Lebensmitt_00063.Adeg~groc.hc.segment)
groc.num$Lebensmitt_00063.denns.Biomarkt <- ifelse(groc.num$Lebensmitt_00063.denns.Biomarkt=="0", 0, 1)
boxplot(groc.num$Lebensmitt_00063.denns.Biomarkt~groc.hc.segment)
#no...

groc.num$Online_00047.00.00.02.00 <- ifelse(groc.num$Online_00047.00.00.02.00=="0", 0, 1)
boxplot(groc.num$Online_00047.00.00.02.00~groc.hc.segment)
groc.num$Online_00047.02.00.04.00 <- ifelse(groc.num$Online_00047.02.00.04.00=="0", 0, 1)
boxplot(groc.num$Online_00047.02.00.04.00~groc.hc.segment)
groc.num$Online_00047.04.00.06.00 <- ifelse(groc.num$Online_00047.04.00.06.00=="0", 0, 1)
boxplot(groc.num$Online_00047.04.00.06.00~groc.hc.segment)
groc.num$Online_00047.06.00.08.00 <- ifelse(groc.num$Online_00047.06.00.08.00=="0", 0, 1)
boxplot(groc.num$Online_00047.06.00.08.00~groc.hc.segment)
groc.num$Online_00047.08.00.10.00 <- ifelse(groc.num$Online_00047.08.00.10.00=="0", 0, 1)
boxplot(groc.num$Online_00047.08.00.10.00~groc.hc.segment)
groc.num$Online_00047.10.00.12.00 <- ifelse(groc.num$Online_00047.10.00.12.00=="0", 0, 1)
boxplot(groc.num$Online_00047.10.00.12.00~groc.hc.segment)
groc.num$Online_00047.12.00.14.00 <- ifelse(groc.num$Online_00047.12.00.14.00=="0", 0, 1)
boxplot(groc.num$Online_00047.12.00.14.00~groc.hc.segment)
groc.num$Online_00047.14.00.16.00 <- ifelse(groc.num$Online_00047.14.00.16.00=="0", 0, 1)
boxplot(groc.num$Online_00047.14.00.16.00~groc.hc.segment)
groc.num$Online_00047.16.00.18.00 <- ifelse(groc.num$Online_00047.16.00.18.00=="0", 0, 1)
boxplot(groc.num$Online_00047.16.00.18.00~groc.hc.segment)
groc.num$Online_00047.18.00.20.00 <- ifelse(groc.num$Online_00047.18.00.20.00=="0", 0, 1)
boxplot(groc.num$Online_00047.18.00.20.00~groc.hc.segment)
groc.num$Online_00047.20.00.22.00 <- ifelse(groc.num$Online_00047.20.00.22.00=="0", 0, 1)
boxplot(groc.num$Online_00047.20.00.22.00~groc.hc.segment)
groc.num$Online_00047.22.00.24.00 <- ifelse(groc.num$Online_00047.22.00.24.00=="0", 0, 1)
boxplot(groc.num$Online_00047.22.00.24.00~groc.hc.segment)
#no

groc.num$Online_00066.Monday <- ifelse(groc.num$Online_00066.Monday=="0", 0, 1)
boxplot(groc.num$Online_00066.Monday~groc.hc.segment)
groc.num$Online_00066.Tuesday <- ifelse(groc.num$Online_00066.Tuesday=="0", 0, 1)
boxplot(groc.num$Online_00066.Tuesday~groc.hc.segment)
groc.num$Online_00066.Wednesday <- ifelse(groc.num$Online_00066.Wednesday=="0", 0, 1)
boxplot(groc.num$Online_00066.Wednesday~groc.hc.segment)
groc.num$Online_00066.Thursday <- ifelse(groc.num$Online_00066.Thursday=="0", 0, 1)
boxplot(groc.num$Online_00066.Thursday~groc.hc.segment)
groc.num$Online_00066.Friday <- ifelse(groc.num$Online_00066.Friday=="0", 0, 1)
boxplot(groc.num$Online_00066.Friday~groc.hc.segment)
groc.num$Online_00066.Saturday <- ifelse(groc.num$Online_00066.Saturday=="0", 0, 1)
boxplot(groc.num$Online_00066.Saturday~groc.hc.segment)
groc.num$Online_00066.Sunday <- ifelse(groc.num$Online_00066.Sunday=="0", 0, 1)
boxplot(groc.num$Online_00066.Sunday~groc.hc.segment)

levels(groc.num$Online_00049)[1] <- 1
levels(groc.num$Online_00049)[2] <- 2
levels(groc.num$Online_00049)[3] <- 3
levels(groc.num$Online_00049)[4] <- 4
levels(groc.num$Online_00049)[5] <- 5
levels(groc.num$Online_00049)[6] <- 6
levels(groc.num$Online_00049)[7] <- 7
levels(groc.num$Online_00049)[8] <- 8
groc.num$Online_00049 <- as.numeric(groc.num$Online_00049)
boxplot(groc.num$Online_00049~groc.hc.segment)
#online + age + number people in houshold??

#kmeans

groc.num[is.na(groc.num)] <- -99 #replace NAs so kmeans works (?)
groc.k <- kmeans(groc.num[, 7:84], centers=4, nstart = 80)

plot(groc.num$Gender ~ groc.k$cluster, ylab="Gender", xlab="Cluster") #gender
boxplot(groc.num$Demographi_00035 ~ groc.k$cluster, ylab="Gender", xlab="Cluster") #alter

# liefern kein ergebnis:
boxplot(groc.num$Lebensmitt_00031.by.foot ~ groc.k$cluster, ylab="Lebensmitt_00031.by.foot", xlab="Cluster")
boxplot(groc.num$Lebensmitt_00031.bicylce ~ groc.k$cluster, ylab="Lebensmitt_00031.bicylce", xlab="Cluster")
boxplot(groc.num$Lebensmitt_00031.public.transpor ~ groc.k$cluster, ylab="Lebensmitt_00031.public.transpor", xlab="Cluster")
boxplot(groc.num$Lebensmitt_00031.car ~ groc.k$cluster, ylab="Lebensmitt_00031.car", xlab="Cluster")
boxplot(groc.num$Lebensmitt_00031.taxi ~ groc.k$cluster, ylab="Lebensmitt_00031.taxi", xlab="Cluster")
boxplot(groc.num$Lebensmitt_00031.motorcycle ~ groc.k$cluster, ylab="Lebensmitt_00031.motorcycle", xlab="Cluster")
boxplot(groc.num$Lebensmitt_00063.Spar...Eurospar ~ groc.k$cluster, ylab="Lebensmitt_00063.Spar...Eurospar", xlab="Cluster")
boxplot(groc.num$Lebensmitt_00063.Hofer ~ groc.k$cluster, ylab="Lebensmitt_00063.Hofer", xlab="Cluster")
boxplot(groc.num$Lebensmitt_00063.Billa ~ groc.k$cluster, ylab="Lebensmitt_00063.Billa", xlab="Cluster")
boxplot(groc.num$Lebensmitt_00063.Merkur ~ groc.k$cluster, ylab="Lebensmitt_00063.Merkur", xlab="Cluster")
boxplot(groc.num$Lebensmitt_00063.Interspar ~ groc.k$cluster, ylab="Lebensmitt_00063.Interspar", xlab="Cluster")
boxplot(groc.num$Lebensmitt_00063.Lidl ~ groc.k$cluster, ylab="Lebensmitt_00063.Lidl", xlab="Cluster")
boxplot(groc.num$Lebensmitt_00063.Penny ~ groc.k$cluster, ylab="Lebensmitt_00063.Penny", xlab="Cluster")
boxplot(groc.num$Lebensmitt_00063.Adeg ~ groc.k$cluster, ylab="Lebensmitt_00063.Adeg", xlab="Cluster")
boxplot(groc.num$Lebensmitt_00063.denns.Biomarkt ~ groc.k$cluster, ylab="Lebensmitt_00063.denns.Biomarkt", xlab="Cluster")

boxplot(groc.num$Online_00047.00.00.02.00 ~ groc.k$cluster, ylab="Online_00047.00.00.02.00", xlab="Cluster")
boxplot(groc.num$Online_00047.02.00.04.00 ~ groc.k$cluster, ylab="Online_00047.02.00.04.00", xlab="Cluster")
boxplot(groc.num$Online_00047.04.00.06.00 ~ groc.k$cluster, ylab="Online_00047.04.00.06.00", xlab="Cluster")
boxplot(groc.num$Online_00047.06.00.08.00 ~ groc.k$cluster, ylab="Online_00047.06.00.08.00", xlab="Cluster")
boxplot(groc.num$Online_00047.08.00.10.00 ~ groc.k$cluster, ylab="Online_00047.08.00.10.00", xlab="Cluster")
boxplot(groc.num$Online_00047.10.00.12.00 ~ groc.k$cluster, ylab="Online_00047.10.00.12.00", xlab="Cluster")
boxplot(groc.num$Online_00047.12.00.14.00 ~ groc.k$cluster, ylab="Online_00047.12.00.14.00", xlab="Cluster")
boxplot(groc.num$Online_00047.14.00.16.00 ~ groc.k$cluster, ylab="Online_00047.14.00.16.00", xlab="Cluster")
boxplot(groc.num$Online_00047.16.00.18.00 ~ groc.k$cluster, ylab="Online_00047.16.00.18.00", xlab="Cluster")
boxplot(groc.num$Online_00047.18.00.20.00 ~ groc.k$cluster, ylab="Online_00047.18.00.20.00", xlab="Cluster")
boxplot(groc.num$Online_00047.20.00.22.00 ~ groc.k$cluster, ylab="Online_00047.20.00.22.00", xlab="Cluster")
boxplot(groc.num$Online_00047.22.00.24.00 ~ groc.k$cluster, ylab="Online_00047.22.00.24.00", xlab="Cluster")

boxplot(groc.num$Online_00066.Monday ~ groc.k$cluster, ylab="Online_00066.Monday", xlab="Cluster")
boxplot(groc.num$Online_00066.Tuesday ~ groc.k$cluster, ylab="Online_00066.Tuesday", xlab="Cluster")
boxplot(groc.num$Online_00066.Wednesday ~ groc.k$cluster, ylab="Online_00066.Wednesday", xlab="Cluster")
boxplot(groc.num$Online_00066.Thursday ~ groc.k$cluster, ylab="Online_00066.Thursday", xlab="Cluster")
boxplot(groc.num$Online_00066.Friday ~ groc.k$cluster, ylab="Online_00066.Friday", xlab="Cluster")
boxplot(groc.num$Online_00066.Saturday ~ groc.k$cluster, ylab="Online_00066.Saturday", xlab="Cluster")
boxplot(groc.num$Online_00066.Sunday ~ groc.k$cluster, ylab="Online_00066.Sunday", xlab="Cluster")

