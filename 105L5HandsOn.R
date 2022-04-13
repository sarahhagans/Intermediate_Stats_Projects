library("rcompanion")
library("fastR2")
library("car")
library("dplyr")


#Data Wrangling
honey$year <- as.character(honey$year)
honey$year <- as.factor(honey$year)

#Postively skewed
plotNormalHistogram(honey$totalprod)

#Log transformation looks great
plotNormalHistogram(log(honey$totalprod))

honey$totalprodLOG <- log(honey$totalprod)

#Check for Assumptions

#Passed assumption of homogenity of variance for normally distributed variable
leveneTest(totalprodLOG ~ year, data=honey)

#Run the Analysis
RManova <- aov(totalprodLOG~year+Error(state), honey)
summary(RManova)

RManova <- aov(log(totalprod)~year, honey)
summary(RManova)
