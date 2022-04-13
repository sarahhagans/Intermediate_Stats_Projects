library("dplyr")
library("rcompanion")
library("car")
library("ggplot2")
library("IDPmisc")

#Does the average price of avocados differ between Albany, Houston, and Seattle?
plotNormalHistogram(avocados$region)

avocado2 <- na.omit(avocados %>% filter(region %in% c("Albany", "Houston", "Seattle")))
avocados$AveragePrice <- as.numeric(avocados$AveragePrice)

plotNormalHistogram(avocado2$AveragePrice)
AveragePriceSQRT <- sqrt(avocados$AveragePrice)
AveragePriceLOG <- log(avocados$AveragePrice)
plotNormalHistogram(AveragePriceLOG)

#Use the LOG!

#Test for Homogeneity

bartlett.test(AveragePrice ~ region, data=avocados)
fligner.test(AveragePrice ~ region, data=avocados)

#Not Homogenous ?must fix?

ANOVA <- lm(AveragePrice ~ region, data=avocado2)
Anova(ANOVA, Type="II", white.adjust=TRUE)
#significant will try ppost hocs when Homogenteity is violated


pairwise.t.test(avocado2$AveragePrice, avocado2$region, p.adjust="bonferroni", pool.sd = FALSE)
#Post Hocs

#means Houston avocados are super cheap at $1.05 Albany is high at $1.56 and Seattle is $1.44
#it is odd that $1.44 is significantly different than $1.56.
avocado2Means <- avocado2 %>% group_by(region) %>% summarize(Mean = mean(AveragePrice))
