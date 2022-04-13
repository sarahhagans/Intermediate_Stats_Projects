library("rcompanion")
library("car")
library("dplyr")
library("IDPmisc")


# Number of suicides by generation by country, with country being the repeated factor
colnames(suicide)
# Check Assumptions

### Normality
plotNormalHistogram(suicide$"suicides/100k pop")

suicide$suicides.100k.popSQRT <- sqrt(suicide$"suicides/100k pop")
plotNormalHistogram(suicide$suicides.100k.popSQRT)

suicide$suicides.100k.popLOG <- log(suicide$"suicides/100k pop")

suicide4 <- NaRV.omit(suicide)

plotNormalHistogram(suicide4$suicides.100k.popLOG)

# USE THE LOG!!!

# Homogeneity of Variance

leveneTest(suicides.100k.popLOG ~ generation, data=suicide4)

#### This failed the assumption (p-value is significant), but proceed anyway for learning purposes :?

### Sample size -you have more than enough data almost 30,000

## Run the analysis

RManova1 <- aov(suicides.100k.popLOG~(generation*year)+Error(country-year/(year)), suicide4)
summary(RManova1)

### Looks like there is a generational effect to suicide, and an interaction to how the year has affected the generation

## Post hocs

pairwise.t.test(suicide4$suicides.100k.popLOG, suicide4$generation, p.adjust="bonferroni")

### Looks like there is a difference in suicide rates among ALL the generations. very small numbers here.

## Determine Means and Draw Conclusions

suicideMeans <- suicide4 %>% group_by(generation, year) %>% summarize(Mean=mean(`suicides/100k pop`))

# Generation Z is the least likely to commit suicide.  They were born mid 90's to early 2000s. The GI generation is the most likely. They were born 1901-1924. You can see that these differ over time as well - looks like the GI generation as do millenials just keeps rising in terms of suicide rates, while others like gen z and gen x are staying steady. 
