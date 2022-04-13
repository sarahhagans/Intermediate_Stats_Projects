
library("dplyr")
library("rcompanion")
library("car")
library("gmodels")
library("tidyr")

#importing dataset
library(readr)
savings <- read_csv("Desktop/Intermediate STATS/savings.csv")
View(savings)

#Scenario 1 one proportion z-test
prop.test(x = 28, n = 94, p = 0.16, alternative = "two-sided", correct = FALSE)

#OHHH greater than 16% maybe there is more FRAUD.


#Scenario 2 One-way ANOVA reference L4 
#starter project
antiseptics.expanded <- antiseptics[rep(row.names(antiseptics), antiseptics$`Number of applications`), 1:2]
#drop Nas

#make antiseptic type numeric
antiseptics.expanded$`Antiseptic TypeR` <- as.numeric(antiseptics.expanded$`Antiseptic Type`)
antiseptics.expanded$Group.A <- as.numeric(gsub("[antiseptics.expanded$, Antiseptic Type]", "A", antiseptics.expanded$Group.A))
antiseptics.expanded$Group.B <- as.numeric(gsub("[antiseptics.expanded$, Antiseptic Type]", "B", antiseptics.expanded$Group.B))
antiseptics.expanded$Group.C <- as.numeric(gsub("[antiseptics.expanded$, Antiseptic Type]", "C", antiseptics.expanded$Group.C))
antiseptics.expanded$Group.D <- as.numeric(gsub("[antiseptics.expanded$, Antiseptic Type]", "D", antiseptics.expanded$Group.D))

#test normality and homogeneity of variance the sample size passes.
plotNormalHistogram(antiseptic$`Antiseptic Type`)

df2 <- read.csv("L10_2.csv")

df2.expanded <- df2[rep(row.names(df2), df2$Number.of.applications), 1:2]
CrossTable(df2.expanded$Clinic, df2.expanded$`AntisepticType`,  
           chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")

#Scenario 3 Independent Chi-Square refer to L3

savings.reformat <- gather(savings, key=`Group`, value=`Price`)

#reading
df3 <- read.csv("L10_3.csv")

#changing groups to numeric
#gsub function replaces all matches of a string, sub function is for substituting the first occurrence of a sub-string. if is not found it won't change

df3$Group.A <- as.numeric(gsub("[\\$,]", "", df3$Group.A))
df3$Group.B <- as.numeric(gsub("[\\$,]", "", df3$Group.B))
df3$Group.C <- as.numeric(gsub("[\\$,]", "", df3$Group.C))
df3$Group.D <- as.numeric(gsub("[\\$,]", "", df3$Group.D))
# reformatting 
df3.reformat <- gather(df3, key="Group", value="Price")


plotNormalHistogram(df3.reformat$Price) # normal

bartlett.test(Price ~ Group, data=df3.reformat)
#Reformatting group as a factor
df3.reformat$Group <- as.factor(df3.reformat$Group)
fligner.test(Price ~ Group, data=df3.reformat)

ANOVA <- aov(df3.reformat$Price ~ df3.reformat$Group)
summary(ANOVA)

ANOVA <- lm(Price ~ Group, data=df3.reformat)
Anova(ANOVA, Type="II", white.adjust=TRUE)
#Scenario 4 two proportion z-test


prop.test(x=c(374, 171), n=c(374+129, 171+74),
          alternative = "two.sided", correct = FALSE)

#no significance there is no difference between demographic groups 
