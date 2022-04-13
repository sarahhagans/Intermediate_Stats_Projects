library("mvnormtest")
library("car")

#Question It is well-known that men are more likely to have heart attacks than women. How does gender (sex) influence some of the heart attack predictors like resting blood pressure (trestbps) and cholesterol (chol)?
#DV are trestbps and chol
#IV is sex.

# don't worry about violations because we dont know how to deal yet.

#Data wrangle

#ensure variables are numeric
str(heartAttacks$trestbps)
str(heartAttacks$chol)
str(heartAttacks$sex)
#They are.we dont want sex as a number
heartAttacks.sex - as.factor(heartAttacks.sex)

#Subset data

keeps <- c("trestbps", "chol")
heartattack1 <- heartAttacks[keeps]

#Limit rows

heartattack2 <- heartattack1[1:8,]

#format as matrix
heartattack3 <- as.matrix(heartattack2)

#Testing assumptions
#Multivariate Normality
mshapiro.test(t(heartattack3))
# P-value is not significant so we pass this test.

#Homogeneity of Variance sex is numeric and binary, should recode to get this to work. but we are moving on with the tests
#sex vs trestbps
leveneTest(trestbps ~ sex, data=heartAttacks)

#sex vs chol
leveneTest(chol ~ sex, data=heartAttacks)

#Absence of Multicollinearity
cor.test(heartAttacks$trestbps, heartAttacks$chol, method="pearson", use="complete.obs")
#r is 0.123 so there is an absence of multicolinearity.

#the MANCOVA
MANOVA <- manova(cbind(trestbps, chol) ~ sex, data = heartAttacks)
summary(MANOVA)
#Significance! 0.002
#PostHoc
summary.aov(MANOVA, test = "wilks") 
# only significant in response to cholesterol

##There is a significant difference between sex and the heart attack predictor cholesterol.
