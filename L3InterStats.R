

library(gmodels)
library(dplyr)
library("tidyr")

#Part 1 
#Does the term of the loan influence loan status? YES. If so, how? 
#More people are current on a 60 month loan. more people pay off a 36 month loan and more people are charged off a 60 month loan.
CrossTable(loans$term, loans$loan_status, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
#Part 2
#How has the ability to own a home changed after 2009?
# It did not change significantly. 
loans$DateR <- as.Date(paste(loans$Date), "%m/%d/%Y")

loans1 <- separate(loans, DateR, c("Year", "Month", "Day"), sep="-")

loans1$YearR <- NA
loans1$YearR[loans1$Year <= 2009] <- 0
loans1$YearR[loans1$Year > 2009] <- 1

loans1$RentvOwn <- NA
loans1$RentvOwn[loans1$home_ownership == "RENT"] <- 0
loans1$RentvOwn[loans1$home_ownership == "OWN"] <- 1

CrossTable(loans1$RentvOwn, loans1$YearR, fisher=TRUE, chisq = TRUE, mcnemar = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")

#Part 3
#The news just ran a story that only 15% of homes are fully paid for in America, and that another 10% have given up on paying it back, so the bank has "charged off" the loan. 
#Does it seem likely that the data for this hands on came from the larger population of America?
# it is significantly different from the expected and the sample we are looking at probably did not come from the same dataset.
loans %>% group_by(loan_status) %>% summarise(count = n())
observed = c(3282, 502 ,18173)
expected = c(0.15, 0.75, .1)
chisq.test(x=observed, p = expected)
