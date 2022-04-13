
#Requirements
# # of trips Fall
# # of participants Fall
# # of trips per Year
# # participants per Year
# increase/decrease of prior year
# Average # people per trip

library(rcompanion)
plotNormalHistogram(Seattle_ParksnRec$'# of trips Fall')
#positive Skew
plotNormalHistogram(Seattle_ParksnRec$'# of participants Fall')
#positive skew
plotNormalHistogram(Seattle_ParksnRec$'# of trips per Year')
#normal maybe a positive kurtosis
plotNormalHistogram(Seattle_ParksnRec$'# participants per Year')
# looks more nirmal but could be positive skew
plotNormalHistogram(Seattle_ParksnRec$'increase/decrease of prior year')
#normal could be positive kurtosis
plotNormalHistogram(Seattle_ParksnRec$'Average # people per trip')
#looks normal
                    