library(dplyr)
setwd("~/Dropbox/ANES")
source("scripts/ANES2008_VariableBuild.R", print.eval=TRUE)
source("scripts/ANES2012_VariableBuild.R", print.eval=TRUE)
source("scripts/ANES2016_VariableBuild.R", print.eval=TRUE)

data_combined <- plyr::rbind.fill(data2008_red, data2012_red, data2016_red)
data_combined$rageg[data_combined$agegroup=="01. Age group 17-20"] <- "Age Group 17-34"
data_combined$rageg[data_combined$agegroup=="01. Age group 18-20"] <- "Age Group 17-34"
data_combined$rageg[data_combined$agegroup=="02. Age group 21-24"] <- "Age Group 17-34"
data_combined$rageg[data_combined$agegroup=="03. Age group 25-29"] <- "Age Group 17-34"
data_combined$rageg[data_combined$agegroup=="04. Age group 30-34"] <- "Age Group 17-34"
data_combined$rageg[data_combined$agegroup=="1. Age group 18-34"] <- "Age Group 17-34"

data_combined$rageg[data_combined$agegroup=="2. Age group 35-50"] <- "Age Group 35-50"
data_combined$rageg[data_combined$agegroup=="05. Age group 35-39"] <- "Age Group 35-50"
data_combined$rageg[data_combined$agegroup=="06. Age group 40-44"] <- "Age Group 35-50"
data_combined$rageg[data_combined$agegroup=="07. Age group 45-49"] <- "Age Group 35-50"

data_combined$rageg[data_combined$agegroup=="3. Age group 51-64"] <- "Age Group 51-64"
data_combined$rageg[data_combined$agegroup=="08. Age group 50-54"] <- "Age Group 51-64"
data_combined$rageg[data_combined$agegroup=="09. Age group 55-59"] <- "Age Group 51-64"
data_combined$rageg[data_combined$agegroup=="10. Age group 60-64"] <- "Age Group 51-64"

data_combined$rageg[data_combined$agegroup=="4. Age group 65+"] <- "Age Group 65+"
data_combined$rageg[data_combined$agegroup=="11. Age group 65-69"] <- "Age Group 65+"
data_combined$rageg[data_combined$agegroup=="09. Age group 70-74"] <- "Age Group 65+"
data_combined$rageg[data_combined$agegroup=="10. Age group 75 or older"] <- "Age Group 65+"
data_combined$rageg <- factor(data_combined$rageg, levels=c("Age Group 17-34", "Age Group 35-50",
                                                                         "Age Group 51-64", "Age Group 65+"))

data_combined$famincome <- as.character(data_combined$famincome)
data_combined$rfinc <- data_combined$famincome

data_combined$rfinc <- as.character(data_combined$rfinc)
data_combined$rfinc[data_combined$famincome=="None or less than $2,999"] <- "Under $5,000"
data_combined$rfinc[data_combined$famincome=="$3,000 -$4,999"] <- "Under $5,000"
data_combined$rfinc[data_combined$famincome=="$5,000 -$7,499"] <- "$5,000-$9,999"
data_combined$rfinc[data_combined$famincome=="$7,500 -$9,999"] <- "$5,000-$9,999"
data_combined$rfinc[data_combined$famincome=="$10,000 -$10,999"] <- "$10,000-$12,499"
data_combined$rfinc[data_combined$famincome=="$10,000-$10,999"] <- "$10,000-$12,499"
data_combined$rfinc[data_combined$famincome=="$11,000-$12,499"] <- "$10,000-$12,499"
data_combined$rfinc[data_combined$famincome=="$10,000-$10,999"] <- "$12,500-$14,999"
data_combined$rfinc[data_combined$famincome=="$11,000-$12,499"] <- "$12,500-$14,999"
data_combined$rfinc[data_combined$famincome=="$15,000-$17,499"] <- "$15,000-$19,999"
data_combined$rfinc[data_combined$famincome=="$17,500-$19,999"] <- "$15,000-$19,999"
data_combined$rfinc[data_combined$famincome=="$15,000-$16,999"] <- "$15,000-$19,999"
data_combined$rfinc[data_combined$famincome=="$17,000-$19,999"] <- "$15,000-$19,999"
data_combined$rfinc[data_combined$famincome=="$20,000-$22,499"] <- "$20,000-$24,999"
data_combined$rfinc[data_combined$famincome=="$20,000-$21,999"] <- "$20,000-$24,999"
data_combined$rfinc[data_combined$famincome=="$22,500-$24,999"] <- "$20,000-$24,999"
data_combined$rfinc[data_combined$famincome=="$22,000-$24,999"] <- "$20,000-$24,999"
data_combined$rfinc[data_combined$famincome=="$25,000-$27,499"] <- "$25,000-$29,999"
data_combined$rfinc[data_combined$famincome=="$27,500-$29,999"] <- "$25,000-$29,999"
data_combined$rfinc[data_combined$famincome=="$50,000-$54,999"] <- "$50,000-$59,999"
data_combined$rfinc[data_combined$famincome=="$55,000-$59,999"] <- "$50,000-$59,999"
data_combined$rfinc[data_combined$famincome=="$60,000-$64,999"] <- "$60,000-$74,999"
data_combined$rfinc[data_combined$famincome=="$65,000-$69,999"] <- "$60,000-$74,999"
data_combined$rfinc[data_combined$famincome=="$70,000-$74,999"] <- "$60,000-$74,999"
data_combined$rfinc[data_combined$famincome=="$75,000-$79,999"] <- "$75,000-$89,999"
data_combined$rfinc[data_combined$famincome=="$80,000-$89,999"] <- "$75,000-$89,999"
data_combined$rfinc[data_combined$famincome=="$110,000-$119,999"] <- "$110,000-$149,999"
data_combined$rfinc[data_combined$famincome=="$120,000-$134,999"] <- "$110,000-$149,999"
data_combined$rfinc[data_combined$famincome=="$135,000-$149,999"] <- "$110,000-$149,999"
data_combined$rfinc[data_combined$famincome=="$110,000-$124,999"] <- "$110,000-$149,999"
data_combined$rfinc[data_combined$famincome=="$125,000-$149,999"] <- "$110,000-$149,999"
data_combined$rfinc[data_combined$famincome=="$150,000-$174,999"] <- "$150,000 and over"
data_combined$rfinc[data_combined$famincome=="$175,000-$249,999"] <- "$150,000 and over"
data_combined$rfinc[data_combined$famincome=="$250,000 or more"] <- "$150,000 and over"

data_combined$rfinc <- factor(data_combined$rfinc, levels=c("Under $5,000", "$5,000-$9,999",
                                                                          "$10,000-$12,499", "$12,500-$14,999",
                                                                          "$15,000-$19,999", "$20,000-$24,999",
                                                                          "$25,000-$29,999", "$30,000-$34,999",
                                                                          "$35,000-$39,999", "$40,000-$44,999",
                                                                          "$45,000-$49,999", "$50,000-$59,999",
                                                                          "$60,000-$74,999", "$75,000-$89,999",
                                                                          "$90,000-$99,999", "$100,000-$109,999",
                                                                          "$110,000-$149,999", "$150,000 and over"))

data_combined$race <- factor(data_combined$race)
data_combined$culturalt[is.nan(data_combined$culturalt)]<- NA
data_combined$culturalt <- as.numeric(data_combined$culturalt)
#Mean Centered
data_combined$support_torture.c <- (data_combined$support_torture-mean(data_combined$support_torture, na.rm=TRUE))
data_combined$tthreat.c <- (data_combined$tthreat-mean(data_combined$tthreat, na.rm=TRUE))
data_combined$poliknow.c <- (data_combined$poliknow-mean(data_combined$poliknow, na.rm=TRUE))
data_combined$auth.c <- (data_combined$auth-mean(data_combined$auth, na.rm=TRUE))
data_combined$auth.both.c <- (data_combined$auth.both-mean(data_combined$auth.both, na.rm=TRUE))
data_combined$auth.both.t.c <- (data_combined$auth.both.t-mean(data_combined$auth.both.t, na.rm=TRUE))
data_combined$culturalt.c <- (data_combined$culturalt-mean(data_combined$culturalt, na.rm=TRUE))
data_combined$age.c <- (data_combined$age-mean(data_combined$age, na.rm=TRUE))

write.csv(data_combined, "ANESCombined.csv")

rm(list = c('data_combined','data2008', 'data2008_red', 'data2012', 'data2012_red', 'data2016', 'data2016_red'))
