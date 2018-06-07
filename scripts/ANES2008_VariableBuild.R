data2008 <- read.csv("data/anes2008.csv")
library(dplyr)
#Weights - 
#V080101a - pre sample product of household non-response adjustment factor by age and education
#V080102 - post sample adjusted for attrition

data2008$weight <- data2008$V080102
data2008$weight <- as.double(data2008$weight)
#V085213 S1. DHS: What were 9/11 terrorist trying to accomplish - RESTRICTED

#V085216 Compared to one year ago, are the chances that there will be a terrorist attack in the United States now MORE, LESS, or ABOUT THE SAME?
#V085216a - HOW MUCH MORE
#V085216b - HOW MUCH LESS
#V085216x SUMMARY

#V085217  -During the next 12 months, how likely is it that there will be a terrorist attack in the United States that kills 100 or more people?
#LIKELY
data2008$tthreat[data2008$V085217=="1. Extremely likely"] <- 5
data2008$tthreat[data2008$V085217=="2. Very likely"] <- 4
data2008$tthreat[data2008$V085217=="3. Moderately likely"] <- 3
data2008$tthreat[data2008$V085217=="4. Slightly likely"] <- 2
data2008$tthreat[data2008$V085217=="5. Not at all likely"] <- 1

#V085231a - How likely is the following suicide bomb
#V085231b - how likely is bomb
#V085231c - radioactive material
#V085231d - nuclear bobm
#V085231e - sniper attack
#V085231f - biologic weapon
#V085231g - chem weapon
#V085231h - other type of attack

#FAVOR OR OPPOSE torture
#Do you FAVOR, OPPOSE, or NEITHER FAVOR NOR OPPOSE the U.S. government
#torturing people, who are suspected of being terrorists, to try to get information?
#V085232 - general
#V085232a - favor
#V085232b - oppose
#V085232x - summary

data2008$support_torture[data2008$V085232=="7. Neither favor nor oppose"] <- 4
data2008$support_torture[data2008$V085232b=="1. A great deal"] <- 1
data2008$support_torture[data2008$V085232b=="3. Moderately"] <- 2
data2008$support_torture[data2008$V085232b=="5. A little"] <- 3
data2008$support_torture[data2008$V085232a=="1. A great deal"] <- 7
data2008$support_torture[data2008$V085232a=="3. Moderately"] <- 6
data2008$support_torture[data2008$V085232a=="5. A little"] <- 5

#Auth
#V085158  - Child trait more important: independence or respect
#V085159  - Child trait more important: curiosity or good manners
#V085160  - Child trait more important: obedience or self-reliance
#V085161  - Child trait more important: considerate or well-behaved
data2008$auth_indnum[data2008$V085158=="1. Independence"] <- 0
data2008$auth_indnum[data2008$V085158=="5. Respect for elders"] <- 1

data2008$auth_curnum[data2008$V085159=="1. Curiosity"] <- 0
data2008$auth_curnum[data2008$V085159=="5. Good manners"] <- 1

data2008$auth_obednum[data2008$V085160=="1. Obedience"] <- 1
data2008$auth_obednum[data2008$V085160=="5. Self-reliance"] <- 0

data2008$auth_considnum[data2008$V085161=="1. Being considerate"] <- 0
data2008$auth_considnum[data2008$V085161=="5. Well behaved"] <- 1

data2008$auth <- rowMeans(subset(data2008, select = c(auth_considnum, auth_curnum, auth_indnum, auth_obednum)), na.rm = TRUE)
data2008$auth[is.nan(data2008$auth)] <- NA

data2008$auth_indnum.both[data2008$V085158=="1. Independence"] <- 0
data2008$auth_indnum.both[data2008$V085158=="-8. Don't know"] <- .5
data2008$auth_indnum.both[data2008$V085158=="3. Both {VOL}"] <- .5
data2008$auth_indnum.both[data2008$V085158=="7. Neither {VOL}"] <- .5
data2008$auth_indnum.both[data2008$V085158=="5. Respect for elders"] <- 1

data2008$auth_curnum.both[data2008$V085159=="1. Curiosity"] <- 0
data2008$auth_curnum.both[data2008$V085159=="-8. Don't know"] <- .5
data2008$auth_curnum.both[data2008$V085159=="3. Both {VOL}"] <- .5
data2008$auth_curnum.both[data2008$V085159=="7. Neither {VOL}"] <- .5
data2008$auth_curnum.both[data2008$V085159=="5. Good manners"] <- 1

data2008$auth_obednum.both[data2008$V085160=="1. Obedience"] <- 1
data2008$auth_obednum.both[data2008$V085160=="-8. Don't know"] <- .5
data2008$auth_obednum.both[data2008$V085160=="3. Both {VOL}"] <- .5
data2008$auth_obednum.both[data2008$V085160=="7. Neither {VOL}"] <- .5
data2008$auth_obednum.both[data2008$V085160=="5. Self-reliance"] <- 0

data2008$auth_considnum.both[data2008$V085161=="1. Being considerate"] <- 0
data2008$auth_considnum.both[data2008$V085161=="-8. Don't know"] <- .5
data2008$auth_considnum.both[data2008$V085161=="3. Both {VOL}"] <- .5
data2008$auth_considnum.both[data2008$V085161=="7. Neither {VOL}"] <- .5
data2008$auth_considnum.both[data2008$V085161=="5. Well behaved"] <- 1

data2008$auth.both <- rowMeans(subset(data2008, select = c(auth_considnum.both, auth_curnum.both, auth_indnum.both, auth_obednum.both)), na.rm = TRUE)
data2008$auth.both[is.nan(data2008$auth.both)] <- NA

data2008$auth.both.t <- (data2008$auth_considnum.both + data2008$auth_obednum.both +
                           data2008$auth_curnum.both +data2008$auth_indnum.both)/4

#POLITICAL KNOWLEDGE

#who has most members in house
data2008$preknow_majhouse_cor[data2008$V085066!="1. Democrats"] <- 0
data2008$preknow_majhouse_cor[data2008$V085066=="1. Democrats"] <- 1
#who has most members in senate
data2008$preknow_majsent_cor[data2008$V085067=="1. Democrats"] <- 1
data2008$preknow_majsent_cor[data2008$V085067!="1. Democrats"] <- 0
data2008$poliknow <- (data2008$preknow_majsent_cor + data2008$preknow_majhouse_cor)/2

#Does R think of self as Dem Rep Ind or what
data2008$democrat[data2008$V083097=="1. Democrat"] <- 1
data2008$democrat[data2008$V083097=="2. Republican"] <- 0
data2008$democrat[data2008$V083097=="3. Independent"] <- 0
data2008$democrat[data2008$V083097=="4. Other party (SPECIFY)"] <- 0

data2008$republican[data2008$V083097=="1. Democrat"] <- 0
data2008$republican[data2008$V083097=="2. Republican"] <- 1
data2008$republican[data2008$V083097=="3. Independent"] <- 0
data2008$republican[data2008$V083097=="4. Other party (SPECIFY)"] <- 0

data2008$independent[data2008$V083097=="1. Democrat"] <- 0
data2008$independent[data2008$V083097=="2. Republican"] <- 0
data2008$independent[data2008$V083097=="3. Independent"] <- 1
data2008$independent[data2008$V083097=="5. Other party (SPECIFY)"] <- 0

data2008$year <- as.factor("2008")

#Pre-Election Age Group - Not correct, this is interview age group, not respondent.
data2008$agegroup <- NA
#data2008$agegroup[data2008$agegroup=="-4. Missing"] <- NA
#data2008$agegroup <- factor(data2008$agegroup)

#True Age not available
#V081104 ??? 
#
data2008$age <- data2008$V083215x
data2008$age[data2008$age=="-8. Don't know"] <- NA
data2008$age[data2008$age=="-9. Refused"] <- NA
data2008$age <- as.numeric(as.character(data2008$age))

#GENDER
data2008$male[data2008$V081101=="1. Male respondent selected"] <- 1
data2008$male[data2008$V081101=="2. Female respondent selected"] <- 0

data2008$female[data2008$V081101=="1. Male respondent selected"] <- 0
data2008$female[data2008$V081101=="2. Female respondent selected"] <- 1

data2008$famincome <- data2008$V083248x
data2008$famincome[data2008$famincome=="-8. Don't know"] <- NA
data2008$famincome[data2008$famincome=="-9. Refused"] <- NA
data2008$famincome <- sub(".*\\. ","",data2008$famincome)

data2008$individincome <- data2008$V083249
data2008$individincome[data2008$individincome=="-8. Don't know"] <- NA
data2008$individincome[data2008$individincome=="-9. Refused"] <- NA
data2008$individincome <- sub(".*\\. ","",data2008$individincome)

data2008$culturalt <- NA

#Do you think the number of immigrants from foreign countries
#who are permitted to come to the United States to live
#should be INCREASED A LOT, INCREASED A LITTLE, LEFT THE
#SAME as it is now, DECREASED A LITTLE, or DECREASED A LOT?
data2008$immigrantlevel <- data2008$V085082
data2008$immigrantlevel <- as.character(data2008$immigrantlevel)
data2008$immigrantlevel[data2008$immigrantlevel=="-2. No Post-election IW"] <- NA
data2008$immigrantlevel[data2008$immigrantlevel=="-8. Don't know"] <- NA
data2008$immigrantlevel[data2008$immigrantlevel=="-9. Refused"] <- NA
data2008$immigrantlevel[data2008$immigrantlevel=="1. Increased a lot"] <- 1
data2008$immigrantlevel[data2008$immigrantlevel=="2. Increased a little"] <- 2
data2008$immigrantlevel[data2008$immigrantlevel=="3. Left the same as it is now"] <- 3
data2008$immigrantlevel[data2008$immigrantlevel=="4. Decreased a little"] <- 4
data2008$immigrantlevel[data2008$immigrantlevel=="5. Decreased a lot"] <- 5
data2008$immigrantlevel <- as.integer(data2008$immigrantlevel)

data2008$muslim_thermo <- data2008$V085065e
data2008$muslim_thermo[data2008$muslim_thermo=="-6. Thermometer: 'don't know who this is'"] <- NA
data2008$muslim_thermo[data2008$muslim_thermo=="-2. No Post-election IW"] <- NA
data2008$muslim_thermo[data2008$muslim_thermo=="-7. Deleted due to partial (post-election) interview"] <- NA
data2008$muslim_thermo[data2008$muslim_thermo=="-8. Don't know ('don't know where to rate')"] <- NA
data2008$muslim_thermo[data2008$muslim_thermo=="-9. Refused"] <- NA
data2008$muslim_thermo <- as.integer(as.character(data2008$muslim_thermo))

data2008$education <- data2008$V083218x
data2008$education <- as.character(data2008$education)
data2008$education[data2008$education=="-6. No post-election interview"] <- NA
data2008$education[data2008$education=="0. DK/RF grades - no HS diploma [-8,-9 in Y3 and 5 in Y3a]"] <- NA
data2008$education[data2008$education=="-8. DK [-8 in Y3a; -8 in Y3 and -9,1 in Y3a; -8 in Y3b]"] <- NA
data2008$education[data2008$education=="-9. RF [-9 in Y3a; -9 in Y3 and -8,1 in Y3a; -9 in Y3b]"] <- NA
data2008$education[data2008$education=="1. 0-8 grades - no HS diploma/equivalency [0-8 in Y3 and 5 in Y3a]"] <- "Less than High School"
data2008$education[data2008$education=="2. 9-12 grades - no diploma/equivalency [9-12 in Y3 and 5 in Y3a]"] <- "Less than High School"
data2008$education[data2008$education=="3. 0-12 grades - HS diploma/equivalency [0-12 in Y3 and 1 in Y3a]"] <- "High School"
data2008$education[data2008$education=="4. 13+ grades, no degree [0 in Y3b]"] <- "Some college"
data2008$education[data2008$education=="5. Junior or community college level degrees (AA degrees) [7 in label define V083218xY3b]"] <- "Some college"
data2008$education[data2008$education=="6. BA level degrees [6 in Y3b] or 17+ grades with no advanced label define V083218xdegree"] <- "Bachelor's Degree"
data2008$education[data2008$education=="7. Advanced degree (including LLB) [2-6 in Y3b]"] <- "Graduate Degree"
data2008$education <- as.factor(data2008$education)

data2008$immjobthreat <- NA

data2008$race <- data2008$V083251a
data2008$race <- as.character(data2008$race)
data2008$race[data2008$race=="-9. Refused"] <- NA
data2008$race[data2008$race=="-8. Don't know"] <- NA
data2008$race[data2008$race=="50. White"] <- "NH White"
data2008$race[data2008$race=="85. Other: recoded to White"] <- "NH White"
data2008$race[data2008$race=="10. Black"] <- "NH Black"
data2008$race[data2008$race=="81. Other: recoded to Black"] <- "NH Black"
data2008$race[data2008$race=="20. Asian"] <- "NH Asian"
data2008$race[data2008$race=="82. Other: recoded to Asian"] <- "NH Asian"
data2008$race[data2008$race=="30. Native American"] <- "NH Native"
data2008$race[data2008$race=="83. Other: recoded to Native American"] <- "NH Native"
data2008$race[data2008$race=="40. Hispanic or Latino"] <- "Hispanic"
data2008$race[data2008$race=="84. Other: recoded to Hispanic"] <- "Hispanic"
data2008$race[data2008$race=="90. Other: {SPECIFY}"] <- "NH Other"
data2008$race[data2008$race=="6. Other non-Hispanic incl multiple races (Web: blank 'Other' counted as a race) "] <- "NH Other"

data2008$wiretap_powers <- NA
data2008$wiretapnowarrant <- NA

data2008 <- data2008 %>% dplyr::select(support_torture, poliknow, tthreat, #3
                                       auth, republican, independent, #6
                                       democrat, weight, year, #9
                                       age, agegroup, male, #12
                                       female, famincome, individincome, #15
                                       culturalt, immigrantlevel, education, #18 
                                       immjobthreat, race, wiretapnowarrant, #21
                                       wiretap_powers, muslim_thermo, auth.both,
                                       auth.both.t, everything()) #25

data2008_red <- data2008[,1:25]
data2008_red$agegroup <- as.character(data2008_red$agegroup)
data2008_red$famincome <- as.character(data2008_red$famincome)



summary(lm(data=data2008, support_torture ~ tthreat*poliknow*auth + republican + independent  + male + agegroup + famincome, weight=weight ))

write.csv(data2008_red, "ANES2008_reduced")

#V085084 liberal to conservative scale
#V085199a attend a protest
#V085199d sign petition  things