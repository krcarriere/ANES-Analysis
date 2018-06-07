library(dplyr)
data2016 <- read.csv("data/anes2016.csv")


#Weight
#V160102
data2016$weight <- data2016$V160102

#Worried about Terrorist Attack
data2016$wor_tthreat[data2016$V162160=="1. Extremely worried"] <- 5
data2016$wor_tthreat[data2016$V162160=="2. Very worried"] <- 4
data2016$wor_tthreat[data2016$V162160=="3. Moderately worried"] <- 3
data2016$wor_tthreat[data2016$V162160=="4. Slightly worried"] <- 2
data2016$wor_tthreat[data2016$V162160=="5. Not at all worried"] <- 1

#How Likely Is an Attack
data2016$tthreat[data2016$V162294=="1. Extremely likely"] <- 5
data2016$tthreat[data2016$V162294=="2. Very likely"] <- 4
data2016$tthreat[data2016$V162294=="3. Moderately likely"] <- 3
data2016$tthreat[data2016$V162294=="4. Slightly likely"] <- 2
data2016$tthreat[data2016$V162294=="5. Not likely at all"] <- 1

#Favor or oppose torture for suspected terrorists
#V162295 - torture general
#V162295a - torture favor
#V162295b - torture opp
data2016$support_torture[data2016$V162295=="3. Neither favor nor oppose"] <- 4
data2016$support_torture[data2016$V162295b=="1. A great deal"] <- 1
data2016$support_torture[data2016$V162295b=="2. Moderately"] <- 2
data2016$support_torture[data2016$V162295b=="3. A little"] <- 3
data2016$support_torture[data2016$V162295a=="1. A great deal"] <- 7
data2016$support_torture[data2016$V162295a=="2. Moderately"] <- 6
data2016$support_torture[data2016$V162295a=="3. A little"] <- 5

#Personal Civil Liberties - Wiretapping w/ Court
#Does Not Exist in 2016
data2016$wiretapnowarrant <- NA

#Personal Civil Liberties - Wiretap of Govt too Far
data2016$wiretap_powers[data2016$V162178=="1. Have gone too far"] <-0 
data2016$wiretap_powers[data2016$V162178=="2. Are just about right"] <- NA
data2016$wiretap_powers[data2016$V162178=="3. Do not go far enough"] <- 1

#RWA 
#V162170 -  Country needs strong leader to take us back to true path
#V162168 -  ‘Our country needs free thinkers who will have the courage to defy traditional ways, even if this upsets many people.’
#V162169 - Our country would be great if we honor the ways of our forefathers, do what the authorities tell us to do, and get rid of the ‘rotten apples’ who are ruining everything.’

#Auth
#V162239  - Child trait more important: independence or respect
#V162240  - Child trait more important: curiosity or good manners
#V162241  - Child trait more important: obedience or self-reliance
#V162242  - Child trait more important: considerate or well-behaved
data2016$auth_curnum[data2016$V162240=="1. Curiosity"] <- 0
data2016$auth_curnum[data2016$V162240=="2. Good manners"] <- 1

data2016$auth_indnum[data2016$V162239=="1. Independence"] <- 0
data2016$auth_indnum[data2016$V162239=="2. Respect for elders"] <- 1

data2016$auth_obednum[data2016$V162241=="1. Obedience"] <- 1
data2016$auth_obednum[data2016$V162241=="2. Self-reliance"] <- 0

data2016$auth_considnum[data2016$V162242=="1. Being considerate"] <- 0
data2016$auth_considnum[data2016$V162242=="2. Well behaved"] <- 1

data2016$auth <- rowMeans(subset(data2016, select = c(auth_considnum, auth_curnum, auth_indnum, auth_obednum)), na.rm = TRUE)
data2016$auth[is.nan(data2016$auth)] <- NA


#BOTH INCLUDED
data2016$auth_indnum.both[data2016$V162239=="1. Independence"] <- 0
data2016$auth_indnum.both[data2016$V162239=="2. Respect for elders"] <- 1
data2016$auth_indnum.both[data2016$V162239=="3. Both"] <- .5
data2016$auth_indnum.both[data2016$V162239=="-8. Don't know"] <- .5

data2016$auth_curnum.both[data2016$V162240=="1. Curiosity"] <- 0
data2016$auth_curnum.both[data2016$V162240=="2. Good manners"] <- 1
data2016$auth_curnum.both[data2016$V162240=="3. Both"] <- .5
data2016$auth_curnum.both[data2016$V162240=="-8. Don't know"] <- .5

data2016$auth_obednum.both[data2016$V162241=="1. Obedience"] <- 1
data2016$auth_obednum.both[data2016$V162241=="2. Self-reliance"] <- 0
data2016$auth_obednum.both[data2016$V162241=="3. Both"] <- .5
data2016$auth_obednum.both[data2016$V162241=="-8. Don't know"] <- .5

data2016$auth_considnum.both[data2016$V162242=="1. Being considerate"] <- 0
data2016$auth_considnum.both[data2016$V162242=="2. Well behaved"] <- 1
data2016$auth_considnum.both[data2016$V162242=="3. Both"] <- .5
data2016$auth_considnum.both[data2016$V162242=="-8. Don't know"] <- .5

data2016$auth.both <- rowMeans(subset(data2016, select = c(auth_considnum.both, auth_curnum.both, auth_indnum.both, auth_obednum.both)), na.rm = TRUE)
data2016$auth.both[is.nan(data2016$auth.both)] <- NA

data2016$auth.both.t <- (data2016$auth_considnum.both + data2016$auth_obednum.both +
  data2016$auth_curnum.both +data2016$auth_indnum.both)/4



#POLITICAL KNOWLEDGE

data2016$preknow_senterm_cor[data2016$V161513!=6] <- 0
data2016$preknow_senterm_cor[data2016$V161513==6] <- 1
data2016$preknow_leastsp_cor[data2016$V161514=="1. Foreign aid"] <- 1
data2016$preknow_leastsp_cor[data2016$V161514!="1. Foreign aid"] <- 0
#who has most members in house
data2016$preknow_majhouse_cor[data2016$V161515!="2. Republicans"] <- 0
data2016$preknow_majhouse_cor[data2016$V161515=="2. Republicans"] <- 1
#who has most members in senate
data2016$preknow_majsent_cor[data2016$V161516=="2. Republicans"] <- 1
data2016$preknow_majsent_cor[data2016$V161516!="2. Republicans"] <- 0
data2016$poliknow <- (data2016$preknow_leastsp_cor + data2016$preknow_senterm_cor + data2016$preknow_majhouse_cor + data2016$preknow_majsent_cor)/4

#POLITICAL PARTY
#V161155
data2016$democrat[data2016$V161155=="1. Democrat"] <- 1
data2016$democrat[data2016$V161155=="2. Republican"] <- 0
data2016$democrat[data2016$V161155=="3. Independent"] <- 0
data2016$democrat[data2016$V161155=="5. Other party SPECIFY"] <- 0

data2016$republican[data2016$V161155=="1. Democrat"] <- 0
data2016$republican[data2016$V161155=="2. Republican"] <- 1
data2016$republican[data2016$V161155=="3. Independent"] <- 0
data2016$republican[data2016$V161155=="5. Other party SPECIFY"] <- 0

data2016$independent[data2016$V161155=="1. Democrat"] <- 0
data2016$independent[data2016$V161155=="2. Republican"] <- 0
data2016$independent[data2016$V161155=="3. Independent"] <- 1
data2016$independent[data2016$V161155=="5. Other party SPECIFY"] <- 0

data2016$year <- as.factor("2016")

#America’s culture is generally harmed by immigrants
data2016$culturalt <- 0
data2016$culturalt[data2016$V162269=="5. Disagree strongly"] <- 1 
data2016$culturalt[data2016$V162269=="4. Disagree somewhat"] <- 2 
data2016$culturalt[data2016$V162269=="3. Neither agree nor disagree"] <- 3 
data2016$culturalt[data2016$V162269=="2. Agree somewhat"] <- 4 
data2016$culturalt[data2016$V162269=="1. Agree strongly"] <- 5
data2016$culturalt[data2016$culturalt==0] <- NA

#Immigration Levels Will Take Away Jobs
data2016$immjobthreat <- 0
data2016$immjobthreat[data2016$V162158=="1. Extremely likely"] <- 4
data2016$immjobthreat[data2016$V162158=="2. Very likely"] <- 3
data2016$immjobthreat[data2016$V162158=="3. Somewhat likely"] <- 2
data2016$immjobthreat[data2016$V162158=="4. Not at all likely"] <- 1
data2016$immjobthreat[data2016$V162158=="-6. No post-election interview"] <- NA
data2016$immjobthreat[data2016$V162158=="-7. No post data, deleted due to incomplete IW"] <- NA
data2016$immjobthreat[data2016$V162158=="-8. Don't know"] <- NA
data2016$immjobthreat[data2016$V162158=="-9. Refused"] <- NA


data2016$immigrantlevel <- data2016$V162157
data2016$immigrantlevel <- as.character(data2016$immigrantlevel)
data2016$immigrantlevel[data2016$immigrantlevel=="-6. No post-election interview"] <- NA
data2016$immigrantlevel[data2016$immigrantlevel=="-7. No post data, deleted due to incomplete IW"] <- NA
data2016$immigrantlevel[data2016$immigrantlevel=="-8. Don't know"] <- NA
data2016$immigrantlevel[data2016$immigrantlevel=="-9. Refused"] <- NA
data2016$immigrantlevel[data2016$immigrantlevel=="1. Increased a lot"] <- 1
data2016$immigrantlevel[data2016$immigrantlevel=="2. Increased a little"] <- 2
data2016$immigrantlevel[data2016$immigrantlevel=="3. Left the same as it is now"] <- 3
data2016$immigrantlevel[data2016$immigrantlevel=="4. Decreased a little"] <- 4
data2016$immigrantlevel[data2016$immigrantlevel=="5. Decreased a lot"] <- 5
data2016$immigrantlevel <- as.integer(data2016$immigrantlevel)
#Age - Private

#V161267
data2016$age <- data2016$V161267
data2016$age <- plyr::revalue(data2016$age, c("90. Age 90 or older"="90"))
data2016$age[data2016$age=="-8. DK (year of birth, FTF only)"] <- NA
data2016$age[data2016$age=="-9. RF (year of birth)"] <- NA
data2016$age <- as.numeric(as.character(data2016$age))

#Age Group... ? Kind of Confused here.
data2016$agegroup <- data2016$V161267x
data2016$agegroup <- as.character(data2016$agegroup)
data2016$agegroup[data2016$agegroup=="-1"] <- "-1. Inapplicable"
data2016$agegroup[data2016$agegroup=="1"] <- "01. Age group 18-20"
data2016$agegroup[data2016$agegroup=="2"] <- "02. Age group 21-24"
data2016$agegroup[data2016$agegroup=="3"] <- "03. Age group 25-29"
data2016$agegroup[data2016$agegroup=="4"] <- "04. Age group 30-34"
data2016$agegroup[data2016$agegroup=="5"] <- "05. Age group 35-39"
data2016$agegroup[data2016$agegroup=="6"] <- "06. Age group 40-44"
data2016$agegroup[data2016$agegroup=="7"] <- "07. Age group 45-49"
data2016$agegroup[data2016$agegroup=="8"] <- "08. Age group 50-54"
data2016$agegroup[data2016$agegroup=="9"] <- "09. Age group 55-59"
data2016$agegroup[data2016$agegroup=="10"] <- "10. Age group 60-64"
data2016$agegroup[data2016$agegroup=="11"] <- "11. Age group 65-69"
data2016$agegroup[data2016$agegroup=="12"] <- "12. Age group 70-74"
data2016$agegroup[data2016$agegroup=="13"] <- "13. Age group 75 or older"
data2016$agegroup[data2016$agegroup=="-1. Inapplicable"] <- NA
data2016$agegroup <- as.factor(data2016$agegroup)


#GENDER
data2016$male[data2016$V161342=="1. Male"] <- 1
data2016$male[data2016$V161342=="2. Female"] <- 0
data2016$male[data2016$V161342=="3. Other"] <- 0
data2016$male[data2016$V161342=="-9. Refused"] <- NA

data2016$female[data2016$V161342=="1. Male"] <- 0
data2016$female[data2016$V161342=="2. Female"] <- 1
data2016$female[data2016$V161342=="3. Other"] <- 0
data2016$female[data2016$V161342=="-9. Refused"] <- NA

#Feeling Thermometer Muslims
data2016$muslim_thermo <- data2016$V162106
data2016$muslim_thermo[data2016$muslim_thermo==" 999. Don't recognize (don't know who this is)"] <- NA
data2016$muslim_thermo[data2016$muslim_thermo=="-6. No post-election interview"] <- NA
data2016$muslim_thermo[data2016$muslim_thermo=="-7. No post data, deleted due to incomplete IW"] <- NA
data2016$muslim_thermo[data2016$muslim_thermo=="998. Don't know (where to rate) "] <- NA
data2016$muslim_thermo[data2016$muslim_thermo=="-9. Refused"] <- NA
data2016$muslim_thermo <- as.integer(as.character(data2016$muslim_thermo))


#V161342a OTHRE GENDER FOLLOW UP - Restricted

#SES
#total income amount - restricted access - inc_totinc / fam - V161351
#V162299 POST TOTAL INCOME
#inc_incgroup_pre OR  V161361x PRE INCOME SUMMARY
data2016$famincome <- data2016$V161361x
data2016$famincome[data2016$famincome=="-1. Inapplicable"] <- NA
data2016$famincome[data2016$famincome=="-8. Don't know"] <- NA
data2016$famincome[data2016$famincome=="-5. Interview breakoff (sufficient partial IW)"] <- NA
data2016$famincome[data2016$famincome=="-9. Refused"] <- NA
data2016$famincome <- sub(".*\\. ","",data2016$famincome)

#Couldnt find one specifically for individuals
data2016$individincome <- NA

#Education
data2016$education <- data2016$V161270
data2016$education <- as.character(data2016$education)
data2016$education[data2016$education=="-6. No post-election interview"] <- NA
data2016$education[data2016$education=="-7. No post data, deleted due to incomplete IW"] <- NA
data2016$education[data2016$education=="-8. Don't know"] <- NA
data2016$education[data2016$education=="-9. Refused"] <- NA
data2016$education[data2016$education=="1. Less than 1st grade"] <- "Less than High School"
data2016$education[data2016$education=="2. 1st, 2nd, 3rd or 4th grade"] <- "Less than High School"
data2016$education[data2016$education=="3. 5th or 6th grade"] <- "Less than High School"
data2016$education[data2016$education=="4. 7th or 8th grade"] <- "Less than High School"
data2016$education[data2016$education=="5. 9th grade"] <- "Less than High School"
data2016$education[data2016$education=="6. 10th grade"] <- "Less than High School"
data2016$education[data2016$education=="7. 11th grade"] <- "Less than High School"
data2016$education[data2016$education=="8. 12th grade no diploma"] <- "Less than High School"
data2016$education[data2016$education=="9. High school graduate- high school diploma or equivalent (for example: GED)"] <- "High School"
data2016$education[data2016$education=="90. Other specify given as: high school graduate"] <- "High School"
data2016$education[data2016$education=="95. Other SPECIFY"] <- NA
data2016$education[data2016$education=="10. Some college but no degree"] <- "Some college"
data2016$education[data2016$education=="11. Associate degree in college - occupational/vocational program"] <- "Some college"
data2016$education[data2016$education=="12. Associate degree in college - academic program"] <- "Some college"
data2016$education[data2016$education=="12. Associate degree in college -- academic program"] <- "Some college"
data2016$education[data2016$education=="13. Bachelor's degree (for example: BA, AB, BS)"] <- "Bachelor's Degree"
data2016$education[data2016$education=="14. Master's degree (for example: MA, MS, MENG, MED, MSW, MBA)"] <- "Graduate Degree"
data2016$education[data2016$education=="15. Professional school degree (for example: MD, DDS, DVM, LLB, JD)"] <- "Graduate Degree"
data2016$education[data2016$education=="16. Doctorate degree (for example: PHD, EDD)"] <- "Graduate Degree"
data2016$education <- as.factor(data2016$education)

#RACE
data2016$race <- data2016$V161310x
data2016$race <- as.character(data2016$race)
data2016$race[data2016$race=="-9. Missing"] <- NA
data2016$race[data2016$race=="1. White, non-Hispanic"] <- "NH White"
data2016$race[data2016$race=="2. Black, non-Hispanic"] <- "NH Black"
data2016$race[data2016$race=="3. Asian, native Hawaiian or other Pacif Islr,non-Hispanic"] <- "NH Asian"
data2016$race[data2016$race=="4. Native American or Alaska Native, non-Hispanic"] <- "NH Native"
data2016$race[data2016$race=="5. Hispanic"] <- "Hispanic"
data2016$race[data2016$race=="6. Other non-Hispanic incl multiple races [WEB: blank 'Other' counted as a race]"] <- "NH Other"
data2016$race[data2016$race=="6. Other non-Hispanic incl multiple races (Web: blank 'Other' counted as a race)"] <- "NH Other"

data2016 <- data2016 %>% dplyr::select(support_torture, poliknow, tthreat, 
                                auth, republican, independent, 
                                democrat, weight, year, 
                                age, agegroup, male, 
                                female, famincome, individincome, 
                                culturalt, immigrantlevel, education, 
                                immjobthreat, race, wiretapnowarrant,
                                wiretap_powers, muslim_thermo, auth.both, 
                                auth.both.t, everything())
summary(lm(data=data2016, support_torture ~ tthreat*poliknow*auth + republican + independent + male + agegroup + famincome, weight=weight ))
data2016_red <- data2016[,1:25]
data2016_red$agegroup <- as.character(data2016_red$agegroup)
data2016_red$famincome <- as.character(data2016_red$famincome)

write.csv(data2016_red, "ANES2016_reduced")
