
data2012 <- read.csv("data/anes2012.csv")
library(dplyr)
#Weight
data2012$weight <- data2012$weight_full

#Terrorist Threat Q
#During the next 12 months, how likely is it that there will be a terrorist attack in the United States that kills 100 or more people?
data2012$tthreat[data2012$dhs_attack=="1. Extremely likely"] <- 5
data2012$tthreat[data2012$dhs_attack=="2. Very likely"] <- 4
data2012$tthreat[data2012$dhs_attack=="3. Moderately likely"] <- 3
data2012$tthreat[data2012$dhs_attack=="4. Slightly likely"] <- 2
data2012$tthreat[data2012$dhs_attack=="5. Not at all likely"] <- 1

#Support Torture 
#Do you FAVOR, OPPOSE, or NEITHER FAVOR NOR OPPOSE the U.S. government
#torturing people, who are suspected of being terrorists, to try to get information?
data2012$support_torture[data2012$dhs_torture=="3. Neither favor nor oppose"] <- 4
data2012$support_torture[data2012$dhs_tortureopp=="1. A great deal"] <- 1
data2012$support_torture[data2012$dhs_tortureopp=="2. Moderately"] <- 2
data2012$support_torture[data2012$dhs_tortureopp=="3. A little"] <- 3
data2012$support_torture[data2012$dhs_torturefav=="3. A little"] <- 5
data2012$support_torture[data2012$dhs_torturefav=="2. Moderately"] <- 6
data2012$support_torture[data2012$dhs_torturefav=="1. A great deal"] <- 7

#Personal Civil Liberties - Wiretapping w/ Court
#Do you favor, oppose, or neither favor nor oppose the U.S.
#government being required to present evidence to get a court's
#permission before it can listen in on phone calls made by American
#citizens who are suspected of being terrorists?
data2012$wiretapnowarrant[data2012$wiretap_warrant=="1. Favor"] <- 0
data2012$wiretapnowarrant[data2012$wiretap_warrant=="2. Oppose"] <- 1
data2012$wiretapnowarrant[data2012$wiretap_warrant=="3. Neither favor nor oppose"] <- NA

#Personal Civil Liberties - Wiretap of Govt too Far
data2012$wiretap_powers[data2012$wiretappo_toofar=="1. Have gone too far"] <-0 
data2012$wiretap_powers[data2012$wiretappo_toofar=="2. Are just about right"] <- NA
data2012$wiretap_powers[data2012$wiretappo_toofar=="3. Do not go far enough"] <- 1

#Does R think of self as rem rep ind or what 
data2012$democrat[data2012$pid_self=="1. Democrat"] <- 1
data2012$democrat[data2012$pid_self=="2. Republican"] <- 0
data2012$democrat[data2012$pid_self=="3. Independent"] <- 0
data2012$democrat[data2012$pid_self=="4. Other party {SPECIFY}"] <- 0

data2012$republican[data2012$pid_self=="1. Democrat"] <- 0
data2012$republican[data2012$pid_self=="2. Republican"] <- 1
data2012$republican[data2012$pid_self=="3. Independent"] <- 0
data2012$republican[data2012$pid_self=="4. Other party {SPECIFY}"] <- 0

data2012$independent[data2012$pid_self=="1. Democrat"] <- 0
data2012$independent[data2012$pid_self=="2. Republican"] <- 0
data2012$independent[data2012$pid_self=="3. Independent"] <- 1
data2012$independent[data2012$pid_self=="5. Other party {SPECIFY}"] <- 0

#RWA
data2012$auth_considnum[data2012$auth_consid=="1. Being considerate"] <- 0
data2012$auth_considnum[data2012$auth_consid=="2. Well behaved"] <- 1
data2012$auth_curnum[data2012$auth_cur=="1. Curiosity"] <- 0
data2012$auth_curnum[data2012$auth_cur=="2. Good manners"] <- 1
data2012$auth_indnum[data2012$auth_ind=="1. Independence"] <- 0
data2012$auth_indnum[data2012$auth_ind=="2. Respect for elders"] <- 1
data2012$auth_obednum[data2012$auth_obed=="1. Obedience"] <- 1
data2012$auth_obednum[data2012$auth_obed=="2. Self-reliance"] <- 0
data2012$auth <- rowMeans(subset(data2012, select = c(auth_considnum, auth_curnum, auth_indnum, auth_obednum)), na.rm = TRUE)
data2012$auth[is.nan(data2012$auth)] <- NA

#RWA.both
data2012$auth_considnum.both[data2012$auth_consid=="1. Being considerate"] <- 0
data2012$auth_considnum.both[data2012$auth_consid=="2. Well behaved"] <- 1
data2012$auth_considnum.both[data2012$auth_consid=="-8. Don't know"] <- .5
data2012$auth_considnum.both[data2012$auth_consid=="3. Both {VOL}"] <- .5

data2012$auth_curnum.both[data2012$auth_cur=="1. Curiosity"] <- 0
data2012$auth_curnum.both[data2012$auth_cur=="2. Good manners"] <- 1
data2012$auth_curnum.both[data2012$auth_cur=="-8. Don't know"] <- .5
data2012$auth_curnum.both[data2012$auth_cur=="3. Both {VOL}"] <- .5

data2012$auth_indnum.both[data2012$auth_ind=="1. Independence"] <- 0
data2012$auth_indnum.both[data2012$auth_ind=="2. Respect for elders"] <- 1
data2012$auth_indnum.both[data2012$auth_ind=="4. Neither {VOL}"] <- .5
data2012$auth_indnum.both[data2012$auth_ind=="-8. Don't know"] <- .5
data2012$auth_indnum.both[data2012$auth_ind=="3. Both {VOL}"] <- .5

data2012$auth_obednum.both[data2012$auth_obed=="1. Obedience"] <- 1
data2012$auth_obednum.both[data2012$auth_obed=="2. Self-reliance"] <- 0
data2012$auth_obednum.both[data2012$auth_obed=="3. Both {VOL}"] <- .5
data2012$auth_obednum.both[data2012$auth_obed=="4. Neither {VOL}"] <- .5
data2012$auth_obednum.both[data2012$auth_obed=="-8. Don't know"] <- .5


data2012$auth.both <- rowMeans(subset(data2012, select = c(auth_considnum.both, auth_curnum.both, auth_indnum.both, auth_obednum.both)), na.rm = TRUE)
data2012$auth.both[is.nan(data2012$auth.both)] <- NA

data2012$auth.both.t <- (data2012$auth_considnum.both + data2012$auth_obednum.both +
                           data2012$auth_curnum.both + data2012$auth_indnum.both)/4

#CASI political knowledge
data2012$preknow_prestimes_cor[data2012$preknow_prestimes==2] <- 1
data2012$preknow_prestimes_cor[data2012$preknow_prestimes!=2] <- 0
data2012$preknow_sizedef_cor[data2012$preknow_sizedef!="1. Bigger"] <- 0
data2012$preknow_sizedef_cor[data2012$preknow_sizedef=="1. Bigger"] <- 1
data2012$preknow_senterm_cor[data2012$preknow_senterm!=6] <- 0
data2012$preknow_senterm_cor[data2012$preknow_senterm==6] <- 1
data2012$preknow_medicare_cor[data2012$preknow_medicare=="1. A program run by the U.S. Federal government to pay for old people's health care"] <- 1
data2012$preknow_medicare_cor[data2012$preknow_medicare!="1. A program run by the U.S. Federal government to pay for old people's health care"] <- 0
data2012$preknow_leastsp_cor[data2012$preknow_leastsp=="1. Foreign aid"] <- 1
data2012$preknow_leastsp_cor[data2012$preknow_leastsp!="1. Foreign aid"] <- 0
data2012$poliknow <- (data2012$preknow_leastsp_cor + data2012$preknow_senterm_cor + data2012$preknow_prestimes_cor + data2012$preknow_sizedef_cor)/4

#YEAR
data2012$year <- as.factor("2012")

#GENDER
data2012$male[data2012$gender_respondent_x=="1. Male"] <- 1
data2012$male[data2012$gender_respondent_x=="2. Female"] <- 0

data2012$female[data2012$gender_respondent_x=="1. Male"] <- 0
data2012$female[data2012$gender_respondent_x=="2. Female"] <- 1

#Age - codebook says its private but here it comes out pretty continuous
data2012$age <- data2012$dem_age_r_x
data2012$age[data2012$age=="-2. Missing"] <- NA
data2012$age <- plyr::revalue(data2012$age, c("90. Age 90 or older"="90"))
data2012$age <- as.numeric(as.character(data2012$age))

#Age Group... ? Kind of Confused here.
data2012$agegroup <- data2012$dem_agegrp_iwdate_x 
data2012$agegroup[data2012$agegroup=="-2. Missing, birthdate fields left blank"] <- NA

#SES
#total income amount - restricted access - inc_totinc / fam

#inc_incgroup_pre OR 
data2012$famincome <- data2012$incgroup_prepost
data2012$famincome[data2012$famincome=="-1. Inapplicable"] <- NA
data2012$famincome[data2012$famincome=="-8. Don't know"] <- NA
data2012$famincome[data2012$famincome=="-9. Refused"] <- NA
data2012$famincome <- sub(".*\\. ","",data2012$famincome)

#Couldnt find one specifically for individuals
data2012$individincome <- NA

#Feeling Thermometer Muslims
data2012$muslim_thermo <- data2012$ftgr_muslims
data2012$muslim_thermo[data2012$muslim_thermo=="-2. Don't recognize ('don't know who this is')"] <- NA
data2012$muslim_thermo[data2012$muslim_thermo=="-6. Not asked, unit nonresponse (no post-election interview)"] <- NA
data2012$muslim_thermo[data2012$muslim_thermo=="-7. Deleted due to partial (post-election) interview"] <- NA
data2012$muslim_thermo[data2012$muslim_thermo=="-8. Don't know ('don't know where to rate')"] <- NA
data2012$muslim_thermo[data2012$muslim_thermo=="-9. Refused"] <- NA
data2012$muslim_thermo <- as.integer(as.character(data2012$muslim_thermo))

#Immigration
# immigpo_level - Do you think the number of immigrants from foreign countries who are permitted to come to the United States to live should be (decrease-increase scale)
# immigpo_jobs - How likely is it that recent immigration levels will take jobs away from people already here (scale) JOB THREAT
#ftcasi_illegal - feeling thermo illegal immigrants

data2012$culturalt <- NA

#Do you think the number of immigrants from foreign countries
#who are permitted to come to the United States to live
#should be INCREASED A LOT, INCREASED A LITTLE, LEFT THE
#SAME as it is now, DECREASED A LITTLE, or DECREASED A LOT?
data2012$immigrantlevel <- data2012$immigpo_level
data2012$immigrantlevel <- as.character(data2012$immigrantlevel)
data2012$immigrantlevel[data2012$immigrantlevel=="-6. Not asked, unit nonresponse (no post-election interview)"] <- NA
data2012$immigrantlevel[data2012$immigrantlevel=="-7. Deleted due to partial (post-election) interview"] <- NA
data2012$immigrantlevel[data2012$immigrantlevel=="-8. Don't know"] <- NA
data2012$immigrantlevel[data2012$immigrantlevel=="-9. Refused"] <- NA
data2012$immigrantlevel[data2012$immigrantlevel=="1. Increased a lot"] <- 1
data2012$immigrantlevel[data2012$immigrantlevel=="2. Increased a little"] <- 2
data2012$immigrantlevel[data2012$immigrantlevel=="3. Left the same as it is now"] <- 3
data2012$immigrantlevel[data2012$immigrantlevel=="4. Decreased a little"] <- 4
data2012$immigrantlevel[data2012$immigrantlevel=="5. Decreased a lot"] <- 5
data2012$immigrantlevel <- as.integer(data2012$immigrantlevel)

data2012$immjobthreat <- NA

data2012$education <- data2012$dem_edugroup_x
data2012$education <- as.character(data2012$education)
data2012$education[data2012$education=="-2. Missing, other not codeable to 1-5"] <- NA
data2012$education[data2012$education=="-6. No post-election interview"] <- NA
data2012$education[data2012$education=="-7. No post data, deleted due to incomplete IW"] <- NA
data2012$education[data2012$education=="-8. Don't know"] <- NA
data2012$education[data2012$education=="-9. Refused"] <- NA
data2012$education[data2012$education=="1. Less than high school credential"] <- "Less than High School"
data2012$education[data2012$education=="2. High school credential"] <- "High School"
data2012$education[data2012$education=="3. Some post-high-school, no bachelor's degree"] <- "Some college"
data2012$education[data2012$education=="4. Bachelor's degree"] <- "Bachelor's Degree"
data2012$education[data2012$education=="5. Graduate degree"] <- "Graduate Degree"
data2012$education <- as.factor(data2012$education)

#Race - ethnicity is restricted access - besides american, what ...
data2012$race <- data2012$dem_raceeth_x
data2012$race <- as.character(data2012$race)
data2012$race[data2012$race=="-9. Missing"] <- NA
data2012$race[data2012$race=="1. White, non-Hispanic"] <- "NH White"
data2012$race[data2012$race=="2. Black, non-Hispanic"] <- "NH Black"
data2012$race[data2012$race=="3. Asian, native Hawaiian or other Pacif Islr, non-Hispanic"] <- "NH Asian"
data2012$race[data2012$race=="4. Native American or Alaska Native, non-Hispanic"] <- "NH Native"
data2012$race[data2012$race=="5. Hispanic"] <- "Hispanic"
data2012$race[data2012$race=="6. Other non-Hispanic incl multiple races (Web: blank 'Other' counted as a race)"] <- "NH Other"

#DEM_RACECPS

data2012 <- data2012 %>% dplyr::select(support_torture, poliknow, tthreat, 
                                auth, republican, independent, 
                                democrat, weight, year, 
                                age, agegroup, male, 
                                female, famincome, individincome, 
                                culturalt, immigrantlevel, education, 
                                immjobthreat, race, wiretapnowarrant,
                                wiretap_powers, muslim_thermo, auth.both,
                                auth.both.t, everything())

data2012_red <- data2012[,1:25]
data2012_red$agegroup <- as.character(data2012_red$agegroup)
data2012_red$famincome <- as.character(data2012_red$famincome)

#Models
summary(lm(data=data2012, support_torture ~ tthreat*poliknow*auth + republican + independent  + male + agegroup + famincome, weight=weight ))
#logistic1 <- glm(data=data2012, wiretapnowarrant ~ tthreat + republican + independent + auth + poliknow, family = "binomial")
#logistic2 <- glm(data=data2012, wiretap_powers ~ tthreat + republican + independent + auth+ poliknow, family = "binomial")
#exp(coef(logistic1))
#exp(coef(logistic2))

write.csv(data2012_red, "ANES2012_reduced")