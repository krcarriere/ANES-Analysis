setwd("~/Dropbox/ANES")
#source("scripts/ANES_Build.R", print.eval=TRUE)
data <- read.csv("ANESCombined.csv")
library(jtools)
library(dplyr)
library(broom)
model_output <- function(model){
  #Get the beta
  beta <- round(coef(model), 3)
  #Get the SE
  out <- summary(model)
  se <- round(out$coefficients[ , 2], 3)
  #Get the P Value
  pval <- round(out$coefficients[ , 4], 3)
  #Make a Table
  table <- as.data.frame(cbind(beta, se, pval))
  #Convert P Value to Symbols
  table$pval[(table$pval<=.1) & (table$pval>.05)] <- "+"
  table$pval[(table$pval<=.05) & (table$pval>.01)] <- "*"
  table$pval[table$pval<=.01 & (table$pval>.001)] <- "**"
  table$pval[table$pval<=.001 & table$pval!="+" & table$pval!="*" & table$pval!="**"] <- "***"
  table$pval[table$pval>.1] <- "NA"
  #Make Row Names into First Column
  table <- tibble::rownames_to_column(table, var="iv")
  return(table)
}

data$year <- as.factor(data$year)

data$auth.f <- Hmisc::cut2(data$auth, g=3)
data$tthreat.f <- Hmisc::cut2(data$tthreat, g=3)
data$poliknow.f <- Hmisc::cut2(data$poliknow, g=3)
data$support_torture.f <- as.ordered(data$support_torture)
data <- within(data, auth.f <- relevel(auth.f, ref = 3))

#FIRST TABLE: TORTURE

#Support for Torture Just Main Effects;  Threat (+) Auth (+), PoliKnow (X)
model1 <- lm(data=data, support_torture ~ tthreat.c + poliknow.c + auth.both.t.c + 
               republican + independent + male + year + age.c + rfinc + education + race + muslim_thermo, 
             weight=weight)
table1 <- model_output(model1)
table1 <- table1 %>% dplyr::filter(iv=="tthreat.c" | iv=="poliknow.c" |
                                     iv=="auth.both.t.c" | iv=="republican" |  iv=="independent" |
                                     iv=="male" | iv=="tthreat.c:poliknow.c" | iv=="tthreat.c:auth.both.t.c" |
                                     iv=="poliknow.c:auth.both.t.c" | iv=="tthreat.c:poliknow.c:auth.both.t.c" | 
                                     iv=="(Intercept)")


#Support for Torture 2 Way Interactions, Threat*Auth (-), Poli*Auth (+), Threat(+), Poli (-, trend), Auth(+)
model2 <- lm(data=data, support_torture ~ tthreat.c*poliknow.c + tthreat.c*auth.both.t.c + auth.both.t.c*poliknow.c +
               republican + independent + male + year + age.c + rfinc + education + race + muslim_thermo, 
             weight=weight)
table2 <- model_output(model2)
table2 <- table2 %>% dplyr::filter(iv=="tthreat.c" | iv=="poliknow.c" |
                                     iv=="auth.both.t.c" | iv=="republican" |  iv=="independent" |
                                     iv=="male" | iv=="tthreat.c:poliknow.c" | iv=="tthreat.c:auth.both.t.c" |
                                     iv=="poliknow.c:auth.both.t.c" | iv=="tthreat.c:poliknow.c:auth.both.t.c" | 
                                     iv=="(Intercept)")

#Support for Torture 3 Way Interactions; 3 Way (-), Poli*Auth (+), Threat*Poli (+), Threat(+), Poli (-), Auth(+)
model3<- lm(data=data, support_torture ~ tthreat.c*poliknow.c*auth.both.t.c +
              republican + independent + male + year + age.c + rfinc  + education + race + muslim_thermo, 
            weight=weight)
out<- summary(model3)
table3 <- model_output(model3)
table3 <- table3 %>% dplyr::filter(iv=="tthreat.c" | iv=="poliknow.c" |
                                     iv=="auth.both.t.c" | iv=="republican" |  iv=="independent" |
                                     iv=="male" | iv=="tthreat.c:poliknow.c" | iv=="tthreat.c:auth.both.t.c" |
                                     iv=="poliknow.c:auth.both.t.c" | iv=="tthreat.c:poliknow.c:auth.both.t.c" | 
                                     iv=="(Intercept)")
table1
table2
table3

jtools::interact_plot(model3, pred = "poliknow.c",
                      modx = "auth.both.t.c", mod2="tthreat.c", mod2.labels=list("Test", "Test2", "test3"),
                      x.label="Political Knowledge", y.label="Support for Torture",
                      legend.main="Right Wing Authoritarianism", modxvals="terciles", mod2vals="terciles")


#jtools::probe_interaction(model3, pred = "poliknow.c",
                      #modx = "auth.both.t.c",
                      #x.label="Political Knowledge", y.label="Support for Torture",
                      #legend.main="Right Wing Authoritarianism", modxvals="terciles")

#jtools::probe_interaction(model3, pred = poliknow.c, modx = auth.both.t.c, mod2 = tthreat.c)

#Esarey & Sumner 2017 to limit false discovery rate 
sim_slopes(model3, pred = poliknow.c, modx=auth.both.t.c, mod2=tthreat.c, modxvals="terciles", mod2vals="terciles", jnplot=TRUE, control.fdr=TRUE)
#sim_slopes(model3, pred = auth.both.t.c, modx=poliknow.c, mod2=tthreat.c, jnplot=TRUE)
sim_slopes(model3, pred = tthreat.c, modx=poliknow.c, mod2=auth.both.t.c, jnplot=TRUE)


anova(model1, model2) #8.8007 ***
anova(model2, model3) #8.6038 ***
anova(model1, model2, model3)

######WIRETAPPING
model4 <- glm(data=data, wiretapnowarrant ~ tthreat.c + poliknow.c + auth.both.t.c +
               republican + independent + male + age.c + rfinc + education + race + muslim_thermo , weight=weight, family="quasibinomial")
#Next part gives us the Odds Ratio SE
#See https://www.andrewheiss.com/blog/2016/04/25/convert-logistic-regression-standard-errors-to-odds-ratios-with-r/

model4df<- broom::tidy(model4) 
table4 <- model4df %>% 
  mutate(or = exp(estimate),  # Odds ratio/gradient
         var.diag = diag(vcov(model4)),  # Variance of each coefficient
         or.se = sqrt(or^2 * var.diag))
table4 <- table4 %>% dplyr::filter(term=="tthreat.c" | term=="poliknow.c" |
                                     term=="auth.both.t.c" | term=="republican" |  term=="independent" |
                                     term=="male" | term=="tthreat.c:poliknow.c" | term=="tthreat.c:auth.both.t.c" |
                                     term=="poliknow.c:auth.both.t.c" | term=="tthreat.c:poliknow.c:auth.both.t.c" | 
                                     term=="(Intercept)")
table4 <- table4 %>% dplyr::select(term, or, or.se,  p.value)
table4$p.value[(table4$p.value<=.1) & (table4$p.value>.05)] <- "+"
table4$p.value[(table4$p.value<=.05) & (table4$p.value>.01)] <- "*"
table4$p.value[table4$p.value<=.01 & (table4$p.value>.001)] <- "**"
table4$p.value[table4$p.value<=.001 & table4$p.value!="+" & table4$p.value!="*" & table4$p.value!="**"] <- "***"
table4$p.value[table4$p.value>.1] <- "NA"
table4$or <- round(table4$or, 3)
table4$or.se <- round(table4$or.se, 3)

#Support for Wiretapping 2 Way Interactions, Threat*Auth (-), Poli*Auth (+), Threat(+), Poli (-, trend), Auth(+)
model5 <- glm(data=data, wiretapnowarrant ~ tthreat.c*poliknow.c + tthreat.c*auth.both.t.c + auth.both.t.c*poliknow.c +
               republican + independent + male + age.c + rfinc + education + race + muslim_thermo, 
             weight=weight, family="quasibinomial")
model5df<- broom::tidy(model5) 
table5 <- model5df %>% 
  mutate(or = exp(estimate),  # Odds ratio/gradient
         var.diag = diag(vcov(model5)),  # Variance of each coefficient
         or.se = sqrt(or^2 * var.diag))
table5 <- table5 %>% dplyr::filter(term=="tthreat.c" | term=="poliknow.c" |
                                     term=="auth.both.t.c" | term=="republican" |  term=="independent" |
                                     term=="male" | term=="tthreat.c:poliknow.c" | term=="tthreat.c:auth.both.t.c" |
                                     term=="poliknow.c:auth.both.t.c" | term=="tthreat.c:poliknow.c:auth.both.t.c" | 
                                     term=="(Intercept)")
table5 <- table5 %>% dplyr::select(term, or, or.se,  p.value)
table5$p.value[(table5$p.value<=.1) & (table5$p.value>.05)] <- "+"
table5$p.value[(table5$p.value<=.05) & (table5$p.value>.01)] <- "*"
table5$p.value[table5$p.value<=.01 & (table5$p.value>.001)] <- "**"
table5$p.value[table5$p.value<=.001 & table5$p.value!="+" & table5$p.value!="*" & table5$p.value!="**"] <- "***"
table5$p.value[table5$p.value>.1] <- "NA"
table5$or <- round(table5$or, 3)
table5$or.se <- round(table5$or.se, 3)

#Support for Wiretapping 3 Way Interactions; 3 Way (-), Poli*Auth (+), Threat*Poli (+), Threat(+), Poli (-), Auth(+)
model6<- glm(data=data, wiretapnowarrant ~ tthreat.c*poliknow.c*auth.both.t.c +
              republican + independent + male  + age.c + rfinc  + education + race + muslim_thermo, 
            weight=weight, family="quasibinomial")
model6df<- broom::tidy(model6) 
table6 <- model6df %>% 
  mutate(or = exp(estimate),  # Odds ratio/gradient
         var.diag = diag(vcov(model6)),  # Variance of each coefficient
         or.se = sqrt(or^2 * var.diag))
table6 <- table6 %>% dplyr::filter(term=="tthreat.c" | term=="poliknow.c" |
                                     term=="auth.both.t.c" | term=="republican" |  term=="independent" |
                                     term=="male" | term=="tthreat.c:poliknow.c" | term=="tthreat.c:auth.both.t.c" |
                                     term=="poliknow.c:auth.both.t.c" | term=="tthreat.c:poliknow.c:auth.both.t.c" | 
                                     term=="(Intercept)")
table6 <- table6 %>% dplyr::select(term, or, or.se,  p.value)
table6$p.value[(table6$p.value<=.1) & (table6$p.value>.05)] <- "+"
table6$p.value[(table6$p.value<=.05) & (table6$p.value>.01)] <- "*"
table6$p.value[table6$p.value<=.01 & (table6$p.value>.001)] <- "**"
table6$p.value[table6$p.value<=.001 & table6$p.value!="+" & table6$p.value!="*" & table6$p.value!="**"] <- "***"
table6$p.value[table6$p.value>.1] <- "NA"
table6$or <- round(table6$or, 3)
table6$or.se <- round(table6$or.se, 3)

table4
table5
table6
summary(model4)
summary(model5)
summary(model6)
anova(model4, model5, test="F")
anova(model5, model6, test="F")

fit <- lm(mpg ~ hp + wt, data = mtcars)
fit_b <- lm(mpg ~ hp + wt + disp, data = mtcars)
fit_c <- lm(mpg ~ hp + wt + disp + drat, data = mtcars)
coef_names <- c("Political Knowledge" = "poliknow.c", "RWA" = "auth.both.t.c",
                "Terrorist Threat" = "tthreat.c", "Poli Know*Auth" = "poliknow.c:auth.both.t.c", 
                "Terrorist * RWA" = "tthreat.c:auth.both.t.c",
                "Terrorist Threat * Poli" = "tthreat.c:poliknow.c", "Three Way"="tthreat.c:poliknow.c:auth.both.t.c")
plot_coefs(model1, model2, model3, scale = TRUE, robust = "HC3", coefs = coef_names)
export_summs(fit, fit_b, fit_c, scale = TRUE, transform.response = TRUE, coefs = coef_names)



jtools::interact_plot(model=model6, pred = "poliknow.c",
                      modx = "auth.both.t.c", mod2 = "tthreat.c",
                      x.label="Political Knowledge",
                      y.label="Wiretapping without Warrant Support",
                      legend.main="RWA",
                      modxvals="terciles", mod2vals="terciles")
sim_slopes(model6, pred = poliknow.c, modx=auth.both.t.c, mod2=tthreat.c, modxvals="terciles", mod2vals="terciles", jnplot=TRUE, control.fdr=TRUE)
jtools::interact_plot(model=model6, pred = "poliknow.c",
                      modx = "auth.both.t.c",
                      x.label="Political Knowledge",
                      y.label="Wiretapping without Warrant Support")
jtools::interact_plot(model=model6, pred = "tthreat.c",
                      modx = "auth.both.t.c",
                      x.label="Terrorist Threat",
                      y.label="Wiretapping without Warrant Support")

#https://www.statalist.org/forums/forum/general-stata-discussion/general/1306897-marginal-effect-plot-for-interaction-term-w-regression-using-multiple-imputations

#SUMMARY STATS
psych::describeBy(data$support_torture)
psych::describeBy(data$tthreat)
psych::describeBy(data$culturalt)
psych::describeBy(data$poliknow)
psych::describeBy(data$auth.both)
psych::describeBy(data$wiretapnowarrant)
psych::describeBy(data$immigrantlevel)
psych::describeBy(data$age)
gmodels::CrossTable(data$rfinc)
gmodels::CrossTable(data$education)
gmodels::CrossTable(data$male, missing.include=TRUE)
