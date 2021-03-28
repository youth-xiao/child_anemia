##############################################
### Group 2: Tanzania Children Anemia      ###
### R code by: Xiaoyang Zou & Wen Zhang    ###
### Date created: 03/06/2021               ###
### Last modified: 03/16/2021              ###
##############################################

# original data set
tan_raw <- read.csv("Tanz_DHS_data2_withchildsex.csv")
library(dplyr)
# updated data set: select id variable and other variables of interest

tan_select <- tan_raw %>%
  dplyr::select(id, child, b4_,
                v025, v106, v190, v191,
                v414h, v414j, v414n, v414o,
                m45_, h34_, h42_, h43_,
                hw1_, hw2_, hw3_, hw4_, hw7_, hw10_, hw57_)

# drop observations that are missing on outcome variable - non-existing subjects
tan_select$outcome <- tan_select$hw57_
sum(is.na(tan_select$outcome))
table(tan_select$outcome)
tan_select <- tan_select %>%
  mutate(outcome1 = replace(outcome, outcome == '', NA))
table(tan_select$outcome1)
sum(is.na(tan_select$outcome1))
# keep observations that have outcomes
tan_new <- tan_select[!is.na(tan_select$outcome1),] # obs drop from 79596 to 8014


######################################
### variable b4_ "sex of children" ###
######################################
tan_new$sex <- tan_new$b4_
summary(tan_new$sex)
table(tan_new$sex)
# female   male 
#   3994   4020 
sum(is.na(tan_new$sex)) # no missing value


##################################################
### variable v025 "type of place of residence" ###
##################################################
tan_new$resident <- tan_new$v025
summary(tan_new$resident) # character type
table(tan_new$resident)
# rural urban 
#  6218  1796
sum(is.na(tan_new$resident)) # no missing value


########################################
### v106 "highest educational level" ###
########################################
tan_new$edu <- tan_new$v106
summary(tan_new$edu) # character type
table(tan_new$edu)
#higher   no education      primary    secondary 
#    68           1771         4812         1363 
sum(is.na(tan_new$edu)) # no missing value


###########################
### v190 "wealth index" ###
###########################

# FOR DISCUSSION PURPOSE
tan_new$wealth <- tan_new$v190
summary(tan_new$wealth) # character type
table(tan_new$wealth)
# middle  poorer poorest  richer richest 
#   1585    1677    1833    1642    1277
sum(is.na(tan_new$wealth)) # no missing value


#####################################################
### v191 "wealth index factor score (5 decimals)" ###
#####################################################

# FOR ANALYSIS PURPOSE - MORE ACCURATE
tan_new$wealth_fs <- tan_new$v191
summary(tan_new$wealth_fs)
#    Min.   1st Qu.   Median    Mean    3rd Qu.     Max. 
# -213877  -85450     -57509  -20301     19343    414042 
sum(is.na(tan_new$wealth_fs)) # no missing value
tan_new$wealth_fs <- tan_new$wealth_fs*0.00001
summary(tan_new$wealth_fs)
# Min.     1st Qu.    Median     Mean    3rd Qu.      Max. 
# -2.1388 -0.8545   -0.5751   -0.2030    0.1934    4.1404


################################################################
### v414h "gave child meat (beef, pork, lamb, chicken, etc)" ###
################################################################
tan_new$meat <- tan_new$v414h
summary(tan_new$meat) # character type
table(tan_new$meat)
#            don't know         no        yes 
#    2909             2       4586        517 
tan_new <- tan_new %>%
  mutate(meat1 = replace(meat, meat == 'don\'t know', NA))
tan_new <- tan_new %>%
  mutate(meat1 = replace(meat1, meat1 == '', NA))
table(tan_new$meat1)
#   no  yes 
# 4586  517
sum(is.na(tan_new$meat1)) # no. of missing = 2911


##########################################################
### v414j "gave child any dark green leafy vegetables" ###
##########################################################
tan_new$vegg <- tan_new$v414j
summary(tan_new$vegg) # character type
table(tan_new$vegg)
#            don't know         no        yes 
#    2909             2       2803        2300
tan_new <- tan_new %>%
  mutate(vegg1 = replace(vegg, vegg == 'don\'t know', NA))
tan_new <- tan_new %>%
  mutate(vegg1 = replace(vegg1, vegg1 == '', NA))
table(tan_new$vegg1)
#   no  yes 
# 2803 2300
sum(is.na(tan_new$vegg1)) # no. of missing = 2911


#####################################################
### variable v414n "gave child fish or shellfish" ###
#####################################################
tan_new$fish <- tan_new$v414n
summary(tan_new$fish) #character type
table(tan_new$fish)
#         don't know         no        yes 
# 2909             2       3972       1131
tan_new <- tan_new %>%
  mutate(fish1 = replace(fish, fish == 'don\'t know', NA))
tan_new <- tan_new %>%
  mutate(fish1 = replace(fish1, fish1 == '', NA))
table(tan_new$fish1)
#   no  yes
# 3972 1131
sum(is.na(tan_new$fish1)) # no. of missing = 2911


#############################################################################
### variable v414o "Gave child food made from beans, peas, lentils, nuts" ###
#############################################################################
tan_new$bean <- tan_new$v414o
summary(tan_new$bean)
table(tan_new$bean)
#            don't know         no        yes 
#     2909            2       3556       1547 
tan_new <- tan_new %>%
  mutate(bean1 = replace(bean, bean == 'don\'t know', NA))
tan_new <- tan_new %>%
  mutate(bean1 = replace(bean1, bean1 == '', NA))
table(tan_new$bean1)
#   no  yes 
# 3556 1547
sum(is.na(tan_new$bean1)) # no. of missing = 2911


##############################################################################
### variable m45_	"During pregnancy, given or bought iron tablets/syrup"   ###
### (binary variable—yes/no) or iron level during pregnancy of the mother? ###
##############################################################################
tan_new$iron_preg <- tan_new$m45_
table(tan_new$iron_preg)
#         don't know         no        yes 
# 2536             4       1111       4363
tan_new <- tan_new %>%
  mutate(iron_preg1 = replace(iron_preg, iron_preg == 'don\'t know', NA))
tan_new <- tan_new %>%
  mutate(iron_preg1 = replace(iron_preg1, iron_preg1 == '', NA))
table(tan_new$iron_preg1)
#  no  yes 
#1111 4363
sum(is.na(tan_new$iron_preg1)) # no. of missing = 2540


##################################################
### variable h34_ "Vitamin A in last 6 months" ###
##################################################
tan_new$vit_a <- tan_new$h34_
summary(tan_new$vit_a) #character type
table(tan_new$vit_a)
#            don't know         no        yes 
#    5               28       4677       3304 
tan_new <- tan_new %>%
  mutate(vit_a1 = replace(vit_a, vit_a == 'don\'t know', NA))
tan_new <- tan_new %>%
  mutate(vit_a1 = replace(vit_a1, vit_a1 == '', NA))
table(tan_new$vit_a1)
#   no  yes 
# 4677 3304
sum(is.na(tan_new$vit_a1)) # no. of missing = 33


############################################################
### variable h42_ "Taking iron pills sprinkles or syrup" ###
############################################################
tan_new$iron <- tan_new$h34_
summary(tan_new$iron) #character type
table(tan_new$iron)
#            don't know         no        yes 
#    5               28       4677       3304 
tan_new <- tan_new %>%
  mutate(iron1 = replace(iron, iron == 'don\'t know', NA))
tan_new <- tan_new %>%
  mutate(iron1 = replace(iron1, iron1 == '', NA))
table(tan_new$iron1)
#   no  yes 
# 4677 3304
sum(is.na(tan_new$iron1)) # no. of missing = 33


#######################################################################
### variable h43_ "Drugs for intestinal parasites in last 6 months" ###
#######################################################################
tan_new$para_drug <- tan_new$h43_
summary(tan_new$para_drug) #character type
table(tan_new$para_drug)
#            don't know         no        yes 
#    5               16       4826       3167 
tan_new <- tan_new %>%
  mutate(para_drug1 = replace(para_drug, para_drug == 'don\'t know', NA))
tan_new <- tan_new %>%
  mutate(para_drug1 = replace(para_drug1, para_drug1 == '', NA))
table(tan_new$para_drug1)
#   no  yes 
# 4826 3167
sum(is.na(tan_new$para_drug1)) # no. of missing = 21


#############################################
### variable hw1_ "Child's age in months" ###
#############################################
tan_new$month_age <- tan_new$hw1_
summary(tan_new$month_age)
#  Min.  1st Qu.  Median    Mean  3rd Qu.   Max. 
#  6.00   17.00   29.00   30.69   44.00   59.00
sum(is.na(tan_new$month_age)) # no missing value


###############################################################
### variable hw2_ "Child's weight in kilograms (1 decimal)" ###
###############################################################
tan_new$wt_kg <- tan_new$hw2_
summary(tan_new$wt_kg) # character type!!!
table(tan_new$wt_kg) # 'refuse'
tan_new$wt_kg <- as.numeric(tan_new$wt_kg)
summary(tan_new$wt_kg)
# Min. 1st Qu.  Median    Mean  3rd Qu.    Max.    NA's 
# 31.0    94.0   115.0   116.5   137.0   325.0       1 
tan_new$wt_kg <- (tan_new$wt_kg)*0.1 # modification for 1 decimal
summary(tan_new$wt_kg)
#    Min. 1st Qu.  Median    Mean   3rd Qu.    Max.    NA's 
#   3.10    9.40   11.50     11.65   13.70   32.50       1 


#################################################################
### variable hw3_ "Child's height in centimeters (1 decimal)" ###
#################################################################
tan_new$ht_cm <- tan_new$hw3_
summary(tan_new$ht_cm) # character type!!!
table(tan_new$ht_cm)
# ‘other' 9   'refused' 4
tan_new$ht_cm <- as.numeric(tan_new$ht_cm)
summary(tan_new$ht_cm)
#  Min. 1st Qu.  Median    Mean   3rd Qu.     Max.    NA's 
#  514.0   760.0   850.0   850.6   942.0   1181.0      13
tan_new$ht_cm <- tan_new$ht_cm*0.1 # modification for 1 decimal
summary(tan_new$ht_cm)
#   Min.   1st Qu.   Median    Mean    3rd Qu.     Max.    NA's 
#  51.40   76.00     85.00     85.06   94.20     118.10      13 


#############################################
### variable hw4_ "Height/Age percentile" ###
#############################################
tan_new$ht_age_pctl <- tan_new$hw4_
summary(tan_new$ht_age_pctl) # character type!!!
tan_new$ht_age_pctl <- as.numeric(tan_new$ht_age_pctl)
summary(tan_new$ht_age_pctl)
# Min.  1st Qu.   Median     Mean     3rd Qu.     Max.    NA's 
# 0.0   155.8   812.0      1855.4     2686.2   9980.0       86
tan_new$ht_age_pctl <- tan_new$ht_age_pctl*0.01 # modification for percentile
summary(tan_new$ht_age_pctl)
#    Min. 1st Qu.  Median    Mean    3rd Qu.    Max.    NA's 
#  0.000   1.558   8.120   18.554     26.863  99.800      86 


#############################################
### variable hw7_ "Weight/Age percentile" ###
#############################################
tan_new$wt_age_pctl <- tan_new$hw7_
summary(tan_new$wt_age_pctl) # character type!!!
table(tan_new$wt_age_pctl)
tan_new$wt_age_pctl <- as.numeric(tan_new$wt_age_pctl)
summary(tan_new$wt_age_pctl)
# Min.   1st Qu.    Median      Mean    3rd Qu.     Max.    NA's 
# 0.0   324.8       1127.5    2055.7    3037.0    9980.0      86
tan_new$wt_age_pctl <- tan_new$wt_age_pctl*0.01
summary(tan_new$wt_age_pctl)
# Min.   1st Qu.   Median      Mean    3rd Qu.    Max.     NA's 
# 0.000   3.248    11.275    20.557    30.370    99.800      86


#################################################
### variable hw10_ "Weight/height percentile" ###
#################################################
tan_new$wt_ht_pctl <- tan_new$hw10_
summary(tan_new$wt_ht_pctl) # character type!!!
table(tan_new$wt_ht_pctl)
tan_new$wt_ht_pctl <- as.numeric(tan_new$wt_ht_pctl)
summary(tan_new$wt_ht_pctl)
#  Min.  1st Qu.  Median    Mean    3rd Qu.    Max.    NA's 
#    0     1547    3515     3912      5963    9980      86
tan_new$wt_ht_pctl <- tan_new$wt_ht_pctl*0.01
summary(tan_new$wt_ht_pctl)
# Min.   1st Qu.  Median    Mean    3rd Qu.    Max.    NA's 
# 0.00    15.47    35.15    39.12   59.63     99.80      86 


#####################################
### variable hw57_ "anemia level" ###
#####################################
tan_new$anemia_level <- tan_new$hw57_
summary(tan_new$anemia_level) # character type!!!
table(tan_new$anemia_level)
#       mild   moderate not anemic     severe 
#       2183       2378       3330        123
sum(is.na(tan_new$anemia_level)) # no missing value


###########################################
### Factorize all categorical variables ###
###########################################
cols <- c("sex", "resident", "edu", "wealth", "meat1", "vegg1", "fish1","bean1",
          "iron_preg1", "vit_a1", "iron1", "para_drug1", "anemia_level")
tan_new[cols] <- lapply(tan_new[cols], as.factor)

tan_new <- tan_new %>%
  dplyr::select(sex, resident, edu, wealth, wealth_fs,
                meat1, vegg1, fish1, bean1,
                iron_preg1, vit_a1, iron1, para_drug1,
                month_age, wt_kg, ht_cm, ht_age_pctl, wt_age_pctl, wt_ht_pctl, anemia_level)


######################################
### Table summary of all variables ###
######################################
library(arsenal)

# CONTROL
my_controls <- tableby.control(
  numeric.test = "anova",
  cat.test = "chisq",
  numeric.stats = c("meansd", "Nmiss"),
  cat.stats = c("countpct", "Nmiss"),
  stats.labels = list(
    meansd = "Mean (SD)",
    Nmiss = "Missing"))

# LABELS
my_labels <- list(
  anemia_level = "anemia level",
  sex = "sex of children",
  resident = "type of place of residence",
  edu = "highest education level",
  wealth = "wealth index",
  wealth_fs = "wealth index factor score",
  meat1 = "gave child meat",
  vegg1 = "gave child dark green leafy veggie",
  fish1 = "gave child fish/shellfish",
  bean1 = "gave child food made from beans, peas, lentils, nuts",
  iron_preg1 = "Given iron tables/syrup during pregnancy",
  vit_a1 = "vitamin in last 6 months",
  iron1 = "take iron supplements",
  para_drug1 = "drugs for intestinal parasites in last 6 months",
  month_age = "child’s age in months",
  wt_kg = "child’s weight in kg",
  ht_cm = "child’s height in cm",
  ht_age_pctl = "height/age percentile",
  wt_age_pctl= "weight/age percentile",
  wt_ht_pctl = "weight/height percentile")

# overall table
tab1 <- tableby(anemia_level ~ sex + resident + edu + wealth + wealth_fs +
                meat1 + vegg1 + fish1 + bean1 +
                iron_preg1 + vit_a1 + iron1 + para_drug1 +
                month_age + wt_kg + ht_cm + ht_age_pctl + wt_age_pctl + wt_ht_pctl,
                data = tan_new)
tab1
summary(tab1, text = TRUE, 
        labelTranslations = my_labels,
        control = my_controls,
        pfootnote = TRUE)
write2word(tab1, "Table1.doc")

# table of characteristic SSE & demographics
tab1_baseline <- tableby(anemia_level ~ sex + resident + edu + wealth + wealth_fs,
                         data = tan_new)
tab1_baseline
summary(tab1_baseline, text = TRUE, 
        labelTranslations = my_labels,
        control = my_controls,
        pfootnote = TRUE)
write2word(tab1_baseline, "Table1_baseline.doc")

# table of dietary factors
tab1_diet <- tableby(anemia_level ~ meat1 + vegg1 + fish1 + bean1,
                         data = tan_new)
tab1_diet
summary(tab1_diet, text = TRUE,
        labelTranslations = my_labels,
        control = my_controls,
        pfootnote = TRUE)
write2word(tab1_diet, "Table1_diet.doc")

# table of supplements & drugs
tab1_suplm <- tableby(anemia_level ~ iron_preg1 + vit_a1 + iron1 + para_drug1,
                     data = tan_new)
tab1_suplm
summary(tab1_suplm, text = TRUE,
        labelTranslations = my_labels,
        control = my_controls,
        pfootnote = TRUE, )
write2word(tab1_suplm, "Table1_supplement.doc")

# table of anthropometric characteristics
tab1_anthrop <- tableby(anemia_level ~ month_age + wt_kg + ht_cm + ht_age_pctl + wt_age_pctl + wt_ht_pctl,
                      data = tan_new)
tab1_anthrop
summary(tab1_anthrop, text = TRUE,
        labelTranslations = my_labels,
        control = my_controls,
        pfootnote = TRUE)
write2word(tab1_anthrop, "Table1_anthropometric.doc")


#############################################################
### Basic graph: distribution of outcome by measures of x ###
#############################################################
# bar plot for binary/categorical variables
graph1 <- ggplot(tan_new, aes(x = sex, fill = anemia_level)) +
  geom_bar(stat = "count") + theme_minimal()
graph1

graph2 <- ggplot(tan_new, aes(x = resident, fill = anemia_level)) +
  geom_bar(stat = "count") + theme_minimal()
graph2

graph3 <- ggplot(tan_new, aes(x = edu, fill = anemia_level)) +
  geom_bar(stat = "count") + theme_minimal()
graph3

graph4 <- ggplot(tan_new, aes(x = wealth, fill = anemia_level)) +
  geom_bar(stat = "count") + theme_minimal()
graph4

graph5 <- ggplot(tan_new, aes(x = meat1, fill = anemia_level)) +
  geom_bar(stat = "count") + theme_minimal()
graph5

graph6 <- ggplot(tan_new, aes(x = vegg1, fill = anemia_level)) +
  geom_bar(stat = "count") + theme_minimal()
graph6

graph7 <- ggplot(tan_new, aes(x = fish1, fill = anemia_level)) +
  geom_bar(stat = "count") + theme_minimal()
graph7

graph8 <- ggplot(tan_new, aes(x = bean1, fill = anemia_level)) +
  geom_bar(stat = "count") + theme_minimal()
graph8

graph9 <- ggplot(tan_new, aes(x = iron_preg1, fill = anemia_level)) +
  geom_bar(stat = "count") + theme_minimal()
graph9

graph10 <- ggplot(tan_new, aes(x = vit_a1, fill = anemia_level)) +
  geom_bar(stat = "count") + theme_minimal()
graph10

graph11 <- ggplot(tan_new, aes(x = iron1, fill = anemia_level)) +
  geom_bar(stat = "count") + theme_minimal()
graph11

graph12 <- ggplot(tan_new, aes(x = para_drug1, fill = anemia_level)) +
  geom_bar(stat = "count") + theme_minimal()
graph12

# boxplot for continuous variables
boxp1 <- ggplot(tan_new, aes(x = wealth_fs, y = anemia_level)) +
  geom_boxplot() + theme_bw() +
  stat_summary(fun = mean, geom = "point", color = "blue", size = 2) +
  ylab("Anemia level") + xlab("Wealth index score")
boxp1

boxp2 <- ggplot(tan_new, aes(x = month_age, y = anemia_level)) +
  geom_boxplot() + theme_bw() +
  stat_summary(fun = mean, geom = "point", color = "blue", size = 2) +
  ylab("Anemia level") + xlab("Age in months")
boxp2

boxp3 <- ggplot(tan_new, aes(x = wt_kg, y = anemia_level)) +
  geom_boxplot() + theme_bw() +
  stat_summary(fun = mean, geom = "point", color = "blue", size = 2) +
  ylab("Anemia level") + xlab("Child's weight in kg")
boxp3

boxp4 <- ggplot(tan_new, aes(x = ht_cm, y = anemia_level)) +
  geom_boxplot() + theme_bw() +
  stat_summary(fun = mean, geom = "point", color = "blue", size = 2) +
  ylab("Anemia level") + xlab("Child's height in cm")
boxp4

boxp5 <- ggplot(tan_new, aes(x = ht_age_pctl, y = anemia_level)) +
  geom_boxplot() + theme_bw() +
  stat_summary(fun = mean, geom = "point", color = "blue", size = 2) +
  ylab("Anemia level") + xlab("Height/age percentile")
boxp5

boxp6 <- ggplot(tan_new, aes(x = wt_age_pctl, y = anemia_level)) +
  geom_boxplot() + theme_bw() +
  stat_summary(fun = mean, geom = "point", color = "blue", size = 2) +
  ylab("Anemia level") + xlab("Weight/age percentile")
boxp6

boxp6 <- ggplot(tan_new, aes(x = wt_ht_pctl, y = anemia_level)) +
  geom_boxplot() + theme_bw() +
  stat_summary(fun = mean, geom = "point", color = "blue", size = 2) +
  ylab("Anemia level") + xlab("Weight/height percentile")
boxp6

###########################################################################
### Logistic Regression of continuous independent variable - UNADJUSTED ###
###########################################################################
# RELEVEL OUTCOME VARIABLE - ANEMIA_LEVEL
tan_new <- tan_new %>%
  mutate(anemia_level = factor(ifelse(anemia_level == "not anemic", 0, 1)))
levels(tan_new$anemia_level)

# ///// MODEL 1: WEALTH INDEX SCORES /////
m1_unadj <- glm(anemia_level ~ wealth_fs, data = tan_new, family = "binomial")
summary(m1_unadj)
# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  0.31347    0.02316  13.532  < 2e-16 ***
# wealth_fs   -0.14272    0.02426  -5.882 4.05e-09 ***

library(broom)
library(ggplot2)
# extract predicted value
pred_m1_unadj <- augment(m1_unadj)
# figure between logit(y) and x
plot1 <- ggplot(pred_m1_unadj, aes(x = wealth_fs, y = .fitted)) +
  geom_point() +
  labs(y = "Logit(Anemia level)",
       x = "Wealth index score") +
  theme_bw()
plot1

# scatter plot
plot1_2 <- ggplot(tan_new, aes(x = wealth_fs, y = anemia_level)) +
  geom_point() +
  labs(y = "Logit(Anemia level)",
       x = "Wealth index score") +
  theme_bw()
plot1_2

# Look at results
confint(m1_unadj)
exp(m1_unadj$coefficients[2])
coef_m1_unadj <- tidy(m1_unadj, conf.int = TRUE) %>%
  mutate(OR = exp(estimate),
         ORL = exp(conf.low),
         ORH = exp(conf.high))
coef_m1_unadj


# ///// MODEL 2: CHILD'S AGE IN MONTHS /////
m2_unadj <- glm(anemia_level ~ month_age, data = tan_new, family = "binomial")
summary(m2_unadj)
# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  1.565057   0.055286   28.31   <2e-16 ***
# month_age   -0.038963   0.001559  -24.99   <2e-16 ***

# extract predicted value
pred_m2_unadj <- augment(m2_unadj)

# figure between logit(y) and x
plot2 <- ggplot(pred_m2_unadj, aes(x = month_age, y = .fitted)) +
  geom_point() +
  labs(y = "Logit(Anemia level)",
       x = "Age in months") +
  theme_bw()
plot2

# scatter plot
plot2_1 <- ggplot(tan_new, aes(x = month_age, y = anemia_level)) +
  geom_point() +
  labs(y = "Logit(Anemia level)",
       x = "Age in months") +
  theme_bw()
plot2_1

# Look at results
confint(m2_unadj)
exp(m2_unadj$coefficients[2])
coef_m2_unadj <- tidy(m2_unadj, conf.int = TRUE) %>%
  mutate(OR = exp(estimate),
         ORL = exp(conf.low),
         ORH = exp(conf.high))
coef_m2_unadj

# ///// MODEL 3: CHILD'S WEIGHT IN KG /////
m3_unadj <- glm(anemia_level ~ wt_kg, data = tan_new, family = "binomial")
summary(m3_unadj)
# Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  2.726690   0.104845   26.01   <2e-16 ***
#  wt_kg       -0.202728   0.008612  -23.54   <2e-16 ***

# extract predicted value
pred_m3_unadj <- augment(m3_unadj)

# figure between logit(y) and x
plot3 <- ggplot(pred_m3_unadj, aes(x = wt_kg, y = .fitted)) +
  geom_point() +
  labs(y = "Logit(Anemia level)",
       x = "(Child's weight (kg)") +
  theme_bw()
plot3

# scatter plot
plot3_1 <- ggplot(tan_new, aes(x = wt_kg, y = anemia_level)) +
  geom_point() +
  labs(y = "Logit(Anemia level)",
       x = "Child's weight (kg)") +
  theme_bw()
plot3_1

which(tan_new$wt_kg == 32.50, arr.ind = TRUE)
tan_new[6185, 14] # likely an outlier, a child weighed 32.50kg aged 53-month

# Look at results
confint(m3_unadj)
exp(m3_unadj$coefficients[2])
coef_m3_unadj <- tidy(m3_unadj, conf.int = TRUE) %>%
  mutate(OR = exp(estimate),
         ORL = exp(conf.low),
         ORH = exp(conf.high))
coef_m3_unadj

# ///// MODEL 4: CHILD'S HEIGHT /////
m4_unadj <- glm(anemia_level ~ ht_cm, data = tan_new, family = "binomial")
summary(m4_unadj)
# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  5.077144   0.190379   26.67   <2e-16 ***
# ht_cm       -0.055317   0.002193  -25.23   <2e-16 ***

# extract predicted value
pred_m4_unadj <- augment(m4_unadj)

# figure between logit(y) and x
plot4 <- ggplot(pred_m4_unadj, aes(x = ht_cm, y = .fitted)) +
  geom_point() +
  labs(y = "Logit(Anemia level)",
       x = "(Child's height (cm)") +
  theme_bw()
plot4

# scatter plot
plot4_1 <- ggplot(tan_new, aes(x = ht_cm, y = anemia_level)) +
  geom_point() +
  labs(y = "Logit(Anemia level)",
       x = "Child's height (cm)") +
  theme_bw()
plot4_1

# Look at results
confint(m4_unadj)
exp(m4_unadj$coefficients[2])
coef_m4_unadj <- tidy(m4_unadj, conf.int = TRUE) %>%
  mutate(OR = exp(estimate),
         ORL = exp(conf.low),
         ORH = exp(conf.high))
coef_m4_unadj

# ///// MODEL 5: CHILD'S HEIGHT /////
m5_unadj <- glm(anemia_level ~ ht_age_pctl, data = tan_new, family = "binomial")
summary(m5_unadj)
# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  0.3876188  0.0290415  13.347   <2e-16 ***
#  ht_age_pctl -0.0027305  0.0009588  -2.848   0.0044 ** 

# extract predicted value
pred_m5_unadj <- augment(m5_unadj)

# figure between logit(y) and x
plot5 <- ggplot(pred_m5_unadj, aes(x = ht_age_pctl, y = .fitted)) +
  geom_point() +
  labs(y = "Logit(Anemia level)",
       x = "Height/age percentile") +
  theme_bw()
plot5

# scatter plot
plot5_1 <- ggplot(tan_new, aes(x = ht_age_pctl, y = anemia_level)) +
  geom_point() +
  labs(y = "Logit(Anemia level)",
       x = "Height/age percentile") +
  theme_bw()
plot5_1

# Look at results
confint(m5_unadj)
exp(m5_unadj$coefficients[2])
coef_m5_unadj <- tidy(m5_unadj, conf.int = TRUE) %>%
  mutate(OR = exp(estimate),
         ORL = exp(conf.low),
         ORH = exp(conf.high))
coef_m5_unadj


# ///// MODEL 5_5: CHILD'S HEIGHT/AGE PERCENTILE /////
m5_1_unadj <- glm(anemia_level ~ ht_age_pctl, data = tan_new, family = "binomial")
summary(m5_5_unadj)

pred_m5_1_unadj <- augment(m5_5_unadj)


# Look at results
confint(m5_1_unadj)
exp(m5_1_unadj$coefficients[2])
coef_m5_1_unadj <- tidy(m5_1_unadj, conf.int = TRUE) %>%
  mutate(OR = exp(estimate),
         ORL = exp(conf.low),
         ORH = exp(conf.high))
coef_m5_1_unadj


# ///// MODEL 6: WEIGHT/AGE PERCENTILE /////
m6_unadj <- glm(anemia_level ~ wt_age_pctl, data = tan_new, family = "binomial")
summary(m6_unadj)
# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  0.3931444  0.0305956  12.850  < 2e-16 ***
# wt_age_pctl -0.0027337  0.0009828  -2.782  0.00541 ** 

# extract predicted value
pred_m6_unadj <- augment(m6_unadj)

# figure between logit(y) and x
plot6 <- ggplot(pred_m6_unadj, aes(x = wt_age_pctl, y = .fitted)) +
  geom_point() +
  labs(y = "Logit(Anemia level)",
       x = "Weight/age percentile") +
  theme_bw()
plot6

# scatter plot
plot6_1 <- ggplot(tan_new, aes(x = wt_age_pctl, y = anemia_level)) +
  geom_point() +
  labs(y = "Logit(Anemia level)",
       x = "Weight/age percentile") +
  theme_bw()
plot6_1

# Look at results
confint(m6_unadj)
exp(m6_unadj$coefficients[2])
coef_m6_unadj <- tidy(m6_unadj, conf.int = TRUE) %>%
  mutate(OR = exp(estimate),
         ORL = exp(conf.low),
         ORH = exp(conf.high))
coef_m6_unadj


# ///// MODEL 7: WEIGHT/HEIGHT PERCENTILE /////
m7_unadj <- glm(anemia_level ~ wt_ht_pctl, data = tan_new, family = "binomial")
summary(m7_unadj)
# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  0.3741773  0.0398873   9.381   <2e-16 ***
# wt_ht_pctl  -0.0009580  0.0008344  -1.148    0.251

# Look at results
confint(m7_unadj)
exp(m7_unadj$coefficients[2])
coef_m7_unadj <- tidy(m7_unadj, conf.int = TRUE) %>%
  mutate(OR = exp(estimate),
         ORL = exp(conf.low),
         ORH = exp(conf.high))
coef_m7_unadj


####################################################################
### Logistic Regression of binary independent variable - DIETARY ###
####################################################################

# ///// MODEL 8: MEAT /////
# Check if levels and class are appropriate
class(tan_new$meat1)
levels(tan_new$meat1)
m8_unadj <- glm(anemia_level ~ meat1, data = tan_new, family = "binomial")

# Look at results
summary(m8_unadj)
confint(m8_unadj)
exp(m8_unadj$coefficients[2])
coef_m8_unadj <- tidy(m8_unadj, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), 
         ORL = exp(conf.low),
         ORH = exp(conf.high))
coef_m8_unadj


# ///// MODEL 9: GREEN LEAFY VEGETABLES /////
# Check if levels and class are appropriate
class(tan_new$vegg1)
levels(tan_new$vegg1)
m9_unadj <- glm(anemia_level ~ vegg1, data = tan_new, family = "binomial")

# Look at results
summary(m9_unadj)
confint(m9_unadj)
exp(m9_unadj$coefficients[2])
coef_m9_unadj <- tidy(m10_unadj, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), 
         ORL = exp(conf.low),
         ORH = exp(conf.high))
coef_m9_unadj


# ///// MODEL 10: FISH/SHELLFISH /////
# Check if levels and class are appropriate
class(tan_new$fish1)
levels(tan_new$fish1)
m10_unadj <- glm(anemia_level ~ fish1, data = tan_new, family = "binomial")

# Look at results
summary(m10_unadj)
confint(m10_unadj)
exp(m10_unadj$coefficients[2])
coef_m10_unadj <- tidy(m10_unadj, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), 
         ORL = exp(conf.low),
         ORH = exp(conf.high))
coef_m10_unadj


# ///// MODEL 11: FOOD MADE OF BEAN, PEAS... /////
# Check if levels and class are appropriate
class(tan_new$bean1)
levels(tan_new$bean1)
m11_unadj <- glm(anemia_level ~ bean1, data = tan_new, family = "binomial")

# Look at results
summary(m11_unadj)
confint(m11_unadj)
exp(m11_unadj$coefficients[2])
coef_m11_unadj <- tidy(m11_unadj, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), 
         ORL = exp(conf.low),
         ORH = exp(conf.high))
coef_m11_unadj

############################################################################
### Logistic Regression of binary independent variable - SUPPLEMENT/DRUG ###
############################################################################

# iron_preg1 + vit_a1 + iron1 + para_drug1 +

# ///// MODEL 12: Iron supplement during pregnancy /////
# Check if levels and class are appropriate
class(tan_new$iron_preg1)
levels(tan_new$iron_preg1)
m12_unadj <- glm(anemia_level ~ iron_preg1, data = tan_new, family = "binomial")

# Look at results
summary(m12_unadj)
confint(m12_unadj)
exp(m12_unadj$coefficients[2])
coef_m12_unadj <- tidy(m12_unadj, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), 
         ORL = exp(conf.low),
         ORH = exp(conf.high))
coef_m12_unadj


# ///// MODEL 13: Child: having vitamin in last 6 months /////
# Check if levels and class are appropriate
class(tan_new$vit_a1)
levels(tan_new$vit_a1)
m13_unadj <- glm(anemia_level ~ vit_a1, data = tan_new, family = "binomial")

# Look at results
summary(m13_unadj)
confint(m13_unadj)
exp(m13_unadj$coefficients[2])
coef_m13_unadj <- tidy(m13_unadj, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), 
         ORL = exp(conf.low),
         ORH = exp(conf.high))
coef_m13_unadj


# ///// MODEL 14: Gave child iron supplement in last 6 months /////
# Check if levels and class are appropriate
class(tan_new$iron1)
levels(tan_new$iron1)
m14_unadj <- glm(anemia_level ~ iron1, data = tan_new, family = "binomial")

# Look at results
summary(m14_unadj)
confint(m14_unadj)
exp(m14_unadj$coefficients[2])
coef_m14_unadj <- tidy(m14_unadj, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), 
         ORL = exp(conf.low),
         ORH = exp(conf.high))
coef_m14_unadj


# ///// MODEL 15: Drugs for intestinal parasites in last 6 months /////
# Check if levels and class are appropriate
class(tan_new$para_drug1)
levels(tan_new$para_drug1)
m15_unadj <- glm(anemia_level ~ para_drug1, data = tan_new, family = "binomial")

# Look at results
summary(m15_unadj)
confint(m15_unadj)
exp(m15_unadj$coefficients[2])
coef_m15_unadj <- tidy(m15_unadj, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), 
         ORL = exp(conf.low),
         ORH = exp(conf.high))
coef_m15_unadj


#############################################################################
### Logistic Regression of binary independent variable - SES/DEMOGRAPHICS ###
#############################################################################

# ///// MODEL 16: Child's sex /////
# Check if levels and class are appropriate
class(tan_new$sex)
levels(tan_new$sex)
m16_unadj <- glm(anemia_level ~ sex, data = tan_new, family = "binomial")

# Look at results
summary(m16_unadj)
confint(m16_unadj)
exp(m16_unadj$coefficients[2])
coef_m16_unadj <- tidy(m16_unadj, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), 
         ORL = exp(conf.low),
         ORH = exp(conf.high))
coef_m16_unadj


# ///// MODEL 17: Place of residency /////
# Check if levels and class are appropriate
class(tan_new$resident)
levels(tan_new$resident)
m17_unadj <- glm(anemia_level ~ resident, data = tan_new, family = "binomial")

# Look at results
summary(m17_unadj)
confint(m17_unadj)
exp(m17_unadj$coefficients[2])
coef_m17_unadj <- tidy(m17_unadj, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), 
         ORL = exp(conf.low),
         ORH = exp(conf.high))
coef_m17_unadj


# ///// MODEL 18: Highest education /////
# Check if levels and class are appropriate
class(tan_new$edu)
levels(tan_new$edu)
tan_new$edu <- factor(tan_new$edu, levels = c("no education", "primary",
                                              "secondary", "higher"))
levels(tan_new$edu)

m18_unadj <- glm(anemia_level ~ edu, data = tan_new, family = "binomial")

# Look at results
summary(m18_unadj)
confint(m18_unadj)
exp(m18_unadj$coefficients[2])
coef_m18_unadj <- tidy(m18_unadj, conf.int = TRUE) %>%
  mutate(OR = exp(estimate), 
         ORL = exp(conf.low),
         ORH = exp(conf.high))
coef_m18_unadj


#########################################################
### AUTOMATIC VARIABLE SELECTION: LOGISTIC REGRESSION ###
#########################################################
library(MASS)
tan_update <- tan_new %>%
  na.omit # obs drop from 8014 to 2919

# full model
full_model <- glm(anemia_level ~ ., data = tan_update, family = "binomial")
coef(full_model)

# step-wise variable selection
step_model <- full_model %>% stepAIC(direction = "both", trace = FALSE)
summary(step_model)

# backward selection
back_model <- full_model %>% stepAIC(direction = "backward", trace = TRUE)
summary(back_model)

# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  0.54897    0.84344   0.651 0.515131    
# sexmale      0.35727    0.08855   4.035 5.47e-05 ***
# wealth_fs   -0.13574    0.04666  -2.909 0.003623 ** 
# vegg1yes    -0.16454    0.08896  -1.850 0.064378 .  
# fish1yes     0.24598    0.10349   2.377 0.017459 *  
# month_age   -0.04595    0.01361  -3.376 0.000735 ***
# wt_kg       -0.11957    0.04619  -2.589 0.009638 ** 
# ht_cm        0.02870    0.01600   1.793 0.072897 .

### !!! Therefore, we choose these 7 predictors back to our model adjustments
### !!! for the data frame with 8014 observations



#################################################################
### ADJUSTED MODEL 2: 7 predictors - derived from auto-select ###
#################################################################
select_model_adj2 <- glm(anemia_level ~ sex + wealth_fs + vegg1 + fish1 +
                           month_age + wt_kg + ht_cm,
                         data = tan_new, family = "binomial")
pred_select_model_adj2 <- augment(select_model_adj2)
summary(select_model_adj2)
# Coefficients:
#   Estimate    Pr(>|z|)    
# (Intercept)    1.04e-09 ***
#   sexmale      6.58e-07 ***
#   wealth_fs    0.000141 ***
#   vegg1yes     0.019621 *  
#   fish1yes     0.000326 ***
#   month_age    0.000487 ***
#   wt_kg        0.027245 *  
#   ht_cm        0.064317 . 

library(car)
vif(select_model_adj2)
# sex      wealth_fs     vegg1     fish1 month_age     wt_kg     ht_cm 
# 1.027470  1.082462  1.014401  1.038716  6.639144  6.511560 10.998955 

# Look at results
confint(select_model_adj2)
exp(select_model_adj2$coefficients)
coef_select_model_adj2 <- tidy(select_model_adj2, conf.int = TRUE) %>%
  mutate(OR = exp(estimate),
         ORL = exp(conf.low),
         ORH = exp(conf.high))
coef_select_model_adj2


### create squares for continous predictors ###
tan_new <- tan_new %>%
  mutate(wealth_fs_sq = wealth_fs^2)
tan_new <- tan_new %>%
  mutate(month_age_sq = month_age^2)
tan_new <- tan_new %>%
  mutate(wt_kg_sq = wt_kg^2)
tan_new <- tan_new %>%
  mutate(ht_cm_sq = ht_cm^2)
tan_new <- tan_new %>%
  mutate(wt_age_pctl_sq = wt_age_pctl^2)
tan_new <- tan_new %>%
  mutate(ht_age_pctl_sq = ht_age_pctl^2)
tan_new <- tan_new %>%
  mutate(wt_ht_pctl_sq = wt_ht_pctl^2)
  
###############################################################
### Logistic regression: Assumptions & diagnostics - PART 1 ###
###     FOR MODEL 2 (7 predictors, 4 continuous)            ###
###############################################################

# Square of wealth factor score for Model 2 with 7 predictors
select_model_adj2_wealthsq <- glm(anemia_level ~ sex + wealth_fs + vegg1 + fish1 + 
                                   month_age + wt_kg + ht_cm + wealth_fs_sq,
                                 data = tan_new, family = "binomial")
summary(select_model_adj2_wealthsq) # significant p value = 0.0223 *

# Square of month_age for Model 2 with 7 predictors
select_model_adj2_agesq <- glm(anemia_level ~ sex + wealth_fs + vegg1 + fish1 + 
                                month_age + wt_kg + ht_cm + month_age_sq,
                              data = tan_new, family = "binomial")
summary(select_model_adj2_agesq) # significant p value = 0.00368 **

# Square of wt_kg for Model 2 with 7 predictors
select_model_adj2_wtsq <- glm(anemia_level ~ sex + wealth_fs + vegg1 + fish1 + 
                               month_age + wt_kg + ht_cm + wt_kg_sq,
                             data = tan_new, family = "binomial")
summary(select_model_adj2_wtsq) # non-significant p value = 0.236

# Square of ht_cm for Model 2 with 7 predictors
select_model_adj2_htsq <- glm(anemia_level ~ sex + wealth_fs + vegg1 + fish1 + 
                               month_age + wt_kg + ht_cm + ht_cm_sq,
                             data = tan_new, family = "binomial")
summary(select_model_adj2_htsq) # non-significant p value = 0.801

library(lmtest)
lrtest(select_model_adj2, select_model_adj2_wealthsq) # p = 0.02162 *
lrtest(select_model_adj2, select_model_adj2_agesq) # p = 0.003648 **
lrtest(select_model_adj2, select_model_adj2_wtsq) # p = 0.2373
lrtest(select_model_adj2, select_model_adj2_htsq) # p = 0.8011

### Check linearity visually ###
# wealth index factor scores 
logit_plot1 <- ggplot(pred_select_model_adj2, aes(y = .fitted, x = wealth_fs)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "Logit(Anemia level)",
       x = "Wealth index scores") +
  theme_bw()
logit_plot1 # not quite linear

# child's age in months
logit_plot2 <- ggplot(pred_select_model_adj2, aes(y = .fitted, x = month_age)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "Logit(Anemia level)",
       x = "Age in months") +
  theme_bw()
logit_plot2 # almost perfectly linear

# child's weight in kg
logit_plot3 <- ggplot(pred_select_model_adj2, aes(y = .fitted, x = wt_kg)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "Logit(Anemia level)",
       x = "Weight (kg)") +
  theme_bw()
logit_plot3 # approximately linear except the higher end of weight

# child's height in cm
logit_plot4 <- ggplot(pred_select_model_adj2, aes(y = .fitted, x = ht_cm)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "Logit(Anemia level)",
       x = "Height (cm)") +
  theme_bw()
logit_plot4 # almost linear, except at the lower end of height

all_logit_plot_model2 <- ggarrange(logit_plot1, logit_plot2, logit_plot3, logit_plot4,
                                   labels = c("A", "B", "C", "D"),
                                   ncol = 2, nrow = 2)
all_logit_plot_model2


##########################################
### Adjusted Logistic Regression Model ###
##########################################

### ADJUSTED MODEL 1 (FULL): 14 predictors
# FYI. We later decided not use include percentiles in our adjusted models,
#      since percentiles themselves are sorted of adjusted
# FYI. We also deleted 'iron1' due to its nature of perfect multicollinearity,
#      which was discovered when we tried to run the VIF test for the model
#      that included 'iron1' and the VIF showed the error of
#      "aliased coefficients in the model"!!!

select_model_adj1 <- glm(anemia_level ~ sex + wealth_fs + vegg1 + fish1 +
                           month_age + wt_kg + ht_cm + meat1 + bean1 +
                           iron_preg1 + vit_a1 + para_drug1 + edu + resident +
                           wt_age_pctl + ht_age_pctl + wt_ht_pctl,
                         data = tan_new, family = "binomial")
summary(select_model_adj1)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   -1.496e-01  2.012e+00  -0.074 0.940722    
# sexmale        3.916e-01  1.038e-01   3.771 0.000163 ***
#   wealth_fs     -9.770e-02  6.891e-02  -1.418 0.156246    
# vegg1yes      -1.478e-01  9.001e-02  -1.642 0.100668    
# fish1yes       2.444e-01  1.049e-01   2.329 0.019863 *  
#   month_age     -4.369e-02  2.655e-02  -1.646 0.099865 .  
# wt_kg         -2.374e-01  1.537e-01  -1.545 0.122436    
# ht_cm          5.218e-02  4.288e-02   1.217 0.223657    
# meat1yes       4.883e-02  1.337e-01   0.365 0.714912    
# bean1yes      -1.250e-01  9.025e-02  -1.385 0.166074    
# iron_preg1yes  6.320e-02  1.095e-01   0.577 0.563751    
# vit_a1yes     -5.579e-02  9.668e-02  -0.577 0.563948    
# para_drug1yes  1.083e-01  1.073e-01   1.010 0.312731    
# eduprimary    -2.259e-01  1.197e-01  -1.887 0.059186 .  
# edusecondary  -2.878e-01  1.592e-01  -1.808 0.070617 .  
# eduhigher     -5.088e-01  4.306e-01  -1.182 0.237343    
# residenturban -5.105e-03  1.295e-01  -0.039 0.968548    
# wt_age_pctl    1.196e-03  4.695e-03   0.255 0.798938    
# ht_age_pctl   -4.498e-05  4.033e-03  -0.011 0.991102    
# wt_ht_pctl     3.360e-03  4.829e-03   0.696 0.486601 

VIF(select_model_adj1)
# GVIF Df GVIF^(1/(2*Df))
# sex          1.436582  1        1.198575
# wealth_fs    2.350339  1        1.533082
# vegg1        1.073249  1        1.035977
# fish1        1.085631  1        1.041936
# month_age    9.604922  1        3.099181
# wt_kg       29.444747  1        5.426301
# ht_cm       29.942895  1        5.472010
# meat1        1.087115  1        1.042648
# bean1        1.020769  1        1.010331
# iron_preg1   1.021268  1        1.010578
# vit_a1       1.241605  1        1.114274
# para_drug1   1.337815  1        1.156640
# edu          1.524252  3        1.072777
# resident     1.695384  1        1.302069
# wt_age_pctl  7.518394  1        2.741969
# ht_age_pctl  5.032158  1        2.243247
# wt_ht_pctl  11.281848  1        3.358846

# Look at results
confint(select_model_adj1)
exp(select_model_adj1$coefficients)
coef_select_model_adj1 <- tidy(select_model_adj1, conf.int = TRUE) %>%
  mutate(OR = exp(estimate),
         ORL = exp(conf.low),
         ORH = exp(conf.high))
coef_select_model_adj1


#################################################################
### DIAGNOTIC TESTING - LINEARITY FOR MODEL 1 - 17 predictors ###
#################################################################
# Square of wealth factor score for Model 1 with 17 predictors
select_model_adj1_wealthsq <- glm(anemia_level ~ sex + wealth_fs + vegg1 + fish1 +
                                   month_age + wt_kg + ht_cm + meat1 + bean1 +
                                   iron_preg1 + vit_a1 + para_drug1 + edu + resident +
                                    wt_age_pctl + ht_age_pctl + wt_ht_pctl + wealth_fs_sq,
                                 data = tan_new, family = "binomial")
summary(select_model_adj1_wealthsq) # non-significant p value = 0.765861

# Square of month_age for Model 1 with 17 predictors
select_model_adj1_agesq <- glm(anemia_level ~ sex + wealth_fs + vegg1 + fish1 +
                                 month_age + wt_kg + ht_cm + meat1 + bean1 +
                                 iron_preg1 + vit_a1 + para_drug1 + edu + resident +
                                 wt_age_pctl + ht_age_pctl + wt_ht_pctl + month_age_sq,
                              data = tan_new, family = "binomial")
summary(select_model_adj1_agesq) # significant p value = 0.00027 ***

# Square of wt_kg for for Model 1 with 17 predictors
select_model_adj1_wtsq <- glm(anemia_level ~ sex + wealth_fs + vegg1 + fish1 +
                                month_age + wt_kg + ht_cm + meat1 + bean1 +
                                iron_preg1 + vit_a1 + para_drug1 + edu + resident +
                                wt_age_pctl + ht_age_pctl + wt_ht_pctl + wt_kg_sq,
                             data = tan_new, family = "binomial")
summary(select_model_adj1_wtsq) # non-significant p value = 0.738834

# Square of ht_cm for for Model 1 with 17 predictors
select_model_adj1_htsq <- glm(anemia_level ~ sex + wealth_fs + vegg1 + fish1 +
                                month_age + wt_kg + ht_cm + meat1 + bean1 +
                                iron_preg1 + vit_a1 + para_drug1 + edu + resident +
                                wt_age_pctl + ht_age_pctl + wt_ht_pctl + ht_cm_sq,
                             data = tan_new, family = "binomial")
summary(select_model_adj1_htsq) # significant p value = 0.01576 *

# Square of ht_age_pctl for for Model 1 with 17 predictors
select_model_adj1_ht_age_pctlsq <- glm(anemia_level ~ sex + wealth_fs + vegg1 + fish1 +
                                month_age + wt_kg + ht_cm + meat1 + bean1 +
                                iron_preg1 + vit_a1 + para_drug1 + edu + resident +
                                wt_age_pctl + ht_age_pctl + wt_ht_pctl + ht_age_pctl_sq,
                              data = tan_new, family = "binomial")
summary(select_model_adj1_ht_age_pctlsq) # non-significant p value = 0.160350

# Square of wt_age_pctl for for Model 1 with 17 predictors
select_model_adj1_wt_age_pctlsq <- glm(anemia_level ~ sex + wealth_fs + vegg1 + fish1 +
                                         month_age + wt_kg + ht_cm + meat1 + bean1 +
                                         iron_preg1 + vit_a1 + para_drug1 + edu + resident +
                                         wt_age_pctl + ht_age_pctl + wt_ht_pctl + wt_age_pctl_sq,
                                       data = tan_new, family = "binomial")
summary(select_model_adj1_wt_age_pctlsq) # non-significant p value = 0.961801

# Square of wt_ht_pctl for for Model 1 with 17 predictors
select_model_adj1_wt_ht_pctlsq <- glm(anemia_level ~ sex + wealth_fs + vegg1 + fish1 +
                                         month_age + wt_kg + ht_cm + meat1 + bean1 +
                                         iron_preg1 + vit_a1 + para_drug1 + edu + resident +
                                         wt_age_pctl + ht_age_pctl + wt_ht_pctl + wt_ht_pctl_sq,
                                       data = tan_new, family = "binomial")
summary(select_model_adj1_wt_ht_pctlsq) # non-significant p value = 0.994170


library(lmtest)
lrtest(select_model_adj1, select_model_adj1_wealthsq) # p = 0.7657
lrtest(select_model_adj1, select_model_adj1_agesq) # p = 0.0002711 **
lrtest(select_model_adj1, select_model_adj1_wtsq) # p = 0.7387
lrtest(select_model_adj1, select_model_adj1_htsq) # p = 0.01664 *
lrtest(select_model_adj1, select_model_adj1_wt_age_pctlsq) # p = 0.9942
lrtest(select_model_adj1, select_model_adj1_ht_age_pctlsq) # p = 0.1562
lrtest(select_model_adj1, select_model_adj1_wt_ht_pctlsq) # p = 0.9942


### Check linearity visually ###
# wealth index factor scores
pred_select_model_adj1 <- augment(select_model_adj1)

logit_plot5 <- ggplot(pred_select_model_adj1, aes(y = .fitted, x = wealth_fs)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "Logit(Anemia level)",
       x = "Wealth index scores") +
  theme_bw()
logit_plot5 # not quite linear

# child's age in months
logit_plot6 <- ggplot(pred_select_model_adj1, aes(y = .fitted, x = month_age)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "Logit(Anemia level)",
       x = "Age in months") +
  theme_bw()
logit_plot6 # perfectly linear

# child's weight in kg
logit_plot7 <- ggplot(pred_select_model_adj1, aes(y = .fitted, x = wt_kg)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "Logit(Anemia level)",
       x = "Weight (kg)") +
  theme_bw()
logit_plot7 # approximately linear except the higher end of weight

# child's height in cm
logit_plot8 <- ggplot(pred_select_model_adj1, aes(y = .fitted, x = ht_cm)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "Logit(Anemia level)",
       x = "Height (cm)") +
  theme_bw()
logit_plot8 # not linear at all - sin curve

#############################################################################
### ADJUSTED MODEL 3: 6 predictors (exclude age_month, since no linearity ###
###                   in the full model with 17 predictors)               ###
#############################################################################
select_model_adj3 <- glm(anemia_level ~ sex + wealth_fs + vegg1 + fish1 +
                           wt_kg + ht_cm,
                         data = tan_new, family = "binomial")
summary(select_model_adj3)
# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  4.21731    0.31577  13.356  < 2e-16 ***
#   sexmale      0.33090    0.06205   5.333 9.68e-08 ***
#   wealth_fs   -0.10992    0.03429  -3.205 0.001349 ** 
#   vegg1yes    -0.12986    0.06195  -2.096 0.036058 *  
#   fish1yes     0.28474    0.07698   3.699 0.000216 ***
#   wt_kg       -0.06982    0.02744  -2.545 0.010929 *  
#   ht_cm       -0.03660    0.00682  -5.366 8.05e-08 ***

VIF(select_model_adj3)
# sex      wealth_fs     vegg1     fish1     wt_kg     ht_cm 
# 1.019091  1.042475  1.008910  1.038376  6.403452  6.369253

# look at results
confint(select_model_adj3)
exp(select_model_adj3$coefficients)
coef_select_model_adj3 <- tidy(select_model_adj3, conf.int = TRUE) %>%
  mutate(OR = exp(estimate),
         ORL = exp(conf.low),
         ORH = exp(conf.high))
coef_select_model_adj3

################################################################
### DIAGNOTIC TESTING - LINEARITY FOR MODEL 3 - 6 predictors ###
################################################################
# Square of wealth factor score for Model 3 with 6 predictors
select_model_adj3_wealthsq <- glm(anemia_level ~ sex + wealth_fs + vegg1 + fish1 +
                                    wt_kg + ht_cm + wealth_fs_sq,
                                  data = tan_new, family = "binomial")
summary(select_model_adj3_wealthsq) # significant p value = 0.01838 *

# Square of wt_kg for for Model 3 with 6 predictors
select_model_adj3_wtsq <- glm(anemia_level ~ sex + wealth_fs + vegg1 + fish1 +
                                wt_kg + ht_cm + wt_kg_sq,
                              data = tan_new, family = "binomial")
summary(select_model_adj3_wtsq) # non-significant p value = 0.218591

# Square of ht_cm for for Model 3 with 6 predictors
select_model_adj3_htsq <- glm(anemia_level ~ sex + wealth_fs + vegg1 + fish1 +
                                wt_kg + ht_cm + ht_cm_sq,
                              data = tan_new, family = "binomial")
summary(select_model_adj3_htsq) # non-significant p value = 0.620358

lrtest(select_model_adj3, select_model_adj3_wealthsq) # p = 0.01779 *
lrtest(select_model_adj3, select_model_adj3_wtsq) # p = 0.22
lrtest(select_model_adj3, select_model_adj3_htsq) # p = 0.6205

### Check linearity visually ###
# wealth index factor scores
pred_select_model_adj3 <- augment(select_model_adj3)

logit_plot9 <- ggplot(pred_select_model_adj3, aes(y = .fitted, x = wealth_fs)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "Logit(Anemia level)",
       x = "Wealth index scores") +
  theme_bw()
logit_plot9 # not quite linear


# child's weight in kg
logit_plot10 <- ggplot(pred_select_model_adj3, aes(y = .fitted, x = wt_kg)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "Logit(Anemia level)",
       x = "Weight (kg)") +
  theme_bw()
logit_plot10 # approximately linear except the higher end of weight


# child's height in cm
logit_plot11 <- ggplot(pred_select_model_adj3, aes(y = .fitted, x = ht_cm)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(method = "loess", formula = y ~ x) +
  labs(y = "Logit(Anemia level)",
       x = "Height (cm)") +
  theme_bw()
logit_plot11 # almost perfectly linear

library(ggpubr)
all_logit_plot_model3 <- ggarrange(logit_plot9, logit_plot10, logit_plot11,
                                   labels = c("A", "B", "C"),
                                   ncol = 3, nrow = 1)
all_logit_plot_model3

##############################################################################
### ADJUSTED MODEL 4: 5 predictors (exclude age_month & wealth_fs,         ###
###                   since no linearity in the  model with 7 predictors)  ###
##############################################################################
select_model_adj4 <- glm(anemia_level ~ sex + vegg1 + fish1 + wt_kg + ht_cm,
                         data = tan_new, family = "binomial")
summary(select_model_adj4)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  4.196711   0.315344  13.308  < 2e-16 ***
#   sexmale      0.327576   0.061969   5.286 1.25e-07 ***
#   vegg1yes    -0.123673   0.061858  -1.999  0.04557 *  
#   fish1yes     0.238888   0.075501   3.164  0.00156 ** 
#   wt_kg       -0.072951   0.027381  -2.664  0.00771 ** 
#   ht_cm       -0.035482   0.006803  -5.215 1.83e-07 ***

VIF(select_model_adj4)
# sex         vegg1    fish1    wt_kg    ht_cm 
# 1.018450 1.008045 1.001206 6.408654 6.363113 

# look at results
confint(select_model_adj4)
exp(select_model_adj4$coefficients)
coef_select_model_adj4 <- tidy(select_model_adj4, conf.int = TRUE) %>%
  mutate(OR = exp(estimate),
         ORL = exp(conf.low),
         ORH = exp(conf.high))
coef_select_model_adj4


################################################################
### DIAGNOTIC TESTING - LINEARITY FOR MODEL 4 - 5 predictors ###
################################################################
# Square of wt_kg for for Model 4 with 5 predictors
select_model_adj4_wtsq <- glm(anemia_level ~ sex + vegg1 + fish1 +
                                wt_kg + ht_cm + wt_kg_sq,
                              data = tan_new, family = "binomial")
summary(select_model_adj4_wtsq) # non-significant p value = 0.2227

# Square of ht_cm for for Model 4 with 5 predictors
select_model_adj4_htsq <- glm(anemia_level ~ sex + vegg1 + fish1 +
                                wt_kg + ht_cm + ht_cm_sq,
                              data = tan_new, family = "binomial")
summary(select_model_adj4_htsq) # non-significant p value = 0.58593

lrtest(select_model_adj4, select_model_adj4_wtsq) # p = 0.2239
lrtest(select_model_adj4, select_model_adj4_htsq) # p = 0.586


##################################################################
### ADJUSTED MODEL 5: 7 predictors - replaced with percentiles ###
##################################################################
select_model_adj5 <- glm(anemia_level ~ sex + wealth_fs + vegg1 + fish1 +
                           month_age + ht_age_pctl + wt_age_pctl,
                         data = tan_new, family = "binomial")
pred_select_model_adj5 <- augment(select_model_adj5)
summary(select_model_adj5)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  1.593411   0.087276  18.257  < 2e-16 ***
#   sexmale      0.254588   0.061897   4.113  3.9e-05 ***
#   wealth_fs   -0.140916   0.035010  -4.025  5.7e-05 ***
#   vegg1yes    -0.160891   0.062356  -2.580 0.009874 ** 
#   fish1yes     0.263247   0.077134   3.413 0.000643 ***
#   month_age   -0.038166   0.001973 -19.348  < 2e-16 ***
#   ht_age_pctl -0.001255   0.001651  -0.760 0.447170    
# wt_age_pctl -0.003445   0.001651  -2.086 0.036973 * 

vif(select_model_adj5)
# sex        wealth_fs       vegg1       fish1   month_age ht_age_pctl wt_age_pctl 
# 1.006460    1.085621    1.014123    1.035967    1.043722    1.645277    1.635366

# Look at results
confint(select_model_adj5)
exp(select_model_adj5$coefficients)
coef_select_model_adj5 <- tidy(select_model_adj5, conf.int = TRUE) %>%
  mutate(OR = exp(estimate),
         ORL = exp(conf.low),
         ORH = exp(conf.high))
coef_select_model_adj5


##############################################################################
### DIAGNOTIC TESTING - LINEARITY FOR MODEL 5 - 7 predictors - percentiles ###
##############################################################################

# Square of wt_age_pctl for for Model 5 with 7 predictors
select_model_adj5_wt_age_pctl_sq <- glm(anemia_level ~ sex + wealth_fs + vegg1 + fish1 +
                                          month_age + ht_age_pctl + wt_age_pctl + wt_age_pctl_sq,
                              data = tan_new, family = "binomial")
summary(select_model_adj5_wt_age_pctl_sq) # non-significant p value = 0.190629 

# Square of ht_age_pctl for for Model 5 with 7 predictors
select_model_adj5_ht_age_pctl_sq <- glm(anemia_level ~ sex + wealth_fs + vegg1 + fish1 +
                                          month_age + ht_age_pctl + wt_age_pctl + ht_age_pctl_sq,
                              data = tan_new, family = "binomial")
summary(select_model_adj5_ht_age_pctl_sq) # non-significant p value = 0.118838

lrtest(select_model_adj5, select_model_adj5_wt_age_pctl_sq) # p = 0.1896
lrtest(select_model_adj5, select_model_adj5_ht_age_pctl_sq) # p = 0.1178


##################################################################
### ADJUSTED MODEL 6: 8 predictors - replaced with percentiles ###
##################################################################
select_model_adj6 <- glm(anemia_level ~ sex + wealth_fs + vegg1 + fish1 +
                           month_age + ht_age_pctl + wt_age_pctl + wt_ht_pctl,
                         data = tan_new, family = "binomial")
pred_select_model_adj6 <- augment(select_model_adj6)
summary(select_model_adj6)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  1.6153633  0.1046347  15.438  < 2e-16 ***
#   sexmale      0.2535444  0.0619586   4.092 4.27e-05 ***
#   wealth_fs   -0.1412231  0.0350184  -4.033 5.51e-05 ***
#   vegg1yes    -0.1616104  0.0623865  -2.590 0.009585 ** 
#   fish1yes     0.2619366  0.0772115   3.392 0.000693 ***
#   month_age   -0.0381319  0.0019743 -19.315  < 2e-16 ***
#   ht_age_pctl -0.0018877  0.0023407  -0.806 0.419968    
# wt_age_pctl -0.0023890  0.0032225  -0.741 0.458479    
# wt_ht_pctl  -0.0008174  0.0021433  -0.381 0.702912   

vif(select_model_adj6)
# sex   wealth_fs       vegg1       fish1   month_age ht_age_pctl wt_age_pctl  wt_ht_pctl 
# 1.008411    1.086135    1.015064    1.037974    1.045685    3.310261    6.220143    3.823171 

# Look at results
confint(select_model_adj6)
exp(select_model_adj6$coefficients)
coef_select_model_adj6 <- tidy(select_model_adj6, conf.int = TRUE) %>%
  mutate(OR = exp(estimate),
         ORL = exp(conf.low),
         ORH = exp(conf.high))
coef_select_model_adj6


##############################################################################
### DIAGNOTIC TESTING - LINEARITY FOR MODEL 6 - 8 predictors - percentiles ###
##############################################################################

# Square of wt_age_pctl for for Model 6 with 8 predictors
select_model_adj6_wt_age_pctl_sq <- glm(anemia_level ~ sex + wealth_fs + vegg1 + fish1 +
                                          month_age + ht_age_pctl + wt_age_pctl + wt_ht_pctl + wt_age_pctl_sq,
                                        data = tan_new, family = "binomial")
summary(select_model_adj6_wt_age_pctl_sq) # non-significant p value = 0.209737

# Square of ht_age_pctl for for Model 6 with 8 predictors
select_model_adj6_ht_age_pctl_sq <- glm(anemia_level ~ sex + wealth_fs + vegg1 + fish1 +
                                          month_age + ht_age_pctl + wt_age_pctl + wt_ht_pctl + ht_age_pctl_sq,
                                        data = tan_new, family = "binomial")
summary(select_model_adj6_ht_age_pctl_sq) # non-significant p value = 0.105562

# Square of wt_ht_pctl for for Model 6 with 8 predictors
select_model_adj6_wt_ht_pctl_sq <- glm(anemia_level ~ sex + wealth_fs + vegg1 + fish1 +
                                         month_age + ht_age_pctl + wt_age_pctl + wt_ht_pctl + wt_ht_pctl_sq,
                                        data = tan_new, family = "binomial")
summary(select_model_adj6_ht_age_pctl_sq) # non-significant p value = 0.105562

lrtest(select_model_adj6, select_model_adj6_wt_age_pctl_sq) # p = 0.2085
lrtest(select_model_adj6, select_model_adj6_ht_age_pctl_sq) # p = 0.1044
lrtest(select_model_adj6, select_model_adj6_wt_ht_pctl_sq) # p = 0.185


###############################################################
### Logistic regression: Assumptions & diagnostics - PART 2 ###
###############################################################
### Influential values - Cook distance ###
plot(select_model_adj1, which = 4, id.n = 3)
plot(select_model_adj2, which = 4, id.n = 3)
plot(select_model_adj3, which = 4, id.n = 3)
plot(select_model_adj4, which = 4, id.n = 3)
plot(select_model_adj5, which = 4, id.n = 3)
plot(select_model_adj6, which = 4, id.n = 3)

library(DescTools)
PseudoR2(m1_unadj, which = "McFadden") # 0.003175836
PseudoR2(select_model_adj1, which = "McFadden") # 0.02356663  (17 predictors)
PseudoR2(select_model_adj2, which = "McFadden") # 0.06870438  (7 predictors)
PseudoR2(select_model_adj3, which = "McFadden") # 0.0668599   (6 predictors)
PseudoR2(select_model_adj4, which = "McFadden") # 0.0653122   (5 predictors)
PseudoR2(select_model_adj5, which = "McFadden") # 0.06604689 (7 predictors)
PseudoR2(select_model_adj6, which = "McFadden") # 0.0660691 (8 predictors)

#####################################################################
### Unify sample size by dropping n/a in the 1st model's data set ###
#####################################################################
library(tidyr)
missing_drop <- tan_new %>%
  drop_na(sex, wealth_fs, vegg1, fish1, month_age, wt_kg, ht_cm, meat1,
          bean1, iron_preg1, vit_a1, para_drug1, edu, resident, ht_age_pctl, wt_age_pctl, wt_ht_pctl)

### Likelihood Ratio Test: Compare Full model and Reduced model
# order matters: lrtest(reduced model, full model)
# If p value is significant, full model is better
# If p value is insignificant, reduced model is better

## A. Model 5 (7 predictors, percentile) vs. Model 4 (7 predictors, no percentile)
m4_adj_missing <- glm(anemia_level ~ sex + wealth_fs + vegg1 + fish1 +
                        month_age + ht_cm + wt_kg,
                      data = missing_drop, family = "binomial")
m5_adj_missing <- glm(anemia_level ~ sex + wealth_fs + vegg1 + fish1 +
                        month_age + ht_age_pctl + wt_age_pctl,
                      data = missing_drop, family = "binomial")
lrtest(m5_adj_missing, m4_adj_missing) # p = 0.0005333 ***
# Conclusion: Model 4

## B. Model 6 vs Model 4
m6_adj_missing <- glm(anemia_level ~ sex + wealth_fs + vegg1 + fish1 +
                        month_age + ht_age_pctl + wt_age_pctl + wt_ht_pctl,
                      data = missing_drop, family = "binomial")
lrtest(m4_adj_missing, m6_adj_missing) # p = 0.001244 **
# Conclusion: Model 6 is better than Model 4

## C. Model 5 vs Model 6
lrtest(m5_adj_missing, m6_adj_missing) # p = 0.3927
# Conclusion: Model 5 is better Model 6

# OVERALL CONCLUSION: Model 5 (7 predictors, percentile) is better than Model 4 (5 predictors)

summary(select_model_adj5)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  1.593411   0.087276  18.257  < 2e-16 ***
#   sexmale      0.254588   0.061897   4.113  3.9e-05 ***
#   wealth_fs   -0.140916   0.035010  -4.025  5.7e-05 ***
#   vegg1yes    -0.160891   0.062356  -2.580 0.009874 ** 
#   fish1yes     0.263247   0.077134   3.413 0.000643 ***
#   month_age   -0.038166   0.001973 -19.348  < 2e-16 ***
#   ht_age_pctl -0.001255   0.001651  -0.760 0.447170    
# wt_age_pctl -0.003445   0.001651  -2.086 0.036973 *


# Logit(Anemia = 1) = 0.25*sex - 0.14*wealth_fs - 0.16*vegg1 + 0.26*fish1 - 0.038*month_age
#                       - 0.0034*wt_age_pctl - 0.0013*ht_age_pctl + 1.59
# Being boy is a risk factor
# Wealthier - protective factor
# Eat veggies - protective factor
# Eat fish - risk factor
# Greater age - protective factor
# Greater weight for age (probably not under-weight) - protective factor
# Greater height for age (probably not stunting) - protective factor


