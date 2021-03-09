#########################################
### Group 2: Tanzania Children Anemia ###
### R code by: Xiaoyang Zou           ###
### Date created: 03/06/2021          ###
### Last modified: 03/08/2021         ###
#########################################

# original data set
tan_raw <- read.csv("Tanz_DHS_data2_withchildsex.csv")
library(dplyr)
# updated data set: select id variable and other variables of interest
tan_select <- tan_raw %>%
  dplyr::select(id, child, b4_,
                v025, v106, v190, v191, v218,
                v414h, v414j, v414k, v414m, v414n, v414o, v414p,
                m45_, h1a_, h2_, h34_, h42_, h43_,
                hw1_, hw2_, hw3_, hw4_, hw7_, hw10_, hw13_, hw53_, hw55_, hw57_, hw73_)

# the following are the variables after second filtering

# tan_select <- tan_raw %>%
#   dplyr::select(id, child, b4_,
#                 v025, v106, v190, v191,
#                 v414h, v414j, v414n, v414o,
#                 m45_, h34_, h42_, h43_,
#                 hw1_, hw2_, hw3_, hw4_, hw7_, hw10_, hw57_)

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
tan_new$resid <- tan_new$v414m
summary(tan_new$resid) # character type
table(tan_new$resid)
#           don't know         no        yes 
#   2909             2       5016         87
tan_new <- tan_new %>%
  mutate(resid1 = replace(resid, resid == 'don\'t know', NA))
tan_new <- tan_new %>%
  mutate(resid1 = replace(resid1, resid1 == '', NA))
table(tan_new$resid1)
sum(is.na(tan_new$resid1)) # no. of missing = 2911


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

########################################
### v218 "number of living children" ###
########################################

### DELETE!
tan_new$livechild <- tan_new$v218
summary(tan_new$livechild)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   2.000   3.000   3.826   5.000  14.000
sum(is.na(tan_new$livechild)) # no missing value


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


###################################################################
### v414k "gave child mangoes, papayas, other vitamin a fruits" ###
###################################################################

### DELETE!
tan_new$fruit <- tan_new$v414k
summary(tan_new$fruit) # character type
table(tan_new$fruit)
#            don't know         no        yes 
#    2909             2       3991        1112
tan_new <- tan_new %>%
  mutate(fruit1 = replace(fruit, fruit == 'don\'t know', NA))
tan_new <- tan_new %>%
  mutate(fruit1 = replace(fruit1, fruit1 == '', NA))
table(tan_new$fruit1)
#   no  yes 
# 3991 1112
sum(is.na(tan_new$fruit1)) # no. of missing = 2911


##############################################################
### variable v414m "Gave child liver, heart, other organs" ###
##############################################################

### DELETE!
tan_new$organ <- tan_new$v414m
summary(tan_new$organ)
table(tan_new$organ)
#         don't know         no        yes 
# 2909             2       5016         87
tan_new <- tan_new %>%
  mutate(organ1 = replace(organ, organ == 'don\'t know', NA))
tan_new <- tan_new %>%
  mutate(organ1 = replace(organ1, organ1 == '', NA))
table(tan_new$organ1)
sum(is.na(tan_new$organ1)) # no. of missing = 2911


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


#######################################################################
### variable v414p "gave child cheese, yogurt, other milk products" ###
#######################################################################

### DELETE!
tan_new$dairy <- tan_new$v414p
summary(tan_new$dairy) #character type
table(tan_new$dairy)
#         don't know         no        yes 
# 2909             2       4937        166
tan_new <- tan_new %>%
  mutate(dairy1 = replace(dairy, dairy == 'don\'t know', NA))
tan_new <- tan_new %>%
  mutate(dairy1 = replace(dairy1, dairy1 == '', NA))
table(tan_new$dairy1)
#   no  yes 
# 4937  166
sum(is.na(tan_new$dairy1)) # no. of missing = 2911


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

#######################################################################
### variable h1a_ "Has health card and or other vaccination record" ###
#######################################################################

### DELETE!
tan_new$record <- tan_new$h1a_
summary(tan_new$record) #character type
table(tan_new$record)
# 3170  does not have either card or other document 
# 706   has card/other document and both were seen 
# 53    has card/other document and none were seen 
# 3     has card/other document but only card was seen 
# 1     has card/other document but only other document was seen 
# 3     has only health card and was seen 
# 3810  has only health card and wasn't see 
# 109   has only other document and was see 
# 155   has only other document and wasn't seen 
# 4


###################################
### variable h2_ "Received BCG" ###
###################################

### DELETE!
tan_new$bcg <- tan_new$h2_
summary(tan_new$bcg) #character type
table(tan_new$bcg)
#         no    reported by mother     vaccination date on card
#  3171   210                  733                         3852
# vaccination marked on card
#                         48
tan_new <- tan_new %>%
  mutate(bcg1 = replace(bcg, bcg == '', NA))
table(tan_new$bcg1)
#   no  yes 
# 4937  166
sum(is.na(tan_new$bcg1)) # no. of missing = 3171


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
tan_new$wt_kg <- tan_new$wt_kg*0.1 # modification for 1 decimal
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


##############################################################
### variable hw13_ "result of measurement - height/weight" ###
##############################################################

### DELETE!
tan_new$ht_wt <- tan_new$hw13_
summary(tan_new$ht_wt) # character type!!!
table(tan_new$ht_wt)
# measured    other  refused 
#     8001       12        1
tan_new <- tan_new %>%
  mutate(ht_wt1 = replace(ht_wt, ht_wt == 'other', NA))
tan_new <- tan_new %>%
  mutate(ht_wt1 = replace(ht_wt1, ht_wt1 == 'refused', NA))
sum(is.na(tan_new$ht_wt1)) # no. of missing = 13


############################################################
### variable hw53_ "hemoglobin level (g/dl - 1 decimal)" ###
############################################################

# DELETE!
tan_new$hb_level <- tan_new$hw53_
summary(tan_new$hb_level) # character type!!!
table(tan_new$hb_level)
sum(is.na(tan_new$hb_level))
tan_new$hb_level <- as.numeric(tan_new$hb_level)
summary(tan_new$hb_level)
# Min.  1st Qu.  Median    Mean  3rd Qu.    Max. 
# 25.0    98.0   108.0   107.5   118.0     177.0
sum(is.na(tan_new$hb_level)) # no missing value


###########################################################
### variable hw55_ "result of measurement - hemoglobin" ###
###########################################################

### DELETE!
tan_new$hb_result <- tan_new$hw55_
summary(tan_new$hb_result) # character type!!!
table(tan_new$hb_result)
# measured 
#     8014 
sum(is.na(tan_new$hb_result)) # no missing value


#####################################
### variable hw57_ "anemia level" ###
#####################################
tan_new$anemia_level <- tan_new$hw57_
summary(tan_new$anemia_level) # character type!!!
table(tan_new$anemia_level)
#       mild   moderate not anemic     severe 
#       2183       2378       3330        123
sum(is.na(tan_new$anemia_level)) # no missing value


#####################################################
### variable hw73_ "BMI standard deviation (WHO)" ###
#####################################################

# DELETE!
tan_new$bmi_dev <- tan_new$hw73_
summary(tan_new$bmi_dev) # character type
table(tan_new$bmi_dev) # 'flagged cases'???
tan_new$bmi_dev <- as.numeric(tan_new$bmi_dev)
summary(tan_new$bmi_dev)
#      Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#  -498.000  -63.000    9.000    7.681   80.000  490.000       79
# can deviation be negative? Do these values have significance? Which are outliers?