# original data set
tan_raw <- read.csv("Tanzania_DHS_data.csv")
library(dplyr)
# updated data set 
tan_new <- tan_raw %>%
  dplyr::select(v025, v106, v190, v191, v218,
                v414h, v414j, v414k, v414m, v414n, v414o, v414p,
                m45_, h1a_, h2_, h34_, h42_, h43_,
                hw1_, hw2_, hw3_, hw4_, hw7_, hw10_, hw13_, hw53_, hw55_, hw57_, hw73_)


# variable v414m "Gave child liver, heart, other organs"

tan_new_organ <- (tan_new$v414m)
summary(tan_new_organ)
table(tan_new_organ)
#           don't know      no        yes 
# 55386     30              23802     378 

# variable v414o "Gave child food made from beans, peas, lentils, nuts"
tan_dou <- as.factor(tan$v414o)
summary(tan_dou)
#            don't know        no          yes 
# 55386      30                17784       6396

# variable v447a "Women's age in years (from household questionnaire)"
tan_womenage <- tan$v447a
summary(tan_womenage)
sum(is.na(tan_womenage))
# no missing values

# variable v455 "Result of measurement - hemoglobin"
tan_hb <- tan$v455
unique(tan_hb)
# "measured"    "refused"     "not present" "other"
sum(is.na(tan_hb))
# no missing values
tan_hb <- as.factor(tan_hb)
summary(tan_hb)
#   measured       not present       other      refused 
#   78612          60                84         840

# variable v457 "Anemia level"
tan_anemia <- tan$v457
unique(tan_anemia)
# "not anemic" "mild"       "moderate"   "severe"     ""
sum(is.na(tan_anemia))
# no missing values?
tan_anemia <- as.factor(tan_anemia)
summary(tan_anemia)
#          mild     moderate   not anemic     severe 
# 984      26556    9162       42060          834
sum(is.na(tan_anemia))

# variable v453 "Hemoglobin level (g/dl - 1 decimal)"
tan_hb_level = tan$v453
unique(tan_hb_level)
tan_hb_level = as.numeric(tan_hb_level)
summary(tan_hb_level)
# Warning message: NAs introduced by coercion

# variable v481 "Covered by health insurance"
tan_insurance <- tan$v481
unique(tan_insurance)
tan_insurance <- as.factor(tan_insurance)
summary(tan_insurance)
#    no     yes 
#    73128  6468

# variable m45_	"During pregnancy, given or bought iron tablets/syrup"
# (binary variable—yes/no)
# or iron level during pregnancy of the mother?
tan_m_fe <- tan$m45_
unique(tan_m_fe)
tan_m_fe <- as.factor(tan_m_fe)
summary(tan_m_fe)
#             don't know     no        yes 
# 72546       5              1370      5675

# variable v414l "Gave child any other fruits"
tan_fruit <- tan$v414l
unique(tan_fruit)
tan_fruit <- as.factor(tan_fruit)
summary(tan_fruit)
#            don't know      no          yes 
#55386       36              20364       3810