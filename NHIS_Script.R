#Ioannides AE, Wadley AL, Kamerman PR
#Arthritis in the USA: a longitudinal analysis of three nationally representative studies

# -- National Health Interview Survey (NHIS) analysis --
#Years: 2002, 2003, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018

#Notes:
#The term "Arthritis", in the context of the NHIS, is used to describe self-reported joint pain, aching, or stiffness in the preceding 30 days 

#Load packages
library(haven)
library(tidyverse)
library(survey)
library(gdata)
library(ggplot2)
library(SAScii)
library(foreign)


options(survey.lonely.psu = "adjust")

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

#----2002----
#National Health Interview Survey
#2002

#----Download----

NHIS02.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2002/samadult.sas"
NHIS02.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2002/samadult.zip"
#store as df
NHIS02_SA <- read.SAScii(NHIS02.fileloc, NHIS02.instructions, zipped = TRUE)
NHIS02_SA


#----Cleaning----
str(NHIS02_SA)
tail(NHIS02_SA)
glimpse(NHIS02_SA)
colnames(NHIS02_SA)

#Select variables
NHIS02 <- select(NHIS02_SA,
                 HHX, FMX, PX,
                 STRATUM, PSU, SRVY_YR, WTFA_SA, JNTSYMP,
                 SEX, AGE_P, BMI)


str(NHIS02)
tail(NHIS02)
glimpse(NHIS02)
colnames(NHIS02)

#joint symptoms recode
table(NHIS02$JNTSYMP)
#recode
NHIS02$JNTSYMP <- recode(NHIS02$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS02$JNTSYMP <- unknownToNA(NHIS02$JNTSYMP, unknown = c("7", "9")))
table(NHIS02$JNTSYMP)
#    0     1 
#21653  9320 
NHIS02$JNTSYMP <- as.factor(NHIS02$JNTSYMP)

#age
table(NHIS02$AGE_P)
NHIS02$AGE_P <- recode(NHIS02$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS02$AGE_P)
#18 to 24     25 to 29     30 to 34     35 to 39     40 to 44     45 to 49     50 to 54     55 to 59     60 to 64     65 to 69 70 and above 
#     3360         2670         3202         3235         3255         2946         2539         2228         1749         1539         4321 
NHIS02$AGE_P <- as.factor(NHIS02$AGE_P)

#sex
NHIS02$SEX <- recode(NHIS02$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS02$SEX)
#Female   Male 
# 17539  13505 
NHIS02$SEX <- as.factor(NHIS02$SEX)


#BMI
NHIS02$BMI
NHIS02$BMI <- unknownToNA(NHIS02$BMI, unknown = c("99.95", "99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS02 %>% top_n(10, BMI)

#Create BMI categories
NHIS02$BMICAT <- ifelse(NHIS02$BMI < 18.50, "Underweight",
                        ifelse(NHIS02$BMI >= 18.50 & NHIS02$BMI < 25.00, "Healthy weight",
                               ifelse(NHIS02$BMI >= 25.00 & NHIS02$BMI < 30.00, "Overweight",
                                      ifelse(NHIS02$BMI >=30.00, "Obese",
                                             NA))))

table(NHIS02$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#         11513           7071          10249            584 
#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#make sure all complex survey requirements are available for design object
NHIS02_SA_dataset <- subset(NHIS02,
                            !is.na(WTFA_SA) &
                              !is.na(STRATUM) &
                              !is.na(PSU))

#Check that the sum of the weights is equal to the US population
sum(NHIS02_SA_dataset$WTFA_SA)
#The sum of the weights is 205 825 095, which is acceptable

#Check the number of people (unique PSU's)
length(unique(NHIS02_SA_dataset[["PSU"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS02_SA_dataset[["STRATUM"]]))
#The number of unique strata is 339

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS02_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRATUM,
                       nest = TRUE,
                       data = NHIS02_SA_dataset)

#_______________________________________________________________________________________________


#----Analysis----

N02_overall <- svymean(~factor(JNTSYMP),
                       NHIS02_DO,
                       na.rm = TRUE)
N02_overall.c <- N02_overall %>%
  as.data.frame(.) %>%
  select(1) %>%
  setNames(c("Proportion")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_overall_ci <- confint(N02_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
N02 <- bind_cols(N02_overall.c, N02_overall_ci)
#remove JNTSYMP = 0
N02 <- N02[-c(1), ] #N02 = final proportion and 95% ci

#Overall number of people
N02.no <- svytotal(~JNTSYMP, 
                   NHIS02_DO, 
                   na.rm = TRUE, 
                   deff = FALSE)
N02.no.df <- as.data.frame(N02.no) %>%
  select(1)
#ci
N02.no.ci <- confint(N02.no) %>%
  as.data.frame(.)
#join number and ci
N02.no <- bind_cols(N02.no.df, N02.no.ci)
#remove JNTSYMP=0
N02.no <- N02.no[-c(1), ] #N02.no = final number of people with arth and 95%ci


#2. Demographic-specific analysis

#A. Arthritis & Age
N02_Arth_age <- svyby(formula = ~JNTSYMP,
                      by = ~AGE_P,
                      design = NHIS02_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N02_Arthtitis_age <- N02_Arth_age %>%
  select(1, 3) %>%
  setNames(c("Age", "Proportion")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_arth_age_ci <- confint(N02_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N02_arth_age_ci <- N02_arth_age_ci[-c(1:11), ]
#join ci and proportions
N02.Age <- bind_cols(N02_Arthtitis_age, N02_arth_age_ci) #N02.Age = final proportion and 95% ci by age group

#Number of people by age
N02.no.age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS02_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N02.no.age.c <- N02.no.age %>%
  select(1, 3) %>%
  setNames(c("Age", "Number of People")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N02.no.age.ci <- confint(N02.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 11 rows
N02.no.age.ci <- N02.no.age.ci[-c(1:11), ]
#join number and ci
N02.no.age <- bind_cols(N02.no.age.c, N02.no.age.ci)


#Age logistic regression
N02_age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS02_DO)
summary(N02_age_glm)
exp(cbind(OR=coef(N02_age_glm), confint(N02_age_glm)))



#B. Arthritis & sex
N02_Arth_sex <- svyby(formula = ~JNTSYMP,
                      by = ~SEX,
                      design = NHIS02_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N02_Arthritis_sex <- N02_Arth_sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Proportion")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_arth_sex_ci <- confint(N02_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N02_arth_sex_ci <- N02_arth_sex_ci[-c(1:2), ]
#join ci and proportions
N02.Sex <- bind_cols(N02_Arthritis_sex, N02_arth_sex_ci) #N02.Sex = final proportion and 95% ci by sex


#Number of people by sex
N02.no.sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS02_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N02.no.sex.c <- N02.no.sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Number of People")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N02.no.sex.ci <- confint(N02.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 2 rows
N02.no.sex.ci <- N02.no.sex.ci[-c(1:2), ]
#join number and ci
N02.no.sex <- bind_cols(N02.no.sex.c, N02.no.sex.ci)


#Sex logistic regression
N02_sex_glm <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                      family = quasibinomial,
                      design = NHIS02_DO)
summary(N02_sex_glm)
exp(cbind(OR=coef(N02_sex_glm), confint(N02_sex_glm)))


#C. Arthritis & BMI
N02_Arth_BMI <- svyby(formula = ~JNTSYMP,
                      by = ~BMICAT,
                      design = NHIS02_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N02_Arthritis_BMI <- N02_Arth_BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Proportion")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N02_arth_BMI_ci <- confint(N02_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0
N02_arth_BMI_ci <- N02_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
N02.BMI <- bind_cols(N02_Arthritis_BMI, N02_arth_BMI_ci) #N02.BMI = final proportion and 95%ci by BMI


#Number of people by BMI
N02.no.BMI <- svyby(formula = ~JNTSYMP,
                    by = ~BMICAT,
                    design = NHIS02_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
N02.no.BMI.c <- N02.no.BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Number of People")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N02.no.BMI.ci <- confint(N02.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 4 rows
N02.no.BMI.ci <- N02.no.BMI.ci[-c(1:4), ]
#join number and ci
N02.no.BMI <- bind_cols(N02.no.BMI.c, N02.no.BMI.ci)


#BMI logistic regression (using original continuous variable from survey)
N02.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS02_DO)
summary(N02.BMI.glm)
exp(cbind(OR=coef(N02.BMI.glm), confint(N02.BMI.glm)))


#   End of 2002 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(NHIS02)
remove(NHIS02_SA_dataset)
remove(NHIS02_DO)
remove(NHIS02_SA)
remove(N02_Arth_age)
remove(N02_arth_age_ci)
remove(N02_Arth_BMI)
remove(N02_arth_BMI_ci)
remove(N02_Arth_sex)
remove(N02_arth_sex_ci)
remove(N02_Arthritis_BMI)
remove(N02_Arthritis_sex)
remove(N02_Arthtitis_age)
remove(N02_arth_age_ci)
remove(N02_overall.c)
remove(N02_overall_ci)
remove(N02.no.age.c)
remove(N02.no.age.ci)
remove(N02.no.BMI.c)
remove(N02.no.BMI.ci)
remove(N02.no.df)
remove(N02.no.ci)
remove(N02.no.sex.c)
remove(N02.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________





#----2003----
#National Health Interview Survey
#2003

#----Download----

NHIS03.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2003/samadult.sas"
NHIS03.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2003/samadult.zip"
#store as df
NHIS03_SA <- read.SAScii(NHIS03.fileloc, NHIS03.instructions, zipped = TRUE)
NHIS03_SA


#----Cleaning----
str(NHIS03_SA)
tail(NHIS03_SA)
glimpse(NHIS03_SA)
colnames(NHIS03_SA)

#Select variables
NHIS03 <- select(NHIS03_SA,
                 HHX, FMX, PX,
                 STRATUM, PSU, SRVY_YR, WTFA_SA, JNTSYMP,
                 SEX, AGE_P, BMI)


str(NHIS03)
tail(NHIS03)
glimpse(NHIS03)
colnames(NHIS03)

#joint symptoms recode
table(NHIS03$JNTSYMP)
#recode
NHIS03$JNTSYMP <- recode(NHIS03$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS03$JNTSYMP <- unknownToNA(NHIS03$JNTSYMP, unknown = c("7", "9")))
table(NHIS03$JNTSYMP)
#    0     1 
#20802  9985 
NHIS03$JNTSYMP <- as.factor(NHIS03$JNTSYMP)

#age
table(NHIS03$AGE_P)
NHIS03$AGE_P <- recode(NHIS03$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS03$AGE_P)
#18 to 24     25 to 29     30 to 34     35 to 39     40 to 44     45 to 49     50 to 54     55 to 59     60 to 64     65 to 69 70 and above 
#    3261         2691         3128         3074         3209         2947         2679         2277         1827         1540         4219 
NHIS03$AGE_P <- as.factor(NHIS03$AGE_P)

#sex
NHIS03$SEX <- recode(NHIS03$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS03$SEX)
#Female   Male 
# 17425  13427 
NHIS03$SEX <- as.factor(NHIS03$SEX)


#BMI
NHIS03$BMI
NHIS03$BMI <- unknownToNA(NHIS03$BMI, unknown = c("99.95", "99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS03 %>% top_n(10, BMI)

#Create BMI categories
NHIS03$BMICAT <- ifelse(NHIS03$BMI < 18.50, "Underweight",
                        ifelse(NHIS03$BMI >= 18.50 & NHIS03$BMI < 25.00, "Healthy weight",
                               ifelse(NHIS03$BMI >= 25.00 & NHIS03$BMI < 30.00, "Overweight",
                                      ifelse(NHIS03$BMI >=30.00, "Obese",
                                             NA))))

table(NHIS03$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#         11351           7010          10418            583 
#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#make sure all complex survey requirements are available for design object
NHIS03_SA_dataset <- subset(NHIS03,
                            !is.na(WTFA_SA) &
                              !is.na(STRATUM) &
                              !is.na(PSU))

#Check that the sum of the weights is equal to the US population
sum(NHIS03_SA_dataset$WTFA_SA)
#The sum of the weights is 213 042 220, which is acceptable

#Check the number of people (unique PSU's)
length(unique(NHIS03_SA_dataset[["PSU"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS03_SA_dataset[["STRATUM"]]))
#The number of unique strata is 339

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS03_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRATUM,
                       nest = TRUE,
                       data = NHIS03_SA_dataset)


#_______________________________________________________________________________________________


#----Analysis----

N03_overall <- svymean(~factor(JNTSYMP),
                       NHIS03_DO,
                       na.rm = TRUE)
N03_overall.c <- N03_overall %>%
  as.data.frame(.) %>%
  select(1) %>%
  setNames(c("Proportion")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_overall_ci <- confint(N03_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
N03 <- bind_cols(N03_overall.c, N03_overall_ci)
#remove JNTSYMP = 0
N03 <- N03[-c(1), ] #N03 = final proportion and 95% ci

#Overall number of people
N03.no <- svytotal(~JNTSYMP, 
                   NHIS03_DO, 
                   na.rm = TRUE, 
                   deff = FALSE)
N03.no.df <- as.data.frame(N03.no) %>%
  select(1)
#ci
N03.no.ci <- confint(N03.no) %>%
  as.data.frame(.)
#join number and ci
N03.no <- bind_cols(N03.no.df, N03.no.ci)
#remove JNTSYMP=0
N03.no <- N03.no[-c(1), ] #N03.no = final number of people with arth and 95%ci


#2. Demographic-specific analysis

#A. Arthritis & Age
N03_Arth_age <- svyby(formula = ~JNTSYMP,
                      by = ~AGE_P,
                      design = NHIS03_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N03_Arthtitis_age <- N03_Arth_age %>%
  select(1, 3) %>%
  setNames(c("Age", "Proportion")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_arth_age_ci <- confint(N03_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N03_arth_age_ci <- N03_arth_age_ci[-c(1:11), ]
#join ci and proportions
N03.Age <- bind_cols(N03_Arthtitis_age, N03_arth_age_ci) #N03.Age = final proportion and 95% ci by age group


#Number of people by age
N03.no.age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS03_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N03.no.age.c <- N03.no.age %>%
  select(1, 3) %>%
  setNames(c("Age", "Number of People")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N03.no.age.ci <- confint(N03.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 11 rows
N03.no.age.ci <- N03.no.age.ci[-c(1:11), ]
#join number and ci
N03.no.age <- bind_cols(N03.no.age.c, N03.no.age.ci)


#Age logistic regression
N03_age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS03_DO)
summary(N03_age_glm)
exp(cbind(OR=coef(N03_age_glm), confint(N03_age_glm)))



#B. Arthritis & sex
N03_Arth_sex <- svyby(formula = ~JNTSYMP,
                      by = ~SEX,
                      design = NHIS03_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N03_Arthritis_sex <- N03_Arth_sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Proportion")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_arth_sex_ci <- confint(N03_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N03_arth_sex_ci <- N03_arth_sex_ci[-c(1:2), ]
#join ci and proportions
N03.Sex <- bind_cols(N03_Arthritis_sex, N03_arth_sex_ci) #N03.Sex = final proportion and 95% ci by sex


#Number of people by sex
N03.no.sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS03_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N03.no.sex.c <- N03.no.sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Number of People")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N03.no.sex.ci <- confint(N03.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 2 rows
N03.no.sex.ci <- N03.no.sex.ci[-c(1:2), ]
#join number and ci
N03.no.sex <- bind_cols(N03.no.sex.c, N03.no.sex.ci)


#Sex logistic regression
N03_sex_glm <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                      family = quasibinomial,
                      design = NHIS03_DO)
summary(N03_sex_glm)
exp(cbind(OR=coef(N03_sex_glm), confint(N03_sex_glm)))


#C. Arthritis & BMI
N03_Arth_BMI <- svyby(formula = ~JNTSYMP,
                      by = ~BMICAT,
                      design = NHIS03_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N03_Arthritis_BMI <- N03_Arth_BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Proportion")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N03_arth_BMI_ci <- confint(N03_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0
N03_arth_BMI_ci <- N03_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
N03.BMI <- bind_cols(N03_Arthritis_BMI, N03_arth_BMI_ci) #N03.BMI = final proportion and 95%ci by BMI

#Number of people by BMI
N03.no.BMI <- svyby(formula = ~JNTSYMP,
                    by = ~BMICAT,
                    design = NHIS03_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
N03.no.BMI.c <- N03.no.BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Number of People")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N03.no.BMI.ci <- confint(N03.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 4 rows
N03.no.BMI.ci <- N03.no.BMI.ci[-c(1:4), ]
#join number and ci
N03.no.BMI <- bind_cols(N03.no.BMI.c, N03.no.BMI.ci)


#BMI logistic regression (using original continuous variable from survey)
N03.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS03_DO)
summary(N03.BMI.glm)
exp(cbind(OR=coef(N03.BMI.glm), confint(N03.BMI.glm)))


#   End of 2003 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(NHIS03)
remove(NHIS03_SA_dataset)
remove(NHIS03_DO)
remove(NHIS03_SA)
remove(N03_Arth_age)
remove(N03_arth_age_ci)
remove(N03_Arth_BMI)
remove(N03_arth_BMI_ci)
remove(N03_Arth_sex)
remove(N03_arth_sex_ci)
remove(N03_Arthritis_BMI)
remove(N03_Arthritis_sex)
remove(N03_Arthtitis_age)
remove(N03_arth_age_ci)
remove(N03_overall.c)
remove(N03_overall_ci)
remove(N03.no.age.c)
remove(N03.no.age.ci)
remove(N03.no.BMI.c)
remove(N03.no.BMI.ci)
remove(N03.no.df)
remove(N03.no.ci)
remove(N03.no.sex.c)
remove(N03.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________


#----2005----
#National Health Interview Survey
#2005

#----Download----

NHIS05.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2005/samadult.sas"
NHIS05.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2005/samadult.zip"
#store as df
NHIS05_SA <- read.SAScii(NHIS05.fileloc, NHIS05.instructions, zipped = TRUE)
NHIS05_SA


#----Cleaning----
str(NHIS05_SA)
tail(NHIS05_SA)
glimpse(NHIS05_SA)
colnames(NHIS05_SA)

#Select variables
NHIS05 <- select(NHIS05_SA,
                 HHX, FMX, FPX,
                 STRATUM, PSU, SRVY_YR, WTFA_SA, JNTSYMP,
                 SEX, AGE_P, BMI)


str(NHIS05)
tail(NHIS05)
glimpse(NHIS05)
colnames(NHIS05)

#joint symptoms recode
table(NHIS05$JNTSYMP)
#recode
NHIS05$JNTSYMP <- recode(NHIS05$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS05$JNTSYMP <- unknownToNA(NHIS05$JNTSYMP, unknown = c("7", "9")))
table(NHIS05$JNTSYMP)
#    0     1 
#21369 10015 
NHIS05$JNTSYMP <- as.factor(NHIS05$JNTSYMP)

#age
table(NHIS05$AGE_P)
NHIS05$AGE_P <- recode(NHIS05$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS05$AGE_P)
#18 to 24     25 to 29     30 to 34     35 to 39     40 to 44     45 to 49     50 to 54     55 to 59     60 to 64     65 to 69 70 and above 
#    3104         2723         2969         3014         3109         3029         2827         2537         2038         1632         4446 
NHIS05$AGE_P <- as.factor(NHIS05$AGE_P)

#sex
NHIS05$SEX <- recode(NHIS05$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS05$SEX)
#Female   Male 
# 17666  13762 
NHIS05$SEX <- as.factor(NHIS05$SEX)


#BMI
NHIS05$BMI
NHIS05$BMI <- unknownToNA(NHIS05$BMI, unknown = c("99.95", "99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS05 %>% top_n(10, BMI)

#Create BMI categories
NHIS05$BMICAT <- ifelse(NHIS05$BMI < 18.50, "Underweight",
                        ifelse(NHIS05$BMI >= 18.50 & NHIS05$BMI < 25.00, "Healthy weight",
                               ifelse(NHIS05$BMI >= 25.00 & NHIS05$BMI < 30.00, "Overweight",
                                      ifelse(NHIS05$BMI >=30.00, "Obese",
                                             NA))))

table(NHIS05$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#         11202           7654          10588            554 
#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#make sure all complex survey requirements are available for design object
NHIS05_SA_dataset <- subset(NHIS05,
                            !is.na(WTFA_SA) &
                              !is.na(STRATUM) &
                              !is.na(PSU))

#Check that the sum of the weights is equal to the US population
sum(NHIS05_SA_dataset$WTFA_SA)
#The sum of the weights is 217 773 755, which is acceptable

#Check the number of people (unique PSU's)
length(unique(NHIS05_SA_dataset[["PSU"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS05_SA_dataset[["STRATUM"]]))
#The number of unique strata is 339

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS05_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRATUM,
                       nest = TRUE,
                       data = NHIS05_SA_dataset)

#_______________________________________________________________________________________________


#----Analysis----

N05_overall <- svymean(~factor(JNTSYMP),
                       NHIS05_DO,
                       na.rm = TRUE)
N05_overall.c <- N05_overall %>%
  as.data.frame(.) %>%
  select(1) %>%
  setNames(c("Proportion")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_overall_ci <- confint(N05_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
N05 <- bind_cols(N05_overall.c, N05_overall_ci)
#remove JNTSYMP = 0
N05 <- N05[-c(1), ] #N05 = final proportion and 95% ci

#Overall number of people
N05.no <- svytotal(~JNTSYMP, 
                   NHIS05_DO, 
                   na.rm = TRUE, 
                   deff = FALSE)
N05.no.df <- as.data.frame(N05.no) %>%
  select(1)
#ci
N05.no.ci <- confint(N05.no) %>%
  as.data.frame(.)
#join number and ci
N05.no <- bind_cols(N05.no.df, N05.no.ci)
#remove JNTSYMP=0
N05.no <- N05.no[-c(1), ] #N05.no = final number of people with arth and 95%ci


#2. Demographic-specific analysis

#A. Arthritis & Age
N05_Arth_age <- svyby(formula = ~JNTSYMP,
                      by = ~AGE_P,
                      design = NHIS05_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N05_Arthtitis_age <- N05_Arth_age %>%
  select(1, 3) %>%
  setNames(c("Age", "Proportion")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_arth_age_ci <- confint(N05_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N05_arth_age_ci <- N05_arth_age_ci[-c(1:11), ]
#join ci and proportions
N05.Age <- bind_cols(N05_Arthtitis_age, N05_arth_age_ci) #N05.Age = final proportion and 95% ci by age group

#Number of people by age
N05.no.age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS05_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N05.no.age.c <- N05.no.age %>%
  select(1, 3) %>%
  setNames(c("Age", "Number of People")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N05.no.age.ci <- confint(N05.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 11 rows
N05.no.age.ci <- N05.no.age.ci[-c(1:11), ]
#join number and ci
N05.no.age <- bind_cols(N05.no.age.c, N05.no.age.ci)


#Age logistic regression
N05_age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS05_DO)
summary(N05_age_glm)
exp(cbind(OR=coef(N05_age_glm), confint(N05_age_glm)))



#B. Arthritis & sex
N05_Arth_sex <- svyby(formula = ~JNTSYMP,
                      by = ~SEX,
                      design = NHIS05_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N05_Arthritis_sex <- N05_Arth_sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Proportion")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_arth_sex_ci <- confint(N05_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N05_arth_sex_ci <- N05_arth_sex_ci[-c(1:2), ]
#join ci and proportions
N05.Sex <- bind_cols(N05_Arthritis_sex, N05_arth_sex_ci) #N05.Sex = final proportion and 95% ci by sex


#Number of people by sex
N05.no.sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS05_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N05.no.sex.c <- N05.no.sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Number of People")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N05.no.sex.ci <- confint(N05.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 2 rows
N05.no.sex.ci <- N05.no.sex.ci[-c(1:2), ]
#join number and ci
N05.no.sex <- bind_cols(N05.no.sex.c, N05.no.sex.ci)


#Sex logistic regression
N05_sex_glm <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                      family = quasibinomial,
                      design = NHIS05_DO)
summary(N05_sex_glm)
exp(cbind(OR=coef(N05_sex_glm), confint(N05_sex_glm)))


#C. Arthritis & BMI
N05_Arth_BMI <- svyby(formula = ~JNTSYMP,
                      by = ~BMICAT,
                      design = NHIS05_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N05_Arthritis_BMI <- N05_Arth_BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Proportion")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N05_arth_BMI_ci <- confint(N05_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0
N05_arth_BMI_ci <- N05_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
N05.BMI <- bind_cols(N05_Arthritis_BMI, N05_arth_BMI_ci) #N05.BMI = final proportion and 95%ci by BMI


#Number of people by BMI
N05.no.BMI <- svyby(formula = ~JNTSYMP,
                    by = ~BMICAT,
                    design = NHIS05_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
N05.no.BMI.c <- N05.no.BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Number of People")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N05.no.BMI.ci <- confint(N05.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 4 rows
N05.no.BMI.ci <- N05.no.BMI.ci[-c(1:4), ]
#join number and ci
N05.no.BMI <- bind_cols(N05.no.BMI.c, N05.no.BMI.ci)


#BMI logistic regression (using original continuous variable from survey)
N05.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS05_DO)
summary(N05.BMI.glm)
exp(cbind(OR=coef(N05.BMI.glm), confint(N05.BMI.glm)))


#   End of 2005 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(NHIS05)
remove(NHIS05_SA_dataset)
remove(NHIS05_DO)
remove(NHIS05_SA)
remove(N05_Arth_age)
remove(N05_arth_age_ci)
remove(N05_Arth_BMI)
remove(N05_arth_BMI_ci)
remove(N05_Arth_sex)
remove(N05_arth_sex_ci)
remove(N05_Arthritis_BMI)
remove(N05_Arthritis_sex)
remove(N05_Arthtitis_age)
remove(N05_arth_age_ci)
remove(N05_overall.c)
remove(N05_overall_ci)
remove(N05.no.age.c)
remove(N05.no.age.ci)
remove(N05.no.BMI.c)
remove(N05.no.BMI.ci)
remove(N05.no.df)
remove(N05.no.ci)
remove(N05.no.sex.c)
remove(N05.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________


#----2006----
#National Health Interview Survey
#2006

#----Download----

NHIS06.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2006/samadult.sas"
NHIS06.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2006/samadult.zip"
#store as df
NHIS06_SA <- read.SAScii(NHIS06.fileloc, NHIS06.instructions, zipped = TRUE)
NHIS06_SA


#----Cleaning----
str(NHIS06_SA)
tail(NHIS06_SA)
glimpse(NHIS06_SA)
colnames(NHIS06_SA)

#Select variables
NHIS06 <- select(NHIS06_SA,
                 HHX, FMX, FPX,
                 STRAT_P, PSU_P, SRVY_YR, WTFA_SA, JNTSYMP,
                 SEX, AGE_P, BMI)

str(NHIS06)
tail(NHIS06)
glimpse(NHIS06)
colnames(NHIS06)

#joint symptoms recode
table(NHIS06$JNTSYMP)
#recode
NHIS06$JNTSYMP <- recode(NHIS06$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS06$JNTSYMP <- unknownToNA(NHIS06$JNTSYMP, unknown = c("7", "9")))
table(NHIS06$JNTSYMP)
#    0     1 
#16837  7388 
NHIS06$JNTSYMP <- as.factor(NHIS06$JNTSYMP)

#age
table(NHIS06$AGE_P)
NHIS06$AGE_P <- recode(NHIS06$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS06$AGE_P)
#18 to 24     25 to 29     30 to 34     35 to 39     40 to 44     45 to 49     50 to 54     55 to 59     60 to 64     65 to 69 70 and above 
#    2670         2209         2220         2319         2332         2326         2124         1941         1487         1346         3301 
NHIS06$AGE_P <- as.factor(NHIS06$AGE_P)

#sex
NHIS06$SEX <- recode(NHIS06$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS06$SEX)
#Female   Male 
# 13560  10715 
NHIS06$SEX <- as.factor(NHIS06$SEX)


#BMI
NHIS06$BMI
NHIS06$BMI <- unknownToNA(NHIS06$BMI, unknown = c("99.95", "99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS06 %>% top_n(10, BMI)

#Create BMI categories
NHIS06$BMICAT <- ifelse(NHIS06$BMI < 18.50, "Underweight",
                        ifelse(NHIS06$BMI >= 18.50 & NHIS06$BMI < 25.00, "Healthy weight",
                               ifelse(NHIS06$BMI >= 25.00 & NHIS06$BMI < 30.00, "Overweight",
                                      ifelse(NHIS06$BMI >=30.00, "Obese",
                                             NA))))

table(NHIS06$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#          8607           6005           8039            408 
#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#make sure all complex survey requirements are available for design object
NHIS06_SA_dataset <- subset(NHIS06,
                            !is.na(WTFA_SA) &
                              !is.na(STRAT_P) &
                              !is.na(PSU_P))

#Check that the sum of the weights is equal to the US population
sum(NHIS06_SA_dataset$WTFA_SA)
#The sum of the weights is 220 266 693, which is acceptable

#Check the number of people (unique PSU's)
length(unique(NHIS06_SA_dataset[["PSU_P"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS06_SA_dataset[["STRAT_P"]]))
#The number of unique strata is 300

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS06_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRAT_P,
                       nest = TRUE,
                       data = NHIS06_SA_dataset)

#_______________________________________________________________________________________________


#----Analysis----

N06_overall <- svymean(~factor(JNTSYMP),
                       NHIS06_DO,
                       na.rm = TRUE)
N06_overall.c <- N06_overall %>%
  as.data.frame(.) %>%
  select(1) %>%
  setNames(c("Proportion")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_overall_ci <- confint(N06_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
N06 <- bind_cols(N06_overall.c, N06_overall_ci)
#remove JNTSYMP = 0
N06 <- N06[-c(1), ] #N06 = final proportion and 95% ci

#Overall number of people
N06.no <- svytotal(~JNTSYMP, 
                   NHIS06_DO, 
                   na.rm = TRUE, 
                   deff = FALSE)
N06.no.df <- as.data.frame(N06.no) %>%
  select(1)
#ci
N06.no.ci <- confint(N06.no) %>%
  as.data.frame(.)
#join number and ci
N06.no <- bind_cols(N06.no.df, N06.no.ci)
#remove JNTSYMP=0
N06.no <- N06.no[-c(1), ] #N06.no = final number of people with arth and 95%ci


#2. Demographic-specific analysis

#A. Arthritis & Age
N06_Arth_age <- svyby(formula = ~JNTSYMP,
                      by = ~AGE_P,
                      design = NHIS06_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N06_Arthtitis_age <- N06_Arth_age %>%
  select(1, 3) %>%
  setNames(c("Age", "Proportion")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_arth_age_ci <- confint(N06_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N06_arth_age_ci <- N06_arth_age_ci[-c(1:11), ]
#join ci and proportions
N06.Age <- bind_cols(N06_Arthtitis_age, N06_arth_age_ci) #N06.Age = final proportion and 95% ci by age group


#Number of people by age
N06.no.age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS06_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N06.no.age.c <- N06.no.age %>%
  select(1, 3) %>%
  setNames(c("Age", "Number of People")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N06.no.age.ci <- confint(N06.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 11 rows
N06.no.age.ci <- N06.no.age.ci[-c(1:11), ]
#join number and ci
N06.no.age <- bind_cols(N06.no.age.c, N06.no.age.ci)


#Age logistic regression
N06_age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS06_DO)
summary(N06_age_glm)
exp(cbind(OR=coef(N06_age_glm), confint(N06_age_glm)))



#B. Arthritis & sex
N06_Arth_sex <- svyby(formula = ~JNTSYMP,
                      by = ~SEX,
                      design = NHIS06_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N06_Arthritis_sex <- N06_Arth_sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Proportion")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_arth_sex_ci <- confint(N06_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N06_arth_sex_ci <- N06_arth_sex_ci[-c(1:2), ]
#join ci and proportions
N06.Sex <- bind_cols(N06_Arthritis_sex, N06_arth_sex_ci) #N06.Sex = final proportion and 95% ci by sex


#Number of people by sex
N06.no.sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS06_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N06.no.sex.c <- N06.no.sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Number of People")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N06.no.sex.ci <- confint(N06.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 2 rows
N06.no.sex.ci <- N06.no.sex.ci[-c(1:2), ]
#join number and ci
N06.no.sex <- bind_cols(N06.no.sex.c, N06.no.sex.ci)


#Sex logistic regression
N06_sex_glm <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                      family = quasibinomial,
                      design = NHIS06_DO)
summary(N06_sex_glm)
exp(cbind(OR=coef(N06_sex_glm), confint(N06_sex_glm)))


#C. Arthritis & BMI
N06_Arth_BMI <- svyby(formula = ~JNTSYMP,
                      by = ~BMICAT,
                      design = NHIS06_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N06_Arthritis_BMI <- N06_Arth_BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Proportion")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N06_arth_BMI_ci <- confint(N06_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0
N06_arth_BMI_ci <- N06_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
N06.BMI <- bind_cols(N06_Arthritis_BMI, N06_arth_BMI_ci) #N06.BMI = final proportion and 95%ci by BMI


#Number of people by BMI
N06.no.BMI <- svyby(formula = ~JNTSYMP,
                    by = ~BMICAT,
                    design = NHIS06_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
N06.no.BMI.c <- N06.no.BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Number of People")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N06.no.BMI.ci <- confint(N06.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 4 rows
N06.no.BMI.ci <- N06.no.BMI.ci[-c(1:4), ]
#join number and ci
N06.no.BMI <- bind_cols(N06.no.BMI.c, N06.no.BMI.ci)


#BMI logistic regression (using original continuous variable from survey)
N06.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS06_DO)
summary(N06.BMI.glm)
exp(cbind(OR=coef(N06.BMI.glm), confint(N06.BMI.glm)))


#   End of 2006 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(NHIS06)
remove(NHIS06_SA_dataset)
remove(NHIS06_DO)
remove(NHIS06_SA)
remove(N06_Arth_age)
remove(N06_arth_age_ci)
remove(N06_Arth_BMI)
remove(N06_arth_BMI_ci)
remove(N06_Arth_sex)
remove(N06_arth_sex_ci)
remove(N06_Arthritis_BMI)
remove(N06_Arthritis_sex)
remove(N06_Arthtitis_age)
remove(N06_arth_age_ci)
remove(N06_overall.c)
remove(N06_overall_ci)
remove(N06.no.age.c)
remove(N06.no.age.ci)
remove(N06.no.BMI.c)
remove(N06.no.BMI.ci)
remove(N06.no.df)
remove(N06.no.ci)
remove(N06.no.sex.c)
remove(N06.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________


#----2007----
#National Health Interview Survey
#2007

#----Download----

NHIS07.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2007/samadult.sas"
NHIS07.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2007/samadult.zip"
#store as df
NHIS07_SA <- read.SAScii(NHIS07.fileloc, NHIS07.instructions, zipped = TRUE)
NHIS07_SA


#----Cleaning----
str(NHIS07_SA)
tail(NHIS07_SA)
glimpse(NHIS07_SA)
colnames(NHIS07_SA)

#Select variables
NHIS07 <- select(NHIS07_SA,
                 HHX, FMX, FPX,
                 STRAT_P, PSU_P, SRVY_YR, WTFA_SA, JNTSYMP,
                 SEX, AGE_P, BMI)

str(NHIS07)
tail(NHIS07)
glimpse(NHIS07)
colnames(NHIS07)

#joint symptoms recode
table(NHIS07$JNTSYMP)
#recode
NHIS07$JNTSYMP <- recode(NHIS07$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS07$JNTSYMP <- unknownToNA(NHIS07$JNTSYMP, unknown = c("7", "9")))
table(NHIS07$JNTSYMP)
#    0     1 
#16794  6570 
NHIS07$JNTSYMP <- as.factor(NHIS07$JNTSYMP)

#age
table(NHIS07$AGE_P)
NHIS07$AGE_P <- recode(NHIS07$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS07$AGE_P)
#18 to 24     25 to 29     30 to 34     35 to 39     40 to 44     45 to 49     50 to 54     55 to 59     60 to 64     65 to 69 70 and above 
#    2494         2050         2148         2172         2171         2254         2112         1791         1618         1370         3213 
NHIS07$AGE_P <- as.factor(NHIS07$AGE_P)

#sex
NHIS07$SEX <- recode(NHIS07$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS07$SEX)
#Female   Male 
# 13018  10375 
NHIS07$SEX <- as.factor(NHIS07$SEX)


#BMI
NHIS07$BMI
NHIS07$BMI <- unknownToNA(NHIS07$BMI, unknown = c("99.95", "99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS07 %>% top_n(10, BMI)

#Create BMI categories
NHIS07$BMICAT <- ifelse(NHIS07$BMI < 18.50, "Underweight",
                        ifelse(NHIS07$BMI >= 18.50 & NHIS07$BMI < 25.00, "Healthy weight",
                               ifelse(NHIS07$BMI >= 25.00 & NHIS07$BMI < 30.00, "Overweight",
                                      ifelse(NHIS07$BMI >=30.00, "Obese",
                                             NA))))

table(NHIS07$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#          8053           5865           7802            399 
#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#make sure all complex survey requirements are available for design object
NHIS07_SA_dataset <- subset(NHIS07,
                            !is.na(WTFA_SA) &
                              !is.na(STRAT_P) &
                              !is.na(PSU_P))

#Check that the sum of the weights is equal to the US population
sum(NHIS07_SA_dataset$WTFA_SA)
#The sum of the weights is 223 180 965, which is acceptable

#Check the number of people (unique PSU's)
length(unique(NHIS07_SA_dataset[["PSU_P"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS07_SA_dataset[["STRAT_P"]]))
#The number of unique strata is 300

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS07_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRAT_P,
                       nest = TRUE,
                       data = NHIS07_SA_dataset)


#_______________________________________________________________________________________________


#----Analysis----

N07_overall <- svymean(~factor(JNTSYMP),
                       NHIS07_DO,
                       na.rm = TRUE)
N07_overall.c <- N07_overall %>%
  as.data.frame(.) %>%
  select(1) %>%
  setNames(c("Proportion")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_overall_ci <- confint(N07_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
N07 <- bind_cols(N07_overall.c, N07_overall_ci)
#remove JNTSYMP = 0
N07 <- N07[-c(1), ] #N07 = final proportion and 95% ci

#Overall number of people
N07.no <- svytotal(~JNTSYMP, 
                   NHIS07_DO, 
                   na.rm = TRUE, 
                   deff = FALSE)
N07.no.df <- as.data.frame(N07.no) %>%
  select(1)
#ci
N07.no.ci <- confint(N07.no) %>%
  as.data.frame(.)
#join number and ci
N07.no <- bind_cols(N07.no.df, N07.no.ci)
#remove JNTSYMP=0
N07.no <- N07.no[-c(1), ] #N07.no = final number of people with arth and 95%ci


#2. Demographic-specific analysis

#A. Arthritis & Age
N07_Arth_age <- svyby(formula = ~JNTSYMP,
                      by = ~AGE_P,
                      design = NHIS07_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N07_Arthtitis_age <- N07_Arth_age %>%
  select(1, 3) %>%
  setNames(c("Age", "Proportion")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_arth_age_ci <- confint(N07_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N07_arth_age_ci <- N07_arth_age_ci[-c(1:11), ]
#join ci and proportions
N07.Age <- bind_cols(N07_Arthtitis_age, N07_arth_age_ci) #N07.Age = final proportion and 95% ci by age group


#Number of people by age
N07.no.age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS07_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N07.no.age.c <- N07.no.age %>%
  select(1, 3) %>%
  setNames(c("Age", "Number of People")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N07.no.age.ci <- confint(N07.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 11 rows
N07.no.age.ci <- N07.no.age.ci[-c(1:11), ]
#join number and ci
N07.no.age <- bind_cols(N07.no.age.c, N07.no.age.ci)


#Age logistic regression
N07_age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS07_DO)
summary(N07_age_glm)
exp(cbind(OR=coef(N07_age_glm), confint(N07_age_glm)))



#B. Arthritis & sex
N07_Arth_sex <- svyby(formula = ~JNTSYMP,
                      by = ~SEX,
                      design = NHIS07_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N07_Arthritis_sex <- N07_Arth_sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Proportion")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_arth_sex_ci <- confint(N07_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N07_arth_sex_ci <- N07_arth_sex_ci[-c(1:2), ]
#join ci and proportions
N07.Sex <- bind_cols(N07_Arthritis_sex, N07_arth_sex_ci) #N07.Sex = final proportion and 95% ci by sex


#Number of people by sex
N07.no.sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS07_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N07.no.sex.c <- N07.no.sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Number of People")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N07.no.sex.ci <- confint(N07.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 2 rows
N07.no.sex.ci <- N07.no.sex.ci[-c(1:2), ]
#join number and ci
N07.no.sex <- bind_cols(N07.no.sex.c, N07.no.sex.ci)


#Sex logistic regression
N07_sex_glm <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                      family = quasibinomial,
                      design = NHIS07_DO)
summary(N07_sex_glm)
exp(cbind(OR=coef(N07_sex_glm), confint(N07_sex_glm)))


#C. Arthritis & BMI
N07_Arth_BMI <- svyby(formula = ~JNTSYMP,
                      by = ~BMICAT,
                      design = NHIS07_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N07_Arthritis_BMI <- N07_Arth_BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Proportion")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N07_arth_BMI_ci <- confint(N07_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0
N07_arth_BMI_ci <- N07_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
N07.BMI <- bind_cols(N07_Arthritis_BMI, N07_arth_BMI_ci) #N07.BMI = final proportion and 95%ci by BMI


#Number of people by BMI
N07.no.BMI <- svyby(formula = ~JNTSYMP,
                    by = ~BMICAT,
                    design = NHIS07_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
N07.no.BMI.c <- N07.no.BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Number of People")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N07.no.BMI.ci <- confint(N07.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 4 rows
N07.no.BMI.ci <- N07.no.BMI.ci[-c(1:4), ]
#join number and ci
N07.no.BMI <- bind_cols(N07.no.BMI.c, N07.no.BMI.ci)


#BMI logistic regression (using original continuous variable from survey)
N07.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS07_DO)
summary(N07.BMI.glm)
exp(cbind(OR=coef(N07.BMI.glm), confint(N07.BMI.glm)))


#   End of 2007 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(NHIS07)
remove(NHIS07_SA_dataset)
remove(NHIS07_DO)
remove(NHIS07_SA)
remove(N07_Arth_age)
remove(N07_arth_age_ci)
remove(N07_Arth_BMI)
remove(N07_arth_BMI_ci)
remove(N07_Arth_sex)
remove(N07_arth_sex_ci)
remove(N07_Arthritis_BMI)
remove(N07_Arthritis_sex)
remove(N07_Arthtitis_age)
remove(N07_arth_age_ci)
remove(N07_overall.c)
remove(N07_overall_ci)
remove(N07.no.age.c)
remove(N07.no.age.ci)
remove(N07.no.BMI.c)
remove(N07.no.BMI.ci)
remove(N07.no.df)
remove(N07.no.ci)
remove(N07.no.sex.c)
remove(N07.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________


#----2008----
#National Health Interview Survey
#2008

#----Download----
NHIS08.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2008/samadult.sas"
NHIS08.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2008/samadult.zip"
#store as df
NHIS08_SA <- read.SAScii(NHIS08.fileloc, NHIS08.instructions, zipped = TRUE)
NHIS08_SA


#----Cleaning----
str(NHIS08_SA)
tail(NHIS08_SA)
glimpse(NHIS08_SA)
colnames(NHIS08_SA)

#Select variables
NHIS08 <- select(NHIS08_SA,
                 HHX, FMX, FPX,
                 STRAT_P, PSU_P, SRVY_YR, WTFA_SA, JNTSYMP,
                 SEX, AGE_P, BMI)

str(NHIS08)
tail(NHIS08)
glimpse(NHIS08)
colnames(NHIS08)

#joint symptoms recode
table(NHIS08$JNTSYMP)
#recode
NHIS08$JNTSYMP <- recode(NHIS08$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS08$JNTSYMP <- unknownToNA(NHIS08$JNTSYMP, unknown = c("7", "9")))
table(NHIS08$JNTSYMP)
#    0     1 
#14829  6944 
NHIS08$JNTSYMP <- as.factor(NHIS08$JNTSYMP)

#age
table(NHIS08$AGE_P)
NHIS08$AGE_P <- recode(NHIS08$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS08$AGE_P)
#18 to 24     25 to 29     30 to 34     35 to 39     40 to 44     45 to 49     50 to 54     55 to 59     60 to 64     65 to 69 70 and above 
#        2130         1979         1965         1993         1954         2038         1932         1840         1506         1306         3138 
NHIS08$AGE_P <- as.factor(NHIS08$AGE_P)

#sex
NHIS08$SEX <- recode(NHIS08$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS08$SEX)
#Female   Male 
# 12267   9514 
NHIS08$SEX <- as.factor(NHIS08$SEX)


#BMI
NHIS08$BMI
NHIS08$BMI <- unknownToNA(NHIS08$BMI, unknown = c("99.95", "99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS08 %>% top_n(10, BMI)

#Create BMI categories
NHIS08$BMICAT <- ifelse(NHIS08$BMI < 18.50, "Underweight",
                        ifelse(NHIS08$BMI >= 18.50 & NHIS08$BMI < 25.00, "Healthy weight",
                               ifelse(NHIS08$BMI >= 25.00 & NHIS08$BMI < 30.00, "Overweight",
                                      ifelse(NHIS08$BMI >=30.00, "Obese",
                                             NA))))

table(NHIS08$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#          7483           5806           7211            362 
#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#make sure all complex survey requirements are available for design object
NHIS08_SA_dataset <- subset(NHIS08,
                            !is.na(WTFA_SA) &
                              !is.na(STRAT_P) &
                              !is.na(PSU_P))

#Check that the sum of the weights is equal to the US population
sum(NHIS08_SA_dataset$WTFA_SA)
#The sum of the weights is 225 227 316, which is acceptable

#Check the number of people (unique PSU's)
length(unique(NHIS08_SA_dataset[["PSU_P"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS08_SA_dataset[["STRAT_P"]]))
#The number of unique strata is 300

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS08_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRAT_P,
                       nest = TRUE,
                       data = NHIS08_SA_dataset)

#_______________________________________________________________________________________________


#----Analysis----

N08_overall <- svymean(~factor(JNTSYMP),
                       NHIS08_DO,
                       na.rm = TRUE)
N08_overall.c <- N08_overall %>%
  as.data.frame(.) %>%
  select(1) %>%
  setNames(c("Proportion")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_overall_ci <- confint(N08_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
N08 <- bind_cols(N08_overall.c, N08_overall_ci)
#remove JNTSYMP = 0
N08 <- N08[-c(1), ] #N08 = final proportion and 95% ci

#Overall number of people
N08.no <- svytotal(~JNTSYMP, 
                   NHIS08_DO, 
                   na.rm = TRUE, 
                   deff = FALSE)
N08.no.df <- as.data.frame(N08.no) %>%
  select(1)
#ci
N08.no.ci <- confint(N08.no) %>%
  as.data.frame(.)
#join number and ci
N08.no <- bind_cols(N08.no.df, N08.no.ci)
#remove JNTSYMP=0
N08.no <- N08.no[-c(1), ] #N08.no = final number of people with arth and 95%ci


#2. Demographic-specific analysis

#A. Arthritis & Age
N08_Arth_age <- svyby(formula = ~JNTSYMP,
                      by = ~AGE_P,
                      design = NHIS08_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N08_Arthtitis_age <- N08_Arth_age %>%
  select(1, 3) %>%
  setNames(c("Age", "Proportion")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_arth_age_ci <- confint(N08_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N08_arth_age_ci <- N08_arth_age_ci[-c(1:11), ]
#join ci and proportions
N08.Age <- bind_cols(N08_Arthtitis_age, N08_arth_age_ci) #N08.Age = final proportion and 95% ci by age group


#Number of people by age
N08.no.age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS08_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N08.no.age.c <- N08.no.age %>%
  select(1, 3) %>%
  setNames(c("Age", "Number of People")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N08.no.age.ci <- confint(N08.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 11 rows
N08.no.age.ci <- N08.no.age.ci[-c(1:11), ]
#join number and ci
N08.no.age <- bind_cols(N08.no.age.c, N08.no.age.ci)


#Age logistic regression
N08_age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS08_DO)
summary(N08_age_glm)
exp(cbind(OR=coef(N08_age_glm), confint(N08_age_glm)))



#B. Arthritis & sex
N08_Arth_sex <- svyby(formula = ~JNTSYMP,
                      by = ~SEX,
                      design = NHIS08_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N08_Arthritis_sex <- N08_Arth_sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Proportion")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_arth_sex_ci <- confint(N08_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N08_arth_sex_ci <- N08_arth_sex_ci[-c(1:2), ]
#join ci and proportions
N08.Sex <- bind_cols(N08_Arthritis_sex, N08_arth_sex_ci) #N08.Sex = final proportion and 95% ci by sex


#Number of people by sex
N08.no.sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS08_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N08.no.sex.c <- N08.no.sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Number of People")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N08.no.sex.ci <- confint(N08.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 2 rows
N08.no.sex.ci <- N08.no.sex.ci[-c(1:2), ]
#join number and ci
N08.no.sex <- bind_cols(N08.no.sex.c, N08.no.sex.ci)


#Sex logistic regression
N08_sex_glm <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                      family = quasibinomial,
                      design = NHIS08_DO)
summary(N08_sex_glm)
exp(cbind(OR=coef(N08_sex_glm), confint(N08_sex_glm)))


#C. Arthritis & BMI
N08_Arth_BMI <- svyby(formula = ~JNTSYMP,
                      by = ~BMICAT,
                      design = NHIS08_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N08_Arthritis_BMI <- N08_Arth_BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Proportion")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N08_arth_BMI_ci <- confint(N08_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0
N08_arth_BMI_ci <- N08_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
N08.BMI <- bind_cols(N08_Arthritis_BMI, N08_arth_BMI_ci) #N08.BMI = final proportion and 95%ci by BMI


#Number of people by BMI
N08.no.BMI <- svyby(formula = ~JNTSYMP,
                    by = ~BMICAT,
                    design = NHIS08_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
N08.no.BMI.c <- N08.no.BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Number of People")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N08.no.BMI.ci <- confint(N08.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 4 rows
N08.no.BMI.ci <- N08.no.BMI.ci[-c(1:4), ]
#join number and ci
N08.no.BMI <- bind_cols(N08.no.BMI.c, N08.no.BMI.ci)


#BMI logistic regression (using original continuous variable from survey)
N08.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS08_DO)
summary(N08.BMI.glm)
exp(cbind(OR=coef(N08.BMI.glm), confint(N08.BMI.glm)))


#   End of 2008 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(NHIS08)
remove(NHIS08_SA_dataset)
remove(NHIS08_DO)
remove(NHIS08_SA)
remove(N08_Arth_age)
remove(N08_arth_age_ci)
remove(N08_Arth_BMI)
remove(N08_arth_BMI_ci)
remove(N08_Arth_sex)
remove(N08_arth_sex_ci)
remove(N08_Arthritis_BMI)
remove(N08_Arthritis_sex)
remove(N08_Arthtitis_age)
remove(N08_arth_age_ci)
remove(N08_overall.c)
remove(N08_overall_ci)
remove(N08.no.age.c)
remove(N08.no.age.ci)
remove(N08.no.BMI.c)
remove(N08.no.BMI.ci)
remove(N08.no.df)
remove(N08.no.ci)
remove(N08.no.sex.c)
remove(N08.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________



#----2009----
#National Health Interview Survey
#2009

#----Download----

NHIS09.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2009/samadult.sas"
NHIS09.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2009/samadult.zip"
#store as df
NHIS09_SA <- read.SAScii(NHIS09.fileloc, NHIS09.instructions, zipped = TRUE)


#----Cleaning----
str(NHIS09_SA)
tail(NHIS09_SA)
glimpse(NHIS09_SA)
colnames(NHIS09_SA)

#Select variables
NHIS09 <- select(NHIS09_SA,
                 HHX, FMX, FPX,
                 STRAT_P, PSU_P, SRVY_YR, WTFA_SA, JNTSYMP,
                 SEX, AGE_P, BMI)

str(NHIS09)
tail(NHIS09)
glimpse(NHIS09)
colnames(NHIS09)

#joint symptoms recode
table(NHIS09$JNTSYMP)
#recode
NHIS09$JNTSYMP <- recode(NHIS09$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS09$JNTSYMP <- unknownToNA(NHIS09$JNTSYMP, unknown = c("7", "9")))
table(NHIS09$JNTSYMP)
#    0     1 
#18520  9186 
NHIS09$JNTSYMP <- as.factor(NHIS09$JNTSYMP)

#age
table(NHIS09$AGE_P)
NHIS09$AGE_P <- recode(NHIS09$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS09$AGE_P)
#18 to 24     25 to 29     30 to 34     35 to 39     40 to 44     45 to 49     50 to 54     55 to 59     60 to 64     65 to 69 70 and above 
#        2822         2457         2524         2474         2498         2590         2563         2260         2050         1621         3872 
NHIS09$AGE_P <- as.factor(NHIS09$AGE_P)

#sex
NHIS09$SEX <- recode(NHIS09$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS09$SEX)
#Female   Male 
# 15470  12261 
NHIS09$SEX <- as.factor(NHIS09$SEX)


#BMI
NHIS09$BMI
NHIS09$BMI <- unknownToNA(NHIS09$BMI, unknown = c("99.95", "99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS09 %>% top_n(10, BMI)

#Create BMI categories
NHIS09$BMICAT <- ifelse(NHIS09$BMI < 18.50, "Underweight",
                        ifelse(NHIS09$BMI >= 18.50 & NHIS09$BMI < 25.00, "Healthy weight",
                               ifelse(NHIS09$BMI >= 25.00 & NHIS09$BMI < 30.00, "Overweight",
                                      ifelse(NHIS09$BMI >=30.00, "Obese",
                                             NA))))

table(NHIS09$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#          9333           7482           9499            465 
#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#make sure all complex survey requirements are available for design object
NHIS09_SA_dataset <- subset(NHIS09,
                            !is.na(WTFA_SA) &
                              !is.na(STRAT_P) &
                              !is.na(PSU_P))

#Check that the sum of the weights is equal to the US population
sum(NHIS09_SA_dataset$WTFA_SA)
#The sum of the weights is 227 371 068, which is acceptable

#Check the number of people (unique PSU's)
length(unique(NHIS09_SA_dataset[["PSU_P"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS09_SA_dataset[["STRAT_P"]]))
#The number of unique strata is 300

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS09_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRAT_P,
                       nest = TRUE,
                       data = NHIS09_SA_dataset)

#_______________________________________________________________________________________________


#----Analysis----

N09_overall <- svymean(~factor(JNTSYMP),
                       NHIS09_DO,
                       na.rm = TRUE)
N09_overall.c <- N09_overall %>%
  as.data.frame(.) %>%
  select(1) %>%
  setNames(c("Proportion")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_overall_ci <- confint(N09_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
N09 <- bind_cols(N09_overall.c, N09_overall_ci)
#remove JNTSYMP = 0
N09 <- N09[-c(1), ] #N09 = final proportion and 95% ci

#Overall number of people
N09.no <- svytotal(~JNTSYMP, 
                   NHIS09_DO, 
                   na.rm = TRUE, 
                   deff = FALSE)
N09.no.df <- as.data.frame(N09.no) %>%
  select(1)
#ci
N09.no.ci <- confint(N09.no) %>%
  as.data.frame(.)
#join number and ci
N09.no <- bind_cols(N09.no.df, N09.no.ci)
#remove JNTSYMP=0
N09.no <- N09.no[-c(1), ] #N09.no = final number of people with arth and 95%ci

#2. Demographic-specific analysis

#A. Arthritis & Age
N09_Arth_age <- svyby(formula = ~JNTSYMP,
                      by = ~AGE_P,
                      design = NHIS09_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N09_Arthtitis_age <- N09_Arth_age %>%
  select(1, 3) %>%
  setNames(c("Age", "Proportion")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_arth_age_ci <- confint(N09_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N09_arth_age_ci <- N09_arth_age_ci[-c(1:11), ]
#join ci and proportions
N09.Age <- bind_cols(N09_Arthtitis_age, N09_arth_age_ci) #N09.Age = final proportion and 95% ci by age group


#Number of people by age
N09.no.age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS09_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N09.no.age.c <- N09.no.age %>%
  select(1, 3) %>%
  setNames(c("Age", "Number of People")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N09.no.age.ci <- confint(N09.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 11 rows
N09.no.age.ci <- N09.no.age.ci[-c(1:11), ]
#join number and ci
N09.no.age <- bind_cols(N09.no.age.c, N09.no.age.ci)


#Age logistic regression
N09_age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS09_DO)
summary(N09_age_glm)
exp(cbind(OR=coef(N09_age_glm), confint(N09_age_glm)))



#B. Arthritis & sex
N09_Arth_sex <- svyby(formula = ~JNTSYMP,
                      by = ~SEX,
                      design = NHIS09_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N09_Arthritis_sex <- N09_Arth_sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Proportion")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_arth_sex_ci <- confint(N09_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N09_arth_sex_ci <- N09_arth_sex_ci[-c(1:2), ]
#join ci and proportions
N09.Sex <- bind_cols(N09_Arthritis_sex, N09_arth_sex_ci) #N09.Sex = final proportion and 95% ci by sex


#Number of people by sex
N09.no.sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS09_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N09.no.sex.c <- N09.no.sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Number of People")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N09.no.sex.ci <- confint(N09.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 2 rows
N09.no.sex.ci <- N09.no.sex.ci[-c(1:2), ]
#join number and ci
N09.no.sex <- bind_cols(N09.no.sex.c, N09.no.sex.ci)


#Sex logistic regression
N09_sex_glm <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                      family = quasibinomial,
                      design = NHIS09_DO)
summary(N09_sex_glm)
exp(cbind(OR=coef(N09_sex_glm), confint(N09_sex_glm)))


#C. Arthritis & BMI
N09_Arth_BMI <- svyby(formula = ~JNTSYMP,
                      by = ~BMICAT,
                      design = NHIS09_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N09_Arthritis_BMI <- N09_Arth_BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Proportion")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N09_arth_BMI_ci <- confint(N09_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0
N09_arth_BMI_ci <- N09_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
N09.BMI <- bind_cols(N09_Arthritis_BMI, N09_arth_BMI_ci) #N09.BMI = final proportion and 95%ci by BMI


#Number of people by BMI
N09.no.BMI <- svyby(formula = ~JNTSYMP,
                    by = ~BMICAT,
                    design = NHIS09_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
N09.no.BMI.c <- N09.no.BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Number of People")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N09.no.BMI.ci <- confint(N09.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 4 rows
N09.no.BMI.ci <- N09.no.BMI.ci[-c(1:4), ]
#join number and ci
N09.no.BMI <- bind_cols(N09.no.BMI.c, N09.no.BMI.ci)


#BMI logistic regression (using original continuous variable from survey)
N09.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS09_DO)
summary(N09.BMI.glm)
exp(cbind(OR=coef(N09.BMI.glm), confint(N09.BMI.glm)))


#   End of 2009 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(NHIS09)
remove(NHIS09_SA_dataset)
remove(NHIS09_DO)
remove(NHIS09_SA)
remove(N09_Arth_age)
remove(N09_arth_age_ci)
remove(N09_Arth_BMI)
remove(N09_arth_BMI_ci)
remove(N09_Arth_sex)
remove(N09_arth_sex_ci)
remove(N09_Arthritis_BMI)
remove(N09_Arthritis_sex)
remove(N09_Arthtitis_age)
remove(N09_arth_age_ci)
remove(N09_overall.c)
remove(N09_overall_ci)
remove(N09.no.age.c)
remove(N09.no.age.ci)
remove(N09.no.BMI.c)
remove(N09.no.BMI.ci)
remove(N09.no.df)
remove(N09.no.ci)
remove(N09.no.sex.c)
remove(N09.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________



#----2010----
#National Health Interview Survey
#2010

#----Download----

NHIS10.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2010/samadult.sas"
NHIS10.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2010/samadult.zip"
#store as df
NHIS10_SA <- read.SAScii(NHIS10.fileloc, NHIS10.instructions, zipped = TRUE)
NHIS10_SA


#----Cleaning----
str(NHIS10_SA)
tail(NHIS10_SA)
glimpse(NHIS10_SA)
colnames(NHIS10_SA)

#Select variables
NHIS10 <- select(NHIS10_SA,
                 HHX, FMX, FPX,
                 STRAT_P, PSU_P, SRVY_YR, WTFA_SA, JNTSYMP,
                 SEX, AGE_P, BMI)

str(NHIS10)
tail(NHIS10)
glimpse(NHIS10)
colnames(NHIS10)

#joint symptoms recode
table(NHIS10$JNTSYMP)
#recode
NHIS10$JNTSYMP <- recode(NHIS10$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS10$JNTSYMP <- unknownToNA(NHIS10$JNTSYMP, unknown = c("7", "9")))
table(NHIS10$JNTSYMP)
#    0     1 
#18037  9099 
NHIS10$JNTSYMP <- as.factor(NHIS10$JNTSYMP)

#age
table(NHIS10$AGE_P)
NHIS10$AGE_P <- recode(NHIS10$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS10$AGE_P)
#18 to 24     25 to 29     30 to 34     35 to 39     40 to 44     45 to 49     50 to 54     55 to 59     60 to 64     65 to 69 70 and above 
#    2801         2473         2501         2425         2380         2440         2415         2193         2079         1617         3833 
NHIS10$AGE_P <- as.factor(NHIS10$AGE_P)

#sex
NHIS10$SEX <- recode(NHIS10$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS10$SEX)
#Female   Male 
# 15171  11986 
NHIS10$SEX <- as.factor(NHIS10$SEX)


#BMI
NHIS10$BMI
NHIS10$BMI <- unknownToNA(NHIS10$BMI, unknown = c("99.95", "99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS10 %>% top_n(10, BMI)

#Create BMI categories
NHIS10$BMICAT <- ifelse(NHIS10$BMI < 18.50, "Underweight",
                        ifelse(NHIS10$BMI >= 18.50 & NHIS10$BMI < 25.00, "Healthy weight",
                               ifelse(NHIS10$BMI >= 25.00 & NHIS10$BMI < 30.00, "Overweight",
                                      ifelse(NHIS10$BMI >=30.00, "Obese",
                                             NA))))

table(NHIS10$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#          9172           7415           9092            492 
#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#make sure all complex survey requirements are available for design object
NHIS10_SA_dataset <- subset(NHIS10,
                            !is.na(WTFA_SA) &
                              !is.na(STRAT_P) &
                              !is.na(PSU_P))

#Check that the sum of the weights is equal to the US population
sum(NHIS10_SA_dataset$WTFA_SA)
#The sum of the weights is 229 505 094, which is acceptable

#Check the number of people (unique PSU's)
length(unique(NHIS10_SA_dataset[["PSU_P"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS10_SA_dataset[["STRAT_P"]]))
#The number of unique strata is 300

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS10_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRAT_P,
                       nest = TRUE,
                       data = NHIS10_SA_dataset)
#_______________________________________________________________________________________________


#----Analysis----

N10_overall <- svymean(~factor(JNTSYMP),
                       NHIS10_DO,
                       na.rm = TRUE)
N10_overall.c <- N10_overall %>%
  as.data.frame(.) %>%
  select(1) %>%
  setNames(c("Proportion")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_overall_ci <- confint(N10_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
N10 <- bind_cols(N10_overall.c, N10_overall_ci)
#remove JNTSYMP = 0
N10 <- N10[-c(1), ] #N10 = final proportion and 95% ci

#Overall number of people
N10.no <- svytotal(~JNTSYMP, 
                   NHIS10_DO, 
                   na.rm = TRUE, 
                   deff = FALSE)
N10.no.df <- as.data.frame(N10.no) %>%
  select(1)
#ci
N10.no.ci <- confint(N10.no) %>%
  as.data.frame(.)
#join number and ci
N10.no <- bind_cols(N10.no.df, N10.no.ci)
#remove JNTSYMP=0
N10.no <- N10.no[-c(1), ] #N10.no = final number of people with arth and 95%ci


#2. Demographic-specific analysis

#A. Arthritis & Age
N10_Arth_age <- svyby(formula = ~JNTSYMP,
                      by = ~AGE_P,
                      design = NHIS10_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N10_Arthtitis_age <- N10_Arth_age %>%
  select(1, 3) %>%
  setNames(c("Age", "Proportion")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_arth_age_ci <- confint(N10_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N10_arth_age_ci <- N10_arth_age_ci[-c(1:11), ]
#join ci and proportions
N10.Age <- bind_cols(N10_Arthtitis_age, N10_arth_age_ci) #N10.Age = final proportion and 95% ci by age group


#Number of people by age
N10.no.age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS10_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N10.no.age.c <- N10.no.age %>%
  select(1, 3) %>%
  setNames(c("Age", "Number of People")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N10.no.age.ci <- confint(N10.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 11 rows
N10.no.age.ci <- N10.no.age.ci[-c(1:11), ]
#join number and ci
N10.no.age <- bind_cols(N10.no.age.c, N10.no.age.ci)


#Age logistic regression
N10_age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS10_DO)
summary(N10_age_glm)
exp(cbind(OR=coef(N10_age_glm), confint(N10_age_glm)))



#B. Arthritis & sex
N10_Arth_sex <- svyby(formula = ~JNTSYMP,
                      by = ~SEX,
                      design = NHIS10_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N10_Arthritis_sex <- N10_Arth_sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Proportion")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_arth_sex_ci <- confint(N10_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N10_arth_sex_ci <- N10_arth_sex_ci[-c(1:2), ]
#join ci and proportions
N10.Sex <- bind_cols(N10_Arthritis_sex, N10_arth_sex_ci) #N10.Sex = final proportion and 95% ci by sex

#Number of people by sex
N10.no.sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS10_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N10.no.sex.c <- N10.no.sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Number of People")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N10.no.sex.ci <- confint(N10.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 2 rows
N10.no.sex.ci <- N10.no.sex.ci[-c(1:2), ]
#join number and ci
N10.no.sex <- bind_cols(N10.no.sex.c, N10.no.sex.ci)


#Sex logistic regression
N10_sex_glm <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                      family = quasibinomial,
                      design = NHIS10_DO)
summary(N10_sex_glm)
exp(cbind(OR=coef(N10_sex_glm), confint(N10_sex_glm)))


#C. Arthritis & BMI
N10_Arth_BMI <- svyby(formula = ~JNTSYMP,
                      by = ~BMICAT,
                      design = NHIS10_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N10_Arthritis_BMI <- N10_Arth_BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Proportion")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N10_arth_BMI_ci <- confint(N10_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0
N10_arth_BMI_ci <- N10_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
N10.BMI <- bind_cols(N10_Arthritis_BMI, N10_arth_BMI_ci) #N10.BMI = final proportion and 95%ci by BMI


#Number of people by BMI
N10.no.BMI <- svyby(formula = ~JNTSYMP,
                    by = ~BMICAT,
                    design = NHIS10_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
N10.no.BMI.c <- N10.no.BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Number of People")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N10.no.BMI.ci <- confint(N10.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 4 rows
N10.no.BMI.ci <- N10.no.BMI.ci[-c(1:4), ]
#join number and ci
N10.no.BMI <- bind_cols(N10.no.BMI.c, N10.no.BMI.ci)


#BMI logistic regression (using original continuous variable from survey)
N10.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS10_DO)
summary(N10.BMI.glm)
exp(cbind(OR=coef(N10.BMI.glm), confint(N10.BMI.glm)))


#   End of 2010 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(NHIS10)
remove(NHIS10_SA_dataset)
remove(NHIS10_DO)
remove(NHIS10_SA)
remove(N10_Arth_age)
remove(N10_arth_age_ci)
remove(N10_Arth_BMI)
remove(N10_arth_BMI_ci)
remove(N10_Arth_sex)
remove(N10_arth_sex_ci)
remove(N10_Arthritis_BMI)
remove(N10_Arthritis_sex)
remove(N10_Arthtitis_age)
remove(N10_arth_age_ci)
remove(N10_overall.c)
remove(N10_overall_ci)
remove(N10.no.age.c)
remove(N10.no.age.ci)
remove(N10.no.BMI.c)
remove(N10.no.BMI.ci)
remove(N10.no.df)
remove(N10.no.ci)
remove(N10.no.sex.c)
remove(N10.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________



#----2011----
#National Health Interview Survey
#2011

#----Download----

NHIS11.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2011/samadult.sas"
NHIS11.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2011/samadult.zip"
#store as df
NHIS11_SA <- read.SAScii(NHIS11.fileloc, NHIS11.instructions, zipped = TRUE)
NHIS11_SA


#----Cleaning----
str(NHIS11_SA)
tail(NHIS11_SA)
glimpse(NHIS11_SA)
colnames(NHIS11_SA)

#Select variables
NHIS11 <- select(NHIS11_SA,
                 HHX, FMX, FPX,
                 STRAT_P, PSU_P, SRVY_YR, WTFA_SA, JNTSYMP,
                 SEX, AGE_P, BMI)

str(NHIS11)
tail(NHIS11)
glimpse(NHIS11)
colnames(NHIS11)

#joint symptoms recode
table(NHIS11$JNTSYMP)
#recode
NHIS11$JNTSYMP <- recode(NHIS11$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS11$JNTSYMP <- unknownToNA(NHIS11$JNTSYMP, unknown = c("7", "9")))
table(NHIS11$JNTSYMP)
#    0     1 
#21448 11539 
NHIS11$JNTSYMP <- as.factor(NHIS11$JNTSYMP)

#age
table(NHIS11$AGE_P)
NHIS11$AGE_P <- recode(NHIS11$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS11$AGE_P)
#18 to 24     25 to 29     30 to 34     35 to 39     40 to 44     45 to 49     50 to 54     55 to 59     60 to 64     65 to 69 70 and above 
#    3291         3031         3037         2832         2843         2909         2850         2692         2627         2113         4789 
NHIS11$AGE_P <- as.factor(NHIS11$AGE_P)

#sex
NHIS11$SEX <- recode(NHIS11$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS11$SEX)
#Female   Male 
# 18203  14811 
NHIS11$SEX <- as.factor(NHIS11$SEX)


#BMI
NHIS11$BMI
NHIS11$BMI <- unknownToNA(NHIS11$BMI, unknown = c("99.95", "99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS11 %>% top_n(10, BMI)

#Create BMI categories
NHIS11$BMICAT <- ifelse(NHIS11$BMI < 18.50, "Underweight",
                        ifelse(NHIS11$BMI >= 18.50 & NHIS11$BMI < 25.00, "Healthy weight",
                               ifelse(NHIS11$BMI >= 25.00 & NHIS11$BMI < 30.00, "Overweight",
                                      ifelse(NHIS11$BMI >=30.00, "Obese",
                                             NA))))

table(NHIS11$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#         11217           9166          11086            554 
#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#make sure all complex survey requirements are available for design object
NHIS11_SA_dataset <- subset(NHIS11,
                            !is.na(WTFA_SA) &
                              !is.na(STRAT_P) &
                              !is.na(PSU_P))

#Check that the sum of the weights is equal to the US population
sum(NHIS11_SA_dataset$WTFA_SA)
#The sum of the weights is 231 375 657, which is acceptable

#Check the number of people (unique PSU's)
length(unique(NHIS11_SA_dataset[["PSU_P"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS11_SA_dataset[["STRAT_P"]]))
#The number of unique strata is 300

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS11_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRAT_P,
                       nest = TRUE,
                       data = NHIS11_SA_dataset)

#_______________________________________________________________________________________________


#----Analysis----

N11_overall <- svymean(~factor(JNTSYMP),
                       NHIS11_DO,
                       na.rm = TRUE)
N11_overall.c <- N11_overall %>%
  as.data.frame(.) %>%
  select(1) %>%
  setNames(c("Proportion")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_overall_ci <- confint(N11_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
N11 <- bind_cols(N11_overall.c, N11_overall_ci)
#remove JNTSYMP = 0
N11 <- N11[-c(1), ] #N11 = final proportion and 95% ci

#Overall number of people
N11.no <- svytotal(~JNTSYMP, 
                   NHIS11_DO, 
                   na.rm = TRUE, 
                   deff = FALSE)
N11.no.df <- as.data.frame(N11.no) %>%
  select(1)
#ci
N11.no.ci <- confint(N11.no) %>%
  as.data.frame(.)
#join number and ci
N11.no <- bind_cols(N11.no.df, N11.no.ci)
#remove JNTSYMP=0
N11.no <- N11.no[-c(1), ] #N11.no = final number of people with arth and 95%ci

#2. Demographic-specific analysis

#A. Arthritis & Age
N11_Arth_age <- svyby(formula = ~JNTSYMP,
                      by = ~AGE_P,
                      design = NHIS11_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N11_Arthtitis_age <- N11_Arth_age %>%
  select(1, 3) %>%
  setNames(c("Age", "Proportion")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_arth_age_ci <- confint(N11_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N11_arth_age_ci <- N11_arth_age_ci[-c(1:11), ]
#join ci and proportions
N11.Age <- bind_cols(N11_Arthtitis_age, N11_arth_age_ci) #N11.Age = final proportion and 95% ci by age group


#Number of people by age
N11.no.age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS11_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N11.no.age.c <- N11.no.age %>%
  select(1, 3) %>%
  setNames(c("Age", "Number of People")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N11.no.age.ci <- confint(N11.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 11 rows
N11.no.age.ci <- N11.no.age.ci[-c(1:11), ]
#join number and ci
N11.no.age <- bind_cols(N11.no.age.c, N11.no.age.ci)


#Age logistic regression
N11_age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS11_DO)
summary(N11_age_glm)
exp(cbind(OR=coef(N11_age_glm), confint(N11_age_glm)))



#B. Arthritis & sex
N11_Arth_sex <- svyby(formula = ~JNTSYMP,
                      by = ~SEX,
                      design = NHIS11_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N11_Arthritis_sex <- N11_Arth_sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Proportion")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_arth_sex_ci <- confint(N11_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N11_arth_sex_ci <- N11_arth_sex_ci[-c(1:2), ]
#join ci and proportions
N11.Sex <- bind_cols(N11_Arthritis_sex, N11_arth_sex_ci) #N11.Sex = final proportion and 95% ci by sex


#Number of people by sex
N11.no.sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS11_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N11.no.sex.c <- N11.no.sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Number of People")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N11.no.sex.ci <- confint(N11.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 2 rows
N11.no.sex.ci <- N11.no.sex.ci[-c(1:2), ]
#join number and ci
N11.no.sex <- bind_cols(N11.no.sex.c, N11.no.sex.ci)


#Sex logistic regression
N11_sex_glm <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                      family = quasibinomial,
                      design = NHIS11_DO)
summary(N11_sex_glm)
exp(cbind(OR=coef(N11_sex_glm), confint(N11_sex_glm)))


#C. Arthritis & BMI
N11_Arth_BMI <- svyby(formula = ~JNTSYMP,
                      by = ~BMICAT,
                      design = NHIS11_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N11_Arthritis_BMI <- N11_Arth_BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Proportion")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N11_arth_BMI_ci <- confint(N11_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0
N11_arth_BMI_ci <- N11_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
N11.BMI <- bind_cols(N11_Arthritis_BMI, N11_arth_BMI_ci) #N11.BMI = final proportion and 95%ci by BMI


#Number of people by BMI
N11.no.BMI <- svyby(formula = ~JNTSYMP,
                    by = ~BMICAT,
                    design = NHIS11_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
N11.no.BMI.c <- N11.no.BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Number of People")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N11.no.BMI.ci <- confint(N11.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 4 rows
N11.no.BMI.ci <- N11.no.BMI.ci[-c(1:4), ]
#join number and ci
N11.no.BMI <- bind_cols(N11.no.BMI.c, N11.no.BMI.ci)


#BMI logistic regression (using original continuous variable from survey)
N11.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS11_DO)
summary(N11.BMI.glm)
exp(cbind(OR=coef(N11.BMI.glm), confint(N11.BMI.glm)))


#   End of 2011 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(NHIS11)
remove(NHIS11_SA_dataset)
remove(NHIS11_DO)
remove(NHIS11_SA)
remove(N11_Arth_age)
remove(N11_arth_age_ci)
remove(N11_Arth_BMI)
remove(N11_arth_BMI_ci)
remove(N11_Arth_sex)
remove(N11_arth_sex_ci)
remove(N11_Arthritis_BMI)
remove(N11_Arthritis_sex)
remove(N11_Arthtitis_age)
remove(N11_arth_age_ci)
remove(N11_overall.c)
remove(N11_overall_ci)
remove(N11.no.age.c)
remove(N11.no.age.ci)
remove(N11.no.BMI.c)
remove(N11.no.BMI.ci)
remove(N11.no.df)
remove(N11.no.ci)
remove(N11.no.sex.c)
remove(N11.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________


#----2012----
#National Health Interview Survey
#2012

#----Download----
NHIS12.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2012/samadult.sas"
NHIS12.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2012/samadult.zip"
#store as df
NHIS12_SA <- read.SAScii(NHIS12.fileloc, NHIS12.instructions, zipped = TRUE)
NHIS12_SA


#----Cleaning----
str(NHIS12_SA)
tail(NHIS12_SA)
glimpse(NHIS12_SA)
colnames(NHIS12_SA)

#Select variables
NHIS12 <- select(NHIS12_SA,
                 HHX, FMX, FPX,
                 STRAT_P, PSU_P, SRVY_YR, WTFA_SA, JNTSYMP,
                 SEX, AGE_P, BMI)

str(NHIS12)
tail(NHIS12)
glimpse(NHIS12)
colnames(NHIS12)

#joint symptoms recode
table(NHIS12$JNTSYMP)
#recode
NHIS12$JNTSYMP <- recode(NHIS12$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS12$JNTSYMP <- unknownToNA(NHIS12$JNTSYMP, unknown = c("7", "9")))
table(NHIS12$JNTSYMP)
#    0     1 
#23528 10978 
NHIS12$JNTSYMP <- as.factor(NHIS12$JNTSYMP)

#age
table(NHIS12$AGE_P)
NHIS12$AGE_P <- recode(NHIS12$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS12$AGE_P)
#18 to 24     25 to 29     30 to 34     35 to 39     40 to 44     45 to 49     50 to 54     55 to 59     60 to 64     65 to 69 70 and above 
#    3417         3003         3111         2903         2856         2875         3080         3011         2887         2306         5076 
NHIS12$AGE_P <- as.factor(NHIS12$AGE_P)

#sex
NHIS12$SEX <- recode(NHIS12$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS12$SEX)
#Female   Male 
# 19252  15273 
NHIS12$SEX <- as.factor(NHIS12$SEX)


#BMI
NHIS12$BMI
NHIS12$BMI <- unknownToNA(NHIS12$BMI, unknown = c("99.95", "99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS12 %>% top_n(10, BMI)

#Create BMI categories
NHIS12$BMICAT <- ifelse(NHIS12$BMI < 18.50, "Underweight",
                        ifelse(NHIS12$BMI >= 18.50 & NHIS12$BMI < 25.00, "Healthy weight",
                               ifelse(NHIS12$BMI >= 25.00 & NHIS12$BMI < 30.00, "Overweight",
                                      ifelse(NHIS12$BMI >=30.00, "Obese",
                                             NA))))

table(NHIS12$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#         11543           9573          11477            577 
#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#make sure all complex survey requirements are available for design object
NHIS12_SA_dataset <- subset(NHIS12,
                            !is.na(WTFA_SA) &
                              !is.na(STRAT_P) &
                              !is.na(PSU_P))

#Check that the sum of the weights is equal to the US population
sum(NHIS12_SA_dataset$WTFA_SA)
#The sum of the weights is 234 920 670, which is acceptable

#Check the number of people (unique PSU's)
length(unique(NHIS12_SA_dataset[["PSU_P"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS12_SA_dataset[["STRAT_P"]]))
#The number of unique strata is 300

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS12_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRAT_P,
                       nest = TRUE,
                       data = NHIS12_SA_dataset)

#_______________________________________________________________________________________________


#----Analysis----

N12_overall <- svymean(~factor(JNTSYMP),
                       NHIS12_DO,
                       na.rm = TRUE)
N12_overall.c <- N12_overall %>%
  as.data.frame(.) %>%
  select(1) %>%
  setNames(c("Proportion")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_overall_ci <- confint(N12_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
N12 <- bind_cols(N12_overall.c, N12_overall_ci)
#remove JNTSYMP = 0
N12 <- N12[-c(1), ] #N12 = final proportion and 95% ci

#Overall number of people
N12.no <- svytotal(~JNTSYMP, 
                   NHIS12_DO, 
                   na.rm = TRUE, 
                   deff = FALSE)
N12.no.df <- as.data.frame(N12.no) %>%
  select(1)
#ci
N12.no.ci <- confint(N12.no) %>%
  as.data.frame(.)
#join number and ci
N12.no <- bind_cols(N12.no.df, N12.no.ci)
#remove JNTSYMP=0
N12.no <- N12.no[-c(1), ] #N12.no = final number of people with arth and 95%ci


#2. Demographic-specific analysis

#A. Arthritis & Age
N12_Arth_age <- svyby(formula = ~JNTSYMP,
                      by = ~AGE_P,
                      design = NHIS12_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N12_Arthtitis_age <- N12_Arth_age %>%
  select(1, 3) %>%
  setNames(c("Age", "Proportion")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_arth_age_ci <- confint(N12_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N12_arth_age_ci <- N12_arth_age_ci[-c(1:11), ]
#join ci and proportions
N12.Age <- bind_cols(N12_Arthtitis_age, N12_arth_age_ci) #N12.Age = final proportion and 95% ci by age group


#Number of people by age
N12.no.age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS12_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N12.no.age.c <- N12.no.age %>%
  select(1, 3) %>%
  setNames(c("Age", "Number of People")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N12.no.age.ci <- confint(N12.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 11 rows
N12.no.age.ci <- N12.no.age.ci[-c(1:11), ]
#join number and ci
N12.no.age <- bind_cols(N12.no.age.c, N12.no.age.ci)


#Age logistic regression
N12_age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS12_DO)
summary(N12_age_glm)
exp(cbind(OR=coef(N12_age_glm), confint(N12_age_glm)))



#B. Arthritis & sex
N12_Arth_sex <- svyby(formula = ~JNTSYMP,
                      by = ~SEX,
                      design = NHIS12_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N12_Arthritis_sex <- N12_Arth_sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Proportion")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_arth_sex_ci <- confint(N12_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N12_arth_sex_ci <- N12_arth_sex_ci[-c(1:2), ]
#join ci and proportions
N12.Sex <- bind_cols(N12_Arthritis_sex, N12_arth_sex_ci) #N12.Sex = final proportion and 95% ci by sex


#Number of people by sex
N12.no.sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS12_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N12.no.sex.c <- N12.no.sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Number of People")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N12.no.sex.ci <- confint(N12.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 2 rows
N12.no.sex.ci <- N12.no.sex.ci[-c(1:2), ]
#join number and ci
N12.no.sex <- bind_cols(N12.no.sex.c, N12.no.sex.ci)

#Sex logistic regression
N12_sex_glm <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                      family = quasibinomial,
                      design = NHIS12_DO)
summary(N12_sex_glm)
exp(cbind(OR=coef(N12_sex_glm), confint(N12_sex_glm)))


#C. Arthritis & BMI
N12_Arth_BMI <- svyby(formula = ~JNTSYMP,
                      by = ~BMICAT,
                      design = NHIS12_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N12_Arthritis_BMI <- N12_Arth_BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Proportion")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N12_arth_BMI_ci <- confint(N12_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0
N12_arth_BMI_ci <- N12_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
N12.BMI <- bind_cols(N12_Arthritis_BMI, N12_arth_BMI_ci) #N12.BMI = final proportion and 95%ci by BMI


#Number of people by BMI
N12.no.BMI <- svyby(formula = ~JNTSYMP,
                    by = ~BMICAT,
                    design = NHIS12_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
N12.no.BMI.c <- N12.no.BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Number of People")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N12.no.BMI.ci <- confint(N12.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 4 rows
N12.no.BMI.ci <- N12.no.BMI.ci[-c(1:4), ]
#join number and ci
N12.no.BMI <- bind_cols(N12.no.BMI.c, N12.no.BMI.ci)

#BMI logistic regression (using original continuous variable from survey)
N12.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS12_DO)
summary(N12.BMI.glm)
exp(cbind(OR=coef(N12.BMI.glm), confint(N12.BMI.glm)))


#   End of 2012 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(NHIS12)
remove(NHIS12_SA_dataset)
remove(NHIS12_DO)
remove(NHIS12_SA)
remove(N12_Arth_age)
remove(N12_arth_age_ci)
remove(N12_Arth_BMI)
remove(N12_arth_BMI_ci)
remove(N12_Arth_sex)
remove(N12_arth_sex_ci)
remove(N12_Arthritis_BMI)
remove(N12_Arthritis_sex)
remove(N12_Arthtitis_age)
remove(N12_arth_age_ci)
remove(N12_overall.c)
remove(N12_overall_ci)
remove(N12.no.age.c)
remove(N12.no.age.ci)
remove(N12.no.BMI.c)
remove(N12.no.BMI.ci)
remove(N12.no.df)
remove(N12.no.ci)
remove(N12.no.sex.c)
remove(N12.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________


#----2013----
#National Health Interview Survey
#2013

#----Download----
NHIS13.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2013/samadult.sas"
NHIS13.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2013/samadult.zip"
#store as df
NHIS13_SA <- read.SAScii(NHIS13.fileloc, NHIS13.instructions, zipped = TRUE)
NHIS13_SA


#----Cleaning----
str(NHIS13_SA)
tail(NHIS13_SA)
glimpse(NHIS13_SA)
colnames(NHIS13_SA)

#Select variables
NHIS13 <- select(NHIS13_SA,
                 HHX, FMX, FPX,
                 STRAT_P, PSU_P, SRVY_YR, WTFA_SA, JNTSYMP,
                 SEX, AGE_P, BMI)

str(NHIS13)
tail(NHIS13)
glimpse(NHIS13)
colnames(NHIS13)

#joint symptoms recode
table(NHIS13$JNTSYMP)
#recode
NHIS13$JNTSYMP <- recode(NHIS13$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS13$JNTSYMP <- unknownToNA(NHIS13$JNTSYMP, unknown = c("7", "9")))
table(NHIS13$JNTSYMP)
#    0     1 
#22819 11706 
NHIS13$JNTSYMP <- as.factor(NHIS13$JNTSYMP)

#age
table(NHIS13$AGE_P)
NHIS13$AGE_P <- recode(NHIS13$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS13$AGE_P)
#18 to 24     25 to 29     30 to 34     35 to 39     40 to 44     45 to 49     50 to 54     55 to 59     60 to 64     65 to 69 70 and above 
#        3289         3117         3174         2810         2876         2786         2986         3056         2731         2486         5246 
NHIS13$AGE_P <- as.factor(NHIS13$AGE_P)

#sex
NHIS13$SEX <- recode(NHIS13$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS13$SEX)
#Female   Male 
# 19117  15440 
NHIS13$SEX <- as.factor(NHIS13$SEX)


#BMI
NHIS13$BMI
NHIS13$BMI <- unknownToNA(NHIS13$BMI, unknown = c("99.95", "99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS13 %>% top_n(10, BMI)

#Create BMI categories
NHIS13$BMICAT <- ifelse(NHIS13$BMI < 18.50, "Underweight",
                        ifelse(NHIS13$BMI >= 18.50 & NHIS13$BMI < 25.00, "Healthy weight",
                               ifelse(NHIS13$BMI >= 25.00 & NHIS13$BMI < 30.00, "Overweight",
                                      ifelse(NHIS13$BMI >=30.00, "Obese",
                                             NA))))

table(NHIS13$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#         11650           9673          11425            640 
#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#make sure all complex survey requirements are available for design object
NHIS13_SA_dataset <- subset(NHIS13,
                            !is.na(WTFA_SA) &
                              !is.na(STRAT_P) &
                              !is.na(PSU_P))

#Check that the sum of the weights is equal to the US population
sum(NHIS13_SA_dataset$WTFA_SA)
#The sum of the weights is 237 394 354, which is acceptable

#Check the number of people (unique PSU's)
length(unique(NHIS13_SA_dataset[["PSU_P"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS13_SA_dataset[["STRAT_P"]]))
#The number of unique strata is 300

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS13_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRAT_P,
                       nest = TRUE,
                       data = NHIS13_SA_dataset)

#_______________________________________________________________________________________________


#----Analysis----

N13_overall <- svymean(~factor(JNTSYMP),
                       NHIS13_DO,
                       na.rm = TRUE)
N13_overall.c <- N13_overall %>%
  as.data.frame(.) %>%
  select(1) %>%
  setNames(c("Proportion")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_overall_ci <- confint(N13_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
N13 <- bind_cols(N13_overall.c, N13_overall_ci)
#remove JNTSYMP = 0
N13 <- N13[-c(1), ] #N13 = final proportion and 95% ci

#Overall number of people
N13.no <- svytotal(~JNTSYMP, 
                   NHIS13_DO, 
                   na.rm = TRUE, 
                   deff = FALSE)
N13.no.df <- as.data.frame(N13.no) %>%
  select(1)
#ci
N13.no.ci <- confint(N13.no) %>%
  as.data.frame(.)
#join number and ci
N13.no <- bind_cols(N13.no.df, N13.no.ci)
#remove JNTSYMP=0
N13.no <- N13.no[-c(1), ] #N13.no = final number of people with arth and 95%ci


#2. Demographic-specific analysis

#A. Arthritis & Age
N13_Arth_age <- svyby(formula = ~JNTSYMP,
                      by = ~AGE_P,
                      design = NHIS13_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N13_Arthtitis_age <- N13_Arth_age %>%
  select(1, 3) %>%
  setNames(c("Age", "Proportion")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_arth_age_ci <- confint(N13_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N13_arth_age_ci <- N13_arth_age_ci[-c(1:11), ]
#join ci and proportions
N13.Age <- bind_cols(N13_Arthtitis_age, N13_arth_age_ci) #N13.Age = final proportion and 95% ci by age group


#Number of people by age
N13.no.age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS13_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N13.no.age.c <- N13.no.age %>%
  select(1, 3) %>%
  setNames(c("Age", "Number of People")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N13.no.age.ci <- confint(N13.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 11 rows
N13.no.age.ci <- N13.no.age.ci[-c(1:11), ]
#join number and ci
N13.no.age <- bind_cols(N13.no.age.c, N13.no.age.ci)


#Age logistic regression
N13_age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS13_DO)
summary(N13_age_glm)
exp(cbind(OR=coef(N13_age_glm), confint(N13_age_glm)))



#B. Arthritis & sex
N13_Arth_sex <- svyby(formula = ~JNTSYMP,
                      by = ~SEX,
                      design = NHIS13_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N13_Arthritis_sex <- N13_Arth_sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Proportion")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_arth_sex_ci <- confint(N13_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N13_arth_sex_ci <- N13_arth_sex_ci[-c(1:2), ]
#join ci and proportions
N13.Sex <- bind_cols(N13_Arthritis_sex, N13_arth_sex_ci) #N13.Sex = final proportion and 95% ci by sex


#Number of people by sex
N13.no.sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS13_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N13.no.sex.c <- N13.no.sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Number of People")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N13.no.sex.ci <- confint(N13.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 2 rows
N13.no.sex.ci <- N13.no.sex.ci[-c(1:2), ]
#join number and ci
N13.no.sex <- bind_cols(N13.no.sex.c, N13.no.sex.ci)


#Sex logistic regression
N13_sex_glm <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                      family = quasibinomial,
                      design = NHIS13_DO)
summary(N13_sex_glm)
exp(cbind(OR=coef(N13_sex_glm), confint(N13_sex_glm)))


#C. Arthritis & BMI
N13_Arth_BMI <- svyby(formula = ~JNTSYMP,
                      by = ~BMICAT,
                      design = NHIS13_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N13_Arthritis_BMI <- N13_Arth_BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Proportion")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N13_arth_BMI_ci <- confint(N13_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0
N13_arth_BMI_ci <- N13_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
N13.BMI <- bind_cols(N13_Arthritis_BMI, N13_arth_BMI_ci) #N13.BMI = final proportion and 95%ci by BMI


#Number of people by BMI
N13.no.BMI <- svyby(formula = ~JNTSYMP,
                    by = ~BMICAT,
                    design = NHIS13_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
N13.no.BMI.c <- N13.no.BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Number of People")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N13.no.BMI.ci <- confint(N13.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 4 rows
N13.no.BMI.ci <- N13.no.BMI.ci[-c(1:4), ]
#join number and ci
N13.no.BMI <- bind_cols(N13.no.BMI.c, N13.no.BMI.ci)


#BMI logistic regression (using original continuous variable from survey)
N13.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS13_DO)
summary(N13.BMI.glm)
exp(cbind(OR=coef(N13.BMI.glm), confint(N13.BMI.glm)))


#   End of 2013 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(NHIS13)
remove(NHIS13_SA_dataset)
remove(NHIS13_DO)
remove(NHIS13_SA)
remove(N13_Arth_age)
remove(N13_arth_age_ci)
remove(N13_Arth_BMI)
remove(N13_arth_BMI_ci)
remove(N13_Arth_sex)
remove(N13_arth_sex_ci)
remove(N13_Arthritis_BMI)
remove(N13_Arthritis_sex)
remove(N13_Arthtitis_age)
remove(N13_arth_age_ci)
remove(N13_overall.c)
remove(N13_overall_ci)
remove(N13.no.age.c)
remove(N13.no.age.ci)
remove(N13.no.BMI.c)
remove(N13.no.BMI.ci)
remove(N13.no.df)
remove(N13.no.ci)
remove(N13.no.sex.c)
remove(N13.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________


#----2014----
#National Health Interview Survey
#2014

#----Download----
NHIS14.instructions <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Program_Code/NHIS/2014/samadult.sas"
NHIS14.fileloc <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2014/samadult.zip"
#store as df
NHIS14_SA <- read.SAScii(NHIS14.fileloc, NHIS14.instructions, zipped = TRUE)
NHIS14_SA


#----Cleaning----
str(NHIS14_SA)
tail(NHIS14_SA)
glimpse(NHIS14_SA)
colnames(NHIS14_SA)

#Select variables
NHIS14 <- select(NHIS14_SA,
                 HHX, FMX, FPX,
                 STRAT_P, PSU_P, SRVY_YR, WTFA_SA, JNTSYMP,
                 SEX, AGE_P, BMI)

str(NHIS14)
tail(NHIS14)
glimpse(NHIS14)
colnames(NHIS14)

#joint symptoms recode
table(NHIS14$JNTSYMP)
#recode
NHIS14$JNTSYMP <- recode(NHIS14$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS14$JNTSYMP <- unknownToNA(NHIS14$JNTSYMP, unknown = c("7", "9")))
table(NHIS14$JNTSYMP)
#    0     1 
#23758 12904 
NHIS14$JNTSYMP <- as.factor(NHIS14$JNTSYMP)

#age
table(NHIS14$AGE_P)
NHIS14$AGE_P <- recode(NHIS14$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS14$AGE_P)
#18 to 24     25 to 29     30 to 34     35 to 39     40 to 44     45 to 49     50 to 54     55 to 59     60 to 64     65 to 69 70 and above 
#    3353         3204         3227         2941         3006         2855         3262         3221         2984         2802         5842 
NHIS14$AGE_P <- as.factor(NHIS14$AGE_P)

#sex
NHIS14$SEX <- recode(NHIS14$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS14$SEX)
#Female   Male 
# 20299  16398 
NHIS14$SEX <- as.factor(NHIS14$SEX)


#BMI
NHIS14$BMI
NHIS14$BMI <- unknownToNA(NHIS14$BMI, unknown = c("99.95", "99.99"))
#quick check to be sure that there are no more potential unknowns
NHIS14 %>% top_n(10, BMI)

#Create BMI categories
NHIS14$BMICAT <- ifelse(NHIS14$BMI < 18.50, "Underweight",
                        ifelse(NHIS14$BMI >= 18.50 & NHIS14$BMI < 25.00, "Healthy weight",
                               ifelse(NHIS14$BMI >= 25.00 & NHIS14$BMI < 30.00, "Overweight",
                                      ifelse(NHIS14$BMI >=30.00, "Obese",
                                             NA))))

table(NHIS14$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#         11944          10580          12190            658 
#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#make sure all complex survey requirements are available for design object
NHIS14_SA_dataset <- subset(NHIS14,
                            !is.na(WTFA_SA) &
                              !is.na(STRAT_P) &
                              !is.na(PSU_P))

#Check that the sum of the weights is equal to the US population
sum(NHIS14_SA_dataset$WTFA_SA)
#The sum of the weights is 239 688 457, which is acceptable

#Check the number of people (unique PSU's)
length(unique(NHIS14_SA_dataset[["PSU_P"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS14_SA_dataset[["STRAT_P"]]))
#The number of unique strata is 300

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS14_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRAT_P,
                       nest = TRUE,
                       data = NHIS14_SA_dataset)

#_______________________________________________________________________________________________


#----Analysis----

N14_overall <- svymean(~factor(JNTSYMP),
                       NHIS14_DO,
                       na.rm = TRUE)
N14_overall.c <- N14_overall %>%
  as.data.frame(.) %>%
  select(1) %>%
  setNames(c("Proportion")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_overall_ci <- confint(N14_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
N14 <- bind_cols(N14_overall.c, N14_overall_ci)
#remove JNTSYMP = 0
N14 <- N14[-c(1), ] #N14 = final proportion and 95% ci

#Overall number of people
N14.no <- svytotal(~JNTSYMP, 
                   NHIS14_DO, 
                   na.rm = TRUE, 
                   deff = FALSE)
N14.no.df <- as.data.frame(N14.no) %>%
  select(1)
#ci
N14.no.ci <- confint(N14.no) %>%
  as.data.frame(.)
#join number and ci
N14.no <- bind_cols(N14.no.df, N14.no.ci)
#remove JNTSYMP=0
N14.no <- N14.no[-c(1), ] #N14.no = final number of people with arth and 95%ci


#2. Demographic-specific analysis

#A. Arthritis & Age
N14_Arth_age <- svyby(formula = ~JNTSYMP,
                      by = ~AGE_P,
                      design = NHIS14_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N14_Arthtitis_age <- N14_Arth_age %>%
  select(1, 3) %>%
  setNames(c("Age", "Proportion")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_arth_age_ci <- confint(N14_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N14_arth_age_ci <- N14_arth_age_ci[-c(1:11), ]
#join ci and proportions
N14.Age <- bind_cols(N14_Arthtitis_age, N14_arth_age_ci) #N14.Age = final proportion and 95% ci by age group


#Number of people by age
N14.no.age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS14_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N14.no.age.c <- N14.no.age %>%
  select(1, 3) %>%
  setNames(c("Age", "Number of People")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N14.no.age.ci <- confint(N14.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 11 rows
N14.no.age.ci <- N14.no.age.ci[-c(1:11), ]
#join number and ci
N14.no.age <- bind_cols(N14.no.age.c, N14.no.age.ci)

#Age logistic regression
N14_age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS14_DO)
summary(N14_age_glm)
exp(cbind(OR=coef(N14_age_glm), confint(N14_age_glm)))

#B. Arthritis & sex
N14_Arth_sex <- svyby(formula = ~JNTSYMP,
                      by = ~SEX,
                      design = NHIS14_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N14_Arthritis_sex <- N14_Arth_sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Proportion")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_arth_sex_ci <- confint(N14_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N14_arth_sex_ci <- N14_arth_sex_ci[-c(1:2), ]
#join ci and proportions
N14.Sex <- bind_cols(N14_Arthritis_sex, N14_arth_sex_ci) #N14.Sex = final proportion and 95% ci by sex


#Number of people by sex
N14.no.sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS14_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N14.no.sex.c <- N14.no.sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Number of People")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N14.no.sex.ci <- confint(N14.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 2 rows
N14.no.sex.ci <- N14.no.sex.ci[-c(1:2), ]
#join number and ci
N14.no.sex <- bind_cols(N14.no.sex.c, N14.no.sex.ci)

#Sex logistic regression
N14_sex_glm <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                      family = quasibinomial,
                      design = NHIS14_DO)
summary(N14_sex_glm)
exp(cbind(OR=coef(N14_sex_glm), confint(N14_sex_glm)))


#C. Arthritis & BMI
N14_Arth_BMI <- svyby(formula = ~JNTSYMP,
                      by = ~BMICAT,
                      design = NHIS14_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N14_Arthritis_BMI <- N14_Arth_BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Proportion")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N14_arth_BMI_ci <- confint(N14_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0
N14_arth_BMI_ci <- N14_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
N14.BMI <- bind_cols(N14_Arthritis_BMI, N14_arth_BMI_ci) #N14.BMI = final proportion and 95%ci by BMI

#Number of people by BMI
N14.no.BMI <- svyby(formula = ~JNTSYMP,
                    by = ~BMICAT,
                    design = NHIS14_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
N14.no.BMI.c <- N14.no.BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Number of People")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N14.no.BMI.ci <- confint(N14.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 4 rows
N14.no.BMI.ci <- N14.no.BMI.ci[-c(1:4), ]
#join number and ci
N14.no.BMI <- bind_cols(N14.no.BMI.c, N14.no.BMI.ci)


#BMI logistic regression (using original continuous variable from survey)
N14.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS14_DO)
summary(N14.BMI.glm)
exp(cbind(OR=coef(N14.BMI.glm), confint(N14.BMI.glm)))


#   End of 2014 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(NHIS14)
remove(NHIS14_SA_dataset)
remove(NHIS14_DO)
remove(NHIS14_SA)
remove(N14_Arth_age)
remove(N14_arth_age_ci)
remove(N14_Arth_BMI)
remove(N14_arth_BMI_ci)
remove(N14_Arth_sex)
remove(N14_arth_sex_ci)
remove(N14_Arthritis_BMI)
remove(N14_Arthritis_sex)
remove(N14_Arthtitis_age)
remove(N14_arth_age_ci)
remove(N14_overall.c)
remove(N14_overall_ci)
remove(N14.no.age.c)
remove(N14.no.age.ci)
remove(N14.no.BMI.c)
remove(N14.no.BMI.ci)
remove(N14.no.df)
remove(N14.no.ci)
remove(N14.no.sex.c)
remove(N14.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

#----2015----
#National Health Interview Survey
#2015

#----Download----
tempN15 <- tempfile()
download.file("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2015/samadult.zip",
              tempN15)
NHIS15_SA <- read.csv(unz(tempN15, "samadult.csv"))
NHIS15_SA


#----Cleaning----
str(NHIS15_SA)
tail(NHIS15_SA)
glimpse(NHIS15_SA)
colnames(NHIS15_SA)

#Select variables
NHIS15 <- select(NHIS15_SA,
                 HHX, FMX, FPX,
                 STRAT_P, PSU_P, SRVY_YR, WTFA_SA, JNTSYMP,
                 SEX, AGE_P, BMI)

str(NHIS15)
tail(NHIS15)
glimpse(NHIS15)
colnames(NHIS15)

#joint symptoms recode
table(NHIS15$JNTSYMP)
#recode
NHIS15$JNTSYMP <- recode(NHIS15$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS15$JNTSYMP <- unknownToNA(NHIS15$JNTSYMP, unknown = c("7", "9")))
table(NHIS15$JNTSYMP)
#    0     1 
#21306 12340 
NHIS15$JNTSYMP <- as.factor(NHIS15$JNTSYMP)

#age
table(NHIS15$AGE_P)
NHIS15$AGE_P <- recode(NHIS15$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS15$AGE_P)
#18 to 24     25 to 29     30 to 34     35 to 39     40 to 44     45 to 49     50 to 54     55 to 59     60 to 64     65 to 69 70 and above 
#    2890         2802         2981         2671         2613         2659         2907         2969         2802         2680         5698
NHIS15$AGE_P <- as.factor(NHIS15$AGE_P)

#sex
NHIS15$SEX <- recode(NHIS15$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS15$SEX)
#Female   Male 
# 18601  15071 
NHIS15$SEX <- as.factor(NHIS15$SEX)


#BMI
NHIS15$BMI
#implied 2 decimal places, therefore divide all values by 100 to get the BMI value
NHIS15 <- NHIS15 %>%
  mutate(BMI = BMI/100)
#change all 99.99 (unknown values) to NA
NHIS15$BMI <- unknownToNA(NHIS15$BMI, unknown = 99.99)
#quick check to be sure that there are no more potential unknowns
NHIS15 %>% top_n(10, BMI)

#Create BMI categories
NHIS15$BMICAT <- ifelse(NHIS15$BMI < 18.50, "Underweight",
                        ifelse(NHIS15$BMI >= 18.50 & NHIS15$BMI < 25.00, "Healthy weight",
                               ifelse(NHIS15$BMI >= 25.00 & NHIS15$BMI < 30.00, "Overweight",
                                      ifelse(NHIS15$BMI >=30.00, "Obese",
                                             NA))))

table(NHIS15$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#         10863          10016          11045            603 
#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#make sure all complex survey requirements are available for design object
NHIS15_SA_dataset <- subset(NHIS15,
                            !is.na(WTFA_SA) &
                              !is.na(STRAT_P) &
                              !is.na(PSU_P))

#Check that the sum of the weights is equal to the US population
sum(NHIS15_SA_dataset$WTFA_SA)
#The sum of the weights is 242 500 657, which is acceptable

#Check the number of people (unique PSU's)
length(unique(NHIS15_SA_dataset[["PSU_P"]]))
#The number of unique PSU's in the data is 2

#Check the number of unique strata
length(unique(NHIS15_SA_dataset[["STRAT_P"]]))
#The number of unique strata is 300

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS15_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~STRAT_P,
                       nest = TRUE,
                       data = NHIS15_SA_dataset)
#_______________________________________________________________________________________________


#----Analysis----

N15_overall <- svymean(~factor(JNTSYMP),
                       NHIS15_DO,
                       na.rm = TRUE)
N15_overall.c <- N15_overall %>%
  as.data.frame(.) %>%
  select(1) %>%
  setNames(c("Proportion")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_overall_ci <- confint(N15_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
N15 <- bind_cols(N15_overall.c, N15_overall_ci)
#remove JNTSYMP = 0
N15 <- N15[-c(1), ] #N15 = final proportion and 95% ci

#Overall number of people
N15.no <- svytotal(~JNTSYMP, 
                   NHIS15_DO, 
                   na.rm = TRUE, 
                   deff = FALSE)
N15.no.df <- as.data.frame(N15.no) %>%
  select(1)
#ci
N15.no.ci <- confint(N15.no) %>%
  as.data.frame(.)
#join number and ci
N15.no <- bind_cols(N15.no.df, N15.no.ci)
#remove JNTSYMP=0
N15.no <- N15.no[-c(1), ] #N15.no = final number of people with arth and 95%ci


#2. Demographic-specific analysis

#A. Arthritis & Age
N15_Arth_age <- svyby(formula = ~JNTSYMP,
                      by = ~AGE_P,
                      design = NHIS15_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N15_Arthtitis_age <- N15_Arth_age %>%
  select(1, 3) %>%
  setNames(c("Age", "Proportion")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_arth_age_ci <- confint(N15_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N15_arth_age_ci <- N15_arth_age_ci[-c(1:11), ]
#join ci and proportions
N15.Age <- bind_cols(N15_Arthtitis_age, N15_arth_age_ci) #N15.Age = final proportion and 95% ci by age group


#Number of people by age
N15.no.age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS15_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N15.no.age.c <- N15.no.age %>%
  select(1, 3) %>%
  setNames(c("Age", "Number of People")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N15.no.age.ci <- confint(N15.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 11 rows
N15.no.age.ci <- N15.no.age.ci[-c(1:11), ]
#join number and ci
N15.no.age <- bind_cols(N15.no.age.c, N15.no.age.ci)


#Age logistic regression
N15_age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS15_DO)
summary(N15_age_glm)
exp(cbind(OR=coef(N15_age_glm), confint(N15_age_glm)))



#B. Arthritis & sex
N15_Arth_sex <- svyby(formula = ~JNTSYMP,
                      by = ~SEX,
                      design = NHIS15_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N15_Arthritis_sex <- N15_Arth_sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Proportion")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_arth_sex_ci <- confint(N15_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N15_arth_sex_ci <- N15_arth_sex_ci[-c(1:2), ]
#join ci and proportions
N15.Sex <- bind_cols(N15_Arthritis_sex, N15_arth_sex_ci) #N15.Sex = final proportion and 95% ci by sex


#Number of people by sex
N15.no.sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS15_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N15.no.sex.c <- N15.no.sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Number of People")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N15.no.sex.ci <- confint(N15.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 2 rows
N15.no.sex.ci <- N15.no.sex.ci[-c(1:2), ]
#join number and ci
N15.no.sex <- bind_cols(N15.no.sex.c, N15.no.sex.ci)


#Sex logistic regression
N15_sex_glm <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                      family = quasibinomial,
                      design = NHIS15_DO)
summary(N15_sex_glm)
exp(cbind(OR=coef(N15_sex_glm), confint(N15_sex_glm)))


#C. Arthritis & BMI
N15_Arth_BMI <- svyby(formula = ~JNTSYMP,
                      by = ~BMICAT,
                      design = NHIS15_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N15_Arthritis_BMI <- N15_Arth_BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Proportion")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N15_arth_BMI_ci <- confint(N15_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0
N15_arth_BMI_ci <- N15_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
N15.BMI <- bind_cols(N15_Arthritis_BMI, N15_arth_BMI_ci) #N15.BMI = final proportion and 95%ci by BMI


#Number of people by BMI
N15.no.BMI <- svyby(formula = ~JNTSYMP,
                    by = ~BMICAT,
                    design = NHIS15_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
N15.no.BMI.c <- N15.no.BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Number of People")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N15.no.BMI.ci <- confint(N15.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 4 rows
N15.no.BMI.ci <- N15.no.BMI.ci[-c(1:4), ]
#join number and ci
N15.no.BMI <- bind_cols(N15.no.BMI.c, N15.no.BMI.ci)


#BMI logistic regression (using original continuous variable from survey)
N15.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS15_DO)
summary(N15.BMI.glm)
exp(cbind(OR=coef(N15.BMI.glm), confint(N15.BMI.glm)))


#   End of 2015 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(NHIS15)
remove(NHIS15_SA_dataset)
remove(NHIS15_DO)
remove(NHIS15_SA)
remove(N15_Arth_age)
remove(N15_arth_age_ci)
remove(N15_Arth_BMI)
remove(N15_arth_BMI_ci)
remove(N15_Arth_sex)
remove(N15_arth_sex_ci)
remove(N15_Arthritis_BMI)
remove(N15_Arthritis_sex)
remove(N15_Arthtitis_age)
remove(N15_arth_age_ci)
remove(N15_overall.c)
remove(N15_overall_ci)
remove(N15.no.age.c)
remove(N15.no.age.ci)
remove(N15.no.BMI.c)
remove(N15.no.BMI.ci)
remove(N15.no.df)
remove(N15.no.ci)
remove(N15.no.sex.c)
remove(N15.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________





#----2016----
#National Health Interview Survey
#2016

#----Download----
tempN16 <- tempfile()
download.file("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2016/samadultcsv.zip",
              tempN16)
NHIS16_SA <- read.csv(unz(tempN16, "samadult.csv"))
NHIS16_SA

#----Cleaning----
str(NHIS16_SA)
tail(NHIS16_SA)
glimpse(NHIS16_SA)
colnames(NHIS16_SA)

#Select variables
NHIS16 <- select(NHIS16_SA,
                 HHX, FMX, FPX,
                 PSTRAT, PPSU, SRVY_YR, WTFA_SA, JNTSYMP,
                 SEX, AGE_P, BMI)

str(NHIS16)
tail(NHIS16)
glimpse(NHIS16)
colnames(NHIS16)

#joint symptoms recode
table(NHIS16$JNTSYMP)
#recode
NHIS16$JNTSYMP <- recode(NHIS16$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS16$JNTSYMP <- unknownToNA(NHIS16$JNTSYMP, unknown = c("7", "9")))
table(NHIS16$JNTSYMP)
#    0     1 
#20561 12444 
NHIS16$JNTSYMP <- as.factor(NHIS16$JNTSYMP)

#age
table(NHIS16$AGE_P)
NHIS16$AGE_P <- recode(NHIS16$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS16$AGE_P)
#18 to 24     25 to 29     30 to 34     35 to 39     40 to 44     45 to 49     50 to 54     55 to 59     60 to 64     65 to 69 70 and above 
#    3024         2499         2576         2558         2271         2479         2795         2943         2981         3013         5889 
NHIS16$AGE_P <- as.factor(NHIS16$AGE_P)

#sex
NHIS16$SEX <- recode(NHIS16$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS16$SEX)
#Female   Male 
# 18037  14991 
NHIS16$SEX <- as.factor(NHIS16$SEX)


#BMI
NHIS16$BMI
#implied 2 decimal places, therefore divide all values by 100 to get the BMI value
NHIS16 <- NHIS16 %>%
  mutate(BMI = BMI/100)
#change all 99.99 (unknown values) to NA
NHIS16$BMI <- unknownToNA(NHIS16$BMI, unknown = 99.99)
#quick check to be sure that there are no more potential unknowns
NHIS16 %>% top_n(10, BMI)

#Create BMI categories
NHIS16$BMICAT <- ifelse(NHIS16$BMI < 18.50, "Underweight",
                        ifelse(NHIS16$BMI >= 18.50 & NHIS16$BMI < 25.00, "Healthy weight",
                               ifelse(NHIS16$BMI >= 25.00 & NHIS16$BMI < 30.00, "Overweight",
                                      ifelse(NHIS16$BMI >=30.00, "Obese",
                                             NA))))

table(NHIS16$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#         10573           9617          11141            566 
#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#make sure all complex survey requirements are available for design object
NHIS16_SA_dataset <- subset(NHIS16,
                            !is.na(WTFA_SA) &
                              !is.na(PSTRAT) &
                              !is.na(PPSU))

#Check that the sum of the weights is equal to the US population
sum(NHIS16_SA_dataset$WTFA_SA)
#The sum of the weights is 245 142 225, which is acceptable

#Check the number of people (unique PSU's)
length(unique(NHIS16_SA_dataset[["PPSU"]]))
#The number of unique PSU's in the data is 111

#Check the number of unique strata
length(unique(NHIS16_SA_dataset[["PSTRAT"]]))
#The number of unique strata is 52

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS16_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~PSTRAT,
                       nest = TRUE,
                       data = NHIS16_SA_dataset)

#_______________________________________________________________________________________________


#----Analysis----

N16_overall <- svymean(~factor(JNTSYMP),
                       NHIS16_DO,
                       na.rm = TRUE)
N16_overall.c <- N16_overall %>%
  as.data.frame(.) %>%
  select(1) %>%
  setNames(c("Proportion")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_overall_ci <- confint(N16_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
N16 <- bind_cols(N16_overall.c, N16_overall_ci)
#remove JNTSYMP = 0
N16 <- N16[-c(1), ] #N16 = final proportion and 95% ci

#Overall number of people
N16.no <- svytotal(~JNTSYMP, 
                   NHIS16_DO, 
                   na.rm = TRUE, 
                   deff = FALSE)
N16.no.df <- as.data.frame(N16.no) %>%
  select(1)
#ci
N16.no.ci <- confint(N16.no) %>%
  as.data.frame(.)
#join number and ci
N16.no <- bind_cols(N16.no.df, N16.no.ci)
#remove JNTSYMP=0
N16.no <- N16.no[-c(1), ] #N16.no = final number of people with arth and 95%ci


#2. Demographic-specific analysis

#A. Arthritis & Age
N16_Arth_age <- svyby(formula = ~JNTSYMP,
                      by = ~AGE_P,
                      design = NHIS16_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N16_Arthtitis_age <- N16_Arth_age %>%
  select(1, 3) %>%
  setNames(c("Age", "Proportion")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_arth_age_ci <- confint(N16_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N16_arth_age_ci <- N16_arth_age_ci[-c(1:11), ]
#join ci and proportions
N16.Age <- bind_cols(N16_Arthtitis_age, N16_arth_age_ci) #N16.Age = final proportion and 95% ci by age group


#Number of people by age
N16.no.age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS16_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N16.no.age.c <- N16.no.age %>%
  select(1, 3) %>%
  setNames(c("Age", "Number of People")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N16.no.age.ci <- confint(N16.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 11 rows
N16.no.age.ci <- N16.no.age.ci[-c(1:11), ]
#join number and ci
N16.no.age <- bind_cols(N16.no.age.c, N16.no.age.ci)


#Age logistic regression
N16_age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS16_DO)
summary(N16_age_glm)
exp(cbind(OR=coef(N16_age_glm), confint(N16_age_glm)))



#B. Arthritis & sex
N16_Arth_sex <- svyby(formula = ~JNTSYMP,
                      by = ~SEX,
                      design = NHIS16_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N16_Arthritis_sex <- N16_Arth_sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Proportion")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_arth_sex_ci <- confint(N16_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N16_arth_sex_ci <- N16_arth_sex_ci[-c(1:2), ]
#join ci and proportions
N16.Sex <- bind_cols(N16_Arthritis_sex, N16_arth_sex_ci) #N16.Sex = final proportion and 95% ci by sex


#Number of people by sex
N16.no.sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS16_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N16.no.sex.c <- N16.no.sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Number of People")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N16.no.sex.ci <- confint(N16.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 2 rows
N16.no.sex.ci <- N16.no.sex.ci[-c(1:2), ]
#join number and ci
N16.no.sex <- bind_cols(N16.no.sex.c, N16.no.sex.ci)


#Sex logistic regression
N16_sex_glm <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                      family = quasibinomial,
                      design = NHIS16_DO)
summary(N16_sex_glm)
exp(cbind(OR=coef(N16_sex_glm), confint(N16_sex_glm)))


#C. Arthritis & BMI
N16_Arth_BMI <- svyby(formula = ~JNTSYMP,
                      by = ~BMICAT,
                      design = NHIS16_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N16_Arthritis_BMI <- N16_Arth_BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Proportion")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N16_arth_BMI_ci <- confint(N16_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0
N16_arth_BMI_ci <- N16_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
N16.BMI <- bind_cols(N16_Arthritis_BMI, N16_arth_BMI_ci) #N16.BMI = final proportion and 95%ci by BMI


#Number of people by BMI
N16.no.BMI <- svyby(formula = ~JNTSYMP,
                    by = ~BMICAT,
                    design = NHIS16_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
N16.no.BMI.c <- N16.no.BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Number of People")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N16.no.BMI.ci <- confint(N16.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 4 rows
N16.no.BMI.ci <- N16.no.BMI.ci[-c(1:4), ]
#join number and ci
N16.no.BMI <- bind_cols(N16.no.BMI.c, N16.no.BMI.ci)


#BMI logistic regression (using original continuous variable from survey)
N16.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS16_DO)
summary(N16.BMI.glm)
exp(cbind(OR=coef(N16.BMI.glm), confint(N16.BMI.glm)))


#   End of 2016 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(NHIS16)
remove(NHIS16_SA_dataset)
remove(NHIS16_DO)
remove(NHIS16_SA)
remove(N16_Arth_age)
remove(N16_arth_age_ci)
remove(N16_Arth_BMI)
remove(N16_arth_BMI_ci)
remove(N16_Arth_sex)
remove(N16_arth_sex_ci)
remove(N16_Arthritis_BMI)
remove(N16_Arthritis_sex)
remove(N16_Arthtitis_age)
remove(N16_arth_age_ci)
remove(N16_overall.c)
remove(N16_overall_ci)
remove(N16.no.age.c)
remove(N16.no.age.ci)
remove(N16.no.BMI.c)
remove(N16.no.BMI.ci)
remove(N16.no.df)
remove(N16.no.ci)
remove(N16.no.sex.c)
remove(N16.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________


#----2017----
#National Health Interview Survey
#2017

#----Download----
tempn17 <- tempfile()
download.file("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2017/samadultcsv.zip",
              tempn17)
NHIS17_SA <- read.csv(unz(tempn17, "samadult.csv"))
NHIS17_SA



#----Cleaning----
str(NHIS17_SA)
tail(NHIS17_SA)
glimpse(NHIS17_SA)
colnames(NHIS17_SA)

#Select variables
NHIS17 <- select(NHIS17_SA,
                 HHX, FMX, FPX,
                 PSTRAT, PPSU, SRVY_YR, WTFA_SA, JNTSYMP,
                 SEX, AGE_P, BMI)

str(NHIS17)
tail(NHIS17)
glimpse(NHIS17)
colnames(NHIS17)

#joint symptoms recode
table(NHIS17$JNTSYMP)
#recode
NHIS17$JNTSYMP <- recode(NHIS17$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS17$JNTSYMP <- unknownToNA(NHIS17$JNTSYMP, unknown = c("7", "9")))
table(NHIS17$JNTSYMP)
#    0     1 
#16594 10129 
NHIS17$JNTSYMP <- as.factor(NHIS17$JNTSYMP)

#age
table(NHIS17$AGE_P)
NHIS17$AGE_P <- recode(NHIS17$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS17$AGE_P)
#18 to 24     25 to 29     30 to 34     35 to 39     40 to 44     45 to 49     50 to 54     55 to 59     60 to 64     65 to 69 70 and above 
#    2328         2045         2136         2060         1838         2027         2212         2317         2445         2336         4998 
NHIS17$AGE_P <- as.factor(NHIS17$AGE_P)

#sex
NHIS17$SEX <- recode(NHIS17$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS17$SEX)
#Female   Male 
# 14646  12096 
NHIS17$SEX <- as.factor(NHIS17$SEX)


#BMI
NHIS17$BMI
#implied 2 decimal places, therefore divide all values by 100 to get the BMI value
NHIS17 <- NHIS17 %>%
  mutate(BMI = BMI/100)
#change all 99.99 (unknown values) to NA
NHIS17$BMI <- unknownToNA(NHIS17$BMI, unknown = 99.99)
#quick check to be sure that there are no more potential unknowns
NHIS17 %>% top_n(10, BMI)

#Create BMI categories
NHIS17$BMICAT <- ifelse(NHIS17$BMI < 18.50, "Underweight",
                        ifelse(NHIS17$BMI >= 18.50 & NHIS17$BMI < 25.00, "Healthy weight",
                               ifelse(NHIS17$BMI >= 25.00 & NHIS17$BMI < 30.00, "Overweight",
                                      ifelse(NHIS17$BMI >=30.00, "Obese",
                                             NA))))

table(NHIS17$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#          8519           8016           8849            434 
#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#make sure all complex survey requirements are available for design object
NHIS17_SA_dataset <- subset(NHIS17,
                            !is.na(WTFA_SA) &
                              !is.na(PSTRAT) &
                              !is.na(PPSU))

#Check that the sum of the weights is equal to the US population
sum(NHIS17_SA_dataset$WTFA_SA)
#The sum of the weights is 246 657 271, which is acceptable

#Check the number of people (unique PSU's)
length(unique(NHIS17_SA_dataset[["PPSU"]]))
#The number of unique PSU's in the data is 100

#Check the number of unique strata
length(unique(NHIS17_SA_dataset[["PSTRAT"]]))
#The number of unique strata is 52

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS17_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~PSTRAT,
                       nest = TRUE,
                       data = NHIS17_SA_dataset)
#_______________________________________________________________________________________________


#----Analysis----

N17_overall <- svymean(~factor(JNTSYMP),
                       NHIS17_DO,
                       na.rm = TRUE)
N17_overall.c <- N17_overall %>%
  as.data.frame(.) %>%
  select(1) %>%
  setNames(c("Proportion")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_overall_ci <- confint(N17_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
N17 <- bind_cols(N17_overall.c, N17_overall_ci)
#remove JNTSYMP = 0
N17 <- N17[-c(1), ] #N17 = final proportion and 95% ci

#Overall number of people
N17.no <- svytotal(~JNTSYMP, 
                   NHIS17_DO, 
                   na.rm = TRUE, 
                   deff = FALSE)
N17.no.df <- as.data.frame(N17.no) %>%
  select(1)
#ci
N17.no.ci <- confint(N17.no) %>%
  as.data.frame(.)
#join number and ci
N17.no <- bind_cols(N17.no.df, N17.no.ci)
#remove JNTSYMP=0
N17.no <- N17.no[-c(1), ] #N17.no = final number of people with arth and 95%ci


#2. Demographic-specific analysis

#A. Arthritis & Age
N17_Arth_age <- svyby(formula = ~JNTSYMP,
                      by = ~AGE_P,
                      design = NHIS17_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N17_Arthtitis_age <- N17_Arth_age %>%
  select(1, 3) %>%
  setNames(c("Age", "Proportion")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_arth_age_ci <- confint(N17_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N17_arth_age_ci <- N17_arth_age_ci[-c(1:11), ]
#join ci and proportions
N17.Age <- bind_cols(N17_Arthtitis_age, N17_arth_age_ci) #N17.Age = final proportion and 95% ci by age group


#Number of people by age
N17.no.age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS17_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N17.no.age.c <- N17.no.age %>%
  select(1, 3) %>%
  setNames(c("Age", "Number of People")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N17.no.age.ci <- confint(N17.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 11 rows
N17.no.age.ci <- N17.no.age.ci[-c(1:11), ]
#join number and ci
N17.no.age <- bind_cols(N17.no.age.c, N17.no.age.ci)


#Age logistic regression
N17_age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS17_DO)
summary(N17_age_glm)
exp(cbind(OR=coef(N17_age_glm), confint(N17_age_glm)))



#B. Arthritis & sex
N17_Arth_sex <- svyby(formula = ~JNTSYMP,
                      by = ~SEX,
                      design = NHIS17_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N17_Arthritis_sex <- N17_Arth_sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Proportion")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_arth_sex_ci <- confint(N17_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N17_arth_sex_ci <- N17_arth_sex_ci[-c(1:2), ]
#join ci and proportions
N17.Sex <- bind_cols(N17_Arthritis_sex, N17_arth_sex_ci) #N17.Sex = final proportion and 95% ci by sex


#Number of people by sex
N17.no.sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS17_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N17.no.sex.c <- N17.no.sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Number of People")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N17.no.sex.ci <- confint(N17.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 2 rows
N17.no.sex.ci <- N17.no.sex.ci[-c(1:2), ]
#join number and ci
N17.no.sex <- bind_cols(N17.no.sex.c, N17.no.sex.ci)


#Sex logistic regression
N17_sex_glm <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                      family = quasibinomial,
                      design = NHIS17_DO)
summary(N17_sex_glm)
exp(cbind(OR=coef(N17_sex_glm), confint(N17_sex_glm)))


#C. Arthritis & BMI
N17_Arth_BMI <- svyby(formula = ~JNTSYMP,
                      by = ~BMICAT,
                      design = NHIS17_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N17_Arthritis_BMI <- N17_Arth_BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Proportion")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N17_arth_BMI_ci <- confint(N17_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0
N17_arth_BMI_ci <- N17_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
N17.BMI <- bind_cols(N17_Arthritis_BMI, N17_arth_BMI_ci) #N17.BMI = final proportion and 95%ci by BMI


#Number of people by BMI
N17.no.BMI <- svyby(formula = ~JNTSYMP,
                    by = ~BMICAT,
                    design = NHIS17_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
N17.no.BMI.c <- N17.no.BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Number of People")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N17.no.BMI.ci <- confint(N17.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 4 rows
N17.no.BMI.ci <- N17.no.BMI.ci[-c(1:4), ]
#join number and ci
N17.no.BMI <- bind_cols(N17.no.BMI.c, N17.no.BMI.ci)


#BMI logistic regression (using original continuous variable from survey)
N17.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS17_DO)
summary(N17.BMI.glm)
exp(cbind(OR=coef(N17.BMI.glm), confint(N17.BMI.glm)))


#   End of 2017 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(NHIS17)
remove(NHIS17_SA_dataset)
remove(NHIS17_DO)
remove(NHIS17_SA)
remove(N17_Arth_age)
remove(N17_arth_age_ci)
remove(N17_Arth_BMI)
remove(N17_arth_BMI_ci)
remove(N17_Arth_sex)
remove(N17_arth_sex_ci)
remove(N17_Arthritis_BMI)
remove(N17_Arthritis_sex)
remove(N17_Arthtitis_age)
remove(N17_arth_age_ci)
remove(N17_overall.c)
remove(N17_overall_ci)
remove(N17.no.age.c)
remove(N17.no.age.ci)
remove(N17.no.BMI.c)
remove(N17.no.BMI.ci)
remove(N17.no.df)
remove(N17.no.ci)
remove(N17.no.sex.c)
remove(N17.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________



#----2018----
#National Health Interview Survey
#2018

#----Download----
tempn18 <- tempfile()
download.file("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NHIS/2018/samadultcsv.zip",
              tempn18)
NHIS18_SA <- read.csv(unz(tempn18, "samadult.csv"))
unlink(tempn18)
NHIS18_SA



#----Cleaning----
str(NHIS18_SA)
tail(NHIS18_SA)
glimpse(NHIS18_SA)
colnames(NHIS18_SA)

#Select variables
NHIS18 <- select(NHIS18_SA,
                 HHX, FMX, FPX,
                 PSTRAT, PPSU, SRVY_YR, WTFA_SA, JNTSYMP,
                 SEX, AGE_P, BMI)

str(NHIS18)
tail(NHIS18)
glimpse(NHIS18)
colnames(NHIS18)

#joint symptoms recode
table(NHIS18$JNTSYMP)
#recode
NHIS18$JNTSYMP <- recode(NHIS18$JNTSYMP,
                         "1" = "1",
                         "2" = "0",
                         "7" = "7",
                         "9" = "9")
(NHIS18$JNTSYMP <- unknownToNA(NHIS18$JNTSYMP, unknown = c("7", "9")))
table(NHIS18$JNTSYMP)
#    0     1 
#15736  9661 
NHIS18$JNTSYMP <- as.factor(NHIS18$JNTSYMP)

#age
table(NHIS18$AGE_P)
NHIS18$AGE_P <- recode(NHIS18$AGE_P,
                       "18" = "18 to 24",
                       "19" = "18 to 24",
                       "20" = "18 to 24",
                       "21" = "18 to 24",
                       "22" = "18 to 24",
                       "23" = "18 to 24",
                       "24" = "18 to 24",
                       "25" = "25 to 29",
                       "26" = "25 to 29",
                       "27" = "25 to 29",
                       "28" = "25 to 29",
                       "29" = "25 to 29",
                       "30" = "30 to 34",
                       "31" = "30 to 34",
                       "32" = "30 to 34",
                       "33" = "30 to 34",
                       "34" = "30 to 34",
                       "35" = "35 to 39",
                       "36" = "35 to 39",
                       "37" = "35 to 39",
                       "38" = "35 to 39", 
                       "39" = "35 to 39",
                       "40" = "40 to 44",
                       "41" = "40 to 44",
                       "42" = "40 to 44",
                       "43" = "40 to 44",
                       "44" = "40 to 44",
                       "45" = "45 to 49",
                       "46" = "45 to 49",
                       "47" = "45 to 49",
                       "48" = "45 to 49",
                       "49" = "45 to 49",
                       "50" = "50 to 54",
                       "51" = "50 to 54",
                       "52" = "50 to 54",
                       "53" = "50 to 54",
                       "54" = "50 to 54",
                       "55" = "55 to 59",
                       "56" = "55 to 59",
                       "57" = "55 to 59",
                       "58" = "55 to 59",
                       "59" = "55 to 59",
                       "60" = "60 to 64",
                       "61" = "60 to 64",
                       "62" = "60 to 64",
                       "63" = "60 to 64",
                       "64" = "60 to 64",
                       "65" = "65 to 69",
                       "66" = "65 to 69",
                       "67" = "65 to 69",
                       "68" = "65 to 69",
                       "69" = "65 to 69",
                       "70" = "70 and above",
                       "71" = "70 and above",
                       "72" = "70 and above",
                       "73" = "70 and above",
                       "74" = "70 and above",
                       "75" = "70 and above",
                       "76" = "70 and above",
                       "77" = "70 and above",
                       "78" = "70 and above",
                       "79" = "70 and above",
                       "80" = "70 and above",
                       "81" = "70 and above",
                       "82" = "70 and above",
                       "83" = "70 and above",
                       "84" = "70 and above",
                       "85" = "70 and above")
table(NHIS18$AGE_P)
#18 to 24     25 to 29     30 to 34     35 to 39     40 to 44     45 to 49     50 to 54     55 to 59     60 to 64     65 to 69 70 and above 
#    1857         1867         2038         1995         1912         1859         1965         2253         2374         2358         4939 
NHIS18$AGE_P <- as.factor(NHIS18$AGE_P)

#sex
NHIS18$SEX <- recode(NHIS18$SEX,
                     "1" = "Male",
                     "2" = "Female")
table(NHIS18$SEX)
#Female   Male 
# 13867  11550
NHIS18$SEX <- as.factor(NHIS18$SEX)


#BMI
NHIS18$BMI
#implied 2 decimal places, therefore divide all values by 100 to get the BMI value
NHIS18 <- NHIS18 %>%
  mutate(BMI = BMI/100)
#change all 99.99 (unknown values) to NA
NHIS18$BMI <- unknownToNA(NHIS18$BMI, unknown = 99.99)
#quick check to be sure that there are no more potential unknowns
NHIS18 %>% top_n(10, BMI)

#Create BMI categories
NHIS18$BMICAT <- ifelse(NHIS18$BMI < 18.50, "Underweight",
                        ifelse(NHIS18$BMI >= 18.50 & NHIS18$BMI < 25.00, "Healthy weight",
                               ifelse(NHIS18$BMI >= 25.00 & NHIS18$BMI < 30.00, "Overweight",
                                      ifelse(NHIS18$BMI >=30.00, "Obese",
                                             NA))))

table(NHIS18$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#          7980           7698           8551            400 

#________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#make sure all complex survey requirements are available for design object
NHIS18_SA_dataset <- subset(NHIS18,
                            !is.na(WTFA_SA) &
                              !is.na(PSTRAT) &
                              !is.na(PPSU))

#Check that the sum of the weights is equal to the US population
sum(NHIS18_SA_dataset$WTFA_SA)
#The sum of the weights is 249 455 533, which is acceptable

#Check the number of people (unique PSU's)
length(unique(NHIS18_SA_dataset[["PPSU"]]))
#The number of unique PSU's in the data is 100

#Check the number of unique strata
length(unique(NHIS18_SA_dataset[["PSTRAT"]]))
#The number of unique strata is 52

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHIS18_DO <- svydesign(ids = ~1,
                       weights = ~WTFA_SA,
                       strata = ~PSTRAT,
                       nest = TRUE,
                       data = NHIS18_SA_dataset)


#_______________________________________________________________________________________________


#----Analysis----

N18_overall <- svymean(~factor(JNTSYMP),
                       NHIS18_DO,
                       na.rm = TRUE)
N18_overall.c <- N18_overall %>%
  as.data.frame(.) %>%
  select(1) %>%
  setNames(c("Proportion")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_overall_ci <- confint(N18_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
N18 <- bind_cols(N18_overall.c, N18_overall_ci)
#remove JNTSYMP = 0
N18 <- N18[-c(1), ] #N18 = final proportion and 95% ci

#Overall number of people
N18.no <- svytotal(~JNTSYMP, 
                   NHIS18_DO, 
                   na.rm = TRUE, 
                   deff = FALSE)
N18.no.df <- as.data.frame(N18.no) %>%
  select(1)
#ci
N18.no.ci <- confint(N18.no) %>%
  as.data.frame(.)
#join number and ci
N18.no <- bind_cols(N18.no.df, N18.no.ci)
#remove JNTSYMP=0
N18.no <- N18.no[-c(1), ] #N18.no = final number of people with arth and 95%ci


#2. Demographic-specific analysis

#A. Arthritis & Age
N18_Arth_age <- svyby(formula = ~JNTSYMP,
                      by = ~AGE_P,
                      design = NHIS18_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N18_Arthtitis_age <- N18_Arth_age %>%
  select(1, 3) %>%
  setNames(c("Age", "Proportion")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_arth_age_ci <- confint(N18_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N18_arth_age_ci <- N18_arth_age_ci[-c(1:11), ]
#join ci and proportions
N18.Age <- bind_cols(N18_Arthtitis_age, N18_arth_age_ci) #N18.Age = final proportion and 95% ci by age group


#Number of people by age
N18.no.age <- svyby(formula = ~JNTSYMP,
                    by = ~AGE_P,
                    design = NHIS18_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N18.no.age.c <- N18.no.age %>%
  select(1, 3) %>%
  setNames(c("Age", "Number of People")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N18.no.age.ci <- confint(N18.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 11 rows
N18.no.age.ci <- N18.no.age.ci[-c(1:11), ]
#join number and ci
N18.no.age <- bind_cols(N18.no.age.c, N18.no.age.ci)


#Age logistic regression
N18_age_glm <- svyglm(JNTSYMP~AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS18_DO)
summary(N18_age_glm)
exp(cbind(OR=coef(N18_age_glm), confint(N18_age_glm)))



#B. Arthritis & sex
N18_Arth_sex <- svyby(formula = ~JNTSYMP,
                      by = ~SEX,
                      design = NHIS18_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N18_Arthritis_sex <- N18_Arth_sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Proportion")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_arth_sex_ci <- confint(N18_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No)
N18_arth_sex_ci <- N18_arth_sex_ci[-c(1:2), ]
#join ci and proportions
N18.Sex <- bind_cols(N18_Arthritis_sex, N18_arth_sex_ci) #N18.Sex = final proportion and 95% ci by sex


#Number of people by sex
N18.no.sex <- svyby(formula = ~JNTSYMP,
                    by = ~SEX,
                    design = NHIS18_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = FALSE)
N18.no.sex.c <- N18.no.sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Number of People")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N18.no.sex.ci <- confint(N18.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 2 rows
N18.no.sex.ci <- N18.no.sex.ci[-c(1:2), ]
#join number and ci
N18.no.sex <- bind_cols(N18.no.sex.c, N18.no.sex.ci)


#Sex logistic regression
N18_sex_glm <- svyglm(JNTSYMP~relevel(SEX, ref = "Male") + AGE_P,
                      family = quasibinomial,
                      design = NHIS18_DO)
summary(N18_sex_glm)
exp(cbind(OR=coef(N18_sex_glm), confint(N18_sex_glm)))


#C. Arthritis & BMI
N18_Arth_BMI <- svyby(formula = ~JNTSYMP,
                      by = ~BMICAT,
                      design = NHIS18_DO,
                      FUN = svymean,
                      na.rm = TRUE)
N18_Arthritis_BMI <- N18_Arth_BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Proportion")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
N18_arth_BMI_ci <- confint(N18_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0
N18_arth_BMI_ci <- N18_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
N18.BMI <- bind_cols(N18_Arthritis_BMI, N18_arth_BMI_ci) #N18.BMI = final proportion and 95%ci by BMI


#Number of people by BMI
N18.no.BMI <- svyby(formula = ~JNTSYMP,
                    by = ~BMICAT,
                    design = NHIS18_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
N18.no.BMI.c <- N18.no.BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Number of People")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
N18.no.BMI.ci <- confint(N18.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for JNTSYMP = 0 (No), which are the first 4 rows
N18.no.BMI.ci <- N18.no.BMI.ci[-c(1:4), ]
#join number and ci
N18.no.BMI <- bind_cols(N18.no.BMI.c, N18.no.BMI.ci)


#BMI logistic regression (using original continuous variable from survey)
N18.BMI.glm <- svyglm(JNTSYMP ~ BMI + AGE_P + SEX,
                      family = quasibinomial,
                      design = NHIS18_DO)
summary(N18.BMI.glm)
exp(cbind(OR=coef(N18.BMI.glm), confint(N18.BMI.glm)))


#   End of 2018 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(NHIS18)
remove(NHIS18_SA_dataset)
remove(NHIS18_DO)
remove(NHIS18_SA)
remove(N18_Arth_age)
remove(N18_arth_age_ci)
remove(N18_Arth_BMI)
remove(N18_arth_BMI_ci)
remove(N18_Arth_sex)
remove(N18_arth_sex_ci)
remove(N18_Arthritis_BMI)
remove(N18_Arthritis_sex)
remove(N18_Arthtitis_age)
remove(N18_arth_age_ci)
remove(N18_overall.c)
remove(N18_overall_ci)
remove(N18.no.age.c)
remove(N18.no.age.ci)
remove(N18.no.BMI.c)
remove(N18.no.BMI.ci)
remove(N18.no.df)
remove(N18.no.ci)
remove(N18.no.sex.c)
remove(N18.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________