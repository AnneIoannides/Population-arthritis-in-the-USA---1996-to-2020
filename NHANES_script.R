#Ioannides AE, Wadley AL, Kamerman PR
#Arthritis in the USA: a longitudinal analysis of three nationally representative studies

# -- National Health and Nutrition Examination Survey (NHANES) Continuous analysis --
#Cycles: 1999/2000, 2001/02, 2003/04, 2005/06, 2007/08, 2009/10, 2011/12, 2013/14, 2015/16, 2017/18

#Notes:
#The term "Arthritis", in the context of the NHANES Continuous, is used to describe a diagnosis of arthritis by a healthcare professional

#Load packages
library(haven)
library(tidyverse)
library(survey)
library(gdata)
library(ggplot2)

options(survey.lonely.psu = "adjust")

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________


#----1999/2000----
#National Health and Nutrition Examination Survey (Continuous)
#1999-2000

#----Download----

##Demographic data (with sampling weights)
NHANES_Demo_9900 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DEMO.XPT"))

##Medical conditions
NHANES_MCQ_9900 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/MCQ.XPT"))

##Body examination (physical exam)
NHANES_BMX_9900 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/BMX.XPT"))

#_______________________________________________________________________________________________

#----Cleaning----

#select relevant variables
NHANES_Demo_9900 <- select(NHANES_Demo_9900, "SEQN", "RIAGENDR", "RIDAGEYR", "WTINT2YR", "SDMVPSU", "SDMVSTRA")
NHANES_MCQ_9900 <- select(NHANES_MCQ_9900, "SEQN", "MCQ160A")
NHANES_BMX_9900 <- select(NHANES_BMX_9900, "SEQN", "BMXBMI")

#Merge the individual datasets by 'seqn'
NHANES_9900a <- merge(NHANES_Demo_9900, NHANES_MCQ_9900)
NHANES9900 <- merge(NHANES_9900a, NHANES_BMX_9900)

#Select only age demographic asked about arthritis
NHANES9900 <- NHANES9900[NHANES9900$RIDAGEYR >= 20, ]

#Observe the dataset
str(NHANES9900)
tail(NHANES9900)
glimpse(NHANES9900)

#Check to see the extent of missing there is missing data in the file
colSums(is.na(NHANES9900))
which(colSums(is.na(NHANES9900)) == nrow(NHANES9900))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
NHANES9900$MCQ160A <- recode(NHANES9900$MCQ160A,
                             "1" = "1",
                             "2" = "0",
                             "7" = "7",
                             "9" = "9")
#Change unknown (7) and refused (9) values to NA
(NHANES9900$MCQ160A <- unknownToNA(NHANES9900$MCQ160A, unknown = c("7", "9")))
table(NHANES9900$MCQ160A)
#   0    1 
#3334 1104 
NHANES9900$MCQ160A <- as.factor(NHANES9900$MCQ160A)
class(NHANES9900$MCQ160A)


#Recode gender variable
NHANES9900$RIAGENDR <- recode(NHANES9900$RIAGENDR,
                              "1" = "Male",
                              "2" = "Female")
table(NHANES9900$RIAGENDR)
#Female   Male 
# 2370   2074
NHANES9900$RIAGENDR <- as.factor(NHANES9900$RIAGENDR)

#Recode age into categories
NHANES9900$RIDAGEYR <- recode(NHANES9900$RIDAGEYR,
                              "20" = "20 to 24",
                              "21" = "20 to 24",
                              "22" = "20 to 24",
                              "23" = "20 to 24",
                              "24" = "20 to 24",
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
table(NHANES9900$RIDAGEYR)
#    20 to 24     25 to 29     30 to 34     35 to 39     40 to 44     45 to 49     50 to 54     55 to 59     60 to 64     65 to 69 70 and above 
#         413          385          398          373          394          325          319          231          410          348          848 
NHANES9900$RIDAGEYR <- as.factor(NHANES9900$RIDAGEYR)

#Check class on BMI numeric
NHANES9900$BMXBMI
class(NHANES9900$BMXBMI)

#Create another BMI variable using BMI categories for figures
NHANES9900$BMICAT <- ifelse(NHANES9900$BMXBMI < 18.5, "Underweight",
                     ifelse(NHANES9900$BMXBMI >= 18.5 & NHANES9900$BMXBMI < 25.0, "Healthy weight",
                     ifelse(NHANES9900$BMXBMI >= 25.0 & NHANES9900$BMXBMI < 30.0, "Overweight",
                     ifelse(NHANES9900$BMXBMI >= 30.0, "Obese",
                     NA))))

table(NHANES9900$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#          1344           1422           1539             69 

#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
NHANES_9900_dataset <- subset(NHANES9900,
                              !is.na(WTINT2YR) &
                                !is.na(SDMVPSU) &
                                !is.na(SDMVSTRA) &
                                !is.na(MCQ160A))

#Check that the sum of the weights is equal to the US population
sum(NHANES_9900_dataset$WTINT2YR)
#The sum of the weights is 174 783 278, which is acceptable

#Check the number of unique PSUs
length(unique(NHANES_9900_dataset[["SDMVPSU"]]))
#3

#Check the number of unique strata
length(unique(NHANES_9900_dataset[["SDMVSTRA"]]))
#The number of unique strata is 13

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHANES_9900_DO <- svydesign(ids = ~1,
                            weights = ~WTINT2YR,
                            strata = ~SDMVSTRA,
                            nest = TRUE,
                            data = NHANES_9900_dataset)
#Observe the design oject
NHANES_9900_DO


#_______________________________________________________________________________________________

#----Analysis----

#1. Overall prevalence
NA_9900 <- svymean(~factor(MCQ160A), 
                   NHANES_9900_DO, 
                   na.rm = TRUE)
NA9900.c <- NA_9900 %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA9900_ci <- confint(NA_9900) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
NA9900 <- bind_cols(NA9900.c, NA9900_ci)
#remove js = 0
NA9900 <- NA9900[-c(1), ] %>%
  select(1,3,4) #final proportion& 95% ci

#Overall number of people
NA9900.no <- svytotal(~MCQ160A,
                      NHANES_9900_DO,
                      na.rm = TRUE,
                      deff = TRUE)
NA9900.no.df <- as.data.frame(NA9900.no) %>%
  setNames(c("Number of People", "SE", "DEFF")) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA9900.no.ci <- confint(NA9900.no) %>%
  as.data.frame(.) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp")) %>%
  mutate(`CI_Prop_low` = as.numeric(`CI_Prop_low`)) %>%
  mutate(`CI_Prop_upp` = as.numeric(`CI_Prop_upp`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci
NA9900.no <- bind_cols(NA9900.no.df, NA9900.no.ci)
NA9900.no <- NA9900.no[c(1,4,5)]
#remove arth = 0
NA9900.no <- NA9900.no[-c(1), ] #NA9900.no = final number of people with arth and 95%ci

#2. Demographic analysis

#A. Arthritis & Age
NA9900_Arth_age <- svyby(formula = ~MCQ160A,
                      by = ~RIDAGEYR,
                      design = NHANES_9900_DO,
                      FUN = svymean,
                      na.rm = TRUE)
NA9900_Arthritis_age <- NA9900_Arth_age %>%
  select(1,3) %>%
  setNames(c("Age", "Proportion")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA9900_arth_age_ci <- confint(NA9900_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA9900_arth_age_ci <- NA9900_arth_age_ci[-c(1:11), ]
#join ci and proportions
NA9900.Age <- bind_cols(NA9900_Arthritis_age, NA9900_arth_age_ci) #NA9900_Age = final proportion and 95% ci by age group

#Number of people by age
NA9900.no.age <- svyby(formula = ~MCQ160A,
                 by = ~RIDAGEYR,
                 design = NHANES_9900_DO,
                 FUN = svytotal,
                 na.rm = TRUE,
                 deff = FALSE)
NA9900.no.age.c <- NA9900.no.age %>%
  select(1, 3) %>%
  setNames(c("Age", "Number of People")) 
A9900.no.age.c <- NA9900.no.age.c %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA9900.no.age.ci <- confint(NA9900.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA9900.no.age.ci <- NA9900.no.age.ci[-c(1:11), ]
NA9900.no.age.ci <- NA9900.no.age.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA9900.no.age <- bind_cols(NA9900.no.age.c, NA9900.no.age.ci)


#Age logistic regression
NA9900_age_glm <- svyglm(MCQ160A~RIDAGEYR + RIAGENDR,
                         family = quasibinomial,
                         design = NHANES_9900_DO)
summary(NA9900_age_glm)
exp(cbind(OR=coef(NA9900_age_glm), confint(NA9900_age_glm)))



#B. Arthritis & Sex
NA9900_Arth_sex <- svyby(formula = ~MCQ160A,
                         by = ~RIAGENDR,
                         design = NHANES_9900_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA9900_Arthritis_sex <- NA9900_Arth_sex %>%
  select(1,3) %>%
  setNames(c("Sex", "Proportion")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA9900_arth_sex_ci <- confint(NA9900_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA9900_arth_sex_ci <- NA9900_arth_sex_ci[-c(1:2), ]
#join ci and proportions
NA9900.Sex <- bind_cols(NA9900_Arthritis_sex, NA9900_arth_sex_ci) #NA9900_Sex = final proportion and 95% ci by sex

#Number of people by sex
NA9900.no.sex <- svyby(formula = ~MCQ160A,
                       by = ~RIAGENDR,
                       design = NHANES_9900_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA9900.no.sex.c <- NA9900.no.sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Number of People")) 
NA9900.no.sex.c <- NA9900.no.sex.c %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA9900.no.sex.ci <- confint(NA9900.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA9900.no.sex.ci <- NA9900.no.sex.ci[-c(1:2), ]
NA9900.no.sex.ci <- NA9900.no.sex.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA9900.no.sex <- bind_cols(NA9900.no.sex.c, NA9900.no.sex.ci)


#Sex logistic regression
NA9900_sex_glm <- svyglm(MCQ160A~RIAGENDR + RIDAGEYR,
                         family = quasibinomial,
                         design = NHANES_9900_DO)
summary(NA9900_sex_glm)
exp(cbind(OR=coef(NA9900_sex_glm), confint(NA9900_sex_glm)))


#C. Arthritis & BMI
NA9900_Arth_BMI <- svyby(formula = ~MCQ160A,
                         by = ~BMICAT,
                         design = NHANES_9900_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA9900_Arthritis_BMI <- NA9900_Arth_BMI %>%
  select(1,3) %>%
  setNames(c("BMI", "Proportion")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA9900_arth_BMI_ci <- confint(NA9900_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA9900_arth_BMI_ci <- NA9900_arth_BMI_ci[-c(1:4), ]
#join ci and proportions
NA9900.BMI <- bind_cols(NA9900_Arthritis_BMI, NA9900_arth_BMI_ci) #NA9900_BMI = final proportion and 95% ci by BMI group

#Number of people by BMI
NA9900.no.BMI <- svyby(formula = ~MCQ160A,
                       by = ~BMICAT,
                       design = NHANES_9900_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA9900.no.BMI.c <- NA9900.no.BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Number of People")) 
NA9900.no.BMI.c <- NA9900.no.BMI.c %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA9900.no.BMI.ci <- confint(NA9900.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA9900.no.BMI.ci <- NA9900.no.BMI.ci[-c(1:4), ]
NA9900.no.BMI.ci <- NA9900.no.BMI.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA9900.no.BMI <- bind_cols(NA9900.no.BMI.c, NA9900.no.BMI.ci)

#BMI logistic regression
NA9900_BMI_glm <- svyglm(MCQ160A~BMXBMI + RIAGENDR + RIDAGEYR,
                         family = quasibinomial,
                         design = NHANES_9900_DO)
summary(NA9900_BMI_glm)
exp(cbind(OR=coef(NA9900_BMI_glm), confint(NA9900_BMI_glm)))

#   End of 1999/2000 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(NHANES9900)
remove(NHANES_9900_dataset)
remove(NHANES_9900a)
remove(NHANES_Demo_9900)
remove(NHANES_BMX_9900)
remove(NHANES_MCQ_9900)
remove(NHANES_9900_DO)
remove(NA9900)
remove(NA9900_age_glm)
remove(NA9900_Arth_age)
remove(NA9900_arth_age_ci)
remove(NA9900_Arth_BMI)
remove(NA9900_arth_BMI_ci)
remove(NA9900_Arth_sex)
remove(NA9900_arth_sex_ci)
remove(NA9900_Arthritis_age)
remove(NA9900_Arthritis_BMI)
remove(NA9900_Arthritis_sex)
remove(NA9900_BMI_glm)
remove(NA9900_ci)
remove(NA9900_sex_glm)
remove(NA9900.Age)
remove(NA9900.BMI)
remove(NA9900.Sex)
remove(NA9900.c)
remove(NA9900.no)
remove(NA9900.no.ci)
remove(NA9900.no.df)
remove(NA9900.no.age)
remove(NA9900.no.age.c)
remove(NA9900.no.age.ci)
remove(NA9900.no.BMI)
remove(NA9900.no.BMI.c)
remove(NA9900.no.BMI.ci)
remove(NA9900.no.sex)
remove(NA9900.no.sex.c)
remove(NA9900.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________




#----2001/02----
#National Health and Nutrition Examination Survey (Continuous)
#2001-2002

#----Download----

##Demographic data (with sampling weights)
NHANES_Demo_0102 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/DEMO_B.XPT"))

##Medical conditions
NHANES_MCQ_0102 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/MCQ_B.XPT"))

##Body examination (physical exam)
NHANES_BMX_0102 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/BMX_B.XPT"))

#_______________________________________________________________________________________________

#----Cleaning----

#select relevant variables
NHANES_Demo_0102 <- select(NHANES_Demo_0102, "SEQN", "RIAGENDR", "RIDAGEYR", "WTINT2YR", "SDMVPSU", "SDMVSTRA")
NHANES_MCQ_0102 <- select(NHANES_MCQ_0102, "SEQN", "MCQ160A")
NHANES_BMX_0102 <- select(NHANES_BMX_0102, "SEQN", "BMXBMI")

#Merge the individual datasets by 'seqn'
NHANES_0102a <- merge(NHANES_Demo_0102, NHANES_MCQ_0102)
NHANES0102 <- merge(NHANES_0102a, NHANES_BMX_0102)

#Select only age demographic asked about arthritis
NHANES0102 <- NHANES0102[NHANES0102$RIDAGEYR >= 20, ]

#Observe the dataset
str(NHANES0102)
tail(NHANES0102)
glimpse(NHANES0102)

#Check to see the extent of missing there is missing data in the file
colSums(is.na(NHANES0102))
which(colSums(is.na(NHANES0102)) == nrow(NHANES0102))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
NHANES0102$MCQ160A <- recode(NHANES0102$MCQ160A,
                             "1" = "1",
                             "2" = "0",
                             "7" = "7",
                             "9" = "9")
#Change unknown (7) and refused (9) values to NA
(NHANES0102$MCQ160A <- unknownToNA(NHANES0102$MCQ160A, unknown = c("7", "9")))
table(NHANES0102$MCQ160A)
#   0    1 
#3829 1194
NHANES0102$MCQ160A <- as.factor(NHANES0102$MCQ160A)
class(NHANES0102$MCQ160A)


#Recode gender variable
NHANES0102$RIAGENDR <- recode(NHANES0102$RIAGENDR,
                              "1" = "Male",
                              "2" = "Female")
table(NHANES0102$RIAGENDR)
#Female   Male 
#  2641   2386
NHANES0102$RIAGENDR <- as.factor(NHANES0102$RIAGENDR)

#Recode age into categories
NHANES0102$RIDAGEYR <- recode(NHANES0102$RIDAGEYR,
                              "20" = "20 to 24",
                              "21" = "20 to 24",
                              "22" = "20 to 24",
                              "23" = "20 to 24",
                              "24" = "20 to 24",
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
table(NHANES0102$RIDAGEYR)
#    20 to 24     25 to 29     30 to 34     35 to 39     40 to 44     45 to 49     50 to 54     55 to 59     60 to 64     65 to 69 70 and above 
#         514          458          430          441          466          425          393          272          386          309          933
NHANES0102$RIDAGEYR <- as.factor(NHANES0102$RIDAGEYR)

#Check class on BMI numeric
NHANES0102$BMXBMI
class(NHANES0102$BMXBMI)

#Create another BMI variable using BMI categories for figures
NHANES0102$BMICAT <- ifelse(NHANES0102$BMXBMI < 18.5, "Underweight",
                            ifelse(NHANES0102$BMXBMI >= 18.5 & NHANES0102$BMXBMI < 25.0, "Healthy weight",
                                   ifelse(NHANES0102$BMXBMI >= 25.0 & NHANES0102$BMXBMI < 30.0, "Overweight",
                                          ifelse(NHANES0102$BMXBMI >= 30.0, "Obese",
                                                 NA))))

table(NHANES0102$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#          1424           1432           1752             82  

#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
NHANES_0102_dataset <- subset(NHANES0102,
                              !is.na(WTINT2YR) &
                                !is.na(SDMVPSU) &
                                !is.na(SDMVSTRA) &
                                !is.na(MCQ160A))

#Check that the sum of the weights is equal to the US population
sum(NHANES_0102_dataset$WTINT2YR)
#The sum of the weights is 189 051 537, which is acceptable

#Check the number of unique PSUs
length(unique(NHANES_0102_dataset[["SDMVPSU"]]))
#2

#Check the number of unique strata
length(unique(NHANES_0102_dataset[["SDMVSTRA"]]))
#The number of unique strata is 15

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHANES_0102_DO <- svydesign(ids = ~1,
                            weights = ~WTINT2YR,
                            strata = ~SDMVSTRA,
                            nest = TRUE,
                            data = NHANES_0102_dataset)
#Observe the design oject
NHANES_0102_DO


#_______________________________________________________________________________________________

#----Analysis----

#1. Overall prevalence
NA_0102 <- svymean(~factor(MCQ160A), 
                   NHANES_0102_DO, 
                   na.rm = TRUE)
NA0102.c <- NA_0102 %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA0102_ci <- confint(NA_0102) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
NA0102 <- bind_cols(NA0102.c, NA0102_ci)
#remove js = 0
NA0102 <- NA0102[-c(1), ] %>%
  select(1,3,4) #final proportion& 95% ci #final proportion, se & 95% ci

#Overall number of people
NA0102.no <- svytotal(~MCQ160A,
                      NHANES_0102_DO,
                      na.rm = TRUE,
                      deff = TRUE)
NA0102.no.df <- as.data.frame(NA0102.no) %>%
  setNames(c("Number of People", "SE", "DEFF")) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA0102.no.ci <- confint(NA0102.no) %>%
  as.data.frame(.) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp")) %>%
  mutate(`CI_Prop_low` = as.numeric(`CI_Prop_low`)) %>%
  mutate(`CI_Prop_upp` = as.numeric(`CI_Prop_upp`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci
NA0102.no <- bind_cols(NA0102.no.df, NA0102.no.ci)
NA0102.no <- NA0102.no[c(1,4,5)]
#remove arth = 0
NA0102.no <- NA0102.no[-c(1), ] #NA0102.no = final number of people with arth and 95%ci


#2. Demographic analysis

#A. Arthritis & Age
NA0102_Arth_age <- svyby(formula = ~MCQ160A,
                         by = ~RIDAGEYR,
                         design = NHANES_0102_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA0102_Arthritis_age <- NA0102_Arth_age %>%
  select(1,3) %>%
  setNames(c("Age", "Proportion")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA0102_arth_age_ci <- confint(NA0102_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA0102_arth_age_ci <- NA0102_arth_age_ci[-c(1:11), ]
#join ci and proportions
NA0102.Age <- bind_cols(NA0102_Arthritis_age, NA0102_arth_age_ci) #NA0102_Age = final proportion and 95% ci by age group

#Number of people by age
NA0102.no.age <- svyby(formula = ~MCQ160A,
                       by = ~RIDAGEYR,
                       design = NHANES_0102_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA0102.no.age.c <- NA0102.no.age %>%
  select(1, 3) %>%
  setNames(c("Age", "Number of People")) 
NA0102.no.age.c <- NA0102.no.age.c %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA0102.no.age.ci <- confint(NA0102.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA0102.no.age.ci <- NA0102.no.age.ci[-c(1:11), ]
NA0102.no.age.ci <- NA0102.no.age.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA0102.no.age <- bind_cols(NA0102.no.age.c, NA0102.no.age.ci)

#Age logistic regression
NA0102_age_glm <- svyglm(MCQ160A~RIDAGEYR + RIAGENDR,
                         family = quasibinomial,
                         design = NHANES_0102_DO)
summary(NA0102_age_glm)
exp(cbind(OR=coef(NA0102_age_glm), confint(NA0102_age_glm)))

#B. Arthritis & Sex
NA0102_Arth_sex <- svyby(formula = ~MCQ160A,
                         by = ~RIAGENDR,
                         design = NHANES_0102_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA0102_Arthritis_sex <- NA0102_Arth_sex %>%
  select(1,3) %>%
  setNames(c("Sex", "Proportion")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA0102_arth_sex_ci <- confint(NA0102_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA0102_arth_sex_ci <- NA0102_arth_sex_ci[-c(1:2), ]
#join ci and proportions
NA0102.Sex <- bind_cols(NA0102_Arthritis_sex, NA0102_arth_sex_ci) #NA0102_Sex = final proportion and 95% ci by sex

#Number of people by sex
NA0102.no.sex <- svyby(formula = ~MCQ160A,
                       by = ~RIAGENDR,
                       design = NHANES_0102_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA0102.no.sex.c <- NA0102.no.sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Number of People")) 
NA0102.no.sex.c <- NA0102.no.sex.c %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA0102.no.sex.ci <- confint(NA0102.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA0102.no.sex.ci <- NA0102.no.sex.ci[-c(1:2), ]
NA0102.no.sex.ci <- NA0102.no.sex.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA0102.no.sex <- bind_cols(NA0102.no.sex.c, NA0102.no.sex.ci)


#Sex logistic regression
NA0102_sex_glm <- svyglm(MCQ160A~RIAGENDR + RIDAGEYR,
                         family = quasibinomial,
                         design = NHANES_0102_DO)
summary(NA0102_sex_glm)
exp(cbind(OR=coef(NA0102_sex_glm), confint(NA0102_sex_glm)))


#C. Arthritis & BMI
NA0102_Arth_BMI <- svyby(formula = ~MCQ160A,
                         by = ~BMICAT,
                         design = NHANES_0102_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA0102_Arthritis_BMI <- NA0102_Arth_BMI %>%
  select(1,3) %>%
  setNames(c("BMI", "Proportion")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA0102_arth_BMI_ci <- confint(NA0102_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA0102_arth_BMI_ci <- NA0102_arth_BMI_ci[-c(1:4), ]
#join ci and proportions
NA0102.BMI <- bind_cols(NA0102_Arthritis_BMI, NA0102_arth_BMI_ci) #NA0102_BMI = final proportion and 95% ci by BMI group

#Number of people by BMI
NA0102.no.BMI <- svyby(formula = ~MCQ160A,
                       by = ~BMICAT,
                       design = NHANES_0102_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA0102.no.BMI.c <- NA0102.no.BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Number of People")) 
NA0102.no.BMI.c <- NA0102.no.BMI.c %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA0102.no.BMI.ci <- confint(NA0102.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA0102.no.BMI.ci <- NA0102.no.BMI.ci[-c(1:4), ]
NA0102.no.BMI.ci <- NA0102.no.BMI.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA0102.no.BMI <- bind_cols(NA0102.no.BMI.c, NA0102.no.BMI.ci)

#BMI logistic regression
NA0102_BMI_glm <- svyglm(MCQ160A~BMXBMI + RIAGENDR + RIDAGEYR,
                         family = quasibinomial,
                         design = NHANES_0102_DO)
summary(NA0102_BMI_glm)
exp(cbind(OR=coef(NA0102_BMI_glm), confint(NA0102_BMI_glm)))

#   End of 2001/2002 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(NHANES0102)
remove(NHANES_0102_dataset)
remove(NHANES_0102a)
remove(NHANES_Demo_0102)
remove(NHANES_BMX_0102)
remove(NHANES_MCQ_0102)
remove(NHANES_0102_DO)
remove(NA0102)
remove(NA0102_age_glm)
remove(NA0102_Arth_age)
remove(NA0102_arth_age_ci)
remove(NA0102_Arth_BMI)
remove(NA0102_arth_BMI_ci)
remove(NA0102_Arth_sex)
remove(NA0102_arth_sex_ci)
remove(NA0102_Arthritis_age)
remove(NA0102_Arthritis_BMI)
remove(NA0102_Arthritis_sex)
remove(NA0102_BMI_glm)
remove(NA0102_ci)
remove(NA0102_sex_glm)
remove(NA0102.Age)
remove(NA0102.BMI)
remove(NA0102.Sex)
remove(NA0102.c)
remove(NA0102.no)
remove(NA0102.no.ci)
remove(NA0102.no.df)
remove(NA0102.no.age)
remove(NA0102.no.age.c)
remove(NA0102.no.age.ci)
remove(NA0102.no.BMI)
remove(NA0102.no.BMI.c)
remove(NA0102.no.BMI.ci)
remove(NA0102.no.sex)
remove(NA0102.no.sex.c)
remove(NA0102.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________



#----2003/04----
#National Health and Nutrition Examination Survey (Continuous)
#2003-2004

#----Download----

##Demographic data (with sampling weights)
NHANES_Demo_0304 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/DEMO_C.XPT"))

##Medical conditions
NHANES_MCQ_0304 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/MCQ_C.XPT"))

##Body examination (physical exam)
NHANES_BMX_0304 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/BMX_C.XPT"))

#_______________________________________________________________________________________________

#----Cleaning----

#select relevant variables
NHANES_Demo_0304 <- select(NHANES_Demo_0304, "SEQN", "RIAGENDR", "RIDAGEYR", "WTINT2YR", "SDMVPSU", "SDMVSTRA")
NHANES_MCQ_0304 <- select(NHANES_MCQ_0304, "SEQN", "MCQ160A")
NHANES_BMX_0304 <- select(NHANES_BMX_0304, "SEQN", "BMXBMI")

#Merge the individual datasets by 'seqn'
NHANES_0304a <- merge(NHANES_Demo_0304, NHANES_MCQ_0304)
NHANES0304 <- merge(NHANES_0304a, NHANES_BMX_0304)

#Select only age demographic asked about arthritis
NHANES0304 <- NHANES0304[NHANES0304$RIDAGEYR >= 20, ]

#Observe the dataset
str(NHANES0304)
tail(NHANES0304)
glimpse(NHANES0304)

#Check to see the extent of missing there is missing data in the file
colSums(is.na(NHANES0304))
which(colSums(is.na(NHANES0304)) == nrow(NHANES0304))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
NHANES0304$MCQ160A <- recode(NHANES0304$MCQ160A,
                             "1" = "1",
                             "2" = "0",
                             "7" = "7",
                             "9" = "9")
#Change unknown (7) and refused (9) values to NA
(NHANES0304$MCQ160A <- unknownToNA(NHANES0304$MCQ160A, unknown = c("7", "9")))
table(NHANES0304$MCQ160A)
#   0    1 
#3405 1323
NHANES0304$MCQ160A <- as.factor(NHANES0304$MCQ160A)
class(NHANES0304$MCQ160A)


#Recode gender variable
NHANES0304$RIAGENDR <- recode(NHANES0304$RIAGENDR,
                              "1" = "Male",
                              "2" = "Female")
table(NHANES0304$RIAGENDR)
#Female   Male 
#  2467   2275
NHANES0304$RIAGENDR <- as.factor(NHANES0304$RIAGENDR)

#Recode age into categories
NHANES0304$RIDAGEYR <- recode(NHANES0304$RIDAGEYR,
                              "20" = "20 to 24",
                              "21" = "20 to 24",
                              "22" = "20 to 24",
                              "23" = "20 to 24",
                              "24" = "20 to 24",
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
table(NHANES0304$RIDAGEYR)
#    20 to 24     25 to 29     30 to 34     35 to 39     40 to 44     45 to 49     50 to 54     55 to 59     60 to 64     65 to 69 70 and above 
#         458          415          430          353          403          356          346          231          378          345         1027 
NHANES0304$RIDAGEYR <- as.factor(NHANES0304$RIDAGEYR)

#Check class on BMI numeric
NHANES0304$BMXBMI
class(NHANES0304$BMXBMI)

#Create another BMI variable using BMI categories for figures
NHANES0304$BMICAT <- ifelse(NHANES0304$BMXBMI < 18.5, "Underweight",
                            ifelse(NHANES0304$BMXBMI >= 18.5 & NHANES0304$BMXBMI < 25.0, "Healthy weight",
                                   ifelse(NHANES0304$BMXBMI >= 25.0 & NHANES0304$BMXBMI < 30.0, "Overweight",
                                          ifelse(NHANES0304$BMXBMI >= 30.0, "Obese",
                                                 NA))))

table(NHANES0304$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#          1408           1538           1631             70 

#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
NHANES_0304_dataset <- subset(NHANES0304,
                              !is.na(WTINT2YR) &
                                !is.na(SDMVPSU) &
                                !is.na(SDMVSTRA) &
                                !is.na(MCQ160A))

#Check that the sum of the weights is equal to the US population
sum(NHANES_0304_dataset$WTINT2YR)
#The sum of the weights is 193 472 727, which is acceptable

#Check the number of unique PSUs
length(unique(NHANES_0304_dataset[["SDMVPSU"]]))
#2

#Check the number of unique strata
length(unique(NHANES_0304_dataset[["SDMVSTRA"]]))
#The number of unique strata is 15

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHANES_0304_DO <- svydesign(ids = ~1,
                            weights = ~WTINT2YR,
                            strata = ~SDMVSTRA,
                            nest = TRUE,
                            data = NHANES_0304_dataset)
#Observe the design oject
NHANES_0304_DO

#_______________________________________________________________________________________________

#----Analysis----

#1. Overall prevalence
NA_0304 <- svymean(~factor(MCQ160A), 
                   NHANES_0304_DO, 
                   na.rm = TRUE)
NA0304.c <- NA_0304 %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA0304_ci <- confint(NA_0304) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
NA0304 <- bind_cols(NA0304.c, NA0304_ci)
#remove js = 0
NA0304 <- NA0304[-c(1), ] %>%
  select(1,3,4) #final proportion& 95% ci #final proportion, se & 95% ci

#Overall number of people
NA0304.no <- svytotal(~MCQ160A,
                      NHANES_0304_DO,
                      na.rm = TRUE,
                      deff = TRUE)
NA0304.no.df <- as.data.frame(NA0304.no) %>%
  setNames(c("Number of People", "SE", "DEFF")) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA0304.no.ci <- confint(NA0304.no) %>%
  as.data.frame(.) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp")) %>%
  mutate(`CI_Prop_low` = as.numeric(`CI_Prop_low`)) %>%
  mutate(`CI_Prop_upp` = as.numeric(`CI_Prop_upp`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci
NA0304.no <- bind_cols(NA0304.no.df, NA0304.no.ci)
NA0304.no <- NA0304.no[c(1,4,5)]
#remove arth = 0
NA0304.no <- NA0304.no[-c(1), ] #NA0304.no = final number of people with arth and 95%ci


#2. Demographic analysis

#A. Arthritis & Age
NA0304_Arth_age <- svyby(formula = ~MCQ160A,
                         by = ~RIDAGEYR,
                         design = NHANES_0304_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA0304_Arthritis_age <- NA0304_Arth_age %>%
  select(1,3) %>%
  setNames(c("Age", "Proportion")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA0304_arth_age_ci <- confint(NA0304_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA0304_arth_age_ci <- NA0304_arth_age_ci[-c(1:11), ]
#join ci and proportions
NA0304.Age <- bind_cols(NA0304_Arthritis_age, NA0304_arth_age_ci) #NA0304_Age = final proportion and 95% ci by age group

#Number of people by age
NA0304.no.age <- svyby(formula = ~MCQ160A,
                       by = ~RIDAGEYR,
                       design = NHANES_0304_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA0304.no.age.c <- NA0304.no.age %>%
  select(1, 3) %>%
  setNames(c("Age", "Number of People")) 
NA0304.no.age.c <- NA0304.no.age.c %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA0304.no.age.ci <- confint(NA0304.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA0304.no.age.ci <- NA0304.no.age.ci[-c(1:11), ]
NA0304.no.age.ci <- NA0304.no.age.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA0304.no.age <- bind_cols(NA0304.no.age.c, NA0304.no.age.ci)

#Age logistic regression
NA0304_age_glm <- svyglm(MCQ160A~RIDAGEYR + RIAGENDR,
                         family = quasibinomial,
                         design = NHANES_0304_DO)
summary(NA0304_age_glm)
exp(cbind(OR=coef(NA0304_age_glm), confint(NA0304_age_glm)))


#B. Arthritis & Sex
NA0304_Arth_sex <- svyby(formula = ~MCQ160A,
                         by = ~RIAGENDR,
                         design = NHANES_0304_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA0304_Arthritis_sex <- NA0304_Arth_sex %>%
  select(1,3) %>%
  setNames(c("Sex", "Proportion")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA0304_arth_sex_ci <- confint(NA0304_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA0304_arth_sex_ci <- NA0304_arth_sex_ci[-c(1:2), ]
#join ci and proportions
NA0304.Sex <- bind_cols(NA0304_Arthritis_sex, NA0304_arth_sex_ci) #NA0304_Sex = final proportion and 95% ci by sex

#Number of people by sex
NA0304.no.sex <- svyby(formula = ~MCQ160A,
                       by = ~RIAGENDR,
                       design = NHANES_0304_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA0304.no.sex.c <- NA0304.no.sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Number of People")) 
NA0304.no.sex.c <- NA0304.no.sex.c %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA0304.no.sex.ci <- confint(NA0304.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA0304.no.sex.ci <- NA0304.no.sex.ci[-c(1:2), ]
NA0304.no.sex.ci <- NA0304.no.sex.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA0304.no.sex <- bind_cols(NA0304.no.sex.c, NA0304.no.sex.ci)

#Sex logistic regression
NA0304_sex_glm <- svyglm(MCQ160A~RIAGENDR + RIDAGEYR,
                         family = quasibinomial,
                         design = NHANES_0304_DO)
summary(NA0304_sex_glm)
exp(cbind(OR=coef(NA0304_sex_glm), confint(NA0304_sex_glm)))


#C. Arthritis & BMI
NA0304_Arth_BMI <- svyby(formula = ~MCQ160A,
                         by = ~BMICAT,
                         design = NHANES_0304_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA0304_Arthritis_BMI <- NA0304_Arth_BMI %>%
  select(1,3) %>%
  setNames(c("BMI", "Proportion")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA0304_arth_BMI_ci <- confint(NA0304_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA0304_arth_BMI_ci <- NA0304_arth_BMI_ci[-c(1:4), ]
#join ci and proportions
NA0304.BMI <- bind_cols(NA0304_Arthritis_BMI, NA0304_arth_BMI_ci) #NA0304_BMI = final proportion and 95% ci by BMI group

#Number of people by BMI
NA0304.no.BMI <- svyby(formula = ~MCQ160A,
                       by = ~BMICAT,
                       design = NHANES_0304_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA0304.no.BMI.c <- NA0304.no.BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Number of People")) 
NA0304.no.BMI.c <- NA0304.no.BMI.c %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA0304.no.BMI.ci <- confint(NA0304.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA0304.no.BMI.ci <- NA0304.no.BMI.ci[-c(1:4), ]
NA0304.no.BMI.ci <- NA0304.no.BMI.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA0304.no.BMI <- bind_cols(NA0304.no.BMI.c, NA0304.no.BMI.ci)

#BMI logistic regression
NA0304_BMI_glm <- svyglm(MCQ160A~BMXBMI + RIAGENDR + RIDAGEYR,
                         family = quasibinomial,
                         design = NHANES_0304_DO)
summary(NA0304_BMI_glm)
exp(cbind(OR=coef(NA0304_BMI_glm), confint(NA0304_BMI_glm)))

#   End of 2003/2004 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(NHANES0304)
remove(NHANES_0304_dataset)
remove(NHANES_0304a)
remove(NHANES_Demo_0304)
remove(NHANES_BMX_0304)
remove(NHANES_MCQ_0304)
remove(NHANES_0304_DO)
remove(NA0304)
remove(NA0304_age_glm)
remove(NA0304_Arth_age)
remove(NA0304_arth_age_ci)
remove(NA0304_Arth_BMI)
remove(NA0304_arth_BMI_ci)
remove(NA0304_Arth_sex)
remove(NA0304_arth_sex_ci)
remove(NA0304_Arthritis_age)
remove(NA0304_Arthritis_BMI)
remove(NA0304_Arthritis_sex)
remove(NA0304_BMI_glm)
remove(NA0304_ci)
remove(NA0304_sex_glm)
remove(NA0304.Age)
remove(NA0304.BMI)
remove(NA0304.Sex)
remove(NA0304.c)
remove(NA0304.no)
remove(NA0304.no.ci)
remove(NA0304.no.df)
remove(NA0304.no.age)
remove(NA0304.no.age.c)
remove(NA0304.no.age.ci)
remove(NA0304.no.BMI)
remove(NA0304.no.BMI.c)
remove(NA0304.no.BMI.ci)
remove(NA0304.no.sex)
remove(NA0304.no.sex.c)
remove(NA0304.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________


#----2005/06----
#National Health and Nutrition Examination Survey (Continuous)
#2005-2006

#----Download----

##Demographic data (with sampling weights)
NHANES_Demo_0506 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DEMO_D.XPT"))

##Medical conditions
NHANES_MCQ_0506 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/MCQ_D.XPT"))

##Body examination (physical exam)
NHANES_BMX_0506 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/BMX_D.XPT"))

#_______________________________________________________________________________________________

#----Cleaning----

#select relevant variables
NHANES_Demo_0506 <- select(NHANES_Demo_0506, "SEQN", "RIAGENDR", "RIDAGEYR", "WTINT2YR", "SDMVPSU", "SDMVSTRA")
NHANES_MCQ_0506 <- select(NHANES_MCQ_0506, "SEQN", "MCQ160A")
NHANES_BMX_0506 <- select(NHANES_BMX_0506, "SEQN", "BMXBMI")

#Merge the individual datasets by 'seqn'
NHANES_0506a <- merge(NHANES_Demo_0506, NHANES_MCQ_0506)
NHANES0506 <- merge(NHANES_0506a, NHANES_BMX_0506)

#Select only age demographic asked about arthritis
NHANES0506 <- NHANES0506[NHANES0506$RIDAGEYR >= 20, ]

#Observe the dataset
str(NHANES0506)
tail(NHANES0506)
glimpse(NHANES0506)

#Check to see the extent of missing there is missing data in the file
colSums(is.na(NHANES0506))
which(colSums(is.na(NHANES0506)) == nrow(NHANES0506))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
NHANES0506$MCQ160A <- recode(NHANES0506$MCQ160A,
                             "1" = "1",
                             "2" = "0",
                             "7" = "7",
                             "9" = "9")
#Change unknown (7) and refused (9) values to NA
(NHANES0506$MCQ160A <- unknownToNA(NHANES0506$MCQ160A, unknown = c("7", "9")))
table(NHANES0506$MCQ160A)
#   0    1 
#3559 1204 
NHANES0506$MCQ160A <- as.factor(NHANES0506$MCQ160A)
class(NHANES0506$MCQ160A)


#Recode gender variable
NHANES0506$RIAGENDR <- recode(NHANES0506$RIAGENDR,
                              "1" = "Male",
                              "2" = "Female")
table(NHANES0506$RIAGENDR)
#Female   Male 
#  2489   2284 
NHANES0506$RIAGENDR <- as.factor(NHANES0506$RIAGENDR)

#Recode age into categories
NHANES0506$RIDAGEYR <- recode(NHANES0506$RIDAGEYR,
                              "20" = "20 to 24",
                              "21" = "20 to 24",
                              "22" = "20 to 24",
                              "23" = "20 to 24",
                              "24" = "20 to 24",
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
table(NHANES0506$RIDAGEYR)
#    20 to 24     25 to 29     30 to 34     35 to 39     40 to 44     45 to 49     50 to 54     55 to 59     60 to 64     65 to 69 70 and above 
#         514          507          429          389          420          395          371          260          363          298          827 
NHANES0506$RIDAGEYR <- as.factor(NHANES0506$RIDAGEYR)

#Check class on BMI numeric
NHANES0506$BMXBMI
class(NHANES0506$BMXBMI)

#Create another BMI variable using BMI categories for figures
NHANES0506$BMICAT <- ifelse(NHANES0506$BMXBMI < 18.5, "Underweight",
                            ifelse(NHANES0506$BMXBMI >= 18.5 & NHANES0506$BMXBMI < 25.0, "Healthy weight",
                                   ifelse(NHANES0506$BMXBMI >= 25.0 & NHANES0506$BMXBMI < 30.0, "Overweight",
                                          ifelse(NHANES0506$BMXBMI >= 30.0, "Obese",
                                                 NA))))

table(NHANES0506$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#          1350           1647           1604             79 

#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
NHANES_0506_dataset <- subset(NHANES0506,
                              !is.na(WTINT2YR) &
                                !is.na(SDMVPSU) &
                                !is.na(SDMVSTRA) &
                                !is.na(MCQ160A))

#Check that the sum of the weights is equal to the US population
sum(NHANES_0506_dataset$WTINT2YR)
#The sum of the weights is 201 916 632, which is acceptable

#Check the number of unique PSUs
length(unique(NHANES_0506_dataset[["SDMVPSU"]]))
#2

#Check the number of unique strata
length(unique(NHANES_0506_dataset[["SDMVSTRA"]]))
#The number of unique strata is 15

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHANES_0506_DO <- svydesign(ids = ~1,
                            weights = ~WTINT2YR,
                            strata = ~SDMVSTRA,
                            nest = TRUE,
                            data = NHANES_0506_dataset)
#Observe the design oject
NHANES_0506_DO
#_______________________________________________________________________________________________

#----Analysis----

#1. Overall prevalence
NA_0506 <- svymean(~factor(MCQ160A), 
                   NHANES_0506_DO, 
                   na.rm = TRUE)
NA0506.c <- NA_0506 %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA0506_ci <- confint(NA_0506) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
NA0506 <- bind_cols(NA0506.c, NA0506_ci)
#remove js = 0
NA0506 <- NA0506[-c(1), ] %>%
  select(1,3,4) #final proportion& 95% ci #final proportion, se & 95% ci

#Overall number of people
NA0506.no <- svytotal(~MCQ160A,
                      NHANES_0506_DO,
                      na.rm = TRUE,
                      deff = TRUE)
NA0506.no.df <- as.data.frame(NA0506.no) %>%
  setNames(c("Number of People", "SE", "DEFF")) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA0506.no.ci <- confint(NA0506.no) %>%
  as.data.frame(.) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp")) %>%
  mutate(`CI_Prop_low` = as.numeric(`CI_Prop_low`)) %>%
  mutate(`CI_Prop_upp` = as.numeric(`CI_Prop_upp`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci
NA0506.no <- bind_cols(NA0506.no.df, NA0506.no.ci)
NA0506.no <- NA0506.no[c(1,4,5)]
#remove arth = 0
NA0506.no <- NA0506.no[-c(1), ] #NA0506.no = final number of people with arth and 95%ci


#2. Demographic analysis

#A. Arthritis & Age
NA0506_Arth_age <- svyby(formula = ~MCQ160A,
                         by = ~RIDAGEYR,
                         design = NHANES_0506_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA0506_Arthritis_age <- NA0506_Arth_age %>%
  select(1,3) %>%
  setNames(c("Age", "Proportion")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA0506_arth_age_ci <- confint(NA0506_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA0506_arth_age_ci <- NA0506_arth_age_ci[-c(1:11), ]
#join ci and proportions
NA0506.Age <- bind_cols(NA0506_Arthritis_age, NA0506_arth_age_ci) #NA0506_Age = final proportion and 95% ci by age group

#Number of people by age
NA0506.no.age <- svyby(formula = ~MCQ160A,
                       by = ~RIDAGEYR,
                       design = NHANES_0506_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA0506.no.age.c <- NA0506.no.age %>%
  select(1, 3) %>%
  setNames(c("Age", "Number of People")) 
NA0506.no.age.c <- NA0506.no.age.c %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA0506.no.age.ci <- confint(NA0506.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA0506.no.age.ci <- NA0506.no.age.ci[-c(1:11), ]
NA0506.no.age.ci <- NA0506.no.age.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA0506.no.age <- bind_cols(NA0506.no.age.c, NA0506.no.age.ci)

#Age logistic regression
NA0506_age_glm <- svyglm(MCQ160A~RIDAGEYR + RIAGENDR,
                         family = quasibinomial,
                         design = NHANES_0506_DO)
summary(NA0506_age_glm)
exp(cbind(OR=coef(NA0506_age_glm), confint(NA0506_age_glm)))



#B. Arthritis & Sex
NA0506_Arth_sex <- svyby(formula = ~MCQ160A,
                         by = ~RIAGENDR,
                         design = NHANES_0506_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA0506_Arthritis_sex <- NA0506_Arth_sex %>%
  select(1,3) %>%
  setNames(c("Sex", "Proportion")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA0506_arth_sex_ci <- confint(NA0506_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA0506_arth_sex_ci <- NA0506_arth_sex_ci[-c(1:2), ]
#join ci and proportions
NA0506.Sex <- bind_cols(NA0506_Arthritis_sex, NA0506_arth_sex_ci) #NA0506_Sex = final proportion and 95% ci by sex

#Number of people by sex
NA0506.no.sex <- svyby(formula = ~MCQ160A,
                       by = ~RIAGENDR,
                       design = NHANES_0506_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA0506.no.sex.c <- NA0506.no.sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Number of People")) 
NA0506.no.sex.c <- NA0506.no.sex.c %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA0506.no.sex.ci <- confint(NA0506.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA0506.no.sex.ci <- NA0506.no.sex.ci[-c(1:2), ]
NA0506.no.sex.ci <- NA0506.no.sex.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA0506.no.sex <- bind_cols(NA0506.no.sex.c, NA0506.no.sex.ci)

#Sex logistic regression
NA0506_sex_glm <- svyglm(MCQ160A~RIAGENDR + RIDAGEYR,
                         family = quasibinomial,
                         design = NHANES_0506_DO)
summary(NA0506_sex_glm)
exp(cbind(OR=coef(NA0506_sex_glm), confint(NA0506_sex_glm)))


#C. Arthritis & BMI
NA0506_Arth_BMI <- svyby(formula = ~MCQ160A,
                         by = ~BMICAT,
                         design = NHANES_0506_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA0506_Arthritis_BMI <- NA0506_Arth_BMI %>%
  select(1,3) %>%
  setNames(c("BMI", "Proportion")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA0506_arth_BMI_ci <- confint(NA0506_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA0506_arth_BMI_ci <- NA0506_arth_BMI_ci[-c(1:4), ]
#join ci and proportions
NA0506.BMI <- bind_cols(NA0506_Arthritis_BMI, NA0506_arth_BMI_ci) #NA0506_BMI = final proportion and 95% ci by BMI group


#Number of people by BMI
NA0506.no.BMI <- svyby(formula = ~MCQ160A,
                       by = ~BMICAT,
                       design = NHANES_0506_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA0506.no.BMI.c <- NA0506.no.BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Number of People")) 
NA0506.no.BMI.c <- NA0506.no.BMI.c %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA0506.no.BMI.ci <- confint(NA0506.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA0506.no.BMI.ci <- NA0506.no.BMI.ci[-c(1:4), ]
NA0506.no.BMI.ci <- NA0506.no.BMI.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA0506.no.BMI <- bind_cols(NA0506.no.BMI.c, NA0506.no.BMI.ci)

#BMI logistic regression
NA0506_BMI_glm <- svyglm(MCQ160A~BMXBMI + RIAGENDR + RIDAGEYR,
                         family = quasibinomial,
                         design = NHANES_0506_DO)
summary(NA0506_BMI_glm)
exp(cbind(OR=coef(NA0506_BMI_glm), confint(NA0506_BMI_glm)))

#   End of 2005/2006 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(NHANES0506)
remove(NHANES_0506_dataset)
remove(NHANES_0506a)
remove(NHANES_Demo_0506)
remove(NHANES_BMX_0506)
remove(NHANES_MCQ_0506)
remove(NHANES_0506_DO)
remove(NA0506)
remove(NA0506_age_glm)
remove(NA0506_Arth_age)
remove(NA0506_arth_age_ci)
remove(NA0506_Arth_BMI)
remove(NA0506_arth_BMI_ci)
remove(NA0506_Arth_sex)
remove(NA0506_arth_sex_ci)
remove(NA0506_Arthritis_age)
remove(NA0506_Arthritis_BMI)
remove(NA0506_Arthritis_sex)
remove(NA0506_BMI_glm)
remove(NA0506_ci)
remove(NA0506_sex_glm)
remove(NA0506.Age)
remove(NA0506.BMI)
remove(NA0506.Sex)
remove(NA0506.c)
remove(NA0506.no)
remove(NA0506.no.ci)
remove(NA0506.no.df)
remove(NA0506.no.age)
remove(NA0506.no.age.c)
remove(NA0506.no.age.ci)
remove(NA0506.no.BMI)
remove(NA0506.no.BMI.c)
remove(NA0506.no.BMI.ci)
remove(NA0506.no.sex)
remove(NA0506.no.sex.c)
remove(NA0506.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________


#----2007/08----
#National Health and Nutrition Examination Survey (Continuous)
#2007-2008

#----Download----

##Demographic data (with sampling weights)
NHANES_Demo_0708 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/DEMO_E.XPT"))

##Medical conditions
NHANES_MCQ_0708 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/MCQ_E.XPT"))

##Body examination (physical exam)
NHANES_BMX_0708 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/BMX_E.XPT"))

#_______________________________________________________________________________________________

#----Cleaning----

#select relevant variables
NHANES_Demo_0708 <- select(NHANES_Demo_0708, "SEQN", "RIAGENDR", "RIDAGEYR", "WTINT2YR", "SDMVPSU", "SDMVSTRA")
NHANES_MCQ_0708 <- select(NHANES_MCQ_0708, "SEQN", "MCQ160A")
NHANES_BMX_0708 <- select(NHANES_BMX_0708, "SEQN", "BMXBMI")

#Merge the individual datasets by 'seqn'
NHANES_0708a <- merge(NHANES_Demo_0708, NHANES_MCQ_0708)
NHANES0708 <- merge(NHANES_0708a, NHANES_BMX_0708)

#Select only age demographic asked about arthritis
NHANES0708 <- NHANES0708[NHANES0708$RIDAGEYR >= 20, ]

#Observe the dataset
str(NHANES0708)
tail(NHANES0708)
glimpse(NHANES0708)

#Check to see the extent of missing there is missing data in the file
colSums(is.na(NHANES0708))
which(colSums(is.na(NHANES0708)) == nrow(NHANES0708))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
NHANES0708$MCQ160A <- recode(NHANES0708$MCQ160A,
                             "1" = "1",
                             "2" = "0",
                             "7" = "7",
                             "9" = "9")
#Change unknown (7) and refused (9) values to NA
(NHANES0708$MCQ160A <- unknownToNA(NHANES0708$MCQ160A, unknown = c("7", "9")))
table(NHANES0708$MCQ160A)
#   0    1 
#4008 1690 
NHANES0708$MCQ160A <- as.factor(NHANES0708$MCQ160A)
class(NHANES0708$MCQ160A)


#Recode gender variable
NHANES0708$RIAGENDR <- recode(NHANES0708$RIAGENDR,
                              "1" = "Male",
                              "2" = "Female")
table(NHANES0708$RIAGENDR)
#Female   Male 
#  2910   2797 
NHANES0708$RIAGENDR <- as.factor(NHANES0708$RIAGENDR)

#Recode age into categories
NHANES0708$RIDAGEYR <- recode(NHANES0708$RIDAGEYR,
                              "20" = "20 to 24",
                              "21" = "20 to 24",
                              "22" = "20 to 24",
                              "23" = "20 to 24",
                              "24" = "20 to 24",
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
table(NHANES0708$RIDAGEYR)
#    20 to 24     25 to 29     30 to 34     35 to 39     40 to 44     45 to 49     50 to 54     55 to 59     60 to 64     65 to 69 70 and above 
#         446          421          466          511          455          472          505          386          577          386         1082 
NHANES0708$RIDAGEYR <- as.factor(NHANES0708$RIDAGEYR)

#Check class on BMI numeric
NHANES0708$BMXBMI
class(NHANES0708$BMXBMI)

#Create another BMI variable using BMI categories for figures
NHANES0708$BMICAT <- ifelse(NHANES0708$BMXBMI < 18.5, "Underweight",
                            ifelse(NHANES0708$BMXBMI >= 18.5 & NHANES0708$BMXBMI < 25.0, "Healthy weight",
                                   ifelse(NHANES0708$BMXBMI >= 25.0 & NHANES0708$BMXBMI < 30.0, "Overweight",
                                          ifelse(NHANES0708$BMXBMI >= 30.0, "Obese",
                                                 NA))))

table(NHANES0708$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#          1532           2049           1933             93 

#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
NHANES_0708_dataset <- subset(NHANES0708,
                              !is.na(WTINT2YR) &
                                !is.na(SDMVPSU) &
                                !is.na(SDMVSTRA) &
                                !is.na(MCQ160A))

#Check that the sum of the weights is equal to the US population
sum(NHANES_0708_dataset$WTINT2YR)
#The sum of the weights is 207 936 631, which is acceptable

#Check the number of unique PSUs
length(unique(NHANES_0708_dataset[["SDMVPSU"]]))
#2

#Check the number of unique strata
length(unique(NHANES_0708_dataset[["SDMVSTRA"]]))
#The number of unique strata is 16

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHANES_0708_DO <- svydesign(ids = ~1,
                            weights = ~WTINT2YR,
                            strata = ~SDMVSTRA,
                            nest = TRUE,
                            data = NHANES_0708_dataset)
#Observe the design oject
NHANES_0708_DO

#_______________________________________________________________________________________________

#----Analysis----

#1. Overall prevalence
NA_0708 <- svymean(~factor(MCQ160A), 
                   NHANES_0708_DO, 
                   na.rm = TRUE)
NA0708.c <- NA_0708 %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA0708_ci <- confint(NA_0708) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
NA0708 <- bind_cols(NA0708.c, NA0708_ci)
#remove js = 0
NA0708 <- NA0708[-c(1), ] %>%
  select(1,3,4) #final proportion& 95% ci #final proportion, se & 95% ci

#Overall number of people
NA0708.no <- svytotal(~MCQ160A,
                      NHANES_0708_DO,
                      na.rm = TRUE,
                      deff = TRUE)
NA0708.no.df <- as.data.frame(NA0708.no) %>%
  setNames(c("Number of People", "SE", "DEFF")) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA0708.no.ci <- confint(NA0708.no) %>%
  as.data.frame(.) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp")) %>%
  mutate(`CI_Prop_low` = as.numeric(`CI_Prop_low`)) %>%
  mutate(`CI_Prop_upp` = as.numeric(`CI_Prop_upp`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci
NA0708.no <- bind_cols(NA0708.no.df, NA0708.no.ci)
NA0708.no <- NA0708.no[c(1,4,5)]
#remove arth = 0
NA0708.no <- NA0708.no[-c(1), ] #NA0708.no = final number of people with arth and 95%ci


#2. Demographic analysis

#A. Arthritis & Age
NA0708_Arth_age <- svyby(formula = ~MCQ160A,
                         by = ~RIDAGEYR,
                         design = NHANES_0708_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA0708_Arthritis_age <- NA0708_Arth_age %>%
  select(1,3) %>%
  setNames(c("Age", "Proportion")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA0708_arth_age_ci <- confint(NA0708_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA0708_arth_age_ci <- NA0708_arth_age_ci[-c(1:11), ]
#join ci and proportions
NA0708.Age <- bind_cols(NA0708_Arthritis_age, NA0708_arth_age_ci) #NA0708_Age = final proportion and 95% ci by age group

#Number of people by age
NA0708.no.age <- svyby(formula = ~MCQ160A,
                       by = ~RIDAGEYR,
                       design = NHANES_0708_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA0708.no.age.c <- NA0708.no.age %>%
  select(1, 3) %>%
  setNames(c("Age", "Number of People")) 
NA0708.no.age.c <- NA0708.no.age.c %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA0708.no.age.ci <- confint(NA0708.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA0708.no.age.ci <- NA0708.no.age.ci[-c(1:11), ]
NA0708.no.age.ci <- NA0708.no.age.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA0708.no.age <- bind_cols(NA0708.no.age.c, NA0708.no.age.ci)


#Age logistic regression
NA0708_age_glm <- svyglm(MCQ160A~RIDAGEYR + RIAGENDR,
                         family = quasibinomial,
                         design = NHANES_0708_DO)
summary(NA0708_age_glm)
exp(cbind(OR=coef(NA0708_age_glm), confint(NA0708_age_glm)))



#B. Arthritis & Sex
NA0708_Arth_sex <- svyby(formula = ~MCQ160A,
                         by = ~RIAGENDR,
                         design = NHANES_0708_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA0708_Arthritis_sex <- NA0708_Arth_sex %>%
  select(1,3) %>%
  setNames(c("Sex", "Proportion")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA0708_arth_sex_ci <- confint(NA0708_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA0708_arth_sex_ci <- NA0708_arth_sex_ci[-c(1:2), ]
#join ci and proportions
NA0708.Sex <- bind_cols(NA0708_Arthritis_sex, NA0708_arth_sex_ci) #NA0708_Sex = final proportion and 95% ci by sex

#Number of people by sex
NA0708.no.sex <- svyby(formula = ~MCQ160A,
                       by = ~RIAGENDR,
                       design = NHANES_0708_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA0708.no.sex.c <- NA0708.no.sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Number of People")) 
NA0708.no.sex.c <- NA0708.no.sex.c %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA0708.no.sex.ci <- confint(NA0708.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA0708.no.sex.ci <- NA0708.no.sex.ci[-c(1:2), ]
NA0708.no.sex.ci <- NA0708.no.sex.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA0708.no.sex <- bind_cols(NA0708.no.sex.c, NA0708.no.sex.ci)


#Sex logistic regression
NA0708_sex_glm <- svyglm(MCQ160A~RIAGENDR + RIDAGEYR,
                         family = quasibinomial,
                         design = NHANES_0708_DO)
summary(NA0708_sex_glm)
exp(cbind(OR=coef(NA0708_sex_glm), confint(NA0708_sex_glm)))


#C. Arthritis & BMI
NA0708_Arth_BMI <- svyby(formula = ~MCQ160A,
                         by = ~BMICAT,
                         design = NHANES_0708_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA0708_Arthritis_BMI <- NA0708_Arth_BMI %>%
  select(1,3) %>%
  setNames(c("BMI", "Proportion")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA0708_arth_BMI_ci <- confint(NA0708_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA0708_arth_BMI_ci <- NA0708_arth_BMI_ci[-c(1:4), ]
#join ci and proportions
NA0708.BMI <- bind_cols(NA0708_Arthritis_BMI, NA0708_arth_BMI_ci) #NA0708_BMI = final proportion and 95% ci by BMI group

#Number of people by BMI
NA0708.no.BMI <- svyby(formula = ~MCQ160A,
                       by = ~BMICAT,
                       design = NHANES_0708_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA0708.no.BMI.c <- NA0708.no.BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Number of People")) 
NA0708.no.BMI.c <- NA0708.no.BMI.c %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA0708.no.BMI.ci <- confint(NA0708.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA0708.no.BMI.ci <- NA0708.no.BMI.ci[-c(1:4), ]
NA0708.no.BMI.ci <- NA0708.no.BMI.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA0708.no.BMI <- bind_cols(NA0708.no.BMI.c, NA0708.no.BMI.ci)

#BMI logistic regression
NA0708_BMI_glm <- svyglm(MCQ160A~BMXBMI + RIAGENDR + RIDAGEYR,
                         family = quasibinomial,
                         design = NHANES_0708_DO)
summary(NA0708_BMI_glm)
exp(cbind(OR=coef(NA0708_BMI_glm), confint(NA0708_BMI_glm)))

#   End of 2007/2008 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(NHANES0708)
remove(NHANES_0708_dataset)
remove(NHANES_0708a)
remove(NHANES_Demo_0708)
remove(NHANES_BMX_0708)
remove(NHANES_MCQ_0708)
remove(NHANES_0708_DO)
remove(NA0708)
remove(NA0708_age_glm)
remove(NA0708_Arth_age)
remove(NA0708_arth_age_ci)
remove(NA0708_Arth_BMI)
remove(NA0708_arth_BMI_ci)
remove(NA0708_Arth_sex)
remove(NA0708_arth_sex_ci)
remove(NA0708_Arthritis_age)
remove(NA0708_Arthritis_BMI)
remove(NA0708_Arthritis_sex)
remove(NA0708_BMI_glm)
remove(NA0708_ci)
remove(NA0708_sex_glm)
remove(NA0708.Age)
remove(NA0708.BMI)
remove(NA0708.Sex)
remove(NA0708.c)
remove(NA0708.no)
remove(NA0708.no.ci)
remove(NA0708.no.df)
remove(NA0708.no.age)
remove(NA0708.no.age.c)
remove(NA0708.no.age.ci)
remove(NA0708.no.BMI)
remove(NA0708.no.BMI.c)
remove(NA0708.no.BMI.ci)
remove(NA0708.no.sex)
remove(NA0708.no.sex.c)
remove(NA0708.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________


#----2009/10----
#National Health and Nutrition Examination Survey (Continuous)
#2009-2010

#----Download----

##Demographic data (with sampling weights)
NHANES_Demo_0910 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/DEMO_F.XPT"))

##Medical conditions
NHANES_MCQ_0910 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/MCQ_F.XPT"))

##Body examination (physical exam)
NHANES_BMX_0910 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/BMX_F.XPT"))

#_______________________________________________________________________________________________

#----Cleaning----

#select relevant variables
NHANES_Demo_0910 <- select(NHANES_Demo_0910, "SEQN", "RIAGENDR", "RIDAGEYR", "WTINT2YR", "SDMVPSU", "SDMVSTRA")
NHANES_MCQ_0910 <- select(NHANES_MCQ_0910, "SEQN", "MCQ160A")
NHANES_BMX_0910 <- select(NHANES_BMX_0910, "SEQN", "BMXBMI")

#Merge the individual datasets by 'seqn'
NHANES_0910a <- merge(NHANES_Demo_0910, NHANES_MCQ_0910)
NHANES0910 <- merge(NHANES_0910a, NHANES_BMX_0910)

#Select only age demographic asked about arthritis
NHANES0910 <- NHANES0910[NHANES0910$RIDAGEYR >= 20, ]

#Observe the dataset
str(NHANES0910)
tail(NHANES0910)
glimpse(NHANES0910)

#Check to see the extent of missing there is missing data in the file
colSums(is.na(NHANES0910))
which(colSums(is.na(NHANES0910)) == nrow(NHANES0910))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
NHANES0910$MCQ160A <- recode(NHANES0910$MCQ160A,
                             "1" = "1",
                             "2" = "0",
                             "7" = "7",
                             "9" = "9")
#Change unknown (7) and refused (9) values to NA
(NHANES0910$MCQ160A <- unknownToNA(NHANES0910$MCQ160A, unknown = c("7", "9")))
table(NHANES0910$MCQ160A)
#   0    1 
#4402 1641 
NHANES0910$MCQ160A <- as.factor(NHANES0910$MCQ160A)
class(NHANES0910$MCQ160A)


#Recode gender variable
NHANES0910$RIAGENDR <- recode(NHANES0910$RIAGENDR,
                              "1" = "Male",
                              "2" = "Female")
table(NHANES0910$RIAGENDR)
#Female   Male 
#  3130   2929 
NHANES0910$RIAGENDR <- as.factor(NHANES0910$RIAGENDR)

#Recode age into categories
NHANES0910$RIDAGEYR <- recode(NHANES0910$RIDAGEYR,
                              "20" = "20 to 24",
                              "21" = "20 to 24",
                              "22" = "20 to 24",
                              "23" = "20 to 24",
                              "24" = "20 to 24",
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
table(NHANES0910$RIDAGEYR)
#    20 to 24     25 to 29     30 to 34     35 to 39     40 to 44     45 to 49     50 to 54     55 to 59     60 to 64     65 to 69 70 and above 
#         532          490          497          514          551          536          516          418          545          402         1058 
NHANES0910$RIDAGEYR <- as.factor(NHANES0910$RIDAGEYR)

#Check class on BMI numeric
NHANES0910$BMXBMI
class(NHANES0910$BMXBMI)

#Create another BMI variable using BMI categories for figures
NHANES0910$BMICAT <- ifelse(NHANES0910$BMXBMI < 18.5, "Underweight",
                            ifelse(NHANES0910$BMXBMI >= 18.5 & NHANES0910$BMXBMI < 25.0, "Healthy weight",
                                   ifelse(NHANES0910$BMXBMI >= 25.0 & NHANES0910$BMXBMI < 30.0, "Overweight",
                                          ifelse(NHANES0910$BMXBMI >= 30.0, "Obese",
                                                 NA))))

table(NHANES0910$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#          1588           2285           2027             94 

#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
NHANES_0910_dataset <- subset(NHANES0910,
                              !is.na(WTINT2YR) &
                                !is.na(SDMVPSU) &
                                !is.na(SDMVSTRA) &
                                !is.na(MCQ160A))

#Check that the sum of the weights is equal to the US population
sum(NHANES_0910_dataset$WTINT2YR)
#The sum of the weights is 213 948 116, which is acceptable

#Check the number of unique PSUs
length(unique(NHANES_0910_dataset[["SDMVPSU"]]))
#3

#Check the number of unique strata
length(unique(NHANES_0910_dataset[["SDMVSTRA"]]))
#The number of unique strata is 15

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHANES_0910_DO <- svydesign(ids = ~1,
                            weights = ~WTINT2YR,
                            strata = ~SDMVSTRA,
                            nest = TRUE,
                            data = NHANES_0910_dataset)
#Observe the design oject
NHANES_0910_DO

#_______________________________________________________________________________________________

#----Analysis----

#1. Overall prevalence
NA_0910 <- svymean(~factor(MCQ160A), 
                   NHANES_0910_DO, 
                   na.rm = TRUE)
NA0910.c <- NA_0910 %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA0910_ci <- confint(NA_0910) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
NA0910 <- bind_cols(NA0910.c, NA0910_ci)
#remove js = 0
NA0910 <- NA0910[-c(1), ] %>%
  select(1,3,4) #final proportion& 95% ci #final proportion, se & 95% ci

#Overall number of people
NA0910.no <- svytotal(~MCQ160A,
                      NHANES_0910_DO,
                      na.rm = TRUE,
                      deff = TRUE)
NA0910.no.df <- as.data.frame(NA0910.no) %>%
  setNames(c("Number of People", "SE", "DEFF")) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA0910.no.ci <- confint(NA0910.no) %>%
  as.data.frame(.) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp")) %>%
  mutate(`CI_Prop_low` = as.numeric(`CI_Prop_low`)) %>%
  mutate(`CI_Prop_upp` = as.numeric(`CI_Prop_upp`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci
NA0910.no <- bind_cols(NA0910.no.df, NA0910.no.ci)
NA0910.no <- NA0910.no[c(1,4,5)]
#remove arth = 0
NA0910.no <- NA0910.no[-c(1), ] #NA0910.no = final number of people with arth and 95%ci


#2. Demographic analysis

#A. Arthritis & Age
NA0910_Arth_age <- svyby(formula = ~MCQ160A,
                         by = ~RIDAGEYR,
                         design = NHANES_0910_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA0910_Arthritis_age <- NA0910_Arth_age %>%
  select(1,3) %>%
  setNames(c("Age", "Proportion")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA0910_arth_age_ci <- confint(NA0910_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA0910_arth_age_ci <- NA0910_arth_age_ci[-c(1:11), ]
#join ci and proportions
NA0910.Age <- bind_cols(NA0910_Arthritis_age, NA0910_arth_age_ci) #NA0910_Age = final proportion and 95% ci by age group

#Number of people by age
NA0910.no.age <- svyby(formula = ~MCQ160A,
                       by = ~RIDAGEYR,
                       design = NHANES_0910_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA0910.no.age.c <- NA0910.no.age %>%
  select(1, 3) %>%
  setNames(c("Age", "Number of People")) 
NA0910.no.age.c <- NA0910.no.age.c %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA0910.no.age.ci <- confint(NA0910.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA0910.no.age.ci <- NA0910.no.age.ci[-c(1:11), ]
NA0910.no.age.ci <- NA0910.no.age.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA0910.no.age <- bind_cols(NA0910.no.age.c, NA0910.no.age.ci)

#Age logistic regression
NA0910_age_glm <- svyglm(MCQ160A~RIDAGEYR + RIAGENDR,
                         family = quasibinomial,
                         design = NHANES_0910_DO)
summary(NA0910_age_glm)
exp(cbind(OR=coef(NA0910_age_glm), confint(NA0910_age_glm)))



#B. Arthritis & Sex
NA0910_Arth_sex <- svyby(formula = ~MCQ160A,
                         by = ~RIAGENDR,
                         design = NHANES_0910_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA0910_Arthritis_sex <- NA0910_Arth_sex %>%
  select(1,3) %>%
  setNames(c("Sex", "Proportion")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA0910_arth_sex_ci <- confint(NA0910_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA0910_arth_sex_ci <- NA0910_arth_sex_ci[-c(1:2), ]
#join ci and proportions
NA0910.Sex <- bind_cols(NA0910_Arthritis_sex, NA0910_arth_sex_ci) #NA0910_Sex = final proportion and 95% ci by sex

#Number of people by sex
NA0910.no.sex <- svyby(formula = ~MCQ160A,
                       by = ~RIAGENDR,
                       design = NHANES_0910_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA0910.no.sex.c <- NA0910.no.sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Number of People")) 
NA0910.no.sex.c <- NA0910.no.sex.c %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA0910.no.sex.ci <- confint(NA0910.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA0910.no.sex.ci <- NA0910.no.sex.ci[-c(1:2), ]
NA0910.no.sex.ci <- NA0910.no.sex.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA0910.no.sex <- bind_cols(NA0910.no.sex.c, NA0910.no.sex.ci)


#Sex logistic regression
NA0910_sex_glm <- svyglm(MCQ160A~RIAGENDR + RIDAGEYR,
                         family = quasibinomial,
                         design = NHANES_0910_DO)
summary(NA0910_sex_glm)
exp(cbind(OR=coef(NA0910_sex_glm), confint(NA0910_sex_glm)))


#C. Arthritis & BMI
NA0910_Arth_BMI <- svyby(formula = ~MCQ160A,
                         by = ~BMICAT,
                         design = NHANES_0910_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA0910_Arthritis_BMI <- NA0910_Arth_BMI %>%
  select(1,3) %>%
  setNames(c("BMI", "Proportion")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA0910_arth_BMI_ci <- confint(NA0910_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA0910_arth_BMI_ci <- NA0910_arth_BMI_ci[-c(1:4), ]
#join ci and proportions
NA0910.BMI <- bind_cols(NA0910_Arthritis_BMI, NA0910_arth_BMI_ci) #NA0910_BMI = final proportion and 95% ci by BMI group


#Number of people by BMI
NA0910.no.BMI <- svyby(formula = ~MCQ160A,
                       by = ~BMICAT,
                       design = NHANES_0910_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA0910.no.BMI.c <- NA0910.no.BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Number of People")) 
NA0910.no.BMI.c <- NA0910.no.BMI.c %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA0910.no.BMI.ci <- confint(NA0910.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA0910.no.BMI.ci <- NA0910.no.BMI.ci[-c(1:4), ]
NA0910.no.BMI.ci <- NA0910.no.BMI.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA0910.no.BMI <- bind_cols(NA0910.no.BMI.c, NA0910.no.BMI.ci)


#BMI logistic regression
NA0910_BMI_glm <- svyglm(MCQ160A~BMXBMI + RIAGENDR + RIDAGEYR,
                         family = quasibinomial,
                         design = NHANES_0910_DO)
summary(NA0910_BMI_glm)
exp(cbind(OR=coef(NA0910_BMI_glm), confint(NA0910_BMI_glm)))

#   End of 2009/2010 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(NHANES0910)
remove(NHANES_0910_dataset)
remove(NHANES_0910a)
remove(NHANES_Demo_0910)
remove(NHANES_BMX_0910)
remove(NHANES_MCQ_0910)
remove(NHANES_0910_DO)
remove(NA0910)
remove(NA0910_age_glm)
remove(NA0910_Arth_age)
remove(NA0910_arth_age_ci)
remove(NA0910_Arth_BMI)
remove(NA0910_arth_BMI_ci)
remove(NA0910_Arth_sex)
remove(NA0910_arth_sex_ci)
remove(NA0910_Arthritis_age)
remove(NA0910_Arthritis_BMI)
remove(NA0910_Arthritis_sex)
remove(NA0910_BMI_glm)
remove(NA0910_ci)
remove(NA0910_sex_glm)
remove(NA0910.Age)
remove(NA0910.BMI)
remove(NA0910.Sex)
remove(NA0910.c)
remove(NA0910.no)
remove(NA0910.no.ci)
remove(NA0910.no.df)
remove(NA0910.no.age)
remove(NA0910.no.age.c)
remove(NA0910.no.age.ci)
remove(NA0910.no.BMI)
remove(NA0910.no.BMI.c)
remove(NA0910.no.BMI.ci)
remove(NA0910.no.sex)
remove(NA0910.no.sex.c)
remove(NA0910.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________


#----2011/12----
#National Health and Nutrition Examination Survey (Continuous)
#2011-2012

#----Download----

##Demographic data (with sampling weights)
NHANES_Demo_1112 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_G.XPT"))

##Medical conditions
NHANES_MCQ_1112 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/MCQ_G.XPT"))

##Body examination (physical exam)
NHANES_BMX_1112 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/BMX_G.XPT"))

#_______________________________________________________________________________________________

#----Cleaning----

#select relevant variables
NHANES_Demo_1112 <- select(NHANES_Demo_1112, "SEQN", "RIAGENDR", "RIDAGEYR", "WTINT2YR", "SDMVPSU", "SDMVSTRA")
NHANES_MCQ_1112 <- select(NHANES_MCQ_1112, "SEQN", "MCQ160A")
NHANES_BMX_1112 <- select(NHANES_BMX_1112, "SEQN", "BMXBMI")

#Merge the individual datasets by 'seqn'
NHANES_1112a <- merge(NHANES_Demo_1112, NHANES_MCQ_1112)
NHANES1112 <- merge(NHANES_1112a, NHANES_BMX_1112)

#Select only age demographic asked about arthritis
NHANES1112 <- NHANES1112[NHANES1112$RIDAGEYR >= 20, ]

#Observe the dataset
str(NHANES1112)
tail(NHANES1112)
glimpse(NHANES1112)

#Check to see the extent of missing there is missing data in the file
colSums(is.na(NHANES1112))
which(colSums(is.na(NHANES1112)) == nrow(NHANES1112))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
NHANES1112$MCQ160A <- recode(NHANES1112$MCQ160A,
                             "1" = "1",
                             "2" = "0",
                             "7" = "7",
                             "9" = "9")
#Change unknown (7) and refused (9) values to NA
(NHANES1112$MCQ160A <- unknownToNA(NHANES1112$MCQ160A, unknown = c("7", "9")))
table(NHANES1112$MCQ160A)
#   0    1 
#4006 1304 
NHANES1112$MCQ160A <- as.factor(NHANES1112$MCQ160A)
class(NHANES1112$MCQ160A)


#Recode gender variable
NHANES1112$RIAGENDR <- recode(NHANES1112$RIAGENDR,
                              "1" = "Male",
                              "2" = "Female")
table(NHANES1112$RIAGENDR)
#Female   Male 
#  2699   2620 
NHANES1112$RIAGENDR <- as.factor(NHANES1112$RIAGENDR)

#Recode age into categories
NHANES1112$RIDAGEYR <- recode(NHANES1112$RIDAGEYR,
                              "20" = "20 to 24",
                              "21" = "20 to 24",
                              "22" = "20 to 24",
                              "23" = "20 to 24",
                              "24" = "20 to 24",
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
table(NHANES1112$RIDAGEYR)
#    20 to 24     25 to 29     30 to 34     35 to 39     40 to 44     45 to 49     50 to 54     55 to 59     60 to 64     65 to 69 70 and above 
#         524          430          470          454          450          425          487          392          521          347          819 
NHANES1112$RIDAGEYR <- as.factor(NHANES1112$RIDAGEYR)

#Check class on BMI numeric
NHANES1112$BMXBMI
class(NHANES1112$BMXBMI)

#Create another BMI variable using BMI categories for figures
NHANES1112$BMICAT <- ifelse(NHANES1112$BMXBMI < 18.5, "Underweight",
                            ifelse(NHANES1112$BMXBMI >= 18.5 & NHANES1112$BMXBMI < 25.0, "Healthy weight",
                                   ifelse(NHANES1112$BMXBMI >= 25.0 & NHANES1112$BMXBMI < 30.0, "Overweight",
                                          ifelse(NHANES1112$BMXBMI >= 30.0, "Obese",
                                                 NA))))

table(NHANES1112$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#          1577           1873           1684            103 

#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
NHANES_1112_dataset <- subset(NHANES1112,
                              !is.na(WTINT2YR) &
                                !is.na(SDMVPSU) &
                                !is.na(SDMVSTRA) &
                                !is.na(MCQ160A))

#Check that the sum of the weights is equal to the US population
sum(NHANES_1112_dataset$WTINT2YR)
#The sum of the weights is 214 783 380, which is acceptable

#Check the number of unique PSUs
length(unique(NHANES_1112_dataset[["SDMVPSU"]]))
#3

#Check the number of unique strata
length(unique(NHANES_1112_dataset[["SDMVSTRA"]]))
#The number of unique strata is 14

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHANES_1112_DO <- svydesign(ids = ~1,
                            weights = ~WTINT2YR,
                            strata = ~SDMVSTRA,
                            nest = TRUE,
                            data = NHANES_1112_dataset)
#Observe the design oject
NHANES_1112_DO

#_______________________________________________________________________________________________

#----Analysis----

#1. Overall prevalence
NA_1112 <- svymean(~factor(MCQ160A), 
                   NHANES_1112_DO, 
                   na.rm = TRUE)
NA1112.c <- NA_1112 %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA1112_ci <- confint(NA_1112) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
NA1112 <- bind_cols(NA1112.c, NA1112_ci)
#remove js = 0
NA1112 <- NA1112[-c(1), ] %>%
  select(1,3,4) #final proportion& 95% ci #final proportion, se & 95% ci

#Overall number of people
NA1112.no <- svytotal(~MCQ160A,
                      NHANES_1112_DO,
                      na.rm = TRUE,
                      deff = TRUE)
NA1112.no.df <- as.data.frame(NA1112.no) %>%
  setNames(c("Number of People", "SE", "DEFF")) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA1112.no.ci <- confint(NA1112.no) %>%
  as.data.frame(.) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp")) %>%
  mutate(`CI_Prop_low` = as.numeric(`CI_Prop_low`)) %>%
  mutate(`CI_Prop_upp` = as.numeric(`CI_Prop_upp`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci
NA1112.no <- bind_cols(NA1112.no.df, NA1112.no.ci)
NA1112.no <- NA1112.no[c(1,4,5)]
#remove arth = 0
NA1112.no <- NA1112.no[-c(1), ] #NA1112.no = final number of people with arth and 95%ci


#2. Demographic analysis

#A. Arthritis & Age
NA1112_Arth_age <- svyby(formula = ~MCQ160A,
                         by = ~RIDAGEYR,
                         design = NHANES_1112_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA1112_Arthritis_age <- NA1112_Arth_age %>%
  select(1,3) %>%
  setNames(c("Age", "Proportion")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA1112_arth_age_ci <- confint(NA1112_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA1112_arth_age_ci <- NA1112_arth_age_ci[-c(1:11), ]
#join ci and proportions
NA1112.Age <- bind_cols(NA1112_Arthritis_age, NA1112_arth_age_ci) #NA1112_Age = final proportion and 95% ci by age group

#Number of people by age
NA1112.no.age <- svyby(formula = ~MCQ160A,
                       by = ~RIDAGEYR,
                       design = NHANES_1112_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA1112.no.age.c <- NA1112.no.age %>%
  select(1, 3) %>%
  setNames(c("Age", "Number of People")) 
NA1112.no.age.c <- NA1112.no.age.c %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA1112.no.age.ci <- confint(NA1112.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA1112.no.age.ci <- NA1112.no.age.ci[-c(1:11), ]
NA1112.no.age.ci <- NA1112.no.age.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA1112.no.age <- bind_cols(NA1112.no.age.c, NA1112.no.age.ci)


#Age logistic regression
NA1112_age_glm <- svyglm(MCQ160A~RIDAGEYR + RIAGENDR,
                         family = quasibinomial,
                         design = NHANES_1112_DO)
summary(NA1112_age_glm)
exp(cbind(OR=coef(NA1112_age_glm), confint(NA1112_age_glm)))



#B. Arthritis & Sex
NA1112_Arth_sex <- svyby(formula = ~MCQ160A,
                         by = ~RIAGENDR,
                         design = NHANES_1112_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA1112_Arthritis_sex <- NA1112_Arth_sex %>%
  select(1,3) %>%
  setNames(c("Sex", "Proportion")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA1112_arth_sex_ci <- confint(NA1112_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA1112_arth_sex_ci <- NA1112_arth_sex_ci[-c(1:2), ]
#join ci and proportions
NA1112.Sex <- bind_cols(NA1112_Arthritis_sex, NA1112_arth_sex_ci) #NA1112_Sex = final proportion and 95% ci by sex

#Number of people by sex
NA1112.no.sex <- svyby(formula = ~MCQ160A,
                       by = ~RIAGENDR,
                       design = NHANES_1112_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA1112.no.sex.c <- NA1112.no.sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Number of People")) 
NA1112.no.sex.c <- NA1112.no.sex.c %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA1112.no.sex.ci <- confint(NA1112.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA1112.no.sex.ci <- NA1112.no.sex.ci[-c(1:2), ]
NA1112.no.sex.ci <- NA1112.no.sex.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA1112.no.sex <- bind_cols(NA1112.no.sex.c, NA1112.no.sex.ci)


#Sex logistic regression
NA1112_sex_glm <- svyglm(MCQ160A~RIAGENDR + RIDAGEYR,
                         family = quasibinomial,
                         design = NHANES_1112_DO)
summary(NA1112_sex_glm)
exp(cbind(OR=coef(NA1112_sex_glm), confint(NA1112_sex_glm)))


#C. Arthritis & BMI
NA1112_Arth_BMI <- svyby(formula = ~MCQ160A,
                         by = ~BMICAT,
                         design = NHANES_1112_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA1112_Arthritis_BMI <- NA1112_Arth_BMI %>%
  select(1,3) %>%
  setNames(c("BMI", "Proportion")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA1112_arth_BMI_ci <- confint(NA1112_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA1112_arth_BMI_ci <- NA1112_arth_BMI_ci[-c(1:4), ]
#join ci and proportions
NA1112.BMI <- bind_cols(NA1112_Arthritis_BMI, NA1112_arth_BMI_ci) #NA1112_BMI = final proportion and 95% ci by BMI group


#Number of people by BMI
NA1112.no.BMI <- svyby(formula = ~MCQ160A,
                       by = ~BMICAT,
                       design = NHANES_1112_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA1112.no.BMI.c <- NA1112.no.BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Number of People")) 
NA1112.no.BMI.c <- NA1112.no.BMI.c %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA1112.no.BMI.ci <- confint(NA1112.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA1112.no.BMI.ci <- NA1112.no.BMI.ci[-c(1:4), ]
NA1112.no.BMI.ci <- NA1112.no.BMI.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA1112.no.BMI <- bind_cols(NA1112.no.BMI.c, NA1112.no.BMI.ci)


#BMI logistic regression
NA1112_BMI_glm <- svyglm(MCQ160A~BMXBMI + RIAGENDR + RIDAGEYR,
                         family = quasibinomial,
                         design = NHANES_1112_DO)
summary(NA1112_BMI_glm)
exp(cbind(OR=coef(NA1112_BMI_glm), confint(NA1112_BMI_glm)))

#   End of 2011/2012 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(NHANES1112)
remove(NHANES_1112_dataset)
remove(NHANES_1112a)
remove(NHANES_Demo_1112)
remove(NHANES_BMX_1112)
remove(NHANES_MCQ_1112)
remove(NHANES_1112_DO)
remove(NA1112)
remove(NA1112_age_glm)
remove(NA1112_Arth_age)
remove(NA1112_arth_age_ci)
remove(NA1112_Arth_BMI)
remove(NA1112_arth_BMI_ci)
remove(NA1112_Arth_sex)
remove(NA1112_arth_sex_ci)
remove(NA1112_Arthritis_age)
remove(NA1112_Arthritis_BMI)
remove(NA1112_Arthritis_sex)
remove(NA1112_BMI_glm)
remove(NA1112_ci)
remove(NA1112_sex_glm)
remove(NA1112.Age)
remove(NA1112.BMI)
remove(NA1112.Sex)
remove(NA1112.c)
remove(NA1112.no)
remove(NA1112.no.ci)
remove(NA1112.no.df)
remove(NA1112.no.age)
remove(NA1112.no.age.c)
remove(NA1112.no.age.ci)
remove(NA1112.no.BMI)
remove(NA1112.no.BMI.c)
remove(NA1112.no.BMI.ci)
remove(NA1112.no.sex)
remove(NA1112.no.sex.c)
remove(NA1112.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________



#----2013/14----
#National Health and Nutrition Examination Survey (Continuous)
#2013-2014

#----Download----

##Demographic data (with sampling weights)
NHANES_Demo_1314 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DEMO_H.XPT"))

##Medical conditions
NHANES_MCQ_1314 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/MCQ_H.XPT"))

##Body examination (physical exam)
NHANES_BMX_1314 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/BMX_H.XPT"))

#_______________________________________________________________________________________________

#----Cleaning----

#select relevant variables
NHANES_Demo_1314 <- select(NHANES_Demo_1314, "SEQN", "RIAGENDR", "RIDAGEYR", "WTINT2YR", "SDMVPSU", "SDMVSTRA")
NHANES_MCQ_1314 <- select(NHANES_MCQ_1314, "SEQN", "MCQ160A")
NHANES_BMX_1314 <- select(NHANES_BMX_1314, "SEQN", "BMXBMI")

#Merge the individual datasets by 'seqn'
NHANES_1314a <- merge(NHANES_Demo_1314, NHANES_MCQ_1314)
NHANES1314 <- merge(NHANES_1314a, NHANES_BMX_1314)

#Select only age demographic asked about arthritis
NHANES1314 <- NHANES1314[NHANES1314$RIDAGEYR >= 20, ]

#Observe the dataset
str(NHANES1314)
tail(NHANES1314)
glimpse(NHANES1314)

#Check to see the extent of missing there is missing data in the file
colSums(is.na(NHANES1314))
which(colSums(is.na(NHANES1314)) == nrow(NHANES1314))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
NHANES1314$MCQ160A <- recode(NHANES1314$MCQ160A,
                             "1" = "1",
                             "2" = "0",
                             "7" = "7",
                             "9" = "9")
#Change unknown (7) and refused (9) values to NA
(NHANES1314$MCQ160A <- unknownToNA(NHANES1314$MCQ160A, unknown = c("7", "9")))
table(NHANES1314$MCQ160A)
#   0    1 
#4108 1467 
NHANES1314$MCQ160A <- as.factor(NHANES1314$MCQ160A)
class(NHANES1314$MCQ160A)


#Recode gender variable
NHANES1314$RIAGENDR <- recode(NHANES1314$RIAGENDR,
                              "1" = "Male",
                              "2" = "Female")
table(NHANES1314$RIAGENDR)
#Female   Male 
#  2919   2669 
NHANES1314$RIAGENDR <- as.factor(NHANES1314$RIAGENDR)

#Recode age into categories
NHANES1314$RIDAGEYR <- recode(NHANES1314$RIDAGEYR,
                              "20" = "20 to 24",
                              "21" = "20 to 24",
                              "22" = "20 to 24",
                              "23" = "20 to 24",
                              "24" = "20 to 24",
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
table(NHANES1314$RIDAGEYR)
#    20 to 24     25 to 29     30 to 34     35 to 39     40 to 44     45 to 49     50 to 54     55 to 59     60 to 64     65 to 69 70 and above 
#         493          426          501          460          534          473          465          451          520          398          867 
NHANES1314$RIDAGEYR <- as.factor(NHANES1314$RIDAGEYR)

#Check class on BMI numeric
NHANES1314$BMXBMI
class(NHANES1314$BMXBMI)

#Create another BMI variable using BMI categories for figures
NHANES1314$BMICAT <- ifelse(NHANES1314$BMXBMI < 18.5, "Underweight",
                            ifelse(NHANES1314$BMXBMI >= 18.5 & NHANES1314$BMXBMI < 25.0, "Healthy weight",
                                   ifelse(NHANES1314$BMXBMI >= 25.0 & NHANES1314$BMXBMI < 30.0, "Overweight",
                                          ifelse(NHANES1314$BMXBMI >= 30.0, "Obese",
                                                 NA))))

table(NHANES1314$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#          1579           2083           1768             90 

#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
NHANES_1314_dataset <- subset(NHANES1314,
                              !is.na(WTINT2YR) &
                                !is.na(SDMVPSU) &
                                !is.na(SDMVSTRA) &
                                !is.na(MCQ160A))

#Check that the sum of the weights is equal to the US population
sum(NHANES_1314_dataset$WTINT2YR)
#The sum of the weights is 221 978 874, which is acceptable

#Check the number of unique PSUs
length(unique(NHANES_1314_dataset[["SDMVPSU"]]))
#2

#Check the number of unique strata
length(unique(NHANES_1314_dataset[["SDMVSTRA"]]))
#The number of unique strata is 15

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHANES_1314_DO <- svydesign(ids = ~1,
                            weights = ~WTINT2YR,
                            strata = ~SDMVSTRA,
                            nest = TRUE,
                            data = NHANES_1314_dataset)
#Observe the design oject
NHANES_1314_DO
#_______________________________________________________________________________________________

#----Analysis----

#1. Overall prevalence
NA_1314 <- svymean(~factor(MCQ160A), 
                   NHANES_1314_DO, 
                   na.rm = TRUE)
NA1314.c <- NA_1314 %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA1314_ci <- confint(NA_1314) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
NA1314 <- bind_cols(NA1314.c, NA1314_ci)
#remove js = 0
NA1314 <- NA1314[-c(1), ] %>%
  select(1,3,4) #final proportion& 95% ci #final proportion, se & 95% ci

#Overall number of people
NA1314.no <- svytotal(~MCQ160A,
                      NHANES_1314_DO,
                      na.rm = TRUE,
                      deff = TRUE)
NA1314.no.df <- as.data.frame(NA1314.no) %>%
  setNames(c("Number of People", "SE", "DEFF")) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA1314.no.ci <- confint(NA1314.no) %>%
  as.data.frame(.) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp")) %>%
  mutate(`CI_Prop_low` = as.numeric(`CI_Prop_low`)) %>%
  mutate(`CI_Prop_upp` = as.numeric(`CI_Prop_upp`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci
NA1314.no <- bind_cols(NA1314.no.df, NA1314.no.ci)
NA1314.no <- NA1314.no[c(1,4,5)]
#remove arth = 0
NA1314.no <- NA1314.no[-c(1), ] #NA1314.no = final number of people with arth and 95%ci


#2. Demographic analysis

#A. Arthritis & Age
NA1314_Arth_age <- svyby(formula = ~MCQ160A,
                         by = ~RIDAGEYR,
                         design = NHANES_1314_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA1314_Arthritis_age <- NA1314_Arth_age %>%
  select(1,3) %>%
  setNames(c("Age", "Proportion")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA1314_arth_age_ci <- confint(NA1314_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA1314_arth_age_ci <- NA1314_arth_age_ci[-c(1:11), ]
#join ci and proportions
NA1314.Age <- bind_cols(NA1314_Arthritis_age, NA1314_arth_age_ci) #NA1314_Age = final proportion and 95% ci by age group

#Number of people by age
NA1314.no.age <- svyby(formula = ~MCQ160A,
                       by = ~RIDAGEYR,
                       design = NHANES_1314_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA1314.no.age.c <- NA1314.no.age %>%
  select(1, 3) %>%
  setNames(c("Age", "Number of People")) 
NA1314.no.age.c <- NA1314.no.age.c %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA1314.no.age.ci <- confint(NA1314.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA1314.no.age.ci <- NA1314.no.age.ci[-c(1:11), ]
NA1314.no.age.ci <- NA1314.no.age.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA1314.no.age <- bind_cols(NA1314.no.age.c, NA1314.no.age.ci)


#Age logistic regression
NA1314_age_glm <- svyglm(MCQ160A~RIDAGEYR + RIAGENDR,
                         family = quasibinomial,
                         design = NHANES_1314_DO)
summary(NA1314_age_glm)
exp(cbind(OR=coef(NA1314_age_glm), confint(NA1314_age_glm)))

#B. Arthritis & Sex
NA1314_Arth_sex <- svyby(formula = ~MCQ160A,
                         by = ~RIAGENDR,
                         design = NHANES_1314_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA1314_Arthritis_sex <- NA1314_Arth_sex %>%
  select(1,3) %>%
  setNames(c("Sex", "Proportion")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA1314_arth_sex_ci <- confint(NA1314_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA1314_arth_sex_ci <- NA1314_arth_sex_ci[-c(1:2), ]
#join ci and proportions
NA1314.Sex <- bind_cols(NA1314_Arthritis_sex, NA1314_arth_sex_ci) #NA1314_Sex = final proportion and 95% ci by sex

#Number of people by sex
NA1314.no.sex <- svyby(formula = ~MCQ160A,
                       by = ~RIAGENDR,
                       design = NHANES_1314_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA1314.no.sex.c <- NA1314.no.sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Number of People")) 
NA1314.no.sex.c <- NA1314.no.sex.c %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA1314.no.sex.ci <- confint(NA1314.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA1314.no.sex.ci <- NA1314.no.sex.ci[-c(1:2), ]
NA1314.no.sex.ci <- NA1314.no.sex.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA1314.no.sex <- bind_cols(NA1314.no.sex.c, NA1314.no.sex.ci)


#Sex logistic regression
NA1314_sex_glm <- svyglm(MCQ160A~RIAGENDR + RIDAGEYR,
                         family = quasibinomial,
                         design = NHANES_1314_DO)
summary(NA1314_sex_glm)
exp(cbind(OR=coef(NA1314_sex_glm), confint(NA1314_sex_glm)))


#C. Arthritis & BMI
NA1314_Arth_BMI <- svyby(formula = ~MCQ160A,
                         by = ~BMICAT,
                         design = NHANES_1314_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA1314_Arthritis_BMI <- NA1314_Arth_BMI %>%
  select(1,3) %>%
  setNames(c("BMI", "Proportion")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA1314_arth_BMI_ci <- confint(NA1314_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA1314_arth_BMI_ci <- NA1314_arth_BMI_ci[-c(1:4), ]
#join ci and proportions
NA1314.BMI <- bind_cols(NA1314_Arthritis_BMI, NA1314_arth_BMI_ci) #NA1314_BMI = final proportion and 95% ci by BMI group


#Number of people by BMI
NA1314.no.BMI <- svyby(formula = ~MCQ160A,
                       by = ~BMICAT,
                       design = NHANES_1314_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA1314.no.BMI.c <- NA1314.no.BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Number of People")) 
NA1314.no.BMI.c <- NA1314.no.BMI.c %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA1314.no.BMI.ci <- confint(NA1314.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA1314.no.BMI.ci <- NA1314.no.BMI.ci[-c(1:4), ]
NA1314.no.BMI.ci <- NA1314.no.BMI.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA1314.no.BMI <- bind_cols(NA1314.no.BMI.c, NA1314.no.BMI.ci)


#BMI logistic regression
NA1314_BMI_glm <- svyglm(MCQ160A~BMXBMI + RIAGENDR + RIDAGEYR,
                         family = quasibinomial,
                         design = NHANES_1314_DO)
summary(NA1314_BMI_glm)
exp(cbind(OR=coef(NA1314_BMI_glm), confint(NA1314_BMI_glm)))

#   End of 2013/2014 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(NHANES1314)
remove(NHANES_1314_dataset)
remove(NHANES_1314a)
remove(NHANES_Demo_1314)
remove(NHANES_BMX_1314)
remove(NHANES_MCQ_1314)
remove(NHANES_1314_DO)
remove(NA1314)
remove(NA1314_age_glm)
remove(NA1314_Arth_age)
remove(NA1314_arth_age_ci)
remove(NA1314_Arth_BMI)
remove(NA1314_arth_BMI_ci)
remove(NA1314_Arth_sex)
remove(NA1314_arth_sex_ci)
remove(NA1314_Arthritis_age)
remove(NA1314_Arthritis_BMI)
remove(NA1314_Arthritis_sex)
remove(NA1314_BMI_glm)
remove(NA1314_ci)
remove(NA1314_sex_glm)
remove(NA1314.Age)
remove(NA1314.BMI)
remove(NA1314.Sex)
remove(NA1314.c)
remove(NA1314.no)
remove(NA1314.no.ci)
remove(NA1314.no.df)
remove(NA1314.no.age)
remove(NA1314.no.age.c)
remove(NA1314.no.age.ci)
remove(NA1314.no.BMI)
remove(NA1314.no.BMI.c)
remove(NA1314.no.BMI.ci)
remove(NA1314.no.sex)
remove(NA1314.no.sex.c)
remove(NA1314.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________


#----2015/16----
#National Health and Nutrition Examination Survey (Continuous)
#2015-2016

#----Download----

##Demographic data (with sampling weights)
NHANES_Demo_1516 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.XPT"))

##Medical conditions
NHANES_MCQ_1516 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/MCQ_I.XPT"))

##Body examination (physical exam)
NHANES_BMX_1516 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BMX_I.XPT"))

#_______________________________________________________________________________________________

#----Cleaning----

#select relevant variables
NHANES_Demo_1516 <- select(NHANES_Demo_1516, "SEQN", "RIAGENDR", "RIDAGEYR", "WTINT2YR", "SDMVPSU", "SDMVSTRA")
NHANES_MCQ_1516 <- select(NHANES_MCQ_1516, "SEQN", "MCQ160A")
NHANES_BMX_1516 <- select(NHANES_BMX_1516, "SEQN", "BMXBMI")

#Merge the individual datasets by 'seqn'
NHANES_1516a <- merge(NHANES_Demo_1516, NHANES_MCQ_1516)
NHANES1516 <- merge(NHANES_1516a, NHANES_BMX_1516)

#Select only age demographic asked about arthritis
NHANES1516 <- NHANES1516[NHANES1516$RIDAGEYR >= 20, ]

#Observe the dataset
str(NHANES1516)
tail(NHANES1516)
glimpse(NHANES1516)

#Check to see the extent of missing there is missing data in the file
colSums(is.na(NHANES1516))
which(colSums(is.na(NHANES1516)) == nrow(NHANES1516))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
NHANES1516$MCQ160A <- recode(NHANES1516$MCQ160A,
                             "1" = "1",
                             "2" = "0",
                             "7" = "7",
                             "9" = "9")
#Change unknown (7) and refused (9) values to NA
(NHANES1516$MCQ160A <- unknownToNA(NHANES1516$MCQ160A, unknown = c("7", "9")))
table(NHANES1516$MCQ160A)
#   0    1 
#4045 1419 
NHANES1516$MCQ160A <- as.factor(NHANES1516$MCQ160A)
class(NHANES1516$MCQ160A)


#Recode gender variable
NHANES1516$RIAGENDR <- recode(NHANES1516$RIAGENDR,
                              "1" = "Male",
                              "2" = "Female")
table(NHANES1516$RIAGENDR)
#Female   Male 
#  2850   2624 
NHANES1516$RIAGENDR <- as.factor(NHANES1516$RIAGENDR)

#Recode age into categories
NHANES1516$RIDAGEYR <- recode(NHANES1516$RIDAGEYR,
                              "20" = "20 to 24",
                              "21" = "20 to 24",
                              "22" = "20 to 24",
                              "23" = "20 to 24",
                              "24" = "20 to 24",
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
table(NHANES1516$RIDAGEYR)
#    20 to 24     25 to 29     30 to 34     35 to 39     40 to 44     45 to 49     50 to 54     55 to 59     60 to 64     65 to 69 70 and above 
#         421          510          478          455          460          453          474          414          507          410          892 
NHANES1516$RIDAGEYR <- as.factor(NHANES1516$RIDAGEYR)

#Check class on BMI numeric
NHANES1516$BMXBMI
class(NHANES1516$BMXBMI)

#Create another BMI variable using BMI categories for figures
NHANES1516$BMICAT <- ifelse(NHANES1516$BMXBMI < 18.5, "Underweight",
                            ifelse(NHANES1516$BMXBMI >= 18.5 & NHANES1516$BMXBMI < 25.0, "Healthy weight",
                                   ifelse(NHANES1516$BMXBMI >= 25.0 & NHANES1516$BMXBMI < 30.0, "Overweight",
                                          ifelse(NHANES1516$BMXBMI >= 30.0, "Obese",
                                                 NA))))

table(NHANES1516$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#          1410           2188           1733             75 

#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
NHANES_1516_dataset <- subset(NHANES1516,
                              !is.na(WTINT2YR) &
                                !is.na(SDMVPSU) &
                                !is.na(SDMVSTRA) &
                                !is.na(MCQ160A))

#Check that the sum of the weights is equal to the US population
sum(NHANES_1516_dataset$WTINT2YR)
#The sum of the weights is 225 010 002, which is acceptable

#Check the number of unique PSUs
length(unique(NHANES_1516_dataset[["SDMVPSU"]]))
#2

#Check the number of unique strata
length(unique(NHANES_1516_dataset[["SDMVSTRA"]]))
#The number of unique strata is 15

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHANES_1516_DO <- svydesign(ids = ~1,
                            weights = ~WTINT2YR,
                            strata = ~SDMVSTRA,
                            nest = TRUE,
                            data = NHANES_1516_dataset)
#Observe the design oject
NHANES_1516_DO

#_______________________________________________________________________________________________

#----Analysis----

#1. Overall prevalence
NA_1516 <- svymean(~factor(MCQ160A), 
                   NHANES_1516_DO, 
                   na.rm = TRUE)
NA1516.c <- NA_1516 %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA1516_ci <- confint(NA_1516) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
NA1516 <- bind_cols(NA1516.c, NA1516_ci)
#remove js = 0
NA1516 <- NA1516[-c(1), ] %>%
  select(1,3,4) #final proportion& 95% ci #final proportion, se & 95% ci


#Overall number of people
NA1516.no <- svytotal(~MCQ160A,
                      NHANES_1516_DO,
                      na.rm = TRUE,
                      deff = TRUE)
NA1516.no.df <- as.data.frame(NA1516.no) %>%
  setNames(c("Number of People", "SE", "DEFF")) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA1516.no.ci <- confint(NA1516.no) %>%
  as.data.frame(.) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp")) %>%
  mutate(`CI_Prop_low` = as.numeric(`CI_Prop_low`)) %>%
  mutate(`CI_Prop_upp` = as.numeric(`CI_Prop_upp`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci
NA1516.no <- bind_cols(NA1516.no.df, NA1516.no.ci)
NA1516.no <- NA1516.no[c(1,4,5)]
#remove arth = 0
NA1516.no <- NA1516.no[-c(1), ] #NA1516.no = final number of people with arth and 95%ci


#2. Demographic analysis

#A. Arthritis & Age
NA1516_Arth_age <- svyby(formula = ~MCQ160A,
                         by = ~RIDAGEYR,
                         design = NHANES_1516_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA1516_Arthritis_age <- NA1516_Arth_age %>%
  select(1,3) %>%
  setNames(c("Age", "Proportion")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA1516_arth_age_ci <- confint(NA1516_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA1516_arth_age_ci <- NA1516_arth_age_ci[-c(1:11), ]
#join ci and proportions
NA1516.Age <- bind_cols(NA1516_Arthritis_age, NA1516_arth_age_ci) #NA1516_Age = final proportion and 95% ci by age group


#Number of people by age
NA1516.no.age <- svyby(formula = ~MCQ160A,
                       by = ~RIDAGEYR,
                       design = NHANES_1516_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA1516.no.age.c <- NA1516.no.age %>%
  select(1, 3) %>%
  setNames(c("Age", "Number of People")) 
NA1516.no.age.c <- NA1516.no.age.c %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA1516.no.age.ci <- confint(NA1516.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA1516.no.age.ci <- NA1516.no.age.ci[-c(1:11), ]
NA1516.no.age.ci <- NA1516.no.age.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA1516.no.age <- bind_cols(NA1516.no.age.c, NA1516.no.age.ci)


#Age logistic regression
NA1516_age_glm <- svyglm(MCQ160A~RIDAGEYR + RIAGENDR,
                         family = quasibinomial,
                         design = NHANES_1516_DO)
summary(NA1516_age_glm)
exp(cbind(OR=coef(NA1516_age_glm), confint(NA1516_age_glm)))



#B. Arthritis & Sex
NA1516_Arth_sex <- svyby(formula = ~MCQ160A,
                         by = ~RIAGENDR,
                         design = NHANES_1516_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA1516_Arthritis_sex <- NA1516_Arth_sex %>%
  select(1,3) %>%
  setNames(c("Sex", "Proportion")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA1516_arth_sex_ci <- confint(NA1516_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA1516_arth_sex_ci <- NA1516_arth_sex_ci[-c(1:2), ]
#join ci and proportions
NA1516.Sex <- bind_cols(NA1516_Arthritis_sex, NA1516_arth_sex_ci) #NA1516_Sex = final proportion and 95% ci by sex

#Number of people by sex
NA1516.no.sex <- svyby(formula = ~MCQ160A,
                       by = ~RIAGENDR,
                       design = NHANES_1516_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA1516.no.sex.c <- NA1516.no.sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Number of People")) 
NA1516.no.sex.c <- NA1516.no.sex.c %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA1516.no.sex.ci <- confint(NA1516.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA1516.no.sex.ci <- NA1516.no.sex.ci[-c(1:2), ]
A1516.no.sex.ci <- NA1516.no.sex.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA1516.no.sex <- bind_cols(NA1516.no.sex.c, NA1516.no.sex.ci)

#Sex logistic regression
NA1516_sex_glm <- svyglm(MCQ160A~RIAGENDR + RIDAGEYR,
                         family = quasibinomial,
                         design = NHANES_1516_DO)
summary(NA1516_sex_glm)
exp(cbind(OR=coef(NA1516_sex_glm), confint(NA1516_sex_glm)))


#C. Arthritis & BMI
NA1516_Arth_BMI <- svyby(formula = ~MCQ160A,
                         by = ~BMICAT,
                         design = NHANES_1516_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA1516_Arthritis_BMI <- NA1516_Arth_BMI %>%
  select(1,3) %>%
  setNames(c("BMI", "Proportion")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA1516_arth_BMI_ci <- confint(NA1516_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA1516_arth_BMI_ci <- NA1516_arth_BMI_ci[-c(1:4), ]
#join ci and proportions
NA1516.BMI <- bind_cols(NA1516_Arthritis_BMI, NA1516_arth_BMI_ci) #NA1516_BMI = final proportion and 95% ci by BMI group

#Number of people by BMI
NA1516.no.BMI <- svyby(formula = ~MCQ160A,
                       by = ~BMICAT,
                       design = NHANES_1516_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA1516.no.BMI.c <- NA1516.no.BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Number of People")) 
NA1516.no.BMI.c <- NA1516.no.BMI.c %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA1516.no.BMI.ci <- confint(NA1516.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA1516.no.BMI.ci <- NA1516.no.BMI.ci[-c(1:4), ]
NA1516.no.BMI.ci <- NA1516.no.BMI.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA1516.no.BMI <- bind_cols(NA1516.no.BMI.c, NA1516.no.BMI.ci)

#BMI logistic regression
NA1516_BMI_glm <- svyglm(MCQ160A~BMXBMI + RIAGENDR + RIDAGEYR,
                         family = quasibinomial,
                         design = NHANES_1516_DO)
summary(NA1516_BMI_glm)
exp(cbind(OR=coef(NA1516_BMI_glm), confint(NA1516_BMI_glm)))

#   End of 2013/2014 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(NHANES1516)
remove(NHANES_1516_dataset)
remove(NHANES_1516a)
remove(NHANES_Demo_1516)
remove(NHANES_BMX_1516)
remove(NHANES_MCQ_1516)
remove(NHANES_1516_DO)
remove(NA1516)
remove(NA1516_age_glm)
remove(NA1516_Arth_age)
remove(NA1516_arth_age_ci)
remove(NA1516_Arth_BMI)
remove(NA1516_arth_BMI_ci)
remove(NA1516_Arth_sex)
remove(NA1516_arth_sex_ci)
remove(NA1516_Arthritis_age)
remove(NA1516_Arthritis_BMI)
remove(NA1516_Arthritis_sex)
remove(NA1516_BMI_glm)
remove(NA1516_ci)
remove(NA1516_sex_glm)
remove(NA1516.Age)
remove(NA1516.BMI)
remove(NA1516.Sex)
remove(NA1516.c)
remove(NA1516.no)
remove(NA1516.no.ci)
remove(NA1516.no.df)
remove(NA1516.no.age)
remove(NA1516.no.age.c)
remove(NA1516.no.age.ci)
remove(NA1516.no.BMI)
remove(NA1516.no.BMI.c)
remove(NA1516.no.BMI.ci)
remove(NA1516.no.sex)
remove(NA1516.no.sex.c)
remove(NA1516.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________



#----2017/18----
#National Health and Nutrition Examination Survey (Continuous)
#2017-2018

#----Download----

##Demographic data (with sampling weights)
NHANES_Demo_1718 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT"))

##Medical conditions
NHANES_MCQ_1718 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/MCQ_J.XPT"))

##Body examination (physical exam)
NHANES_BMX_1718 <- read_xpt(url("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BMX_J.XPT"))

#_______________________________________________________________________________________________

#----Cleaning----

#select relevant variables
NHANES_Demo_1718 <- select(NHANES_Demo_1718, "SEQN", "RIAGENDR", "RIDAGEYR", "WTINT2YR", "SDMVPSU", "SDMVSTRA")
NHANES_MCQ_1718 <- select(NHANES_MCQ_1718, "SEQN", "MCQ160A")
NHANES_BMX_1718 <- select(NHANES_BMX_1718, "SEQN", "BMXBMI")

#Merge the individual datasets by 'seqn'
NHANES_1718a <- merge(NHANES_Demo_1718, NHANES_MCQ_1718)
NHANES1718 <- merge(NHANES_1718a, NHANES_BMX_1718)

#Select only age demographic asked about arthritis
NHANES1718 <- NHANES1718[NHANES1718$RIDAGEYR >= 20, ]

#Observe the dataset
str(NHANES1718)
tail(NHANES1718)
glimpse(NHANES1718)

#Check to see the extent of missing there is missing data in the file
colSums(is.na(NHANES1718))
which(colSums(is.na(NHANES1718)) == nrow(NHANES1718))
#named integer = 0, which means that at least one reponse is present for the variables selected.

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
NHANES1718$MCQ160A <- recode(NHANES1718$MCQ160A,
                             "1" = "1",
                             "2" = "0",
                             "7" = "7",
                             "9" = "9")
#Change unknown (7) and refused (9) values to NA
(NHANES1718$MCQ160A <- unknownToNA(NHANES1718$MCQ160A, unknown = c("7", "9")))
table(NHANES1718$MCQ160A)
#   0    1 
#3645 1605 
NHANES1718$MCQ160A <- as.factor(NHANES1718$MCQ160A)
class(NHANES1718$MCQ160A)


#Recode gender variable
NHANES1718$RIAGENDR <- recode(NHANES1718$RIAGENDR,
                              "1" = "Male",
                              "2" = "Female")
table(NHANES1718$RIAGENDR)
#Female   Male 
# 2724   2541 
NHANES1718$RIAGENDR <- as.factor(NHANES1718$RIAGENDR)

#Recode age into categories
NHANES1718$RIDAGEYR <- recode(NHANES1718$RIDAGEYR,
                              "20" = "20 to 24",
                              "21" = "20 to 24",
                              "22" = "20 to 24",
                              "23" = "20 to 24",
                              "24" = "20 to 24",
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
table(NHANES1718$RIDAGEYR)
#    20 to 24     25 to 29     30 to 34     35 to 39     40 to 44     45 to 49     50 to 54     55 to 59     60 to 64     65 to 69 70 and above 
#         382          394          422          391          387          391          410          470          626          431          961 
NHANES1718$RIDAGEYR <- as.factor(NHANES1718$RIDAGEYR)

#Check class on BMI numeric
NHANES1718$BMXBMI
class(NHANES1718$BMXBMI)

#Create another BMI variable using BMI categories for figures
NHANES1718$BMICAT <- ifelse(NHANES1718$BMXBMI < 18.5, "Underweight",
                            ifelse(NHANES1718$BMXBMI >= 18.5 & NHANES1718$BMXBMI < 25.0, "Healthy weight",
                                   ifelse(NHANES1718$BMXBMI >= 25.0 & NHANES1718$BMXBMI < 30.0, "Overweight",
                                          ifelse(NHANES1718$BMXBMI >= 30.0, "Obese",
                                                 NA))))

table(NHANES1718$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#          1255           2171           1667             82 

#__________________________________________________________________________________________________________________

#Some final checks:

options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
NHANES_1718_dataset <- subset(NHANES1718,
                              !is.na(WTINT2YR) &
                                !is.na(SDMVPSU) &
                                !is.na(SDMVSTRA) &
                                !is.na(MCQ160A))

#Check that the sum of the weights is equal to the US population
sum(NHANES_1718_dataset$WTINT2YR)
#The sum of the weights is 225 441 502, which is acceptable

#Check the number of unique PSUs
length(unique(NHANES_1718_dataset[["SDMVPSU"]]))
#2

#Check the number of unique strata
length(unique(NHANES_1718_dataset[["SDMVSTRA"]]))
#The number of unique strata is 15

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
NHANES_1718_DO <- svydesign(ids = ~1,
                            weights = ~WTINT2YR,
                            strata = ~SDMVSTRA,
                            nest = TRUE,
                            data = NHANES_1718_dataset)
#Observe the design oject
NHANES_1718_DO

#_______________________________________________________________________________________________

#----Analysis----

#1. Overall prevalence
NA_1718 <- svymean(~factor(MCQ160A), 
                   NHANES_1718_DO, 
                   na.rm = TRUE)
NA1718.c <- NA_1718 %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA1718_ci <- confint(NA_1718) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & prop
NA1718 <- bind_cols(NA1718.c, NA1718_ci)
#remove js = 0
NA1718 <- NA1718[-c(1), ] %>%
  select(1,3,4) #final proportion& 95% ci #final proportion, se & 95% ci

#Overall number of people
NA1718.no <- svytotal(~MCQ160A,
                      NHANES_1718_DO,
                      na.rm = TRUE,
                      deff = TRUE)
NA1718.no.df <- as.data.frame(NA1718.no) %>%
  setNames(c("Number of People", "SE", "DEFF")) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA1718.no.ci <- confint(NA1718.no) %>%
  as.data.frame(.) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp")) %>%
  mutate(`CI_Prop_low` = as.numeric(`CI_Prop_low`)) %>%
  mutate(`CI_Prop_upp` = as.numeric(`CI_Prop_upp`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci
NA1718.no <- bind_cols(NA1718.no.df, NA1718.no.ci)
NA1718.no <- NA1718.no[c(1,4,5)]
#remove arth = 0
NA1718.no <- NA1718.no[-c(1), ] #NA1718.no = final number of people with arth and 95%ci


#2. Demographic analysis

#A. Arthritis & Age
NA1718_Arth_age <- svyby(formula = ~MCQ160A,
                         by = ~RIDAGEYR,
                         design = NHANES_1718_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA1718_Arthritis_age <- NA1718_Arth_age %>%
  select(1,3) %>%
  setNames(c("Age", "Proportion")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA1718_arth_age_ci <- confint(NA1718_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA1718_arth_age_ci <- NA1718_arth_age_ci[-c(1:11), ]
#join ci and proportions
NA1718.Age <- bind_cols(NA1718_Arthritis_age, NA1718_arth_age_ci) #NA1718_Age = final proportion and 95% ci by age group

#Number of people by age
NA1718.no.age <- svyby(formula = ~MCQ160A,
                       by = ~RIDAGEYR,
                       design = NHANES_1718_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA1718.no.age.c <- NA1718.no.age %>%
  select(1, 3) %>%
  setNames(c("Age", "Number of People")) 
NA1718.no.age.c <- NA1718.no.age.c %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA1718.no.age.ci <- confint(NA1718.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA1718.no.age.ci <- NA1718.no.age.ci[-c(1:11), ]
NA1718.no.age.ci <- NA1718.no.age.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA1718.no.age <- bind_cols(NA1718.no.age.c, NA1718.no.age.ci)


#Age logistic regression
NA1718_age_glm <- svyglm(MCQ160A~RIDAGEYR + RIAGENDR,
                         family = quasibinomial,
                         design = NHANES_1718_DO)
summary(NA1718_age_glm)
exp(cbind(OR=coef(NA1718_age_glm), confint(NA1718_age_glm)))



#B. Arthritis & Sex
NA1718_Arth_sex <- svyby(formula = ~MCQ160A,
                         by = ~RIAGENDR,
                         design = NHANES_1718_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA1718_Arthritis_sex <- NA1718_Arth_sex %>%
  select(1,3) %>%
  setNames(c("Sex", "Proportion")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA1718_arth_sex_ci <- confint(NA1718_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA1718_arth_sex_ci <- NA1718_arth_sex_ci[-c(1:2), ]
#join ci and proportions
NA1718.Sex <- bind_cols(NA1718_Arthritis_sex, NA1718_arth_sex_ci) #NA1718_Sex = final proportion and 95% ci by sex

#Number of people by sex
NA1718.no.sex <- svyby(formula = ~MCQ160A,
                       by = ~RIAGENDR,
                       design = NHANES_1718_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA1718.no.sex.c <- NA1718.no.sex %>%
  select(1, 3) %>%
  setNames(c("Sex", "Number of People")) 
NA1718.no.sex.c <- NA1718.no.sex.c %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA1718.no.sex.ci <- confint(NA1718.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA1718.no.sex.ci <- NA1718.no.sex.ci[-c(1:2), ]
NA1718.no.sex.ci <- NA1718.no.sex.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA1718.no.sex <- bind_cols(NA1718.no.sex.c, NA1718.no.sex.ci)


#Sex logistic regression
NA1718_sex_glm <- svyglm(MCQ160A~RIAGENDR + RIDAGEYR,
                         family = quasibinomial,
                         design = NHANES_1718_DO)
summary(NA1718_sex_glm)
exp(cbind(OR=coef(NA1718_sex_glm), confint(NA1718_sex_glm)))


#C. Arthritis & BMI
NA1718_Arth_BMI <- svyby(formula = ~MCQ160A,
                         by = ~BMICAT,
                         design = NHANES_1718_DO,
                         FUN = svymean,
                         na.rm = TRUE)
NA1718_Arthritis_BMI <- NA1718_Arth_BMI %>%
  select(1,3) %>%
  setNames(c("BMI", "Proportion")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
NA1718_arth_BMI_ci <- confint(NA1718_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for no arthritis
NA1718_arth_BMI_ci <- NA1718_arth_BMI_ci[-c(1:4), ]
#join ci and proportions
NA1718.BMI <- bind_cols(NA1718_Arthritis_BMI, NA1718_arth_BMI_ci) #NA1718_BMI = final proportion and 95% ci by BMI group


#Number of people by BMI
NA1718.no.BMI <- svyby(formula = ~MCQ160A,
                       by = ~BMICAT,
                       design = NHANES_1718_DO,
                       FUN = svytotal,
                       na.rm = TRUE,
                       deff = FALSE)
NA1718.no.BMI.c <- NA1718.no.BMI %>%
  select(1, 3) %>%
  setNames(c("BMI", "Number of People")) 
NA1718.no.BMI.c <- NA1718.no.BMI.c %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
NA1718.no.BMI.ci <- confint(NA1718.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
NA1718.no.BMI.ci <- NA1718.no.BMI.ci[-c(1:4), ]
NA1718.no.BMI.ci <- NA1718.no.BMI.ci %>%
  mutate_if(is.numeric, list(~floor(.)))
#join number and ci's
NA1718.no.BMI <- bind_cols(NA1718.no.BMI.c, NA1718.no.BMI.ci)


#BMI logistic regression
NA1718_BMI_glm <- svyglm(MCQ160A~BMXBMI + RIAGENDR + RIDAGEYR,
                         family = quasibinomial,
                         design = NHANES_1718_DO)
summary(NA1718_BMI_glm)
exp(cbind(OR=coef(NA1718_BMI_glm), confint(NA1718_BMI_glm)))

#   End of 2013/2014 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(NHANES1718)
remove(NHANES_1718_dataset)
remove(NHANES_1718a)
remove(NHANES_Demo_1718)
remove(NHANES_BMX_1718)
remove(NHANES_MCQ_1718)
remove(NHANES_1718_DO)
remove(NA1718)
remove(NA1718_age_glm)
remove(NA1718_Arth_age)
remove(NA1718_arth_age_ci)
remove(NA1718_Arth_BMI)
remove(NA1718_arth_BMI_ci)
remove(NA1718_Arth_sex)
remove(NA1718_arth_sex_ci)
remove(NA1718_Arthritis_age)
remove(NA1718_Arthritis_BMI)
remove(NA1718_Arthritis_sex)
remove(NA1718_BMI_glm)
remove(NA1718_ci)
remove(NA1718_sex_glm)
remove(NA1718.Age)
remove(NA1718.BMI)
remove(NA1718.Sex)
remove(NA1718.c)
remove(NA1718.no)
remove(NA1718.no.ci)
remove(NA1718.no.df)
remove(NA1718.no.age)
remove(NA1718.no.age.c)
remove(NA1718.no.age.ci)
remove(NA1718.no.BMI)
remove(NA1718.no.BMI.c)
remove(NA1718.no.BMI.ci)
remove(NA1718.no.sex)
remove(NA1718.no.sex.c)
remove(NA1718.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

