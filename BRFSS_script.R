#Ioannides AE, Wadley AL, Kamerman PR
#Arthritis in the USA: a longitudinal analysis of three nationally representative studies

# -- Behavioural Risk Factor Surveillance System (BRFSS) analysis -- 
# Core years: 2001, 2003, 2005, 2007, 2009, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020
# Module years: 1996, 1997, 1998, 1999, 2000, 2002, 2004, 2010

#Notes:
#The term "Arthritis", in the context of the BRFSS, is used to describe a diagnosis of a rheumatic disorder by a healthcare professional

#Load packages
library(haven)
library(tidyverse)
library(survey)
library(gdata)
library(ggplot2)

options(survey.lonely.psu = "adjust")

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________


#----1996----
#Behavioural Risk Factor Surveillance System (BRFSS)
#1996
#----Download----

B96_url <- "http://www.cdc.gov/brfss/annual_data/1996/files/CDBRFS96XPT.zip"

tempB96 <- tempfile()
tempB96b <- tempfile()

download.file(B96_url, tempB96, mode = "wb")
unzip(zipfile = tempB96, exdir = tempB96b)
BRFSS96_raw <- read_xpt(file.path(tempB96b, "CDBRFS96.XPT"))

unlink(c(tempB96, tempB96b))

#_______________________________________________________________________________________________

#----Cleaning----

#Observe the dataset
str(BRFSS96_raw) 
tail(BRFSS96_raw) 
glimpse(BRFSS96_raw) 
colnames(BRFSS96_raw)

BRFSS96 <- select(BRFSS96_raw,
                  "_PSU", "_FINALWT", "_STSTR", "HAVARTH",
                  "AGE", "SEX", "_BMI") 

#Observations
str(BRFSS96)
tail(BRFSS96)
glimpse(BRFSS96)
colnames(BRFSS96)


#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS96)) #Large proportion of arthritis data "missing", which makes sense due to being a module year
which(colSums(is.na(BRFSS96)) == nrow(BRFSS96)) #Named integer = 0, which means that at least one reponse is present for the variables selected.


#HAVARTH

#Observe how many people answered yes, no, don't know, & refused
table(BRFSS96$HAVARTH)
#   1    2    7    9 
#3311 8987   62  582

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS96$HAVARTH <- recode(BRFSS96$HAVARTH,
                          "1" = "1",
                          "2" = "0",
                          "7" = "7",
                          "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS96$HAVARTH <- unknownToNA(BRFSS96$HAVARTH, unknown = c("7", "9")))
table(BRFSS96$HAVARTH)
#   0    1 
#8987 3311
BRFSS96$HAVARTH <- as.factor(BRFSS96$HAVARTH)
class(BRFSS96$HAVARTH)


#AGE

#Observe data
table(BRFSS96$AGE)
#Need to make this continuous data analogous to later BRFSS 5-year categories in order to make meaningful comparisons between years
#Recode age data into categories corresponding to later BRFSS codebook categories
BRFSS96$AGE<- recode(BRFSS96$AGE,
                     "7" = "7",
                     "9" = "9",
                     "18" = "Age 18 to 24",
                     "19" = "Age 18 to 24",
                     "20" = "Age 18 to 24",
                     "21" = "Age 18 to 24",
                     "22" = "Age 18 to 24",
                     "23" = "Age 18 to 24",
                     "24" = "Age 18 to 24",
                     "25" = "Age 25 to 29",
                     "26" = "Age 25 to 29",
                     "27" = "Age 25 to 29",
                     "28" = "Age 25 to 29",
                     "29" = "Age 25 to 29",
                     "30" = "Age 30 to 34",
                     "31" = "Age 30 to 34",
                     "32" = "Age 30 to 34",
                     "33" = "Age 30 to 34",
                     "34" = "Age 30 to 34",
                     "35" = "Age 35 to 39",
                     "36" = "Age 35 to 39",
                     "37" = "Age 35 to 39",
                     "38" = "Age 35 to 39",
                     "39" = "Age 35 to 39",
                     "40" = "Age 40 to 44",
                     "41" = "Age 40 to 44",
                     "42" = "Age 40 to 44",
                     "43" = "Age 40 to 44",
                     "44" = "Age 40 to 44",
                     "45" = "Age 45 to 49",
                     "46" = "Age 45 to 49",
                     "47" = "Age 45 to 49",
                     "48" = "Age 45 to 49",
                     "49" = "Age 45 to 49",
                     "50" = "Age 50 to 54",
                     "51" = "Age 50 to 54",
                     "52" = "Age 50 to 54",
                     "53" = "Age 50 to 54",
                     "54" = "Age 50 to 54",
                     "55" = "Age 55 to 59",
                     "56" = "Age 55 to 59",
                     "57" = "Age 55 to 59",
                     "58" = "Age 55 to 59",
                     "59" = "Age 55 to 59",
                     "60" = "Age 60 to 64",
                     "61" = "Age 60 to 64",
                     "62" = "Age 60 to 64",
                     "63" = "Age 60 to 64",
                     "64" = "Age 60 to 64",
                     "65" = "Age 65 to 69",
                     "66" = "Age 65 to 69",
                     "67" = "Age 65 to 69",
                     "68" = "Age 65 to 69", 
                     "69" = "Age 65 to 69",
                     "70" = "Age 70 and above",
                     "71" = "Age 70 and above",
                     "72" = "Age 70 and above",
                     "73" = "Age 70 and above",
                     "74" = "Age 70 and above",
                     "75" = "Age 70 and above",
                     "76" = "Age 70 and above",
                     "77" = "Age 70 and above",
                     "78" = "Age 70 and above",
                     "79" = "Age 70 and above",
                     "80" = "Age 70 and above",
                     "81" = "Age 70 and above",
                     "82" = "Age 70 and above",
                     "83" = "Age 70 and above",
                     "84" = "Age 70 and above",
                     "85" = "Age 70 and above",
                     "86" = "Age 70 and above",
                     "87" = "Age 70 and above",
                     "88" = "Age 70 and above",
                     "89" = "Age 70 and above",
                     "90" = "Age 70 and above",
                     "91" = "Age 70 and above",
                     "92" = "Age 70 and above",
                     "93" = "Age 70 and above",
                     "94" = "Age 70 and above",
                     "95" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "98" = "Age 70 and above",
                     "99" = "Age 70 and above")
#Change the unknown (7) and refused (9) values to NA
(BRFSS96$AGE <- unknownToNA(BRFSS96$AGE, unknown = c("7", "9")))
table(BRFSS96$AGE)

#    Age 18 to 24     Age 25 to 29     Age 30 to 34     Age 35 to 39     Age 40 to 44     Age 45 to 49     Age 50 to 54     Age 55 to 59     Age 60 to 64 
#           11089            11193            13280            14868            13728            11858             9118             7326             6713 
#Age 65 to 69         Age 70 and above 
#        7627                    16768 
BRFSS96$AGE <- as.factor(BRFSS96$AGE)
class(BRFSS96$AGE)


#SEX

#Observe data
table(BRFSS96$SEX)
#Recode sex data into categories corresponding to BRFSS codebook
BRFSS96$SEX <- recode(BRFSS96$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS96$SEX)
#Female   Male 
# 72997  51088
BRFSS96$SEX <- as.factor(BRFSS96$SEX)
class(BRFSS96$SEX)


#BMI

#Observe data
table(BRFSS96$`_BMI`)
#one implied decimal place, therefore pull comma back one place
BRFSS96 <- BRFSS96 %>%
  mutate(`_BMI` = `_BMI`/10)
BRFSS96$`_BMI` <- unknownToNA(BRFSS96$`_BMI`, unknown = "99.9") #as per codebook
BRFSS96 %>% top_n(10, `_BMI`) #check that 99.9 does not appear

#Create another BMI variable using BMI categories for figures
BRFSS96$BMICAT <- ifelse(BRFSS96$`_BMI` < 18.5, "Underweight",
                  ifelse(BRFSS96$`_BMI` >= 18.5 & BRFSS96$`_BMI` < 25.0, "Healthy weight",
                  ifelse(BRFSS96$`_BMI` >= 25.0 & BRFSS96$`_BMI` < 30.0, "Overweight",
                  ifelse(BRFSS96$`_BMI` >= 30.0, "Obese",
                  NA))))

table(BRFSS96$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#         55325          19444          41406           3010


#__________________________________________________________________________________________________________________
#DESIGN OBJECT CREATION

#Some final checks:
options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS96_dataset <- subset(BRFSS96,
                          !is.na(`_PSU`) &
                          !is.na(`_STSTR`) &
                          !is.na(`_FINALWT`) &
                          !is.na(HAVARTH))

#Check that the sum of the weights is equal to the US population
sum(BRFSS96_dataset$`_FINALWT`)
#The sum of the weights is 17 638 547 {MODULE YEAR SO REDUCED POPULATION SIZE TO BE EXPECTED}

#Check the number of people (unique PSU's)
length(unique(BRFSS96_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 7 926

#Check the number of unique strata
length(unique(BRFSS96_dataset[["_STSTR"]]))
#The number of unique strata is 15

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS96_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_FINALWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS96_dataset)
#Observe the design oject
BRFSS96_DO

#_______________________________________________________________________________________________

#----Analysis----

#1. Overall prevalence
B96_overall <- svymean(~factor(HAVARTH),
                       BRFSS96_DO,
                       na.rm = TRUE)
B96_overall.c <- B96_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B96_overall_ci <- confint(B96_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B96 <- bind_cols(B96_overall.c, B96_overall_ci)
#remove havarth = 0
B96 <- B96[-c(1), ] #B96 = final proportion, se and 95% ci


#2. Demographic analysis

#A. Arthritis & Age
B96_Arth_age <- svyby(formula = ~HAVARTH,
                      by = ~AGE,
                      design = BRFSS96_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B96_Arthtitis_age <- B96_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B96_arth_age_ci <- confint(B96_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0 (No)
B96_arth_age_ci <- B96_arth_age_ci[-c(1:11), ]
#join ci and proportions
B96.Age <- bind_cols(B96_Arthtitis_age, B96_arth_age_ci) #B96.Age = final proportion, se and 95% ci by age group

#Age logistic regression
B96_age_glm <- svyglm(HAVARTH~AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS96_DO)
summary(B96_age_glm)
exp(cbind(OR=coef(B96_age_glm), confint(B96_age_glm)))



#B. Arthritis & Sex
B96_Arth_sex <- svyby(formula = ~HAVARTH,
                      by = ~SEX,
                      design = BRFSS96_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B96_Arthritis_sex <- B96_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B96_arth_sex_ci <- confint(B96_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0 (No)
B96_arth_sex_ci <- B96_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B96.Sex <- bind_cols(B96_Arthritis_sex, B96_arth_sex_ci) #B96.Sex = final proportion, se and 95% ci by sex

#Sex logistic regression
B96_sex_glm <- svyglm(HAVARTH~relevel(SEX, ref = "Male") + AGE,
                      family = quasibinomial,
                      design = BRFSS96_DO)
summary(B96_sex_glm)
exp(cbind(OR=coef(B96_sex_glm), confint(B96_sex_glm)))


#C. Arthritis & BMI
B96_Arth_BMI <- svyby(formula = ~HAVARTH,
                      by = ~BMICAT,
                      design = BRFSS96_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B96_Arthritis_BMI <- B96_Arth_BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Proportion", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B96_arth_BMI_ci <- confint(B96_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0
B96_arth_BMI_ci <- B96_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
B96.BMI <- bind_cols(B96_Arthritis_BMI, B96_arth_BMI_ci) #B96.BMI = final proportion, se and 95%ci by BMI


#BMI logistic regression (using original continuous variable from survey)
B96.BMI.glm <- svyglm(HAVARTH ~ `_BMI` + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS96_DO)
summary(B96.BMI.glm)
exp(cbind(OR=coef(B96.BMI.glm), confint(B96.BMI.glm)))

#   End of 1996 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(BRFSS96)
remove(BRFSS96_dataset)
remove(BRFSS96_DO)
remove(BRFSS96_raw)
remove(B96_Arth_age)
remove(B96_arth_age_ci)
remove(B96_Arth_BMI)
remove(B96_arth_BMI_ci)
remove(B96_Arth_sex)
remove(B96_arth_sex_ci)
remove(B96_Arthritis_BMI)
remove(B96_Arthritis_sex)
remove(B96_Arthtitis_age)
remove(B96_arth_age_ci)
remove(B96_overall.c)
remove(B96_overall_ci)
remove(B96.no.age.c)
remove(B96.no.age.ci)
remove(B96.no.BMI.c)
remove(B96.no.BMI.ci)
remove(B96.no.df)
remove(B96.no.ci)
remove(B96.no.sex.c)
remove(B96.no.sex.ci)
remove(B96_age_glm)
remove(B96_sex_glm)
remove(B96.BMI.glm)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

#Behavioural Risk Factor Surveillance System (BRFSS)
#1997




#----1997----
#----Download----

B97_url <- "http://www.cdc.gov/brfss/annual_data/1997/files/CDBRFS97XPT.zip"

tempB97 <- tempfile()
tempB97b <- tempfile()

download.file(B97_url, tempB97, mode = "wb")
unzip(zipfile = tempB97, exdir = tempB97b)
BRFSS97_raw <- read_xpt(file.path(tempB97b, "CDBRFS97.XPT"))

unlink(c(tempB97, tempB97b))

#_______________________________________________________________________________________________

#----Cleaning----

#Observe the dataset
str(BRFSS97_raw) 
tail(BRFSS97_raw) 
glimpse(BRFSS97_raw) 
colnames(BRFSS97_raw)

BRFSS97 <- select(BRFSS97_raw,
                  "_PSU", "_FINALWT", "_STSTR", "HAVARTH",
                  "AGE", "SEX", "_BMI") 

#Observations
str(BRFSS97)
tail(BRFSS97)
glimpse(BRFSS97)
colnames(BRFSS97)


#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS97)) #Large proportion of arthritis data "missing", which makes sense due to being a module year
which(colSums(is.na(BRFSS97)) == nrow(BRFSS97)) #Named integer = 0, which means that at least one reponse is present for the variables selected.


#HAVARTH

#Observe how many people answered yes, no, don't know, & refused
table(BRFSS97$HAVARTH)
#   1    2    7    9 
#2373 7728   49  360

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS97$HAVARTH <- recode(BRFSS97$HAVARTH,
                          "1" = "1",
                          "2" = "0",
                          "7" = "7",
                          "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS97$HAVARTH <- unknownToNA(BRFSS97$HAVARTH, unknown = c("7", "9")))
table(BRFSS97$HAVARTH)
#   0    1 
#7728 2373
BRFSS97$HAVARTH <- as.factor(BRFSS97$HAVARTH)
class(BRFSS97$HAVARTH)


#AGE

#Observe data
table(BRFSS97$AGE)
#Need to make this continuous data analogous to later BRFSS 5-year categories in order to make meaningful comparisons between years
#Recode age data into categories corresponding to later BRFSS codebook categories
BRFSS97$AGE<- recode(BRFSS97$AGE,
                     "7" = "7",
                     "9" = "9",
                     "18" = "Age 18 to 24",
                     "19" = "Age 18 to 24",
                     "20" = "Age 18 to 24",
                     "21" = "Age 18 to 24",
                     "22" = "Age 18 to 24",
                     "23" = "Age 18 to 24",
                     "24" = "Age 18 to 24",
                     "25" = "Age 25 to 29",
                     "26" = "Age 25 to 29",
                     "27" = "Age 25 to 29",
                     "28" = "Age 25 to 29",
                     "29" = "Age 25 to 29",
                     "30" = "Age 30 to 34",
                     "31" = "Age 30 to 34",
                     "32" = "Age 30 to 34",
                     "33" = "Age 30 to 34",
                     "34" = "Age 30 to 34",
                     "35" = "Age 35 to 39",
                     "36" = "Age 35 to 39",
                     "37" = "Age 35 to 39",
                     "38" = "Age 35 to 39",
                     "39" = "Age 35 to 39",
                     "40" = "Age 40 to 44",
                     "41" = "Age 40 to 44",
                     "42" = "Age 40 to 44",
                     "43" = "Age 40 to 44",
                     "44" = "Age 40 to 44",
                     "45" = "Age 45 to 49",
                     "46" = "Age 45 to 49",
                     "47" = "Age 45 to 49",
                     "48" = "Age 45 to 49",
                     "49" = "Age 45 to 49",
                     "50" = "Age 50 to 54",
                     "51" = "Age 50 to 54",
                     "52" = "Age 50 to 54",
                     "53" = "Age 50 to 54",
                     "54" = "Age 50 to 54",
                     "55" = "Age 55 to 59",
                     "56" = "Age 55 to 59",
                     "57" = "Age 55 to 59",
                     "58" = "Age 55 to 59",
                     "59" = "Age 55 to 59",
                     "60" = "Age 60 to 64",
                     "61" = "Age 60 to 64",
                     "62" = "Age 60 to 64",
                     "63" = "Age 60 to 64",
                     "64" = "Age 60 to 64",
                     "65" = "Age 65 to 69",
                     "66" = "Age 65 to 69",
                     "67" = "Age 65 to 69",
                     "68" = "Age 65 to 69", 
                     "69" = "Age 65 to 69",
                     "70" = "Age 70 and above",
                     "71" = "Age 70 and above",
                     "72" = "Age 70 and above",
                     "73" = "Age 70 and above",
                     "74" = "Age 70 and above",
                     "75" = "Age 70 and above",
                     "76" = "Age 70 and above",
                     "77" = "Age 70 and above",
                     "78" = "Age 70 and above",
                     "79" = "Age 70 and above",
                     "80" = "Age 70 and above",
                     "81" = "Age 70 and above",
                     "82" = "Age 70 and above",
                     "83" = "Age 70 and above",
                     "84" = "Age 70 and above",
                     "85" = "Age 70 and above",
                     "86" = "Age 70 and above",
                     "87" = "Age 70 and above",
                     "88" = "Age 70 and above",
                     "89" = "Age 70 and above",
                     "90" = "Age 70 and above",
                     "91" = "Age 70 and above",
                     "92" = "Age 70 and above",
                     "93" = "Age 70 and above",
                     "94" = "Age 70 and above",
                     "95" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "98" = "Age 70 and above",
                     "99" = "Age 70 and above")
#Change the unknown (7) and refused (9) values to NA
(BRFSS97$AGE <- unknownToNA(BRFSS97$AGE, unknown = c("7", "9")))
table(BRFSS97$AGE)

#    Age 18 to 24     Age 25 to 29     Age 30 to 34     Age 35 to 39     Age 40 to 44     Age 45 to 49     Age 50 to 54     Age 55 to 59     Age 60 to 64 
#           11822            12065            13859            16012            14907            12955            10573             8417             7390 
#Age 65 to 69         Age 70 and above 
#        8114                    18752
BRFSS97$AGE <- as.factor(BRFSS97$AGE)
class(BRFSS97$AGE)


#SEX

#Observe data
table(BRFSS97$SEX)
#Recode sex data into categories corresponding to BRFSS codebook
BRFSS97$SEX <- recode(BRFSS97$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS97$SEX)
#Female   Male 
# 79506  56076
BRFSS97$SEX <- as.factor(BRFSS97$SEX)
class(BRFSS97$SEX)


#BMI

#Observe data
table(BRFSS97$`_BMI`)
#one implied decimal place, therefore pull comma back one place
BRFSS97 <- BRFSS97 %>%
  mutate(`_BMI` = `_BMI`/10)
BRFSS97$`_BMI` <- unknownToNA(BRFSS97$`_BMI`, unknown = "99.9") #as per codebook
BRFSS97 %>% top_n(10, `_BMI`) #check that 99.9 does not appear

#Create another BMI variable using BMI categories for figures
BRFSS97$BMICAT <- ifelse(BRFSS97$`_BMI` < 18.5, "Underweight",
                  ifelse(BRFSS97$`_BMI` >= 18.5 & BRFSS97$`_BMI` < 25.0, "Healthy weight",
                  ifelse(BRFSS97$`_BMI` >= 25.0 & BRFSS97$`_BMI` < 30.0, "Overweight",
                  ifelse(BRFSS97$`_BMI` >= 30.0, "Obese",
                  NA))))

table(BRFSS97$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#         58938          22220          46511           3036


#__________________________________________________________________________________________________________________
#DESIGN OBJECT CREATION

#Some final checks:
options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS97_dataset <- subset(BRFSS97,
                          !is.na(`_PSU`) &
                          !is.na(`_STSTR`) &
                          !is.na(`_FINALWT`) &
                          !is.na(HAVARTH))

#Check that the sum of the weights is equal to the US population
sum(BRFSS97_dataset$`_FINALWT`)
#The sum of the weights is 13 483 408 {MODULE YEAR SO REDUCED POPULATION SIZE TO BE EXPECTED}

#Check the number of people (unique PSU's)
length(unique(BRFSS97_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 6 516

#Check the number of unique strata
length(unique(BRFSS97_dataset[["_STSTR"]]))
#The number of unique strata is 12

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS97_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_FINALWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS97_dataset)
#Observe the design oject
BRFSS97_DO

#_______________________________________________________________________________________________

#----Analysis----

#1. Overall prevalence
B97_overall <- svymean(~factor(HAVARTH),
                       BRFSS97_DO,
                       na.rm = TRUE)
B97_overall.c <- B97_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B97_overall_ci <- confint(B97_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B97 <- bind_cols(B97_overall.c, B97_overall_ci)
#remove havarth = 0
B97 <- B97[-c(1), ] #B97 = final proportion, se and 95% ci


#2. Demographic analysis

#A. Arthritis & Age
B97_Arth_age <- svyby(formula = ~HAVARTH,
                      by = ~AGE,
                      design = BRFSS97_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B97_Arthtitis_age <- B97_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B97_arth_age_ci <- confint(B97_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0 (No)
B97_arth_age_ci <- B97_arth_age_ci[-c(1:11), ]
#join ci and proportions
B97.Age <- bind_cols(B97_Arthtitis_age, B97_arth_age_ci) #B97.Age = final proportion, se and 95% ci by age group

#Age logistic regression
B97_age_glm <- svyglm(HAVARTH~AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS97_DO)
summary(B97_age_glm)
exp(cbind(OR=coef(B97_age_glm), confint(B97_age_glm)))



#B. Arthritis & Sex
B97_Arth_sex <- svyby(formula = ~HAVARTH,
                      by = ~SEX,
                      design = BRFSS97_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B97_Arthritis_sex <- B97_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B97_arth_sex_ci <- confint(B97_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0 (No)
B97_arth_sex_ci <- B97_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B97.Sex <- bind_cols(B97_Arthritis_sex, B97_arth_sex_ci) #B97.Sex = final proportion, se and 95% ci by sex

#Sex logistic regression
B97_sex_glm <- svyglm(HAVARTH~relevel(SEX, ref = "Male") + AGE,
                      family = quasibinomial,
                      design = BRFSS97_DO)
summary(B97_sex_glm)
exp(cbind(OR=coef(B97_sex_glm), confint(B97_sex_glm)))


#C. Arthritis & BMI
B97_Arth_BMI <- svyby(formula = ~HAVARTH,
                      by = ~BMICAT,
                      design = BRFSS97_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B97_Arthritis_BMI <- B97_Arth_BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Proportion", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B97_arth_BMI_ci <- confint(B97_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0
B97_arth_BMI_ci <- B97_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
B97.BMI <- bind_cols(B97_Arthritis_BMI, B97_arth_BMI_ci) #B97.BMI = final proportion, se and 95%ci by BMI

#BMI logistic regression (using original continuous variable from survey)
B97.BMI.glm <- svyglm(HAVARTH ~ `_BMI` + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS97_DO)
summary(B97.BMI.glm)
exp(cbind(OR=coef(B97.BMI.glm), confint(B97.BMI.glm)))


#   End of 1997 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(BRFSS97)
remove(BRFSS97_dataset)
remove(BRFSS97_DO)
remove(BRFSS97_raw)
remove(B97_Arth_age)
remove(B97_arth_age_ci)
remove(B97_Arth_BMI)
remove(B97_arth_BMI_ci)
remove(B97_Arth_sex)
remove(B97_arth_sex_ci)
remove(B97_Arthritis_BMI)
remove(B97_Arthritis_sex)
remove(B97_Arthtitis_age)
remove(B97_arth_age_ci)
remove(B97_overall.c)
remove(B97_overall_ci)
remove(B97.no.age.c)
remove(B97.no.age.ci)
remove(B97.no.BMI.c)
remove(B97.no.BMI.ci)
remove(B97.no.df)
remove(B97.no.ci)
remove(B97.no.sex.c)
remove(B97.no.sex.ci)
remove(B97_age_glm)
remove(B97_sex_glm)
remove(B97.BMI.glm)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________


#Behavioural Risk Factor Surveillance System (BRFSS)
#1998



#----1998----

#----Download----

B98_url <- "http://www.cdc.gov/brfss/annual_data/1998/files/CDBRFS98XPT.zip"

tempB98 <- tempfile()
tempB98b <- tempfile()

download.file(B98_url, tempB98, mode = "wb")
unzip(zipfile = tempB98, exdir = tempB98b)
BRFSS98_raw <- read_xpt(file.path(tempB98b, "CDBRFS98.XPT"))

unlink(c(tempB98, tempB98b))

#_______________________________________________________________________________________________

#----Cleaning----

#Observe the dataset
str(BRFSS98_raw) 
tail(BRFSS98_raw) 
glimpse(BRFSS98_raw) 
colnames(BRFSS98_raw)

BRFSS98 <- select(BRFSS98_raw,
                  "_PSU", "_FINALWT", "_STSTR", "HAVARTH",
                  "AGE", "SEX", "_BMI") 

#Observations
str(BRFSS98)
tail(BRFSS98)
glimpse(BRFSS98)
colnames(BRFSS98)


#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS98)) #Large proportion of arthritis data "missing", which makes sense due to being a module year
which(colSums(is.na(BRFSS98)) == nrow(BRFSS98)) #Named integer = 0, which means that at least one reponse is present for the variables selected.


#HAVARTH

#Observe how many people answered yes, no, don't know, & refused
table(BRFSS98$HAVARTH)
#   1    2    7    9 
#2292 7220   37  202 

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS98$HAVARTH <- recode(BRFSS98$HAVARTH,
                          "1" = "1",
                          "2" = "0",
                          "7" = "7",
                          "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS98$HAVARTH <- unknownToNA(BRFSS98$HAVARTH, unknown = c("7", "9")))
table(BRFSS98$HAVARTH)
#   0    1 
#7220 2292 
BRFSS98$HAVARTH <- as.factor(BRFSS98$HAVARTH)
class(BRFSS98$HAVARTH)


#AGE

#Observe data
table(BRFSS98$AGE)
#Need to make this continuous data analogous to later BRFSS 5-year categories in order to make meaningful comparisons between years
#Recode age data into categories corresponding to later BRFSS codebook categories
BRFSS98$AGE<- recode(BRFSS98$AGE,
                     "7" = "7",
                     "9" = "9",
                     "18" = "Age 18 to 24",
                     "19" = "Age 18 to 24",
                     "20" = "Age 18 to 24",
                     "21" = "Age 18 to 24",
                     "22" = "Age 18 to 24",
                     "23" = "Age 18 to 24",
                     "24" = "Age 18 to 24",
                     "25" = "Age 25 to 29",
                     "26" = "Age 25 to 29",
                     "27" = "Age 25 to 29",
                     "28" = "Age 25 to 29",
                     "29" = "Age 25 to 29",
                     "30" = "Age 30 to 34",
                     "31" = "Age 30 to 34",
                     "32" = "Age 30 to 34",
                     "33" = "Age 30 to 34",
                     "34" = "Age 30 to 34",
                     "35" = "Age 35 to 39",
                     "36" = "Age 35 to 39",
                     "37" = "Age 35 to 39",
                     "38" = "Age 35 to 39",
                     "39" = "Age 35 to 39",
                     "40" = "Age 40 to 44",
                     "41" = "Age 40 to 44",
                     "42" = "Age 40 to 44",
                     "43" = "Age 40 to 44",
                     "44" = "Age 40 to 44",
                     "45" = "Age 45 to 49",
                     "46" = "Age 45 to 49",
                     "47" = "Age 45 to 49",
                     "48" = "Age 45 to 49",
                     "49" = "Age 45 to 49",
                     "50" = "Age 50 to 54",
                     "51" = "Age 50 to 54",
                     "52" = "Age 50 to 54",
                     "53" = "Age 50 to 54",
                     "54" = "Age 50 to 54",
                     "55" = "Age 55 to 59",
                     "56" = "Age 55 to 59",
                     "57" = "Age 55 to 59",
                     "58" = "Age 55 to 59",
                     "59" = "Age 55 to 59",
                     "60" = "Age 60 to 64",
                     "61" = "Age 60 to 64",
                     "62" = "Age 60 to 64",
                     "63" = "Age 60 to 64",
                     "64" = "Age 60 to 64",
                     "65" = "Age 65 to 69",
                     "66" = "Age 65 to 69",
                     "67" = "Age 65 to 69",
                     "68" = "Age 65 to 69", 
                     "69" = "Age 65 to 69",
                     "70" = "Age 70 and above",
                     "71" = "Age 70 and above",
                     "72" = "Age 70 and above",
                     "73" = "Age 70 and above",
                     "74" = "Age 70 and above",
                     "75" = "Age 70 and above",
                     "76" = "Age 70 and above",
                     "77" = "Age 70 and above",
                     "78" = "Age 70 and above",
                     "79" = "Age 70 and above",
                     "80" = "Age 70 and above",
                     "81" = "Age 70 and above",
                     "82" = "Age 70 and above",
                     "83" = "Age 70 and above",
                     "84" = "Age 70 and above",
                     "85" = "Age 70 and above",
                     "86" = "Age 70 and above",
                     "87" = "Age 70 and above",
                     "88" = "Age 70 and above",
                     "89" = "Age 70 and above",
                     "90" = "Age 70 and above",
                     "91" = "Age 70 and above",
                     "92" = "Age 70 and above",
                     "93" = "Age 70 and above",
                     "94" = "Age 70 and above",
                     "95" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "98" = "Age 70 and above",
                     "99" = "Age 70 and above")
#Change the unknown (7) and refused (9) values to NA
(BRFSS98$AGE <- unknownToNA(BRFSS98$AGE, unknown = c("7", "9")))
table(BRFSS98$AGE)

#    Age 18 to 24     Age 25 to 29     Age 30 to 34     Age 35 to 39     Age 40 to 44     Age 45 to 49     Age 50 to 54     Age 55 to 59     Age 60 to 64 
#           13214            13115            14880            17202            16509            14451            12310             9608             8358 
#    Age 65 to 69 Age 70 and above 
#            8559            20461 
BRFSS98$AGE <- as.factor(BRFSS98$AGE)
class(BRFSS98$AGE)


#SEX

#Observe data
table(BRFSS98$SEX)
#Recode sex data into categories corresponding to BRFSS codebook
BRFSS98$SEX <- recode(BRFSS98$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS98$SEX)
#Female   Male 
# 88192  61150
BRFSS98$SEX <- as.factor(BRFSS98$SEX)
class(BRFSS98$SEX)


#BMI

#Observe data
table(BRFSS98$`_BMI`)
#one implied decimal place, therefore pull comma back one place
BRFSS98 <- BRFSS98 %>%
  mutate(`_BMI` = `_BMI`/10)
BRFSS98$`_BMI` <- unknownToNA(BRFSS98$`_BMI`, unknown = "99.9") #as per codebook
BRFSS98 %>% top_n(10, `_BMI`) #check that 99.9 does not appear

#Create another BMI variable using BMI categories for figures
BRFSS98$BMICAT <- ifelse(BRFSS98$`_BMI` < 18.5, "Underweight",
                  ifelse(BRFSS98$`_BMI` >= 18.5 & BRFSS98$`_BMI` < 25.0, "Healthy weight",
                  ifelse(BRFSS98$`_BMI` >= 25.0 & BRFSS98$`_BMI` < 30.0, "Overweight",
                  ifelse(BRFSS98$`_BMI` >= 30.0, "Obese",
                  NA))))

table(BRFSS98$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#         63169          26285          50691           3284 


#__________________________________________________________________________________________________________________
#DESIGN OBJECT CREATION

#Some final checks:
options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS98_dataset <- subset(BRFSS98,
                          !is.na(`_PSU`) &
                          !is.na(`_STSTR`) &
                          !is.na(`_FINALWT`) &
                          !is.na(HAVARTH))

#Check that the sum of the weights is equal to the US population
sum(BRFSS98_dataset$`_FINALWT`)
#The sum of the weights is 22 910 818 {MODULE YEAR SO REDUCED POPULATION SIZE TO BE EXPECTED}

#Check the number of people (unique PSU's)
length(unique(BRFSS98_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 8 483

#Check the number of unique strata
length(unique(BRFSS98_dataset[["_STSTR"]]))
#The number of unique strata is 24

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS98_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_FINALWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS98_dataset)
#Observe the design oject
BRFSS98_DO
#_______________________________________________________________________________________________

#----Analysis----

#1. Overall
# Prevalence
B98_overall <- svymean(~factor(HAVARTH),
                       BRFSS98_DO,
                       na.rm = TRUE)
B98_overall.c <- B98_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B98_overall_ci <- confint(B98_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B98 <- bind_cols(B98_overall.c, B98_overall_ci)
#remove havarth = 0
B98 <- B98[-c(1), ] #B98 = final proportion, se and 95% ci

#2. Demographic analysis

#A. Arthritis & Age
B98_Arth_age <- svyby(formula = ~HAVARTH,
                      by = ~AGE,
                      design = BRFSS98_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B98_Arthtitis_age <- B98_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B98_arth_age_ci <- confint(B98_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0 (No)
B98_arth_age_ci <- B98_arth_age_ci[-c(1:11), ]
#join ci and proportions
B98.Age <- bind_cols(B98_Arthtitis_age, B98_arth_age_ci) #B98.Age = final proportion, se and 95% ci by age group

#Age logistic regression
B98_age_glm <- svyglm(HAVARTH~AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS98_DO)
summary(B98_age_glm)
exp(cbind(OR=coef(B98_age_glm), confint(B98_age_glm)))



#B. Arthritis & Sex
B98_Arth_sex <- svyby(formula = ~HAVARTH,
                      by = ~SEX,
                      design = BRFSS98_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B98_Arthritis_sex <- B98_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B98_arth_sex_ci <- confint(B98_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0 (No)
B98_arth_sex_ci <- B98_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B98.Sex <- bind_cols(B98_Arthritis_sex, B98_arth_sex_ci) #B98.Sex = final proportion, se and 95% ci by sex

#Sex logistic regression
B98_sex_glm <- svyglm(HAVARTH~relevel(SEX, ref = "Male") + AGE,
                      family = quasibinomial,
                      design = BRFSS98_DO)
summary(B98_sex_glm)
exp(cbind(OR=coef(B98_sex_glm), confint(B98_sex_glm)))


#C. Arthritis & BMI
B98_Arth_BMI <- svyby(formula = ~HAVARTH,
                      by = ~BMICAT,
                      design = BRFSS98_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B98_Arthritis_BMI <- B98_Arth_BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Proportion", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B98_arth_BMI_ci <- confint(B98_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0
B98_arth_BMI_ci <- B98_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
B98.BMI <- bind_cols(B98_Arthritis_BMI, B98_arth_BMI_ci) #B98.BMI = final proportion, se and 95%ci by BMI

#BMI logistic regression (using original continuous variable from survey)
B98.BMI.glm <- svyglm(HAVARTH ~ `_BMI` + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS98_DO)
summary(B98.BMI.glm)
exp(cbind(OR=coef(B98.BMI.glm), confint(B98.BMI.glm)))


#   End of 1998 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(BRFSS98)
remove(BRFSS98_dataset)
remove(BRFSS98_DO)
remove(BRFSS98_raw)
remove(B98_Arth_age)
remove(B98_arth_age_ci)
remove(B98_Arth_BMI)
remove(B98_arth_BMI_ci)
remove(B98_Arth_sex)
remove(B98_arth_sex_ci)
remove(B98_Arthritis_BMI)
remove(B98_Arthritis_sex)
remove(B98_Arthtitis_age)
remove(B98_arth_age_ci)
remove(B98_overall.c)
remove(B98_overall_ci)
remove(B98.no.age.c)
remove(B98.no.age.ci)
remove(B98.no.BMI.c)
remove(B98.no.BMI.ci)
remove(B98.no.df)
remove(B98.no.ci)
remove(B98.no.sex.c)
remove(B98.no.sex.ci)
remove(B98_age_glm)
remove(B98_sex_glm)
remove(B98.BMI.glm)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

#Behavioural Risk Factor Surveillance System (BRFSS)
#1999



#----1999----


#----Download----

B99_url <- "http://www.cdc.gov/brfss/annual_data/1999/files/CDBRFS99XPT.zip"

tempB99 <- tempfile()
tempB99b <- tempfile()

download.file(B99_url, tempB99, mode = "wb")
unzip(zipfile = tempB99, exdir = tempB99b)
BRFSS99_raw <- read_xpt(file.path(tempB99b, "CDBRFS99.XPT"))

unlink(c(tempB99, tempB99b))

#_______________________________________________________________________________________________

#----Cleaning----

#Observe the dataset
str(BRFSS99_raw) 
tail(BRFSS99_raw) 
glimpse(BRFSS99_raw) 
colnames(BRFSS99_raw)

BRFSS99 <- select(BRFSS99_raw,
                  "_PSU", "_FINALWT", "_STSTR", "HAVARTH",
                  "AGE", "SEX", "_BMI") 

#Observations
str(BRFSS99)
tail(BRFSS99)
glimpse(BRFSS99)
colnames(BRFSS99)


#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS99)) #Large proportion of arthritis data "missing", which makes sense due to being a module year
which(colSums(is.na(BRFSS99)) == nrow(BRFSS99)) #Named integer = 0, which means that at least one reponse is present for the variables selected.


#HAVARTH

#Observe how many people answered yes, no, don't know, & refused
table(BRFSS99$HAVARTH)
#   1     2     7     9 
#5870 15978    66   309 

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS99$HAVARTH <- recode(BRFSS99$HAVARTH,
                          "1" = "1",
                          "2" = "0",
                          "7" = "7",
                          "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS99$HAVARTH <- unknownToNA(BRFSS99$HAVARTH, unknown = c("7", "9")))
table(BRFSS99$HAVARTH)
#    0     1 
#15978  5870
BRFSS99$HAVARTH <- as.factor(BRFSS99$HAVARTH)
class(BRFSS99$HAVARTH)


#AGE

#Observe data
table(BRFSS99$AGE)
#Need to make this continuous data analogous to later BRFSS 5-year categories in order to make meaningful comparisons between years
#Recode age data into categories corresponding to later BRFSS codebook categories
BRFSS99$AGE<- recode(BRFSS99$AGE,
                     "7" = "7",
                     "9" = "9",
                     "18" = "Age 18 to 24",
                     "19" = "Age 18 to 24",
                     "20" = "Age 18 to 24",
                     "21" = "Age 18 to 24",
                     "22" = "Age 18 to 24",
                     "23" = "Age 18 to 24",
                     "24" = "Age 18 to 24",
                     "25" = "Age 25 to 29",
                     "26" = "Age 25 to 29",
                     "27" = "Age 25 to 29",
                     "28" = "Age 25 to 29",
                     "29" = "Age 25 to 29",
                     "30" = "Age 30 to 34",
                     "31" = "Age 30 to 34",
                     "32" = "Age 30 to 34",
                     "33" = "Age 30 to 34",
                     "34" = "Age 30 to 34",
                     "35" = "Age 35 to 39",
                     "36" = "Age 35 to 39",
                     "37" = "Age 35 to 39",
                     "38" = "Age 35 to 39",
                     "39" = "Age 35 to 39",
                     "40" = "Age 40 to 44",
                     "41" = "Age 40 to 44",
                     "42" = "Age 40 to 44",
                     "43" = "Age 40 to 44",
                     "44" = "Age 40 to 44",
                     "45" = "Age 45 to 49",
                     "46" = "Age 45 to 49",
                     "47" = "Age 45 to 49",
                     "48" = "Age 45 to 49",
                     "49" = "Age 45 to 49",
                     "50" = "Age 50 to 54",
                     "51" = "Age 50 to 54",
                     "52" = "Age 50 to 54",
                     "53" = "Age 50 to 54",
                     "54" = "Age 50 to 54",
                     "55" = "Age 55 to 59",
                     "56" = "Age 55 to 59",
                     "57" = "Age 55 to 59",
                     "58" = "Age 55 to 59",
                     "59" = "Age 55 to 59",
                     "60" = "Age 60 to 64",
                     "61" = "Age 60 to 64",
                     "62" = "Age 60 to 64",
                     "63" = "Age 60 to 64",
                     "64" = "Age 60 to 64",
                     "65" = "Age 65 to 69",
                     "66" = "Age 65 to 69",
                     "67" = "Age 65 to 69",
                     "68" = "Age 65 to 69", 
                     "69" = "Age 65 to 69",
                     "70" = "Age 70 and above",
                     "71" = "Age 70 and above",
                     "72" = "Age 70 and above",
                     "73" = "Age 70 and above",
                     "74" = "Age 70 and above",
                     "75" = "Age 70 and above",
                     "76" = "Age 70 and above",
                     "77" = "Age 70 and above",
                     "78" = "Age 70 and above",
                     "79" = "Age 70 and above",
                     "80" = "Age 70 and above",
                     "81" = "Age 70 and above",
                     "82" = "Age 70 and above",
                     "83" = "Age 70 and above",
                     "84" = "Age 70 and above",
                     "85" = "Age 70 and above",
                     "86" = "Age 70 and above",
                     "87" = "Age 70 and above",
                     "88" = "Age 70 and above",
                     "89" = "Age 70 and above",
                     "90" = "Age 70 and above",
                     "91" = "Age 70 and above",
                     "92" = "Age 70 and above",
                     "93" = "Age 70 and above",
                     "94" = "Age 70 and above",
                     "95" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "98" = "Age 70 and above",
                     "99" = "Age 70 and above")
#Change the unknown (7) and refused (9) values to NA
(BRFSS99$AGE <- unknownToNA(BRFSS99$AGE, unknown = c("7", "9")))
table(BRFSS99$AGE)

#   Age 18 to 24     Age 25 to 29     Age 30 to 34     Age 35 to 39     Age 40 to 44     Age 45 to 49     Age 50 to 54 
#          14541            13789            15456            17967            17584            15755            13533 
#Age 55 to 59     Age 60 to 64     Age 65 to 69 Age 70 and above 
#       10804             9043             9138            21517
BRFSS99$AGE <- as.factor(BRFSS99$AGE)
class(BRFSS99$AGE)


#SEX

#Observe data
table(BRFSS99$SEX)
#Recode sex data into categories corresponding to BRFSS codebook
BRFSS99$SEX <- recode(BRFSS99$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS99$SEX)
#Female   Male 
# 94679  65310
BRFSS99$SEX <- as.factor(BRFSS99$SEX)
class(BRFSS99$SEX)


#BMI

#Observe data
table(BRFSS99$`_BMI`)
#one implied decimal place, therefore pull comma back one place
BRFSS99 <- BRFSS99 %>%
  mutate(`_BMI` = `_BMI`/10)
BRFSS99$`_BMI` <- unknownToNA(BRFSS99$`_BMI`, unknown = "99.9") #as per codebook
BRFSS99 %>% top_n(10, `_BMI`) #check that 99.9 does not appear

#Create another BMI variable using BMI categories for figures
BRFSS99$BMICAT <- ifelse(BRFSS99$`_BMI` < 18.5, "Underweight",
                  ifelse(BRFSS99$`_BMI` >= 18.5 & BRFSS99$`_BMI` < 25.0, "Healthy weight",
                  ifelse(BRFSS99$`_BMI` >= 25.0 & BRFSS99$`_BMI` < 30.0, "Overweight",
                  ifelse(BRFSS99$`_BMI` >= 30.0, "Obese",
                  NA))))

table(BRFSS99$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#         65236          29907          55288           3379


#__________________________________________________________________________________________________________________
#DESIGN OBJECT CREATION

#Some final checks:
options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS99_dataset <- subset(BRFSS99,
                          !is.na(`_PSU`) &
                          !is.na(`_STSTR`) &
                          !is.na(`_FINALWT`) &
                          !is.na(HAVARTH))

#Check that the sum of the weights is equal to the US population
sum(BRFSS99_dataset$`_FINALWT`)
#The sum of the weights is 23 533 065 {MODULE YEAR SO REDUCED POPULATION SIZE TO BE EXPECTED}

#Check the number of people (unique PSU's)
length(unique(BRFSS99_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 14 057

#Check the number of unique strata
length(unique(BRFSS99_dataset[["_STSTR"]]))
#The number of unique strata is 28

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS99_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_FINALWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS99_dataset)
#Observe the design oject
BRFSS99_DO
#_______________________________________________________________________________________________

#----Analysis----

#1. Overall
# Prevalence
B99_overall <- svymean(~factor(HAVARTH),
                       BRFSS99_DO,
                       na.rm = TRUE)
B99_overall.c <- B99_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B99_overall_ci <- confint(B99_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B99 <- bind_cols(B99_overall.c, B99_overall_ci)
#remove havarth = 0
B99 <- B99[-c(1), ] #B99 = final proportion, se and 95% ci


#2. Demographic analysis

#A. Arthritis & Age
B99_Arth_age <- svyby(formula = ~HAVARTH,
                      by = ~AGE,
                      design = BRFSS99_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B99_Arthtitis_age <- B99_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B99_arth_age_ci <- confint(B99_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0 (No)
B99_arth_age_ci <- B99_arth_age_ci[-c(1:11), ]
#join ci and proportions
B99.Age <- bind_cols(B99_Arthtitis_age, B99_arth_age_ci) #B99.Age = final proportion, se and 95% ci by age group

#Age logistic regression
B99_age_glm <- svyglm(HAVARTH~AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS99_DO)
summary(B99_age_glm)
exp(cbind(OR=coef(B99_age_glm), confint(B99_age_glm)))



#B. Arthritis & Sex
B99_Arth_sex <- svyby(formula = ~HAVARTH,
                      by = ~SEX,
                      design = BRFSS99_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B99_Arthritis_sex <- B99_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B99_arth_sex_ci <- confint(B99_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0 (No)
B99_arth_sex_ci <- B99_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B99.Sex <- bind_cols(B99_Arthritis_sex, B99_arth_sex_ci) #B99.Sex = final proportion, se and 95% ci by sex

#Sex logistic regression
B99_sex_glm <- svyglm(HAVARTH~relevel(SEX, ref = "Male") + AGE,
                      family = quasibinomial,
                      design = BRFSS99_DO)
summary(B99_sex_glm)
exp(cbind(OR=coef(B99_sex_glm), confint(B99_sex_glm)))


#C. Arthritis & BMI
B99_Arth_BMI <- svyby(formula = ~HAVARTH,
                      by = ~BMICAT,
                      design = BRFSS99_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B99_Arthritis_BMI <- B99_Arth_BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Proportion", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B99_arth_BMI_ci <- confint(B99_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0
B99_arth_BMI_ci <- B99_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
B99.BMI <- bind_cols(B99_Arthritis_BMI, B99_arth_BMI_ci) #B99.BMI = final proportion, se and 95%ci by BMI

#BMI logistic regression (using original continuous variable from survey)
B99.BMI.glm <- svyglm(HAVARTH ~ `_BMI` + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS99_DO)
summary(B99.BMI.glm)
exp(cbind(OR=coef(B99.BMI.glm), confint(B99.BMI.glm)))


#   End of 1999 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(BRFSS99)
remove(BRFSS99_dataset)
remove(BRFSS99_DO)
remove(BRFSS99_raw)
remove(B99_Arth_age)
remove(B99_arth_age_ci)
remove(B99_Arth_BMI)
remove(B99_arth_BMI_ci)
remove(B99_Arth_sex)
remove(B99_arth_sex_ci)
remove(B99_Arthritis_BMI)
remove(B99_Arthritis_sex)
remove(B99_Arthtitis_age)
remove(B99_arth_age_ci)
remove(B99_overall.c)
remove(B99_overall_ci)
remove(B99.no.age.c)
remove(B99.no.age.ci)
remove(B99.no.BMI.c)
remove(B99.no.BMI.ci)
remove(B99.no.df)
remove(B99.no.ci)
remove(B99.no.sex.c)
remove(B99.no.sex.ci)
remove(B99_age_glm)
remove(B99_sex_glm)
remove(B99.BMI.glm)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

#Behavioural Risk Factor Surveillance System (BRFSS)
#2000


#----2000----


#----Download----

B00_url <- "http://www.cdc.gov/brfss/annual_data/2000/files/CDBRFS00XPT.ZIP"

tempB00 <- tempfile()
tempB00b <- tempfile()

download.file(B00_url, tempB00, mode = "wb")
unzip(zipfile = tempB00, exdir = tempB00b)
BRFSS00_raw <- read_xpt(file.path(tempB00b, "CDBRFS00.XPT"))

unlink(c(tempB00, tempB00b))

#_______________________________________________________________________________________________

#----Cleaning----

#Observe the dataset
str(BRFSS00_raw) 
tail(BRFSS00_raw) 
glimpse(BRFSS00_raw) 
colnames(BRFSS00_raw)

BRFSS00 <- select(BRFSS00_raw,
                  "_PSU", "_FINALWT", "_STSTR", "HAVARTH",
                  "AGE", "SEX", "_BMI2") 

#Observations
str(BRFSS00)
tail(BRFSS00)
glimpse(BRFSS00)
colnames(BRFSS00)


#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS00)) #Large proportion of arthritis data "missing", which makes sense due to being a module year
which(colSums(is.na(BRFSS00)) == nrow(BRFSS00)) #Named integer = 0, which means that at least one reponse is present for the variables selected.


#HAVARTH

#Observe how many people answered yes, no, don't know, & refused
table(BRFSS00$HAVARTH)
#    1     2     7     9 
#29776 89954   371  1436  

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS00$HAVARTH <- recode(BRFSS00$HAVARTH,
                          "1" = "1",
                          "2" = "0",
                          "7" = "7",
                          "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS00$HAVARTH <- unknownToNA(BRFSS00$HAVARTH, unknown = c("7", "9")))
table(BRFSS00$HAVARTH)
#    0     1 
#89954 29776
BRFSS00$HAVARTH <- as.factor(BRFSS00$HAVARTH)
class(BRFSS00$HAVARTH)


#AGE

#Observe data
table(BRFSS00$AGE)
#Need to make this continuous data analogous to later BRFSS 5-year categories in order to make meaningful comparisons between years
#Recode age data into categories corresponding to later BRFSS codebook categories
BRFSS00$AGE<- recode(BRFSS00$AGE,
                     "7" = "7",
                     "9" = "9",
                     "18" = "Age 18 to 24",
                     "19" = "Age 18 to 24",
                     "20" = "Age 18 to 24",
                     "21" = "Age 18 to 24",
                     "22" = "Age 18 to 24",
                     "23" = "Age 18 to 24",
                     "24" = "Age 18 to 24",
                     "25" = "Age 25 to 29",
                     "26" = "Age 25 to 29",
                     "27" = "Age 25 to 29",
                     "28" = "Age 25 to 29",
                     "29" = "Age 25 to 29",
                     "30" = "Age 30 to 34",
                     "31" = "Age 30 to 34",
                     "32" = "Age 30 to 34",
                     "33" = "Age 30 to 34",
                     "34" = "Age 30 to 34",
                     "35" = "Age 35 to 39",
                     "36" = "Age 35 to 39",
                     "37" = "Age 35 to 39",
                     "38" = "Age 35 to 39",
                     "39" = "Age 35 to 39",
                     "40" = "Age 40 to 44",
                     "41" = "Age 40 to 44",
                     "42" = "Age 40 to 44",
                     "43" = "Age 40 to 44",
                     "44" = "Age 40 to 44",
                     "45" = "Age 45 to 49",
                     "46" = "Age 45 to 49",
                     "47" = "Age 45 to 49",
                     "48" = "Age 45 to 49",
                     "49" = "Age 45 to 49",
                     "50" = "Age 50 to 54",
                     "51" = "Age 50 to 54",
                     "52" = "Age 50 to 54",
                     "53" = "Age 50 to 54",
                     "54" = "Age 50 to 54",
                     "55" = "Age 55 to 59",
                     "56" = "Age 55 to 59",
                     "57" = "Age 55 to 59",
                     "58" = "Age 55 to 59",
                     "59" = "Age 55 to 59",
                     "60" = "Age 60 to 64",
                     "61" = "Age 60 to 64",
                     "62" = "Age 60 to 64",
                     "63" = "Age 60 to 64",
                     "64" = "Age 60 to 64",
                     "65" = "Age 65 to 69",
                     "66" = "Age 65 to 69",
                     "67" = "Age 65 to 69",
                     "68" = "Age 65 to 69", 
                     "69" = "Age 65 to 69",
                     "70" = "Age 70 and above",
                     "71" = "Age 70 and above",
                     "72" = "Age 70 and above",
                     "73" = "Age 70 and above",
                     "74" = "Age 70 and above",
                     "75" = "Age 70 and above",
                     "76" = "Age 70 and above",
                     "77" = "Age 70 and above",
                     "78" = "Age 70 and above",
                     "79" = "Age 70 and above",
                     "80" = "Age 70 and above",
                     "81" = "Age 70 and above",
                     "82" = "Age 70 and above",
                     "83" = "Age 70 and above",
                     "84" = "Age 70 and above",
                     "85" = "Age 70 and above",
                     "86" = "Age 70 and above",
                     "87" = "Age 70 and above",
                     "88" = "Age 70 and above",
                     "89" = "Age 70 and above",
                     "90" = "Age 70 and above",
                     "91" = "Age 70 and above",
                     "92" = "Age 70 and above",
                     "93" = "Age 70 and above",
                     "94" = "Age 70 and above",
                     "95" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "98" = "Age 70 and above",
                     "99" = "Age 70 and above")
#Change the unknown (7) and refused (9) values to NA
(BRFSS00$AGE <- unknownToNA(BRFSS00$AGE, unknown = c("7", "9")))
table(BRFSS00$AGE)

#    Age 18 to 24     Age 25 to 29     Age 30 to 34     Age 35 to 39     Age 40 to 44     Age 45 to 49     Age 50 to 54 
#           16466            15688            17393            20483            20300            18582            16563 
#    Age 55 to 59     Age 60 to 64     Age 65 to 69 Age 70 and above 
#           13033            10746            10250            23824
BRFSS00$AGE <- as.factor(BRFSS00$AGE)
class(BRFSS00$AGE)


#SEX

#Observe data
table(BRFSS00$SEX)
#Recode sex data into categories corresponding to BRFSS codebook
BRFSS00$SEX <- recode(BRFSS00$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS00$SEX)
#Female   Male 
#109680  74770
BRFSS00$SEX <- as.factor(BRFSS00$SEX)
class(BRFSS00$SEX)


#BMI

#Observe data
table(BRFSS00$`_BMI2`)
#one implied decimal place, therefore pull comma back one place
BRFSS00 <- BRFSS00 %>%
  mutate(`_BMI2` = `_BMI2`/10)
BRFSS00$`_BMI2` <- unknownToNA(BRFSS00$`_BMI2`, unknown = "99.9") #as per codebook
BRFSS00 %>% top_n(10, `_BMI2`) #check that 99.9 does not appear

#Create another BMI variable using BMI categories for figures
BRFSS00$BMICAT <- ifelse(BRFSS00$`_BMI2` < 18.5, "Underweight",
                  ifelse(BRFSS00$`_BMI2` >= 18.5 & BRFSS00$`_BMI2` < 25.0, "Healthy weight",
                  ifelse(BRFSS00$`_BMI2` >= 25.0 & BRFSS00$`_BMI2` < 30.0, "Overweight",
                  ifelse(BRFSS00$`_BMI2` >= 30.0, "Obese",
                  NA))))

table(BRFSS00$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#         73465          35878          63138           3701


#__________________________________________________________________________________________________________________
#DESIGN OBJECT CREATION

#Some final checks:
options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS00_dataset <- subset(BRFSS00,
                          !is.na(`_PSU`) &
                          !is.na(`_STSTR`) &
                          !is.na(`_FINALWT`) &
                          !is.na(HAVARTH))

#Check that the sum of the weights is equal to the US population
sum(BRFSS00_dataset$`_FINALWT`)
#The sum of the weights is 142 187 598 {MODULE YEAR SO REDUCED POPULATION SIZE TO BE EXPECTED}

#Check the number of people (unique PSU's)
length(unique(BRFSS00_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 43 744

#Check the number of unique strata
length(unique(BRFSS00_dataset[["_STSTR"]]))
#The number of unique strata is 213

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS00_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_FINALWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS00_dataset)
#Observe the design oject
BRFSS00_DO
#_______________________________________________________________________________________________

#----Analysis----

#1. Overall
# Prevalence
B00_overall <- svymean(~factor(HAVARTH),
                       BRFSS00_DO,
                       na.rm = TRUE)
B00_overall.c <- B00_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B00_overall_ci <- confint(B00_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B00 <- bind_cols(B00_overall.c, B00_overall_ci)
#remove havarth = 0
B00 <- B00[-c(1), ] #B00 = final proportion, se and 95% ci

#2. Demographic analysis

#A. Arthritis & Age
B00_Arth_age <- svyby(formula = ~HAVARTH,
                      by = ~AGE,
                      design = BRFSS00_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B00_Arthtitis_age <- B00_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B00_arth_age_ci <- confint(B00_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0 (No)
B00_arth_age_ci <- B00_arth_age_ci[-c(1:11), ]
#join ci and proportions
B00.Age <- bind_cols(B00_Arthtitis_age, B00_arth_age_ci) #B00.Age = final proportion, se and 95% ci by age group

#Age logistic regression
B00_age_glm <- svyglm(HAVARTH~AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS00_DO)
summary(B00_age_glm)
exp(cbind(OR=coef(B00_age_glm), confint(B00_age_glm)))



#B. Arthritis & Sex
B00_Arth_sex <- svyby(formula = ~HAVARTH,
                      by = ~SEX,
                      design = BRFSS00_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B00_Arthritis_sex <- B00_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B00_arth_sex_ci <- confint(B00_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0 (No)
B00_arth_sex_ci <- B00_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B00.Sex <- bind_cols(B00_Arthritis_sex, B00_arth_sex_ci) #B00.Sex = final proportion, se and 95% ci by sex

#Sex logistic regression
B00_sex_glm <- svyglm(HAVARTH~relevel(SEX, ref = "Male") + AGE,
                      family = quasibinomial,
                      design = BRFSS00_DO)
summary(B00_sex_glm)
exp(cbind(OR=coef(B00_sex_glm), confint(B00_sex_glm)))


#C. Arthritis & BMI
B00_Arth_BMI <- svyby(formula = ~HAVARTH,
                      by = ~BMICAT,
                      design = BRFSS00_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B00_Arthritis_BMI <- B00_Arth_BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Proportion", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B00_arth_BMI_ci <- confint(B00_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0
B00_arth_BMI_ci <- B00_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
B00.BMI <- bind_cols(B00_Arthritis_BMI, B00_arth_BMI_ci) #B00.BMI = final proportion, se and 95%ci by BMI

#BMI logistic regression (using original continuous variable from survey)
B00.BMI.glm <- svyglm(HAVARTH ~ `_BMI2` + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS00_DO)
summary(B00.BMI.glm)
exp(cbind(OR=coef(B00.BMI.glm), confint(B00.BMI.glm)))


#   End of 2000 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(BRFSS00)
remove(BRFSS00_dataset)
remove(BRFSS00_DO)
remove(BRFSS00_raw)
remove(B00_Arth_age)
remove(B00_arth_age_ci)
remove(B00_Arth_BMI)
remove(B00_arth_BMI_ci)
remove(B00_Arth_sex)
remove(B00_arth_sex_ci)
remove(B00_Arthritis_BMI)
remove(B00_Arthritis_sex)
remove(B00_Arthtitis_age)
remove(B00_arth_age_ci)
remove(B00_overall.c)
remove(B00_overall_ci)
remove(B00.no.age.c)
remove(B00.no.age.ci)
remove(B00.no.BMI.c)
remove(B00.no.BMI.ci)
remove(B00.no.df)
remove(B00.no.ci)
remove(B00.no.sex.c)
remove(B00.no.sex.ci)
remove(B00_age_glm)
remove(B00_sex_glm)
remove(B00.BMI.glm)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

#Behavioural Risk Factor Surveillance System (BRFSS)
#2001



#----2001----


#----Download----

B01_url <- "http://www.cdc.gov/brfss/annual_data/2001/files/CDBRFS01XPT.zip"

tempB01 <- tempfile()
tempB01b <- tempfile()

download.file(B01_url, tempB01, mode = "wb")
unzip(zipfile = tempB01, exdir = tempB01b)
BRFSS01_raw <- read_xpt(file.path(tempB01b, "CDBRFS01.XPT"))

unlink(c(tempB01, tempB01b))

#_______________________________________________________________________________________________

#----Cleaning----

#Observe the dataset
str(BRFSS01_raw) 
tail(BRFSS01_raw) 
glimpse(BRFSS01_raw) 
colnames(BRFSS01_raw)

BRFSS01 <- select(BRFSS01_raw,
                  "_PSU", "_FINALWT", "_STSTR", "HAVARTH",
                  "AGE", "SEX", "_BMI2") 

#Observations
str(BRFSS01)
tail(BRFSS01)
glimpse(BRFSS01)
colnames(BRFSS01)


#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS01))
which(colSums(is.na(BRFSS01)) == nrow(BRFSS01)) #Named integer = 0, which means that at least one reponse is present for the variables selected.


#HAVARTH

#Observe how many people answered yes, no, don't know, & refused
table(BRFSS01$HAVARTH)
#    1       2      7      9 
# 52311 157619    706     31

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS01$HAVARTH <- recode(BRFSS01$HAVARTH,
                          "1" = "1",
                          "2" = "0",
                          "7" = "7",
                          "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS01$HAVARTH <- unknownToNA(BRFSS01$HAVARTH, unknown = c("7", "9")))
table(BRFSS01$HAVARTH)
#     0      1 
#157619  52311
BRFSS01$HAVARTH <- as.factor(BRFSS01$HAVARTH)
class(BRFSS01$HAVARTH)


#AGE

#Observe data
table(BRFSS01$AGE)
#Need to make this continuous data analogous to later BRFSS 5-year categories in order to make meaningful comparisons between years
#Recode age data into categories corresponding to later BRFSS codebook categories
BRFSS01$AGE<- recode(BRFSS01$AGE,
                     "7" = "7",
                     "9" = "9",
                     "18" = "Age 18 to 24",
                     "19" = "Age 18 to 24",
                     "20" = "Age 18 to 24",
                     "21" = "Age 18 to 24",
                     "22" = "Age 18 to 24",
                     "23" = "Age 18 to 24",
                     "24" = "Age 18 to 24",
                     "25" = "Age 25 to 29",
                     "26" = "Age 25 to 29",
                     "27" = "Age 25 to 29",
                     "28" = "Age 25 to 29",
                     "29" = "Age 25 to 29",
                     "30" = "Age 30 to 34",
                     "31" = "Age 30 to 34",
                     "32" = "Age 30 to 34",
                     "33" = "Age 30 to 34",
                     "34" = "Age 30 to 34",
                     "35" = "Age 35 to 39",
                     "36" = "Age 35 to 39",
                     "37" = "Age 35 to 39",
                     "38" = "Age 35 to 39",
                     "39" = "Age 35 to 39",
                     "40" = "Age 40 to 44",
                     "41" = "Age 40 to 44",
                     "42" = "Age 40 to 44",
                     "43" = "Age 40 to 44",
                     "44" = "Age 40 to 44",
                     "45" = "Age 45 to 49",
                     "46" = "Age 45 to 49",
                     "47" = "Age 45 to 49",
                     "48" = "Age 45 to 49",
                     "49" = "Age 45 to 49",
                     "50" = "Age 50 to 54",
                     "51" = "Age 50 to 54",
                     "52" = "Age 50 to 54",
                     "53" = "Age 50 to 54",
                     "54" = "Age 50 to 54",
                     "55" = "Age 55 to 59",
                     "56" = "Age 55 to 59",
                     "57" = "Age 55 to 59",
                     "58" = "Age 55 to 59",
                     "59" = "Age 55 to 59",
                     "60" = "Age 60 to 64",
                     "61" = "Age 60 to 64",
                     "62" = "Age 60 to 64",
                     "63" = "Age 60 to 64",
                     "64" = "Age 60 to 64",
                     "65" = "Age 65 to 69",
                     "66" = "Age 65 to 69",
                     "67" = "Age 65 to 69",
                     "68" = "Age 65 to 69", 
                     "69" = "Age 65 to 69",
                     "70" = "Age 70 and above",
                     "71" = "Age 70 and above",
                     "72" = "Age 70 and above",
                     "73" = "Age 70 and above",
                     "74" = "Age 70 and above",
                     "75" = "Age 70 and above",
                     "76" = "Age 70 and above",
                     "77" = "Age 70 and above",
                     "78" = "Age 70 and above",
                     "79" = "Age 70 and above",
                     "80" = "Age 70 and above",
                     "81" = "Age 70 and above",
                     "82" = "Age 70 and above",
                     "83" = "Age 70 and above",
                     "84" = "Age 70 and above",
                     "85" = "Age 70 and above",
                     "86" = "Age 70 and above",
                     "87" = "Age 70 and above",
                     "88" = "Age 70 and above",
                     "89" = "Age 70 and above",
                     "90" = "Age 70 and above",
                     "91" = "Age 70 and above",
                     "92" = "Age 70 and above",
                     "93" = "Age 70 and above",
                     "94" = "Age 70 and above",
                     "95" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "98" = "Age 70 and above",
                     "99" = "Age 70 and above")
#Change the unknown (7) and refused (9) values to NA
(BRFSS01$AGE <- unknownToNA(BRFSS01$AGE, unknown = c("7", "9")))
table(BRFSS01$AGE)

#Age 18 to 24     Age 25 to 29     Age 30 to 34     Age 35 to 39     Age 40 to 44     Age 45 to 49     Age 50 to 54     Age 55 to 59     Age 60 to 64 
#       18504            17199            19994            22199            23151            21546            20090            15103            12626 
#Age 65 to 69 Age 70 and above 
#       11697            28195 
BRFSS01$AGE <- as.factor(BRFSS01$AGE)
class(BRFSS01$AGE)


#SEX

#Observe data
table(BRFSS01$SEX)
#Recode sex data into categories corresponding to BRFSS codebook
BRFSS01$SEX <- recode(BRFSS01$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS01$SEX)
#Female   Male 
#126048  86462
BRFSS01$SEX <- as.factor(BRFSS01$SEX)
class(BRFSS01$SEX)


#BMI

#Observe data
table(BRFSS01$`_BMI2`)
#recode unknown numerical to NA
BRFSS01$`_BMI2` <- unknownToNA(BRFSS01$`_BMI2`, unknown = "999999")
#four implied decimal places, therefore pull comma back four places
BRFSS01 <- BRFSS01 %>%
  mutate(`_BMI2` = `_BMI2`/10000)
#check that unknown data do not appear
BRFSS01 %>% top_n(10, `_BMI2`)

#Create another BMI variable using BMI categories for figures
BRFSS01$BMICAT <- ifelse(BRFSS01$`_BMI2` < 18.5000, "Underweight",
                  ifelse(BRFSS01$`_BMI2` >= 18.5000 & BRFSS01$`_BMI2` < 25.0000, "Healthy weight",
                  ifelse(BRFSS01$`_BMI2` >= 25.0000 & BRFSS01$`_BMI2` < 30.0000, "Overweight",
                  ifelse(BRFSS01$`_BMI2` >= 30.0000, "Obese",
                  NA))))

table(BRFSS01$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#         81262          43114          73545           4180 

#__________________________________________________________________________________________________________________

#DESIGN OBJECT CREATION

#Some final checks:
options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS01_dataset <- subset(BRFSS01,
                          !is.na(`_PSU`) &
                            !is.na(`_STSTR`) &
                            !is.na(`_FINALWT`) &
                            !is.na(HAVARTH))

#Check that the sum of the weights is equal to the US population
sum(BRFSS01_dataset$`_FINALWT`)
#The sum of the weights is 209 731 699

#Check the number of people (unique PSU's)
length(unique(BRFSS01_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 63 446

#Check the number of unique strata
length(unique(BRFSS01_dataset[["_STSTR"]]))
#The number of unique strata is 154

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS01_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_FINALWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS01_dataset)
#Observe the design oject
BRFSS01_DO
#_______________________________________________________________________________________________

#----Analysis----

#1. Overall prevalence
B01_overall <- svymean(~factor(HAVARTH),
                       BRFSS01_DO,
                       na.rm = TRUE)
B01_overall.c <- B01_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B01_overall_ci <- confint(B01_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B01 <- bind_cols(B01_overall.c, B01_overall_ci)
#remove havarth = 0
B01 <- B01[-c(1), ] #B01 = final proportion, se and 95% ci

#Overall number of people
B01.no <- svytotal(~HAVARTH, 
                   BRFSS01_DO, 
                   na.rm = TRUE, 
                   deff = TRUE)
B01.no.df <- as.data.frame(B01.no)
#ci
B01.no.ci <- confint(B01.no) %>%
  as.data.frame(.)
#join number and ci
B01.no <- bind_cols(B01.no.df, B01.no.ci)
#remove havarth=0
B01.no <- B01.no[-c(1), ] #B01.no = final number of people with arth, se, design effect and 95%ci


#2. Demographic-specific analysis

#A. Arthritis & Age
B01_Arth_age <- svyby(formula = ~HAVARTH,
                      by = ~AGE,
                      design = BRFSS01_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B01_Arthtitis_age <- B01_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B01_arth_age_ci <- confint(B01_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0 (No)
B01_arth_age_ci <- B01_arth_age_ci[-c(1:11), ]
#join ci and proportions
B01.Age <- bind_cols(B01_Arthtitis_age, B01_arth_age_ci) #B01.Age = final proportion, se and 95% ci by age group


#Number of people by age
B01.no.age <- svyby(formula = ~HAVARTH,
                    by = ~AGE,
                    design = BRFSS01_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B01.no.age.c <- B01.no.age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Number of People", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B01.no.age.ci <- confint(B01.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0 (No), which are the first 11 rows
B01.no.age.ci <- B01.no.age.ci[-c(1:11), ]
#join number and ci
B01.no.age <- bind_cols(B01.no.age.c, B01.no.age.ci)


#Age logistic regression
B01_age_glm <- svyglm(HAVARTH~AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS01_DO)
summary(B01_age_glm)
exp(cbind(OR=coef(B01_age_glm), confint(B01_age_glm)))



#B. Arthritis & Sex
B01_Arth_sex <- svyby(formula = ~HAVARTH,
                      by = ~SEX,
                      design = BRFSS01_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B01_Arthritis_sex <- B01_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B01_arth_sex_ci <- confint(B01_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0 (No)
B01_arth_sex_ci <- B01_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B01.Sex <- bind_cols(B01_Arthritis_sex, B01_arth_sex_ci) #B01.Sex = final proportion, se and 95% ci by sex


#Number of people by sex
B01.no.sex <- svyby(formula = ~HAVARTH,
                    by = ~SEX,
                    design = BRFSS01_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B01.no.sex.c <- B01.no.sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Number of People", "SE")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B01.no.sex.ci <- confint(B01.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0 (No), which are the first 2 rows
B01.no.sex.ci <- B01.no.sex.ci[-c(1:2), ]
#join number and ci
B01.no.sex <- bind_cols(B01.no.sex.c, B01.no.sex.ci)


#Sex logistic regression
B01_sex_glm <- svyglm(HAVARTH~relevel(SEX, ref = "Male") + AGE,
                      family = quasibinomial,
                      design = BRFSS01_DO)
summary(B01_sex_glm)
exp(cbind(OR=coef(B01_sex_glm), confint(B01_sex_glm)))


#C. Arthritis & BMI
B01_Arth_BMI <- svyby(formula = ~HAVARTH,
                      by = ~BMICAT,
                      design = BRFSS01_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B01_Arthritis_BMI <- B01_Arth_BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Proportion", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B01_arth_BMI_ci <- confint(B01_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0
B01_arth_BMI_ci <- B01_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
B01.BMI <- bind_cols(B01_Arthritis_BMI, B01_arth_BMI_ci) #B01.BMI = final proportion, se and 95%ci by BMI


#Number of people by BMI
B01.no.BMI <- svyby(formula = ~HAVARTH,
                    by = ~BMICAT,
                    design = BRFSS01_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B01.no.BMI.c <- B01.no.BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Number of People", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B01.no.BMI.ci <- confint(B01.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH = 0 (No), which are the first 4 rows
B01.no.BMI.ci <- B01.no.BMI.ci[-c(1:4), ]
#join number and ci
B01.no.BMI <- bind_cols(B01.no.BMI.c, B01.no.BMI.ci)


#BMI logistic regression (using original continuous variable from survey)
B01.BMI.glm <- svyglm(HAVARTH ~ `_BMI2` + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS01_DO)
summary(B01.BMI.glm)
exp(cbind(OR=coef(B01.BMI.glm), confint(B01.BMI.glm)))


#   End of 2001 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(BRFSS01)
remove(BRFSS01_dataset)
remove(BRFSS01_DO)
remove(BRFSS01_raw)
remove(B01_Arth_age)
remove(B01_arth_age_ci)
remove(B01_Arth_BMI)
remove(B01_arth_BMI_ci)
remove(B01_Arth_sex)
remove(B01_arth_sex_ci)
remove(B01_Arthritis_BMI)
remove(B01_Arthritis_sex)
remove(B01_Arthtitis_age)
remove(B01_arth_age_ci)
remove(B01_overall.c)
remove(B01_overall_ci)
remove(B01.no.age.c)
remove(B01.no.age.ci)
remove(B01.no.BMI.c)
remove(B01.no.BMI.ci)
remove(B01.no.df)
remove(B01.no.ci)
remove(B01.no.sex.c)
remove(B01.no.sex.ci)
remove(B01_age_glm)
remove(B01_sex_glm)
remove(B01.BMI.glm)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

#Behavioural Risk Factor Surveillance System (BRFSS)
#2002



#----2002----


#----Download----

B02_url <- "http://www.cdc.gov/brfss/annual_data/2002/files/CDBRFS02XPT.ZIP"

tempB02 <- tempfile()
tempB02b <- tempfile()

download.file(B02_url, tempB02, mode = "wb")
unzip(zipfile = tempB02, exdir = tempB02b)
BRFSS02_raw <- read_xpt(file.path(tempB02b, "CDBRFS02.XPT"))

unlink(c(tempB02, tempB02b))

#_______________________________________________________________________________________________

#----Cleaning----

#Observe the dataset
str(BRFSS02_raw) 
tail(BRFSS02_raw) 
glimpse(BRFSS02_raw) 
colnames(BRFSS02_raw)

BRFSS02 <- select(BRFSS02_raw,
                  "_PSU", "_FINALWT", "_STSTR", "HAVARTH2",
                  "AGE", "SEX", "_BMI2") 

#Observations
str(BRFSS02)
tail(BRFSS02)
glimpse(BRFSS02)
colnames(BRFSS02)


#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS02)) #Large proportion of arthritis data "missing", which makes sense due to being a module year
which(colSums(is.na(BRFSS02)) == nrow(BRFSS02)) #Named integer = 0, which means that at least one reponse is present for the variables selected.


#HAVARTH2

#Observe how many people answered yes, no, don't know, & refused
table(BRFSS02$HAVARTH2)
#    1     2     7     9 
#41067 89878   489  2897  

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS02$HAVARTH2 <- recode(BRFSS02$HAVARTH2,
                           "1" = "1",
                           "2" = "0",
                           "7" = "7",
                           "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS02$HAVARTH2 <- unknownToNA(BRFSS02$HAVARTH2, unknown = c("7", "9")))
table(BRFSS02$HAVARTH2)
#    0     1 
#89878 41067
BRFSS02$HAVARTH2 <- as.factor(BRFSS02$HAVARTH2)
class(BRFSS02$HAVARTH2)


#AGE

#Observe data
table(BRFSS02$AGE)
#Need to make this continuous data analogous to later BRFSS 5-year categories in order to make meaningful comparisons between years
#Recode age data into categories corresponding to later BRFSS codebook categories
BRFSS02$AGE<- recode(BRFSS02$AGE,
                     "7" = "7",
                     "9" = "9",
                     "18" = "Age 18 to 24",
                     "19" = "Age 18 to 24",
                     "20" = "Age 18 to 24",
                     "21" = "Age 18 to 24",
                     "22" = "Age 18 to 24",
                     "23" = "Age 18 to 24",
                     "24" = "Age 18 to 24",
                     "25" = "Age 25 to 29",
                     "26" = "Age 25 to 29",
                     "27" = "Age 25 to 29",
                     "28" = "Age 25 to 29",
                     "29" = "Age 25 to 29",
                     "30" = "Age 30 to 34",
                     "31" = "Age 30 to 34",
                     "32" = "Age 30 to 34",
                     "33" = "Age 30 to 34",
                     "34" = "Age 30 to 34",
                     "35" = "Age 35 to 39",
                     "36" = "Age 35 to 39",
                     "37" = "Age 35 to 39",
                     "38" = "Age 35 to 39",
                     "39" = "Age 35 to 39",
                     "40" = "Age 40 to 44",
                     "41" = "Age 40 to 44",
                     "42" = "Age 40 to 44",
                     "43" = "Age 40 to 44",
                     "44" = "Age 40 to 44",
                     "45" = "Age 45 to 49",
                     "46" = "Age 45 to 49",
                     "47" = "Age 45 to 49",
                     "48" = "Age 45 to 49",
                     "49" = "Age 45 to 49",
                     "50" = "Age 50 to 54",
                     "51" = "Age 50 to 54",
                     "52" = "Age 50 to 54",
                     "53" = "Age 50 to 54",
                     "54" = "Age 50 to 54",
                     "55" = "Age 55 to 59",
                     "56" = "Age 55 to 59",
                     "57" = "Age 55 to 59",
                     "58" = "Age 55 to 59",
                     "59" = "Age 55 to 59",
                     "60" = "Age 60 to 64",
                     "61" = "Age 60 to 64",
                     "62" = "Age 60 to 64",
                     "63" = "Age 60 to 64",
                     "64" = "Age 60 to 64",
                     "65" = "Age 65 to 69",
                     "66" = "Age 65 to 69",
                     "67" = "Age 65 to 69",
                     "68" = "Age 65 to 69", 
                     "69" = "Age 65 to 69",
                     "70" = "Age 70 and above",
                     "71" = "Age 70 and above",
                     "72" = "Age 70 and above",
                     "73" = "Age 70 and above",
                     "74" = "Age 70 and above",
                     "75" = "Age 70 and above",
                     "76" = "Age 70 and above",
                     "77" = "Age 70 and above",
                     "78" = "Age 70 and above",
                     "79" = "Age 70 and above",
                     "80" = "Age 70 and above",
                     "81" = "Age 70 and above",
                     "82" = "Age 70 and above",
                     "83" = "Age 70 and above",
                     "84" = "Age 70 and above",
                     "85" = "Age 70 and above",
                     "86" = "Age 70 and above",
                     "87" = "Age 70 and above",
                     "88" = "Age 70 and above",
                     "89" = "Age 70 and above",
                     "90" = "Age 70 and above",
                     "91" = "Age 70 and above",
                     "92" = "Age 70 and above",
                     "93" = "Age 70 and above",
                     "94" = "Age 70 and above",
                     "95" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "98" = "Age 70 and above",
                     "99" = "Age 70 and above")
#Change the unknown (7) and refused (9) values to NA
(BRFSS02$AGE <- unknownToNA(BRFSS02$AGE, unknown = c("7", "9")))
table(BRFSS02$AGE)

#    Age 18 to 24     Age 25 to 29     Age 30 to 34     Age 35 to 39     Age 40 to 44     Age 45 to 49     Age 50 to 54 
#           18879            18029            22009            23957            26522            25593            23941 
#    Age 55 to 59     Age 60 to 64     Age 65 to 69 Age 70 and above 
#           19828            16185            14854            36202
BRFSS02$AGE <- as.factor(BRFSS02$AGE)
class(BRFSS02$AGE)


#SEX

#Observe data
table(BRFSS02$SEX)
#Recode sex data into categories corresponding to BRFSS codebook
BRFSS02$SEX <- recode(BRFSS02$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS02$SEX)
#Female   Male 
#148702  99262
BRFSS02$SEX <- as.factor(BRFSS02$SEX)
class(BRFSS02$SEX)


#BMI

#Observe data
table(BRFSS02$`_BMI2`)
#two implied decimal places, therefore pull comma back two places
BRFSS02 <- BRFSS02 %>%
  mutate(`_BMI2` = `_BMI2`/100)
BRFSS02$`_BMI2` <- unknownToNA(BRFSS02$`_BMI2`, unknown = "99.99") #as per codebook
BRFSS02 %>% top_n(10, `_BMI2`) #check that 99.99 does not appear

#Create another BMI variable using BMI categories for figures
BRFSS02$BMICAT <- ifelse(BRFSS02$`_BMI2` < 18.5, "Underweight",
                  ifelse(BRFSS02$`_BMI2` >= 18.5 & BRFSS02$`_BMI2` < 25.0, "Healthy weight",
                  ifelse(BRFSS02$`_BMI2` >= 25.0 & BRFSS02$`_BMI2` < 30.0, "Overweight",
                  ifelse(BRFSS02$`_BMI2` >= 30.0, "Obese",
                  NA))))

table(BRFSS02$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#         93920          51902          85870           4595


#__________________________________________________________________________________________________________________
#DESIGN OBJECT CREATION

#Some final checks:
options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS02_dataset <- subset(BRFSS02,
                          !is.na(`_PSU`) &
                          !is.na(`_STSTR`) &
                          !is.na(`_FINALWT`) &
                          !is.na(HAVARTH2))

#Check that the sum of the weights is equal to the US population
sum(BRFSS02_dataset$`_FINALWT`)
#The sum of the weights is 126 670 296 {MODULE YEAR SO REDUCED POPULATION SIZE TO BE EXPECTED}

#Check the number of people (unique PSU's)
length(unique(BRFSS02_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 51 252

#Check the number of unique strata
length(unique(BRFSS02_dataset[["_STSTR"]]))
#The number of unique strata is 500

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS02_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_FINALWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS02_dataset)
#Observe the design oject
BRFSS02_DO

#_______________________________________________________________________________________________

#----Analysis----

#1. Overall
# Prevalence
B02_overall <- svymean(~factor(HAVARTH2),
                       BRFSS02_DO,
                       na.rm = TRUE)
B02_overall.c <- B02_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B02_overall_ci <- confint(B02_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B02 <- bind_cols(B02_overall.c, B02_overall_ci)
#remove havarth = 0
B02 <- B02[-c(1), ] #B02 = final proportion, se and 95% ci


#2. Demographic analysis

#A. Arthritis & Age
B02_Arth_age <- svyby(formula = ~HAVARTH2,
                      by = ~AGE,
                      design = BRFSS02_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B02_Arthtitis_age <- B02_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B02_arth_age_ci <- confint(B02_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0 (No)
B02_arth_age_ci <- B02_arth_age_ci[-c(1:11), ]
#join ci and proportions
B02.Age <- bind_cols(B02_Arthtitis_age, B02_arth_age_ci) #B02.Age = final proportion, se and 95% ci by age group

#Age logistic regression
B02_age_glm <- svyglm(HAVARTH2~AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS02_DO)
summary(B02_age_glm)
exp(cbind(OR=coef(B02_age_glm), confint(B02_age_glm)))



#B. Arthritis & Sex
B02_Arth_sex <- svyby(formula = ~HAVARTH2,
                      by = ~SEX,
                      design = BRFSS02_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B02_Arthritis_sex <- B02_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B02_arth_sex_ci <- confint(B02_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0 (No)
B02_arth_sex_ci <- B02_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B02.Sex <- bind_cols(B02_Arthritis_sex, B02_arth_sex_ci) #B02.Sex = final proportion, se and 95% ci by sex

#Sex logistic regression
B02_sex_glm <- svyglm(HAVARTH2~relevel(SEX, ref = "Male") + AGE,
                      family = quasibinomial,
                      design = BRFSS02_DO)
summary(B02_sex_glm)
exp(cbind(OR=coef(B02_sex_glm), confint(B02_sex_glm)))


#C. Arthritis & BMI
B02_Arth_BMI <- svyby(formula = ~HAVARTH2,
                      by = ~BMICAT,
                      design = BRFSS02_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B02_Arthritis_BMI <- B02_Arth_BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Proportion", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B02_arth_BMI_ci <- confint(B02_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0
B02_arth_BMI_ci <- B02_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
B02.BMI <- bind_cols(B02_Arthritis_BMI, B02_arth_BMI_ci) #B02.BMI = final proportion, se and 95%ci by BMI

#BMI logistic regression (using original continuous variable from survey)
B02.BMI.glm <- svyglm(HAVARTH2 ~ `_BMI2` + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS02_DO)
summary(B02.BMI.glm)
exp(cbind(OR=coef(B02.BMI.glm), confint(B02.BMI.glm)))


#   End of 2002 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(BRFSS02)
remove(BRFSS02_dataset)
remove(BRFSS02_DO)
remove(BRFSS02_raw)
remove(B02_Arth_age)
remove(B02_arth_age_ci)
remove(B02_Arth_BMI)
remove(B02_arth_BMI_ci)
remove(B02_Arth_sex)
remove(B02_arth_sex_ci)
remove(B02_Arthritis_BMI)
remove(B02_Arthritis_sex)
remove(B02_Arthtitis_age)
remove(B02_arth_age_ci)
remove(B02_overall.c)
remove(B02_overall_ci)
remove(B02.no.age.c)
remove(B02.no.age.ci)
remove(B02.no.BMI.c)
remove(B02.no.BMI.ci)
remove(B02.no.df)
remove(B02.no.ci)
remove(B02.no.sex.c)
remove(B02.no.sex.ci)
remove(B02_age_glm)
remove(B02_sex_glm)
remove(B02.BMI.glm)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

#Behavioural Risk Factor Surveillance System (BRFSS)
#2003


#----2003----


#----Download----

B03_url <- "http://www.cdc.gov/brfss/annual_data/2003/files/CDBRFS03XPT.ZIP"

tempB03 <- tempfile()
tempB03b <- tempfile()

download.file(B03_url, tempB03, mode = "wb")
unzip(zipfile = tempB03, exdir = tempB03b)
BRFSS03_raw <- read_xpt(file.path(tempB03b, "CDBRFS03.XPT"))

unlink(c(tempB03, tempB03b))

#_______________________________________________________________________________________________

#----Cleaning----

#Observe the dataset
str(BRFSS03_raw) 
tail(BRFSS03_raw) 
glimpse(BRFSS03_raw) 
colnames(BRFSS03_raw)

BRFSS03 <- select(BRFSS03_raw,
                  "_PSU", "_FINALWT", "_STSTR", "HAVARTH2",
                  "AGE", "SEX", "_BMI3") 

#Observations
str(BRFSS03)
tail(BRFSS03)
glimpse(BRFSS03)
colnames(BRFSS03)


#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS03))
which(colSums(is.na(BRFSS03)) == nrow(BRFSS03)) #Named integer = 0, which means that at least one reponse is present for the variables selected.


#HAVARTH

#Observe how many people answered yes, no, don't know, & refused
table(BRFSS03$HAVARTH2)
#     1      2      7      9 
# 82373 176773    827    594

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS03$HAVARTH2 <- recode(BRFSS03$HAVARTH2,
                           "1" = "1",
                           "2" = "0",
                           "7" = "7",
                           "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS03$HAVARTH2 <- unknownToNA(BRFSS03$HAVARTH2, unknown = c("7", "9")))
table(BRFSS03$HAVARTH2)
#     0      1 
#176773  82373
BRFSS03$HAVARTH2 <- as.factor(BRFSS03$HAVARTH2)
class(BRFSS03$HAVARTH2)


#AGE

#Observe data
table(BRFSS03$AGE)
#Need to make this continuous data analogous to later BRFSS 5-year categories in order to make meaningful comparisons between years
#Recode age data into categories corresponding to later BRFSS codebook categories
BRFSS03$AGE<- recode(BRFSS03$AGE,
                     "7" = "7",
                     "9" = "9",
                     "18" = "Age 18 to 24",
                     "19" = "Age 18 to 24",
                     "20" = "Age 18 to 24",
                     "21" = "Age 18 to 24",
                     "22" = "Age 18 to 24",
                     "23" = "Age 18 to 24",
                     "24" = "Age 18 to 24",
                     "25" = "Age 25 to 29",
                     "26" = "Age 25 to 29",
                     "27" = "Age 25 to 29",
                     "28" = "Age 25 to 29",
                     "29" = "Age 25 to 29",
                     "30" = "Age 30 to 34",
                     "31" = "Age 30 to 34",
                     "32" = "Age 30 to 34",
                     "33" = "Age 30 to 34",
                     "34" = "Age 30 to 34",
                     "35" = "Age 35 to 39",
                     "36" = "Age 35 to 39",
                     "37" = "Age 35 to 39",
                     "38" = "Age 35 to 39",
                     "39" = "Age 35 to 39",
                     "40" = "Age 40 to 44",
                     "41" = "Age 40 to 44",
                     "42" = "Age 40 to 44",
                     "43" = "Age 40 to 44",
                     "44" = "Age 40 to 44",
                     "45" = "Age 45 to 49",
                     "46" = "Age 45 to 49",
                     "47" = "Age 45 to 49",
                     "48" = "Age 45 to 49",
                     "49" = "Age 45 to 49",
                     "50" = "Age 50 to 54",
                     "51" = "Age 50 to 54",
                     "52" = "Age 50 to 54",
                     "53" = "Age 50 to 54",
                     "54" = "Age 50 to 54",
                     "55" = "Age 55 to 59",
                     "56" = "Age 55 to 59",
                     "57" = "Age 55 to 59",
                     "58" = "Age 55 to 59",
                     "59" = "Age 55 to 59",
                     "60" = "Age 60 to 64",
                     "61" = "Age 60 to 64",
                     "62" = "Age 60 to 64",
                     "63" = "Age 60 to 64",
                     "64" = "Age 60 to 64",
                     "65" = "Age 65 to 69",
                     "66" = "Age 65 to 69",
                     "67" = "Age 65 to 69",
                     "68" = "Age 65 to 69", 
                     "69" = "Age 65 to 69",
                     "70" = "Age 70 and above",
                     "71" = "Age 70 and above",
                     "72" = "Age 70 and above",
                     "73" = "Age 70 and above",
                     "74" = "Age 70 and above",
                     "75" = "Age 70 and above",
                     "76" = "Age 70 and above",
                     "77" = "Age 70 and above",
                     "78" = "Age 70 and above",
                     "79" = "Age 70 and above",
                     "80" = "Age 70 and above",
                     "81" = "Age 70 and above",
                     "82" = "Age 70 and above",
                     "83" = "Age 70 and above",
                     "84" = "Age 70 and above",
                     "85" = "Age 70 and above",
                     "86" = "Age 70 and above",
                     "87" = "Age 70 and above",
                     "88" = "Age 70 and above",
                     "89" = "Age 70 and above",
                     "90" = "Age 70 and above",
                     "91" = "Age 70 and above",
                     "92" = "Age 70 and above",
                     "93" = "Age 70 and above",
                     "94" = "Age 70 and above",
                     "95" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "98" = "Age 70 and above",
                     "99" = "Age 70 and above")
#Change the unknown (7) and refused (9) values to NA
(BRFSS03$AGE <- unknownToNA(BRFSS03$AGE, unknown = c("7", "9")))
table(BRFSS03$AGE)

#    Age 18 to 24     Age 25 to 29     Age 30 to 34     Age 35 to 39     Age 40 to 44     Age 45 to 49     Age 50 to 54     Age 55 to 59     Age 60 to 64 
#           18359            18089            22149            24190            27836            27568            26422            22790            19109 
#    Age 65 to 69     Age 70 and above 
#           16678                39419 
BRFSS03$AGE <- as.factor(BRFSS03$AGE)
class(BRFSS03$AGE)


#SEX

#Observe data
table(BRFSS03$SEX)
#Recode sex data into categories corresponding to BRFSS codebook
BRFSS03$SEX <- recode(BRFSS03$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS03$SEX)
#Female   Male 
#160284 104400
BRFSS03$SEX <- as.factor(BRFSS03$SEX)
class(BRFSS03$SEX)


#BMI

#Observe data
table(BRFSS03$`_BMI3`)
#recode unknown numerical to NA
BRFSS03$`_BMI3` <- unknownToNA(BRFSS03$`_BMI3`, unknown = "9999")
#two implied decimal places, therefore pull comma back two places
BRFSS03 <- BRFSS03 %>%
  mutate(`_BMI3` = `_BMI3`/100)
#check that unknown data do not appear
BRFSS03 %>% top_n(10, `_BMI3`)

#Create another BMI variable using BMI categories for figures
BRFSS03$BMICAT <- ifelse(BRFSS03$`_BMI3` < 18.50, "Underweight",
                  ifelse(BRFSS03$`_BMI3` >= 18.50 & BRFSS03$`_BMI3` < 25.00, "Healthy weight",
                  ifelse(BRFSS03$`_BMI3` >= 25.00 & BRFSS03$`_BMI3` < 30.00, "Overweight",
                  ifelse(BRFSS03$`_BMI3` >= 30.00, "Obese",
                  NA))))

table(BRFSS03$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#         97800          58493          90831           4734 

#__________________________________________________________________________________________________________________

#DESIGN OBJECT CREATION

#Some final checks:
options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS03_dataset <- subset(BRFSS03,
                          !is.na(`_PSU`) &
                            !is.na(`_STSTR`) &
                            !is.na(`_FINALWT`) &
                            !is.na(HAVARTH2))

#Check that the sum of the weights is equal to the US population
sum(BRFSS03_dataset$`_FINALWT`)
#The sum of the weights is 212 665 720

#Check the number of people (unique PSU's)
length(unique(BRFSS03_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 63 410

#Check the number of unique strata
length(unique(BRFSS03_dataset[["_STSTR"]]))
#The number of unique strata is 514

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS03_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_FINALWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS03_dataset)
#Observe the design oject
BRFSS03_DO


#_______________________________________________________________________________________________

#----Analysis----

#1. Overall prevalence
B03_overall <- svymean(~factor(HAVARTH2),
                       BRFSS03_DO,
                       na.rm = TRUE)
B03_overall.c <- B03_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B03_overall_ci <- confint(B03_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B03 <- bind_cols(B03_overall.c, B03_overall_ci)
#remove HAVARTH2 = 0
B03 <- B03[-c(1), ] #B03 = final proportion, se and 95% ci

#Overall number of people
B03.no <- svytotal(~HAVARTH2, 
                   BRFSS03_DO, 
                   na.rm = TRUE, 
                   deff = TRUE)
B03.no.df <- as.data.frame(B03.no)
#ci
B03.no.ci <- confint(B03.no) %>%
  as.data.frame(.)
#join number and ci
B03.no <- bind_cols(B03.no.df, B03.no.ci)
#remove HAVARTH2=0
B03.no <- B03.no[-c(1), ] #B03.no = final number of people with arth, se, design effect and 95%ci


#2. Demographic-specific analysis

#A. Arthritis & Age
B03_Arth_age <- svyby(formula = ~HAVARTH2,
                      by = ~AGE,
                      design = BRFSS03_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B03_Arthtitis_age <- B03_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B03_arth_age_ci <- confint(B03_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0 (No)
B03_arth_age_ci <- B03_arth_age_ci[-c(1:11), ]
#join ci and proportions
B03.Age <- bind_cols(B03_Arthtitis_age, B03_arth_age_ci) #B03.Age = final proportion, se and 95% ci by age group


#Number of people by age
B03.no.age <- svyby(formula = ~HAVARTH2,
                    by = ~AGE,
                    design = BRFSS03_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B03.no.age.c <- B03.no.age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Number of People", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B03.no.age.ci <- confint(B03.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0 (No), which are the first 11 rows
B03.no.age.ci <- B03.no.age.ci[-c(1:11), ]
#join number and ci
B03.no.age <- bind_cols(B03.no.age.c, B03.no.age.ci)


#Age logistic regression
B03_age_glm <- svyglm(HAVARTH2~AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS03_DO)
summary(B03_age_glm)
exp(cbind(OR=coef(B03_age_glm), confint(B03_age_glm)))



#B. Arthritis & Sex
B03_Arth_sex <- svyby(formula = ~HAVARTH2,
                      by = ~SEX,
                      design = BRFSS03_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B03_Arthritis_sex <- B03_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B03_arth_sex_ci <- confint(B03_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0 (No)
B03_arth_sex_ci <- B03_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B03.Sex <- bind_cols(B03_Arthritis_sex, B03_arth_sex_ci) #B03.Sex = final proportion, se and 95% ci by sex


#Number of people by sex
B03.no.sex <- svyby(formula = ~HAVARTH2,
                    by = ~SEX,
                    design = BRFSS03_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B03.no.sex.c <- B03.no.sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Number of People", "SE")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B03.no.sex.ci <- confint(B03.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0 (No), which are the first 2 rows
B03.no.sex.ci <- B03.no.sex.ci[-c(1:2), ]
#join number and ci
B03.no.sex <- bind_cols(B03.no.sex.c, B03.no.sex.ci)


#Sex logistic regression
B03_sex_glm <- svyglm(HAVARTH2~relevel(SEX, ref = "Male") + AGE,
                      family = quasibinomial,
                      design = BRFSS03_DO)
summary(B03_sex_glm)
exp(cbind(OR=coef(B03_sex_glm), confint(B03_sex_glm)))


#C. Arthritis & BMI
B03_Arth_BMI <- svyby(formula = ~HAVARTH2,
                      by = ~BMICAT,
                      design = BRFSS03_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B03_Arthritis_BMI <- B03_Arth_BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Proportion", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B03_arth_BMI_ci <- confint(B03_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0
B03_arth_BMI_ci <- B03_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
B03.BMI <- bind_cols(B03_Arthritis_BMI, B03_arth_BMI_ci) #B03.BMI = final proportion, se and 95%ci by BMI


#Number of people by BMI
B03.no.BMI <- svyby(formula = ~HAVARTH2,
                    by = ~BMICAT,
                    design = BRFSS03_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B03.no.BMI.c <- B03.no.BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Number of People", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B03.no.BMI.ci <- confint(B03.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0 (No), which are the first 4 rows
B03.no.BMI.ci <- B03.no.BMI.ci[-c(1:4), ]
#join number and ci
B03.no.BMI <- bind_cols(B03.no.BMI.c, B03.no.BMI.ci)


#BMI logistic regression (using original continuous variable from survey)
B03.BMI.glm <- svyglm(HAVARTH2 ~ `_BMI3` + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS03_DO)
summary(B03.BMI.glm)
exp(cbind(OR=coef(B03.BMI.glm), confint(B03.BMI.glm)))


#   End of 2003 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(BRFSS03)
remove(BRFSS03_dataset)
remove(BRFSS03_DO)
remove(BRFSS03_raw)
remove(B03_Arth_age)
remove(B03_arth_age_ci)
remove(B03_Arth_BMI)
remove(B03_arth_BMI_ci)
remove(B03_Arth_sex)
remove(B03_arth_sex_ci)
remove(B03_Arthritis_BMI)
remove(B03_Arthritis_sex)
remove(B03_Arthtitis_age)
remove(B03_arth_age_ci)
remove(B03_overall.c)
remove(B03_overall_ci)
remove(B03.no.age.c)
remove(B03.no.age.ci)
remove(B03.no.BMI.c)
remove(B03.no.BMI.ci)
remove(B03.no.df)
remove(B03.no.ci)
remove(B03.no.sex.c)
remove(B03.no.sex.ci)
remove(B03_age_glm)
remove(B03_sex_glm)
remove(B03.BMI.glm)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

#Behavioural Risk Factor Surveillance System (BRFSS)
#2004


#----2004----


#----Download----

B04_url <- "http://www.cdc.gov/brfss/annual_data/2004/files/CDBRFS04XPT.zip"

tempB04 <- tempfile()
tempB04b <- tempfile()

download.file(B04_url, tempB04)
unzip(zipfile = tempB04, exdir = tempB04b)
BRFSS04_raw <- read_xpt(file.path(tempB04b, "CDBRFS04.XPT"))

unlink(c(tempB04, tempB04b))

#_______________________________________________________________________________________________

#----Cleaning----

#Observe the dataset
str(BRFSS04_raw) 
tail(BRFSS04_raw) 
glimpse(BRFSS04_raw) 
colnames(BRFSS04_raw)

BRFSS04 <- select(BRFSS04_raw,
                  "_PSU", "_FINALWT", "_STSTR", "HAVARTH2",
                  "AGE", "SEX", "_BMI4") 

#Observations
str(BRFSS04)
tail(BRFSS04)
glimpse(BRFSS04)
colnames(BRFSS04)


#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS04)) #Large proportion of arthritis data "missing", which makes sense due to being a module year
which(colSums(is.na(BRFSS04)) == nrow(BRFSS04)) #Named integer = 0, which means that at least one reponse is present for the variables selected.


#HAVARTH2

#Observe how many people answered yes, no, don't know, & refused
table(BRFSS04$HAVARTH2)
#    1     2     7     9 
#17763 33223   172    12  

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS04$HAVARTH2 <- recode(BRFSS04$HAVARTH2,
                           "1" = "1",
                           "2" = "0",
                           "7" = "7",
                           "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS04$HAVARTH2 <- unknownToNA(BRFSS04$HAVARTH2, unknown = c("7", "9")))
table(BRFSS04$HAVARTH2)
#    0     1 
#33223 17763
BRFSS04$HAVARTH2 <- as.factor(BRFSS04$HAVARTH2)
class(BRFSS04$HAVARTH2)


#AGE

#Observe data
table(BRFSS04$AGE)
#Need to make this continuous data analogous to later BRFSS 5-year categories in order to make meaningful comparisons between years
#Recode age data into categories corresponding to later BRFSS codebook categories
BRFSS04$AGE<- recode(BRFSS04$AGE,
                     "7" = "7",
                     "9" = "9",
                     "18" = "Age 18 to 24",
                     "19" = "Age 18 to 24",
                     "20" = "Age 18 to 24",
                     "21" = "Age 18 to 24",
                     "22" = "Age 18 to 24",
                     "23" = "Age 18 to 24",
                     "24" = "Age 18 to 24",
                     "25" = "Age 25 to 29",
                     "26" = "Age 25 to 29",
                     "27" = "Age 25 to 29",
                     "28" = "Age 25 to 29",
                     "29" = "Age 25 to 29",
                     "30" = "Age 30 to 34",
                     "31" = "Age 30 to 34",
                     "32" = "Age 30 to 34",
                     "33" = "Age 30 to 34",
                     "34" = "Age 30 to 34",
                     "35" = "Age 35 to 39",
                     "36" = "Age 35 to 39",
                     "37" = "Age 35 to 39",
                     "38" = "Age 35 to 39",
                     "39" = "Age 35 to 39",
                     "40" = "Age 40 to 44",
                     "41" = "Age 40 to 44",
                     "42" = "Age 40 to 44",
                     "43" = "Age 40 to 44",
                     "44" = "Age 40 to 44",
                     "45" = "Age 45 to 49",
                     "46" = "Age 45 to 49",
                     "47" = "Age 45 to 49",
                     "48" = "Age 45 to 49",
                     "49" = "Age 45 to 49",
                     "50" = "Age 50 to 54",
                     "51" = "Age 50 to 54",
                     "52" = "Age 50 to 54",
                     "53" = "Age 50 to 54",
                     "54" = "Age 50 to 54",
                     "55" = "Age 55 to 59",
                     "56" = "Age 55 to 59",
                     "57" = "Age 55 to 59",
                     "58" = "Age 55 to 59",
                     "59" = "Age 55 to 59",
                     "60" = "Age 60 to 64",
                     "61" = "Age 60 to 64",
                     "62" = "Age 60 to 64",
                     "63" = "Age 60 to 64",
                     "64" = "Age 60 to 64",
                     "65" = "Age 65 to 69",
                     "66" = "Age 65 to 69",
                     "67" = "Age 65 to 69",
                     "68" = "Age 65 to 69", 
                     "69" = "Age 65 to 69",
                     "70" = "Age 70 and above",
                     "71" = "Age 70 and above",
                     "72" = "Age 70 and above",
                     "73" = "Age 70 and above",
                     "74" = "Age 70 and above",
                     "75" = "Age 70 and above",
                     "76" = "Age 70 and above",
                     "77" = "Age 70 and above",
                     "78" = "Age 70 and above",
                     "79" = "Age 70 and above",
                     "80" = "Age 70 and above",
                     "81" = "Age 70 and above",
                     "82" = "Age 70 and above",
                     "83" = "Age 70 and above",
                     "84" = "Age 70 and above",
                     "85" = "Age 70 and above",
                     "86" = "Age 70 and above",
                     "87" = "Age 70 and above",
                     "88" = "Age 70 and above",
                     "89" = "Age 70 and above",
                     "90" = "Age 70 and above",
                     "91" = "Age 70 and above",
                     "92" = "Age 70 and above",
                     "93" = "Age 70 and above",
                     "94" = "Age 70 and above",
                     "95" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "98" = "Age 70 and above",
                     "99" = "Age 70 and above")
#Change the unknown (7) and refused (9) values to NA
(BRFSS04$AGE <- unknownToNA(BRFSS04$AGE, unknown = c("7", "9")))
table(BRFSS04$AGE)

#    Age 18 to 24     Age 25 to 29     Age 30 to 34     Age 35 to 39     Age 40 to 44     Age 45 to 49     Age 50 to 54 
#           18909            19933            24405            26755            30237            31476            30704 
#    Age 55 to 59     Age 60 to 64     Age 65 to 69 Age 70 and above 
#           27754            23125            20532            47962
BRFSS04$AGE <- as.factor(BRFSS04$AGE)
class(BRFSS04$AGE)


#SEX

#Observe data
table(BRFSS04$SEX)
#Recode sex data into categories corresponding to BRFSS codebook
BRFSS04$SEX <- recode(BRFSS04$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS04$SEX)
#Female   Male 
#186256 117566
BRFSS04$SEX <- as.factor(BRFSS04$SEX)
class(BRFSS04$SEX)


#BMI

#Observe data
table(BRFSS04$`_BMI4`)
#two implied decimal places, therefore pull comma back two places
BRFSS04 <- BRFSS04 %>%
  mutate(`_BMI4` = `_BMI4`/100)
BRFSS04$`_BMI4` <- unknownToNA(BRFSS04$`_BMI4`, unknown = "99.99") #as per codebook
BRFSS04 %>% top_n(10, `_BMI4`) #check that 99.99 does not appear

#Create another BMI variable using BMI categories for figures
BRFSS04$BMICAT <- ifelse(BRFSS04$`_BMI4` < 18.5, "Underweight",
                  ifelse(BRFSS04$`_BMI4` >= 18.5 & BRFSS04$`_BMI4` < 25.0, "Healthy weight",
                  ifelse(BRFSS04$`_BMI4` >= 25.0 & BRFSS04$`_BMI4` < 30.0, "Overweight",
                  ifelse(BRFSS04$`_BMI4` >= 30.0, "Obese",
                  NA))))

table(BRFSS04$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#        109123          69452         105753           5123


#__________________________________________________________________________________________________________________
#DESIGN OBJECT CREATION

#Some final checks:
options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS04_dataset <- subset(BRFSS04,
                          !is.na(`_PSU`) &
                          !is.na(`_STSTR`) &
                          !is.na(`_FINALWT`) &
                          !is.na(HAVARTH2))

#Check that the sum of the weights is equal to the US population
sum(BRFSS04_dataset$`_FINALWT`)
#The sum of the weights is 45 088 268 {MODULE YEAR SO REDUCED POPULATION SIZE TO BE EXPECTED}

#Check the number of people (unique PSU's)
length(unique(BRFSS04_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 34 156

#Check the number of unique strata
length(unique(BRFSS04_dataset[["_STSTR"]]))
#The number of unique strata is 144

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS04_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_FINALWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS04_dataset)
#Observe the design oject
BRFSS04_DO


#_______________________________________________________________________________________________

#----Analysis----

#1. Overall
# Prevalence
B04_overall <- svymean(~factor(HAVARTH2),
                       BRFSS04_DO,
                       na.rm = TRUE)
B04_overall.c <- B04_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B04_overall_ci <- confint(B04_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B04 <- bind_cols(B04_overall.c, B04_overall_ci)
#remove havarth = 0
B04 <- B04[-c(1), ] #B04 = final proportion, se and 95% ci


#2. Demographic analysis

#A. Arthritis & Age
B04_Arth_age <- svyby(formula = ~HAVARTH2,
                      by = ~AGE,
                      design = BRFSS04_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B04_Arthtitis_age <- B04_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B04_arth_age_ci <- confint(B04_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0 (No)
B04_arth_age_ci <- B04_arth_age_ci[-c(1:11), ]
#join ci and proportions
B04.Age <- bind_cols(B04_Arthtitis_age, B04_arth_age_ci) #B04.Age = final proportion, se and 95% ci by age group

#Age logistic regression
B04_age_glm <- svyglm(HAVARTH2~AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS04_DO)
summary(B04_age_glm)
exp(cbind(OR=coef(B04_age_glm), confint(B04_age_glm)))



#B. Arthritis & Sex
B04_Arth_sex <- svyby(formula = ~HAVARTH2,
                      by = ~SEX,
                      design = BRFSS04_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B04_Arthritis_sex <- B04_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B04_arth_sex_ci <- confint(B04_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0 (No)
B04_arth_sex_ci <- B04_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B04.Sex <- bind_cols(B04_Arthritis_sex, B04_arth_sex_ci) #B04.Sex = final proportion, se and 95% ci by sex

#Sex logistic regression
B04_sex_glm <- svyglm(HAVARTH2~relevel(SEX, ref = "Male") + AGE,
                      family = quasibinomial,
                      design = BRFSS04_DO)
summary(B04_sex_glm)
exp(cbind(OR=coef(B04_sex_glm), confint(B04_sex_glm)))


#C. Arthritis & BMI
B04_Arth_BMI <- svyby(formula = ~HAVARTH2,
                      by = ~BMICAT,
                      design = BRFSS04_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B04_Arthritis_BMI <- B04_Arth_BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Proportion", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B04_arth_BMI_ci <- confint(B04_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0
B04_arth_BMI_ci <- B04_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
B04.BMI <- bind_cols(B04_Arthritis_BMI, B04_arth_BMI_ci) #B04.BMI = final proportion, se and 95%ci by BMI

#BMI logistic regression (using original continuous variable from survey)
B04.BMI.glm <- svyglm(HAVARTH2 ~ `_BMI4` + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS04_DO)
summary(B04.BMI.glm)
exp(cbind(OR=coef(B04.BMI.glm), confint(B04.BMI.glm)))


#   End of 2004 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(BRFSS04)
remove(BRFSS04_dataset)
remove(BRFSS04_DO)
remove(BRFSS04_raw)
remove(B04_Arth_age)
remove(B04_arth_age_ci)
remove(B04_Arth_BMI)
remove(B04_arth_BMI_ci)
remove(B04_Arth_sex)
remove(B04_arth_sex_ci)
remove(B04_Arthritis_BMI)
remove(B04_Arthritis_sex)
remove(B04_Arthtitis_age)
remove(B04_arth_age_ci)
remove(B04_overall.c)
remove(B04_overall_ci)
remove(B04.no.age.c)
remove(B04.no.age.ci)
remove(B04.no.BMI.c)
remove(B04.no.BMI.ci)
remove(B04.no.df)
remove(B04.no.ci)
remove(B04.no.sex.c)
remove(B04.no.sex.ci)
remove(B04_age_glm)
remove(B04_sex_glm)
remove(B04.BMI.glm)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

#Behavioural Risk Factor Surveillance System (BRFSS)
#2005


#----2005----


#----Download----

B05_url <- "http://www.cdc.gov/brfss/annual_data/2005/files/CDBRFS05XPT.zip"

tempB05 <- tempfile()
tempB05b <- tempfile()

download.file(B05_url, tempB05)
unzip(zipfile = tempB05, exdir = tempB05b)
BRFSS05_raw <- read_xpt(file.path(tempB05b, "CDBRFS05.XPT"))

unlink(c(tempB05, tempB05b))

#_______________________________________________________________________________________________

#----Cleaning----

#Observe the dataset
str(BRFSS05_raw) 
tail(BRFSS05_raw) 
glimpse(BRFSS05_raw) 
colnames(BRFSS05_raw)

BRFSS05 <- select(BRFSS05_raw,
                  "_PSU", "_FINALWT", "_STSTR", "HAVARTH2",
                  "AGE", "SEX", "_BMI4") 

#Observations
str(BRFSS05)
tail(BRFSS05)
glimpse(BRFSS05)
colnames(BRFSS05)


#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS05))
which(colSums(is.na(BRFSS05)) == nrow(BRFSS05)) #Named integer = 0, which means that at least one reponse is present for the variables selected.


#HAVARTH

#Observe how many people answered yes, no, don't know, & refused
table(BRFSS05$HAVARTH2)
#     1      2      7      9 
#119485 230651   1417    100

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS05$HAVARTH2 <- recode(BRFSS05$HAVARTH2,
                           "1" = "1",
                           "2" = "0",
                           "7" = "7",
                           "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS05$HAVARTH2 <- unknownToNA(BRFSS05$HAVARTH2, unknown = c("7", "9")))
table(BRFSS05$HAVARTH2)
#     0      1 
#230651 119485
BRFSS05$HAVARTH2 <- as.factor(BRFSS05$HAVARTH2)
class(BRFSS05$HAVARTH2)


#AGE

#Observe data
table(BRFSS05$AGE)
#Need to make this continuous data analogous to later BRFSS 5-year categories in order to make meaningful comparisons between years
#Recode age data into categories corresponding to later BRFSS codebook categories
BRFSS05$AGE<- recode(BRFSS05$AGE,
                     "7" = "7",
                     "9" = "9",
                     "18" = "Age 18 to 24",
                     "19" = "Age 18 to 24",
                     "20" = "Age 18 to 24",
                     "21" = "Age 18 to 24",
                     "22" = "Age 18 to 24",
                     "23" = "Age 18 to 24",
                     "24" = "Age 18 to 24",
                     "25" = "Age 25 to 29",
                     "26" = "Age 25 to 29",
                     "27" = "Age 25 to 29",
                     "28" = "Age 25 to 29",
                     "29" = "Age 25 to 29",
                     "30" = "Age 30 to 34",
                     "31" = "Age 30 to 34",
                     "32" = "Age 30 to 34",
                     "33" = "Age 30 to 34",
                     "34" = "Age 30 to 34",
                     "35" = "Age 35 to 39",
                     "36" = "Age 35 to 39",
                     "37" = "Age 35 to 39",
                     "38" = "Age 35 to 39",
                     "39" = "Age 35 to 39",
                     "40" = "Age 40 to 44",
                     "41" = "Age 40 to 44",
                     "42" = "Age 40 to 44",
                     "43" = "Age 40 to 44",
                     "44" = "Age 40 to 44",
                     "45" = "Age 45 to 49",
                     "46" = "Age 45 to 49",
                     "47" = "Age 45 to 49",
                     "48" = "Age 45 to 49",
                     "49" = "Age 45 to 49",
                     "50" = "Age 50 to 54",
                     "51" = "Age 50 to 54",
                     "52" = "Age 50 to 54",
                     "53" = "Age 50 to 54",
                     "54" = "Age 50 to 54",
                     "55" = "Age 55 to 59",
                     "56" = "Age 55 to 59",
                     "57" = "Age 55 to 59",
                     "58" = "Age 55 to 59",
                     "59" = "Age 55 to 59",
                     "60" = "Age 60 to 64",
                     "61" = "Age 60 to 64",
                     "62" = "Age 60 to 64",
                     "63" = "Age 60 to 64",
                     "64" = "Age 60 to 64",
                     "65" = "Age 65 to 69",
                     "66" = "Age 65 to 69",
                     "67" = "Age 65 to 69",
                     "68" = "Age 65 to 69", 
                     "69" = "Age 65 to 69",
                     "70" = "Age 70 and above",
                     "71" = "Age 70 and above",
                     "72" = "Age 70 and above",
                     "73" = "Age 70 and above",
                     "74" = "Age 70 and above",
                     "75" = "Age 70 and above",
                     "76" = "Age 70 and above",
                     "77" = "Age 70 and above",
                     "78" = "Age 70 and above",
                     "79" = "Age 70 and above",
                     "80" = "Age 70 and above",
                     "81" = "Age 70 and above",
                     "82" = "Age 70 and above",
                     "83" = "Age 70 and above",
                     "84" = "Age 70 and above",
                     "85" = "Age 70 and above",
                     "86" = "Age 70 and above",
                     "87" = "Age 70 and above",
                     "88" = "Age 70 and above",
                     "89" = "Age 70 and above",
                     "90" = "Age 70 and above",
                     "91" = "Age 70 and above",
                     "92" = "Age 70 and above",
                     "93" = "Age 70 and above",
                     "94" = "Age 70 and above",
                     "95" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "98" = "Age 70 and above",
                     "99" = "Age 70 and above")
#Change the unknown (7) and refused (9) values to NA
(BRFSS05$AGE <- unknownToNA(BRFSS05$AGE, unknown = c("7", "9")))
table(BRFSS05$AGE)

#    Age 18 to 24     Age 25 to 29     Age 30 to 34     Age 35 to 39     Age 40 to 44     Age 45 to 49     Age 50 to 54     Age 55 to 59 
#           18290            20602            26011            29844            33581            36288            37009            35078 
#    Age 60 to 64     Age 65 to 69 Age 70 and above 
#           29363            25530            61781 
BRFSS05$AGE <- as.factor(BRFSS05$AGE)
class(BRFSS05$AGE)


#SEX

#Observe data
table(BRFSS05$SEX)
#Recode sex data into categories corresponding to BRFSS codebook
BRFSS05$SEX <- recode(BRFSS05$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS05$SEX)
#Female   Male 
#219911 136201
BRFSS05$SEX <- as.factor(BRFSS05$SEX)
class(BRFSS05$SEX)


#BMI

#Observe data
table(BRFSS05$`_BMI4`)
#recode unknown numerical to NA
BRFSS05$`_BMI4` <- unknownToNA(BRFSS05$`_BMI4`, unknown = "9999")
#two implied decimal places, therefore pull comma back two places
BRFSS05 <- BRFSS05 %>%
  mutate(`_BMI4` = `_BMI4`/100)
#check that unknown data do not appear
BRFSS05 %>% top_n(10, `_BMI4`)

#Create another BMI variable using BMI categories for figures
BRFSS05$BMICAT <- ifelse(BRFSS05$`_BMI4` < 18.50, "Underweight",
                  ifelse(BRFSS05$`_BMI4` >= 18.50 & BRFSS05$`_BMI4` < 25.00, "Healthy weight",
                  ifelse(BRFSS05$`_BMI4` >= 25.00 & BRFSS05$`_BMI4` < 30.00, "Overweight",
                  ifelse(BRFSS05$`_BMI4` >= 30.00, "Obese",
                  NA))))

table(BRFSS05$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#        123734          86463         123692           5779 

#__________________________________________________________________________________________________________________

#DESIGN OBJECT CREATION

#Some final checks:
options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS05_dataset <- subset(BRFSS05,
                          !is.na(`_PSU`) &
                            !is.na(`_STSTR`) &
                            !is.na(`_FINALWT`) &
                            !is.na(HAVARTH2))

#Check that the sum of the weights is equal to the US population
sum(BRFSS05_dataset$`_FINALWT`)
#The sum of the weights is 219 154 382

#Check the number of people (unique PSU's)
length(unique(BRFSS05_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 101 753

#Check the number of unique strata
length(unique(BRFSS05_dataset[["_STSTR"]]))
#The number of unique strata is 771

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS05_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_FINALWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS05_dataset)
#Observe the design oject
BRFSS05_DO


#_______________________________________________________________________________________________

#----Analysis----

#1. Overall prevalence
B05_overall <- svymean(~factor(HAVARTH2),
                       BRFSS05_DO,
                       na.rm = TRUE)
B05_overall.c <- B05_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B05_overall_ci <- confint(B05_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B05 <- bind_cols(B05_overall.c, B05_overall_ci)
#remove HAVARTH2 = 0
B05 <- B05[-c(1), ] #B05 = final proportion, se and 95% ci

#Overall number of people
B05.no <- svytotal(~HAVARTH2, 
                   BRFSS05_DO, 
                   na.rm = TRUE, 
                   deff = TRUE)
B05.no.df <- as.data.frame(B05.no)
#ci
B05.no.ci <- confint(B05.no) %>%
  as.data.frame(.)
#join number and ci
B05.no <- bind_cols(B05.no.df, B05.no.ci)
#remove HAVARTH2=0
B05.no <- B05.no[-c(1), ] #B05.no = final number of people with arth, se, design effect and 95%ci


#2. Demographic-specific analysis

#A. Arthritis & Age
B05_Arth_age <- svyby(formula = ~HAVARTH2,
                      by = ~AGE,
                      design = BRFSS05_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B05_Arthtitis_age <- B05_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B05_arth_age_ci <- confint(B05_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0 (No)
B05_arth_age_ci <- B05_arth_age_ci[-c(1:11), ]
#join ci and proportions
B05.Age <- bind_cols(B05_Arthtitis_age, B05_arth_age_ci) #B05.Age = final proportion, se and 95% ci by age group


#Number of people by age
B05.no.age <- svyby(formula = ~HAVARTH2,
                    by = ~AGE,
                    design = BRFSS05_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B05.no.age.c <- B05.no.age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Number of People", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B05.no.age.ci <- confint(B05.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0 (No), which are the first 11 rows
B05.no.age.ci <- B05.no.age.ci[-c(1:11), ]
#join number and ci
B05.no.age <- bind_cols(B05.no.age.c, B05.no.age.ci)


#Age logistic regression
B05_age_glm <- svyglm(HAVARTH2~AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS05_DO)
summary(B05_age_glm)
exp(cbind(OR=coef(B05_age_glm), confint(B05_age_glm)))



#B. Arthritis & Sex
B05_Arth_sex <- svyby(formula = ~HAVARTH2,
                      by = ~SEX,
                      design = BRFSS05_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B05_Arthritis_sex <- B05_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B05_arth_sex_ci <- confint(B05_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0 (No)
B05_arth_sex_ci <- B05_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B05.Sex <- bind_cols(B05_Arthritis_sex, B05_arth_sex_ci) #B05.Sex = final proportion, se and 95% ci by sex

#Number of people by sex
B05.no.sex <- svyby(formula = ~HAVARTH2,
                    by = ~SEX,
                    design = BRFSS05_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B05.no.sex.c <- B05.no.sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Number of People", "SE")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B05.no.sex.ci <- confint(B05.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0 (No), which are the first 2 rows
B05.no.sex.ci <- B05.no.sex.ci[-c(1:2), ]
#join number and ci
B05.no.sex <- bind_cols(B05.no.sex.c, B05.no.sex.ci)

#Sex logistic regression
B05_sex_glm <- svyglm(HAVARTH2~relevel(SEX, ref = "Male") + AGE,
                      family = quasibinomial,
                      design = BRFSS05_DO)
summary(B05_sex_glm)
exp(cbind(OR=coef(B05_sex_glm), confint(B05_sex_glm)))


#C. Arthritis & BMI
B05_Arth_BMI <- svyby(formula = ~HAVARTH2,
                      by = ~BMICAT,
                      design = BRFSS05_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B05_Arthritis_BMI <- B05_Arth_BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Proportion", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B05_arth_BMI_ci <- confint(B05_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0
B05_arth_BMI_ci <- B05_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
B05.BMI <- bind_cols(B05_Arthritis_BMI, B05_arth_BMI_ci) #B05.BMI = final proportion, se and 95%ci by BMI


#Number of people by BMI
B05.no.BMI <- svyby(formula = ~HAVARTH2,
                    by = ~BMICAT,
                    design = BRFSS05_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B05.no.BMI.c <- B05.no.BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Number of People", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B05.no.BMI.ci <- confint(B05.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0 (No), which are the first 4 rows
B05.no.BMI.ci <- B05.no.BMI.ci[-c(1:4), ]
#join number and ci
B05.no.BMI <- bind_cols(B05.no.BMI.c, B05.no.BMI.ci)

#BMI logistic regression (using original continuous variable from survey)
B05.BMI.glm <- svyglm(HAVARTH2 ~ `_BMI4` + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS05_DO)
summary(B05.BMI.glm)
exp(cbind(OR=coef(B05.BMI.glm), confint(B05.BMI.glm)))


#   End of 2005 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(BRFSS05)
remove(BRFSS05_dataset)
remove(BRFSS05_DO)
remove(BRFSS05_raw)
remove(B05_Arth_age)
remove(B05_arth_age_ci)
remove(B05_Arth_BMI)
remove(B05_arth_BMI_ci)
remove(B05_Arth_sex)
remove(B05_arth_sex_ci)
remove(B05_Arthritis_BMI)
remove(B05_Arthritis_sex)
remove(B05_Arthtitis_age)
remove(B05_arth_age_ci)
remove(B05_overall.c)
remove(B05_overall_ci)
remove(B05.no.age.c)
remove(B05.no.age.ci)
remove(B05.no.BMI.c)
remove(B05.no.BMI.ci)
remove(B05.no.df)
remove(B05.no.ci)
remove(B05.no.sex.c)
remove(B05.no.sex.ci)
remove(B05_age_glm)
remove(B05_sex_glm)
remove(B05.BMI.glm)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________
#Behavioural Risk Factor Surveillance System (BRFSS)
#no 2006
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

#Behavioural Risk Factor Surveillance System (BRFSS)
#2007


#----2007----


#----Download----

B07_url <- "http://www.cdc.gov/brfss/annual_data/2007/files/CDBRFS07XPT.ZIP"

tempB07 <- tempfile()
tempB07b <- tempfile()

download.file(B07_url, tempB07, mode = "wb")
unzip(zipfile = tempB07, exdir = tempB07b)
BRFSS07_raw <- read_xpt(file.path(tempB07b, "CDBRFS07.XPT"))

unlink(c(tempB07, tempB07b))

#_______________________________________________________________________________________________

#----Cleaning----

#Observe the dataset
str(BRFSS07_raw) 
tail(BRFSS07_raw) 
glimpse(BRFSS07_raw) 
colnames(BRFSS07_raw)

BRFSS07 <- select(BRFSS07_raw,
                  "_PSU", "_FINALWT", "_STSTR", "HAVARTH2",
                  "AGE", "SEX", "_BMI4") 

#Observations
str(BRFSS07)
tail(BRFSS07)
glimpse(BRFSS07)
colnames(BRFSS07)


#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS07))
which(colSums(is.na(BRFSS07)) == nrow(BRFSS07)) #Named integer = 0, which means that at least one reponse is present for the variables selected.


#HAVARTH

#Observe how many people answered yes, no, don't know, & refused
table(BRFSS07$HAVARTH2)
#     1      2      7      9 
#154710 267037   1732    129

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS07$HAVARTH2 <- recode(BRFSS07$HAVARTH2,
                           "1" = "1",
                           "2" = "0",
                           "7" = "7",
                           "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS07$HAVARTH2 <- unknownToNA(BRFSS07$HAVARTH2, unknown = c("7", "9")))
table(BRFSS07$HAVARTH2)
#     0      1 
#267037 154710
BRFSS07$HAVARTH2 <- as.factor(BRFSS07$HAVARTH2)
class(BRFSS07$HAVARTH2)


#AGE

#Observe data
table(BRFSS07$AGE)
#Need to make this continuous data analogous to later BRFSS 5-year categories in order to make meaningful comparisons between years
#Recode age data into categories corresponding to later BRFSS codebook categories
BRFSS07$AGE<- recode(BRFSS07$AGE,
                     "7" = "7",
                     "9" = "9",
                     "18" = "Age 18 to 24",
                     "19" = "Age 18 to 24",
                     "20" = "Age 18 to 24",
                     "21" = "Age 18 to 24",
                     "22" = "Age 18 to 24",
                     "23" = "Age 18 to 24",
                     "24" = "Age 18 to 24",
                     "25" = "Age 25 to 29",
                     "26" = "Age 25 to 29",
                     "27" = "Age 25 to 29",
                     "28" = "Age 25 to 29",
                     "29" = "Age 25 to 29",
                     "30" = "Age 30 to 34",
                     "31" = "Age 30 to 34",
                     "32" = "Age 30 to 34",
                     "33" = "Age 30 to 34",
                     "34" = "Age 30 to 34",
                     "35" = "Age 35 to 39",
                     "36" = "Age 35 to 39",
                     "37" = "Age 35 to 39",
                     "38" = "Age 35 to 39",
                     "39" = "Age 35 to 39",
                     "40" = "Age 40 to 44",
                     "41" = "Age 40 to 44",
                     "42" = "Age 40 to 44",
                     "43" = "Age 40 to 44",
                     "44" = "Age 40 to 44",
                     "45" = "Age 45 to 49",
                     "46" = "Age 45 to 49",
                     "47" = "Age 45 to 49",
                     "48" = "Age 45 to 49",
                     "49" = "Age 45 to 49",
                     "50" = "Age 50 to 54",
                     "51" = "Age 50 to 54",
                     "52" = "Age 50 to 54",
                     "53" = "Age 50 to 54",
                     "54" = "Age 50 to 54",
                     "55" = "Age 55 to 59",
                     "56" = "Age 55 to 59",
                     "57" = "Age 55 to 59",
                     "58" = "Age 55 to 59",
                     "59" = "Age 55 to 59",
                     "60" = "Age 60 to 64",
                     "61" = "Age 60 to 64",
                     "62" = "Age 60 to 64",
                     "63" = "Age 60 to 64",
                     "64" = "Age 60 to 64",
                     "65" = "Age 65 to 69",
                     "66" = "Age 65 to 69",
                     "67" = "Age 65 to 69",
                     "68" = "Age 65 to 69", 
                     "69" = "Age 65 to 69",
                     "70" = "Age 70 and above",
                     "71" = "Age 70 and above",
                     "72" = "Age 70 and above",
                     "73" = "Age 70 and above",
                     "74" = "Age 70 and above",
                     "75" = "Age 70 and above",
                     "76" = "Age 70 and above",
                     "77" = "Age 70 and above",
                     "78" = "Age 70 and above",
                     "79" = "Age 70 and above",
                     "80" = "Age 70 and above",
                     "81" = "Age 70 and above",
                     "82" = "Age 70 and above",
                     "83" = "Age 70 and above",
                     "84" = "Age 70 and above",
                     "85" = "Age 70 and above",
                     "86" = "Age 70 and above",
                     "87" = "Age 70 and above",
                     "88" = "Age 70 and above",
                     "89" = "Age 70 and above",
                     "90" = "Age 70 and above",
                     "91" = "Age 70 and above",
                     "92" = "Age 70 and above",
                     "93" = "Age 70 and above",
                     "94" = "Age 70 and above",
                     "95" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "98" = "Age 70 and above",
                     "99" = "Age 70 and above")
#Change the unknown (7) and refused (9) values to NA
(BRFSS07$AGE <- unknownToNA(BRFSS07$AGE, unknown = c("7", "9")))
table(BRFSS07$AGE)

#    Age 18 to 24     Age 25 to 29     Age 30 to 34     Age 35 to 39     Age 40 to 44     Age 45 to 49     Age 50 to 54     Age 55 to 59     Age 60 to 64 
#           15936            19042            25552            31843            35932            41815            46463            45694            42408 
#    Age 65 to 69 Age 70 and above 
#           36050            86481 
BRFSS07$AGE <- as.factor(BRFSS07$AGE)
class(BRFSS07$AGE)


#SEX

#Observe data
table(BRFSS07$SEX)
#Recode sex data into categories corresponding to BRFSS codebook
BRFSS07$SEX <- recode(BRFSS07$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS07$SEX)
#Female   Male 
#270161 160751
BRFSS07$SEX <- as.factor(BRFSS07$SEX)
class(BRFSS07$SEX)


#BMI

#Observe data
table(BRFSS07$`_BMI4`)
#recode unknown numerical to NA
BRFSS07$`_BMI4` <- unknownToNA(BRFSS07$`_BMI4`, unknown = "9999")
#two implied decimal places, therefore pull comma back two places
BRFSS07 <- BRFSS07 %>%
  mutate(`_BMI4` = `_BMI4`/100)
#check that unknown data do not appear
BRFSS07 %>% top_n(10, `_BMI4`)

#Create another BMI variable using BMI categories for figures
BRFSS07$BMICAT <- ifelse(BRFSS07$`_BMI4` < 18.50, "Underweight",
                  ifelse(BRFSS07$`_BMI4` >= 18.50 & BRFSS07$`_BMI4` < 25.00, "Healthy weight",
                  ifelse(BRFSS07$`_BMI4` >= 25.00 & BRFSS07$`_BMI4` < 30.00, "Overweight",
                  ifelse(BRFSS07$`_BMI4` >= 30.00, "Obese",
                  NA))))

table(BRFSS07$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#        141564         111958         150670           6854 

#__________________________________________________________________________________________________________________

#DESIGN OBJECT CREATION

#Some final checks:
options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS07_dataset <- subset(BRFSS07,
                          !is.na(`_PSU`) &
                            !is.na(`_STSTR`) &
                            !is.na(`_FINALWT`) &
                            !is.na(HAVARTH2))

#Check that the sum of the weights is equal to the US population
sum(BRFSS07_dataset$`_FINALWT`)
#The sum of the weights is 224 165 210

#Check the number of people (unique PSU's)
length(unique(BRFSS07_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 158 147

#Check the number of unique strata
length(unique(BRFSS07_dataset[["_STSTR"]]))
#The number of unique strata is 1 006

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS07_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_FINALWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS07_dataset)
#Observe the design oject
BRFSS07_DO

#_______________________________________________________________________________________________

#----Analysis----

#1. Overall prevalence
B07_overall <- svymean(~factor(HAVARTH2),
                       BRFSS07_DO,
                       na.rm = TRUE)
B07_overall.c <- B07_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B07_overall_ci <- confint(B07_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B07 <- bind_cols(B07_overall.c, B07_overall_ci)
#remove HAVARTH2 = 0
B07 <- B07[-c(1), ] #B07 = final proportion, se and 95% ci

#Overall number of people
B07.no <- svytotal(~HAVARTH2, 
                   BRFSS07_DO, 
                   na.rm = TRUE, 
                   deff = TRUE)
B07.no.df <- as.data.frame(B07.no)
#ci
B07.no.ci <- confint(B07.no) %>%
  as.data.frame(.)
#join number and ci
B07.no <- bind_cols(B07.no.df, B07.no.ci)
#remove HAVARTH2=0
B07.no <- B07.no[-c(1), ] #B07.no = final number of people with arth, se, design effect and 95%ci


#2. Demographic-specific analysis

#A. Arthritis & Age
B07_Arth_age <- svyby(formula = ~HAVARTH2,
                      by = ~AGE,
                      design = BRFSS07_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B07_Arthtitis_age <- B07_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B07_arth_age_ci <- confint(B07_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0 (No)
B07_arth_age_ci <- B07_arth_age_ci[-c(1:11), ]
#join ci and proportions
B07.Age <- bind_cols(B07_Arthtitis_age, B07_arth_age_ci) #B07.Age = final proportion, se and 95% ci by age group


#Number of people by age
B07.no.age <- svyby(formula = ~HAVARTH2,
                    by = ~AGE,
                    design = BRFSS07_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B07.no.age.c <- B07.no.age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Number of People", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B07.no.age.ci <- confint(B07.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0 (No), which are the first 11 rows
B07.no.age.ci <- B07.no.age.ci[-c(1:11), ]
#join number and ci
B07.no.age <- bind_cols(B07.no.age.c, B07.no.age.ci)


#Age logistic regression
B07_age_glm <- svyglm(HAVARTH2~AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS07_DO)
summary(B07_age_glm)
exp(cbind(OR=coef(B07_age_glm), confint(B07_age_glm)))



#B. Arthritis & Sex
B07_Arth_sex <- svyby(formula = ~HAVARTH2,
                      by = ~SEX,
                      design = BRFSS07_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B07_Arthritis_sex <- B07_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B07_arth_sex_ci <- confint(B07_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0 (No)
B07_arth_sex_ci <- B07_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B07.Sex <- bind_cols(B07_Arthritis_sex, B07_arth_sex_ci) #B07.Sex = final proportion, se and 95% ci by sex

#Number of people by sex
B07.no.sex <- svyby(formula = ~HAVARTH2,
                    by = ~SEX,
                    design = BRFSS07_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B07.no.sex.c <- B07.no.sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Number of People", "SE")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B07.no.sex.ci <- confint(B07.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0 (No), which are the first 2 rows
B07.no.sex.ci <- B07.no.sex.ci[-c(1:2), ]
#join number and ci
B07.no.sex <- bind_cols(B07.no.sex.c, B07.no.sex.ci)


#Sex logistic regression
B07_sex_glm <- svyglm(HAVARTH2~relevel(SEX, ref = "Male") + AGE,
                      family = quasibinomial,
                      design = BRFSS07_DO)
summary(B07_sex_glm)
exp(cbind(OR=coef(B07_sex_glm), confint(B07_sex_glm)))


#C. Arthritis & BMI
B07_Arth_BMI <- svyby(formula = ~HAVARTH2,
                      by = ~BMICAT,
                      design = BRFSS07_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B07_Arthritis_BMI <- B07_Arth_BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Proportion", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B07_arth_BMI_ci <- confint(B07_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0
B07_arth_BMI_ci <- B07_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
B07.BMI <- bind_cols(B07_Arthritis_BMI, B07_arth_BMI_ci) #B07.BMI = final proportion, se and 95%ci by BMI


#Number of people by BMI
B07.no.BMI <- svyby(formula = ~HAVARTH2,
                    by = ~BMICAT,
                    design = BRFSS07_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B07.no.BMI.c <- B07.no.BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Number of People", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B07.no.BMI.ci <- confint(B07.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0 (No), which are the first 4 rows
B07.no.BMI.ci <- B07.no.BMI.ci[-c(1:4), ]
#join number and ci
B07.no.BMI <- bind_cols(B07.no.BMI.c, B07.no.BMI.ci)


#BMI logistic regression (using original continuous variable from survey)
B07.BMI.glm <- svyglm(HAVARTH2 ~ `_BMI4` + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS07_DO)
summary(B07.BMI.glm)
exp(cbind(OR=coef(B07.BMI.glm), confint(B07.BMI.glm)))


#   End of 2007 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(BRFSS07)
remove(BRFSS07_dataset)
remove(BRFSS07_DO)
remove(BRFSS07_raw)
remove(B07_Arth_age)
remove(B07_arth_age_ci)
remove(B07_Arth_BMI)
remove(B07_arth_BMI_ci)
remove(B07_Arth_sex)
remove(B07_arth_sex_ci)
remove(B07_Arthritis_BMI)
remove(B07_Arthritis_sex)
remove(B07_Arthtitis_age)
remove(B07_arth_age_ci)
remove(B07_overall.c)
remove(B07_overall_ci)
remove(B07.no.age.c)
remove(B07.no.age.ci)
remove(B07.no.BMI.c)
remove(B07.no.BMI.ci)
remove(B07.no.df)
remove(B07.no.ci)
remove(B07.no.sex.c)
remove(B07.no.sex.ci)
remove(B07_age_glm)
remove(B07_sex_glm)
remove(B07.BMI.glm)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________
#Behavioural Risk Factor Surveillance System (BRFSS)
#no 2008
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________
#Behavioural Risk Factor Surveillance System (BRFSS)
#2009


#----2009----


#----Download----

B09_url <- "http://www.cdc.gov/brfss/annual_data/2009/files/CDBRFS09XPT.ZIP"

tempB09 <- tempfile()
tempB09b <- tempfile()

download.file(B09_url, tempB09, mode = "wb")
unzip(zipfile = tempB09, exdir = tempB09b)
BRFSS09_raw <- read_xpt(file.path(tempB09b, "CDBRFS09.XPT"))


unlink(c(tempB09, tempB09b))

#_______________________________________________________________________________________________

#----Cleaning----

#Observe the dataset
str(BRFSS09_raw) 
tail(BRFSS09_raw) 
glimpse(BRFSS09_raw) 
colnames(BRFSS09_raw)

BRFSS09 <- select(BRFSS09_raw,
                  "_PSU", "_FINALWT", "_STSTR", "HAVARTH2",
                  "AGE", "SEX", "_BMI4") 

#Observations
str(BRFSS09)
tail(BRFSS09)
glimpse(BRFSS09)
colnames(BRFSS09)


#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS09))
which(colSums(is.na(BRFSS09)) == nrow(BRFSS09)) #Named integer = 0, which means that at least one reponse is present for the variables selected.


#HAVARTH

#Observe how many people answered yes, no, don't know, & refused
table(BRFSS09$HAVARTH2)
#     1      2      7      9 
#153914 263366   2004   2167

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS09$HAVARTH2 <- recode(BRFSS09$HAVARTH2,
                           "1" = "1",
                           "2" = "0",
                           "7" = "7",
                           "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS09$HAVARTH2 <- unknownToNA(BRFSS09$HAVARTH2, unknown = c("7", "9")))
table(BRFSS09$HAVARTH2)
#     0      1 
#263366 153914
BRFSS09$HAVARTH2 <- as.factor(BRFSS09$HAVARTH2)
class(BRFSS09$HAVARTH2)


#AGE

#Observe data
table(BRFSS09$AGE)
#Need to make this continuous data analogous to later BRFSS 5-year categories in order to make meaningful comparisons between years
#Recode age data into categories corresponding to later BRFSS codebook categories
BRFSS09$AGE<- recode(BRFSS09$AGE,
                     "7" = "7",
                     "9" = "9",
                     "18" = "Age 18 to 24",
                     "19" = "Age 18 to 24",
                     "20" = "Age 18 to 24",
                     "21" = "Age 18 to 24",
                     "22" = "Age 18 to 24",
                     "23" = "Age 18 to 24",
                     "24" = "Age 18 to 24",
                     "25" = "Age 25 to 29",
                     "26" = "Age 25 to 29",
                     "27" = "Age 25 to 29",
                     "28" = "Age 25 to 29",
                     "29" = "Age 25 to 29",
                     "30" = "Age 30 to 34",
                     "31" = "Age 30 to 34",
                     "32" = "Age 30 to 34",
                     "33" = "Age 30 to 34",
                     "34" = "Age 30 to 34",
                     "35" = "Age 35 to 39",
                     "36" = "Age 35 to 39",
                     "37" = "Age 35 to 39",
                     "38" = "Age 35 to 39",
                     "39" = "Age 35 to 39",
                     "40" = "Age 40 to 44",
                     "41" = "Age 40 to 44",
                     "42" = "Age 40 to 44",
                     "43" = "Age 40 to 44",
                     "44" = "Age 40 to 44",
                     "45" = "Age 45 to 49",
                     "46" = "Age 45 to 49",
                     "47" = "Age 45 to 49",
                     "48" = "Age 45 to 49",
                     "49" = "Age 45 to 49",
                     "50" = "Age 50 to 54",
                     "51" = "Age 50 to 54",
                     "52" = "Age 50 to 54",
                     "53" = "Age 50 to 54",
                     "54" = "Age 50 to 54",
                     "55" = "Age 55 to 59",
                     "56" = "Age 55 to 59",
                     "57" = "Age 55 to 59",
                     "58" = "Age 55 to 59",
                     "59" = "Age 55 to 59",
                     "60" = "Age 60 to 64",
                     "61" = "Age 60 to 64",
                     "62" = "Age 60 to 64",
                     "63" = "Age 60 to 64",
                     "64" = "Age 60 to 64",
                     "65" = "Age 65 to 69",
                     "66" = "Age 65 to 69",
                     "67" = "Age 65 to 69",
                     "68" = "Age 65 to 69", 
                     "69" = "Age 65 to 69",
                     "70" = "Age 70 and above",
                     "71" = "Age 70 and above",
                     "72" = "Age 70 and above",
                     "73" = "Age 70 and above",
                     "74" = "Age 70 and above",
                     "75" = "Age 70 and above",
                     "76" = "Age 70 and above",
                     "77" = "Age 70 and above",
                     "78" = "Age 70 and above",
                     "79" = "Age 70 and above",
                     "80" = "Age 70 and above",
                     "81" = "Age 70 and above",
                     "82" = "Age 70 and above",
                     "83" = "Age 70 and above",
                     "84" = "Age 70 and above",
                     "85" = "Age 70 and above",
                     "86" = "Age 70 and above",
                     "87" = "Age 70 and above",
                     "88" = "Age 70 and above",
                     "89" = "Age 70 and above",
                     "90" = "Age 70 and above",
                     "91" = "Age 70 and above",
                     "92" = "Age 70 and above",
                     "93" = "Age 70 and above",
                     "94" = "Age 70 and above",
                     "95" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "98" = "Age 70 and above",
                     "99" = "Age 70 and above")
#Change the unknown (7) and refused (9) values to NA
(BRFSS09$AGE <- unknownToNA(BRFSS09$AGE, unknown = c("7", "9")))
table(BRFSS09$AGE)

#    Age 18 to 24     Age 25 to 29     Age 30 to 34     Age 35 to 39     Age 40 to 44     Age 45 to 49     Age 50 to 54     Age 55 to 59 
#           13266            15117            22171            28046            32012            39905            46884            48094 
#    Age 60 to 64     Age 65 to 69 Age 70 and above 
#           47134            40314            95892 
BRFSS09$AGE <- as.factor(BRFSS09$AGE)
class(BRFSS09$AGE)


#SEX

#Observe data
table(BRFSS09$SEX)
#Recode sex data into categories corresponding to BRFSS codebook
BRFSS09$SEX <- recode(BRFSS09$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS09$SEX)
#Female   Male 
#268646 163961
BRFSS09$SEX <- as.factor(BRFSS09$SEX)
class(BRFSS09$SEX)


#BMI

#Observe data
table(BRFSS09$`_BMI4`)
#recode unknown numerical to NA
BRFSS09$`_BMI4` <- unknownToNA(BRFSS09$`_BMI4`, unknown = "9999")
#two implied decimal places, therefore pull comma back two places
BRFSS09 <- BRFSS09 %>%
  mutate(`_BMI4` = `_BMI4`/100)
#check that unknown data do not appear
BRFSS09 %>% top_n(10, `_BMI4`)

#Create another BMI variable using BMI categories for figures
BRFSS09$BMICAT <- ifelse(BRFSS09$`_BMI4` < 18.50, "Underweight",
                         ifelse(BRFSS09$`_BMI4` >= 18.50 & BRFSS09$`_BMI4` < 25.00, "Healthy weight",
                                ifelse(BRFSS09$`_BMI4` >= 25.00 & BRFSS09$`_BMI4` < 30.00, "Overweight",
                                       ifelse(BRFSS09$`_BMI4` >= 30.00, "Obese",
                                              NA))))

table(BRFSS09$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#        138682         116335         151187           6583 

#__________________________________________________________________________________________________________________

#DESIGN OBJECT CREATION

#Some final checks:
options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS09_dataset <- subset(BRFSS09,
                          !is.na(`_PSU`) &
                            !is.na(`_STSTR`) &
                            !is.na(`_FINALWT`) &
                            !is.na(HAVARTH2))

#Check that the sum of the weights is equal to the US population
sum(BRFSS09_dataset$`_FINALWT`)
#The sum of the weights is 224 051 170

#Check the number of people (unique PSU's)
length(unique(BRFSS09_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 136 011

#Check the number of unique strata
length(unique(BRFSS09_dataset[["_STSTR"]]))
#The number of unique strata is 1 104

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS09_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_FINALWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS09_dataset)
#Observe the design oject
BRFSS09_DO


#_______________________________________________________________________________________________

#----Analysis----

#1. Overall prevalence
B09_overall <- svymean(~factor(HAVARTH2),
                       BRFSS09_DO,
                       na.rm = TRUE)
B09_overall.c <- B09_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B09_overall_ci <- confint(B09_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B09 <- bind_cols(B09_overall.c, B09_overall_ci)
#remove HAVARTH2 = 0
B09 <- B09[-c(1), ] #B09 = final proportion, se and 95% ci

#Overall number of people
B09.no <- svytotal(~HAVARTH2, 
                   BRFSS09_DO, 
                   na.rm = TRUE, 
                   deff = TRUE)
B09.no.df <- as.data.frame(B09.no)
#ci
B09.no.ci <- confint(B09.no) %>%
  as.data.frame(.)
#join number and ci
B09.no <- bind_cols(B09.no.df, B09.no.ci)
#remove HAVARTH2=0
B09.no <- B09.no[-c(1), ] #B09.no = final number of people with arth, se, design effect and 95%ci


#2. Demographic-specific analysis

#A. Arthritis & Age
B09_Arth_age <- svyby(formula = ~HAVARTH2,
                      by = ~AGE,
                      design = BRFSS09_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B09_Arthtitis_age <- B09_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B09_arth_age_ci <- confint(B09_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0 (No)
B09_arth_age_ci <- B09_arth_age_ci[-c(1:11), ]
#join ci and proportions
B09.Age <- bind_cols(B09_Arthtitis_age, B09_arth_age_ci) #B09.Age = final proportion, se and 95% ci by age group


#Number of people by age
B09.no.age <- svyby(formula = ~HAVARTH2,
                    by = ~AGE,
                    design = BRFSS09_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B09.no.age.c <- B09.no.age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Number of People", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B09.no.age.ci <- confint(B09.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0 (No), which are the first 11 rows
B09.no.age.ci <- B09.no.age.ci[-c(1:11), ]
#join number and ci
B09.no.age <- bind_cols(B09.no.age.c, B09.no.age.ci)

#Age logistic regression
B09_age_glm <- svyglm(HAVARTH2~AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS09_DO)
summary(B09_age_glm)
exp(cbind(OR=coef(B09_age_glm), confint(B09_age_glm)))



#B. Arthritis & Sex
B09_Arth_sex <- svyby(formula = ~HAVARTH2,
                      by = ~SEX,
                      design = BRFSS09_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B09_Arthritis_sex <- B09_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B09_arth_sex_ci <- confint(B09_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0 (No)
B09_arth_sex_ci <- B09_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B09.Sex <- bind_cols(B09_Arthritis_sex, B09_arth_sex_ci) #B09.Sex = final proportion, se and 95% ci by sex

#Number of people by sex
B09.no.sex <- svyby(formula = ~HAVARTH2,
                    by = ~SEX,
                    design = BRFSS09_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B09.no.sex.c <- B09.no.sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Number of People", "SE")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B09.no.sex.ci <- confint(B09.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0 (No), which are the first 2 rows
B09.no.sex.ci <- B09.no.sex.ci[-c(1:2), ]
#join number and ci
B09.no.sex <- bind_cols(B09.no.sex.c, B09.no.sex.ci)


#Sex logistic regression
B09_sex_glm <- svyglm(HAVARTH2~relevel(SEX, ref = "Male") + AGE,
                      family = quasibinomial,
                      design = BRFSS09_DO)
summary(B09_sex_glm)
exp(cbind(OR=coef(B09_sex_glm), confint(B09_sex_glm)))


#C. Arthritis & BMI
B09_Arth_BMI <- svyby(formula = ~HAVARTH2,
                      by = ~BMICAT,
                      design = BRFSS09_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B09_Arthritis_BMI <- B09_Arth_BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Proportion", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B09_arth_BMI_ci <- confint(B09_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0
B09_arth_BMI_ci <- B09_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
B09.BMI <- bind_cols(B09_Arthritis_BMI, B09_arth_BMI_ci) #B09.BMI = final proportion, se and 95%ci by BMI


#Number of people by BMI
B09.no.BMI <- svyby(formula = ~HAVARTH2,
                    by = ~BMICAT,
                    design = BRFSS09_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B09.no.BMI.c <- B09.no.BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Number of People", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B09.no.BMI.ci <- confint(B09.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0 (No), which are the first 4 rows
B09.no.BMI.ci <- B09.no.BMI.ci[-c(1:4), ]
#join number and ci
B09.no.BMI <- bind_cols(B09.no.BMI.c, B09.no.BMI.ci)


#BMI logistic regression (using original continuous variable from survey)
B09.BMI.glm <- svyglm(HAVARTH2 ~ `_BMI4` + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS09_DO)
summary(B09.BMI.glm)
exp(cbind(OR=coef(B09.BMI.glm), confint(B09.BMI.glm)))


#   End of 2009 analysis

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(BRFSS09)
remove(BRFSS09_dataset)
remove(BRFSS09_DO)
remove(BRFSS09_raw)
remove(B09_Arth_age)
remove(B09_arth_age_ci)
remove(B09_Arth_BMI)
remove(B09_arth_BMI_ci)
remove(B09_Arth_sex)
remove(B09_arth_sex_ci)
remove(B09_Arthritis_BMI)
remove(B09_Arthritis_sex)
remove(B09_Arthtitis_age)
remove(B09_arth_age_ci)
remove(B09_overall.c)
remove(B09_overall_ci)
remove(B09.no.age.c)
remove(B09.no.age.ci)
remove(B09.no.BMI.c)
remove(B09.no.BMI.ci)
remove(B09.no.df)
remove(B09.no.ci)
remove(B09.no.sex.c)
remove(B09.no.sex.ci)
remove(B09_age_glm)
remove(B09_sex_glm)
remove(B09.BMI.glm)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

#Behavioural Risk Factor Surveillance System (BRFSS)
#2010


#----2010----


#----Download----

B10_url <- "http://www.cdc.gov/brfss/annual_data/2010/files/CDBRFS10XPT.zip"

tempB10 <- tempfile()
tempB10b <- tempfile()

download.file(B10_url, tempB10)
unzip(zipfile = tempB10, exdir = tempB10b)
BRFSS10_raw <- read_xpt(file.path(tempB10b, "CDBRFS10.XPT"))

unlink(c(tempB10, tempB10b))

#_______________________________________________________________________________________________

#----Cleaning----

#Observe the dataset
str(BRFSS10_raw) 
tail(BRFSS10_raw) 
glimpse(BRFSS10_raw) 
colnames(BRFSS10_raw)

BRFSS10 <- select(BRFSS10_raw,
                  "_PSU", "_FINALWT", "_STSTR", "HAVARTH2",
                  "AGE", "SEX", "_BMI4") 

#Observations
str(BRFSS10)
tail(BRFSS10)
glimpse(BRFSS10)
colnames(BRFSS10)


#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS10)) #Large proportion of arthritis data "missing", which makes sense due to being a module year
which(colSums(is.na(BRFSS10)) == nrow(BRFSS10)) #Named integer = 0, which means that at least one reponse is present for the variables selected.


#HAVARTH2

#Observe how many people answered yes, no, don't know, & refused
table(BRFSS10$HAVARTH2)
#    1     2     7     9 
#15538 19810   117    13  

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS10$HAVARTH2 <- recode(BRFSS10$HAVARTH2,
                           "1" = "1",
                           "2" = "0",
                           "7" = "7",
                           "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS10$HAVARTH2 <- unknownToNA(BRFSS10$HAVARTH2, unknown = c("7", "9")))
table(BRFSS10$HAVARTH2)
#    0     1 
#19810 15538
BRFSS10$HAVARTH2 <- as.factor(BRFSS10$HAVARTH2)
class(BRFSS10$HAVARTH2)


#AGE

#Observe data
table(BRFSS10$AGE)
#Need to make this continuous data analogous to later BRFSS 5-year categories in order to make meaningful comparisons between years
#Recode age data into categories corresponding to later BRFSS codebook categories
BRFSS10$AGE<- recode(BRFSS10$AGE,
                     "7" = "7",
                     "9" = "9",
                     "18" = "Age 18 to 24",
                     "19" = "Age 18 to 24",
                     "20" = "Age 18 to 24",
                     "21" = "Age 18 to 24",
                     "22" = "Age 18 to 24",
                     "23" = "Age 18 to 24",
                     "24" = "Age 18 to 24",
                     "25" = "Age 25 to 29",
                     "26" = "Age 25 to 29",
                     "27" = "Age 25 to 29",
                     "28" = "Age 25 to 29",
                     "29" = "Age 25 to 29",
                     "30" = "Age 30 to 34",
                     "31" = "Age 30 to 34",
                     "32" = "Age 30 to 34",
                     "33" = "Age 30 to 34",
                     "34" = "Age 30 to 34",
                     "35" = "Age 35 to 39",
                     "36" = "Age 35 to 39",
                     "37" = "Age 35 to 39",
                     "38" = "Age 35 to 39",
                     "39" = "Age 35 to 39",
                     "40" = "Age 40 to 44",
                     "41" = "Age 40 to 44",
                     "42" = "Age 40 to 44",
                     "43" = "Age 40 to 44",
                     "44" = "Age 40 to 44",
                     "45" = "Age 45 to 49",
                     "46" = "Age 45 to 49",
                     "47" = "Age 45 to 49",
                     "48" = "Age 45 to 49",
                     "49" = "Age 45 to 49",
                     "50" = "Age 50 to 54",
                     "51" = "Age 50 to 54",
                     "52" = "Age 50 to 54",
                     "53" = "Age 50 to 54",
                     "54" = "Age 50 to 54",
                     "55" = "Age 55 to 59",
                     "56" = "Age 55 to 59",
                     "57" = "Age 55 to 59",
                     "58" = "Age 55 to 59",
                     "59" = "Age 55 to 59",
                     "60" = "Age 60 to 64",
                     "61" = "Age 60 to 64",
                     "62" = "Age 60 to 64",
                     "63" = "Age 60 to 64",
                     "64" = "Age 60 to 64",
                     "65" = "Age 65 to 69",
                     "66" = "Age 65 to 69",
                     "67" = "Age 65 to 69",
                     "68" = "Age 65 to 69", 
                     "69" = "Age 65 to 69",
                     "70" = "Age 70 and above",
                     "71" = "Age 70 and above",
                     "72" = "Age 70 and above",
                     "73" = "Age 70 and above",
                     "74" = "Age 70 and above",
                     "75" = "Age 70 and above",
                     "76" = "Age 70 and above",
                     "77" = "Age 70 and above",
                     "78" = "Age 70 and above",
                     "79" = "Age 70 and above",
                     "80" = "Age 70 and above",
                     "81" = "Age 70 and above",
                     "82" = "Age 70 and above",
                     "83" = "Age 70 and above",
                     "84" = "Age 70 and above",
                     "85" = "Age 70 and above",
                     "86" = "Age 70 and above",
                     "87" = "Age 70 and above",
                     "88" = "Age 70 and above",
                     "89" = "Age 70 and above",
                     "90" = "Age 70 and above",
                     "91" = "Age 70 and above",
                     "92" = "Age 70 and above",
                     "93" = "Age 70 and above",
                     "94" = "Age 70 and above",
                     "95" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "96" = "Age 70 and above",
                     "98" = "Age 70 and above",
                     "99" = "Age 70 and above")
#Change the unknown (7) and refused (9) values to NA
(BRFSS10$AGE <- unknownToNA(BRFSS10$AGE, unknown = c("7", "9")))
table(BRFSS10$AGE)

#    Age 18 to 24     Age 25 to 29     Age 30 to 34     Age 35 to 39     Age 40 to 44     Age 45 to 49     Age 50 to 54 
#           12621            13638            21273            27001            31642            39192            46885 
#    Age 55 to 59     Age 60 to 64     Age 65 to 69 Age 70 and above 
#           50794            51713            45620           106402
BRFSS10$AGE <- as.factor(BRFSS10$AGE)
class(BRFSS10$AGE)


#SEX

#Observe data
table(BRFSS10$SEX)
#Recode sex data into categories corresponding to BRFSS codebook
BRFSS10$SEX <- recode(BRFSS10$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS10$SEX)
#Female   Male 
#280961 170114
BRFSS10$SEX <- as.factor(BRFSS10$SEX)
class(BRFSS10$SEX)


#BMI

#Observe data
table(BRFSS10$`_BMI4`)
#two implied decimal places, therefore pull comma back two places
BRFSS10 <- BRFSS10 %>%
  mutate(`_BMI4` = `_BMI4`/100)
BRFSS10$`_BMI4` <- unknownToNA(BRFSS10$`_BMI4`, unknown = "99.99") #as per codebook
BRFSS10 %>% top_n(10, `_BMI4`) #check that 99.99 does not appear

#Create another BMI variable using BMI categories for figures
BRFSS10$BMICAT <- ifelse(BRFSS10$`_BMI4` < 18.5, "Underweight",
                  ifelse(BRFSS10$`_BMI4` >= 18.5 & BRFSS10$`_BMI4` < 25.0, "Healthy weight",
                  ifelse(BRFSS10$`_BMI4` >= 25.0 & BRFSS10$`_BMI4` < 30.0, "Overweight",
                  ifelse(BRFSS10$`_BMI4` >= 30.0, "Obese",
                  NA))))

table(BRFSS10$BMICAT)
#Healthy weight          Obese     Overweight    Underweight 
#        143373         121959         157348           6982


#__________________________________________________________________________________________________________________
#DESIGN OBJECT CREATION

#Some final checks:
options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS10_dataset <- subset(BRFSS10,
                          !is.na(`_PSU`) &
                            !is.na(`_STSTR`) &
                            !is.na(`_FINALWT`) &
                            !is.na(HAVARTH2))

#Check that the sum of the weights is equal to the US population
sum(BRFSS10_dataset$`_FINALWT`)
#The sum of the weights is 15 548 620 {MODULE YEAR SO REDUCED POPULATION SIZE TO BE EXPECTED}

#Check the number of people (unique PSU's)
length(unique(BRFSS10_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 30 283

#Check the number of unique strata
length(unique(BRFSS10_dataset[["_STSTR"]]))
#The number of unique strata is 94

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS10_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_FINALWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS10_dataset)
#Observe the design oject
BRFSS10_DO

#_______________________________________________________________________________________________

#----Analysis----

#1. Overall
# Prevalence
B10_overall <- svymean(~factor(HAVARTH2),
                       BRFSS10_DO,
                       na.rm = TRUE)
B10_overall.c <- B10_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B10_overall_ci <- confint(B10_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B10 <- bind_cols(B10_overall.c, B10_overall_ci)
#remove havarth = 0
B10 <- B10[-c(1), ] #B10 = final proportion, se and 95% ci


#2. Demographic analysis

#A. Arthritis & Age
B10_Arth_age <- svyby(formula = ~HAVARTH2,
                      by = ~AGE,
                      design = BRFSS10_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B10_Arthtitis_age <- B10_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B10_arth_age_ci <- confint(B10_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0 (No)
B10_arth_age_ci <- B10_arth_age_ci[-c(1:11), ]
#join ci and proportions
B10.Age <- bind_cols(B10_Arthtitis_age, B10_arth_age_ci) #B10.Age = final proportion, se and 95% ci by age group


#Age logistic regression
B10_age_glm <- svyglm(HAVARTH2~AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS10_DO)
summary(B10_age_glm)
exp(cbind(OR=coef(B10_age_glm), confint(B10_age_glm)))



#B. Arthritis & Sex
B10_Arth_sex <- svyby(formula = ~HAVARTH2,
                      by = ~SEX,
                      design = BRFSS10_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B10_Arthritis_sex <- B10_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B10_arth_sex_ci <- confint(B10_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0 (No)
B10_arth_sex_ci <- B10_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B10.Sex <- bind_cols(B10_Arthritis_sex, B10_arth_sex_ci) #B10.Sex = final proportion, se and 95% ci by sex

#Sex logistic regression
B10_sex_glm <- svyglm(HAVARTH2~relevel(SEX, ref = "Male") + AGE,
                      family = quasibinomial,
                      design = BRFSS10_DO)
summary(B10_sex_glm)
exp(cbind(OR=coef(B10_sex_glm), confint(B10_sex_glm)))


#C. Arthritis & BMI
B10_Arth_BMI <- svyby(formula = ~HAVARTH2,
                      by = ~BMICAT,
                      design = BRFSS10_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B10_Arthritis_BMI <- B10_Arth_BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Proportion", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B10_arth_BMI_ci <- confint(B10_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH2 = 0
B10_arth_BMI_ci <- B10_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
B10.BMI <- bind_cols(B10_Arthritis_BMI, B10_arth_BMI_ci) #B10.BMI = final proportion, se and 95%ci by BMI

#BMI logistic regression (using original continuous variable from survey)
B10.BMI.glm <- svyglm(HAVARTH2 ~ `_BMI4` + AGE + SEX,
                      family = quasibinomial,
                      design = BRFSS10_DO)
summary(B10.BMI.glm)
exp(cbind(OR=coef(B10.BMI.glm), confint(B10.BMI.glm)))


#   End of 2010 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(BRFSS10)
remove(BRFSS10_dataset)
remove(BRFSS10_DO)
remove(BRFSS10_raw)
remove(B10_Arth_age)
remove(B10_arth_age_ci)
remove(B10_Arth_BMI)
remove(B10_arth_BMI_ci)
remove(B10_Arth_sex)
remove(B10_arth_sex_ci)
remove(B10_Arthritis_BMI)
remove(B10_Arthritis_sex)
remove(B10_Arthtitis_age)
remove(B10_arth_age_ci)
remove(B10_overall.c)
remove(B10_overall_ci)
remove(B10.no.age.c)
remove(B10.no.age.ci)
remove(B10.no.BMI.c)
remove(B10.no.BMI.ci)
remove(B10.no.df)
remove(B10.no.ci)
remove(B10.no.sex.c)
remove(B10.no.sex.ci)
remove(B10_age_glm)
remove(B10_sex_glm)
remove(B10.BMI.glm)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

#Behavioural Risk Factor Surveillance System (BRFSS)
#2011


#----2011----


#----Download----

B11_url <- "http://www.cdc.gov/brfss/annual_data/2011/files/LLCP2011XPT.ZIP"

tempB11 <- tempfile()
tempB11b <- tempfile()

download.file(B11_url, tempB11, mode = "wb")
unzip(zipfile = tempB11, exdir = tempB11b)
BRFSS11_raw <- read_xpt(file.path(tempB11b, "LLCP2011.XPT"))

unlink(c(tempB11, tempB11b))


#_______________________________________________________________________________________________

#----Cleaning----

#Observe the dataset
str(BRFSS11_raw) 
tail(BRFSS11_raw) 
glimpse(BRFSS11_raw) 
colnames(BRFSS11_raw)

BRFSS11 <- dplyr::select(BRFSS11_raw,
                         "_PSU", "_LLCPWT", "_STSTR", "HAVARTH3",
                         "_AGEG5YR", "SEX", "_BMI5", "_BMI5CAT")

#Observations
str(BRFSS11)
tail(BRFSS11)
glimpse(BRFSS11)
colnames(BRFSS11)


#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS11))
which(colSums(is.na(BRFSS11)) == nrow(BRFSS11)) #Named integer = 0, which means that at least one reponse is present for the variables selected.



#HAVARTH

#Observe how many people answered yes, no, don't know, & refused
table(BRFSS11$HAVARTH3)
#     1      2      7      9 
#168483 333602   3089    167

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS11$HAVARTH3 <- recode(BRFSS11$HAVARTH3,
                           "1" = "1",
                           "2" = "0",
                           "7" = "7",
                           "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS11$HAVARTH3 <- unknownToNA(BRFSS11$HAVARTH3, unknown = c("7", "9")))
table(BRFSS11$HAVARTH3)
#     0      1 
#333602 168483
BRFSS11$HAVARTH3 <- as.factor(BRFSS11$HAVARTH3)
class(BRFSS11$HAVARTH3)


#AGE

#Observe data
table(BRFSS11$`_AGEG5YR`)

#Recode age data into categories corresponding to BRFSS codebook
BRFSS11$`_AGEG5YR`<- recode(BRFSS11$`_AGEG5YR`,
                            "1" = "Age 18 to 24",
                            "2" = "Age 25 to 29",
                            "3" = "Age 30 to 34",
                            "4" = "Age 35 to 39",
                            "5" = "Age 40 to 44",
                            "6" = "Age 45 to 49",
                            "7" = "Age 50 to 54",
                            "8" = "Age 55 to 59",
                            "9" = "Age 60 to 64",
                            "10" = "Age 65 to 69",
                            "11" = "Age 70 and above",
                            "12" = "Age 70 and above",
                            "13" = "Age 70 and above",
                            "14" = "14")
#Change the unknown (14) values to NA
(BRFSS11$`_AGEG5YR` <- unknownToNA(BRFSS11$`_AGEG5YR`, unknown = "14"))
table(BRFSS11$`_AGEG5YR`)
#    Age 18 to 24     Age 25 to 29     Age 30 to 34     Age 35 to 39     Age 40 to 44     Age 45 to 49     Age 50 to 54     Age 55 to 59 
#           23067            21977            27601            29930            35096            40877            50652            54986 
#    Age 60 to 64     Age 65 to 69 Age 70 and above 
#           56787            47782           112747 
BRFSS11$`_AGEG5YR` <- as.factor(BRFSS11$`_AGEG5YR`)


#SEX

#Observe data
table(BRFSS11$SEX)
#Recode sex data into categories corresponding to BRFSS codebook
BRFSS11$SEX <- recode(BRFSS11$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS11$SEX)
#Female   Male 
#307655 198812 
BRFSS11$SEX <- as.factor(BRFSS11$SEX)
class(BRFSS11$SEX)


#BMI

#observe data
table(BRFSS11$`_BMI5`)
#recode unknown numerical to NA
BRFSS11$`_BMI5` <- unknownToNA(BRFSS11$`_BMI5`, unknown = "9999")
#implied 2 decimal places, therefore divide all values by 100 to get the BMI value
BRFSS11 <- BRFSS11 %>%
  mutate(`_BMI5` = `_BMI5`/100)
#check that unknown data do not appear
BRFSS11 %>% top_n(10, `_BMI5`)


#Recode BMI categories
BRFSS11$`_BMI5CAT` <- recode(BRFSS11$`_BMI5CAT`,
                             "1" = "Underweight",
                             "2" = "Healthy weight",
                             "3" = "Overweight",
                             "4" = "Obese")
BRFSS11$`_BMI5CAT` <- as.factor(BRFSS11$`_BMI5CAT`)
table(BRFSS11$`_BMI5CAT`)
#Healthy weight          Obese     Overweight    Underweight 
#        163604         133190         173756           8289


#__________________________________________________________________________________________________________________

#DESIGN OBJECT CREATION

#Some final checks:
options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS11_dataset <- subset(BRFSS11,
                          !is.na(`_PSU`) &
                            !is.na(`_STSTR`) &
                            !is.na(`_LLCPWT`) &
                            !is.na(HAVARTH3))

#Check that the sum of the weights is equal to the US population
sum(BRFSS11_dataset$`_LLCPWT`)
#The sum of the weights is 233 590 626

#Check the number of people (unique PSU's)
length(unique(BRFSS11_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 25 401

#Check the number of unique strata
length(unique(BRFSS11_dataset[["_STSTR"]]))
#The number of unique strata is 1 220

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS11_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_LLCPWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS11_dataset)
#Observe the design oject
BRFSS11_DO


#_______________________________________________________________________________________________

#----Analysis----

#1. Overall prevalence
B11_overall <- svymean(~factor(HAVARTH3),
                       BRFSS11_DO,
                       na.rm = TRUE)
B11_overall.c <- B11_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B11_overall_ci <- confint(B11_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B11 <- bind_cols(B11_overall.c, B11_overall_ci)
#remove HAVARTH3 = 0
B11 <- B11[-c(1), ] #B11 = final proportion, se and 95% ci

#Overall number of people
B11.no <- svytotal(~HAVARTH3, 
                   BRFSS11_DO, 
                   na.rm = TRUE, 
                   deff = TRUE)
B11.no.df <- as.data.frame(B11.no)
#ci
B11.no.ci <- confint(B11.no) %>%
  as.data.frame(.)
#join number and ci
B11.no <- bind_cols(B11.no.df, B11.no.ci)
#remove HAVARTH3=0
B11.no <- B11.no[-c(1), ] #B11.no = final number of people with arth, se, design effect and 95%ci


#2. Demographic-specific analysis

#A. Arthritis & Age
B11_Arth_age <- svyby(formula = ~HAVARTH3,
                      by = ~`_AGEG5YR`,
                      design = BRFSS11_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B11_Arthtitis_age <- B11_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B11_arth_age_ci <- confint(B11_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No)
B11_arth_age_ci <- B11_arth_age_ci[-c(1:11), ]
#join ci and proportions
B11.Age <- bind_cols(B11_Arthtitis_age, B11_arth_age_ci) #B11.Age = final proportion, se and 95% ci by age group

#Number of people by age
B11.no.age <- svyby(formula = ~HAVARTH3,
                    by = ~`_AGEG5YR`,
                    design = BRFSS11_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B11.no.age.c <- B11.no.age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Number of People", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B11.no.age.ci <- confint(B11.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No), which are the first 11 rows
B11.no.age.ci <- B11.no.age.ci[-c(1:11), ]
#join number and ci
B11.no.age <- bind_cols(B11.no.age.c, B11.no.age.ci)


#Age logistic regression
B11_age_glm <- svyglm(HAVARTH3~`_AGEG5YR` + SEX,
                      family = quasibinomial,
                      design = BRFSS11_DO)
summary(B11_age_glm)
exp(cbind(OR=coef(B11_age_glm), confint(B11_age_glm)))



#B. Arthritis & Sex
B11_Arth_sex <- svyby(formula = ~HAVARTH3,
                      by = ~SEX,
                      design = BRFSS11_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B11_Arthritis_sex <- B11_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B11_arth_sex_ci <- confint(B11_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No)
B11_arth_sex_ci <- B11_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B11.Sex <- bind_cols(B11_Arthritis_sex, B11_arth_sex_ci) #B11.Sex = final proportion, se and 95% ci by sex


#Number of people by sex
B11.no.sex <- svyby(formula = ~HAVARTH3,
                    by = ~SEX,
                    design = BRFSS11_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B11.no.sex.c <- B11.no.sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Number of People", "SE")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B11.no.sex.ci <- confint(B11.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No), which are the first 2 rows
B11.no.sex.ci <- B11.no.sex.ci[-c(1:2), ]
#join number and ci
B11.no.sex <- bind_cols(B11.no.sex.c, B11.no.sex.ci)

#Sex logistic regression
B11_sex_glm <- svyglm(HAVARTH3~relevel(SEX, ref = "Male") + `_AGEG5YR`,
                      family = quasibinomial,
                      design = BRFSS11_DO)
summary(B11_sex_glm)
exp(cbind(OR=coef(B11_sex_glm), confint(B11_sex_glm)))


#C. Arthritis & BMI
B11_Arth_BMI <- svyby(formula = ~HAVARTH3,
                      by = ~`_BMI5CAT`,
                      design = BRFSS11_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B11_Arthritis_BMI <- B11_Arth_BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Proportion", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B11_arth_BMI_ci <- confint(B11_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0
B11_arth_BMI_ci <- B11_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
B11.BMI <- bind_cols(B11_Arthritis_BMI, B11_arth_BMI_ci) #B11.BMI = final proportion, se and 95%ci by BMI

#Number of people by BMI
B11.no.BMI <- svyby(formula = ~HAVARTH3,
                    by = ~`_BMI5CAT`,
                    design = BRFSS11_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B11.no.BMI.c <- B11.no.BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Number of People", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B11.no.BMI.ci <- confint(B11.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No), which are the first 4 rows
B11.no.BMI.ci <- B11.no.BMI.ci[-c(1:4), ]
#join number and ci
B11.no.BMI <- bind_cols(B11.no.BMI.c, B11.no.BMI.ci)

#BMI logistic regression (using original continuous variable from survey)
B11.BMI.glm <- svyglm(HAVARTH3 ~ `_BMI5` + `_AGEG5YR` + SEX,
                      family = quasibinomial,
                      design = BRFSS11_DO)
summary(B11.BMI.glm)
exp(cbind(OR=coef(B11.BMI.glm), confint(B11.BMI.glm)))


#   End of 2011 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(BRFSS11)
remove(BRFSS11_dataset)
remove(BRFSS11_DO)
remove(BRFSS11_raw)
remove(B11_Arth_age)
remove(B11_arth_age_ci)
remove(B11_Arth_BMI)
remove(B11_arth_BMI_ci)
remove(B11_Arth_sex)
remove(B11_arth_sex_ci)
remove(B11_Arthritis_BMI)
remove(B11_Arthritis_sex)
remove(B11_Arthtitis_age)
remove(B11_arth_age_ci)
remove(B11_overall.c)
remove(B11_overall_ci)
remove(B11.no.age.c)
remove(B11.no.age.ci)
remove(B11.no.BMI.c)
remove(B11.no.BMI.ci)
remove(B11.no.df)
remove(B11.no.ci)
remove(B11.no.sex.c)
remove(B11.no.sex.ci)
remove(B11_age_glm)
remove(B11_sex_glm)
remove(B11.BMI.glm)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

#Behavioural Risk Factor Surveillance System (BRFSS)
#2012


#----2012----


#----Download----

B12_url <- "http://www.cdc.gov/brfss/annual_data/2012/files/LLCP2012XPT.ZIP"

tempB12 <- tempfile()
tempB12b <- tempfile()

download.file(B12_url, tempB12, mode = "wb")
unzip(zipfile = tempB12, exdir = tempB12b)
BRFSS12_raw <- read_xpt(file.path(tempB12b, "LLCP2012.XPT"))

unlink(c(tempB12, tempB12b))

#_______________________________________________________________________________________________

#----Cleaning----

#Observe the dataset
str(BRFSS12_raw) 
tail(BRFSS12_raw) 
glimpse(BRFSS12_raw) 
colnames(BRFSS12_raw)

BRFSS12 <- dplyr::select(BRFSS12_raw,
                         "_PSU", "_LLCPWT", "_STSTR", "HAVARTH3",
                         "_AGEG5YR", "SEX", "_BMI5", "_BMI5CAT")

#Observations
str(BRFSS12)
tail(BRFSS12)
glimpse(BRFSS12)
colnames(BRFSS12)


#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS12))
which(colSums(is.na(BRFSS12)) == nrow(BRFSS12)) #Named integer = 0, which means that at least one reponse is present for the variables selected.



#HAVARTH

#Observe how many people answered yes, no, don't know, & refused
table(BRFSS12$HAVARTH3)
#     1      2      7      9 
#162758 310423   2310    193 

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS12$HAVARTH3 <- recode(BRFSS12$HAVARTH3,
                           "1" = "1",
                           "2" = "0",
                           "7" = "7",
                           "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS12$HAVARTH3 <- unknownToNA(BRFSS12$HAVARTH3, unknown = c("7", "9")))
table(BRFSS12$HAVARTH3)
#     0      1 
#310423 162758 
BRFSS12$HAVARTH3 <- as.factor(BRFSS12$HAVARTH3)
class(BRFSS12$HAVARTH3)


#AGE

#Observe data
table(BRFSS12$`_AGEG5YR`)

#Recode age data into categories corresponding to BRFSS codebook
BRFSS12$`_AGEG5YR`<- recode(BRFSS12$`_AGEG5YR`,
                            "1" = "Age 18 to 24",
                            "2" = "Age 25 to 29",
                            "3" = "Age 30 to 34",
                            "4" = "Age 35 to 39",
                            "5" = "Age 40 to 44",
                            "6" = "Age 45 to 49",
                            "7" = "Age 50 to 54",
                            "8" = "Age 55 to 59",
                            "9" = "Age 60 to 64",
                            "10" = "Age 65 to 69",
                            "11" = "Age 70 and above",
                            "12" = "Age 70 and above",
                            "13" = "Age 70 and above",
                            "14" = "14")
#Change the unknown (14) values to NA
(BRFSS12$`_AGEG5YR` <- unknownToNA(BRFSS12$`_AGEG5YR`, unknown = "14"))
table(BRFSS12$`_AGEG5YR`)
#    Age 18 to 24     Age 25 to 29     Age 30 to 34     Age 35 to 39     Age 40 to 44     Age 45 to 49     Age 50 to 54     Age 55 to 59 
#           24894            21507            26264            27608            32100            36583            46817            50528 
#    Age 60 to 64     Age 65 to 69 Age 70 and above 
#           52265            47089           105452 
BRFSS12$`_AGEG5YR` <- as.factor(BRFSS12$`_AGEG5YR`)


#SEX

#Observe data
table(BRFSS12$SEX)
#Recode sex data into categories corresponding to BRFSS codebook
BRFSS12$SEX <- recode(BRFSS12$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS12$SEX)
#Female   Male 
#283950 191737 
BRFSS12$SEX <- as.factor(BRFSS12$SEX)
class(BRFSS12$SEX)


#BMI

#observe data
table(BRFSS12$`_BMI5`)
#recode unknown numerical to NA
BRFSS12$`_BMI5` <- unknownToNA(BRFSS12$`_BMI5`, unknown = "9999")
#implied 2 decimal places, therefore divide all values by 100 to get the BMI value
BRFSS12 <- BRFSS12 %>%
  mutate(`_BMI5` = `_BMI5`/100)
#check that unknown data do not appear
BRFSS12 %>% top_n(10, `_BMI5`)


#Recode BMI categories
BRFSS12$`_BMI5CAT` <- recode(BRFSS12$`_BMI5CAT`,
                             "1" = "Underweight",
                             "2" = "Healthy weight",
                             "3" = "Overweight",
                             "4" = "Obese")
BRFSS12$`_BMI5CAT` <- as.factor(BRFSS12$`_BMI5CAT`)
table(BRFSS12$`_BMI5CAT`)
#Healthy weight          Obese     Overweight    Underweight 
#        151985         127656         162768           7803


#__________________________________________________________________________________________________________________

#DESIGN OBJECT CREATION

#Some final checks:
options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS12_dataset <- subset(BRFSS12,
                          !is.na(`_PSU`) &
                            !is.na(`_STSTR`) &
                            !is.na(`_LLCPWT`) &
                            !is.na(HAVARTH3))

#Check that the sum of the weights is equal to the US population
sum(BRFSS12_dataset$`_LLCPWT`)
#The sum of the weights is 241 899 298

#Check the number of people (unique PSU's)
length(unique(BRFSS12_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 21 719

#Check the number of unique strata
length(unique(BRFSS12_dataset[["_STSTR"]]))
#The number of unique strata is 1 102

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS12_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_LLCPWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS12_dataset)
#Observe the design oject
BRFSS12_DO


#_______________________________________________________________________________________________

#----Analysis----

#1. Overall prevalence
B12_overall <- svymean(~factor(HAVARTH3),
                       BRFSS12_DO,
                       na.rm = TRUE)
B12_overall.c <- B12_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B12_overall_ci <- confint(B12_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B12 <- bind_cols(B12_overall.c, B12_overall_ci)
#remove HAVARTH3 = 0
B12 <- B12[-c(1), ] #B12 = final proportion, se and 95% ci

#Overall number of people
B12.no <- svytotal(~HAVARTH3, 
                   BRFSS12_DO, 
                   na.rm = TRUE, 
                   deff = TRUE)
B12.no.df <- as.data.frame(B12.no)
#ci
B12.no.ci <- confint(B12.no) %>%
  as.data.frame(.)
#join number and ci
B12.no <- bind_cols(B12.no.df, B12.no.ci)
#remove HAVARTH3=0
B12.no <- B12.no[-c(1), ] #B12.no = final number of people with arth, se, design effect and 95%ci


#2. Demographic-specific analysis

#A. Arthritis & Age
B12_Arth_age <- svyby(formula = ~HAVARTH3,
                      by = ~`_AGEG5YR`,
                      design = BRFSS12_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B12_Arthtitis_age <- B12_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B12_arth_age_ci <- confint(B12_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No)
B12_arth_age_ci <- B12_arth_age_ci[-c(1:11), ]
#join ci and proportions
B12.Age <- bind_cols(B12_Arthtitis_age, B12_arth_age_ci) #B12.Age = final proportion, se and 95% ci by age group


#Number of people by age
B12.no.age <- svyby(formula = ~HAVARTH3,
                    by = ~`_AGEG5YR`,
                    design = BRFSS12_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B12.no.age.c <- B12.no.age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Number of People", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B12.no.age.ci <- confint(B12.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No), which are the first 11 rows
B12.no.age.ci <- B12.no.age.ci[-c(1:11), ]
#join number and ci
B12.no.age <- bind_cols(B12.no.age.c, B12.no.age.ci)

#Age logistic regression
B12_age_glm <- svyglm(HAVARTH3~`_AGEG5YR` + SEX,
                      family = quasibinomial,
                      design = BRFSS12_DO)
summary(B12_age_glm)
exp(cbind(OR=coef(B12_age_glm), confint(B12_age_glm)))



#B. Arthritis & Sex
B12_Arth_sex <- svyby(formula = ~HAVARTH3,
                      by = ~SEX,
                      design = BRFSS12_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B12_Arthritis_sex <- B12_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B12_arth_sex_ci <- confint(B12_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No)
B12_arth_sex_ci <- B12_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B12.Sex <- bind_cols(B12_Arthritis_sex, B12_arth_sex_ci) #B12.Sex = final proportion, se and 95% ci by sex

#Number of people by sex
B12.no.sex <- svyby(formula = ~HAVARTH3,
                    by = ~SEX,
                    design = BRFSS12_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B12.no.sex.c <- B12.no.sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Number of People", "SE")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B12.no.sex.ci <- confint(B12.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No), which are the first 2 rows
B12.no.sex.ci <- B12.no.sex.ci[-c(1:2), ]
#join number and ci
B12.no.sex <- bind_cols(B12.no.sex.c, B12.no.sex.ci)

#Sex logistic regression
B12_sex_glm <- svyglm(HAVARTH3~relevel(SEX, ref = "Male") + `_AGEG5YR`,
                      family = quasibinomial,
                      design = BRFSS12_DO)
summary(B12_sex_glm)
exp(cbind(OR=coef(B12_sex_glm), confint(B12_sex_glm)))


#C. Arthritis & BMI
B12_Arth_BMI <- svyby(formula = ~HAVARTH3,
                      by = ~`_BMI5CAT`,
                      design = BRFSS12_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B12_Arthritis_BMI <- B12_Arth_BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Proportion", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B12_arth_BMI_ci <- confint(B12_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0
B12_arth_BMI_ci <- B12_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
B12.BMI <- bind_cols(B12_Arthritis_BMI, B12_arth_BMI_ci) #B12.BMI = final proportion, se and 95%ci by BMI

#Number of people by BMI
B12.no.BMI <- svyby(formula = ~HAVARTH3,
                    by = ~`_BMI5CAT`,
                    design = BRFSS12_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B12.no.BMI.c <- B12.no.BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Number of People", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B12.no.BMI.ci <- confint(B12.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No), which are the first 4 rows
B12.no.BMI.ci <- B12.no.BMI.ci[-c(1:4), ]
#join number and ci
B12.no.BMI <- bind_cols(B12.no.BMI.c, B12.no.BMI.ci)


#BMI logistic regression (using original continuous variable from survey)
B12.BMI.glm <- svyglm(HAVARTH3 ~ `_BMI5` + `_AGEG5YR` + SEX,
                      family = quasibinomial,
                      design = BRFSS12_DO)
summary(B12.BMI.glm)
exp(cbind(OR=coef(B12.BMI.glm), confint(B12.BMI.glm)))


#   End of 2012 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(BRFSS12)
remove(BRFSS12_dataset)
remove(BRFSS12_DO)
remove(BRFSS12_raw)
remove(B12_Arth_age)
remove(B12_arth_age_ci)
remove(B12_Arth_BMI)
remove(B12_arth_BMI_ci)
remove(B12_Arth_sex)
remove(B12_arth_sex_ci)
remove(B12_Arthritis_BMI)
remove(B12_Arthritis_sex)
remove(B12_Arthtitis_age)
remove(B12_arth_age_ci)
remove(B12_overall.c)
remove(B12_overall_ci)
remove(B12.no.age.c)
remove(B12.no.age.ci)
remove(B12.no.BMI.c)
remove(B12.no.BMI.ci)
remove(B12.no.df)
remove(B12.no.ci)
remove(B12.no.sex.c)
remove(B12.no.sex.ci)
remove(B12_age_glm)
remove(B12_sex_glm)
remove(B12.BMI.glm)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

#Behavioural Risk Factor Surveillance System (BRFSS)
#2013


#----2013----


#----Download----

B13_url <- "http://www.cdc.gov/brfss/annual_data/2013/files/LLCP2013XPT.ZIP"

tempB13 <- tempfile()
tempB13b <- tempfile()

download.file(B13_url, tempB13, mode = "wb")
unzip(zipfile = tempB13, exdir = tempB13b)
BRFSS13_raw <- read_xpt(file.path(tempB13b, "LLCP2013.XPT"))

unlink(c(tempB13, tempB13b))

#_______________________________________________________________________________________________

#----Cleaning----

#Observe the dataset
str(BRFSS13_raw) 
tail(BRFSS13_raw) 
glimpse(BRFSS13_raw) 
colnames(BRFSS13_raw)

BRFSS13 <- dplyr::select(BRFSS13_raw,
                         "_PSU", "_LLCPWT", "_STSTR", "HAVARTH3",
                         "_AGEG5YR", "SEX", "_BMI5", "_BMI5CAT")

#Observations
str(BRFSS13)
tail(BRFSS13)
glimpse(BRFSS13)
colnames(BRFSS13)


#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS13))
which(colSums(is.na(BRFSS13)) == nrow(BRFSS13)) #Named integer = 0, which means that at least one reponse is present for the variables selected.



#HAVARTH

#Observe how many people answered yes, no, don't know, & refused
table(BRFSS13$HAVARTH3)
#     1      2      7      9 
#165192 323602   2707    272 

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS13$HAVARTH3 <- recode(BRFSS13$HAVARTH3,
                           "1" = "1",
                           "2" = "0",
                           "7" = "7",
                           "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS13$HAVARTH3 <- unknownToNA(BRFSS13$HAVARTH3, unknown = c("7", "9")))
table(BRFSS13$HAVARTH3)
#     0      1 
#323602 165192 
BRFSS13$HAVARTH3 <- as.factor(BRFSS13$HAVARTH3)
class(BRFSS13$HAVARTH3)


#AGE

#Observe data
table(BRFSS13$`_AGEG5YR`)

#Recode age data into categories corresponding to BRFSS codebook
BRFSS13$`_AGEG5YR`<- recode(BRFSS13$`_AGEG5YR`,
                            "1" = "Age 18 to 24",
                            "2" = "Age 25 to 29",
                            "3" = "Age 30 to 34",
                            "4" = "Age 35 to 39",
                            "5" = "Age 40 to 44",
                            "6" = "Age 45 to 49",
                            "7" = "Age 50 to 54",
                            "8" = "Age 55 to 59",
                            "9" = "Age 60 to 64",
                            "10" = "Age 65 to 69",
                            "11" = "Age 70 and above",
                            "12" = "Age 70 and above",
                            "13" = "Age 70 and above",
                            "14" = "14")
#Change the unknown (14) values to NA
(BRFSS13$`_AGEG5YR` <- unknownToNA(BRFSS13$`_AGEG5YR`, unknown = "14"))
table(BRFSS13$`_AGEG5YR`)
#    Age 18 to 24     Age 25 to 29     Age 30 to 34     Age 35 to 39     Age 40 to 44     Age 45 to 49     Age 50 to 54     Age 55 to 59 
#           27186            22904            27210            28095            31474            36185            47164            52496 
#    Age 60 to 64     Age 65 to 69 Age 70 and above 
#           53628            50024           110678
BRFSS13$`_AGEG5YR` <- as.factor(BRFSS13$`_AGEG5YR`)


#SEX

#Observe data
table(BRFSS13$SEX)
#Recode sex data into categories corresponding to BRFSS codebook
BRFSS13$SEX <- recode(BRFSS13$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS13$SEX)
#Female   Male 
#290498 201275 
BRFSS13$SEX <- as.factor(BRFSS13$SEX)
class(BRFSS13$SEX)


#BMI

#observe data
table(BRFSS13$`_BMI5`)
#recode unknown numerical to NA
BRFSS13$`_BMI5` <- unknownToNA(BRFSS13$`_BMI5`, unknown = "9999")
#implied 2 decimal places, therefore divide all values by 100 to get the BMI value
BRFSS13 <- BRFSS13 %>%
  mutate(`_BMI5` = `_BMI5`/100)
#check that unknown data do not appear
BRFSS13 %>% top_n(10, `_BMI5`)


#Recode BMI categories
BRFSS13$`_BMI5CAT` <- recode(BRFSS13$`_BMI5CAT`,
                             "1" = "Underweight",
                             "2" = "Healthy weight",
                             "3" = "Overweight",
                             "4" = "Obese")
BRFSS13$`_BMI5CAT` <- as.factor(BRFSS13$`_BMI5CAT`)
table(BRFSS13$`_BMI5CAT`)
#Healthy weight          Obese     Overweight    Underweight 
#        154993         134677         167118           8264

#__________________________________________________________________________________________________________________

#DESIGN OBJECT CREATION

#Some final checks:
options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS13_dataset <- subset(BRFSS13,
                          !is.na(`_PSU`) &
                          !is.na(`_STSTR`) &
                          !is.na(`_LLCPWT`) &
                          !is.na(HAVARTH3))

#Check that the sum of the weights is equal to the US population
sum(BRFSS13_dataset$`_LLCPWT`)
#The sum of the weights is 244 712 695

#Check the number of people (unique PSU's)
length(unique(BRFSS13_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 34 103

#Check the number of unique strata
length(unique(BRFSS13_dataset[["_STSTR"]]))
#The number of unique strata is 1 304

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS13_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_LLCPWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS13_dataset)
#Observe the design oject
BRFSS13_DO


#_______________________________________________________________________________________________

#----Analysis----

#1. Overall prevalence
B13_overall <- svymean(~factor(HAVARTH3),
                       BRFSS13_DO,
                       na.rm = TRUE)
B13_overall.c <- B13_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B13_overall_ci <- confint(B13_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B13 <- bind_cols(B13_overall.c, B13_overall_ci)
#remove HAVARTH3 = 0
B13 <- B13[-c(1), ] #B13 = final proportion, se and 95% ci

#Overall number of people
B13.no <- svytotal(~HAVARTH3, 
                   BRFSS13_DO, 
                   na.rm = TRUE, 
                   deff = TRUE)
B13.no.df <- as.data.frame(B13.no)
#ci
B13.no.ci <- confint(B13.no) %>%
  as.data.frame(.)
#join number and ci
B13.no <- bind_cols(B13.no.df, B13.no.ci)
#remove HAVARTH3=0
B13.no <- B13.no[-c(1), ] #B13.no = final number of people with arth, se, design effect and 95%ci


#2. Demographic-specific analysis

#A. Arthritis & Age
B13_Arth_age <- svyby(formula = ~HAVARTH3,
                      by = ~`_AGEG5YR`,
                      design = BRFSS13_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B13_Arthtitis_age <- B13_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B13_arth_age_ci <- confint(B13_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No)
B13_arth_age_ci <- B13_arth_age_ci[-c(1:11), ]
#join ci and proportions
B13.Age <- bind_cols(B13_Arthtitis_age, B13_arth_age_ci) #B13.Age = final proportion, se and 95% ci by age group


#Number of people by age
B13.no.age <- svyby(formula = ~HAVARTH3,
                    by = ~`_AGEG5YR`,
                    design = BRFSS13_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B13.no.age.c <- B13.no.age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Number of People", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B13.no.age.ci <- confint(B13.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No), which are the first 11 rows
B13.no.age.ci <- B13.no.age.ci[-c(1:11), ]
#join number and ci
B13.no.age <- bind_cols(B13.no.age.c, B13.no.age.ci)


#Age logistic regression
B13_age_glm <- svyglm(HAVARTH3~`_AGEG5YR` + SEX,
                      family = quasibinomial,
                      design = BRFSS13_DO)
summary(B13_age_glm)
exp(cbind(OR=coef(B13_age_glm), confint(B13_age_glm)))



#B. Arthritis & Sex
B13_Arth_sex <- svyby(formula = ~HAVARTH3,
                      by = ~SEX,
                      design = BRFSS13_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B13_Arthritis_sex <- B13_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B13_arth_sex_ci <- confint(B13_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No)
B13_arth_sex_ci <- B13_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B13.Sex <- bind_cols(B13_Arthritis_sex, B13_arth_sex_ci) #B13.Sex = final proportion, se and 95% ci by sex


#Number of people by sex
B13.no.sex <- svyby(formula = ~HAVARTH3,
                    by = ~SEX,
                    design = BRFSS13_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B13.no.sex.c <- B13.no.sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Number of People", "SE")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B13.no.sex.ci <- confint(B13.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No), which are the first 2 rows
B13.no.sex.ci <- B13.no.sex.ci[-c(1:2), ]
#join number and ci
B13.no.sex <- bind_cols(B13.no.sex.c, B13.no.sex.ci)


#Sex logistic regression
B13_sex_glm <- svyglm(HAVARTH3~relevel(SEX, ref = "Male") + `_AGEG5YR`,
                      family = quasibinomial,
                      design = BRFSS13_DO)
summary(B13_sex_glm)
exp(cbind(OR=coef(B13_sex_glm), confint(B13_sex_glm)))


#C. Arthritis & BMI
B13_Arth_BMI <- svyby(formula = ~HAVARTH3,
                      by = ~`_BMI5CAT`,
                      design = BRFSS13_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B13_Arthritis_BMI <- B13_Arth_BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Proportion", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B13_arth_BMI_ci <- confint(B13_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0
B13_arth_BMI_ci <- B13_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
B13.BMI <- bind_cols(B13_Arthritis_BMI, B13_arth_BMI_ci) #B13.BMI = final proportion, se and 95%ci by BMI


#Number of people by BMI
B13.no.BMI <- svyby(formula = ~HAVARTH3,
                    by = ~`_BMI5CAT`,
                    design = BRFSS13_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B13.no.BMI.c <- B13.no.BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Number of People", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B13.no.BMI.ci <- confint(B13.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No), which are the first 4 rows
B13.no.BMI.ci <- B13.no.BMI.ci[-c(1:4), ]
#join number and ci
B13.no.BMI <- bind_cols(B13.no.BMI.c, B13.no.BMI.ci)


#BMI logistic regression (using original continuous variable from survey)
B13.BMI.glm <- svyglm(HAVARTH3 ~ `_BMI5` + `_AGEG5YR` + SEX,
                      family = quasibinomial,
                      design = BRFSS13_DO)
summary(B13.BMI.glm)
exp(cbind(OR=coef(B13.BMI.glm), confint(B13.BMI.glm)))


#   End of 2013 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(BRFSS13)
remove(BRFSS13_dataset)
remove(BRFSS13_DO)
remove(BRFSS13_raw)
remove(B13_Arth_age)
remove(B13_arth_age_ci)
remove(B13_Arth_BMI)
remove(B13_arth_BMI_ci)
remove(B13_Arth_sex)
remove(B13_arth_sex_ci)
remove(B13_Arthritis_BMI)
remove(B13_Arthritis_sex)
remove(B13_Arthtitis_age)
remove(B13_arth_age_ci)
remove(B13_overall.c)
remove(B13_overall_ci)
remove(B13.no.age.c)
remove(B13.no.age.ci)
remove(B13.no.BMI.c)
remove(B13.no.BMI.ci)
remove(B13.no.df)
remove(B13.no.ci)
remove(B13.no.sex.c)
remove(B13.no.sex.ci)
remove(B13_age_glm)
remove(B13_sex_glm)
remove(B13.BMI.glm)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

#Behavioural Risk Factor Surveillance System (BRFSS)
#2014


#----2014----


#----Download----

B14_url <- "http://www.cdc.gov/brfss/annual_data/2014/files/LLCP2014XPT.ZIP"

BRtemp14 <- tempfile()
BRtemp14b <- tempfile()

download.file(B14_url, BRtemp14, mode = "wb")
unzip(zipfile = BRtemp14, exdir = BRtemp14b)
BRFSS14_raw <- read_xpt(file.path(BRtemp14b, "LLCP2014.XPT"))

unlink(c(BRtemp14, BRtemp14b))

#_______________________________________________________________________________________________


#----Cleaning----

#Observe the dataset
str(BRFSS14_raw) 
tail(BRFSS14_raw) 
glimpse(BRFSS14_raw) 
colnames(BRFSS14_raw)

BRFSS14 <- dplyr::select(BRFSS14_raw,
                         "_PSU", "_LLCPWT", "_STSTR", "HAVARTH3",
                         "_AGEG5YR", "SEX", "_BMI5", "_BMI5CAT")

#Observations
str(BRFSS14)
tail(BRFSS14)
glimpse(BRFSS14)
colnames(BRFSS14)


#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS14))
which(colSums(is.na(BRFSS14)) == nrow(BRFSS14)) #Named integer = 0, which means that at least one reponse is present for the variables selected.



#HAVARTH

#Observe how many people answered yes, no, don't know, & refused
table(BRFSS14$HAVARTH3)
#     1      2      7      9 
#161814 300402   2125    321 

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS14$HAVARTH3 <- recode(BRFSS14$HAVARTH3,
                           "1" = "1",
                           "2" = "0",
                           "7" = "7",
                           "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS14$HAVARTH3 <- unknownToNA(BRFSS14$HAVARTH3, unknown = c("7", "9")))
table(BRFSS14$HAVARTH3)
#     0      1 
#300402 161814 
BRFSS14$HAVARTH3 <- as.factor(BRFSS14$HAVARTH3)
class(BRFSS14$HAVARTH3)


#AGE

#Observe data
table(BRFSS14$`_AGEG5YR`)

#Recode age data into categories corresponding to BRFSS codebook
BRFSS14$`_AGEG5YR`<- recode(BRFSS14$`_AGEG5YR`,
                            "1" = "Age 18 to 24",
                            "2" = "Age 25 to 29",
                            "3" = "Age 30 to 34",
                            "4" = "Age 35 to 39",
                            "5" = "Age 40 to 44",
                            "6" = "Age 45 to 49",
                            "7" = "Age 50 to 54",
                            "8" = "Age 55 to 59",
                            "9" = "Age 60 to 64",
                            "10" = "Age 65 to 69",
                            "11" = "Age 70 and above",
                            "12" = "Age 70 and above",
                            "13" = "Age 70 and above",
                            "14" = "14")
#Change the unknown (14) values to NA
(BRFSS14$`_AGEG5YR` <- unknownToNA(BRFSS14$`_AGEG5YR`, unknown = "14"))
table(BRFSS14$`_AGEG5YR`)
#    Age 18 to 24     Age 25 to 29     Age 30 to 34     Age 35 to 39     Age 40 to 44     Age 45 to 49 
#           24198            19891            23662            25444            28597            32686 
#    Age 50 to 54     Age 55 to 59     Age 60 to 64     Age 65 to 69 Age 70 and above 
#           43366            49432            52620            50680           108310
BRFSS14$`_AGEG5YR` <- as.factor(BRFSS14$`_AGEG5YR`)


#SEX

#Observe data
table(BRFSS14$SEX)
#Recode sex data into categories corresponding to BRFSS codebook
BRFSS14$SEX <- recode(BRFSS14$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS14$SEX)
#Female   Male 
#271694 192970 
BRFSS14$SEX <- as.factor(BRFSS14$SEX)
class(BRFSS14$SEX)


#BMI

#observe data
table(BRFSS14$`_BMI5`)
#recode unknown numerical to NA
BRFSS14$`_BMI5` <- unknownToNA(BRFSS14$`_BMI5`, unknown = "9999")
#implied 2 decimal places, therefore divide all values by 100 to get the BMI value
BRFSS14 <- BRFSS14 %>%
  mutate(`_BMI5` = `_BMI5`/100)
#check that unknown data do not appear
BRFSS14 %>% top_n(10, `_BMI5`)


#Recode BMI categories
BRFSS14$`_BMI5CAT` <- recode(BRFSS14$`_BMI5CAT`,
                             "1" = "Underweight",
                             "2" = "Healthy weight",
                             "3" = "Overweight",
                             "4" = "Obese")
BRFSS14$`_BMI5CAT` <- as.factor(BRFSS14$`_BMI5CAT`)
table(BRFSS14$`_BMI5CAT`)
#Healthy weight          Obese     Overweight    Underweight 
#        141872         127583         157243           7272


#__________________________________________________________________________________________________________________

#DESIGN OBJECT CREATION

#Some final checks:
options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS14_dataset <- subset(BRFSS14,
                          !is.na(`_PSU`) &
                          !is.na(`_STSTR`) &
                          !is.na(`_LLCPWT`) &
                          !is.na(HAVARTH3))

#Check that the sum of the weights is equal to the US population
sum(BRFSS14_dataset$`_LLCPWT`)
#The sum of the weights is 247 103 316

#Check the number of people (unique PSU's)
length(unique(BRFSS14_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 22 392

#Check the number of unique strata
length(unique(BRFSS14_dataset[["_STSTR"]]))
#The number of unique strata is 1 279

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS14_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_LLCPWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS14_dataset)
#Observe the design object
BRFSS14_DO

#_______________________________________________________________________________________________


#----Analysis----

#1. Overall prevalence
B14_overall <- svymean(~factor(HAVARTH3),
                       BRFSS14_DO,
                       na.rm = TRUE)
B14_overall.c <- B14_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B14_overall_ci <- confint(B14_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B14 <- bind_cols(B14_overall.c, B14_overall_ci)
#remove HAVARTH3 = 0
B14 <- B14[-c(1), ] #B14 = final proportion, se and 95% ci

#Overall number of people
B14.no <- svytotal(~HAVARTH3, 
                   BRFSS14_DO, 
                   na.rm = TRUE, 
                   deff = TRUE)
B14.no.df <- as.data.frame(B14.no)
#ci
B14.no.ci <- confint(B14.no) %>%
  as.data.frame(.)
#join number and ci
B14.no <- bind_cols(B14.no.df, B14.no.ci)
#remove HAVARTH3=0
B14.no <- B14.no[-c(1), ] #B14.no = final number of people with arth, se, design effect and 95%ci


#2. Demographic-specific analysis

#A. Arthritis & Age
B14_Arth_age <- svyby(formula = ~HAVARTH3,
                      by = ~`_AGEG5YR`,
                      design = BRFSS14_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B14_Arthtitis_age <- B14_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B14_arth_age_ci <- confint(B14_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No)
B14_arth_age_ci <- B14_arth_age_ci[-c(1:11), ]
#join ci and proportions
B14.Age <- bind_cols(B14_Arthtitis_age, B14_arth_age_ci) #B14.Age = final proportion, se and 95% ci by age group


#Number of people by age
B14.no.age <- svyby(formula = ~HAVARTH3,
                    by = ~`_AGEG5YR`,
                    design = BRFSS14_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B14.no.age.c <- B14.no.age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Number of People", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B14.no.age.ci <- confint(B14.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No), which are the first 11 rows
B14.no.age.ci <- B14.no.age.ci[-c(1:11), ]
#join number and ci
B14.no.age <- bind_cols(B14.no.age.c, B14.no.age.ci)


#Age logistic regression
B14_age_glm <- svyglm(HAVARTH3~`_AGEG5YR` + SEX,
                      family = quasibinomial,
                      design = BRFSS14_DO)
summary(B14_age_glm)
exp(cbind(OR=coef(B14_age_glm), confint(B14_age_glm)))



#B. Arthritis & Sex
B14_Arth_sex <- svyby(formula = ~HAVARTH3,
                      by = ~SEX,
                      design = BRFSS14_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B14_Arthritis_sex <- B14_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B14_arth_sex_ci <- confint(B14_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No)
B14_arth_sex_ci <- B14_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B14.Sex <- bind_cols(B14_Arthritis_sex, B14_arth_sex_ci) #B14.Sex = final proportion, se and 95% ci by sex


#Number of people by sex
B14.no.sex <- svyby(formula = ~HAVARTH3,
                    by = ~SEX,
                    design = BRFSS14_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B14.no.sex.c <- B14.no.sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Number of People", "SE")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B14.no.sex.ci <- confint(B14.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No), which are the first 2 rows
B14.no.sex.ci <- B14.no.sex.ci[-c(1:2), ]
#join number and ci
B14.no.sex <- bind_cols(B14.no.sex.c, B14.no.sex.ci)


#Sex logistic regression
B14_sex_glm <- svyglm(HAVARTH3~relevel(SEX, ref = "Male") + `_AGEG5YR`,
                      family = quasibinomial,
                      design = BRFSS14_DO)
summary(B14_sex_glm)
exp(cbind(OR=coef(B14_sex_glm), confint(B14_sex_glm)))


#C. Arthritis & BMI
B14_Arth_BMI <- svyby(formula = ~HAVARTH3,
                      by = ~`_BMI5CAT`,
                      design = BRFSS14_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B14_Arthritis_BMI <- B14_Arth_BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Proportion", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B14_arth_BMI_ci <- confint(B14_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0
B14_arth_BMI_ci <- B14_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
B14.BMI <- bind_cols(B14_Arthritis_BMI, B14_arth_BMI_ci) #B14.BMI = final proportion, se and 95%ci by BMI

#Number of people by BMI
B14.no.BMI <- svyby(formula = ~HAVARTH3,
                    by = ~`_BMI5CAT`,
                    design = BRFSS14_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B14.no.BMI.c <- B14.no.BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Number of People", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B14.no.BMI.ci <- confint(B14.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No), which are the first 4 rows
B14.no.BMI.ci <- B14.no.BMI.ci[c(5:8), ]
#join number and ci
B14.no.BMI <- bind_cols(B14.no.BMI.c, B14.no.BMI.ci)


#BMI logistic regression (using original continuous variable from survey)
B14.BMI.glm <- svyglm(HAVARTH3 ~ `_BMI5` + `_AGEG5YR` + SEX,
                      family = quasibinomial,
                      design = BRFSS14_DO)
summary(B14.BMI.glm)
exp(cbind(OR=coef(B14.BMI.glm), confint(B14.BMI.glm)))


#   End of 2014 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(BRFSS14)
remove(BRFSS14_dataset)
remove(BRFSS14_DO)
remove(BRFSS14_raw)
remove(B14_Arth_age)
remove(B14_arth_age_ci)
remove(B14_Arth_BMI)
remove(B14_arth_BMI_ci)
remove(B14_Arth_sex)
remove(B14_arth_sex_ci)
remove(B14_Arthritis_BMI)
remove(B14_Arthritis_sex)
remove(B14_Arthtitis_age)
remove(B14_arth_age_ci)
remove(B14_overall.c)
remove(B14_overall_ci)
remove(B14.no.age.c)
remove(B14.no.age.ci)
remove(B14.no.BMI.c)
remove(B14.no.BMI.ci)
remove(B14.no.df)
remove(B14.no.ci)
remove(B14.no.sex.c)
remove(B14.no.sex.ci)
remove(B14_age_glm)
remove(B14_sex_glm)
remove(B14.BMI.glm)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

#Behavioural Risk Factor Surveillance System (BRFSS)
#2015


#----2015----


#----Download----

B15_url <- "https://www.cdc.gov/brfss/annual_data/2015/files/LLCP2015XPT.zip"

BRtemp15 <- tempfile()
BRtemp15b <- tempfile()

download.file(B15_url, BRtemp15, mode = "wb")
unzip(zipfile = BRtemp15, exdir = BRtemp15b)
BRFSS15_raw <- read_xpt(file.path(BRtemp15b, "LLCP2015.XPT"))

unlink(c(BRtemp15, BRtemp15b))

#_______________________________________________________________________________________________


#----Cleaning----

#Observe the dataset
str(BRFSS15_raw) 
tail(BRFSS15_raw) 
glimpse(BRFSS15_raw) 
colnames(BRFSS15_raw)

BRFSS15 <- dplyr::select(BRFSS15_raw,
                         "_PSU", "_LLCPWT", "_STSTR", "HAVARTH3",
                         "_AGEG5YR", "SEX", "_BMI5", "_BMI5CAT")

#Observations
str(BRFSS15)
tail(BRFSS15)
glimpse(BRFSS15)
colnames(BRFSS15)


#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS15))
which(colSums(is.na(BRFSS15)) == nrow(BRFSS15)) #Named integer = 0, which means that at least one reponse is present for the variables selected.



#HAVARTH

#Observe how many people answered yes, no, don't know, & refused
table(BRFSS15$HAVARTH3)
#     1      2      7      9 
#148036 290621   2563    235 

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS15$HAVARTH3 <- recode(BRFSS15$HAVARTH3,
                           "1" = "1",
                           "2" = "0",
                           "7" = "7",
                           "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS15$HAVARTH3 <- unknownToNA(BRFSS15$HAVARTH3, unknown = c("7", "9")))
table(BRFSS15$HAVARTH3)
#     0      1 
#290621 148036 
BRFSS15$HAVARTH3 <- as.factor(BRFSS15$HAVARTH3)
class(BRFSS15$HAVARTH3)


#AGE

#Observe data
table(BRFSS15$`_AGEG5YR`)

#Recode age data into categories corresponding to BRFSS codebook
BRFSS15$`_AGEG5YR`<- recode(BRFSS15$`_AGEG5YR`,
                            "1" = "Age 18 to 24",
                            "2" = "Age 25 to 29",
                            "3" = "Age 30 to 34",
                            "4" = "Age 35 to 39",
                            "5" = "Age 40 to 44",
                            "6" = "Age 45 to 49",
                            "7" = "Age 50 to 54",
                            "8" = "Age 55 to 59",
                            "9" = "Age 60 to 64",
                            "10" = "Age 65 to 69",
                            "11" = "Age 70 and above",
                            "12" = "Age 70 and above",
                            "13" = "Age 70 and above",
                            "14" = "14")
#Change the unknown (14) values to NA
(BRFSS15$`_AGEG5YR` <- unknownToNA(BRFSS15$`_AGEG5YR`, unknown = "14"))
table(BRFSS15$`_AGEG5YR`)
#    Age 18 to 24     Age 25 to 29     Age 30 to 34     Age 35 to 39     Age 40 to 44     Age 45 to 49 
#           24192            19746            22917            24545            25942            30276 
#    Age 50 to 54     Age 55 to 59     Age 60 to 64     Age 65 to 69 Age 70 and above 
#           39881            46209            49794            49264           103354
BRFSS15$`_AGEG5YR` <- as.factor(BRFSS15$`_AGEG5YR`)


#SEX

#Observe data
table(BRFSS15$SEX)
#Recode sex data into categories corresponding to BRFSS codebook
BRFSS15$SEX <- recode(BRFSS15$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS15$SEX)
#Female   Male 
#254518 186938 
BRFSS15$SEX <- as.factor(BRFSS15$SEX)
class(BRFSS15$SEX)


#BMI

#observe data
table(BRFSS15$`_BMI5`)
#recode unknown numerical to NA
BRFSS15$`_BMI5` <- unknownToNA(BRFSS15$`_BMI5`, unknown = "9999")
#implied 2 decimal places, therefore divide all values by 100 to get the BMI value
BRFSS15 <- BRFSS15 %>%
  mutate(`_BMI5` = `_BMI5`/100)
#check that unknown data do not appear
BRFSS15 %>% top_n(10, `_BMI5`)


#Recode BMI categories
BRFSS15$`_BMI5CAT` <- recode(BRFSS15$`_BMI5CAT`,
                             "1" = "Underweight",
                             "2" = "Healthy weight",
                             "3" = "Overweight",
                             "4" = "Obese")
BRFSS15$`_BMI5CAT` <- as.factor(BRFSS15$`_BMI5CAT`)
table(BRFSS15$`_BMI5CAT`)
#Healthy weight          Obese     Overweight    Underweight 
#        131409         119924         147004           6721


#__________________________________________________________________________________________________________________

#DESIGN OBJECT CREATION

#Some final checks:
options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS15_dataset <- subset(BRFSS15,
                          !is.na(`_PSU`) &
                            !is.na(`_STSTR`) &
                            !is.na(`_LLCPWT`) &
                            !is.na(HAVARTH3))

#Check that the sum of the weights is equal to the US population
sum(BRFSS15_dataset$`_LLCPWT`)
#The sum of the weights is 249 827 768

#Check the number of people (unique PSU's)
length(unique(BRFSS15_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 23 206

#Check the number of unique strata
length(unique(BRFSS15_dataset[["_STSTR"]]))
#The number of unique strata is 1 306

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS15_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_LLCPWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS15_dataset)
#Observe the design object
BRFSS15_DO


#_______________________________________________________________________________________________


#----Analysis----

#1. Overall prevalence
B15_overall <- svymean(~factor(HAVARTH3),
                       BRFSS15_DO,
                       na.rm = TRUE)
B15_overall.c <- B15_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B15_overall_ci <- confint(B15_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B15 <- bind_cols(B15_overall.c, B15_overall_ci)
#remove HAVARTH3 = 0
B15 <- B15[-c(1), ] #B15 = final proportion, se and 95% ci

#Overall number of people
B15.no <- svytotal(~HAVARTH3, 
                   BRFSS15_DO, 
                   na.rm = TRUE, 
                   deff = TRUE)
B15.no.df <- as.data.frame(B15.no)
#ci
B15.no.ci <- confint(B15.no) %>%
  as.data.frame(.)
#join number and ci
B15.no <- bind_cols(B15.no.df, B15.no.ci)
#remove HAVARTH3=0
B15.no <- B15.no[-c(1), ] #B15.no = final number of people with arth, se, design effect and 95%ci


#2. Demographic-specific analysis

#A. Arthritis & Age
B15_Arth_age <- svyby(formula = ~HAVARTH3,
                      by = ~`_AGEG5YR`,
                      design = BRFSS15_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B15_Arthtitis_age <- B15_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B15_arth_age_ci <- confint(B15_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No)
B15_arth_age_ci <- B15_arth_age_ci[-c(1:11), ]
#join ci and proportions
B15.Age <- bind_cols(B15_Arthtitis_age, B15_arth_age_ci) #B15.Age = final proportion, se and 95% ci by age group

#Number of people by age
B15.no.age <- svyby(formula = ~HAVARTH3,
                    by = ~`_AGEG5YR`,
                    design = BRFSS15_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B15.no.age.c <- B15.no.age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Number of People", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B15.no.age.ci <- confint(B15.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No), which are the first 11 rows
B15.no.age.ci <- B15.no.age.ci[-c(1:11), ]
#join number and ci
B15.no.age <- bind_cols(B15.no.age.c, B15.no.age.ci)


#Age logistic regression
B15_age_glm <- svyglm(HAVARTH3~`_AGEG5YR` + SEX,
                      family = quasibinomial,
                      design = BRFSS15_DO)
summary(B15_age_glm)
exp(cbind(OR=coef(B15_age_glm), confint(B15_age_glm)))



#B. Arthritis & Sex
B15_Arth_sex <- svyby(formula = ~HAVARTH3,
                      by = ~SEX,
                      design = BRFSS15_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B15_Arthritis_sex <- B15_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B15_arth_sex_ci <- confint(B15_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No)
B15_arth_sex_ci <- B15_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B15.Sex <- bind_cols(B15_Arthritis_sex, B15_arth_sex_ci) #B15.Sex = final proportion, se and 95% ci by sex

#Number of people by sex
B15.no.sex <- svyby(formula = ~HAVARTH3,
                    by = ~SEX,
                    design = BRFSS15_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B15.no.sex.c <- B15.no.sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Number of People", "SE")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B15.no.sex.ci <- confint(B15.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No), which are the first 2 rows
B15.no.sex.ci <- B15.no.sex.ci[-c(1:2), ]
#join number and ci
B15.no.sex <- bind_cols(B15.no.sex.c, B15.no.sex.ci)


#Sex logistic regression
B15_sex_glm <- svyglm(HAVARTH3~relevel(SEX, ref = "Male") + `_AGEG5YR`,
                      family = quasibinomial,
                      design = BRFSS15_DO)
summary(B15_sex_glm)
exp(cbind(OR=coef(B15_sex_glm), confint(B15_sex_glm)))


#C. Arthritis & BMI
B15_Arth_BMI <- svyby(formula = ~HAVARTH3,
                      by = ~`_BMI5CAT`,
                      design = BRFSS15_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B15_Arthritis_BMI <- B15_Arth_BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Proportion", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B15_arth_BMI_ci <- confint(B15_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0
B15_arth_BMI_ci <- B15_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
B15.BMI <- bind_cols(B15_Arthritis_BMI, B15_arth_BMI_ci) #B15.BMI = final proportion, se and 95%ci by BMI


#Number of people by BMI
B15.no.BMI <- svyby(formula = ~HAVARTH3,
                    by = ~`_BMI5CAT`,
                    design = BRFSS15_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B15.no.BMI.c <- B15.no.BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Number of People", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B15.no.BMI.ci <- confint(B15.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No), which are the first 4 rows
B15.no.BMI.ci <- B15.no.BMI.ci[-c(1:4), ]
#join number and ci
B15.no.BMI <- bind_cols(B15.no.BMI.c, B15.no.BMI.ci)

#BMI logistic regression (using original continuous variable from survey)
B15.BMI.glm <- svyglm(HAVARTH3 ~ `_BMI5` + `_AGEG5YR` + SEX,
                      family = quasibinomial,
                      design = BRFSS15_DO)
summary(B15.BMI.glm)
exp(cbind(OR=coef(B15.BMI.glm), confint(B15.BMI.glm)))


#   End of 2015 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(BRFSS15)
remove(BRFSS15_dataset)
remove(BRFSS15_DO)
remove(BRFSS15_raw)
remove(B15_Arth_age)
remove(B15_arth_age_ci)
remove(B15_Arth_BMI)
remove(B15_arth_BMI_ci)
remove(B15_Arth_sex)
remove(B15_arth_sex_ci)
remove(B15_Arthritis_BMI)
remove(B15_Arthritis_sex)
remove(B15_Arthtitis_age)
remove(B15_arth_age_ci)
remove(B15_overall.c)
remove(B15_overall_ci)
remove(B15.no.age.c)
remove(B15.no.age.ci)
remove(B15.no.BMI.c)
remove(B15.no.BMI.ci)
remove(B15.no.df)
remove(B15.no.ci)
remove(B15.no.sex.c)
remove(B15.no.sex.ci)
remove(B15_age_glm)
remove(B15_sex_glm)
remove(B15.BMI.glm)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

#Behavioural Risk Factor Surveillance System (BRFSS)
#2016


#----2016----


#----Download----

B16_url <- "https://www.cdc.gov/brfss/annual_data/2016/files/LLCP2016XPT.zip"

BRtemp16 <- tempfile()
BRtemp16b <- tempfile()

download.file(B16_url, BRtemp16, mode = "wb")
unzip(zipfile = BRtemp16, exdir = BRtemp16b)
BRFSS16_raw <- read_xpt(file.path(BRtemp16b, "LLCP2016.XPT"))

unlink(c(BRtemp16, BRtemp16b))


#----Cleaning----

#Observe the dataset
str(BRFSS16_raw) 
tail(BRFSS16_raw) 
glimpse(BRFSS16_raw) 
colnames(BRFSS16_raw)

BRFSS16 <- dplyr::select(BRFSS16_raw,
                         "_PSU", "_LLCPWT", "_STSTR", "HAVARTH3",
                         "_AGEG5YR", "SEX", "_BMI5", "_BMI5CAT")

#Observations
str(BRFSS16)
tail(BRFSS16)
glimpse(BRFSS16)
colnames(BRFSS16)


#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS16))
which(colSums(is.na(BRFSS16)) == nrow(BRFSS16)) #Named integer = 0, which means that at least one reponse is present for the variables selected.


#HAVARTH

#Observe how many people answered yes, no, don't know, & refused
table(BRFSS16$HAVARTH3)
#     1      2      7      9 
#169793 314033   2203    271 

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS16$HAVARTH3 <- recode(BRFSS16$HAVARTH3,
                           "1" = "1",
                           "2" = "0",
                           "7" = "7",
                           "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS16$HAVARTH3 <- unknownToNA(BRFSS16$HAVARTH3, unknown = c("7", "9")))
table(BRFSS16$HAVARTH3)
#     0      1 
#314033 169793
BRFSS16$HAVARTH3 <- as.factor(BRFSS16$HAVARTH3)
class(BRFSS16$HAVARTH3)


#AGE

#Observe data
table(BRFSS16$`_AGEG5YR`)

#Recode age data into categories corresponding to BRFSS codebook
BRFSS16$`_AGEG5YR`<- recode(BRFSS16$`_AGEG5YR`,
                            "1" = "Age 18 to 24",
                            "2" = "Age 25 to 29",
                            "3" = "Age 30 to 34",
                            "4" = "Age 35 to 39",
                            "5" = "Age 40 to 44",
                            "6" = "Age 45 to 49",
                            "7" = "Age 50 to 54",
                            "8" = "Age 55 to 59",
                            "9" = "Age 60 to 64",
                            "10" = "Age 65 to 69",
                            "11" = "Age 70 and above",
                            "12" = "Age 70 and above",
                            "13" = "Age 70 and above",
                            "14" = "14")
#Change the unknown (14) values to NA
(BRFSS16$`_AGEG5YR` <- unknownToNA(BRFSS16$`_AGEG5YR`, unknown = "14"))
table(BRFSS16$`_AGEG5YR`)
#    Age 18 to 24     Age 25 to 29     Age 30 to 34     Age 35 to 39     Age 40 to 44     Age 45 to 49 
#           26626            23034            25432            27192            27195            33014 
#    Age 50 to 54     Age 55 to 59     Age 60 to 64     Age 65 to 69 Age 70 and above 
#           41810            49799            54770            55831           114903
BRFSS16$`_AGEG5YR` <- as.factor(BRFSS16$`_AGEG5YR`)


#SEX

#Observe data
table(BRFSS16$SEX)
#Recode sex data into categories corresponding to BRFSS codebook
BRFSS16$SEX <- recode(BRFSS16$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS16$SEX)
#Female   Male 
#275631 210606 
BRFSS16$SEX <- as.factor(BRFSS16$SEX)
class(BRFSS16$SEX)


#BMI

#observe data
table(BRFSS16$`_BMI5`)
#recode unknown numerical to NA
BRFSS16$`_BMI5` <- unknownToNA(BRFSS16$`_BMI5`, unknown = "9999")
#implied 2 decimal places, therefore divide all values by 100 to get the BMI value
BRFSS16 <- BRFSS16 %>%
  mutate(`_BMI5` = `_BMI5`/100)
#check that unknown data do not appear
BRFSS16 %>% top_n(10, `_BMI5`)


#Recode BMI categories
BRFSS16$`_BMI5CAT` <- recode(BRFSS16$`_BMI5CAT`,
                             "1" = "Underweight",
                             "2" = "Healthy weight",
                             "3" = "Overweight",
                             "4" = "Obese")
BRFSS16$`_BMI5CAT` <- as.factor(BRFSS16$`_BMI5CAT`)
table(BRFSS16$`_BMI5CAT`)
#Healthy weight          Obese     Overweight    Underweight 
#        142110         135765         161282           7530


#__________________________________________________________________________________________________________________

#DESIGN OBJECT CREATION

#Some final checks:
options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS16_dataset <- subset(BRFSS16,
                          !is.na(`_PSU`) &
                            !is.na(`_STSTR`) &
                            !is.na(`_LLCPWT`) &
                            !is.na(HAVARTH3))

#Check that the sum of the weights is equal to the US population
sum(BRFSS16_dataset$`_LLCPWT`)
#The sum of the weights is 252 827 750

#Check the number of people (unique PSU's)
length(unique(BRFSS16_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 36 937

#Check the number of unique strata
length(unique(BRFSS16_dataset[["_STSTR"]]))
#The number of unique strata is 1 655

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS16_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_LLCPWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS16_dataset)
#Observe the design object
BRFSS16_DO


#_______________________________________________________________________________________________


#----Analysis----

#1. Overall prevalence
B16_overall <- svymean(~factor(HAVARTH3),
                       BRFSS16_DO,
                       na.rm = TRUE)
B16_overall.c <- B16_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B16_overall_ci <- confint(B16_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B16 <- bind_cols(B16_overall.c, B16_overall_ci)
#remove HAVARTH3 = 0
B16 <- B16[-c(1), ] #B16 = final proportion, se and 95% ci

#Overall number of people
B16.no <- svytotal(~HAVARTH3, 
                   BRFSS16_DO, 
                   na.rm = TRUE, 
                   deff = TRUE)
B16.no.df <- as.data.frame(B16.no)
#ci
B16.no.ci <- confint(B16.no) %>%
  as.data.frame(.)
#join number and ci
B16.no <- bind_cols(B16.no.df, B16.no.ci)
#remove HAVARTH3=0
B16.no <- B16.no[-c(1), ] #B16.no = final number of people with arth, se, design effect and 95%ci


#2. Demographic-specific analysis

#A. Arthritis & Age
B16_Arth_age <- svyby(formula = ~HAVARTH3,
                      by = ~`_AGEG5YR`,
                      design = BRFSS16_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B16_Arthtitis_age <- B16_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B16_arth_age_ci <- confint(B16_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No)
B16_arth_age_ci <- B16_arth_age_ci[-c(1:11), ]
#join ci and proportions
B16.Age <- bind_cols(B16_Arthtitis_age, B16_arth_age_ci) #B16.Age = final proportion, se and 95% ci by age group


#Number of people by age
B16.no.age <- svyby(formula = ~HAVARTH3,
                    by = ~`_AGEG5YR`,
                    design = BRFSS16_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B16.no.age.c <- B16.no.age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Number of People", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B16.no.age.ci <- confint(B16.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No), which are the first 11 rows
B16.no.age.ci <- B16.no.age.ci[-c(1:11), ]
#join number and ci
B16.no.age <- bind_cols(B16.no.age.c, B16.no.age.ci)

#Age logistic regression
B16_age_glm <- svyglm(HAVARTH3~`_AGEG5YR` + SEX,
                      family = quasibinomial,
                      design = BRFSS16_DO)
summary(B16_age_glm)
exp(cbind(OR=coef(B16_age_glm), confint(B16_age_glm)))



#B. Arthritis & Sex
B16_Arth_sex <- svyby(formula = ~HAVARTH3,
                      by = ~SEX,
                      design = BRFSS16_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B16_Arthritis_sex <- B16_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B16_arth_sex_ci <- confint(B16_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No)
B16_arth_sex_ci <- B16_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B16.Sex <- bind_cols(B16_Arthritis_sex, B16_arth_sex_ci) #B16.Sex = final proportion, se and 95% ci by sex


#Number of people by sex
B16.no.sex <- svyby(formula = ~HAVARTH3,
                    by = ~SEX,
                    design = BRFSS16_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B16.no.sex.c <- B16.no.sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Number of People", "SE")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B16.no.sex.ci <- confint(B16.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No), which are the first 2 rows
B16.no.sex.ci <- B16.no.sex.ci[-c(1:2), ]
#join number and ci
B16.no.sex <- bind_cols(B16.no.sex.c, B16.no.sex.ci)


#Sex logistic regression
B16_sex_glm <- svyglm(HAVARTH3~relevel(SEX, ref = "Male") + `_AGEG5YR`,
                      family = quasibinomial,
                      design = BRFSS16_DO)
summary(B16_sex_glm)
exp(cbind(OR=coef(B16_sex_glm), confint(B16_sex_glm)))


#C. Arthritis & BMI
B16_Arth_BMI <- svyby(formula = ~HAVARTH3,
                      by = ~`_BMI5CAT`,
                      design = BRFSS16_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B16_Arthritis_BMI <- B16_Arth_BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Proportion", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B16_arth_BMI_ci <- confint(B16_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0
B16_arth_BMI_ci <- B16_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
B16.BMI <- bind_cols(B16_Arthritis_BMI, B16_arth_BMI_ci) #B16.BMI = final proportion, se and 95%ci by BMI


#Number of people by BMI
B16.no.BMI <- svyby(formula = ~HAVARTH3,
                    by = ~`_BMI5CAT`,
                    design = BRFSS16_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B16.no.BMI.c <- B16.no.BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Number of People", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B16.no.BMI.ci <- confint(B16.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No), which are the first 4 rows
B16.no.BMI.ci <- B16.no.BMI.ci[-c(1:4), ]
#join number and ci
B16.no.BMI <- bind_cols(B16.no.BMI.c, B16.no.BMI.ci)


#BMI logistic regression (using original continuous variable from survey)
B16.BMI.glm <- svyglm(HAVARTH3 ~ `_BMI5` + `_AGEG5YR` + SEX,
                      family = quasibinomial,
                      design = BRFSS16_DO)
summary(B16.BMI.glm)
exp(cbind(OR=coef(B16.BMI.glm), confint(B16.BMI.glm)))


#   End of 2016 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(BRFSS16)
remove(BRFSS16_dataset)
remove(BRFSS16_DO)
remove(BRFSS16_raw)
remove(B16_Arth_age)
remove(B16_arth_age_ci)
remove(B16_Arth_BMI)
remove(B16_arth_BMI_ci)
remove(B16_Arth_sex)
remove(B16_arth_sex_ci)
remove(B16_Arthritis_BMI)
remove(B16_Arthritis_sex)
remove(B16_Arthtitis_age)
remove(B16_arth_age_ci)
remove(B16_overall.c)
remove(B16_overall_ci)
remove(B16.no.age.c)
remove(B16.no.age.ci)
remove(B16.no.BMI.c)
remove(B16.no.BMI.ci)
remove(B16.no.df)
remove(B16.no.ci)
remove(B16.no.sex.c)
remove(B16.no.sex.ci)
remove(B16_age_glm)
remove(B16_sex_glm)
remove(B16.BMI.glm)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

#Behavioural Risk Factor Surveillance System (BRFSS)
#2017


#----2017----


#----Download----

B17_url <- "https://www.cdc.gov/brfss/annual_data/2017/files/LLCP2017XPT.zip"
BRtemp17 <- tempfile()
BRtemp17b <- tempfile()

download.file(B17_url, BRtemp17, mode = "wb")
unzip(zipfile = BRtemp17, exdir = BRtemp17b)
BRFSS17_raw <- read_xpt(file.path(BRtemp17b, "LLCP2017.XPT"))


unlink(c(BRtemp17, BRtemp17b))


#----Cleaning----

#Observe the dataset
str(BRFSS17_raw) 
tail(BRFSS17_raw) 
glimpse(BRFSS17_raw) 
colnames(BRFSS17_raw)

BRFSS17 <- dplyr::select(BRFSS17_raw,
                         "_PSU", "_LLCPWT", "_STSTR", "HAVARTH3",
                         "_AGEG5YR", "SEX", "_BMI5", "_BMI5CAT")

#Observations
str(BRFSS17)
tail(BRFSS17)
glimpse(BRFSS17)
colnames(BRFSS17)


#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS17))
which(colSums(is.na(BRFSS17)) == nrow(BRFSS17)) #Named integer = 0, which means that at least one reponse is present for the variables selected.


#HAVARTH

#Observe how many people answered yes, no, don't know, & refused
table(BRFSS17$HAVARTH3)
#     1      2      7      9 
#147288 299934   2546    245 

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS17$HAVARTH3 <- recode(BRFSS17$HAVARTH3,
                           "1" = "1",
                           "2" = "0",
                           "7" = "7",
                           "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS17$HAVARTH3 <- unknownToNA(BRFSS17$HAVARTH3, unknown = c("7", "9")))
table(BRFSS17$HAVARTH3)
#     0      1 
#299934 147288
BRFSS17$HAVARTH3 <- as.factor(BRFSS17$HAVARTH3)
class(BRFSS17$HAVARTH3)


#AGE

#Observe data
table(BRFSS17$`_AGEG5YR`)

#Recode age data into categories corresponding to BRFSS codebook
BRFSS17$`_AGEG5YR`<- recode(BRFSS17$`_AGEG5YR`,
                            "1" = "Age 18 to 24",
                            "2" = "Age 25 to 29",
                            "3" = "Age 30 to 34",
                            "4" = "Age 35 to 39",
                            "5" = "Age 40 to 44",
                            "6" = "Age 45 to 49",
                            "7" = "Age 50 to 54",
                            "8" = "Age 55 to 59",
                            "9" = "Age 60 to 64",
                            "10" = "Age 65 to 69",
                            "11" = "Age 70 and above",
                            "12" = "Age 70 and above",
                            "13" = "Age 70 and above",
                            "14" = "14")
#Change the unknown (14) values to NA
(BRFSS17$`_AGEG5YR` <- unknownToNA(BRFSS17$`_AGEG5YR`, unknown = "14"))
table(BRFSS17$`_AGEG5YR`)
#    Age 18 to 24     Age 25 to 29     Age 30 to 34     Age 35 to 39     Age 40 to 44     Age 45 to 49     Age 50 to 54 
#           26233            22388            24799            26297            25300            30134            37149 
#    Age 55 to 59     Age 60 to 64     Age 65 to 69 Age 70 and above 
#           45124            49974            50020           106497
BRFSS17$`_AGEG5YR` <- as.factor(BRFSS17$`_AGEG5YR`)


#SEX

#Observe data
table(BRFSS17$SEX)
#Recode sex data into categories corresponding to BRFSS codebook
BRFSS17$SEX <- recode(BRFSS17$SEX,
                      "1" = "Male",
                      "2" = "Female")
table(BRFSS17$SEX)
#Female   Male 
#251007 198725 
BRFSS17$SEX <- as.factor(BRFSS17$SEX)
class(BRFSS17$SEX)


#BMI

#observe data
table(BRFSS17$`_BMI5`)
#recode unknown numerical to NA
BRFSS17$`_BMI5` <- unknownToNA(BRFSS17$`_BMI5`, unknown = "9999")
#implied 2 decimal places, therefore divide all values by 100 to get the BMI value
BRFSS17 <- BRFSS17 %>%
  mutate(`_BMI5` = `_BMI5`/100)
#check that unknown data do not appear
BRFSS17 %>% top_n(10, `_BMI5`)


#Recode BMI categories
BRFSS17$`_BMI5CAT` <- recode(BRFSS17$`_BMI5CAT`,
                             "1" = "Underweight",
                             "2" = "Healthy weight",
                             "3" = "Overweight",
                             "4" = "Obese")
BRFSS17$`_BMI5CAT` <- as.factor(BRFSS17$`_BMI5CAT`)
table(BRFSS17$`_BMI5CAT`)
#Healthy weight          Obese     Overweight    Underweight 
#        128856         128641         149148           6925


#__________________________________________________________________________________________________________________

#DESIGN OBJECT CREATION

#Some final checks:
options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS17_dataset <- subset(BRFSS17,
                          !is.na(`_PSU`) &
                            !is.na(`_STSTR`) &
                            !is.na(`_LLCPWT`) &
                            !is.na(HAVARTH3))

#Check that the sum of the weights is equal to the US population
sum(BRFSS17_dataset$`_LLCPWT`)
#The sum of the weights is 254 205 400

#Check the number of people (unique PSU's)
length(unique(BRFSS17_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 22 058

#Check the number of unique strata
length(unique(BRFSS17_dataset[["_STSTR"]]))
#The number of unique strata is 1 659

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS17_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_LLCPWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS17_dataset)
#Observe the design object
BRFSS17_DO


#_______________________________________________________________________________________________


#----Analysis----

#1. Overall prevalence
B17_overall <- svymean(~factor(HAVARTH3),
                       BRFSS17_DO,
                       na.rm = TRUE)
B17_overall.c <- B17_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B17_overall_ci <- confint(B17_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B17 <- bind_cols(B17_overall.c, B17_overall_ci)
#remove HAVARTH3 = 0
B17 <- B17[-c(1), ] #B17 = final proportion, se and 95% ci

#Overall number of people
B17.no <- svytotal(~HAVARTH3, 
                   BRFSS17_DO, 
                   na.rm = TRUE, 
                   deff = TRUE)
B17.no.df <- as.data.frame(B17.no)
#ci
B17.no.ci <- confint(B17.no) %>%
  as.data.frame(.)
#join number and ci
B17.no <- bind_cols(B17.no.df, B17.no.ci)
#remove HAVARTH3=0
B17.no <- B17.no[-c(1), ] #B17.no = final number of people with arth, se, design effect and 95%ci


#2. Demographic-specific analysis

#A. Arthritis & Age
B17_Arth_age <- svyby(formula = ~HAVARTH3,
                      by = ~`_AGEG5YR`,
                      design = BRFSS17_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B17_Arthtitis_age <- B17_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B17_arth_age_ci <- confint(B17_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No)
B17_arth_age_ci <- B17_arth_age_ci[-c(1:11), ]
#join ci and proportions
B17.Age <- bind_cols(B17_Arthtitis_age, B17_arth_age_ci) #B17.Age = final proportion, se and 95% ci by age group


#Number of people by age
B17.no.age <- svyby(formula = ~HAVARTH3,
                    by = ~`_AGEG5YR`,
                    design = BRFSS17_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B17.no.age.c <- B17.no.age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Number of People", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B17.no.age.ci <- confint(B17.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No), which are the first 11 rows
B17.no.age.ci <- B17.no.age.ci[-c(1:11), ]
#join number and ci
B17.no.age <- bind_cols(B17.no.age.c, B17.no.age.ci)


#Age logistic regression
B17_age_glm <- svyglm(HAVARTH3~`_AGEG5YR` + SEX,
                      family = quasibinomial,
                      design = BRFSS17_DO)
summary(B17_age_glm)
exp(cbind(OR=coef(B17_age_glm), confint(B17_age_glm)))



#B. Arthritis & Sex
B17_Arth_sex <- svyby(formula = ~HAVARTH3,
                      by = ~SEX,
                      design = BRFSS17_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B17_Arthritis_sex <- B17_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B17_arth_sex_ci <- confint(B17_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No)
B17_arth_sex_ci <- B17_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B17.Sex <- bind_cols(B17_Arthritis_sex, B17_arth_sex_ci) #B17.Sex = final proportion, se and 95% ci by sex


#Number of people by sex
B17.no.sex <- svyby(formula = ~HAVARTH3,
                    by = ~SEX,
                    design = BRFSS17_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B17.no.sex.c <- B17.no.sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Number of People", "SE")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B17.no.sex.ci <- confint(B17.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No), which are the first 2 rows
B17.no.sex.ci <- B17.no.sex.ci[-c(1:2), ]
#join number and ci
B17.no.sex <- bind_cols(B17.no.sex.c, B17.no.sex.ci)


#Sex logistic regression
B17_sex_glm <- svyglm(HAVARTH3~relevel(SEX, ref = "Male") + `_AGEG5YR`,
                      family = quasibinomial,
                      design = BRFSS17_DO)
summary(B17_sex_glm)
exp(cbind(OR=coef(B17_sex_glm), confint(B17_sex_glm)))


#C. Arthritis & BMI
B17_Arth_BMI <- svyby(formula = ~HAVARTH3,
                      by = ~`_BMI5CAT`,
                      design = BRFSS17_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B17_Arthritis_BMI <- B17_Arth_BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Proportion", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B17_arth_BMI_ci <- confint(B17_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0
B17_arth_BMI_ci <- B17_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
B17.BMI <- bind_cols(B17_Arthritis_BMI, B17_arth_BMI_ci) #B17.BMI = final proportion, se and 95%ci by BMI


#Number of people by BMI
B17.no.BMI <- svyby(formula = ~HAVARTH3,
                    by = ~`_BMI5CAT`,
                    design = BRFSS17_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B17.no.BMI.c <- B17.no.BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Number of People", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B17.no.BMI.ci <- confint(B17.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No), which are the first 4 rows
B17.no.BMI.ci <- B17.no.BMI.ci[-c(1:4), ]
#join number and ci
B17.no.BMI <- bind_cols(B17.no.BMI.c, B17.no.BMI.ci)

#BMI logistic regression (using original continuous variable from survey)
B17.BMI.glm <- svyglm(HAVARTH3 ~ `_BMI5` + `_AGEG5YR` + SEX,
                      family = quasibinomial,
                      design = BRFSS17_DO)
summary(B17.BMI.glm)
exp(cbind(OR=coef(B17.BMI.glm), confint(B17.BMI.glm)))


#   End of 2017 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(BRFSS17)
remove(BRFSS17_dataset)
remove(BRFSS17_DO)
remove(BRFSS17_raw)
remove(B17_Arth_age)
remove(B17_arth_age_ci)
remove(B17_Arth_BMI)
remove(B17_arth_BMI_ci)
remove(B17_Arth_sex)
remove(B17_arth_sex_ci)
remove(B17_Arthritis_BMI)
remove(B17_Arthritis_sex)
remove(B17_Arthtitis_age)
remove(B17_arth_age_ci)
remove(B17_overall.c)
remove(B17_overall_ci)
remove(B17.no.age.c)
remove(B17.no.age.ci)
remove(B17.no.BMI.c)
remove(B17.no.BMI.ci)
remove(B17.no.df)
remove(B17.no.ci)
remove(B17.no.sex.c)
remove(B17.no.sex.ci)
remove(B17_age_glm)
remove(B17_sex_glm)
remove(B17.BMI.glm)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

#Behavioural Risk Factor Surveillance System (BRFSS)
#2018


#----2018----


#----Download----

B18_url <- "https://www.cdc.gov/brfss/annual_data/2018/files/LLCP2018XPT.zip"

tempB18 <- tempfile()
tempB18b <- tempfile()

download.file(B18_url, tempB18)
unzip(zipfile = tempB18, exdir = tempB18b)
BRFSS18_raw <- read_xpt(file.path(tempB18b, "LLCP2018.XPT"))

unlink(c(tempB18, tempB18b))

#_______________________________________________________________________________________________

#----Cleaning----

#Observe the dataset
str(BRFSS18_raw) 
tail(BRFSS18_raw) 
glimpse(BRFSS18_raw) 
colnames(BRFSS18_raw)

BRFSS18 <- dplyr::select(BRFSS18_raw,
                         "_PSU", "_LLCPWT", "_STSTR", "HAVARTH3",
                         "_AGEG5YR", "SEX1", "_BMI5", "_BMI5CAT")

#Observations
str(BRFSS18)
tail(BRFSS18)
glimpse(BRFSS18)
colnames(BRFSS18)


#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS18))
which(colSums(is.na(BRFSS18)) == nrow(BRFSS18)) #Named integer = 0, which means that at least one reponse is present for the variables selected.


#HAVARTH

#Observe how many people answered yes, no, don't know, & refused
table(BRFSS18$HAVARTH3)
#     1      2      7      9 
#149614 285288   2231    278 

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS18$HAVARTH3 <- recode(BRFSS18$HAVARTH3,
                           "1" = "1",
                           "2" = "0",
                           "7" = "7",
                           "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS18$HAVARTH3 <- unknownToNA(BRFSS18$HAVARTH3, unknown = c("7", "9")))
table(BRFSS18$HAVARTH3)
#     0      1 
#285288 149614
BRFSS18$HAVARTH3 <- as.factor(BRFSS18$HAVARTH3)
class(BRFSS18$HAVARTH3)


#AGE

#Observe data
table(BRFSS18$`_AGEG5YR`)

#Recode age data into categories corresponding to BRFSS codebook
BRFSS18$`_AGEG5YR`<- recode(BRFSS18$`_AGEG5YR`,
                            "1" = "Age 18 to 24",
                            "2" = "Age 25 to 29",
                            "3" = "Age 30 to 34",
                            "4" = "Age 35 to 39",
                            "5" = "Age 40 to 44",
                            "6" = "Age 45 to 49",
                            "7" = "Age 50 to 54",
                            "8" = "Age 55 to 59",
                            "9" = "Age 60 to 64",
                            "10" = "Age 65 to 69",
                            "11" = "Age 70 and above",
                            "12" = "Age 70 and above",
                            "13" = "Age 70 and above",
                            "14" = "14")
#Change the unknown (14) values to NA
(BRFSS18$`_AGEG5YR` <- unknownToNA(BRFSS18$`_AGEG5YR`, unknown = "14"))
table(BRFSS18$`_AGEG5YR`)
#    Age 18 to 24     Age 25 to 29     Age 30 to 34     Age 35 to 39     Age 40 to 44     Age 45 to 49 
#           26005            22286            24269            26118            25583            28846 
#    Age 50 to 54     Age 55 to 59     Age 60 to 64     Age 65 to 69 Age 70 and above 
#           35060            42160            46994            47329           104314
BRFSS18$`_AGEG5YR` <- as.factor(BRFSS18$`_AGEG5YR`)


#SEX

#Observe data
table(BRFSS18$SEX1)
#Recode sex data into categories corresponding to BRFSS codebook
BRFSS18$SEX1 <- recode(BRFSS18$SEX1,
                       "1" = "Male",
                       "2" = "Female")
table(BRFSS18$SEX1)
#Female   Male 
#238911 197412 
BRFSS18$SEX1 <- as.factor(BRFSS18$SEX1)
class(BRFSS18$SEX1)


#BMI

#observe data
table(BRFSS18$`_BMI5`)
#recode unknown numerical to NA
BRFSS18$`_BMI5` <- unknownToNA(BRFSS18$`_BMI5`, unknown = "9999")
#implied 2 decimal places, therefore divide all values by 100 to get the BMI value
BRFSS18 <- BRFSS18 %>%
  mutate(`_BMI5` = `_BMI5`/100)
#check that unknown data do not appear
BRFSS18 %>% top_n(10, `_BMI5`)


#Recode BMI categories
BRFSS18$`_BMI5CAT` <- recode(BRFSS18$`_BMI5CAT`,
                             "1" = "Underweight",
                             "2" = "Healthy weight",
                             "3" = "Overweight",
                             "4" = "Obese")
BRFSS18$`_BMI5CAT` <- as.factor(BRFSS18$`_BMI5CAT`)
table(BRFSS18$`_BMI5CAT`)
#Healthy weight          Obese     Overweight    Underweight 
#        123522         127998         143878           6776


#__________________________________________________________________________________________________________________

#DESIGN OBJECT CREATION

#Some final checks:
options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS18_dataset <- subset(BRFSS18,
                          !is.na(`_PSU`) &
                            !is.na(`_STSTR`) &
                            !is.na(`_LLCPWT`) &
                            !is.na(HAVARTH3))

#Check that the sum of the weights is equal to the US population
sum(BRFSS18_dataset$`_LLCPWT`)
#The sum of the weights is 256 672 777

#Check the number of people (unique PSU's)
length(unique(BRFSS18_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 35 618

#Check the number of unique strata
length(unique(BRFSS18_dataset[["_STSTR"]]))
#The number of unique strata is 1 737

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS18_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_LLCPWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS18_dataset)
#Observe the design object
BRFSS18_DO


#_______________________________________________________________________________________________

#----Analysis----

#1. Overall prevalence
B18_overall <- svymean(~factor(HAVARTH3),
                       BRFSS18_DO,
                       na.rm = TRUE)
B18_overall.c <- B18_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B18_overall_ci <- confint(B18_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B18 <- bind_cols(B18_overall.c, B18_overall_ci)
#remove HAVARTH3 = 0
B18 <- B18[-c(1), ] #B18 = final proportion, se and 95% ci

#Overall number of people
B18.no <- svytotal(~HAVARTH3, 
                   BRFSS18_DO, 
                   na.rm = TRUE, 
                   deff = TRUE)
B18.no.df <- as.data.frame(B18.no)
#ci
B18.no.ci <- confint(B18.no) %>%
  as.data.frame(.)
#join number and ci
B18.no <- bind_cols(B18.no.df, B18.no.ci)
#remove HAVARTH3=0
B18.no <- B18.no[-c(1), ] #B18.no = final number of people with arth, se, design effect and 95%ci

#2. Demographic-specific analysis

#A. Arthritis & Age
B18_Arth_age <- svyby(formula = ~HAVARTH3,
                      by = ~`_AGEG5YR`,
                      design = BRFSS18_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B18_Arthtitis_age <- B18_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B18_arth_age_ci <- confint(B18_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No)
B18_arth_age_ci <- B18_arth_age_ci[-c(1:11), ]
#join ci and proportions
B18.Age <- bind_cols(B18_Arthtitis_age, B18_arth_age_ci) #B18.Age = final proportion, se and 95% ci by age group


#Number of people by age
B18.no.age <- svyby(formula = ~HAVARTH3,
                    by = ~`_AGEG5YR`,
                    design = BRFSS18_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B18.no.age.c <- B18.no.age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Number of People", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B18.no.age.ci <- confint(B18.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No), which are the first 11 rows
B18.no.age.ci <- B18.no.age.ci[-c(1:11), ]
#join number and ci
B18.no.age <- bind_cols(B18.no.age.c, B18.no.age.ci)


#Age logistic regression
B18_age_glm <- svyglm(HAVARTH3~`_AGEG5YR` + SEX1,
                      family = quasibinomial,
                      design = BRFSS18_DO)
summary(B18_age_glm)
exp(cbind(OR=coef(B18_age_glm), confint(B18_age_glm)))



#B. Arthritis & Sex
B18_Arth_sex <- svyby(formula = ~HAVARTH3,
                      by = ~SEX1,
                      design = BRFSS18_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B18_Arthritis_sex <- B18_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B18_arth_sex_ci <- confint(B18_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No)
B18_arth_sex_ci <- B18_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B18.Sex <- bind_cols(B18_Arthritis_sex, B18_arth_sex_ci) #B18.Sex = final proportion, se and 95% ci by sex


#Number of people by sex
B18.no.sex <- svyby(formula = ~HAVARTH3,
                    by = ~SEX1,
                    design = BRFSS18_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B18.no.sex.c <- B18.no.sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Number of People", "SE")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B18.no.sex.ci <- confint(B18.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No), which are the first 2 rows
B18.no.sex.ci <- B18.no.sex.ci[-c(1:2), ]
#join number and ci
B18.no.sex <- bind_cols(B18.no.sex.c, B18.no.sex.ci)


#Sex logistic regression
B18_sex_glm <- svyglm(HAVARTH3~relevel(SEX1, ref = "Male") + `_AGEG5YR`,
                      family = quasibinomial,
                      design = BRFSS18_DO)
summary(B18_sex_glm)
exp(cbind(OR=coef(B18_sex_glm), confint(B18_sex_glm)))


#C. Arthritis & BMI
B18_Arth_BMI <- svyby(formula = ~HAVARTH3,
                      by = ~`_BMI5CAT`,
                      design = BRFSS18_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B18_Arthritis_BMI <- B18_Arth_BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Proportion", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B18_arth_BMI_ci <- confint(B18_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0
B18_arth_BMI_ci <- B18_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
B18.BMI <- bind_cols(B18_Arthritis_BMI, B18_arth_BMI_ci) #B18.BMI = final proportion, se and 95%ci by BMI


#Number of people by BMI
B18.no.BMI <- svyby(formula = ~HAVARTH3,
                    by = ~`_BMI5CAT`,
                    design = BRFSS18_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B18.no.BMI.c <- B18.no.BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Number of People", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B18.no.BMI.ci <- confint(B18.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH3 = 0 (No), which are the first 4 rows
B18.no.BMI.ci <- B18.no.BMI.ci[-c(1:4), ]
#join number and ci
B18.no.BMI <- bind_cols(B18.no.BMI.c, B18.no.BMI.ci)


#BMI logistic regression (using original continuous variable from survey)
B18.BMI.glm <- svyglm(HAVARTH3 ~ `_BMI5` + `_AGEG5YR` + SEX1,
                      family = quasibinomial,
                      design = BRFSS18_DO)
summary(B18.BMI.glm)
exp(cbind(OR=coef(B18.BMI.glm), confint(B18.BMI.glm)))


#   End of 2018 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(BRFSS18)
remove(BRFSS18_dataset)
remove(BRFSS18_DO)
remove(BRFSS18_raw)
remove(B18_Arth_age)
remove(B18_arth_age_ci)
remove(B18_Arth_BMI)
remove(B18_arth_BMI_ci)
remove(B18_Arth_sex)
remove(B18_arth_sex_ci)
remove(B18_Arthritis_BMI)
remove(B18_Arthritis_sex)
remove(B18_Arthtitis_age)
remove(B18_arth_age_ci)
remove(B18_overall.c)
remove(B18_overall_ci)
remove(B18.no.age.c)
remove(B18.no.age.ci)
remove(B18.no.BMI.c)
remove(B18.no.BMI.ci)
remove(B18.no.df)
remove(B18.no.ci)
remove(B18.no.sex.c)
remove(B18.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

#Behavioural Risk Factor Surveillance System (BRFSS)
#2019


#----2019----


#----Download----

B19_url <- "https://www.cdc.gov/brfss/annual_data/2019/files/LLCP2019XPT.zip"

tempB19 <- tempfile()
tempB19b <- tempfile()

download.file(B19_url, tempB19)
unzip(zipfile = tempB19, exdir = tempB19b)
BRFSS19_raw <- read_xpt(file.path(tempB19b, "LLCP2019.XPT"))

unlink(c(tempB19, tempB19b))

#_______________________________________________________________________________________________

#----Cleaning----

#Observe the dataset
str(BRFSS19_raw) 
tail(BRFSS19_raw) 
glimpse(BRFSS19_raw) 
colnames(BRFSS19_raw)

BRFSS19 <- select(BRFSS19_raw,
                  "_PSU", "_LLCPWT", "_STSTR", "HAVARTH4",
                  "_AGEG5YR", "SEXVAR", "_BMI5", "_BMI5CAT")


#Observations
str(BRFSS19)
tail(BRFSS19)
glimpse(BRFSS19)
colnames(BRFSS19)


#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS19))
which(colSums(is.na(BRFSS19)) == nrow(BRFSS19)) #Named integer = 0, which means that at least one reponse is present for the variables selected.



#HAVARTH

#Observe how many people answered yes, no, don't know, & refused
table(BRFSS19$HAVARTH4)
#     1      2      7      9 
#139703 276020   2286    249 

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS19$HAVARTH4 <- recode(BRFSS19$HAVARTH4,
                           "1" = "1",
                           "2" = "0",
                           "7" = "7",
                           "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS19$HAVARTH4 <- unknownToNA(BRFSS19$HAVARTH4, unknown = c("7", "9")))
table(BRFSS19$HAVARTH4)
#     0      1 
#276020 139703
BRFSS19$HAVARTH4 <- as.factor(BRFSS19$HAVARTH4)
class(BRFSS19$HAVARTH4)


#AGE

#Observe data
table(BRFSS19$`_AGEG5YR`)

#Recode age data into categories corresponding to BRFSS codebook
BRFSS19$`_AGEG5YR`<- recode(BRFSS19$`_AGEG5YR`,
                            "1" = "Age 18 to 24",
                            "2" = "Age 25 to 29",
                            "3" = "Age 30 to 34",
                            "4" = "Age 35 to 39",
                            "5" = "Age 40 to 44",
                            "6" = "Age 45 to 49",
                            "7" = "Age 50 to 54",
                            "8" = "Age 55 to 59",
                            "9" = "Age 60 to 64",
                            "10" = "Age 65 to 69",
                            "11" = "Age 70 and above",
                            "12" = "Age 70 and above",
                            "13" = "Age 70 and above",
                            "14" = "14")
#Change the unknown (14) values to NA
(BRFSS19$`_AGEG5YR` <- unknownToNA(BRFSS19$`_AGEG5YR`, unknown = "14"))
table(BRFSS19$`_AGEG5YR`)
#    Age 18 to 24     Age 25 to 29     Age 30 to 34     Age 35 to 39     Age 40 to 44     Age 45 to 49     Age 50 to 54     Age 55 to 59 
#           25098            20817            23058            24724            24258            26075            31768            38902 
#    Age 60 to 64     Age 65 to 69 Age 70 and above 
#           44456            45206           107218
BRFSS19$`_AGEG5YR` <- as.factor(BRFSS19$`_AGEG5YR`)


#SEX

#Observe data
table(BRFSS19$SEXVAR)
#Recode sex data into categories corresponding to BRFSS codebook
BRFSS19$SEXVAR <- recode(BRFSS19$SEXVAR,
                         "1" = "Male",
                         "2" = "Female")
table(BRFSS19$SEXVAR)
#Female   Male 
#228419 189849
BRFSS19$SEXVAR <- as.factor(BRFSS19$SEXVAR)
class(BRFSS19$SEXVAR)


#BMI

#observe data
table(BRFSS19$`_BMI5`)
#recode unknown numerical to NA
BRFSS19$`_BMI5` <- unknownToNA(BRFSS19$`_BMI5`, unknown = "9999")
#implied 2 decimal places, therefore divide all values by 100 to get the BMI value
BRFSS19 <- BRFSS19 %>%
  mutate(`_BMI5` = `_BMI5`/100)
#check that unknown data do not appear
BRFSS19 %>% top_n(10, `_BMI5`)


#Recode BMI categories
BRFSS19$`_BMI5CAT` <- recode(BRFSS19$`_BMI5CAT`,
                             "1" = "Underweight",
                             "2" = "Healthy weight",
                             "3" = "Overweight",
                             "4" = "Obese")
BRFSS19$`_BMI5CAT` <- as.factor(BRFSS19$`_BMI5CAT`)
table(BRFSS19$`_BMI5CAT`)
#Healthy weight          Obese     Overweight    Underweight 
#        115886         123216         136399           6564


#__________________________________________________________________________________________________________________

#DESIGN OBJECT CREATION

#Some final checks:
options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS19_dataset <- subset(BRFSS19,
                          !is.na(`_PSU`) &
                            !is.na(`_STSTR`) &
                            !is.na(`_LLCPWT`) &
                            !is.na(HAVARTH4))

#Check that the sum of the weights is equal to the US population
sum(BRFSS19_dataset$`_LLCPWT`)
#The sum of the weights is 250 938 904

#Check the number of people (unique PSU's)
length(unique(BRFSS19_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 17 414

#Check the number of unique strata
length(unique(BRFSS19_dataset[["_STSTR"]]))
#The number of unique strata is 1 632

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS19_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_LLCPWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS19_dataset)
#Observe the design oject
BRFSS19_DO


#_______________________________________________________________________________________________

#----Analysis----

#1. Overall prevalence
B19_overall <- svymean(~factor(HAVARTH4),
                       BRFSS19_DO,
                       na.rm = TRUE)
B19_overall.c <- B19_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B19_overall_ci <- confint(B19_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B19 <- bind_cols(B19_overall.c, B19_overall_ci)
#remove HAVARTH4 = 0
B19 <- B19[-c(1), ] #B19 = final proportion, se and 95% ci

#Overall number of people
B19.no <- svytotal(~HAVARTH4, 
                   BRFSS19_DO, 
                   na.rm = TRUE, 
                   deff = TRUE)
B19.no.df <- as.data.frame(B19.no)
#ci
B19.no.ci <- confint(B19.no) %>%
  as.data.frame(.)
#join number and ci
B19.no <- bind_cols(B19.no.df, B19.no.ci)
#remove HAVARTH4=0
B19.no <- B19.no[-c(1), ] #B19.no = final number of people with arth, se, design effect and 95%ci


#2. Demographic-specific analysis

#A. Arthritis & Age
B19_Arth_age <- svyby(formula = ~HAVARTH4,
                      by = ~`_AGEG5YR`,
                      design = BRFSS19_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B19_Arthtitis_age <- B19_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B19_arth_age_ci <- confint(B19_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No)
B19_arth_age_ci <- B19_arth_age_ci[-c(1:11), ]
#join ci and proportions
B19.Age <- bind_cols(B19_Arthtitis_age, B19_arth_age_ci) #B19.Age = final proportion, se and 95% ci by age group


#Number of people by age
B19.no.age <- svyby(formula = ~HAVARTH4,
                    by = ~`_AGEG5YR`,
                    design = BRFSS19_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B19.no.age.c <- B19.no.age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Number of People", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B19.no.age.ci <- confint(B19.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
B19.no.age.ci <- B19.no.age.ci[-c(1:11), ]
#join number and ci
B19.no.age <- bind_cols(B19.no.age.c, B19.no.age.ci)

#Age logistic regression
B19_age_glm <- svyglm(HAVARTH4~`_AGEG5YR` + SEXVAR,
                      family = quasibinomial,
                      design = BRFSS19_DO)
summary(B19_age_glm)
exp(cbind(OR=coef(B19_age_glm), confint(B19_age_glm)))



#B. Arthritis & sex
B19_Arth_sex <- svyby(formula = ~HAVARTH4,
                      by = ~SEXVAR,
                      design = BRFSS19_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B19_Arthritis_sex <- B19_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B19_arth_sex_ci <- confint(B19_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No)
B19_arth_sex_ci <- B19_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B19.Sex <- bind_cols(B19_Arthritis_sex, B19_arth_sex_ci) #B19.Sex = final proportion, se and 95% ci by sex


#Number of people by sex
B19.no.sex <- svyby(formula = ~HAVARTH4,
                    by = ~SEXVAR,
                    design = BRFSS19_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B19.no.sex.c <- B19.no.sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Number of People", "SE")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B19.no.sex.ci <- confint(B19.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 2 rows
B19.no.sex.ci <- B19.no.sex.ci[-c(1:2), ]
#join number and ci
B19.no.sex <- bind_cols(B19.no.sex.c, B19.no.sex.ci)

#Sex logistic regression
B19_sex_glm <- svyglm(HAVARTH4~relevel(SEXVAR, ref = "Male") + `_AGEG5YR`,
                      family = quasibinomial,
                      design = BRFSS19_DO)
summary(B19_sex_glm)
exp(cbind(OR=coef(B19_sex_glm), confint(B19_sex_glm)))


#C. Arthritis & BMI
B19_Arth_BMI <- svyby(formula = ~HAVARTH4,
                      by = ~`_BMI5CAT`,
                      design = BRFSS19_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B19_Arthritis_BMI <- B19_Arth_BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Proportion", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B19_arth_BMI_ci <- confint(B19_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0
B19_arth_BMI_ci <- B19_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
B19.BMI <- bind_cols(B19_Arthritis_BMI, B19_arth_BMI_ci) #B19.BMI = final proportion, se and 95%ci by BMI

#Number of people by BMI
B19.no.BMI <- svyby(formula = ~HAVARTH4,
                    by = ~`_BMI5CAT`,
                    design = BRFSS19_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B19.no.BMI.c <- B19.no.BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Number of People", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B19.no.BMI.ci <- confint(B19.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 4 rows
B19.no.BMI.ci <- B19.no.BMI.ci[-c(1:4), ]
#join number and ci
B19.no.BMI <- bind_cols(B19.no.BMI.c, B19.no.BMI.ci)

#BMI logistic regression (using original continuous variable from survey)
B19.BMI.glm <- svyglm(HAVARTH4 ~ `_BMI5` + `_AGEG5YR` + SEXVAR,
                      family = quasibinomial,
                      design = BRFSS19_DO)
summary(B19.BMI.glm)
exp(cbind(OR=coef(B19.BMI.glm), confint(B19.BMI.glm)))


#   End of 2019 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(BRFSS19)
remove(BRFSS19_dataset)
remove(BRFSS19_DO)
remove(BRFSS19_raw)
remove(B19_Arth_age)
remove(B19_arth_age_ci)
remove(B19_Arth_BMI)
remove(B19_arth_BMI_ci)
remove(B19_Arth_sex)
remove(B19_arth_sex_ci)
remove(B19_Arthritis_BMI)
remove(B19_Arthritis_sex)
remove(B19_Arthtitis_age)
remove(B19_arth_age_ci)
remove(B19_overall.c)
remove(B19_overall_ci)
remove(B19.no.age.c)
remove(B19.no.age.ci)
remove(B19.no.BMI.c)
remove(B19.no.BMI.ci)
remove(B19.no.df)
remove(B19.no.ci)
remove(B19.no.sex.c)
remove(B19.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

#Behavioural Risk Factor Surveillance System (BRFSS)
#2020


#----2020----


#----Download----

B20_url <- "https://www.cdc.gov/brfss/annual_data/2020/files/LLCP2020XPT.zip"

tempB20 <- tempfile()
tempB20b <- tempfile()

download.file(B20_url, tempB20)
unzip(zipfile = tempB20, exdir = tempB20b)
BRFSS20_raw <- read_xpt(file.path(tempB20b, "LLCP2020.XPT"))

unlink(c(tempB20, tempB20b))

#_______________________________________________________________________________________________

#----Cleaning----

#Observe the dataset
str(BRFSS20_raw) 
tail(BRFSS20_raw) 
glimpse(BRFSS20_raw) 
colnames(BRFSS20_raw)

BRFSS20 <- select(BRFSS20_raw,
                  "_PSU", "_LLCPWT", "_STSTR", "HAVARTH4",
                  "_AGEG5YR", "SEXVAR", "_BMI5", "_BMI5CAT")


#Observations
str(BRFSS20)
tail(BRFSS20)
glimpse(BRFSS20)
colnames(BRFSS20)


#Check to see the extent of missing there is missing data in the file
colSums(is.na(BRFSS20))
which(colSums(is.na(BRFSS20)) == nrow(BRFSS20)) #Named integer = 0, which means that at least one reponse is present for the variables selected.



#HAVARTH

#Observe how many people answered yes, no, don't know, & refused
table(BRFSS20$HAVARTH4)
#     1      2      7      9 
#124479 275176   1945    353 

#Recode arthritis variable: 1 = Yes = 1 and 2 = No = 0
BRFSS20$HAVARTH4 <- recode(BRFSS20$HAVARTH4,
                           "1" = "1",
                           "2" = "0",
                           "7" = "7",
                           "9" = "9")
#Change unknown (7) and refused (9) values to NA
(BRFSS20$HAVARTH4 <- unknownToNA(BRFSS20$HAVARTH4, unknown = c("7", "9")))
table(BRFSS20$HAVARTH4)
#     0      1 
#275176 124479
BRFSS20$HAVARTH4 <- as.factor(BRFSS20$HAVARTH4)
class(BRFSS20$HAVARTH4)


#AGE

#Observe data
table(BRFSS20$`_AGEG5YR`)

#Recode age data into categories corresponding to BRFSS codebook
BRFSS20$`_AGEG5YR`<- recode(BRFSS20$`_AGEG5YR`,
                            "1" = "Age 18 to 24",
                            "2" = "Age 25 to 29",
                            "3" = "Age 30 to 34",
                            "4" = "Age 35 to 39",
                            "5" = "Age 40 to 44",
                            "6" = "Age 45 to 49",
                            "7" = "Age 50 to 54",
                            "8" = "Age 55 to 59",
                            "9" = "Age 60 to 64",
                            "10" = "Age 65 to 69",
                            "11" = "Age 70 and above",
                            "12" = "Age 70 and above",
                            "13" = "Age 70 and above",
                            "14" = "14")
#Change the unknown (14) values to NA
(BRFSS20$`_AGEG5YR` <- unknownToNA(BRFSS20$`_AGEG5YR`, unknown = "14"))
table(BRFSS20$`_AGEG5YR`)
#        Age 18 to 24     Age 25 to 29     Age 30 to 34     Age 35 to 39     Age 40 to 44     Age 45 to 49     Age 50 to 54     Age 55 to 59 
#               25648            20911            23408            25492            25729            26428            31110            36219 
#        Age 60 to 64     Age 65 to 69 Age 70 and above 
#               41151            41570            96035
BRFSS20$`_AGEG5YR` <- as.factor(BRFSS20$`_AGEG5YR`)


#SEX

#Observe data
table(BRFSS20$SEXVAR)
#Recode sex data into categories corresponding to BRFSS codebook
BRFSS20$SEXVAR <- recode(BRFSS20$SEXVAR,
                         "1" = "Male",
                         "2" = "Female")
table(BRFSS20$SEXVAR)
#Female   Male 
#218016 183942 
BRFSS20$SEXVAR <- as.factor(BRFSS20$SEXVAR)
class(BRFSS20$SEXVAR)


#BMI

#observe data
table(BRFSS20$`_BMI5`)
#recode unknown numerical to NA
BRFSS20$`_BMI5` <- unknownToNA(BRFSS20$`_BMI5`, unknown = "9999")
#implied 2 decimal places, therefore divide all values by 100 to get the BMI value
BRFSS20 <- BRFSS20 %>%
  mutate(`_BMI5` = `_BMI5`/100)
#check that unknown data do not appear
BRFSS20 %>% top_n(10, `_BMI5`)


#Recode BMI categories
BRFSS20$`_BMI5CAT` <- recode(BRFSS20$`_BMI5CAT`,
                             "1" = "Underweight",
                             "2" = "Healthy weight",
                             "3" = "Overweight",
                             "4" = "Obese")
BRFSS20$`_BMI5CAT` <- as.factor(BRFSS20$`_BMI5CAT`)
table(BRFSS20$`_BMI5CAT`)
#Healthy weight          Obese     Overweight    Underweight 
#        110121         115541         128946           5993


#__________________________________________________________________________________________________________________

#DESIGN OBJECT CREATION

#Some final checks:
options(survey.lonely.psu = "adjust")

#Although there should not be any NA values in the PSU, weighting, strata and arthritis variables, subset the data to be sure
BRFSS20_dataset <- subset(BRFSS20,
                          !is.na(`_PSU`) &
                            !is.na(`_STSTR`) &
                            !is.na(`_LLCPWT`) &
                            !is.na(HAVARTH4))

#Check that the sum of the weights is equal to the US population
sum(BRFSS20_dataset$`_LLCPWT`)
#The sum of the weights is 258 931 982

#Check the number of people (unique PSU's)
length(unique(BRFSS20_dataset[["_PSU"]]))
#The number of unique PSU's in the data is 15 832

#Check the number of unique strata
length(unique(BRFSS20_dataset[["_STSTR"]]))
#The number of unique strata is 1 601

#Generate a design object such that data can be adequately weighted, accounting for strata and clustering
BRFSS20_DO <- svydesign(ids = ~`_PSU`,
                        weights = ~`_LLCPWT`,
                        strata = ~`_STSTR`,
                        nest = TRUE,
                        data = BRFSS20_dataset)
#Observe the design oject
BRFSS20_DO


#_______________________________________________________________________________________________

#----Analysis----

#1. Overall prevalence
B20_overall <- svymean(~factor(HAVARTH4),
                       BRFSS20_DO,
                       na.rm = TRUE)
B20_overall.c <- B20_overall %>%
  as.data.frame(.) %>%
  setNames(c("Proportion", "SE")) %>%
  mutate(Proportion = as.numeric(Proportion)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B20_overall_ci <- confint(B20_overall) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#join ci & proportion
B20 <- bind_cols(B20_overall.c, B20_overall_ci)
#remove HAVARTH4 = 0
B20 <- B20[-c(1), ] #B20 = final proportion, se and 95% ci

#Overall number of people
B20.no <- svytotal(~HAVARTH4, 
                   BRFSS20_DO, 
                   na.rm = TRUE, 
                   deff = TRUE)
B20.no.df <- as.data.frame(B20.no)
#ci
B20.no.ci <- confint(B20.no) %>%
  as.data.frame(.)
#join number and ci
B20.no <- bind_cols(B20.no.df, B20.no.ci)
#remove HAVARTH4=0
B20.no <- B20.no[-c(1), ] #B20.no = final number of people with arth, se, design effect and 95%ci


#2. Demographic-specific analysis

#A. Arthritis & Age
B20_Arth_age <- svyby(formula = ~HAVARTH4,
                      by = ~`_AGEG5YR`,
                      design = BRFSS20_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B20_Arthtitis_age <- B20_Arth_age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Proportion", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B20_arth_age_ci <- confint(B20_Arth_age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No)
B20_arth_age_ci <- B20_arth_age_ci[-c(1:11), ]
#join ci and proportions
B20.Age <- bind_cols(B20_Arthtitis_age, B20_arth_age_ci) #B20.Age = final proportion, se and 95% ci by age group

#Number of people by age
B20.no.age <- svyby(formula = ~HAVARTH4,
                    by = ~`_AGEG5YR`,
                    design = BRFSS20_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B20.no.age.c <- B20.no.age %>%
  select(1, 3, 5) %>%
  setNames(c("Age", "Number of People", "SE")) %>%
  mutate(Age = as.factor(Age)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B20.no.age.ci <- confint(B20.no.age) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 11 rows
B20.no.age.ci <- B20.no.age.ci[-c(1:11), ]
#join number and ci
B20.no.age <- bind_cols(B20.no.age.c, B20.no.age.ci)

#Age logistic regression
B20_age_glm <- svyglm(HAVARTH4~`_AGEG5YR` + SEXVAR,
                      family = quasibinomial,
                      design = BRFSS20_DO)
summary(B20_age_glm)
exp(cbind(OR=coef(B20_age_glm), confint(B20_age_glm)))



#B. Arthritis & sex
B20_Arth_sex <- svyby(formula = ~HAVARTH4,
                      by = ~SEXVAR,
                      design = BRFSS20_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B20_Arthritis_sex <- B20_Arth_sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Proportion", "SE")) %>%
  mutate(Sex = as.character(Sex)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B20_arth_sex_ci <- confint(B20_Arth_sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No)
B20_arth_sex_ci <- B20_arth_sex_ci[-c(1:2), ]
#join ci and proportions
B20.Sex <- bind_cols(B20_Arthritis_sex, B20_arth_sex_ci) #B20.Sex = final proportion, se and 95% ci by sex


#Number of people by sex
B20.no.sex <- svyby(formula = ~HAVARTH4,
                    by = ~SEXVAR,
                    design = BRFSS20_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B20.no.sex.c <- B20.no.sex %>%
  select(1, 3, 5) %>%
  setNames(c("Sex", "Number of People", "SE")) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B20.no.sex.ci <- confint(B20.no.sex) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 2 rows
B20.no.sex.ci <- B20.no.sex.ci[-c(1:2), ]
#join number and ci
B20.no.sex <- bind_cols(B20.no.sex.c, B20.no.sex.ci)


#Sex logistic regression
B20_sex_glm <- svyglm(HAVARTH4~relevel(SEXVAR, ref = "Male") + `_AGEG5YR`,
                      family = quasibinomial,
                      design = BRFSS20_DO)
summary(B20_sex_glm)
exp(cbind(OR=coef(B20_sex_glm), confint(B20_sex_glm)))


#C. Arthritis & BMI
B20_Arth_BMI <- svyby(formula = ~HAVARTH4,
                      by = ~`_BMI5CAT`,
                      design = BRFSS20_DO,
                      FUN = svymean,
                      na.rm = TRUE)
B20_Arthritis_BMI <- B20_Arth_BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Proportion", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2)))
#ci
B20_arth_BMI_ci <- confint(B20_Arth_BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~round(. * 100, digits = 2))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0
B20_arth_BMI_ci <- B20_arth_BMI_ci[-c(1:4), ]
#join ci and proportion
B20.BMI <- bind_cols(B20_Arthritis_BMI, B20_arth_BMI_ci) #B20.BMI = final proportion, se and 95%ci by BMI


#Number of people by BMI
B20.no.BMI <- svyby(formula = ~HAVARTH4,
                    by = ~`_BMI5CAT`,
                    design = BRFSS20_DO,
                    FUN = svytotal,
                    na.rm = TRUE,
                    deff = TRUE)
B20.no.BMI.c <- B20.no.BMI %>%
  select(1, 3, 5) %>%
  setNames(c("BMI", "Number of People", "SE")) %>%
  mutate(BMI = as.factor(BMI)) %>%
  mutate(`Number of People` = as.numeric(`Number of People`)) %>%
  mutate_if(is.numeric, list(~floor(.)))
#ci
B20.no.BMI.ci <- confint(B20.no.BMI) %>%
  as.data.frame(.) %>%
  mutate_if(is.numeric, list(~floor(.))) %>%
  setNames(c("CI_Prop_low", "CI_Prop_upp"))
#Remove CI for HAVARTH4 = 0 (No), which are the first 4 rows
B20.no.BMI.ci <- B20.no.BMI.ci[-c(1:4), ]
#join number and ci
B20.no.BMI <- bind_cols(B20.no.BMI.c, B20.no.BMI.ci)


#BMI logistic regression (using original continuous variable from survey)
B20.BMI.glm <- svyglm(HAVARTH4 ~ `_BMI5` + `_AGEG5YR` + SEXVAR,
                      family = quasibinomial,
                      design = BRFSS20_DO)
summary(B20.BMI.glm)
exp(cbind(OR=coef(B20.BMI.glm), confint(B20.BMI.glm)))


#   End of 2020 analysis
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

remove(BRFSS20)
remove(BRFSS20_dataset)
remove(BRFSS20_DO)
remove(BRFSS20_raw)
remove(B20_Arth_age)
remove(B20_arth_age_ci)
remove(B20_Arth_BMI)
remove(B20_arth_BMI_ci)
remove(B20_Arth_sex)
remove(B20_arth_sex_ci)
remove(B20_Arthritis_BMI)
remove(B20_Arthritis_sex)
remove(B20_Arthtitis_age)
remove(B20_arth_age_ci)
remove(B20_overall.c)
remove(B20_overall_ci)
remove(B20.no.age.c)
remove(B20.no.age.ci)
remove(B20.no.BMI.c)
remove(B20.no.BMI.ci)
remove(B20.no.df)
remove(B20.no.ci)
remove(B20.no.sex.c)
remove(B20.no.sex.ci)
gc()

#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________