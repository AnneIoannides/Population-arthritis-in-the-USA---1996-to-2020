#Ioannides AE, Wadley AL, Kamerman PR
#Arthritis in the USA: a longitudinal analysis of three nationally representative studies

# -- Calculating compound annual growth rates (CAGRs) --

#load packages
library(tidyverse)

# Pull the data files in from Dropbox
B.overall <- read.csv("BRFSS_prevalences.csv")
B.NO.overall <- read.csv("BRFSS_numbers.csv")
N.overall <- read.csv("NHIS_prevalences.csv")
N.NO.overall <- read.csv("NHIS_numbers.csv")
NA.overall <- read.csv("NHANES_prevalences.csv")
NA.NO.overall <- read.csv("NHANES_numbers.csv")

#----Calculate CAGR's --

#BRFSS
B.pop <- B.overall[ which(B.overall$Dem_group == "US Population"), ]
B.CAGR <- B.pop[B.pop$Year %in% c("2001", "2020"), ]
B.CAGR$Year[B.CAGR$Year == "2001"] <- "Start"
B.CAGR$Year[B.CAGR$Year == "2020"] <- "End"
#since we are only working with the proportions themselves, temporarily remove CI's
B.CAGR <- subset(B.CAGR, select = c("Proportion", "Year"))

B.pop.no <- B.NO.overall[ which(B.NO.overall$Dem_group == "US Population"), ]
B.CAGR.no <- B.pop.no[B.pop.no$Year %in% c("2001", "2020"), ]
B.CAGR.no$Year[B.CAGR.no$Year == "2001"] <- "Start"
B.CAGR.no$Year[B.CAGR.no$Year == "2020"] <- "End"
#since we are only working with the numbers of people themselves, temporarily remove CI's
B.CAGR.no <- subset(B.CAGR.no, select = c("Number.of.People", "Year"))

#merge
B.CAGR.t <- merge(B.CAGR, B.CAGR.no)
CAGR2 <- gather(B.CAGR.t, key = "metric", value = "amount", 2:3)
CAGR3B <- spread(CAGR2, key = Year, value = amount)

#Function: BRFSS
CAGR_formula_BRFSS <- function(EndVal, StartVal, years = 20) {
  values <- ((EndVal/StartVal)^(1/years) - 1)
  return(values)
}
CAGR_B <- mutate(CAGR3B,
                 CAGR_Perc = CAGR_formula_BRFSS(StartVal = Start, EndVal = End, years = 20)*100)

#NHIS
N.pop <- N.overall[ which(N.overall$Dem_group == "US Population"), ]
N.CAGR <- N.pop[N.pop$Year %in% c("2002", "2018"), ]
N.CAGR$Year[N.CAGR$Year == "2002"] <- "Start"
N.CAGR$Year[N.CAGR$Year == "2018"] <- "End"
#since we are only working with the proportions themselves, temporarily remove CI's
N.CAGR <- subset(N.CAGR, select = c("Proportion", "Year"))

N.pop.no <- N.NO.overall[ which(N.NO.overall$Dem_group == "US Population"), ]
N.CAGR.no <- N.pop.no[N.pop.no$Year %in% c("2002", "2018"), ]
N.CAGR.no$Year[N.CAGR.no$Year == "2002"] <- "Start"
N.CAGR.no$Year[N.CAGR.no$Year == "2018"] <- "End"
#since we are only working with the numbers of people themselves, temporarily remove CI's
N.CAGR.no <- subset(N.CAGR.no, select = c("Number.of.People", "Year"))

#merge
N.CAGR.t <- merge(N.CAGR, N.CAGR.no)
CAGR2 <- gather(N.CAGR.t, key = "metric", value = "amount", 2:3)
CAGR3N <- spread(CAGR2, key = Year, value = amount)

#Function: NHIS
CAGR_formula_NHIS <- function(EndVal, StartVal, years = 17) {
  values <- ((EndVal/StartVal)^(1/years) -1)
  return(values)
}

CAGR_N <- mutate(CAGR3N,
                 CAGR_Perc = CAGR_formula_NHIS(StartVal = Start, EndVal = End, years = 17)*100)

#NHANES
NA.pop <- NA.overall[ which(NA.overall$Dem_group == "US Population"), ]
NA.CAGR <- NA.pop[NA.pop$Year %in% c("2000", "2018"), ]
NA.CAGR$Year[NA.CAGR$Year == "2000"] <- "Start"
NA.CAGR$Year[NA.CAGR$Year == "2018"] <- "End"
#since we are only working with the numbers of people themselves, temporarily remove CI's
NA.CAGR <- subset(NA.CAGR, select = c("Proportion", "Year"))

NA.pop.no <- NA.NO.overall[ which(NA.NO.overall$Dem_group == "US Population"), ]
NA.CAGR.no <- NA.pop.no[NA.pop.no$Year %in% c("2000", "2018"), ]
NA.CAGR.no$Year[NA.CAGR.no$Year == "2000"] <- "Start"
NA.CAGR.no$Year[NA.CAGR.no$Year == "2018"] <- "End"
#since we are only working with the numbers of people themselves, temporarily remove CI's
NA.CAGR.no <- subset(NA.CAGR.no, select = c("Number.of.People", "Year"))

#merge
NA.CAGR.t <- merge(NA.CAGR, NA.CAGR.no)
CAGR2 <- gather(NA.CAGR.t, key = "metric", value = "amount", 2:3)
CAGR3NA <- spread(CAGR2, key = Year, value = amount)

#Function: NHANES [note: compounded bi-annually, therefore need to divide time by 2 to imply compounding "half" a time per year
#Hence, although the actual number of years is 20, use 10 (= 20/2)]
CAGR_formula_NHANES <- function(EndVal, StartVal, years = 10) {
  values <- ((EndVal/StartVal)^(1/years) -1)
  return(values)
}

CAGR_NA <- mutate(CAGR3NA,
                  CAGR_Perc = CAGR_formula_NHANES(StartVal = Start, EndVal = End, years = 10)*100)

#BRFSS
#CAGR age
B.age <- B.overall[ which(B.overall$Category == "Age"), ]
B.CAGR.age <- B.age[B.age$Year %in% c("2001", "2020"), ]
B.CAGR.age$Year[B.CAGR.age$Year == "2001"] <- "Start"
B.CAGR.age$Year[B.CAGR.age$Year == "2020"] <- "End"
B.CAGR.age <- subset(B.CAGR.age, select = c("Dem_group", "Proportion", "Year"))

#CAGR age numbers
B.NO.age <- B.NO.overall[ which(B.NO.overall$Category == "Age"), ]
B.CAGR.age.no <- B.NO.age[B.NO.age$Year %in% c("2001", "2020"), ]
B.CAGR.age.no$Year[B.CAGR.age.no$Year == "2001"] <- "Start"
B.CAGR.age.no$Year[B.CAGR.age.no$Year == "2020"] <- "End"
B.CAGR.age.no <- subset(B.CAGR.age.no, select = c("Dem_group", "Number.of.People", "Year"))

#merge
B.CAGR.no.t <- merge(B.CAGR.age, B.CAGR.age.no)
B.CAGR.no2 <- gather(B.CAGR.no.t, key = "metric", value = "amount", 3:4)
CAGR3Bno <- spread(B.CAGR.no2, key = Year, value = amount)

CAGR_B.age <- mutate(CAGR3Bno,
                     CAGR_Perc = CAGR_formula_BRFSS(StartVal = Start, EndVal = End, years = 20)*100)
summary(CAGR_B.age)

#NHIS
#CAGR age
N.Age <- N.overall[ which(N.overall$Category == "Age"), ]
N.CAGR.age <- N.Age[N.Age$Year %in% c("2002", "2018"), ]
N.CAGR.age$Year[N.CAGR.age$Year == "2002"] <- "Start"
N.CAGR.age$Year[N.CAGR.age$Year == "2018"] <- "End"
N.CAGR.age <- subset(N.CAGR.age, select = c("Dem_group", "Proportion", "Year"))

#CAGR age numbers
N.NO.Age <- N.NO.overall[ which(N.NO.overall$Category == "Age"), ]
N.CAGR.age.no <- N.NO.Age[N.NO.Age$Year %in% c("2002", "2018"), ]
N.CAGR.age.no$Year[N.CAGR.age.no$Year == "2002"] <- "Start"
N.CAGR.age.no$Year[N.CAGR.age.no$Year == "2018"] <- "End"
N.CAGR.age.no <- subset(N.CAGR.age.no, select = c("Dem_group", "Number.of.People", "Year"))

#merge
N.CAGR.no.t <- merge(N.CAGR.age, N.CAGR.age.no)
N.CAGR.no2 <- gather(N.CAGR.no.t, key = "metric", value = "amount", 3:4)
CAGR3Nno <- spread(N.CAGR.no2, key = Year, value = amount)


CAGR_N.age <- mutate(CAGR3Nno,
                     CAGR_Perc = CAGR_formula_NHIS(StartVal = Start, EndVal = End, years = 17)*100)
summary(CAGR_N.age)

#NHANES
#CAGR age
NA.age <- NA.overall[ which(NA.overall$Category == "Age"), ]
NA.CAGR.age <- NA.age[NA.age$Year %in% c("2000", "2018"), ]
NA.CAGR.age$Year[NA.CAGR.age$Year == "2000"] <- "Start"
NA.CAGR.age$Year[NA.CAGR.age$Year == "2018"] <- "End"
NA.CAGR.age <- subset(NA.CAGR.age, select = c("Dem_group", "Proportion", "Year"))

#CAGR age numbers
NA.NO.age <- NA.NO.overall[ which(NA.NO.overall$Category == "Age"), ]
NA.CAGR.age.no <- NA.NO.age[NA.NO.age$Year %in% c("2000", "2018"), ]
NA.CAGR.age.no$Year[NA.CAGR.age.no$Year == "2000"] <- "Start"
NA.CAGR.age.no$Year[NA.CAGR.age.no$Year == "2018"] <- "End"
NA.CAGR.age.no <- subset(NA.CAGR.age.no, select = c("Dem_group", "Number.of.People", "Year"))

#merge
NA.CAGR.no.t <- merge(NA.CAGR.age, NA.CAGR.age.no)
NA.CAGR.no2 <- gather(NA.CAGR.no.t, key = "metric", value = "amount", 3:4)
CAGR3NAno <- spread(NA.CAGR.no2, key = Year, value = amount)


CAGR_NA.age <- mutate(CAGR3NAno,
                      CAGR_Perc = CAGR_formula_NHIS(StartVal = Start, EndVal = End, years = 17)*100)
summary(CAGR_NA.age)

#BRFSS
#CAGR sex
B.sex <- B.overall[ which(B.overall$Category == "Sex"), ]
B.CAGR.sex <- B.sex[B.sex$Year %in% c("2001", "2020"), ]
B.CAGR.sex$Year[B.CAGR.sex$Year == "2001"] <- "Start"
B.CAGR.sex$Year[B.CAGR.sex$Year == "2020"] <- "End"
B.CAGR.sex <- subset(B.CAGR.sex, select = c("Dem_group", "Proportion", "Year"))

#CAGR sex numbers
B.NO.sex <- B.NO.overall[ which(B.NO.overall$Category == "Sex"), ]
B.CAGR.sex.no <- B.NO.sex[B.NO.sex$Year %in% c("2001", "2020"), ]
B.CAGR.sex.no$Year[B.CAGR.sex.no$Year == "2001"] <- "Start"
B.CAGR.sex.no$Year[B.CAGR.sex.no$Year == "2020"] <- "End"
B.CAGR.sex.no <- subset(B.CAGR.sex.no, select = c("Dem_group", "Number.of.People", "Year"))

#merge
B.CAGR.no.t <- merge(B.CAGR.sex, B.CAGR.sex.no)
B.CAGR.no2 <- gather(B.CAGR.no.t, key = "metric", value = "amount", 3:4)
CAGR3Bno <- spread(B.CAGR.no2, key = Year, value = amount)


CAGR_B.sex <- mutate(CAGR3Bno,
                     CAGR_Perc = CAGR_formula_BRFSS(StartVal = Start, EndVal = End, years = 20)*100)
summary(CAGR_B.sex)

#NHANES
#CAGR sex
NA.sex <- NA.overall[ which(NA.overall$Category == "Sex"), ]
NA.CAGR.sex <- NA.sex[NA.sex$Year %in% c("2000", "2018"), ]
NA.CAGR.sex$Year[NA.CAGR.sex$Year == "2000"] <- "Start"
NA.CAGR.sex$Year[NA.CAGR.sex$Year == "2018"] <- "End"
NA.CAGR.sex <- subset(NA.CAGR.sex, select = c("Dem_group", "Proportion", "Year"))

#CAGR sex numbers
NA.NO.sex <- NA.NO.overall[ which(NA.NO.overall$Category == "Sex"), ]
NA.CAGR.sex.no <- NA.NO.sex[NA.NO.sex$Year %in% c("2000", "2018"), ]
NA.CAGR.sex.no$Year[NA.CAGR.sex.no$Year == "2000"] <- "Start"
NA.CAGR.sex.no$Year[NA.CAGR.sex.no$Year == "2018"] <- "End"
NA.CAGR.sex.no <- subset(NA.CAGR.sex.no, select = c("Dem_group", "Number.of.People", "Year"))

#merge
NA.CAGR.no.t <- merge(NA.CAGR.sex, NA.CAGR.sex.no)
NA.CAGR.no2 <- gather(NA.CAGR.no.t, key = "metric", value = "amount", 3:4)
CAGR3NAno <- spread(NA.CAGR.no2, key = Year, value = amount)


CAGR_NA.sex <- mutate(CAGR3NAno,
                      CAGR_Perc = CAGR_formula_NHANES(StartVal = Start, EndVal = End, years = 10)*100)
summary(CAGR_NA.sex)

#NHIS
#CAGR sex
N.sex <- N.overall[ which(N.overall$Category == "Sex"), ]
N.CAGR.sex <- N.sex[N.sex$Year %in% c("2002", "2018"), ]
N.CAGR.sex$Year[N.CAGR.sex$Year == "2002"] <- "Start"
N.CAGR.sex$Year[N.CAGR.sex$Year == "2018"] <- "End"
N.CAGR.sex <- subset(N.CAGR.sex, select = c("Dem_group", "Proportion", "Year"))

#CAGR sex numbers
N.NO.sex <- N.NO.overall[ which(N.NO.overall$Category == "Sex"), ]
N.CAGR.sex.no <- N.NO.sex[N.NO.sex$Year %in% c("2002", "2018"), ]
N.CAGR.sex.no$Year[N.CAGR.sex.no$Year == "2002"] <- "Start"
N.CAGR.sex.no$Year[N.CAGR.sex.no$Year == "2018"] <- "End"
N.CAGR.sex.no <- subset(N.CAGR.sex.no, select = c("Dem_group", "Number.of.People", "Year"))

#merge
N.CAGR.no.t <- merge(N.CAGR.sex, N.CAGR.sex.no)
N.CAGR.no2 <- gather(N.CAGR.no.t, key = "metric", value = "amount", 3:4)
CAGR3Nno <- spread(N.CAGR.no2, key = Year, value = amount)


CAGR_N.sex <- mutate(CAGR3Nno,
                     CAGR_Perc = CAGR_formula_NHIS(StartVal = Start, EndVal = End, years = 17)*100)
summary(CAGR_N.sex)

#cagr bmi props
#brfss
B.BMI <- B.overall[ which(B.overall$Category == "BMI"), ]
B.CAGR.BMI <- B.BMI[B.BMI$Year %in% c("2001", "2020"), ]
B.CAGR.BMI$Year[B.CAGR.BMI$Year == "2001"] <- "Start"
B.CAGR.BMI$Year[B.CAGR.BMI$Year == "2020"] <- "End"
B.CAGR.BMI <- subset(B.CAGR.BMI, select = c("Dem_group", "Proportion", "Year"))

B.NO.BMI <- B.NO.overall[ which(B.NO.overall$Category == "BMI"), ]
B.CAGR.BMI.no <- B.NO.BMI[B.NO.BMI$Year %in% c("2001", "2020"), ]
B.CAGR.BMI.no$Year[B.CAGR.BMI.no$Year == "2001"] <- "Start"
B.CAGR.BMI.no$Year[B.CAGR.BMI.no$Year == "2020"] <- "End"
B.CAGR.BMI.no <- subset(B.CAGR.BMI.no, select = c("Dem_group", "Number.of.People", "Year"))

#merge
B.CAGR.BMI.t <- merge(B.CAGR.BMI, B.CAGR.BMI.no)
B.CAGR.BMI2 <- gather(B.CAGR.BMI.t, key = "metric", value = "amount", 3:4)
B.CAGR.BMI <- spread(B.CAGR.BMI2, key = Year, value = amount)

CAGR_B.BMI <- mutate(B.CAGR.BMI,
                     CAGR_Perc = CAGR_formula_BRFSS(StartVal = Start, EndVal = End, years = 20)*100)

#nhis
N.BMI <- N.overall[ which(N.overall$Category == "BMI"), ]
N.CAGR.BMI <- N.BMI[N.BMI$Year %in% c("2002", "2018"), ]
N.CAGR.BMI$Year[N.CAGR.BMI$Year == "2002"] <- "Start"
N.CAGR.BMI$Year[N.CAGR.BMI$Year == "2018"] <- "End"
N.CAGR.BMI <- subset(N.CAGR.BMI, select = c("Dem_group", "Proportion", "Year"))

N.NO.BMI <- N.NO.overall[ which(N.NO.overall$Category == "BMI"), ]
N.CAGR.BMI.no <- N.NO.BMI[N.NO.BMI$Year %in% c("2002", "2018"), ]
N.CAGR.BMI.no$Year[N.CAGR.BMI.no$Year == "2002"] <- "Start"
N.CAGR.BMI.no$Year[N.CAGR.BMI.no$Year == "2018"] <- "End"
N.CAGR.BMI.no <- subset(N.CAGR.BMI.no, select = c("Dem_group", "Number.of.People", "Year"))

#merge
N.CAGR.BMI.t <- merge(N.CAGR.BMI, N.CAGR.BMI.no)
N.CAGR.BMI2 <- gather(N.CAGR.BMI.t, key = "metric", value = "amount", 3:4)
N.CAGR.BMI <- spread(N.CAGR.BMI2, key = Year, value = amount)

CAGR_N.BMI <- mutate(N.CAGR.BMI,
                     CAGR_Perc = CAGR_formula_NHIS(StartVal = Start, EndVal = End, years = 17)*100)


#nhanes
NA.BMI <- NA.overall[ which(NA.overall$Category == "BMI"), ]
NA.CAGR.BMI <- NA.BMI[NA.BMI$Year %in% c("2000", "2018"), ]
NA.CAGR.BMI$Year[NA.CAGR.BMI$Year == "2000"] <- "Start"
NA.CAGR.BMI$Year[NA.CAGR.BMI$Year == "2018"] <- "End"
NA.CAGR.BMI <- subset(NA.CAGR.BMI, select = c("Dem_group", "Proportion", "Year"))

NA.NO.BMI <- NA.NO.overall[ which(NA.NO.overall$Category == "BMI"), ]
NA.CAGR.BMI.no <- NA.NO.BMI[NA.NO.BMI$Year %in% c("2000", "2018"), ]
NA.CAGR.BMI.no$Year[NA.CAGR.BMI.no$Year == "2000"] <- "Start"
NA.CAGR.BMI.no$Year[NA.CAGR.BMI.no$Year == "2018"] <- "End"
NA.CAGR.BMI.no <- subset(NA.CAGR.BMI.no, select = c("Dem_group", "Number.of.People", "Year"))

#merge
NA.CAGR.BMI.t <- merge(NA.CAGR.BMI, NA.CAGR.BMI.no)
NA.CAGR.BMI2 <- gather(NA.CAGR.BMI.t, key = "metric", value = "amount", 3:4)
NA.CAGR.BMI <- spread(NA.CAGR.BMI2, key = Year, value = amount)

CAGR_NA.BMI <- mutate(NA.CAGR.BMI,
                      CAGR_Perc = CAGR_formula_NHANES(StartVal = Start, EndVal = End, years = 10)*100)

