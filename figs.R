#Make figures

#Packages
library(tidyverse)
library(ggplot2)

#----Overall----
#--percentage----
B.overall <- read.csv(url("https://www.dropbox.com/s/0gt96oerc4awtae/B.overall.csv?dl=1"))
N.overall <- read.csv(url("https://www.dropbox.com/s/hyui4j0xrwa56fe/N.overall.csv?dl=1"))
NA.overall <- read.csv(url("https://www.dropbox.com/s/2ndo9z4w87ajwp9/NA.overall.csv?dl=1"))

BRFSS_years <- c(2001, 2003, 2005, 2007, 2009, 2011:2020)

#- == OVERALL PREVALENCE FIGURE == ----

#merge overalls
#first add survey column to each
B.overall$Survey <- c("BRFSS")
B.overall <- B.overall |> filter(Year %in% BRFSS_years)
N.overall$Survey <- c("NHIS")
NA.overall$Survey <- c("NHANES")

Overall <- Reduce(function(x, y) merge(x, y, all=TRUE), list(B.overall, N.overall, NA.overall))
Overall$Year <- as.numeric(Overall$Year)

#Figure 1A (overall prevalence)
fig1a <- ggplot(data = Overall,
                aes(x = Year, y = Proportion, fill = Survey, colour = Survey)) +
  geom_line(size = 0.5) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = CI_Prop_low, ymax = CI_Prop_upp, fill = Survey, linetype = NA), alpha = 0.3) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 25, 50, 75, 100), limits = c(0, 100)) +
  scale_x_continuous(name = "Survey cycle (year)", breaks = seq(2000, 2020, 4), limits = c(2000, 2020)) +
  scale_colour_manual(values = c("aquamarine4", "goldenrod3", "midnightblue")) +
  scale_fill_manual(values = c("aquamarine4", "goldenrod3", "midnightblue")) +
  guides(colour = guide_legend(ncol = 3)) +
  ggtitle("1A: Prevalence (95% CI) of arthritis / joint pain cases") +
  theme_bw(base_size = 20, base_family = 'serif') +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = 20),
        axis.line = element_line(colour = "black", size = 0.5),
        axis.ticks = element_line(colour = "black", size = 0.5),
        axis.text = element_text(colour = 'black'),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = c(0.5, 0.95)); fig1a

ggsave("Figure1A.tiff", width = 200, height = 150, units = "mm", dpi = 300)

#--number----

B.NO.overall <- read.csv(url("https://www.dropbox.com/s/5ur6y720qkmxibd/B.NO.overall.csv?dl=1"))
N.NO.overall <- read.csv(url("https://www.dropbox.com/s/vosmookjgmem7v4/N.NO.overall.csv?dl=1"))
NA.NO.overall <- read.csv(url("https://www.dropbox.com/s/irme0unkqoo5exe/NA.NO.overall.csv?dl=1"))

#- == OVERALL NUMBERS FIGURE == ----

B.NO.overall$Survey <- c("BRFSS")
B.NO.overall <- B.NO.overall |> filter(Year %in% BRFSS_years)
N.NO.overall$Survey <- c("NHIS")
NA.NO.overall$Survey <- c("NHANES")

Overall.NO <- Reduce(function(x, y) merge(x, y, all=TRUE), list(B.NO.overall, N.NO.overall, NA.NO.overall))
Overall.NO$Year <- as.numeric(Overall.NO$Year)

pop_2000 <- c(Number.of.People = 209128094,
              CI_Prop_low = NA,
              CI_Prop_upp = NA,
              Year = 2000,
              Dem_group = "US population",
              Category = "Overall",
              Survey = "United States")
pop_2010 <- c(Number.of.People = 234564071,
              CI_Prop_low = NA,
              CI_Prop_upp = NA,
              Year = 2010,
              Dem_group = "US population",
              Category = "Overall",
              Survey = "United States")
pop_2020 <- c(Number.of.People = 258343281,
              CI_Prop_low = NA,
              CI_Prop_upp = NA,
              Year = 2020,
              Dem_group = "US population",
              Category = "Overall",
              Survey = "United States")

Overall.NO <- rbind(Overall.NO[, -1], pop_2000)
Overall.NO <- rbind(Overall.NO, pop_2010)
Overall.NO <- rbind(Overall.NO, pop_2020)

Overall.NO$Number.of.People <- as.numeric(Overall.NO$Number.of.People)
Overall.NO$CI_Prop_low <- as.numeric(Overall.NO$CI_Prop_low)
Overall.NO$CI_Prop_upp <- as.numeric(Overall.NO$CI_Prop_upp)
Overall.NO$Year <- as.numeric(Overall.NO$Year)

Overall.NO <- Overall.NO %>%
  mutate(Number.of.People = Number.of.People/1000000) %>%
  mutate(CI_Prop_low = CI_Prop_low/1000000) %>%
  mutate(CI_Prop_upp = CI_Prop_upp/1000000)

class(Overall.NO$Number.of.People)
class(Overall.NO$CI_Prop_low)
class(Overall.NO$CI_Prop_upp)
class(Overall.NO$Year)

#Plot overall growth of arthritis versus overall population
fig1b <- ggplot(data = Overall.NO,
                aes(x = Year, y = Number.of.People, fill = Survey, colour = Survey)) +
  geom_line(size = 0.5) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = CI_Prop_low, ymax = CI_Prop_upp, fill = Survey, linetype = NA), alpha = 0.3) +
  scale_y_continuous(name = "Number of people (in millions)", limits = c(25, 300), breaks = c(50, 100, 150, 200, 250, 300)) +
  scale_x_continuous(name = "Survey cycle (year)", breaks = seq(2000, 2020, 4), limits = c(2000, 2020)) +
  scale_colour_manual(values = c("aquamarine4", "goldenrod3", "midnightblue", "black"),
                      labels = c('BRFSS', 'NHANES', 'NHIS', 'Population of USA')) +
  scale_fill_manual(values = c("aquamarine4", "goldenrod3", "midnightblue", "black"),
                    labels = c('BRFSS', 'NHANES', 'NHIS', 'Population of USA')) +
  guides(colour = guide_legend(ncol = 4)) +
  ggtitle("1B: Number (95% CI) of arthritis / joint pain cases") +
  theme_bw(base_size = 20, base_family = 'serif') +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = 20),
        axis.line = element_line(colour = "black", size = 0.5),
        axis.ticks = element_line(colour = "black", size = 0.5),
        axis.text = element_text(colour = 'black'),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = c(0.5, 0.95)); fig1b

ggsave("Figure1B.tiff", width = 200, height = 150, units = "mm", dpi = 300)

#----Age----
#--percentage----
B.age <- read.csv(url("https://www.dropbox.com/s/5y3fe6s46ih7mvf/B.age.csv?dl=1"))
N.Age <- read.csv(url("https://www.dropbox.com/s/r3x1ovzbongkaie/N.age.csv?dl=1"))
NA.age <- read.csv(url("https://www.dropbox.com/s/i3gp10g22t29l4v/NA.age.csv?dl=1"))

#- == AGE PREVALENCE FIGURE == ----

#merge overalls
#first add survey column to each
B.age$Survey <- c("BRFSS")
B.age <- B.age |> filter(Year %in% BRFSS_years)
N.Age$Survey <- c("NHIS")
NA.age$Survey <- c("NHANES")

Age <- Reduce(function(x, y) merge(x, y, all=TRUE), list(B.age, N.Age, NA.age))
Age$Year <- as.numeric(Age$Year)

Age$Dem_group <- gsub(x = Age$Dem_group, pattern = 'Age ', replacement = '')
Age$Dem_group[Age$Dem_group == "18 to 24"] <- "18 to 24 years"
Age$Dem_group[Age$Dem_group == "20 to 24"] <- "18 to 24 years"
Age$Dem_group[Age$Dem_group == "25 to 29"] <- "25 to 29 years"
Age$Dem_group[Age$Dem_group == "30 to 34"] <- "30 to 34 years"
Age$Dem_group[Age$Dem_group == "35 to 39"] <- "35 to 39 years"
Age$Dem_group[Age$Dem_group == "40 to 44"] <- "40 to 44 years"
Age$Dem_group[Age$Dem_group == "45 to 49"] <- "45 to 49 years"
Age$Dem_group[Age$Dem_group == "50 to 54"] <- "50 to 54 years"
Age$Dem_group[Age$Dem_group == "55 to 59"] <- "55 to 59 years"
Age$Dem_group[Age$Dem_group == "60 to 64"] <- "60 to 64 years"
Age$Dem_group[Age$Dem_group == "65 to 69"] <- "65 to 69 years"
Age$Dem_group[Age$Dem_group == "70 and above"] <- "70 years and above"

#plot
fig2a <- ggplot(data = Age,
                aes(x = Year, y = Proportion, fill = Survey, colour = Survey)) +
  geom_line(size = 0.3) +
  geom_point(size = 1.2) +
  geom_ribbon(aes(ymin = CI_Prop_low, ymax = CI_Prop_upp, fill = Survey, linetype = NA), alpha = 0.3) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 25, 50, 75, 100), limits = c(0, 100)) +
  scale_colour_manual(values = c("aquamarine4", "goldenrod3", "midnightblue", "black")) +
  scale_fill_manual(values = c("aquamarine4", "goldenrod3", "midnightblue", "black")) +
  scale_x_continuous(name = "Survey cycle (year)", breaks = seq(2000, 2020, 6), limits = c(2000, 2020)) +
  facet_wrap(facets = ~Dem_group, ncol = 4) +
  guides(colour = guide_legend(ncol = 1)) +
  ggtitle("2A: Prevalence (95% CI) of arthritis / joint pain cases by age") +
  theme_bw(base_size = 16, base_family = 'serif') +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = 16),
        axis.line = element_line(colour = "black", size = 0.5),
        axis.ticks = element_line(colour = "black", size = 0.5),
        axis.text = element_text(colour = 'black'),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.margin = margin(),
        legend.position = c(0.9, 0.18)); fig2a

ggsave("Figure2A.tiff", width = 200, height = 150, units = "mm", dpi = 300)


#--number----
B.NO.age <- read.csv(url("https://www.dropbox.com/s/c804ztyzp5p2fms/B.NO.age.csv?dl=1"))
N.NO.Age <- read.csv(url("https://www.dropbox.com/s/a8fbaxi61glkkma/N.NO.age.csv?dl=1"))
NA.NO.age <- read.csv(url("https://www.dropbox.com/s/ozzeskll6kjyjsv/NA.NO.age.csv?dl=1"))

#- == AGE NUMBERS FIGURE == ----

B.NO.age$Survey <- c("BRFSS")
B.NO.age <- B.NO.age |> filter(Year %in% BRFSS_years)
N.NO.Age$Survey <- c("NHIS")
NA.NO.age$Survey <- c("NHANES")

Age.NO <- bind_rows(B.NO.age, N.NO.Age, NA.NO.age) |>
    select(-X)

Age.NO$Dem_group <- gsub(x = Age.NO$Dem_group, pattern = 'Age ', replacement = '')
Age.NO$Dem_group[Age.NO$Dem_group == "18 to 24"] <- "18 to 24 years"
Age.NO$Dem_group[Age.NO$Dem_group == "20 to 24"] <- "18 to 24 years"
Age.NO$Dem_group[Age.NO$Dem_group == "25 to 29"] <- "25 to 29 years"
Age.NO$Dem_group[Age.NO$Dem_group == "30 to 34"] <- "30 to 34 years"
Age.NO$Dem_group[Age.NO$Dem_group == "35 to 39"] <- "35 to 39 years"
Age.NO$Dem_group[Age.NO$Dem_group == "40 to 44"] <- "40 to 44 years"
Age.NO$Dem_group[Age.NO$Dem_group == "45 to 49"] <- "45 to 49 years"
Age.NO$Dem_group[Age.NO$Dem_group == "50 to 54"] <- "50 to 54 years"
Age.NO$Dem_group[Age.NO$Dem_group == "55 to 59"] <- "55 to 59 years"
Age.NO$Dem_group[Age.NO$Dem_group == "60 to 64"] <- "60 to 64 years"
Age.NO$Dem_group[Age.NO$Dem_group == "65 to 69"] <- "65 to 69 years"
Age.NO$Dem_group[Age.NO$Dem_group == "70 and above"] <- "70 years and above"


#adjust age labels for compatibility
Age.NO <- Age.NO %>%
  rename(Number.People = Number.of.People) |>
  mutate(Number.People = Number.People/1000000) %>%
  mutate(CI_Prop_low = CI_Prop_low/1000000) %>%
  mutate(CI_Prop_upp = CI_Prop_upp/1000000)

pop_2000 <- data.frame(Number.People = c(27.1, 19.4, 20.5, 22.7, 22.4, 20.1, 17.6, 13.5, 10.8, 9.5, 25.5),
                       CI_Prop_low = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                       CI_Prop_upp = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                       Year = c(2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000),
                       Dem_group = c("18 to 24 years", "25 to 29 years", "30 to 34 years", "35 to 39 years", "40 to 44 years", "45 to 49 years", "50 to 54 years", "55 to 59 years", "60 to 64 years", "65 to 69 years", "70 years and above"),
                       Category = c("Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age"),
                       Survey = c("United States", "United States", "United States", "United States", "United States", "United States", "United States", "United States", "United States", "United States", "United States"))

pop_2010 <- data.frame(Number.People = c(30.7, 21.1, 20.0, 20.2, 20.9, 22.7, 22.3, 19.7, 16.8, 12.4, 27.8),
                       CI_Prop_low = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                       CI_Prop_upp = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                       Year = c(2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010, 2010),
                       Dem_group = c("18 to 24 years", "25 to 29 years", "30 to 34 years", "35 to 39 years", "40 to 44 years", "45 to 49 years", "50 to 54 years", "55 to 59 years", "60 to 64 years", "65 to 69 years", "70 years and above"),
                       Category = c("Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age"),
                       Survey = c("United States", "United States", "United States", "United States", "United States", "United States", "United States", "United States", "United States", "United States", "United States"))

pop_2020 <- data.frame(Number.People = c(25.8, 18.1, 22.9, 22.2, 20.7, 20.3, 20.8, 22.0, 21.0, 18.0, 36.4),
                       CI_Prop_low = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                       CI_Prop_upp = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                       Year = c(2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020),
                       Dem_group = c("18 to 24 years", "25 to 29 years", "30 to 34 years", "35 to 39 years", "40 to 44 years", "45 to 49 years", "50 to 54 years", "55 to 59 years", "60 to 64 years", "65 to 69 years", "70 years and above"),
                       Category = c("Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age"),
                       Survey = c("United States", "United States", "United States", "United States", "United States", "United States", "United States", "United States", "United States", "United States", "United States"))

Age.NO <- Reduce(function(x, y) merge(x, y, all=TRUE), list(Age.NO, pop_2000, pop_2010, pop_2020))


fig2b <- ggplot(data = Age.NO,
                aes(x = Year, y = Number.People, fill = Survey, colour = Survey)) +
  geom_line(size = 0.3) +
  geom_point(size = 1.2) +
  geom_ribbon(aes(ymin = CI_Prop_low, ymax = CI_Prop_upp, fill = Survey, linetype = NA), alpha = 0.3) +
  scale_y_continuous(name = "Number of people (in millions)") +
  scale_colour_manual(values = c("aquamarine4", "goldenrod3", "midnightblue", "black"),
                      labels = c('BRFSS', 'NHANES', 'NHIS', 'Population of USA')) +
  scale_fill_manual(values = c("aquamarine4", "goldenrod3", "midnightblue", "black"),
                    labels = c('BRFSS', 'NHANES', 'NHIS', 'Population of USA')) +
  scale_x_continuous(name = "Survey cycle (year)", breaks = seq(2000, 2020, 6), limits = c(2000, 2020)) +
  facet_wrap(facets = ~Dem_group, ncol = 4) +
  guides(colour = guide_legend(ncol = 1)) +
  ggtitle("2B: Number (95% CI) of arthritis / joint pain cases by age") +
  theme_bw(base_size = 16, base_family = 'serif') +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = 16),
        axis.line = element_line(colour = "black", size = 0.5),
        axis.ticks = element_line(colour = "black", size = 0.5),
        axis.text = element_text(colour = 'black'),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.margin = margin(),
        legend.position = c(0.9, 0.18)); fig2b

ggsave("Figure2B.tiff", width = 200, height = 150, units = "mm", dpi = 300)

#----Sex----
#--percentage----

B.sex <- read.csv(url("https://www.dropbox.com/s/msr0ckevqzzebu7/B.sex.csv?dl=1"))
N.sex <- read.csv(url("https://www.dropbox.com/s/hz54tam1macu7j6/N.sex.csv?dl=1"))
NA.sex <- read.csv(url("https://www.dropbox.com/s/fxeo15d1nacngxp/NA.sex.csv?dl=1"))


#- == SEX PREVALENCE FIGURE == ----

#merge overalls
#first add survey column to each
B.sex$Survey <- c("BRFSS")
N.sex$Survey <- c("NHIS")
NA.sex$Survey <- c("NHANES")

Sex <- Reduce(function(x, y) merge(x, y, all=TRUE), list(B.sex, N.sex, NA.sex))
Sex$Year <- as.numeric(Sex$Year)

pop_2000 <- data.frame(Proportion = c(49.1, 50.9),
                       `CI_Prop_low` = c(NA, NA),
                       `CI_Prop_upp` = c(NA, NA),
                       Year = c(2000, 2000),
                       Dem_group = c("Male", "Female"),
                       Category = c("Sex", "Sex"),
                       Survey = c("United States", "United States"))

pop_2010 <- data.frame(Proportion = c(49.2, 50.8),
                       `CI_Prop_low` = c(NA, NA),
                       `CI_Prop_upp` = c(NA, NA),
                       Year = c(2010, 2010),
                       Dem_group = c("Male", "Female"),
                       Category = c("Sex", "Sex"),
                       Survey = c("United States", "United States"))


pop_2020 <- data.frame(Proportion = c(49.5, 50.5),
                       `CI_Prop_low` = c(NA, NA),
                       `CI_Prop_upp` = c(NA, NA),
                       Year = c(2020, 2020),
                       Dem_group = c("Male", "Female"),
                       Category = c("Sex", "Sex"),
                       Survey = c("United States", "United States"))

Sex <- Reduce(function(x, y) merge(x, y, all=TRUE), list(Sex, pop_2000, pop_2010, pop_2020))

#plot
fig3a <- ggplot(data = Sex,
                aes(x = Year, y = Proportion, group = Survey, fill = Survey, colour = Survey)) +
  geom_line(size = 0.2) +
  geom_point(size = 0.3) +
  geom_ribbon(aes(ymin = Sex$CI_Prop_low, ymax = Sex$CI_Prop_upp, fill = Survey, linetype = NA), alpha = 0.2) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 25, 50, 75, 100), limits = c(0, 100)) +
  scale_colour_manual(values = c("aquamarine4", "goldenrod3", "midnightblue", "black")) +
  scale_fill_manual(values = c("aquamarine4", "goldenrod3", "midnightblue", "black")) +
  scale_x_continuous(name = "Survey cycles (years)", breaks = seq(2000, 2020, 4), limits = c(2000, 2020)) +
  facet_wrap(facets = ~Dem_group, ncol = 4) +
  theme_bw() +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(text=element_text(family = "myfont", size = 10)) +
  xlab("Survey cycles (years)") +
  ggtitle("3A") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 8, family = "myfont")) +
  theme(plot.title = element_text(size = 12, face = "bold", family = "myfont"),
        legend.title = element_text(size = 10, face = "bold", family = "myfont"),
        legend.text = element_text(size = 10, family = "myfont"),
        legend.position = "bottom")
ggsave("Figure3A.tiff", width = 150, height = 100, units = "mm", device = "tiff", dpi = 500)


#--number----

B.NO.sex <- read.csv(url("https://www.dropbox.com/s/oapc5cbn7jxoqrd/B.NO.sex.csv?dl=1"))
N.NO.sex <- read.csv(url("https://www.dropbox.com/s/dyvopgggktplu8t/N.NO.sex.csv?dl=1"))
NA.NO.sex <- read.csv(url("https://www.dropbox.com/s/6yhvmm5sx6fg0dz/NA.NO.sex.csv?dl=1"))

#- == SEX NUMBERS FIGURE == ----

B.NO.sex$Survey <- c("BRFSS")
N.NO.sex$Survey <- c("NHIS")
NA.NO.sex$Survey <- c("NHANES")
NA.NO.sex$Dem_group <- as.character(NA.NO.sex$Dem_group)
Sex.NO <- Reduce(function(x, y) merge(x, y, all=TRUE), list(B.NO.sex, N.NO.sex, NA.NO.sex))
Sex.NO$Year <- as.numeric(Sex.NO$Year)
names(Sex.NO)[names(Sex.NO) == "Number of People"] <- "Number.People"


Sex.NO <- Sex.NO %>%
  mutate(Number.People = Number.People/1000000) %>%
  mutate(CI_Prop_low = CI_Prop_low/1000000) %>%
  mutate(CI_Prop_upp = CI_Prop_upp/1000000)

pop_2000 <- data.frame(Number.People = c(138.1, 143.4),
                       `CI_Prop_low` = c(NA, NA),
                       `CI_Prop_upp` = c(NA, NA),
                       Year = c(2000, 2000),
                       Dem_group = c("Male", "Female"),
                       Category = c("Sex", "Sex"),
                       Survey = c("United States", "United States"))

pop_2010 <- data.frame(Number.People = c(151.8, 157.0),
                       `CI_Prop_low` = c(NA, NA),
                       `CI_Prop_upp` = c(NA, NA),
                       Year = c(2010, 2010),
                       Dem_group = c("Male", "Female"),
                       Category = c("Sex", "Sex"),
                       Survey = c("United States", "United States"))

pop_2020 <- data.frame(Number.People = c(164.2, 167.3),
                       `CI_Prop_low` = c(NA, NA),
                       `CI_Prop_upp` = c(NA, NA),
                       Year = c(2020, 2020),
                       Dem_group = c("Male", "Female"),
                       Category = c("Sex", "Sex"),
                       Survey = c("United States", "United States"))

Sex.NO <- Reduce(function(x, y) merge(x, y, all=TRUE), list(Sex.NO, pop_2000, pop_2010, pop_2020))


fig3b <- ggplot(data = Sex.NO,
                aes(x = Year, y = Number.People, group = Survey, fill = Survey, colour = Survey)) +
  geom_line(size = 0.2) +
  geom_point(size = 0.3) +
  geom_ribbon(aes(ymin = Sex.NO$CI_Prop_low, ymax = Sex.NO$CI_Prop_upp, fill = Survey, linetype = NA), alpha = 0.2) +
  scale_y_continuous(name = "Number of people affected (in millions)") +
  scale_colour_manual(values = c("aquamarine4", "goldenrod3", "midnightblue", "black")) +
  scale_fill_manual(values = c("aquamarine4", "goldenrod3", "midnightblue", "black")) +
  scale_x_continuous(name = "Survey cycles (years)", breaks = seq(2000, 2020, 4), limits = c(2000, 2020)) +
  facet_wrap(facets = ~Dem_group, ncol = 4) +
  theme_bw() +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(text=element_text(family = "myfont", size = 10)) +
  xlab("Survey cycles (years)") +
  ggtitle("3B") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 8, family = "myfont")) +
  theme(plot.title = element_text(size = 12, face = "bold", family = "myfont"),
        legend.title = element_text(size = 10, face = "bold", family = "myfont"),
        legend.text = element_text(size = 10, family = "myfont"),
        legend.position = "bottom")
ggsave("Figure3B.tiff", width = 150, height = 100, units = "mm", device = "tiff", dpi = 500)


#----BMI----
# the prevalence of people in each weight category nationally, and the number of people in each weight category nationally,
# are calculated in 2000, 2010, 2020 using the BRFSS, NHIS and NHANES percentages (prevalence), and then applied to the
# national numbers to obtain the number of people.

#--percentage----

B.BMI <- read.csv(url("https://www.dropbox.com/s/9taeil9a7ymd38u/B.BMI.csv?dl=1"))
N.BMI <- read.csv(url("https://www.dropbox.com/s/23daxh29apzpam2/N.BMI.csv?dl=1"))
NA.BMI <- read.csv(url("https://www.dropbox.com/s/vfuyksioocfaiip/NA.BMI.csv?dl=1"))


#- == BMI PREVALENCE FIGURE == -----

#merge overalls
#first add survey column to each
B.BMI$Survey <- c("BRFSS")
N.BMI$Survey <- c("NHIS")
NA.BMI$Survey <- c("NHANES")

BMI <- Reduce(function(x, y) merge(x, y, all=TRUE), list(B.BMI, N.BMI, NA.BMI))
BMI$Year <- as.numeric(BMI$Year)

pop_2000 <- data.frame(Proportion = c(37.5, 2.15, 35.35, 25.05),
                       `CI_Prop_low` = c(NA, NA, NA, NA),
                       `CI_Prop_upp` = c(NA, NA, NA, NA),
                       Year = c(2000, 2000, 2000, 2000),
                       Dem_group = c("Healthy weight", "Underweight", "Overweight", "Obese"),
                       Category = c("BMI", "BMI", "BMI", "BMI"),
                       Survey = c("United States", "United States", "United States", "United States"))

pop_2010 <- data.frame(Proportion = c(32.16, 1.78, 34.55, 31.51),
                       `CI_Prop_low` = c(NA, NA, NA, NA),
                       `CI_Prop_upp` = c(NA, NA, NA, NA),
                       Year = c(2010, 2010, 2010, 2010),
                       Dem_group = c("Healthy weight", "Underweight", "Overweight", "Obese"),
                       Category = c("BMI", "BMI", "BMI", "BMI"),
                       Survey = c("United States", "United States", "United States", "United States"))


pop_2020 <- data.frame(Proportion = c(31.34, 1.9, 34.84, 31.92),
                       `CI_Prop_low` = c(NA, NA, NA, NA),
                       `CI_Prop_upp` = c(NA, NA, NA, NA),
                       Year = c(2020, 2020, 2020, 2020),
                       Dem_group = c("Healthy weight", "Underweight", "Overweight", "Obese"),
                       Category = c("BMI", "BMI", "BMI", "BMI"),
                       Survey = c("United States", "United States", "United States", "United States"))

BMI <- Reduce(function(x, y) merge(x, y, all=TRUE), list(BMI, pop_2000, pop_2010, pop_2020))

#plot
fig4a <- ggplot(data = BMI,
                aes(x = Year, y = Proportion, group = Survey, fill = Survey, colour = Survey)) +
  geom_line(size = 0.2) +
  geom_point(size = 0.3) +
  geom_ribbon(aes(ymin = BMI$CI_Prop_low, ymax = BMI$CI_Prop_upp, fill = Survey, linetype = NA), alpha = 0.2) +
  scale_y_continuous(name = "Prevalence (%)", breaks = c(0, 25, 50, 75, 100), limits = c(0, 100)) +
  scale_colour_manual(values = c("aquamarine4", "goldenrod3", "midnightblue", "black")) +
  scale_fill_manual(values = c("aquamarine4", "goldenrod3", "midnightblue", "black")) +
  scale_x_continuous(name = "Survey cycles (years)", breaks = seq(2000, 2020, 4), limits = c(2000, 2020)) +
  facet_wrap(facets = ~Dem_group, ncol = 2) +
  theme_bw() +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(text=element_text(family = "myfont", size = 10)) +
  xlab("Survey cycles (years)") +
  ggtitle("4A") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 8, family = "myfont")) +
  theme(plot.title = element_text(size = 12, face = "bold", family = "myfont"),
        legend.title = element_text(size = 10, face = "bold", family = "myfont"),
        legend.text = element_text(size = 10, family = "myfont"),
        legend.position = "bottom")
ggsave("Figure4A.tiff", width = 150, height = 100, units = "mm", device = "tiff", dpi = 500)


#--number----

B.NO.BMI <- read.csv(url("https://www.dropbox.com/s/8lwoa8c27wm7qs8/B.NO.BMI.csv?dl=1"))
N.NO.BMI <- read.csv(url("https://www.dropbox.com/s/tyg6tp4ca65gmfn/N.NO.BMI.csv?dl=1"))
NA.NO.BMI <- read.csv(url("https://www.dropbox.com/s/u2vwmda1ds4d9ky/NA.NO.BMI.csv?dl=1"))

#- == BMI NUMBERS FIGURE == -----

B.NO.BMI$Survey <- c("BRFSS")
N.NO.BMI$Survey <- c("NHIS")
NA.NO.BMI$Survey <- c("NHANES")
NA.NO.BMI$Dem_group <- as.character(NA.NO.BMI$Dem_group)
BMI.NO <- Reduce(function(x, y) merge(x, y, all=TRUE), list(B.NO.BMI, N.NO.BMI, NA.NO.BMI))
BMI.NO$Year <- as.numeric(BMI.NO$Year)
names(BMI.NO)[names(BMI.NO) == "Number of People"] <- "Number.People"


BMI.NO <- BMI.NO %>%
  mutate(Number.People = Number.People/1000000) %>%
  mutate(CI_Prop_low = CI_Prop_low/1000000) %>%
  mutate(CI_Prop_upp = CI_Prop_upp/1000000)

pop_2000 <- data.frame(Number.People = c(105.39, 6.05, 99.48, 70.5),
                       `CI_Prop_low` = c(NA, NA, NA, NA),
                       `CI_Prop_upp` = c(NA, NA, NA, NA),
                       Year = c(2000, 2000, 2000, 2000),
                       Dem_group = c("Healthy weight", "Underweight", "Overweight", "Obese"),
                       Category = c("BMI", "BMI", "BMI", "BMI"),
                       Survey = c("United States", "United States", "United States", "United States"))

pop_2010 <- data.frame(Number.People = c(99.29, 5.50, 106.67, 97.29),
                       `CI_Prop_low` = c(NA, NA, NA, NA),
                       `CI_Prop_upp` = c(NA, NA, NA, NA),
                       Year = c(2010, 2010, 2010, 2010),
                       Dem_group = c("Healthy weight", "Underweight", "Overweight", "Obese"),
                       Category = c("BMI", "BMI", "BMI", "BMI"),
                       Survey = c("United States", "United States", "United States", "United States"))

pop_2020 <- data.frame(Number.People = c(103.89, 6.3, 115.49, 105.8),
                       `CI_Prop_low` = c(NA, NA, NA, NA),
                       `CI_Prop_upp` = c(NA, NA, NA, NA),
                       Year = c(2020, 2020, 2020, 2020),
                       Dem_group = c("Healthy weight", "Underweight", "Overweight", "Obese"),
                       Category = c("BMI", "BMI", "BMI", "BMI"),
                       Survey = c("United States", "United States", "United States", "United States"))

BMI.NO <- Reduce(function(x, y) merge(x, y, all=TRUE), list(BMI.NO, pop_2000, pop_2010, pop_2020))


fig4b <- ggplot(data = BMI.NO,
                aes(x = Year, y = Number.People, group = Survey, fill = Survey, colour = Survey)) +
  geom_line(size = 0.2) +
  geom_point(size = 0.3) +
  geom_ribbon(aes(ymin = BMI.NO$CI_Prop_low, ymax = BMI.NO$CI_Prop_upp, fill = Survey, linetype = NA), alpha = 0.2) +
  scale_y_continuous(name = "Number of people affected (in millions)") +
  scale_colour_manual(values = c("aquamarine4", "goldenrod3", "midnightblue", "black")) +
  scale_fill_manual(values = c("aquamarine4", "goldenrod3", "midnightblue", "black")) +
  scale_x_continuous(name = "Survey cycles (years)", breaks = seq(2000, 2020, 4), limits = c(2000, 2020)) +
  facet_wrap(facets = ~Dem_group, ncol = 2) +
  theme_bw() +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(text=element_text(family = "myfont", size = 10)) +
  xlab("Survey cycles (years)") +
  ggtitle("4B") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 8, family = "myfont")) +
  theme(plot.title = element_text(size = 12, face = "bold", family = "myfont"),
        legend.title = element_text(size = 10, face = "bold", family = "myfont"),
        legend.text = element_text(size = 10, family = "myfont"),
        legend.position = "bottom")
ggsave("Figure4B.tiff", width = 150, height = 100, units = "mm", device = "tiff", dpi = 500)

