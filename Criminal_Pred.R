# Task 2
# Q1. Import	modules	needed	to implement	predictive	maintenance,	 R	is	used	(	ggplot	and	dplyr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

# Task 2
# Q2. Read	the non	violent	dataset	 to	read	the	number	of	rows
raw_data <- read.csv("data/compas-scores-two-years.csv")

# Task 2
# Q3. Remove	rows and columns	based	on	following	conditions
df <- dplyr::select(raw_data, age, c_charge_degree, race, age_cat, score_text, sex, priors_count, 
                    days_b_screening_arrest, decile_score, is_recid, two_year_recid, c_jail_in, c_jail_out) %>%
  filter(days_b_screening_arrest <= 30) %>%
  filter(days_b_screening_arrest >= -30) %>%
  filter(is_recid != -1) %>%
  filter(c_charge_degree != "O") %>%
  filter(score_text != 'N/A')

# Task 2
# Q4. Get	new	column	longer	length	of	stay
## does the compas score affects the number of days those criminal will have to stay?
## there is sligth correlation that if the compas score is higher the longest day you will stay in jail.
df$length_of_stay <- as.numeric(as.Date(df$c_jail_out, format = "%d-%m-%Y %H:%M") - as.Date(df$c_jail_in, format = "%d-%m-%Y %H:%M"))
cor(df$length_of_stay, df$decile_score)
# Extra
nrow(df)
ncol(df)
str(df)
summary(df)

# Task 2
# Q5. Get	the	summary	of	race,	gender,	age,	xtabs	by	sex	and	race
table(df$race)
table(df$sex)
table(df$age_cat)

round((table(df$race)/nrow(df))*100,2)
round((table(df$age_cat)/nrow(df))*100,2)
round((table(df$sex)/nrow(df))*100,2)

xtabs(~ sex + race, data=df)

# Task 2
# Q6. Plot	the	data	with	race	and	decile	score
#Judges are often presented with two sets of scores from the Compas system -- one that classifies people into High, Medium and Low risk, and a corresponding 
#decile score. There is a clear downward trend in the decile scores as those scores increase for white defendants.

pblack <- ggplot(data=filter(df, race =="African-American"), aes(ordered(decile_score))) + 
  geom_bar() + 
  xlab("Decile Score") +
  ggtitle("Black Defendant's Decile Scores")

pwhite <- ggplot(data=filter(df, race =="Caucasian"), aes(ordered(decile_score))) + 
  geom_bar() + 
  xlab("Decile Score") +
  ggtitle("White Defendant's Decile Scores")

grid.arrange(pblack, pwhite,  nrow = 2)
grid.arrange(pblack, pwhite,  ncol = 2)

# Task 3

df <- mutate(df, crime_factor = factor(c_charge_degree)) %>%
  mutate(age_factor = as.factor(age_cat)) %>%
  within(age_factor <- relevel(age_factor, ref = 1)) %>%
  mutate(race_factor = factor(race)) %>%
  within(race_factor <- relevel(race_factor, ref = 3)) %>%
  mutate(gender_factor = factor(sex, labels= c("Female","Male"))) %>%
  within(gender_factor <- relevel(gender_factor, ref = 2)) %>%
  mutate(score_factor = factor(score_text != "Low", labels = c("LowScore","HighScore")))

str(df)

model <- glm(score_factor ~ gender_factor + age_factor + race_factor + priors_count + crime_factor + two_year_recid, 
             family="binomial", data=df)

summary(model)

str(model)

## optional
control <- exp(-1.52554) / (1 + exp(-1.52554))
control

####Black defendants are 45% more likely than white defendants to receive a higher score correcting for the seriousness of their crime, previous arrests, and future criminal behavior.
exp(0.47721) / (1 - control + (control * exp(0.47721)))


# Task 4
df2 <- dplyr::select(raw_data, age, c_charge_degree, race, age_cat, v_score_text, sex, priors_count, 
                     days_b_screening_arrest, v_decile_score, is_violent_recid, two_year_recid, c_jail_in, c_jail_out) %>% 
  filter(days_b_screening_arrest <= 30) %>%
  filter(days_b_screening_arrest >= -30) %>% 
  filter(is_violent_recid != -1) %>%
  filter(c_charge_degree != "O") %>%
  filter(v_score_text != 'N/A')


df2$v_length_of_stay <- as.numeric(as.Date(df2$c_jail_out, format = "%d-%m-%Y %H:%M") - as.Date(df2$c_jail_in, format = "%d-%m-%Y %H:%M"))
cor(df2$v_length_of_stay, df2$v_decile_score)

nrow(df2)
ncol(df2)
str(df2)
summary(df2)

table(df2$race)
table(df2$age_cat)
table(df2$sex)

round((table(df2$race)/nrow(df2))*100,2)
round((table(df2$age_cat)/nrow(df2))*100,2)
round((table(df2$sex)/nrow(df2))*100,2)

xtabs(~ sex + race, data=df2)

pblack <- ggplot(data=filter(df2, race =="African-American"), aes(ordered(v_decile_score))) + 
  geom_bar() + xlab("Violent Decile Score") +
  ggtitle("Black Defendant's Violent Decile Scores")

pwhite <- ggplot(data=filter(df2, race =="Caucasian"), aes(ordered(v_decile_score))) + 
  geom_bar() + xlab("Violent Decile Score") +
  ggtitle("White Defendant's Violent Decile Scores")

grid.arrange(pblack, pwhite,  ncol = 2)

# Optional
df2 <- mutate(df2, crime_factor = factor(c_charge_degree)) %>%
  mutate(age_factor = as.factor(age_cat)) %>%
  within(age_factor <- relevel(age_factor, ref = 1)) %>%
  mutate(race_factor = factor(race,
                              labels = c("African-American", 
                                         "Asian",
                                         "Caucasian", 
                                         "Hispanic", 
                                         "Native American",
                                         "Other"))) %>%
  within(race_factor <- relevel(race_factor, ref = 3)) %>%
  mutate(gender_factor = factor(sex, labels= c("Female","Male"))) %>%
  within(gender_factor <- relevel(gender_factor, ref = 2)) %>%
  mutate(score_factor = factor(v_score_text != "Low", labels = c("LowScore","HighScore")))

str(df2)

model_2 <- glm(score_factor ~ gender_factor + age_factor + race_factor +
                 priors_count + crime_factor + two_year_recid, family="binomial", data=df2)

summary(model_2)

control <- exp(-2.24274) / (1 + exp(-2.24274))
control

####Black defendants are 77% more likely than white defendants to receive a higher score correcting for the seriousness of their crime, previous arrests, and future criminal behavior.
exp(0.627982) / (1 - control + (control * exp(0.627982)))

# Task 5:   Non-Violent Based of Skin color
library(survival)
library(ggfortify)

df3 = read.csv("data/cox-parsed.csv")

nrow(df3)

data <- filter(filter(df3 , score_text != "N/A"), end > start) %>%
  mutate(race_factor = factor(race,
                              labels = c("African-American", 
                                         "Asian",
                                         "Caucasian", 
                                         "Hispanic", 
                                         "Native American",
                                         "Other"))) %>%
  within(race_factor <- relevel(race_factor, ref = 3)) %>%
  mutate(score_factor = factor(score_text)) %>%
  within(score_factor <- relevel(score_factor, ref=2))

nrow(data)

grp <- data[!duplicated(data$id),]

nrow(grp)

str(grp)

summary(grp$score_factor)

summary(grp$race_factor)

##### using the survival function and coxph
f <- Surv(start, end, event, type="counting") ~ score_factor
summary(f)

plotty <- function(fit, title) {
  return(autoplot(fit, conf.int=T, censor=F) + ggtitle(title) + ylim(0,1))
}
white <- filter(grp, race == "Caucasian")
white_fit <- survfit(f, data=white)

black <- filter(grp, race == "African-American")
black_fit <- survfit(f, data=black)

grid.arrange(plotty(white_fit, "White defendants"), 
             plotty(black_fit, "Black defendants"), ncol=2)

# Task 6  - Violent based on skin color
df4 = read.csv("data/cox-violent-parsed.csv")

nrow(df4)

violent_data <- filter(filter(df4, score_text != "N/A"), end > start) %>%
  mutate(race_factor = factor(race,
                              labels = c("African-American", 
                                         "Asian",
                                         "Caucasian", 
                                         "Hispanic", 
                                         "Native American",
                                         "Other"))) %>%
  within(race_factor <- relevel(race_factor, ref = 3)) %>%
  mutate(score_factor = factor(score_text)) %>%
  within(score_factor <- relevel(score_factor, ref=2))

vgrp <- violent_data[!duplicated(violent_data$id),]

nrow(vgrp)

vf <- Surv(start, end, event, type="counting") ~ score_factor
vmodel <- coxph(vf, data=vgrp)
summary(vmodel)

plotty <- function(fit, title) {
  return(autoplot(fit, conf.int=T, censor=F) + ggtitle(title) + ylim(0,1))
}
white <- filter(vgrp, race == "Caucasian")
white_fit <- survfit(vf, data=white)

black <- filter(vgrp ,race == "African-American")
black_fit <- survfit(vf, data=black)
grid.arrange(plotty(white_fit, "White defendants"), 
             plotty(black_fit, "Black defendants"), ncol=2)

# Task 7 - Non violent , Violent based on gender
#-- non violent
female <- filter(grp, sex == "Female")
male   <- filter(grp, sex == "Male")
f <- Surv(start, end, event, type="counting") ~ score_factor
male_fit <- survfit(f, data=male)
female_fit <- survfit(f, data=female)

summary(male_fit, times=c(730))

summary(female_fit, times=c(730))

grid.arrange(plotty(female_fit, "Female"), plotty(male_fit, "Male"),ncol=2)

#-- violent
vfemale <- filter(vgrp, sex == "Female")
vmale   <- filter(vgrp, sex == "Male")
vf <- Surv(start, end, event, type="counting") ~ score_factor
v_male_fit <- survfit(vf, data=vmale)
v_female_fit <- survfit(vf, data=vfemale)

summary(v_male_fit, times=c(730))

summary(v_female_fit, times=c(730))

grid.arrange(plotty(v_female_fit, "Female"), plotty(v_male_fit, "Male"),ncol=2)

# optional - overall
overal_fit <- survfit(f, data=grp)
plotty(overal_fit, "Overall - Non Violent")

# optional - overall
v_overal_fit <- survfit(vf , data=vgrp)
plotty(v_overal_fit, "Overall - Violent")