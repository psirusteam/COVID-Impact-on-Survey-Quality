###############################################################################################
#Data used: Integrated Household Panel Survey
#2010-2013-2016-2019 (Long-Term Panel, 102 EAs)
#https://microdata.worldbank.org/index.php/catalog/3819/data-dictionary/F299?file_name=hh_mod_b_19
#variable label: y4_hhid: household ID id_code: individual ID within the household
#hh_b04: relationship to head ; 1 - YES; 2 - NO
#hh_b06_4: does [name] have a working cell phone; 1- YES; 2 - NO
#hh_c05_1: can [Name] read a short text in any language]; 1 - YES; 2 - NO
#hh_e06_8a: ... economic activity did [NAME] spent most time in the last 12 months;
##### 1- Wage employment excluding Ganyu;
##### 2- household business (nonag)
##### 3- unpaid household labour (agric)
##### 4- unpaid apprenticeship
##### 5- Ganyu
# hh_wgt: weight at household level
#https://microdata.worldbank.org/index.php/catalog/3818/data-dictionary/F2?file_name=hh_mod_a_filt.dta
#############################################################################################

rm(list = ls())

#load packages needed
library(tidyverse)
library(readr)
library(vcd)
library(DescTools)
library(magrittr)
library(PracTools)
library(sampling)
library(TeachingSampling)

# Reading data ------------------------------------------------------------

# HFPS = High Frequency Phone Survey - Malawi
# IHPS = Integrated Household Panel Survey - Malawi (the weights are here)

HFPS_hh_roster <- read.csv("Data/hh_mod_b_19.csv")
HFPS_hh_edu <- read.csv("Data/hh_mod_c_19.csv")
HFPS_hh_eco <- read.csv("Data/hh_mod_e_19.csv")
IHPS_hh_filter <- read.csv("Data/hh_mod_a_filt_19.csv")
HFPS_hh_support <- read.csv("Data/hh_mod_r_19.csv")  %>%
  filter(hh_r0a == 111)


# Data cleaning -----------------------------------------------------------

HFPS <- HFPS_hh_roster %>%
  mutate(Age = case_when(
    hh_b06b <= 1959 ~ "60 +",
    hh_b06b <= 1999 ~ "20 - 59",
    TRUE ~ "0 - 19")) %>%
  inner_join(HFPS_hh_edu) %>%
  inner_join(HFPS_hh_eco) %>%
  inner_join(IHPS_hh_filter) %>%
  inner_join(HFPS_hh_support) %>%
  select(y4_hhid,
         id_code,
         hh_b04,
         hh_b06_4,
         hh_c05_1,
         hh_e06_8a,
         hh_wgt,
         panelweight_2019,
         hh_r01,
         hh_r0a,
         hh_b03,
         Age) %>%
  drop_na() %>%
  filter(hh_e06_8a != 4)

# Household head data -----------------------------------------------------

# Identifying if the hh has a working phone
hh_phone <-
  HFPS %>%
  group_by(y4_hhid) %>%
  summarise(hh_phone = min(hh_b06_4, na.rm = TRUE),
            tt_wgt = sum(hh_wgt))

# Identifying hh heads
head_hh <- HFPS %>% filter(hh_b04 == 1)

hh_temp <- head_hh %>%
  inner_join(hh_phone) %>%
  drop_na() %>%
  mutate(
    Phone =
      case_when(hh_b06_4 == 1 ~ "Yes",
                TRUE ~ "No"),
    Literacy =
      case_when(hh_c05_1 == 1 ~ "Literate",
                TRUE ~ "Illiterate"),
  )


# Chi-sqrd and Cramer's-V tests -------------------------------------------


# Phone ownership is correlated with literacy
# From the phone subset a sample is selected in a further 2nd phase
# The phone data conducted by phone
crosstable1 <- table(hh_temp$Phone, hh_temp$Literacy)

# Table 5 of the guidance
crosstable1

# Para 82 of the guidance
chisq.test(crosstable1)
CramerV(crosstable1,
        conf.level = 0.95)

assocstats(crosstable1)
# Figure 5 of the guidance
crosstable2 <- table(hh_temp$Phone, hh_temp$hh_e06_8a)
prop.table(crosstable2, margin = 2)[2,]

# Propensity score model --------------------------------------------------

# logistic regression
ps_fit1 <- glm(
  formula = factor(Phone) ~ 1 + factor(hh_c05_1) + factor(hh_e06_8a),
  family = binomial(link = "logit"),
  data = hh_temp
)

summary(ps_fit1)
hh_temp$ps <- predict(ps_fit1, type = "response")
sum(hh_temp$ps)
table(hh_temp$Phone)
hist(hh_temp$ps)
summary(hh_temp$ps)

#####################################################################
# R-indicators
######################################################################

## At the national - level
summary_ps_national <- hh_temp %>%
  summarise(
    n = n(),
    urp = mean(ps),
    rho.bar = weighted.mean(ps, hh_wgt),
    urr = sum(Phone == "Yes") / n(),
    wrr = weighted.mean(Phone == "Yes", hh_wgt),
    sdp = sd(ps),
    R.hat = 1 - 2 * sqrt(weighted.mean((ps - rho.bar) ^ 2, hh_wgt))
  )

summary_ps_national

sd(hh_temp$ps)
N.hat <- sum(hh_temp$hh_wgt)
rho.bar <- sum(hh_temp$ps * hh_temp$hh_wgt) / N.hat
R.hat <- 1 - 2 * 
  sqrt((1 / (N.hat - 1)) * sum(hh_temp$hh_wgt * (hh_temp$ps - rho.bar) ^ 2))
summary(hh_temp$ps)

## At the dissaggregated - level

# Literacy
with(hh_temp, boxplot(ps ~ Literacy))

par(mfrow = c(1, 3))
hist(hh_temp$ps)
hist(hh_temp$ps[hh_temp$Literacy == "Literate"])
hist(hh_temp$ps[hh_temp$Literacy == "Illiterate"])

summary_ps_Literacy <- hh_temp %>%
  group_by(Literacy) %>% 
  summarise(
    n = n(),
    urp = mean(ps),
    rho.bar = weighted.mean(ps, hh_wgt),
    urr = sum(Phone == "Yes") / n(),
    wrr = weighted.mean(Phone == "Yes", hh_wgt),
    sdp = sd(ps),
    R.hat = 1 - 2 * sqrt(weighted.mean((ps - rho.bar) ^ 2, hh_wgt))
  ) %>% as.data.frame()

summary_ps_Literacy

dev.off( )

# Activity
with(hh_temp, boxplot(ps ~ hh_e06_8a))
hist(hh_temp$ps)
par(mfrow = c(2, 2))
hist(hh_temp$ps[hh_temp$hh_e06_8a == 1])
hist(hh_temp$ps[hh_temp$hh_e06_8a == 2])
hist(hh_temp$ps[hh_temp$hh_e06_8a == 3])
hist(hh_temp$ps[hh_temp$hh_e06_8a == 5])

dev.off( )

summary_ps_Activity <- hh_temp %>%
  group_by(hh_e06_8a) %>% 
  summarise(
    n = n(),
    urp = mean(ps),
    rho.bar = weighted.mean(ps, hh_wgt),
    urr = sum(Phone == "Yes") / n(),
    wrr = weighted.mean(Phone == "Yes", hh_wgt),
    sdp = sd(ps),
    R.hat = 1 - 2 * sqrt(weighted.mean((ps - rho.bar) ^ 2, hh_wgt))
  ) %>% as.data.frame()

summary_ps_Activity


# Activity and Literacy


summary_ps_LiteracyActivity <- hh_temp %>%
  group_by(Literacy, hh_e06_8a) %>% 
  summarise(
    n = n(),
    urp = mean(ps),
    rho.bar = weighted.mean(ps, hh_wgt),
    urr = sum(Phone == "Yes") / n(),
    wrr = weighted.mean(Phone == "Yes", hh_wgt),
    sdp = sd(ps),
    R.hat = 1 - 2 * sqrt(weighted.mean((ps - rho.bar) ^ 2, hh_wgt))
  ) %>% as.data.frame()

summary_ps_LiteracyActivity

# Class variable weighting ------------------------------------------------

# Table 11 of the guidance

hh_temp_agg <- hh_temp %>%
  group_by (hh_c05_1, hh_e06_8a, hh_b06_4) %>%
  summarise(n = n(),
            thh_wgt = sum(panelweight_2019))

hh_temp_agg_resp <- hh_temp_agg %>% filter(hh_b06_4 == 1)
hh_temp_agg_nonr <- hh_temp_agg  %>% filter(hh_b06_4 == 2)

# Table 11

Table11 <- data.frame(hh_temp_agg_nonr, 
                 hh_temp_agg_resp %>% ungroup %>% 
                   select(-hh_c05_1, -hh_e06_8a, -hh_b06_4))

sum(hh_temp_agg_resp$thh_wgt)
sum(hh_temp_agg_nonr$thh_wgt)
sum(hh_temp$panelweight_2019)
# In Malawi the average household size is 4.5
# In Malawi the population is about 19.3 million
# That means that the total of households in Malawi 
# should be close to 4.3 
sum(IHPS_hh_filter$panelweight_2019)
sum(IHPS_hh_filter$panelweight_2019 * IHPS_hh_filter$hhsize)

hh_temp_agg_resp$ac <-
  (hh_temp_agg_resp$thh_wgt + hh_temp_agg_nonr$thh_wgt) / hh_temp_agg_resp$thh_wgt

hh_temp_resp <-  hh_temp %>% 
  inner_join(hh_temp_agg_resp %>% 
               select(-n, -thh_wgt) )

hh_temp_resp$w0ac <- with(hh_temp_resp, panelweight_2019 * ac)
sum(hh_temp_resp$panelweight_2019)
sum(hh_temp_resp$w0ac)

plot(hh_temp_resp$panelweight_2019, hh_temp_resp$w0ac)
abline(a = 0, b = 1, col = 2)
hist(hh_temp_resp$panelweight_2019)
hist(hh_temp_resp$w0ac)

# Classes for PS ----------------------------------------------------------

psclass = pclass(
  factor(Phone) ~ 1 + factor(hh_c05_1) +
    factor(hh_e06_8a),
  type = "unwtd",
  data = hh_temp
)

table(psclass$p.class, useNA = "always")
summary(psclass$propensities)
hh_temp$ps <- predict(ps_fit1, type = "response")
hh_temp$class <- psclass$p.class

summary_ps_5classes <- hh_temp %>%
  group_by(class) %>%
  summarise(
    n = n(),
    urp = mean(ps),
    wrp = weighted.mean(ps, hh_wgt),
    urr = sum(Phone == "Yes") / n(),
    wrr = weighted.mean(Phone == "Yes", hh_wgt),
    mps = median(ps)
  )

ps_classified <- summary_ps_5classes %>% select(class, wrp)

hh_temp_resp <- hh_temp %>% 
  inner_join(ps_classified) %>% 
  inner_join(hh_temp_resp)

table(hh_temp$Phone)
table(hh_temp$class)

hh_temp_resp$w1ps <- with(hh_temp_resp, panelweight_2019 / wrp)
sum(hh_temp_resp$panelweight_2019)
sum(hh_temp_resp$w1ps)

plot(hh_temp_resp$panelweight_2019, hh_temp_resp$w1ps)
abline(a = 0, b = 1, col = 2)

plot(hh_temp_resp$w0ac, hh_temp_resp$w1ps)
abline(a = 0, b = 1, col = 2)

hist(hh_temp_resp$panelweight_2019)
hist(hh_temp_resp$w0ac)
hist(hh_temp_resp$w1ps)


#####################################################################
# Calibration
#####################################################################
# 1 Male - 2 Female
table(HFPS$hh_b03, useNA = "a") 
table(hh_temp_resp$hh_b03)
# Age groups
table(hh_temp$Age) 
table(hh_temp_resp$Age) 

hh_temp_resp$AgeSex <- paste(hh_temp_resp$hh_b03, hh_temp_resp$Age)
table(hh_temp_resp$AgeSex)

summary(hh_temp_resp$tt_wgt)
sum(hh_temp_resp$tt_wgt)

x0s <- as.matrix(Domains(hh_temp_resp$AgeSex))
tx0 <- c(4816925, 3318502, 386033,
         4937607, 3598910, 505772) / 4.5

sum(tx0)

g0k <- calib(x0s,
             d = hh_temp_resp$panelweight_2019,
             total = tx0,
             method = "linear"
             )

hh_temp_resp$w2cal <- hh_temp_resp$panelweight_2019 * g0k
sum(hh_temp_resp$w2cal)

tx0
colSums(x0s*hh_temp_resp$panelweight_2019)

tx0 / colSums(x0s*hh_temp_resp$panelweight_2019)
table(g0k)

sum(hh_temp_resp$panelweight_2019)
sum(hh_temp_resp$w2cal)

plot(hh_temp_resp$panelweight_2019, hh_temp_resp$w2cal)
abline(a = 0, b = 1, col = 2)
hist(hh_temp_resp$panelweight_2019)
hist(hh_temp_resp$w2cal)

summary(hh_temp_resp$panelweight_2019)
summary(hh_temp_resp$w2cal)

hh_temp_resp %>% group_by(AgeSex) %>% count()

#####################################################################
# PS + Calibration
#####################################################################

plot(hh_temp_resp$w1ps, hh_temp_resp$w2cal)
abline(a = 0, b = 1, col = 2)
hist(hh_temp_resp$w1ps)
hist(hh_temp_resp$w2cal)

g1k <- calib(x0s,
             d = hh_temp_resp$w1ps,
             total = tx0,
             method = "linear"
)

hh_temp_resp$w3pscal <- hh_temp_resp$w1ps * g1k

sum(hh_temp_resp$panelweight_2019)
sum(hh_temp_resp$w1ps)
sum(hh_temp_resp$w2cal)

plot(hh_temp_resp$w1ps, hh_temp_resp$w3pscal)
abline(a = 0, b = 1, col = 2)
hist(hh_temp_resp$w1ps)
hist(hh_temp_resp$w3pscal)

summary(hh_temp_resp$w1ps)
summary(hh_temp_resp$w3pscal)

plot(hh_temp_resp$w2cal, hh_temp_resp$w3pscal)
abline(a = 0, b = 1, col = 2)
hist(hh_temp_resp$w2cal)
hist(hh_temp_resp$w3pscal)

summary(hh_temp_resp$w2cal)
summary(hh_temp_resp$w3pscal)

