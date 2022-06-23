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
#############################################################################################

rm(list = ls())

#load packages needed
library(tidyverse)
library(readr)
library(vcd)
library(DescTools)
library(magrittr)

# Reading data ------------------------------------------------------------

HLPS_hh_roster <- read.csv("Data/hh_mod_b_19.csv")
HLPS_hh_edu <- read.csv("Data/hh_mod_c_19.csv")
HLPS_hh_eco <- read.csv("Data/hh_mod_e_19.csv")
IHPS_hh_filter <- read.csv("Data/hh_mod_a_filt_19.csv")
HLPS_hh_support <- read.csv("Data/hh_mod_r_19.csv") %>% 
  filter(hh_r0a == 111)

HLPS <- HLPS_hh_roster %>% 
  inner_join(HLPS_hh_edu) %>%
  inner_join(HLPS_hh_eco) %>%
  inner_join(IHPS_hh_filter) %>%
  inner_join(HLPS_hh_support) %>% 
  select(y4_hhid,
       id_code,
       hh_b04,
       hh_b06_4,
       hh_c05_1,
       hh_e06_8a,
       hh_wgt,
       hh_r01) %>% 
  drop_na()


# Cramer test -------------------------------------------------------------


# Identifying if the hh has a working phone
hh_phone <-
  HLPS %>%
  group_by(y4_hhid) %>%
  summarise(hh_phone = min(hh_b06_4, na.rm = TRUE))

# Identifying hh heads
head_edu <- HLPS %>% filter(hh_b04 == 1)

temp <- head_edu %>%
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

crosstable <- table(temp$Phone, temp$Literacy)
crosstable
assocstats(crosstable)
CramerV(crosstable,
        conf.level = 0.95)


# Propensity score model --------------------------------------------------

# logistic regression
ps_fit <- glm(
  formula = factor(Phone) ~ factor(hh_c05_1) + factor(hh_e06_8a),
  family = binomial(link = "logit"),
  data = temp
)

summary(ps_fit)
temp$ps <- predict(ps_fit, type = "response")
sum(temp$ps)
table(temp$Phone)


# Class variable weighting ------------------------------------------------



temp_agg <- temp %>%
  group_by (hh_c05_1, hh_e06_8a, hh_b06_4) %>%
  summarise(n = n(), 
            tt_wgt = sum(hh_wgt))

temp_agg$ps <- predict(ps_fit, type = "response", newdata  = temp_agg)
table(temp_agg$ps)

#####################################################################
# propensity score model fitting, adding new variables logistic regression
######################################################################


#####################################################################
# R-indicators
######################################################################

mydata <-
  merge(all31, select(IHPS_hh_filter, c("y4_hhid", "hh_wgt")))
mydata2 <- mydata[complete.cases(mydata),]

N.hat <- sum(mydata2$hh_wgt)
rho.bar <- sum(mydata2$ps * mydata2$hh_wgt) / N.hat
R.hat <-
  1 - 2 * sqrt((1 / (N.hat - 1)) * sum(mydata2$hh_wgt * (mydata2$ps - rho.bar) **
                                         2))
summary(mydata2$ps)
