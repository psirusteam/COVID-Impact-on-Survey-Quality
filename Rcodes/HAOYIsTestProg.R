setwd(
  "C:/Users/haoyi.Chen/OneDrive - United Nations/Doc/ISWGHS/New/ISWGHS/Task forces/Covid/3. Covid impact survey paper/2. Drafting/Data"
)
#load packages needed
library(dplyr, warn.conflicts = FALSE)
install.packages("openxlsx")
library(openxlsx)
install.packages("tidyr")
library(tidyr)

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


############################### Cramer ####################################################
HLPS_hh_roster <- read.csv('hh_mod_b_19.csv')
HLPS_hh_edu <- read.csv("hh_mod_c_19.csv")
HLPS_m1 <-
  merge(
    HLPS_hh_roster,
    HLPS_hh_edu,
    by.x = c("y4_hhid", "id_code"),
    by.y = c("y4_hhid", "id_code"),
    all = TRUE
  )
sub_m1 <-
  select(HLPS_m1, y4_hhid, id_code, hh_b04, hh_b06_4, hh_c05_1)
hh_phone <-
  sub_m1 %>% group_by(y4_hhid) %>% summarise(hh_phone = min(hh_b06_4, na.rm =
                                                              TRUE))
head_edu <- filter(sub_m1, hh_b04 == 1)
all <-
  merge(hh_phone, head_edu, by = "y4_hhid")
install.packages("rcompanion")
library(rcompanion)
Input = ("
PHone               Literate   Illiterate
Yes                 1779        188
No                  767         434
")
matrix <- as.matrix(read.table(
  textConnection(Input),
  header = TRUE,
  row.names = 1
))
cramerV(matrix, digits = 4)
install.packages("vcd")
library(vcd)

assocstats(matrix)
install.packages("DescTools")
library(DescTools)

CramerV(matrix,
        conf.level = 0.95)
#################################################
## Logistic regression for bias assessment;
# same model used for propensity score
################################################

# load the economic characteristics data
HLPS_hh_eco <- read.csv('hh_mod_e_19.csv')
HLPS_m2 <-
  merge(
    sub_m1,
    HLPS_hh_eco,
    by.x = c("y4_hhid", "id_code"),
    by.y = c("y4_hhid", "id_code"),
    all = TRUE
  )
sub_m2 <-
  select(HLPS_m2, y4_hhid, id_code, hh_b04, hh_b06_4, hh_c05_1, hh_e06_8a)
head_eco <- filter(sub_m2, hh_b04 == 1)
all2 <- merge(hh_phone, head_eco, by = "y4_hhid")

# logistic regression
all3 <- all2 %>% mutate(V1 = ifelse (hh_phone == 2, 0, 1))
all3 %>% mutate(factor(hh_e06_8a))

head(all3)
dim(all3)

all4 <- glm(
  formula = V1 ~ factor(hh_c05_1) + factor(hh_e06_8a),
  family = binomial,
  data = all3
)
summary(all4)
write.xlsx(all, "CramerR_example1.xlsx")
write.xlsx(all2, "CramerR_example2.xlsx")
write.xlsx(all4, "LogisticFit.xlsx")
write.xlsx(all3, "data.xlsx")

##############################################
# Class variable weighting
##############################################
IHPS_hh_filter <- read.csv("hh_mod_a_filt_19.csv")
HLPS_m3 <-
  merge(sub_m2,
        IHPS_hh_filter,
        by.x = "y4_hhid",
        by.y = "y4_hhid",
        all = TRUE)
sub_m3 <-
  select(HLPS_m3,
         y4_hhid,
         id_code,
         hh_b04,
         hh_b06_4,
         hh_c05_1,
         hh_e06_8a,
         hh_wgt)
head_m3 <- filter(sub_m3, hh_b04 == 1)
Count <-
  head_m3 %>% group_by (hh_c05_1, hh_e06_8a, hh_b06_4) %>% summarise(n =
                                                                       n(), tt_wgt = sum(hh_wgt))

write.xlsx(Count, "nonresponse weight.xlsx")
write.xlsx(head_m3, "nonresponse weight2.xlsx")


##################################################
# propensity score model fitting, logistic regression
###################################################
all31 <- all3 %>%
  mutate (
    Prob = case_when(
      hh_c05_1 == 1 & hh_e06_8a == 1 ~ exp(2.056) / (1 + exp(2.056)),
      hh_c05_1 == 1 &
        hh_e06_8a == 2 ~ exp(2.056 - 0.651) / (1 + exp(2.056 - 0.651)),
      hh_c05_1 == 1 &
        hh_e06_8a == 3 ~ exp(2.056 - 1.711) / (1 + exp(2.056 - 1.711)),
      hh_c05_1 == 1 &
        hh_e06_8a == 4 ~ exp(2.056 + 11.51) / (1 + exp(2.056 + 11.51)),
      hh_c05_1 == 1 &
        hh_e06_8a == 5 ~ exp(2.056 - 1.859) / (1 + exp(2.056 - 1.859)),
      hh_c05_1 == 2 &
        hh_e06_8a == 1 ~ exp(2.056 - 1.39) / (1 + exp(2.056 - 1.39)),
      hh_c05_1 == 2 &
        hh_e06_8a == 2 ~ exp(2.056 - 1.39 - 0.651) / (1 + exp(2.056 - 1.39 - 0.651)),
      hh_c05_1 == 2 &
        hh_e06_8a == 3 ~ exp(2.056 - 1.39 - 1.711) / (1 + exp(2.056 - 1.39 - 1.711)),
      hh_c05_1 == 2 &
        hh_e06_8a == 4 ~ exp(2.056 - 1.39 + 11.51) / (1 + exp(2.056 - 1.39 + 11.51)),
      hh_c05_1 == 2 &
        hh_e06_8a == 5 ~ exp(2.056 - 1.39 - 1.859) / (1 + exp(2.056 - 1.39 - 1.859)),
    )
  )

all31$ps <- predict(all4, type = "response", newdata  = all31)

summary(all31$hh_c05_1)
summary(all31$hh_e06_8a)
summary(all31$ps)


head(all31)

write.xlsx(all31, "propensity.xlsx")

#####################################################################
# propensity score model fitting, adding new variables logistic regression
######################################################################

HLPS_hh_support <- read.csv('hh_mod_r_19.csv')
HLPS_m4 <-
  merge(
    sub_m3,
    HLPS_hh_support,
    by.x = "y4_hhid",
    by.y = "y4_hhid",
    all = TRUE
  )
sub_m4 <- filter(HLPS_m4, hh_r0a == 111)
sub_m41 <-
  select(sub_m4,
         y4_hhid,
         id_code,
         hh_b04,
         hh_b06_4,
         hh_c05_1,
         hh_e06_8a,
         hh_wgt,
         hh_r01)


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
