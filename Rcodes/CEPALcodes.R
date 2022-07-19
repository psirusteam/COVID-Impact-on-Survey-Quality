rm(list = ls())

# Loading libraries -------------------------------------------------------

library(pacman)
p_load(
  tidyverse,
  readr,
  vcd,
  DescTools,
  magrittr,
  PracTools,
  sampling,
  TeachingSampling,
  psych
)

# Reading data ------------------------------------------------------------

# HFPS = High Frequency Phone Survey - Malawi
# IHPS = Integrated Household Panel Survey - Malawi

HFPS_hh_roster <- read.csv("Data/hh_mod_b_19.csv")
HFPS_hh_edu <- read.csv("Data/hh_mod_c_19.csv")
HFPS_hh_eco <- read.csv("Data/hh_mod_e_19.csv")
IHPS_hh_filter <- read.csv("Data/hh_mod_a_filt_19.csv")

# Data cleaning -----------------------------------------------------------

HFPS <- HFPS_hh_roster %>%
  inner_join(HFPS_hh_edu) %>%
  inner_join(HFPS_hh_eco) %>%
  inner_join(IHPS_hh_filter) %>%
  select(
    y4_hhid,
    id_code,
    hh_b04,
    hh_b06_4,
    hh_c05_1,
    hh_e06_8a,
    hh_wgt,
    panelweight_2019,
    region,
    reside,
    hhsize
  ) %>% 
  drop_na() %>%
  filter(hh_e06_8a != 4)      # Excluding Unpaid Apprenticeship

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
crosstable1

chisq.test(crosstable1)
CramerV(crosstable1,
        conf.level = 0.95)

assocstats(crosstable1)

crosstable2 <- table(hh_temp$Phone, hh_temp$hh_e06_8a)
prop.table(crosstable2, margin = 2)[2, ]

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

par(mfrow = c(3, 1))
hist(hh_temp$ps,
     main = "All units",
     xlab = "Estimated PS",
     xlim = c(0, 1))
hist(hh_temp$ps[hh_temp$Phone == "Yes"],
     main = "Respondents",
     xlab = "Estimated PS",
     xlim = c(0, 1))
hist(hh_temp$ps[hh_temp$Phone == "No"],
     main = "Nonrespondents",
     xlab = "Estimated PS",
     xlim = c(0, 1))
dev.off()

par(mfrow = c(2, 2))
with(
  hh_temp[hh_temp$Phone == "Yes",],
  boxplot(
    ps ~ factor(hh_c05_1),
    main = "Respondents",
    ylab = "Estimated PS",
    xlab = "Literacy"
  )
)

with(
  hh_temp[hh_temp$Phone == "No",],
  boxplot(
    ps ~ factor(hh_c05_1),
    main = "Nonrespondents",
    ylab = "Estimated PS",
    xlab = "Literacy"
  )
)

with(
  hh_temp[hh_temp$Phone == "Yes",],
  boxplot(
    ps ~ factor(hh_e06_8a),
    main = "Respondents",
    ylab = "Estimated PS",
    xlab = "Employment Status"
  )
)

with(
  hh_temp[hh_temp$Phone == "No",],
  boxplot(
    ps ~ factor(hh_e06_8a),
    main = "Nonrespondents",
    ylab = "Estimated PS",
    xlab = "Employment Status"
  )
)
dev.off()


# Representativity Indicators ---------------------------------------------

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
dev.off()

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


# Activity
with(hh_temp, boxplot(ps ~ hh_e06_8a))
hist(hh_temp$ps)

par(mfrow = c(2, 2))
hist(hh_temp$ps[hh_temp$hh_e06_8a == 1])
hist(hh_temp$ps[hh_temp$hh_e06_8a == 2])
hist(hh_temp$ps[hh_temp$hh_e06_8a == 3])
hist(hh_temp$ps[hh_temp$hh_e06_8a == 5])
dev.off()

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

hh_temp_agg <- hh_temp %>%
  group_by (hh_c05_1, hh_e06_8a, hh_b06_4) %>%
  summarise(n = n(),
            thh_wgt = sum(panelweight_2019))

hh_temp_agg_resp <- hh_temp_agg %>% filter(hh_b06_4 == 1)
hh_temp_agg_nonr <- hh_temp_agg  %>% filter(hh_b06_4 == 2)

Table11 <- data.frame(
  hh_temp_agg_nonr,
  hh_temp_agg_resp %>% ungroup %>%
    select(-hh_c05_1,-hh_e06_8a,-hh_b06_4)
)

sum(hh_temp_agg_resp$thh_wgt)
sum(hh_temp_agg_nonr$thh_wgt)
sum(hh_temp$panelweight_2019)

hh_temp_agg_resp$ac <-
  (hh_temp_agg_resp$thh_wgt + hh_temp_agg_nonr$thh_wgt) / hh_temp_agg_resp$thh_wgt

hh_temp_resp <-  hh_temp %>%
  inner_join(hh_temp_agg_resp %>%
               select(-n,-thh_wgt))

hh_temp_resp$wac <-
  with(hh_temp_resp, panelweight_2019 * ac * hhsize)
hh_temp_resp$w0 <-
  hh_temp_resp$panelweight_2019 * hh_temp_resp$hhsize

sum(hh_temp_resp$w0)
sum(hh_temp_resp$wac)

plot(
  hh_temp_resp$w0,
  hh_temp_resp$wac,
  xlab = "w0",
  ylab = "wac",
  xlim = c(0, 200000),
  ylim = c(0, 200000)
)
abline(a = 0, b = 1, col = 2)

hist(hh_temp_resp$panelweight_2019)
hist(hh_temp_resp$wac)

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

ps_classified <- summary_ps_5classes %>% select(-n)

hh_temp_resp <- hh_temp %>%
  inner_join(ps_classified) %>%
  inner_join(hh_temp_resp)

table(hh_temp$Phone)
table(hh_temp$class)

hh_temp_resp$w1ps <-
  with(hh_temp_resp, hhsize * panelweight_2019 / urp)
hh_temp_resp$w2ps <-
  with(hh_temp_resp, hhsize * panelweight_2019 / wrp)
hh_temp_resp$w3ps <-
  with(hh_temp_resp, hhsize * panelweight_2019 / urr)
hh_temp_resp$w4ps <-
  with(hh_temp_resp, hhsize * panelweight_2019 / wrr)

sum(hh_temp_resp$w0)
sum(hh_temp_resp$w1ps)
sum(hh_temp_resp$w2ps)
sum(hh_temp_resp$w3ps)
sum(hh_temp_resp$w4ps)

par(mfrow = c(2, 2))

plot(
  hh_temp_resp$wac,
  hh_temp_resp$w1ps,
  ylab = "w1ps",
  xlab = "wac",
  xlim = c(0, 200000),
  ylim = c(0, 200000)
)
abline(a = 0, b = 1, col = 2)
plot(
  hh_temp_resp$wac,
  hh_temp_resp$w2ps,
  ylab = "w2ps",
  xlab = "wac",
  xlim = c(0, 200000),
  ylim = c(0, 200000)
)
abline(a = 0, b = 1, col = 2)
plot(
  hh_temp_resp$wac,
  hh_temp_resp$w3ps,
  ylab = "w3ps",
  xlab = "wac",
  xlim = c(0, 200000),
  ylim = c(0, 200000)
)
abline(a = 0, b = 1, col = 2)
plot(
  hh_temp_resp$wac,
  hh_temp_resp$w4ps,
  ylab = "w4ps",
  xlab = "wac",
  xlim = c(0, 200000),
  ylim = c(0, 200000)
)
abline(a = 0, b = 1, col = 2)

dev.off()


# Calibration -------------------------------------------------------------

# Reside and Region
table(HFPS$reside)
table(hh_temp$reside)

table(HFPS$region)
table(hh_temp$region)

table(paste(HFPS$reside, HFPS$region))
sum(HFPS$hh_wgt)

HFPS %>%
  group_by(region) %>%
  summarise(n = n(),
            N = sum(hh_wgt))

HFPS %>%
  group_by(reside) %>%
  summarise(n = n(),
            N = sum(hh_wgt))

totals <- HFPS %>%
  group_by(reside, region) %>%
  summarise(n = n(),
            N = sum(hh_wgt))

x0s <-
  as.matrix(Domains(paste(
    hh_temp_resp$reside, hh_temp_resp$region
  )))
tx0 <- as.matrix(totals$N)

sum(tx0)

g0k <- calib(
  x0s,
  d = hh_temp_resp$panelweight_2019 * hh_temp_resp$hhsize,
  total = tx0,
  method = "linear"
)

hh_temp_resp$wcal <-
  hh_temp_resp$panelweight_2019 * g0k * hh_temp_resp$hhsize
sum(hh_temp_resp$wcal)

tx0
colSums(x0s * hh_temp_resp$panelweight_2019 * hh_temp_resp$hhsize)

tx0 / colSums(x0s * hh_temp_resp$panelweight_2019 * hh_temp_resp$hhsize)
table(g0k)

sum(hh_temp_resp$wac)
sum(hh_temp_resp$wcal)

plot(hh_temp_resp$wac,
     hh_temp_resp$wcal,
     xlab = "wac",
     ylab = "wcal")
abline(a = 0, b = 1, col = 2)


# PS + Calibration --------------------------------------------------------

plot(hh_temp_resp$w1ps, hh_temp_resp$wcal)
abline(a = 0, b = 1, col = 2)
hist(hh_temp_resp$w1ps)
hist(hh_temp_resp$wcal)

g1k <- calib(x0s,
             d = hh_temp_resp$w1ps,
             total = tx0,
             method = "linear")

hh_temp_resp$wpscal <- hh_temp_resp$w1ps * g1k

sum(hh_temp_resp$w0)
sum(hh_temp_resp$w1ps)
sum(hh_temp_resp$wcal)
sum(hh_temp_resp$wpscal)

plot(hh_temp_resp$w1ps,
     hh_temp_resp$wpscal,
     ylab = "wpscal",
     xlab = "wps")
abline(a = 0, b = 1, col = 2)
hist(hh_temp_resp$w1ps)
hist(hh_temp_resp$wpscal)

summary(hh_temp_resp$w1ps)


# Final plots -------------------------------------------------------------

dataweights <- hh_temp_resp %>%
  select(w0, wac, w1ps, wcal, wpscal)

cor(dataweights)
pairs.panels(dataweights)
