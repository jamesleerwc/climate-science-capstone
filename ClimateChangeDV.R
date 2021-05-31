# Packages

library(here)
library(tidyverse)
library(ggpubr)

# Load and clean data

datalikert <- read_csv(here("data", "datalikert.csv"))
datalikert

###################

# Compute medians and IQRs for each Likert scale statement:

datmedians <- t(datalikert %>%
  group_by(group) %>%
  summarise(
    count = n(),
    median1 = median(quest1, na.rm = TRUE),
    IQR1 = IQR(quest1, na.rm = TRUE),
    median2 = median(quest2, na.rm = TRUE),
    IQR2 = IQR(quest2, na.rm = TRUE),
    median3 = median(quest3, na.rm = TRUE),
    IQR3 = IQR(quest3, na.rm = TRUE),
    median4 = median(quest4, na.rm = TRUE),
    IQR4 = IQR(quest4, na.rm = TRUE),
    median5 = median(quest5, na.rm = TRUE),
    IQR5 = IQR(quest5, na.rm = TRUE),
    median6 = median(quest6, na.rm = TRUE),
    IQR6 = IQR(quest6, na.rm = TRUE),
    median7 = median(quest7, na.rm = TRUE),
    IQR7 = IQR(quest7, na.rm = TRUE),
    median8 = median(quest8, na.rm = TRUE),
    IQR8 = IQR(quest8, na.rm = TRUE),
    median9 = median(quest9, na.rm = TRUE),
    IQR9 = IQR(quest9, na.rm = TRUE)
  ))
datmedians <- datmedians[, c(2,1)]
datmedians

# Compute the differences:

d1 <- with(datalikert, quest1[group == "after"] - quest1[group == "before"])
d2 <- with(datalikert, quest2[group == "after"] - quest2[group == "before"])
d3 <- with(datalikert, quest3[group == "after"] - quest3[group == "before"])
d4 <- with(datalikert, quest4[group == "after"] - quest4[group == "before"])
d5 <- with(datalikert, quest5[group == "after"] - quest5[group == "before"])
d6 <- with(datalikert, quest6[group == "after"] - quest6[group == "before"])
d7 <- with(datalikert, quest7[group == "after"] - quest7[group == "before"])
d8 <- with(datalikert, quest8[group == "after"] - quest8[group == "before"])
d9 <- with(datalikert, quest9[group == "after"] - quest9[group == "before"])

# Shapiro-Wilk normality test for the differences:

shapiro.test(d1) ## p-value = 0.000116   ## Use Wilcoxon signed-rank test
shapiro.test(d2) ## p-value = 0.003767   ## Use Wilcoxon signed-rank test
shapiro.test(d3) ## p-value = 0.03487    ## Use Wilcoxon signed-rank test
shapiro.test(d4) ## p-value = 0.3826     ## Can use t-test
shapiro.test(d5) ## p-value = 0.1951     ## Can use t-test
shapiro.test(d6) ## p-value = 0.0005224  ## Use Wilcoxon signed-rank test
shapiro.test(d7) ## p-value = 4.025e-06  ## Use Wilcoxon signed-rank test
shapiro.test(d8) ## p-value = 0.002736   ## Use Wilcoxon signed-rank test
shapiro.test(d9) ## p-value = 0.1374     ## Can use t-test

# Compute t-tests where allowable:

ptt4 <- t.test(quest4 ~ group, data = datalikert, paired = TRUE)
ptt5 <- t.test(quest5 ~ group, data = datalikert, paired = TRUE)
ptt9 <- t.test(quest9 ~ group, data = datalikert, paired = TRUE)

ptt4   ## p-value = 0.005961  ## True difference in means is not equal to 0
ptt5   ## p-value = 0.0001305 ## True difference in means is not equal to 0
ptt9   ## p-value = 0.02117   ## True difference in means is not equal to 0

# Compute Wilcoxon signed-rank tests:

wsr1 <- wilcox.test(quest1 ~ group, data = datalikert, paired = TRUE)
wsr2 <- wilcox.test(quest2 ~ group, data = datalikert, paired = TRUE)
wsr3 <- wilcox.test(quest3 ~ group, data = datalikert, paired = TRUE)
wsr4 <- wilcox.test(quest4 ~ group, data = datalikert, paired = TRUE)
wsr5 <- wilcox.test(quest5 ~ group, data = datalikert, paired = TRUE)
wsr6 <- wilcox.test(quest6 ~ group, data = datalikert, paired = TRUE)
wsr7 <- wilcox.test(quest7 ~ group, data = datalikert, paired = TRUE)
wsr8 <- wilcox.test(quest8 ~ group, data = datalikert, paired = TRUE)
wsr9 <- wilcox.test(quest9 ~ group, data = datalikert, paired = TRUE)

wsr1   ## p-value = 0.03689   ## True location shift is not equal to 0
wsr2   ## p-value = 0.1294
wsr3   ## p-value = 0.01199   ## True location shift is not equal to 0
wsr4   ## p-value = 0.01214   ## True location shift is not equal to 0
wsr5   ## p-value = 0.003583  ## True location shift is not equal to 0
wsr6   ## p-value - 0.02627   ## True location shift is not equal to 0
wsr7   ## p-value = 0.3458
wsr8   ## p-value = 0.233
wsr9   ## p-value = 0.03301   ## True location shift is not equal to 0
