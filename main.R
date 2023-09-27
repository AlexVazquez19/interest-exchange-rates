# load libraries
library(readr)
library(collapse)
library(lubridate)
library(forecast)
library(xts)
library(dplyr)
library(ggpubr)
library(ggplot2)

# clear environment
rm(list = ls())



## LOAD DATA

# load federal funds rate data
fedfunds <- read_csv("FEDFUNDS.csv")

# load exchange rate data
CAUS <- read_csv("DEXCAUS.csv")
MXUS <- read_csv("DEXMXUS.csv")
CHUS <- read_csv("DEXCHUS.csv")
JPUS <- read_csv("DEXJPUS.csv")
UKUS <- read_csv("DEXUSUK.csv")



## CLEAN/TRANSFORM DATA

# remove the dates where the exchange rate was not recorded (indicated by '.')
CAUS <- subset(CAUS, CAUS$DEXCAUS!='.')
MXUS <- subset(MXUS, MXUS$DEXMXUS!='.')
CHUS <- subset(CHUS, CHUS$DEXCHUS!='.')
JPUS <- subset(JPUS, JPUS$DEXJPUS!='.')
UKUS <- subset(UKUS, UKUS$DEXUSUK!='.')

# convert fed funds rate to double type
fedfunds$FEDFUNDS <- as.double(fedfunds$FEDFUNDS)

# convert exchange rate to double type
CAUS$DEXCAUS <- as.double(CAUS$DEXCAUS)
MXUS$DEXMXUS <- as.double(MXUS$DEXMXUS)
CHUS$DEXCHUS <- as.double(CHUS$DEXCHUS)
JPUS$DEXJPUS <- as.double(JPUS$DEXJPUS)
UKUS$DEXUSUK <- as.double(UKUS$DEXUSUK)

# convert date columns to date type
fedfunds$DATE <- as.Date(fedfunds$DATE)
CAUS$DATE <- as.Date(CAUS$DATE)
MXUS$DATE <- as.Date(MXUS$DATE)
CHUS$DATE <- as.Date(CHUS$DATE)
JPUS$DATE <- as.Date(JPUS$DATE)
UKUS$DATE <- as.Date(UKUS$DATE)

# add new column for month
CAUS$month <- floor_date(CAUS$DATE, "month")
MXUS$month <- floor_date(MXUS$DATE, "month")
CHUS$month <- floor_date(CHUS$DATE, "month")
JPUS$month <- floor_date(JPUS$DATE, "month")
UKUS$month <- floor_date(UKUS$DATE, "month")

# aggregate exchange rate data by month
CAUS_monthly = CAUS %>% group_by(month) %>% summarise('MEAN DEXCAUS' = mean(DEXCAUS))
MXUS_monthly = MXUS %>% group_by(month) %>% summarise('MEAN DEXMXUS' = mean(DEXMXUS))
CHUS_monthly = CHUS %>% group_by(month) %>% summarise('MEAN DEXCHUS' = mean(DEXCHUS))
JPUS_monthly = JPUS %>% group_by(month) %>% summarise('MEAN DEXJPUS' = mean(DEXJPUS))
UKUS_monthly = UKUS %>% group_by(month) %>% summarise('MEAN DEXUSUK' = mean(DEXUSUK))

# add column for percent change
CAUS_monthly <- fmutate(CAUS_monthly, pct_change = fgrowth(CAUS_monthly$`MEAN DEXCAUS`))
MXUS_monthly <- fmutate(MXUS_monthly, pct_change = fgrowth(MXUS_monthly$`MEAN DEXMXUS`))
CHUS_monthly <- fmutate(CHUS_monthly, pct_change = fgrowth(CHUS_monthly$`MEAN DEXCHUS`))
JPUS_monthly <- fmutate(JPUS_monthly, pct_change = fgrowth(JPUS_monthly$`MEAN DEXJPUS`))
UKUS_monthly <- fmutate(UKUS_monthly, pct_change = fgrowth(UKUS_monthly$`MEAN DEXUSUK`))

# add column with min-max normalized value for fed funds rate
fedfunds$min_max <- (fedfunds$FEDFUNDS-min(fedfunds$FEDFUNDS)) /
  (max(fedfunds$FEDFUNDS)-min(fedfunds$FEDFUNDS))

# add column with min-max normalized value for exchange rate
CAUS_monthly$min_max <- (CAUS_monthly$`MEAN DEXCAUS`-min(CAUS_monthly$`MEAN DEXCAUS`)) /
  (max(CAUS_monthly$`MEAN DEXCAUS`)-min(CAUS_monthly$`MEAN DEXCAUS`))
MXUS_monthly$min_max <- (MXUS_monthly$`MEAN DEXMXUS`-min(MXUS_monthly$`MEAN DEXMXUS`)) /
  (max(MXUS_monthly$`MEAN DEXMXUS`)-min(MXUS_monthly$`MEAN DEXMXUS`))
CHUS_monthly$min_max <- (CHUS_monthly$`MEAN DEXCHUS`-min(CHUS_monthly$`MEAN DEXCHUS`)) /
  (max(CHUS_monthly$`MEAN DEXCHUS`)-min(CHUS_monthly$`MEAN DEXCHUS`))
JPUS_monthly$min_max <- (JPUS_monthly$`MEAN DEXJPUS`-min(JPUS_monthly$`MEAN DEXJPUS`)) /
  (max(JPUS_monthly$`MEAN DEXJPUS`)-min(JPUS_monthly$`MEAN DEXJPUS`))
UKUS_monthly$min_max <- (UKUS_monthly$`MEAN DEXUSUK`-min(UKUS_monthly$`MEAN DEXUSUK`)) /
  (max(UKUS_monthly$`MEAN DEXUSUK`)-min(UKUS_monthly$`MEAN DEXUSUK`))



## FILTER DATA

# filter fed funds dates to align with exchange rate dates
fedfunds_ca <- fedfunds %>% filter(DATE >= as.Date("1971-01-01"))
fedfunds_mx <- fedfunds %>% filter(DATE >= as.Date("1993-11-01"))
fedfunds_ch <- fedfunds %>% filter(DATE >= as.Date("1981-01-01"))
fedfunds_jp <- fedfunds %>% filter(DATE >= as.Date("1971-01-01"))
fedfunds_uk <- fedfunds %>% filter(DATE >= as.Date("1971-01-01"))

# filter DEX data to align dates with fed funds rate dates
CAUS_monthly <- CAUS_monthly %>% filter(month <= as.Date("2023-08-01"))
MXUS_monthly <- MXUS_monthly %>% filter(month <= as.Date("2023-08-01"))
CHUS_monthly <- CHUS_monthly %>% filter(month <= as.Date("2023-08-01"))
JPUS_monthly <- JPUS_monthly %>% filter(month <= as.Date("2023-08-01"))
UKUS_monthly <- UKUS_monthly %>% filter(month <= as.Date("2023-08-01"))



## BOXPLOTS

# create boxplot for the exchange rate data to see if there are outliers.
# no outliers means the min-max normalization will not magnify single points
# and mess up the data.

boxplot(CAUS_monthly$`MEAN DEXCAUS`) # no outliers
boxplot(MXUS_monthly$`MEAN DEXMXUS`) # no outliers
boxplot(CHUS_monthly$`MEAN DEXCHUS`) # no outliers
boxplot(JPUS_monthly$`MEAN DEXJPUS`) # no outliers
boxplot(UKUS_monthly$`MEAN DEXUSUK`) # includes outliers



## PEARSON CORRELATION

# Null hypothesis: the true correlation between the fed funds rate and exchange 
# rate is zero.

# Canada
COR_CAUSFF = cor.test(fedfunds_ca$FEDFUNDS, CAUS_monthly$`MEAN DEXCAUS`, method="pearson")
plot(fedfunds_ca$DATE, fedfunds_ca$min_max, type='l')
lines(CAUS_monthly$month, CAUS_monthly$min_max, col='orange')
COR_CAUSFF # not statistically significant

# Mexico
COR_MXUSFF = cor.test(fedfunds_mx$FEDFUNDS, MXUS_monthly$`MEAN DEXMXUS`, method="pearson")
plot(fedfunds$DATE, fedfunds$min_max, type='l')
lines(MXUS_monthly$month, MXUS_monthly$min_max, col='orange')
COR_MXUSFF # strong negative correlation, statistically significant

# China
COR_CHUSFF = cor.test(fedfunds_ch$FEDFUNDS, CHUS_monthly$`MEAN DEXCHUS`, method="pearson")
plot(fedfunds_ch$DATE, fedfunds_ch$min_max, type='l')
lines(CHUS_monthly$month, CHUS_monthly$min_max, col='orange')
COR_CHUSFF # strong negative correlation, statistically significant

# Japan
COR_JPUSFF = cor.test(fedfunds_jp$FEDFUNDS, JPUS_monthly$`MEAN DEXJPUS`, method="pearson")
plot(fedfunds_jp$DATE, fedfunds_jp$min_max, type='l')
lines(JPUS_monthly$month, JPUS_monthly$min_max, col='orange')
COR_JPUSFF # strong positive correlation, statistically significant

# United Kingdom
COR_UKUSFF = cor.test(fedfunds_uk$FEDFUNDS, UKUS_monthly$`MEAN DEXUSUK`, method="pearson")
plot(fedfunds_uk$DATE, fedfunds_uk$min_max, type='l')
lines(UKUS_monthly$month, UKUS_monthly$min_max, col='orange')
COR_UKUSFF # medium positive correlation, statistically significant



## SCATTER PLOTS

# visualize the fed funds rate vs the exchange rates to see if there is a trend

scatter.smooth(fedfunds_ca$FEDFUNDS,CAUS_monthly$`MEAN DEXCAUS`)
scatter.smooth(fedfunds_mx$FEDFUNDS,MXUS_monthly$`MEAN DEXMXUS`)
scatter.smooth(fedfunds_ch$FEDFUNDS,CHUS_monthly$`MEAN DEXCHUS`)
scatter.smooth(fedfunds_jp$FEDFUNDS,JPUS_monthly$`MEAN DEXJPUS`)
scatter.smooth(fedfunds_uk$FEDFUNDS,UKUS_monthly$`MEAN DEXUSUK`)
  # Japan and China have a slight linear relation but otherwise the data is 
  # generally too scattered for it to be considered meaningful



## LINEAR REGRESSIONS

# Canada
model_ca <- lm(CAUS_monthly$`MEAN DEXCAUS` ~ fedfunds_ca$FEDFUNDS)
summary(model_ca)
ggplot(fedfunds_ca, aes(x = fedfunds_ca$FEDFUNDS, y = CAUS_monthly$`MEAN DEXCAUS`)) + 
  geom_point() + stat_smooth(method = "lm", col = "red")
  # Because the p-value associated with the fedfunds coefficient is significantly
  # higher than 0.05, the null hypothesis should not be rejected. This means there
  # is no observed effect of the fed funds rate on the us-canada exchange rate.

# Mexico
model_mx <- lm(MXUS_monthly$`MEAN DEXMXUS` ~ fedfunds_mx$FEDFUNDS)
summary(model_mx)
ggplot(fedfunds_mx, aes(x = fedfunds_mx$FEDFUNDS, y = MXUS_monthly$`MEAN DEXMXUS`)) + 
  geom_point() + stat_smooth(method = "lm", col = "red")
  # Because the p-value associated with the fedfunds coefficient is significantly
  # less than 0.05, we can say there is a statistically significant association
  # between the fed funds rate and the us-mexico exchange rate. However, the low
  # Multiple R-squared of 0.3393 tells us that the fed funds rate only explains
  # about 34% of the variation in the us-mexico exchange rate.

# China
model_ch <- lm(CHUS_monthly$`MEAN DEXCHUS` ~ fedfunds_ch$FEDFUNDS)
summary(model_ch)
ggplot(fedfunds_ch, aes(x = fedfunds_ch$FEDFUNDS, y = CHUS_monthly$`MEAN DEXCHUS`)) + 
  geom_point() + stat_smooth(method = "lm", col = "red")
  # Because the p-value associated with the fedfunds coefficient is significantly
  # less than 0.05, we can say there is a statistically significant association
  # between the fed funds rate and the us-china exchange rate. However, the
  # relatively low Multiple R-squared of 0.395 tells us that the fed funds rate 
  # only explains about 40% of the variation in the us-china exchange rate.

# Japan
model_jp <- lm(JPUS_monthly$`MEAN DEXJPUS` ~ fedfunds_jp$FEDFUNDS)
summary(model_jp)
ggplot(fedfunds_jp, aes(x = fedfunds_jp$FEDFUNDS, y = JPUS_monthly$`MEAN DEXJPUS`)) + 
  geom_point() + stat_smooth(method = "lm", col = "red")
  # Because the p-value associated with the fedfunds coefficient is significantly
  # less than 0.05, we can say there is a statistically significant association
  # between the fed funds rate and the us-japan exchange rate. However, the
  # low Multiple R-squared of 0.3417 tells us that the fed funds rate only
  # explains about 34% of the variation in the us-japan exchange rate.

# United Kingdom
model_uk <- lm(UKUS_monthly$`MEAN DEXUSUK` ~ fedfunds_uk$FEDFUNDS)
summary(model_uk)
ggplot(fedfunds_uk, aes(x = fedfunds_uk$FEDFUNDS, y = UKUS_monthly$`MEAN DEXUSUK`)) + 
  geom_point() + stat_smooth(method = "lm", col = "red")
  # Because the p-value associated with the fedfunds coefficient is significantly
  # less than 0.05, we can say there is a statistically significant association
  # between the fed funds rate and the us-uk exchange rate. However, the very
  # low Multiple R-squared of 0.1893 tells us that the fed funds rate only
  # explains about 19% of the variation in the us-uk exchange rate.



## RESIDUAL PLOTS











#define residuals
res <- resid(model)

#produce residual vs. fitted plot
plot(fitted(model), res)

#add a horizontal line at 0 
abline(0,0)

#create Q-Q plot for residuals
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res) 

# confounding variables
# - inflation rate
# - other country's interest rate
# https://www.dallasfed.org/research/economics/2023/0926
# https://stats.stackexchange.com/questions/207020/regression-with-skewed-data


# ## FORECAST
# 
# # forecast fed funds rate for next 12 months
# fedfunds_ts <- xts(fedfunds$FEDFUNDS, fedfunds$DATE)
# fc <- auto.arima(fedfunds_ts)
# plot(forecast(fc, 12))
