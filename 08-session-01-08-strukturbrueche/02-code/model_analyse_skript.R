
#..................................................
# Set-Up ----

# Lade here Paket
library(here)

# Säubere Umgebung
rm(list=ls())

# Lade Pakete
library(zoo)
library(dynlm)
library(sandwich)
library(lmtest)

# Lade helper functions
source(here("05-session-01-05-schaetzung-msfe-pi", "02-code", "poos_funktion.R"))

#..................................................
# Einlesen der Daten ----
us_macro_qua <- read.table(
  here("05-session-01-05-schaetzung-msfe-pi", "01-daten", "us_macro_quarterly_merged.csv"),
  sep = ";",
  header = TRUE)

# Umwandeln der Datumsvariablen
us_macro_qua$datum_tag <- as.Date(us_macro_qua$datum_tag)
us_macro_qua$datum_qtr <- as.yearqtr(us_macro_qua$datum_tag)

# Umwandlung in ein ts Objekt
us_macro_qua_ts <- ts(us_macro_qua, frequency = 4, start = c(1950, 1), end = c(2026, 1))

# Berechnung der Wachstumsrate
GDPGR <- 400 * log(us_macro_qua_ts[,"GDPC1"]/stats::lag(us_macro_qua_ts[,"GDPC1"], -1))

# Berechnung der Zinsspanne (Term Spread)
TSpread <- us_macro_qua_ts[,"GS10"] - us_macro_qua_ts[,"TB3MS"]

us_macro_qua_ts <- cbind(us_macro_qua_ts, GDPGR, TSpread)

colnames(us_macro_qua_ts) <- sub(".*\\.", "", colnames(us_macro_qua_ts))

#..................................................
# Schätzung Modelle ----

# Schätzung AR(1) Model: 1962-Q1 - 2017-Q3
mean_dynlm <- dynlm(GDPGR ~ 1,
                     data = us_macro_qua_ts,
                     start = c(1962, 1), end = c(2017, 3))
summary(mean_dynlm)

# Schätzung AR(1) Model: 1962-Q1 - 2017-Q3
ar_01_dynlm <- dynlm(GDPGR ~ L(GDPGR,1),
                     data = us_macro_qua_ts,
                     start = c(1962, 1), end = c(2017, 3))
summary(ar_01_dynlm)

# Schätzung AR(2) Model: 1962-Q1 - 2017-Q3
ar_02_dynlm <- dynlm(GDPGR ~ L(GDPGR,1) + L(GDPGR,2),
                     data = us_macro_qua_ts,
                     start = c(1962, 1), end = c(2017, 3))
summary(ar_02_dynlm)

# Schätzung ADL(2,1) Model: 1962-Q1 - 2017-Q3
adl_0201_dynlm <- dynlm(GDPGR ~ L(GDPGR,1) + L(GDPGR,2) + L(TSpread,1),
                        data = us_macro_qua_ts,
                        start = c(1962, 1), end = c(2017, 3))
summary(adl_0201_dynlm)

# Schätzung ADL(2,2) Model: 1962-Q1 - 2017-Q3
adl_0202_dynlm <- dynlm(GDPGR ~ L(GDPGR,1) + L(GDPGR,2) + L(TSpread,1) + L(TSpread,2),
                        data = us_macro_qua_ts,
                        start = c(1962, 1), end = c(2017, 3))
summary(adl_0202_dynlm)

#..................................................
# MSFE ----

# MSFE_SER

mean_rmsfe_ser <- summary(mean_dynlm)$sigma
ar_01_rmsfe_ser <- summary(ar_01_dynlm)$sigma
ar_02_rmsfe_ser <- summary(ar_02_dynlm)$sigma
adl_0201_rmsfe_ser <- summary(adl_0201_dynlm)$sigma
adl_0202_rmsfe_ser <- summary(adl_0202_dynlm)$sigma

# MSFE_FPE

mean_rmsfe_fpe <- sqrt((nrow(mean_dynlm$model) + length(mean_dynlm$coefficients)) / (nrow(mean_dynlm$model) - length(mean_dynlm$coefficients)) * sum(mean_dynlm$residuals^2) / nrow(mean_dynlm$model))
ar_01_rmsfe_fpe <- sqrt((nrow(ar_01_dynlm$model) + length(ar_01_dynlm$coefficients)) / (nrow(ar_01_dynlm$model) - length(ar_01_dynlm$coefficients)) * sum(ar_01_dynlm$residuals^2) / nrow(ar_01_dynlm$model))
ar_02_rmsfe_fpe <- sqrt((nrow(ar_02_dynlm$model) + length(ar_02_dynlm$coefficients)) / (nrow(ar_02_dynlm$model) - length(ar_02_dynlm$coefficients)) * sum(ar_02_dynlm$residuals^2) / nrow(ar_02_dynlm$model))
adl_0201_rmsfe_fpe <- sqrt((nrow(adl_0201_dynlm$model) + length(adl_0201_dynlm$coefficients)) / (nrow(adl_0201_dynlm$model) - length(adl_0201_dynlm$coefficients)) * sum(adl_0201_dynlm$residuals^2) / nrow(adl_0201_dynlm$model))
adl_0202_rmsfe_fpe <- sqrt((nrow(adl_0202_dynlm$model) + length(adl_0202_dynlm$coefficients)) / (nrow(adl_0202_dynlm$model) - length(adl_0202_dynlm$coefficients)) * sum(adl_0202_dynlm$residuals^2) / nrow(adl_0202_dynlm$model))

# MSFE_POOS
data.all.ts <- us_macro_qua_ts

# extract time period of interest
data.all.ts <- window(data.all.ts, start = c(1959, 3), end = c(2017, 4))
# corresponding date
date <- seq.Date(from = as.Date("1959-07-01"), to = as.Date("2017-10-01"), by = "quarter")

# dates data frame (date column is important!)
date.df <- data.frame(date = date,
                      year = format(date, "%Y"),
                      quarter = quarters(date),
                      month = format(date, "%m"),
                      day = format(date, "%d"))

# target variable (data frame)
GDP <- as.numeric(data.all.ts[,4])
h <- 1

data.01 <- data.frame(GDPGR_h = (400/h) * (log(GDP / lag_fun(GDP, h))))

# observed predictors (variables) (data frame)
data.02 <- data.frame(GDPGR_h_l1 = lag_fun(log(GDP / lag_fun(GDP, 1)), h),
                      CONST = 1)

# other inputs
h <- 1
fre <- "quarter"
all.per.sta <- as.Date("1962-01-01") # 1962-Q1 -> 2017-Q3
all.per.end <- as.Date("2017-10-01")
est.per.sta <- as.Date("1962-01-01") # 1962-Q1 -> 2006-Q4
est.per.end <- as.Date("2006-10-01") 
pre.per.sta <- as.Date("2006-10-01") # 2006-Q4 -> 2017-Q3
pre.per.end <- as.Date("2017-10-01")
print <- FALSE
# log.file <- rprojroot::find_rstudio_root_file("05-ergebnisse/ar_01_poos_diagnosen.txt")
log.file <-  NULL

AR1.POOS.h1 <- POOS_function(date.df = date.df, data.01 = data.01, data.02 = data.02,
                             h = 1, fre = "quarter",
                             all.per.sta = all.per.sta, all.per.end = all.per.end,
                             est.per.sta = est.per.sta, est.per.end = est.per.end,
                             pre.per.sta = pre.per.sta, pre.per.end = pre.per.end,
                             print = TRUE, log.file = log.file)
ar_01_rmsfe_poos <-AR1.POOS.h1$RMSFE.POOS



# observed predictors (variables) (data frame)
data.02 <- data.frame(GDPGR_h_l1 = lag_fun(log(GDP / lag_fun(GDP, 1)), h),
                      GDPGR_h_l2 = lag_fun(log(GDP / lag_fun(GDP, 1)), h + 1),
                      CONST = 1)

AR2.POOS.h1 <- POOS_function(date.df = date.df, data.01 = data.01, data.02 = data.02,
                             h = 1, fre = "quarter",
                             all.per.sta = all.per.sta, all.per.end = all.per.end,
                             est.per.sta = est.per.sta, est.per.end = est.per.end,
                             pre.per.sta = pre.per.sta, pre.per.end = pre.per.end,
                             print = TRUE, log.file = log.file)
ar_02_rmsfe_poos <- AR2.POOS.h1$RMSFE.POOS



# additional variable (data frame)
TSpread <- as.numeric(data.all.ts[,20])

# observed predictors (variables) (data frame)
data.02 <- data.frame(GDPGR_h_l1 = lag_fun(log(GDP / lag_fun(GDP, 1)), h),
                      GDPGR_h_l2 = lag_fun(log(GDP / lag_fun(GDP, 1)), h + 1),
                      TSpread_h_l1 = lag_fun(TSpread, h),
                      CONST = 1)

ADL0201.POOS.h1 <- POOS_function(date.df = date.df, data.01 = data.01, data.02 = data.02,
                             h = 1, fre = "quarter",
                             all.per.sta = all.per.sta, all.per.end = all.per.end,
                             est.per.sta = est.per.sta, est.per.end = est.per.end,
                             pre.per.sta = pre.per.sta, pre.per.end = pre.per.end,
                             print = TRUE, log.file = log.file)
adl_0201_rmsfe_poos <- ADL0201.POOS.h1$RMSFE.POOS
adl_0201_rmsfe_poos



# observed predictors (variables) (data frame)
data.02 <- data.frame(GDPGR_h_l1 = lag_fun(log(GDP / lag_fun(GDP, 1)), h),
                      GDPGR_h_l2 = lag_fun(log(GDP / lag_fun(GDP, 1)), h + 1),
                      TSpread_h_l1 = lag_fun(TSpread, h),
                      TSpread_h_l2 = lag_fun(TSpread, h + 1),
                      CONST = 1)

ADL0202.POOS.h1 <- POOS_function(date.df = date.df, data.01 = data.01, data.02 = data.02,
                                 h = 1, fre = "quarter",
                                 all.per.sta = all.per.sta, all.per.end = all.per.end,
                                 est.per.sta = est.per.sta, est.per.end = est.per.end,
                                 pre.per.sta = pre.per.sta, pre.per.end = pre.per.end,
                                 print = TRUE, log.file = log.file)
adl_0202_rmsfe_poos <- ADL0202.POOS.h1$RMSFE.POOS
adl_0202_rmsfe_poos



# observed predictors (variables) (data frame)
data.02 <- data.frame(CONST = 1)

MEAN.POOS.h1 <- POOS_function(date.df = date.df, data.01 = data.01, data.02 = data.02,
                                 h = 1, fre = "quarter",
                                 all.per.sta = all.per.sta, all.per.end = all.per.end,
                                 est.per.sta = est.per.sta, est.per.end = est.per.end,
                                 pre.per.sta = pre.per.sta, pre.per.end = pre.per.end,
                                 print = TRUE, log.file = log.file)
mean_rmsfe_poos <- MEAN.POOS.h1$RMSFE.POOS
mean_rmsfe_poos


# Ergebnisse ----
res.df <- data.frame(
  SER = c(mean_rmsfe_ser, ar_01_rmsfe_ser, ar_02_rmsfe_ser, adl_0201_rmsfe_ser, adl_0202_rmsfe_ser),
  FPE = c(mean_rmsfe_fpe, ar_01_rmsfe_fpe, ar_02_rmsfe_fpe, adl_0201_rmsfe_fpe, adl_0202_rmsfe_fpe),
  POOS = c(mean_rmsfe_poos, ar_01_rmsfe_poos, ar_02_rmsfe_poos, adl_0201_rmsfe_poos, adl_0202_rmsfe_poos)
  )

rownames(res.df) <- c("MEAN", "AR(1)", "AR(2)", "ADL(2,1)", "ADL(2,2)")

res.df
