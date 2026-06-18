library(readxl)
library(here)
library(zoo)

# ..............................
# Quartalsfrequenz ----

# Rohdaten: https://www.princeton.edu/~mwatson/Stock-Watson_4E/Stock-Watson-Resources-4e.html

# Laden der Rohodaten
us_macro_qua <- read_xlsx(
  here("01-session-01-01-einfuehrung", "01-daten", "USMacro_Monthly_Quarterly_raw", "us_macro_quarterly.xlsx")
)

# Umwandlung in Zeitreihe
us_macro_qua_ts <- ts(
  us_macro_qua,
  frequency = 4,
  start = c(1955, 1),
  end = c(2017, 4)
)

# Quartalsweise Datumssequenz als Basis für monatliche Zeitreihe
qua_date_seq <- seq.Date(
  from = as.Date("1950-01-01"),
  to   = as.Date("2026-01-01"),
  by   = "quarter"
)

qua_date_seq_ts <- ts(
  cbind(
    qua_date_seq,
    as.character(qua_date_seq),
    as.character(as.yearqtr(qua_date_seq))
  ),
  frequency = 4,
  start = c(1950, 1),
  end = c(2026, 1)
)

colnames(qua_date_seq_ts) <- c("datum_num", "datum_tag", "datum_qtr")

# ..............................
# Monatsfrequenz ----

us_macro_mon <- read_xlsx(
  here("01-session-01-01-einfuehrung", "01-daten", "USMacro_Monthly_Quarterly_raw", "us_macro_monthly.xlsx")
)

# Umwandlung in Zeitreihe
us_macro_mon_ts <- ts(
  us_macro_mon,
  frequency = 12,
  start = c(1955, 1),
  end = c(2017, 12)
)

# Monatliche Datumssequenz als Basis für monatliche Zeitreihe
mon_date_seq <- seq.Date(
  from = as.Date("1950-01-01"),
  to = as.Date("2026-01-01"),
  by = "month"
)

mon_date_seq <- ts(
  mon_date_seq,
  frequency = 12,
  start = c(1950, 1),
  end = c(2026,1)
)

# ..............................
# Zusammenführen von Quartals- und Monatsdaten ----

# 1. Gleitender 3-Monats-Durchschnitt (rückblickend)
mon_roll <- rollapply(
  us_macro_mon_ts,
  width = 3,
  FUN = mean,
  align = "right"
)

# 2. Umwandlung in quartalsweise Zeitreihe und Ausrichtung auf Quartalsdaten
us_macro_mon_qua_aligned <- ts(
  mon_roll[cycle(mon_roll) %in% c(3, 6, 9, 12),],
  frequency = 4,
  start = c(1955, 1)
)

# 3. Zusammenführen beider Datensätze
us_macro_qua_mon_ts <- cbind(qua_date_seq_ts, 
                             us_macro_qua_ts[, !(colnames(us_macro_qua_ts) %in% c("freq"))], 
                             us_macro_mon_qua_aligned[, !(colnames(us_macro_mon_qua_aligned) %in% c("freq", "CPIAUCSL"))])

colnames(us_macro_qua_mon_ts) <- c("datum_num", "datum_tag", "datum_qtr", "GDPC1", "JAPAN_IP",
                                   "PCECTPI", "CPIAUCSL", "EXUSUK", "FEDFUNDS", "GS1", "GS10",
                                   "INDPRO", "PCEPI", "TB3MS", "UNRATE", "WPU0561", "PAYEMS", "DJIA")

# 4. Typsicheres Zusammenführen in einem data frame
us_macro_qua_mon_df <- data.frame(
  datum_num = as.numeric(us_macro_qua_mon_ts[,"datum_num"]),
  datum_tag = as.character(us_macro_qua_mon_ts[,"datum_tag"]),
  datum_qtr = as.character(us_macro_qua_mon_ts[,"datum_qtr"]),
  GDPC1 = as.numeric(us_macro_qua_mon_ts[,"GDPC1"]),
  JAPAN_IP = as.numeric(us_macro_qua_mon_ts[,"JAPAN_IP"]),
  PCECTPI = as.numeric(us_macro_qua_mon_ts[,"PCECTPI"]),
  CPIAUCSL = as.numeric(us_macro_qua_mon_ts[,"CPIAUCSL"]),
  EXUSUK = as.numeric(us_macro_qua_mon_ts[,"EXUSUK"]),
  FEDFUNDS = as.numeric(us_macro_qua_mon_ts[,"FEDFUNDS"]),
  GS1 = as.numeric(us_macro_qua_mon_ts[,"GS1"]),
  GS10 = as.numeric(us_macro_qua_mon_ts[,"GS10"]),
  INDPRO = as.numeric(us_macro_qua_mon_ts[,"INDPRO"]),
  PCEPI = as.numeric(us_macro_qua_mon_ts[,"PCEPI"]),
  TB3MS = as.numeric(us_macro_qua_mon_ts[,"TB3MS"]),
  UNRATE = as.numeric(us_macro_qua_mon_ts[,"UNRATE"]),
  WPU0561 = as.numeric(us_macro_qua_mon_ts[,"WPU0561"]),
  PAYEMS = as.numeric(us_macro_qua_mon_ts[,"PAYEMS"]),
  DJIA = as.numeric(us_macro_qua_mon_ts[,"DJIA"])
)

# 4. Speichern als CSV
write.table(us_macro_qua_mon_df,
            here("01-session-01-01-einfuehrung", "01-daten", "us_macro_quarterly_merged.csv"),
            row.names = FALSE,
            sep = ";"
)
