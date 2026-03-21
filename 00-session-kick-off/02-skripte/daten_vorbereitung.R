library(readxl)
library(here)
library(zoo)

us_macro_qua <- read_xlsx(
  here("01-daten", "USMacro_Monthly_Quarterly", "us_macro_quarterly.xlsx")
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