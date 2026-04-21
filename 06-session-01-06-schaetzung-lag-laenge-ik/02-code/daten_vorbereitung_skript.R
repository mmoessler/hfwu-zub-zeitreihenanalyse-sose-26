
# Lade Pakete
library(here)

# Einlesen der Daten.
us_macro <- read.table(here("04-session-01-04-adl-modelle", "01-daten", "us_macro_quarterly_merged.csv"),
                       header = TRUE,
                       sep = ";"
)

# Umwandlung in ein `ts` Object.
us_macro_ts <- ts(
  us_macro,
  frequency = 4,
  start = c(1950, 1),
  end = c(2026, 1)
)

us_macro_ts <- window(us_macro_ts,
                      start = c(1955, 1),
                      end = c(2017, 4)
)

# Berechnung der annualisierten Wachstumsrate.
GDP <- us_macro_ts[,"GDPC1"]
GDPGR <- 400 * log(GDP/lag(GDP, -1))

# Berechnung der Zinsspanne.
TSpread <- us_macro_ts[,"GS10"] - us_macro_ts[,"TB3MS"]

# Anbindung der annualisierten Wachstumsrate an `us_macro_ts`.

# Anbindung
us_macro_ts <- cbind(us_macro_ts, GDPGR, TSpread)

colnames(us_macro_ts) <- sub(".*\\.", "", colnames(us_macro_ts))
