
# Lade Pakete
library(here)
library(R.matlab)


# Einlesen der Daten.
us_macro <- read.table(here("11-session-03-03-df-modelle", "01-daten", "us_macro_quarterly_merged.csv"),
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




#..................................................
# Load all observable macro variables: The data are provided by S&W in a matlab matrix, datain.mat

tmp <- readMat(con = here("11-session-03-03-df-modelle", "01-daten", "datain.mat"))

bpdata_raw <- tmp$datain[1,1,1]$bpdata.raw
bpdata <- tmp$datain[2,1,1]$bpdata # use this for estimation
bpdata_noa <- tmp$datain[3,1,1]$bpdata.noa
bpnamevec <- tmp$datain[4,1,1]$bpnamevec
bplabvec.long <- tmp$datain[5,1,1]$bplabvec.long
bplabvec.short <- tmp$datain[6,1,1]$bplabvec.short
bpoutliervec <- tmp$datain[7,1,1]$bpoutliervec
bptcodevec <- tmp$datain[8,1,1]$bptcodevec
bpcatcode <- tmp$datain[9,1,1]$bpcatcode
bpinclcode <- tmp$datain[10,1,1]$bpinclcode 
calvec <- tmp$datain[11,1,1]$calvec
calds <- tmp$datain[12,1,1]$calds
dnobs <- tmp$datain[13,1,1]$dnobs

# Transform to matrix used for estimation
macro.02.dat <- bpdata

# Normalize missing values: use NA instead of NaN
macro.02.dat <- as.matrix(macro.02.dat)
macro.02.dat[is.nan(macro.02.dat)] <- NA

# Extract labels
col_names <- sapply(bplabvec.short, function(x) x[[1]][1, 1])

stopifnot(length(col_names) == ncol(macro.02.dat))

# Optional: keep original labels for documentation
colnames(macro.02.dat) <- col_names

# Check
sum(is.nan(macro.02.dat))
sum(is.na(macro.02.dat))

# Save exact R object for computation
saveRDS(
  macro.02.dat,
  here("11-session-03-03-df-modelle", "01-daten", "us_macro_quarterly_big.rds")
)

# Save CSV for inspection / teaching
write.table(
  format(macro.02.dat, digits = 17, scientific = TRUE),
  here("11-session-03-03-df-modelle", "01-daten", "us_macro_quarterly_big.csv"),
  row.names = FALSE,
  col.names = TRUE,
  sep = ";",
  quote = FALSE,
  na = "NA"
)
