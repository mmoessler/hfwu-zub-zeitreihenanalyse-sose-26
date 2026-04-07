---
output: html_document
editor_options: 
  chunk_output_type: console
---

---

# Praxist-Teil Session 3:<br>Autoregression und das Autoregressive (AR) Modell

Dieses Dokument enthält den Praxis-Teil von Session 3: Autoregression und das Autoregressive (AR) Modell. 

---

## Setup


``` r
# Lade here Paket
library(here)

# Optionen Rendering
knitr::opts_knit$set(root.dir = here())
knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE,
                      warning = FALSE,
                      fig.align = "center",
                      fig.cap = "",
                      fig.height = 5,
                      fig.width = 8)

# Säubere Umgebung
rm(list=ls())

# Lade Packete
library(zoo)
library(dynlm)
library(sandwich)
library(lmtest)
```

---

## Datenaufbereitung

Einlesen der Daten.


``` r
us_macro <- read.table(here("03-session-01-03-ar-modelle", "01-daten", "us_macro_quarterly_merged.csv"),
                       header = TRUE,
                       sep = ";"
)
```

Umwandlung in ein `ts` Object.


``` r
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
```

Berechnung der annualisierten Wachstumsrate.


``` r
GDP <- us_macro_ts[,"GDPC1"]
GDPGR <- 400 * log(GDP/lag(GDP, -1))
```

Anbindung der annualisierten Wachstumsrate an `us_macro_ts`.


``` r
# An
us_macro_ts <- cbind(us_macro_ts, GDPGR)

colnames(us_macro_ts) <- sub(".*\\.", "", colnames(us_macro_ts))
```

---

### Frage 1

Welche Schritte umfasst die Aufbereitung der Daten?

...

...

...

...

...

---

## Darstellung der US BIP Daten


``` r
# Darstellung der BIP Variablen
par(mfrow = c(1,2))
plot(log(na.omit(us_macro_ts[,'GDPC1'])),
     col = "steelblue",
     lwd = 2,
     ylab = "Logarithmus",
     xlab = "Zeit",
     main = "Reales vierteljährliches US-BIP")
plot(na.omit(us_macro_ts[,'GDPGR']),
     col = "steelblue",
     lwd = 2,
     ylab = "Wachstumsrate",
     xlab = "Zeit",
     main = "Reales vierteljährliches US-BIP")
```

<img src="./03-ergebnisse/us_bip_daten-1.svg" alt="" style="display: block; margin: auto;" />

---

### Frage 2

Warum verwenden wir die Wachstumsrate des BIP anstatt das "normale" BIP?

...

...

...

...

...

---

## AR(1) Modell: Schätzung

Schätzung eines AR(1) Modells für 1962-Q1 - 2017-Q3


``` r
# Schätzung
ar01.dynlm <- dynlm(GDPGR ~ L(GDPGR,1),
                    data = us_macro_ts,
                    start = c(1962, 1), end = c(2017, 3))
summary(ar01.dynlm)
```

```
## 
## Time series regression with "ts" data:
## Start = 1962(1), End = 2017(3)
## 
## Call:
## dynlm(formula = GDPGR ~ L(GDPGR, 1), data = us_macro_ts, start = c(1962, 
##     1), end = c(2017, 3))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.5863  -1.5362   0.1331   1.7433  12.8338 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.95006    0.27785   7.018 2.72e-11 ***
## L(GDPGR, 1)  0.34084    0.06285   5.423 1.53e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.055 on 221 degrees of freedom
##   (0 observations deleted due to missingness)
## Multiple R-squared:  0.1174,	Adjusted R-squared:  0.1134 
## F-statistic: 29.41 on 1 and 221 DF,  p-value: 1.534e-07
```

---

### Frage 3

Was können wir von dem Ergebnis der Schätzung lernen?

...

...

...

...

...

---

## AR(1) Modell: Tests der Koeffizienten


``` r
# Tests
ct.ar01.dynlm <- coeftest(ar01.dynlm, vcov=vcovHC(ar01.dynlm, type="HC0"))
ct.ar01.dynlm
```

```
## 
## t test of coefficients:
## 
##             Estimate Std. Error t value  Pr(>|t|)    
## (Intercept) 1.950062   0.322372  6.0491 6.157e-09 ***
## L(GDPGR, 1) 0.340837   0.073009  4.6684 5.264e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

---

### Frage 4

Was können wir von dem Ergebnis der Tests der Koeffizienten lernen?

...

...

...

...

...

---

## AR(1) Modell Prognose

Verwendung des geschätzten AR(1)-Modells zur Prognose.


``` r
# Daten für die Prognose 
X1 <- matrix(rev(window(us_macro_ts,start=c(2017,3),end=c(2017,3))[,"GDPGR"]),ncol=1)
XX <- rbind(1, X1)
# AR(1) Koeffizienten
bet <- matrix(ar01.dynlm$coefficients,nrow=1)
# Prgnose für 2017 Q4
prog_erg <- bet %*% XX
prog_erg
```

```
##          [,1]
## [1,] 3.009081
```

``` r
# Tatsächlicher beobachteter Wert
tats_wert <- window(us_macro_ts[, "GDPGR"], start = c(2017, 4), end = c(2017, 4))
tats_wert
```

```
##          Qtr4
## 2017 2.504579
```

``` r
# Prognosefehler
prog_fehler <- tats_wert - prog_erg
prog_fehler
```

```
##            Qtr4
## 2017 -0.5045016
```

---

### Frage 5

Um welche Art von Prognose handelt es sich hier? Welche Schritte umfasst die Prognose basierend auf einem AR(1) Modell? Wie würden Sie die Prognose bewerten?

...

...

...

...

...

---
  
## AR(2) Modell: Schätzung

Schätzung eines AR(2) Modells für 1962-Q1 - 2017-Q3


``` r
# Schätzung
ar02.dynlm <- dynlm(GDPGR ~ L(GDPGR,1) + L(GDPGR,2),
                    data = us_macro_ts,
                    start = c(1962, 1), end = c(2017, 3))
summary(ar02.dynlm)
```

```
## 
## Time series regression with "ts" data:
## Start = 1962(1), End = 2017(3)
## 
## Call:
## dynlm(formula = GDPGR ~ L(GDPGR, 1) + L(GDPGR, 2), data = us_macro_ts, 
##     start = c(1962, 1), end = c(2017, 3))
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -10.342  -1.589  -0.057   1.811  13.259 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  1.60275    0.30313   5.287 2.99e-07 ***
## L(GDPGR, 1)  0.27923    0.06611   4.224 3.52e-05 ***
## L(GDPGR, 2)  0.17673    0.06593   2.681  0.00791 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.013 on 220 degrees of freedom
##   (0 observations deleted due to missingness)
## Multiple R-squared:  0.1453,	Adjusted R-squared:  0.1376 
## F-statistic: 18.71 on 2 and 220 DF,  p-value: 3.139e-08
```

``` r
# Tests
ct.ar02.dynlm <- coeftest(ar02.dynlm, vcov=vcovHC(ar02.dynlm, type="HC0"))
ct.ar02.dynlm
```

```
## 
## t test of coefficients:
## 
##             Estimate Std. Error t value  Pr(>|t|)    
## (Intercept) 1.602751   0.369103  4.3423 2.152e-05 ***
## L(GDPGR, 1) 0.279235   0.076380  3.6558 0.0003207 ***
## L(GDPGR, 2) 0.176734   0.076732  2.3033 0.0221980 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

---

### Frage 6

Was können wir von dem Ergebnis der Schätzung und der Tests der Koeffizienten lernen?

...

...

...

...

...

---

## AR(2) Modell: Prognose

Verwendung des geschätzten AR(2)-Modells zur Prognose.


``` r
# Daten für die Prognose 
X1 <- matrix(rev(window(us_macro_ts,start=c(2017,2),end=c(2017,3))[,"GDPGR"]),ncol=1)
XX <- rbind(1, X1)
# AR(2) Koeffizienten
bet <- matrix(ar02.dynlm$coefficients,nrow=1)
# Prgnose für 2017 Q4
prog_erg <- bet %*% XX
prog_erg
```

```
##          [,1]
## [1,] 3.003033
```

``` r
# Tatsächlicher beobachteter Wert
tats_wert <- window(us_macro_ts[, "GDPGR"], start = c(2017, 4), end = c(2017, 4))
tats_wert
```

```
##          Qtr4
## 2017 2.504579
```

``` r
# Prognosefehler
prog_fehler <- tats_wert - prog_erg
prog_fehler
```

```
##            Qtr4
## 2017 -0.4984537
```

---

### Frage 7

Welche Schritte umfasst eine Prognose auf Basis eines AR(2)-Modells? Wie würden Sie die Prognose bewerten?

* Ich wähle die letzten zwei(!) Beobachtungen aus
* Ich sezte die letzten zwei(!) Beobachgungen in das Ar(2)-Regressinsmodell um den Wert für 2017-Q4 zu prognostzizeren

...

...

...

...

...
