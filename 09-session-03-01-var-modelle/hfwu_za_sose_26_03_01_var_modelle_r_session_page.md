---
output:
  html_document
editor_options:
  chunk_output_type: console
---

---

# Praxist-Teil Session 9:<br>VAR-Modelle

Dieses Dokument enthält den Praxis-Teil von Session 9: VAR-Modelle.

---

# Setup


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

# Lade Pakete
library(zoo)
library(dynlm)
library(sandwich)
library(lmtest)
library(AER)
library(vars)
```

---

## Vorbereitung der Daten


``` r
#  "01-daten/us_macro_quarterly_merged.csv" -> "us_macro_ts" (data.frame)
source(here("09-session-03-01-var-modelle", "02-code", "daten_vorbereitung_skript.R"))
```

---

## Schätzung der Parameter von VAR-Modellen

Hinweis: Die Analse im Buch und in den Folien basiert auf den Zeitraum 1981-Q1--2017-Q3


``` r
# Bereite Variablen vor (wähle 1980-Q3 als Start da 1980-Q3 & 1980-Q4 durch zwei Lags "verloren" gehen)
us_macro_ts_win <- window(us_macro_ts, start = c(1980, 3), end = c(2017, 3))
y <- us_macro_ts_win[,c("GDPGR", "TSpread")]

# Schätze VAR(2)-Modell via vars-Paket
var_res <- VAR(y, p = 2, type = "const")

# Ergebnis
var_res
```

```
## 
## VAR Estimation Results:
## ======================= 
## 
## Estimated coefficients for equation GDPGR: 
## ========================================== 
## Call:
## GDPGR = GDPGR.l1 + TSpread.l1 + GDPGR.l2 + TSpread.l2 + const 
## 
##   GDPGR.l1 TSpread.l1   GDPGR.l2 TSpread.l2      const 
##  0.2854767 -0.8622934  0.2046793  1.2770276  0.5449047 
## 
## 
## Estimated coefficients for equation TSpread: 
## ============================================ 
## Call:
## TSpread = GDPGR.l1 + TSpread.l1 + GDPGR.l2 + TSpread.l2 + const 
## 
##     GDPGR.l1   TSpread.l1     GDPGR.l2   TSpread.l2        const 
##  0.008228097  1.058958128 -0.052647472 -0.220064710  0.440820948
```

### Frage 1

Interpretieren Sie das Ergebnis der Schätzung der Parameter.

...

...

...

...

...
    
---

**Vergleich Ergebnis Schätzung der ADL-Modelle**


``` r
# Schätzung ADL(2,2) Model für GDPGR: 1981-Q1 - 2017-Q3
gdpgr_adl_0202_dynlm <- dynlm(GDPGR ~ L(GDPGR,1) + L(GDPGR,2) + L(TSpread,1) + L(TSpread,2),
                        data = us_macro_ts,
                        start = c(1981, 1), end = c(2017, 3))
                   
coeftest(gdpgr_adl_0202_dynlm, vcov=vcovHC(gdpgr_adl_0202_dynlm, type="HC0"))
```

```
## 
## t test of coefficients:
## 
##                Estimate Std. Error t value Pr(>|t|)   
## (Intercept)    0.544905   0.496627  1.0972 0.274406   
## L(GDPGR, 1)    0.285477   0.105200  2.7137 0.007479 **
## L(GDPGR, 2)    0.204679   0.082455  2.4823 0.014218 * 
## L(TSpread, 1) -0.862293   0.354301 -2.4338 0.016184 * 
## L(TSpread, 2)  1.277028   0.387919  3.2920 0.001255 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
# Schätzung ADL(2,2) Model für GDPGR: 1981-Q1 - 2017-Q3
tspread_adl_0202_dynlm <- dynlm(TSpread ~ L(GDPGR,1) + L(GDPGR,2) + L(TSpread,1) + L(TSpread,2),
                        data = us_macro_ts,
                        start = c(1981, 1), end = c(2017, 3))

coeftest(tspread_adl_0202_dynlm, vcov=vcovHC(tspread_adl_0202_dynlm, type="HC0"))
```

```
## 
## t test of coefficients:
## 
##                 Estimate Std. Error t value  Pr(>|t|)    
## (Intercept)    0.4408209  0.1161888  3.7940 0.0002187 ***
## L(GDPGR, 1)    0.0082281  0.0205322  0.4007 0.6892138    
## L(GDPGR, 2)   -0.0526475  0.0251565 -2.0928 0.0381459 *  
## L(TSpread, 1)  1.0589581  0.0954038 11.0998 < 2.2e-16 ***
## L(TSpread, 2) -0.2200647  0.1061266 -2.0736 0.0399226 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

### Frage 2

Vergleichen Sie das Ergebnis der Schätzung des VAR-Modells und des ADL-Modells.

...

...

...

...

...

---



## Bestimmung der Lag-Länge von VAR-Modellen


``` r
var_sel_res <- VARselect(y, lag.max = 10, type = "const")

var_sel_res
```

```
## $selection
## AIC(n)  HQ(n)  SC(n) FPE(n) 
##      2      2      2      2 
## 
## $criteria
##                  1           2           3           4           5           6          7          8
## AIC(n) -0.15057395 -0.30037868 -0.27421858 -0.22556336 -0.20304640 -0.14992217 -0.1171140 -0.1337040
## HQ(n)  -0.09909947 -0.21458788 -0.15411146 -0.07113991 -0.01430664  0.07313391  0.1402584  0.1579847
## SC(n)  -0.02390601 -0.08926544  0.02133994  0.15444046  0.26140271  0.39897223  0.5162257  0.5840810
## FPE(n)  0.86022565  0.74058374  0.76029555  0.79835612  0.81678195  0.86171933  0.8909860  0.8770049
##                 9         10
## AIC(n) -0.1594040 -0.1386613
## HQ(n)   0.1666011  0.2216601
## SC(n)   0.6428263  0.7480143
## FPE(n)  0.8555934  0.8745906
```

### Frage 3

Interpretieren Sie das Ergebnis der Bestimmung der Lag-Länge.

...

...

...

...

...

---

## Test der Vorhersagbarkeit


``` r
# GDPGR -> TSpread
gdpgr_pred <- causality(var_res, cause = "GDPGR", vcov.=vcovHC(var_res, type = "HC0"))
gdpgr_pred$Granger
```

```
## 
## 	Granger causality H0: GDPGR do not Granger-cause TSpread
## 
## data:  VAR object var_res
## F-Test = 3.2193, df1 = 2, df2 = 284, p-value = 0.04144
```

``` r
# TSpread -> GDPGR
tspread_pred <- causality(var_res, cause = "TSpread", vcov.=vcovHC(var_res, type = "HC0"))
tspread_pred$Granger
```

```
## 
## 	Granger causality H0: TSpread do not Granger-cause GDPGR
## 
## data:  VAR object var_res
## F-Test = 5.5956, df1 = 2, df2 = 284, p-value = 0.004136
```

### Frage 4

Interpretieren Sie das Ergebnis der Bestimmung der Tests.

...

...

...

...

...

---

**Vergleich Ergebnis via `linearHypothesis` Funktion**


``` r
# GDPGR -> TSpread
linearHypothesis(tspread_adl_0202_dynlm, c("L(GDPGR, 1)=0",
                                           "L(GDPGR, 2)=0"), white.adjust = "hc0")
```

```
## 
## Linear hypothesis test:
## L(GDPGR,0
## L(GDPGR, 2) = 0
## 
## Model 1: restricted model
## Model 2: TSpread ~ L(GDPGR, 1) + L(GDPGR, 2) + L(TSpread, 1) + L(TSpread, 
##     2)
## 
## Note: Coefficient covariance matrix supplied.
## 
##   Res.Df Df      F  Pr(>F)  
## 1    144                    
## 2    142  2 3.2193 0.04292 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
# TSpread -> GDPGR
linearHypothesis(gdpgr_adl_0202_dynlm, c("L(TSpread, 1)=0",
                                         "L(TSpread, 2)=0"), white.adjust = "hc0")
```

```
## 
## Linear hypothesis test:
## L(TSpread,0
## L(TSpread, 2) = 0
## 
## Model 1: restricted model
## Model 2: GDPGR ~ L(GDPGR, 1) + L(GDPGR, 2) + L(TSpread, 1) + L(TSpread, 
##     2)
## 
## Note: Coefficient covariance matrix supplied.
## 
##   Res.Df Df      F  Pr(>F)   
## 1    144                     
## 2    142  2 5.5956 0.00458 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```
