---
output:
  html_document
editor_options:
  chunk_output_type: console
---

---

# Praxist-Teil Session 10:<br>Mehrperiodenprognosen

Dieses Dokument enthält den Praxis-Teil von Session 10: Mehrperiodenprognosen.

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
source(here("10-session-03-02-mehrperiodenprognosen", "02-code", "daten_vorbereitung_skript.R"))
```

---

## Iterative Mehrperiodenprognosen

**AR(2)-Modell**

*Hinweis: Die Prognose basierend auf dem AR(2)-Modell im Buch und in den Folien basiert auf den Zeitraum 1962-Q1--2017-Q3.*


``` r
# Schätzung
ar02.dynlm <- dynlm(GDPGR ~ L(GDPGR,1) + L(GDPGR,2),
                    data = us_macro_ts,
                    start = c(1962, 1), end = c(2017, 3))
ar02.dynlm
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
## Coefficients:
## (Intercept)  L(GDPGR, 1)  L(GDPGR, 2)  
##      1.6028       0.2792       0.1767
```

Letzte Beobachtungen:


``` r
tail(ar02.dynlm$model)
```

```
##            GDPGR L(GDPGR, 1) L(GDPGR, 2)
## 2016 Q2 2.213161   0.5786133   0.4845201
## 2016 Q3 2.742267   2.2131605   0.5786133
## 2016 Q4 1.743040   2.7422666   2.2131605
## 2017 Q1 1.228157   1.7430403   2.7422666
## 2017 Q2 3.013954   1.2281573   1.7430403
## 2017 Q3 3.107115   3.0139537   1.2281573
```

### Frage 1

Berechnen Sie die $h=1$ Prognose und die $h=2$ Prognose. Um welche "Art" von Prognose handelt es sich bei dieser Prognose? Welche Methoden zur Erstellung derartiger Prognosen haben wir besprochen?

...

...

...

...

...
    
---

**VAR(2)-Modell**

*Hinweis: Die Prognose basierend auf dem AR(2)-Modell im Buch und in den Folien basiert auf den Zeitraum 1981-Q1--2017-Q3.*


``` r
# Bereite Variablen vor (wähle 1980-Q3 als Start da 1980-Q3 & 1980-Q4 durch zwei Lags "verloren" gehen)
us.macro.ts.win <- window(us_macro_ts, start = c(1980, 3), end = c(2017, 3))
y <- us.macro.ts.win[,c("GDPGR", "TSpread")]

# Schätze VAR(2)-Modell via vars-Paket
var.res <- VAR(y, p = 2, type = "const")

# Ergebnis
var.res
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

**Prognose: VAR(2)-Modell**


``` r
# Verwendung der predict Methode
predict(var.res, n.ahead = 10)
```

```
## $GDPGR
##           fcst     lower    upper       CI
##  [1,] 2.762091 -1.813198 7.337380 4.575289
##  [2,] 2.403748 -2.411989 7.219484 4.815737
##  [3,] 2.233766 -2.750723 7.218255 4.984489
##  [4,] 2.153373 -2.921140 7.227887 5.074514
##  [5,] 2.143279 -2.991098 7.277656 5.134377
##  [6,] 2.180432 -3.003449 7.364312 5.183880
##  [7,] 2.245311 -2.981165 7.471787 5.226476
##  [8,] 2.323243 -2.938799 7.585286 5.262043
##  [9,] 2.403529 -2.886399 7.693457 5.289928
## [10,] 2.478973 -2.831313 7.789258 5.310286
## 
## $TSpread
##           fcst         lower    upper        CI
##  [1,] 1.283297  0.3568359056 2.209758 0.9264609
##  [2,] 1.393379  0.0423343159 2.744423 1.3510445
##  [3,] 1.508304 -0.0838243767 3.100431 1.5921279
##  [4,] 1.623246 -0.1338137594 3.380306 1.7570599
##  [5,] 1.727962 -0.1373866313 3.593311 1.8653489
##  [6,] 1.817707 -0.1158354138 3.751249 1.9335423
##  [7,] 1.890536 -0.0836693848 3.864740 1.9742049
##  [8,] 1.946486 -0.0505121948 3.943485 1.9969984
##  [9,] 1.986934 -0.0220524083 3.995921 2.0089865
## [10,] 2.014012 -0.0009562469 4.028979 2.0149679
```

Letzte Beobachtungen:


``` r
tail(var.res$datamat)
```

```
##        GDPGR  TSpread  GDPGR.l1 TSpread.l1  GDPGR.l2 TSpread.l2 const
## 142 2.213161 1.496667 0.5786133   1.633333 0.4845201   2.066667     1
## 143 2.742267 1.266667 2.2131605   1.496667 0.5786133   1.633333     1
## 144 1.743040 1.700000 2.7422666   1.266667 2.2131605   1.496667     1
## 145 1.228157 1.853333 1.7430403   1.700000 2.7422666   1.266667     1
## 146 3.013954 1.373333 1.2281573   1.853333 1.7430403   1.700000     1
## 147 3.107115 1.206667 3.0139537   1.373333 1.2281573   1.853333     1
```

### Frage 2

Versuchen Sie die Ergebnisee auch "per Hand" basierend auf den letzten Beobachtungen zu berechnen.

...

...

...

...

...
    
---

## Direkte Mehrperiodenprognosen

**Schätzung: AR(2)-Modell**

*Hinweis: Die Prognose basierend auf dem AR(2)-Modell im Buch und in den Folien basiert auf den Zeitraum 1962-Q1--2017-Q3.*


``` r
# Hilfsregresion über den gesamten Zeithorizont um die komplette "Modelldaten" zu bekommen
tmp <-  dynlm(GDPGR ~ L(GDPGR,1) + L(GDPGR,2),
              data = us_macro_ts,
              start = c(1959, 3), end = c(2017, 4))

data.mod.01 <- ts(cbind(tmp$model[,1], 1, tmp$model[,-1]),
                  frequency = 4, start = c(1960, 1), end = c(2017, 4))

# Regression für die direkte Prognose
ar.dynlm.tmp <- dynlm(GDPGR ~ L(GDPGR,2) + L(GDPGR,3),
                      data = us_macro_ts,
                      start = c(1962,1), end = c(2017, 3))
```

**Prognose: AR(2)-Modell**


``` r
# Koeffizienten Prognoseregression
bet.tmp <- matrix(coefficients(ar.dynlm.tmp), nrow = 1)
# Werte unabhängige Variablen
dat.tmp <- matrix(window(data.mod.01[,-1], start = c(2017, 4), end = c(2017, 4)), ncol = 1)
# Prognose
y.hat <- bet.tmp %*% dat.tmp
y.hat
```

```
##          [,1]
## [1,] 2.448252
```

**Evaluierung: AR(2)-Modell**


``` r
# Tatsächlicher Wert
y.act <- matrix(window(data.mod.01[,1], start = c(2017, 4), end = c(2017, 4)), ncol = 1)
# Prognosefehler
u.tmp <- y.act -  y.hat
u.tmp
```

```
##           [,1]
## [1,] 0.5657016
```

### Frage 3

Um welche "Art" von Prognose handelt es sich bei der Prognose oben?

...

...

...

...

...
    
---

**Schätzung: VAR(2)-Modell**

*Hinweis: Die Prognose basierend auf dem AR(2)-Modell im Buch und in den Folien basiert auf den Zeitraum 1962-Q1--2017-Q3.*


``` r
# Hilfsregresion über den gesamten Zeithorizont um die komplette "Modelldaten" zu bekommen
tmp <-  dynlm(GDPGR ~ L(GDPGR,1) + L(GDPGR,2) + L(TSpread,1) + L(TSpread,2),
              data = us_macro_ts,
              start = c(1959, 3), end = c(2017, 4))

data.mod.01 <- ts(cbind(tmp$model[,1], 1 , tmp$model[,-1]),
                  frequency = 4, start = c(1960, 1), end = c(2017, 4))

# Regression für die direkte Prognose
adl.dynlm.tmp <- dynlm(GDPGR ~ L(GDPGR,2) + L(GDPGR,3) + L(TSpread,2) + L(TSpread,3),
                       data = us_macro_ts,
                       start = c(1981,1), end = c(2017, 3))
```

**Prognose: VAR(2)-Modell**


``` r
# Koeffizienten Prognoseregression
bet.tmp <- matrix(coefficients(adl.dynlm.tmp), nrow = 1)
# Werte unabhängige Variablen
dat.tmp <- matrix(window(data.mod.01[,-1], start = c(2017, 4), end = c(2017, 4)), ncol = 1)
# Prognose
y.hat <- bet.tmp %*% dat.tmp
y.hat
```

```
##          [,1]
## [1,] 2.127999
```

**Evaluierung: VAR(2)-Modell**


``` r
# Tatsächlicher Wert
y.act <- matrix(window(data.mod.01[,1], start = c(2017, 4), end = c(2017, 4)), ncol = 1)
# Prognosefehler
u.tmp <- y.act -  y.hat
u.tmp
```

```
##           [,1]
## [1,] 0.8859542
```

### Frage 4

Um welche "Art" von Prognose handelt es sich bei der Prognose oben?

...

...

...

...

...
    
---
