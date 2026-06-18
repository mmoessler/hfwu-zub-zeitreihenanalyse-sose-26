---
output:
  html_document
editor_options:
  chunk_output_type: console
---

---

# Praxist-Teil Session 11:<br>Dynamische Faktormodelle und Hauptkomponenten

Dieses Dokument enthält den Praxis-Teil von Session 11: Dynamische Faktormodelle und Hauptkomponenten.

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
```

```
## 
## Attaching package: 'zoo'
```

```
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

``` r
library(dynlm)
library(sandwich)
library(lmtest)
library(AER)
```

```
## Loading required package: car
```

```
## Loading required package: carData
```

```
## Loading required package: survival
```

``` r
library(vars)
```

```
## Loading required package: MASS
```

```
## Loading required package: strucchange
```

```
## Loading required package: urca
```

``` r
library(scales)
library(R.matlab)
```

```
## R.matlab v3.7.0 (2022-08-25 21:52:34 UTC) successfully loaded. See ?R.matlab for help.
```

```
## 
## Attaching package: 'R.matlab'
```

```
## The following objects are masked from 'package:base':
## 
##     getOption, isOpen
```

``` r
library(xtable)
library(kableExtra)

source(here("11-session-03-03-df-modelle", "02-code", "dfm_functions.r"))
```

## Vorbereitung der Daten


``` r
# Load observable macro variables
macro.01.dat <- read.table(here("11-session-03-03-df-modelle", "01-daten", "us_macro_data.txt"),
                           header = TRUE,
                           sep = ",",
                           colClasses = c("character", "numeric", "numeric", "numeric"))

macro.01.ts <- ts(macro.01.dat[,-c(1)], frequency = 4, start = c(1955, 2), end = c(2017, 4))
```

Einlesen der "großen" Daten


``` r
#..................................................
# Load all observable macro variables: The data are provided by S&W in a matlab matrix, datain.mat

tmp <- readMat(con = here("11-session-03-03-df-modelle", "01-daten", "datain.mat"))

bpdata <- tmp$datain[2,1,1]$bpdata # use this for estimation
bplabvec.short <- tmp$datain[6,1,1]$bplabvec.short
bplabvec.long <- tmp$datain[5,1,1]$bplabvec.long
```

**Schritte zur Datenumwandlung**

Der Datensatz `datain.mat` basiert auf den FRED-Daten von Stock und Watson und wurde für die Faktorenanalyse in mehreren Schritten vorverarbeitet:

1. Monatliche und vierteljährliche makroökonomische sowie finanzwirtschaftliche Zeitreihen wurden aus dem FRED-basierten Datensatz eingelesen.
2. Zusätzliche ökonomisch relevante Variablen wurden konstruiert, darunter Zinsstrukturspreads und ausgewählte Kennzahlen.
3. Nominale Variablen wurden mithilfe geeigneter Preisdeflatoren in reale Größen umgerechnet.
4. Monatliche Zeitreihen wurden auf Quartalsfrequenz aggregiert, um für alle Variablen eine einheitliche Beobachtungsfrequenz zu gewährleisten.
5. Variablenspezifische Transformationen (Niveaus, Differenzen, Logarithmen und Log-Differenzen) wurden angewendet, um stationäre Zeitreihen für die Faktorenanalyse zu erhalten.
6. Ausreißer wurden anhand eines Interquartilsabstands-Kriteriums identifiziert und mithilfe eines gleitenden Medianverfahrens ersetzt.
7. Die transformierten monatlichen und vierteljährlichen Variablen wurden zu einem gemeinsamen vierteljährlichen Paneldatensatz zusammengeführt.
8. Die für die Faktorenanalyse vorgesehenen Variablen wurden ausgewählt und nach ökonomischen Kategorien geordnet.
9. Der resultierende transformierte und ausreißerbereinigte Paneldatensatz wurde als Grundlage für die Faktorextraktion und die anschließende Analyse verwendet.

**Verwendete Variablen (Codes)**


``` r
# Extract names
short_names <- sapply(bplabvec.short, function(x) x[[1]][1, 1])
long_names  <- sapply(bplabvec.long,  function(x) x[[1]][1, 1])

# Create table
var_dict <- data.frame(
  short_name = short_names,
  long_name  = long_names,
  stringsAsFactors = FALSE
)

kable_styling(kable(var_dict), full_width = FALSE)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> short_name </th>
   <th style="text-align:left;"> long_name </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Cons:Dur </td>
   <td style="text-align:left;"> Real personal consumption expenditures: Durable goods (chain-type </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cons:Svc </td>
   <td style="text-align:left;"> Real personal consumption expenditures: Services (chain-type quantity </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cons:NonDur </td>
   <td style="text-align:left;"> Real personal consumption expenditures: Nondurable goods (chain-type </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Inv:NonResStruct </td>
   <td style="text-align:left;"> Real private fixed investment: Nonresidential: Structures (chain-type quantity index), Quarterly, Seasonally Adjusted </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Inv:IP </td>
   <td style="text-align:left;"> Real Gross Private Domestic Investment: Fixed Investment: Nonresidential: Intellectual Property Products (chain-type quantity index), Quarterly, Seasonally Adjusted </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Inv:ResStruct </td>
   <td style="text-align:left;"> Real private fixed investment: Residential: Structures (chain-type quantity index), Quarterly, Seasonally Adjusted </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Inv:Equip </td>
   <td style="text-align:left;"> Real Gross Private Domestic Investment: Fixed Investment: Nonresidential: Equipment (chain-type quantity index </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Gov:Fed </td>
   <td style="text-align:left;"> Real government consumption expenditures and gross investment: Federal </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Real_Gov Receipts </td>
   <td style="text-align:left;"> Government Current Receipts (Nominal) Defl by GDP Def </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Gov:State&amp;Local </td>
   <td style="text-align:left;"> Real government consumption expenditures and gross investment: State and local </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Exports </td>
   <td style="text-align:left;"> Real exports of goods and services (chain-type quantity index) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ch. Inv/GDP </td>
   <td style="text-align:left;"> Ch. Inv/GDP </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Imports </td>
   <td style="text-align:left;"> Real imports of goods and services (chain-type quantity index) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IP: Energy Prds </td>
   <td style="text-align:left;"> IP: Consumer Energy Products </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IP: Dur gds materials </td>
   <td style="text-align:left;"> Industrial Production: Durable Materials </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IP: Nondur gds materials </td>
   <td style="text-align:left;"> Industrial Production: nondurable Materials </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IP: Dur Cons. Goods </td>
   <td style="text-align:left;"> Industrial Production: Durable Consumer Goods </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IP: Auto </td>
   <td style="text-align:left;"> IP: Automotive products </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IP:NonDur Cons God </td>
   <td style="text-align:left;"> Industrial Production: Nondurable Consumer Goods </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IP: Equip </td>
   <td style="text-align:left;"> Industrial Production: Business Equipment </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Capu Tot </td>
   <td style="text-align:left;"> Capacity Utilization: Total Industry </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Emp: DurGoods </td>
   <td style="text-align:left;"> All Employees: Durable Goods Manufacturing </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Emp: Const </td>
   <td style="text-align:left;"> All Employees: Construction </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Emp: Edu&amp;Health </td>
   <td style="text-align:left;"> All Employees: Education &amp; Health Services </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Emp: Finance </td>
   <td style="text-align:left;"> All Employees: Financial Activities </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Emp: Infor </td>
   <td style="text-align:left;"> All Employees: Information Services </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Emp: Bus Serv </td>
   <td style="text-align:left;"> All Employees: Professional &amp; Business Services </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Emp:Leisure </td>
   <td style="text-align:left;"> All Employees: Leisure &amp; Hospitality </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Emp:OtherSvcs </td>
   <td style="text-align:left;"> All Employees: Other Services </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Emp: Mining/NatRes </td>
   <td style="text-align:left;"> All Employees: Natural Resources &amp; Mining </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Emp:Trade&amp;Trans </td>
   <td style="text-align:left;"> All Employees: Trade  Transportation &amp; Utilities </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Emp:Retail </td>
   <td style="text-align:left;"> All Employees: Retail Trade </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Emp:Wholesal </td>
   <td style="text-align:left;"> All Employees: Wholesale Trade </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Emp: Gov(Fed) </td>
   <td style="text-align:left;"> Employment Federal Government </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Emp: Gov (State) </td>
   <td style="text-align:left;"> Employment State government </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Emp: Gov (Local) </td>
   <td style="text-align:left;"> Employment Local government </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Urate: Age16-19 </td>
   <td style="text-align:left;"> Unemployment Rate - 16-19 yrs </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Urate:Age&gt;20 Men </td>
   <td style="text-align:left;"> Unemployment Rate - 20 yrs. &amp; over  Men </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Urate: Age&gt;20 Women </td>
   <td style="text-align:left;"> Unemployment Rate - 20 yrs. &amp; over  Women </td>
  </tr>
  <tr>
   <td style="text-align:left;"> U:dur&gt;15-26wks </td>
   <td style="text-align:left;"> Civilians Unemployed for 15-26 Weeks </td>
  </tr>
  <tr>
   <td style="text-align:left;"> U: Dur&gt;27wks </td>
   <td style="text-align:left;"> Number Unemployed for 27 Weeks &amp; over </td>
  </tr>
  <tr>
   <td style="text-align:left;"> U:Dur5-14wks </td>
   <td style="text-align:left;"> Number Unemployed for 5-14 Weeks </td>
  </tr>
  <tr>
   <td style="text-align:left;"> U: Dur&lt;5wks </td>
   <td style="text-align:left;"> Number Unemployed for Less than 5 Weeks </td>
  </tr>
  <tr>
   <td style="text-align:left;"> U: LF Reenty </td>
   <td style="text-align:left;"> Unemployment Level - Reentrants to Labor Force </td>
  </tr>
  <tr>
   <td style="text-align:left;"> U: Job losers </td>
   <td style="text-align:left;"> Unemployment Level - Job Losers </td>
  </tr>
  <tr>
   <td style="text-align:left;"> U: New Entrants </td>
   <td style="text-align:left;"> Unemployment Level - New Entrants </td>
  </tr>
  <tr>
   <td style="text-align:left;"> U: Job Leavers </td>
   <td style="text-align:left;"> Unemployment Level - Job Leavers </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Emp:SlackWk </td>
   <td style="text-align:left;"> Employment Level - Part-Time for Economic Reasons  All Industries </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AWH Man </td>
   <td style="text-align:left;"> Average Weekly Hours: Manufacturing </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AWH Privat </td>
   <td style="text-align:left;"> Average Weekly Hours: Total Private Industrie </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AWH Overtime </td>
   <td style="text-align:left;"> Average Weekly Hours: Overtime: Manufacturing </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hpermits </td>
   <td style="text-align:left;"> New Private Housing Units Authorized by Building Permit </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hstarts:MW </td>
   <td style="text-align:left;"> Housing Starts in Midwest Census Region </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hstarts:NE </td>
   <td style="text-align:left;"> Housing Starts in Northeast Census Region </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Real_Const Spending </td>
   <td style="text-align:left;"> Total Construction Spending, Monthly, Seasonally Adjusted Annual Rate Defl by PCE(LFE) Def </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hstarts:S </td>
   <td style="text-align:left;"> Housing Starts in South Census Region </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hstarts:W </td>
   <td style="text-align:left;"> Housing Starts in West Census Region </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Real_Unfilled Oders MFG </td>
   <td style="text-align:left;"> Value of Manufacturers' Unfilled Orders for Durable Goods Industries, Monthly, Seasonally Adjusted Defl by PCE(LFE) Def </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Real_Orders: Dur Cons Goods </td>
   <td style="text-align:left;"> Value of Manufacturers' New Orders for Consumer Goods: Consumer Durable Goods Industries, Monthly, Seasonally Adjusted Defl by PCE(LFE) Def </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Real_Shipments MFG </td>
   <td style="text-align:left;"> Value of Manufacturers' Shipments for All Manufacturing Industries, Monthly, Seasonally Adjusted Defl by PCE(LFE) Def </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Inv2Shif MFG </td>
   <td style="text-align:left;"> Ratio of Manufacturers' Total Inventories to Shipments for All Manufacturing Industries, Monthly, Seasonally Adjusted </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Real_NewOrders CG MFG </td>
   <td style="text-align:left;"> Manufacturers' New Orders: Nondefense Capital Goods Excluding Aircraft, Monthly, Seasonally Adjusted Defl by PCE(LFE) Def </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Inv2Sales Busines </td>
   <td style="text-align:left;"> Total Business: Inventories to Sales Ratio, Monthly, Seasonally Adjusted </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GPDI Defl </td>
   <td style="text-align:left;"> Gross Private Domestic Investment: Chain-type Price Index </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BusSec Defl </td>
   <td style="text-align:left;"> Business Sector: Implicit Price Deflator </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PCED_MotorVec </td>
   <td style="text-align:left;"> Motor vehicles and parts </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PCED_DurHousehold </td>
   <td style="text-align:left;"> Furnishings and durable household equipment </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PCED_Recreation </td>
   <td style="text-align:left;"> Recreational goods and vehicles </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PCED_OthDurGds </td>
   <td style="text-align:left;"> Other durable goods </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PCED_Food_Bev </td>
   <td style="text-align:left;"> Food and beverages purchased for off-premises consumption </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PCED_Clothing </td>
   <td style="text-align:left;"> Clothing and footwear </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PCED_Gas_Enrgy </td>
   <td style="text-align:left;"> Gasoline and other energy goods </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PCED_OthNDurGds </td>
   <td style="text-align:left;"> Other nondurable goods </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PCED_Housing-Utilities </td>
   <td style="text-align:left;"> Housing and utilities </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PCED_HealthCare </td>
   <td style="text-align:left;"> Health care </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PCED_TransSvg </td>
   <td style="text-align:left;"> Transportation services </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PCED_RecServices </td>
   <td style="text-align:left;"> Recreation services </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PCED_FoodServ_Acc. </td>
   <td style="text-align:left;"> Food services and accommodations </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PCED_FIRE </td>
   <td style="text-align:left;"> Financial services and insurance </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PCED_OtherServices </td>
   <td style="text-align:left;"> Other services </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PPI:FinConsGds </td>
   <td style="text-align:left;"> Producer Price Index by Commodity for Final Demand: Personal Consumption Goods (Finished Consumer Goods) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PPI:FinConsGds(Food) </td>
   <td style="text-align:left;"> Producer Price Index by Commodity for Final Demand: Finished Consumer Foods </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PPI:IndCom </td>
   <td style="text-align:left;"> Producer Price Index: Industrial Commodities </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PPI:IntMat </td>
   <td style="text-align:left;"> Producer Price Index by Commodity for Intermediate Demand by Commodity Type: Materials and Components for Manufacturing </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Real_Price:NatGas </td>
   <td style="text-align:left;"> PPI: Natural Gas Defl by PCE(LFE) Def </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CPH:NFB </td>
   <td style="text-align:left;"> Nonfarm Business Sector: Real Compensation Per Hour </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CPH:Bus </td>
   <td style="text-align:left;"> Business Sector: Real Compensation Per Hour </td>
  </tr>
  <tr>
   <td style="text-align:left;"> OPH:nfb </td>
   <td style="text-align:left;"> Nonfarm Business Sector: Output Per Hour of All Persons </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ULC:NFB </td>
   <td style="text-align:left;"> Nonfarm Business Sector: Unit Labor Cost </td>
  </tr>
  <tr>
   <td style="text-align:left;"> UNLPay:nfb </td>
   <td style="text-align:left;"> Nonfarm Business Sector: Unit Nonlabor Payments </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FEDFUNDS_TB3MS </td>
   <td style="text-align:left;"> Spread: FEDFUNDS_TB3MS </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TB-3Mth </td>
   <td style="text-align:left;"> 3-Month Treasury Bill: Secondary Market Rate </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TB6MS_TB3MS </td>
   <td style="text-align:left;"> Spread: TB6MS_TB3MS </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GS1_TB3MS </td>
   <td style="text-align:left;"> Spread: GS1_TB3MS </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GS10_TB3MS </td>
   <td style="text-align:left;"> Spread: GS10_TB3MS </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DCPF3M_TB3MS </td>
   <td style="text-align:left;"> Spread: DCPF3M_TB3MS </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MORTGAGE30US_TB3MS </td>
   <td style="text-align:left;"> Spread: MORTGAGE30US_TB3MS </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AAA_TB3MS </td>
   <td style="text-align:left;"> Spread: AAA_TB3MS </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BAA_TB3MS </td>
   <td style="text-align:left;"> Spread: BAA_TB3MS </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ted_spr </td>
   <td style="text-align:left;"> TED Spread FRED </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Real_C&amp;Lloand </td>
   <td style="text-align:left;"> Commercial and Industrial Loans at All Commercial Banks Defl by PCE(LFE) Def </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Real_ConsLoans </td>
   <td style="text-align:left;"> Consumer (Individual) Loans at All Commercial Banks  Outlier Code because of change in data in April 2010  see FRB H8 Release Defl by PCE(LFE) Def </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Real_NonRevCredit </td>
   <td style="text-align:left;"> Total Nonrevolving Credit Outstanding Defl by PCE(LFE) Def </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Real_LoansRealEst </td>
   <td style="text-align:left;"> Real Estate Loans at All Commercial Banks Defl by PCE(LFE) Def </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Real_RevolvCredit </td>
   <td style="text-align:left;"> Total Revolving Credit Outstanding Defl by PCE(LFE) Def </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FRBSLO_Consumers </td>
   <td style="text-align:left;"> FRB Senior Loans Officer Opions. Net Percentage of Domestic Respondents Reporting Increased Willingness to Make Consumer Installment Loans </td>
  </tr>
  <tr>
   <td style="text-align:left;"> S&amp;P 500 </td>
   <td style="text-align:left;"> S&amp;P'S COMMON STOCK PRICE INDEX: COMPOSITE (1941-43=10)  daily .. Use FRED to Aggregate to montly </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Real_HHW:TL </td>
   <td style="text-align:left;"> Real Total Liabilities of Households and Non Profits, $Billions (Seas Adj by RATS-X11) Defl by PCE(LFE) Def </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Real_HHW:TA_Fin </td>
   <td style="text-align:left;"> Total Financial Assets of households and and Nonprofits, $Billions  (Seas Adj by RATS-X11) Defl by PCE(LFE) Def </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Real_HHW:TA_RE </td>
   <td style="text-align:left;"> Households and nonprofit organizations; real estate at market value, Level, Quarterly, Not Seasonally Adjusted Millions of $  (Seas Adj by RATS-X11) Defl by PCE(LFE) Def </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Real_HHW:TA_NonFin_XRE </td>
   <td style="text-align:left;"> HHW:Total NonFinanicial Assets less Real Estate Defl by PCE(LFE) Def </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DJIA </td>
   <td style="text-align:left;"> COMMON STOCK PRICES: DOW JONES INDUSTRIAL AVERAGE  daily .. Use FRED to Aggregate to montly </td>
  </tr>
  <tr>
   <td style="text-align:left;"> VXO </td>
   <td style="text-align:left;"> VXO  daily .. Use FRED to Aggregate to montly </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Real_Hprice:OFHEO </td>
   <td style="text-align:left;"> House Price Index for the United States Defl by PCE(LFE) Def </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Real_CS_10 </td>
   <td style="text-align:left;"> Case-Shiller 10 City Average Defl by PCE(LFE) Def </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Real_CS_20 </td>
   <td style="text-align:left;"> Case-Shiller 20 City Average Defl by PCE(LFE) Def </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ex rate: major </td>
   <td style="text-align:left;"> FRB Nominal Major Currencies Dollar Index (Linked to EXRUS in 1973:1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ex rate: Euro </td>
   <td style="text-align:left;"> U.S. / Euro Foreign Exchange Rate </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ex rate: Switz </td>
   <td style="text-align:left;"> Switzerland / U.S. Foreign Exchange Rate </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ex rate: Japan </td>
   <td style="text-align:left;"> Japan / U.S. Foreign Exchange Rate </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ex rate: UK </td>
   <td style="text-align:left;"> U.S. / U.K. Foreign Exchange Rate </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EX rate: Canada </td>
   <td style="text-align:left;"> Canada / U.S. Foreign Exchange Rate </td>
  </tr>
  <tr>
   <td style="text-align:left;"> OECD GDP </td>
   <td style="text-align:left;"> OECD: Gross Domestic Product by Expenditure in Constant Prices: Total Gross; Growth Rate (Quartely); Fred Series NAEXKP01O1Q657S </td>
  </tr>
  <tr>
   <td style="text-align:left;"> IP Europe </td>
   <td style="text-align:left;"> OECD: Total Ind. Prod (excl Consturction) Europe Growth Rate (Quarterly); Fred Series PRINTO01OEQ657S </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Cons.Sent </td>
   <td style="text-align:left;"> University of Michigan  Consumer Sentiment </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PoilcyUncertainty </td>
   <td style="text-align:left;"> Economic Policy Uncertainty Index for United States, Daily, Not Seasonally Adjusted Daily 7 day .. Have FRED aggegate to quarterly (note  quarterly series back to 1985 available on BBD webapge) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Real_Price:Oil </td>
   <td style="text-align:left;"> PPI: Crude Petroleum Defl by PCE(LFE) Def </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Real_Crudeoil Price </td>
   <td style="text-align:left;"> Crude Oil: West Texas Intermediate (WTI) - Cushing Oklahoma Defl by PCE(LFE) Def </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Real_CrudeOil </td>
   <td style="text-align:left;"> Crude Oil Prices: Brent - Europe Defl by PCE(LFE) Def </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Real_Price Gasoline </td>
   <td style="text-align:left;"> Conventional Gasoline Prices: New York Harbor  Regular Defl by PCE(LFE) Def </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Real_CPI Gasoline </td>
   <td style="text-align:left;"> CPI Gasoline (NSA) BLS: CUUR0000SETB01 Defl by PCE(LFE) Def </td>
  </tr>
</tbody>
</table>

---

Umwandlung der Daten


``` r
# Transform to time series
macro.02.dat <- bpdata

macro.02.ts <- ts(macro.02.dat, frequency = 4, start = c(1959, 1), end = c(2017, 4))
colnames(macro.02.ts) <- paste0("V", seq(1,ncol(macro.02.ts)))
```

## Schätzung der Unbeobachtbaren Faktoren

Basierend auf allen Makro Variablen.


``` r
#..................................................
# Estimate unobserved factors based on all macro variables ----

# determine the start and end of period for factor estimation
# dat.seq <- seq.Date(from = as.Date("1955-04-01"), to = as.Date("2017-10-01"), by = "quarter")
dat.seq <- seq.Date(from = as.Date("1959-01-01"), to = as.Date("2017-10-01"), by = "quarter")

# dat.tmp <- window(macro.02.ts, start = c(1955, 4), end = c(2014, 1))
dat.tmp <- window(macro.02.ts, start = c(1959, 1), end = c(2017, 4))
xdata <- matrix(as.vector(dat.tmp), nrow = nrow(dat.tmp), ncol = ncol(dat.tmp)) # transform to "matrix"/"array"

# factor estimation using variables without missing values based on PCA

# fa based on ordinary pca
# X <- as.matrix(scale(packr(xdata), center = TRUE, scale = FALSE))
X <- as.matrix(scale(packr(xdata[-1,]), center = TRUE, scale = TRUE))
n.fac <- 4

# based on svd directly
sv <- svd(X)
scores <- X %*% sv$v
F.est <- scores[,1:n.fac]

factor.est.pca <- F.est
# factor.est.pca.ts <- ts(factor.est.pca, frequency = 4, start = c(1959, 1), end = c(2017, 4))
factor.est.pca.ts <- ts(factor.est.pca, frequency = 4, start = c(1959, 2), end = c(2017, 4))

# factor estimation using all variables based on alternating LS
# factor.est.ite <- factor_estimation_ls(xdata = xdata, n.fac = 4, nt.min = 20, init = "svd")
factor.est.ite <- factor_estimation_ls(xdata = xdata[-1,], n.fac = 4, nt.min = 20, init = "svd")
# factor.est.ite.ts <- ts(factor.est.ite$fac, frequency = 4, start = c(1959, 1), end = c(2017, 4))
factor.est.ite.ts <- ts(factor.est.ite$fac, frequency = 4, start = c(1959, 2), end = c(2017, 4))

# use canonical correlation to compare the estimate factors
cc <- cancor(
  scale(factor.est.pca.ts),
  scale(factor.est.ite.ts)
)

cc$cor
```

```
## [1] 0.9965276 0.9916752 0.9587087 0.7363387
```


``` r
#..................................................
# Merge time series ----

# use some date sequence as basis for ts
date <- ts(seq.Date(from = as.Date("1950-01-01"), to = as.Date("2025-01-01"), by = "quarter"), frequency = 4, start = c(1950, 1), end = c(2025,1))
TT <- length(date)

# merge all variables/factors
data.all.ts <- cbind(date, macro.01.ts, macro.02.ts, factor.est.pca.ts, factor.est.ite.ts) # based on estimated factors

# extract time period of interest
data.all.ts <- window(data.all.ts, start = c(1959, 1), end = c(2017, 4))
# corresponding date
date <- seq.Date(from = as.Date("1959-01-01"), to = as.Date("2017-10-01"), by = "quarter")
```

## FAVAR Analyse No 01

Nicht-rekursive Schätzung der Faktoren auf der Grundlage von Variablen ohne fehlende Werte


``` r
#..................................................
# A 1) Unfair analysis on variables without missing values ----

# A 1.1) h=1 ----

# dates data frame (date column is important!)
date.df <- data.frame(date = date,
                      year = format(date, "%Y"),
                      quarter = quarters(date),
                      month = format(date, "%m"),
                      day = format(date, "%d"))

# target variable (data frame)
GDP <- as.numeric(data.all.ts[,2])
h <- 1

data.01 <- data.frame(GDPGR_h = (400/h) * (log(GDP / lag_fun(GDP, h))))

# observed predictors (variables) (data frame)

# factors estimated based on PCA (without missing values)
F1 <- as.numeric(data.all.ts[,136])
F2 <- as.numeric(data.all.ts[,137])
F3 <- as.numeric(data.all.ts[,138])
F4 <- as.numeric(data.all.ts[,139])

data.02 <- data.frame(GDPGR_h_l1 = lag_fun(log(GDP / lag_fun(GDP, 1)), h),
                      GDPGR_h_l2 = lag_fun(log(GDP / lag_fun(GDP, 1)), h + 1),
                      F1_h_l1 = lag_fun(F1, h),
                      F2_h_l1 = lag_fun(F2, h),
                      F3_h_l1 = lag_fun(F3, h),
                      F4_h_l1 = lag_fun(F4, h),
                      CONST = 1)

# observed predictors for (fair) construction of unobserved factors (matrix/array)
data.03 <- NULL

log.file <- here("11-session-03-03-df-modelle", "03-ergebnisse", "favar_01_h1_poos_diagnosen.txt")
FVAR.POOS.r4.h1.01 <- FVAR_POOS_function(date.df = date.df, data.01 = data.01, data.02 = data.02, data.03 = data.03, h = 1, n.fac = 4, fre = "quarter",
                                         all.per.sta = as.Date("1981-01-01"), all.per.end = as.Date("2017-10-01"),
                                         est.per.sta = as.Date("1981-01-01"), est.per.end = as.Date("2002-10-01"),
                                         pre.per.sta = as.Date("2002-10-01"), pre.per.end = as.Date("2017-10-01"),
                                         mis.val = TRUE, print = TRUE, log.file = log.file)
FVAR.POOS.r4.h1.01$RMSFE.POOS
```

```
## [1] 2.072482
```

``` r
# A 1.2) h=4 ----

# dates data frame (date column is important!)
date.df <- data.frame(date = date,
                      year = format(date, "%Y"),
                      quarter = quarters(date),
                      month = format(date, "%m"),
                      day = format(date, "%d"))

# target variable (data frame)
GDP <- as.numeric(data.all.ts[,2])
h <- 4

data.01 <- data.frame(GDPGR_h = (400/h) * (log(GDP / lag_fun(GDP, h))))

# observed predictors (variables) (data frame)
F1 <- as.numeric(data.all.ts[,136])
F2 <- as.numeric(data.all.ts[,137])
F3 <- as.numeric(data.all.ts[,138])
F4 <- as.numeric(data.all.ts[,139])

data.02 <- data.frame(GDPGR_h_l1 = lag_fun(log(GDP / lag_fun(GDP, 1)), h),
                      GDPGR_h_l2 = lag_fun(log(GDP / lag_fun(GDP, 1)), h + 1),
                      F1_h_l1 = lag_fun(F1, h),
                      F2_h_l1 = lag_fun(F2, h),
                      F3_h_l1 = lag_fun(F3, h),
                      F4_h_l1 = lag_fun(F4, h),
                      CONST = 1)

# observed predictors for (fair) construction of unobserved factors (matrix/array)
data.03 <- NULL

log.file <- here("11-session-03-03-df-modelle", "03-ergebnisse", "favar_01_h4_poos_diagnosen.txt")
FVAR.POOS.r4.h4.01 <- FVAR_POOS_function(date.df = date.df, data.01 = data.01, data.02 = data.02, data.03 = data.03, h = 4, n.fac = 4, fre = "quarter",
                                         all.per.sta = as.Date("1981-01-01"), all.per.end = as.Date("2017-10-01"),
                                         est.per.sta = as.Date("1981-01-01"), est.per.end = as.Date("2002-10-01"),
                                         pre.per.sta = as.Date("2002-10-01"), pre.per.end = as.Date("2017-10-01"),
                                         mis.val = TRUE, print = TRUE, log.file = log.file)
FVAR.POOS.r4.h4.01$RMSFE.POOS
```

```
## [1] 1.41133
```

``` r
# A 1.3) h=8 ----

# dates data frame (date column is important!)
date.df <- data.frame(date = date,
                      year = format(date, "%Y"),
                      quarter = quarters(date),
                      month = format(date, "%m"),
                      day = format(date, "%d"))

# target variable (data frame)
GDP <- as.numeric(data.all.ts[,2])
h <- 8

data.01 <- data.frame(GDPGR_h = (400/h) * (log(GDP / lag_fun(GDP, h))))

# observed predictors (variables) (data frame)
F1 <- as.numeric(data.all.ts[,136])
F2 <- as.numeric(data.all.ts[,137])
F3 <- as.numeric(data.all.ts[,138])
F4 <- as.numeric(data.all.ts[,139])

data.02 <- data.frame(GDPGR_h_l1 = lag_fun(log(GDP / lag_fun(GDP, 1)), h),
                      GDPGR_h_l2 = lag_fun(log(GDP / lag_fun(GDP, 1)), h + 1),
                      F1_h_l1 = lag_fun(F1, h),
                      F2_h_l1 = lag_fun(F2, h),
                      F3_h_l1 = lag_fun(F3, h),
                      F4_h_l1 = lag_fun(F4, h),
                      CONST = 1)

# observed predictors for (fair) construction of unobserved factors (matrix/array)
data.03 <- NULL

log.file <- here("11-session-03-03-df-modelle", "03-ergebnisse", "favar_01_h8_poos_diagnosen.txt")
FVAR.POOS.r4.h8.01 <- FVAR_POOS_function(date.df = date.df, data.01 = data.01, data.02 = data.02, data.03 = data.03, h = 8, n.fac = 4, fre = "quarter",
                                         all.per.sta = as.Date("1981-01-01"), all.per.end = as.Date("2017-10-01"),
                                         est.per.sta = as.Date("1981-01-01"), est.per.end = as.Date("2002-10-01"),
                                         pre.per.sta = as.Date("2002-10-01"), pre.per.end = as.Date("2017-10-01"),
                                         mis.val = TRUE, print = TRUE, log.file = log.file)
FVAR.POOS.r4.h8.01$RMSFE.POOS
```

```
## [1] 1.478493
```

## FAVAR Analyse No 02

Nicht-rekursive Schätzung der Faktoren auf der Grundlage aller Variablen (vgl. S&W, 2020, Tabelle 17.3, Zeile 3)


``` r
#..................................................
# A 2) Unfair analysis based on all observations ----

# A 2.1) h=1 ----

# dates data frame (date column is important!)
date.df <- data.frame(date = date,
                      year = format(date, "%Y"),
                      quarter = quarters(date),
                      month = format(date, "%m"),
                      day = format(date, "%d"))

# target variable (data frame)
GDP <- as.numeric(data.all.ts[,2])
h <- 1

data.01 <- data.frame(GDPGR_h = (400/h) * (log(GDP / lag_fun(GDP, h))))

# observed predictors (variables) (data frame)

# factors estimated based on alternating least squares (with missing values)
F1 <- as.numeric(data.all.ts[,140])
F2 <- as.numeric(data.all.ts[,141])
F3 <- as.numeric(data.all.ts[,142])
F4 <- as.numeric(data.all.ts[,143])

data.02 <- data.frame(GDPGR_h_l1 = lag_fun(log(GDP / lag_fun(GDP, 1)), h),
                      GDPGR_h_l2 = lag_fun(log(GDP / lag_fun(GDP, 1)), h + 1),
                      F1_h_l1 = lag_fun(F1, h),
                      F2_h_l1 = lag_fun(F2, h),
                      F3_h_l1 = lag_fun(F3, h),
                      F4_h_l1 = lag_fun(F4, h),
                      CONST = 1)

# observed predictors for (fair) construction of unobserved factors (matrix/array)
data.03 <- NULL

log.file <- here("11-session-03-03-df-modelle", "03-ergebnisse", "favar_02_h1_poos_diagnosen.txt")
FVAR.POOS.r4.h1.02 <- FVAR_POOS_function(date.df = date.df, data.01 = data.01, data.02 = data.02, data.03 = data.03, h = 1, n.fac = 4, fre = "quarter",
                                         all.per.sta = as.Date("1981-01-01"), all.per.end = as.Date("2017-10-01"),
                                         est.per.sta = as.Date("1981-01-01"), est.per.end = as.Date("2002-10-01"),
                                         pre.per.sta = as.Date("2002-10-01"), pre.per.end = as.Date("2017-10-01"),
                                         mis.val = TRUE, print = TRUE, log.file = log.file)
FVAR.POOS.r4.h1.02$RMSFE.POOS
```

```
## [1] 2.143408
```

``` r
# A 2.2) h=4 ----

# dates data frame (date column is important!)
date.df <- data.frame(date = date,
                      year = format(date, "%Y"),
                      quarter = quarters(date),
                      month = format(date, "%m"),
                      day = format(date, "%d"))

# target variable (data frame)
GDP <- as.numeric(data.all.ts[,2])
h <- 4

data.01 <- data.frame(GDPGR_h = (400/h) * (log(GDP / lag_fun(GDP, h))))

# observed predictors (variables) (data frame)
F1 <- as.numeric(data.all.ts[,140])
F2 <- as.numeric(data.all.ts[,141])
F3 <- as.numeric(data.all.ts[,142])
F4 <- as.numeric(data.all.ts[,143])

data.02 <- data.frame(GDPGR_h_l1 = lag_fun(log(GDP / lag_fun(GDP, 1)), h),
                      GDPGR_h_l2 = lag_fun(log(GDP / lag_fun(GDP, 1)), h + 1),
                      F1_h_l1 = lag_fun(F1, h),
                      F2_h_l1 = lag_fun(F2, h),
                      F3_h_l1 = lag_fun(F3, h),
                      F4_h_l1 = lag_fun(F4, h),
                      CONST = 1)

# observed predictors for (fair) construction of unobserved factors (matrix/array)
data.03 <- NULL

log.file <- here("11-session-03-03-df-modelle", "03-ergebnisse", "favar_02_h4_poos_diagnosen.txt")
FVAR.POOS.r4.h4.02 <- FVAR_POOS_function(date.df = date.df, data.01 = data.01, data.02 = data.02, data.03 = data.03, h = 4, n.fac = 4, fre = "quarter",
                                         all.per.sta = as.Date("1981-01-01"), all.per.end = as.Date("2017-10-01"),
                                         est.per.sta = as.Date("1981-01-01"), est.per.end = as.Date("2002-10-01"),
                                         pre.per.sta = as.Date("2002-10-01"), pre.per.end = as.Date("2017-10-01"),
                                         mis.val = TRUE, print = TRUE, log.file = log.file)
FVAR.POOS.r4.h4.02$RMSFE.POOS
```

```
## [1] 1.404919
```

``` r
# A 2.3) h=8 ----

# dates data frame (date column is important!)
date.df <- data.frame(date = date,
                      year = format(date, "%Y"),
                      quarter = quarters(date),
                      month = format(date, "%m"),
                      day = format(date, "%d"))

# target variable (data frame)
GDP <- as.numeric(data.all.ts[,2])
h <- 8

data.01 <- data.frame(GDPGR_h = (400/h) * (log(GDP / lag_fun(GDP, h))))

# observed predictors (variables) (data frame)
F1 <- as.numeric(data.all.ts[,140])
F2 <- as.numeric(data.all.ts[,141])
F3 <- as.numeric(data.all.ts[,142])
F4 <- as.numeric(data.all.ts[,143])

data.02 <- data.frame(GDPGR_h_l1 = lag_fun(log(GDP / lag_fun(GDP, 1)), h),
                      GDPGR_h_l2 = lag_fun(log(GDP / lag_fun(GDP, 1)), h + 1),
                      F1_h_l1 = lag_fun(F1, h),
                      F2_h_l1 = lag_fun(F2, h),
                      F3_h_l1 = lag_fun(F3, h),
                      F4_h_l1 = lag_fun(F4, h),
                      CONST = 1)

# observed predictors for (fair) construction of unobserved factors (matrix/array)
data.03 <- NULL

log.file <- here("11-session-03-03-df-modelle", "03-ergebnisse", "favar_02_h8_poos_diagnosen.txt")
FVAR.POOS.r4.h8.02 <- FVAR_POOS_function(date.df = date.df, data.01 = data.01, data.02 = data.02, data.03 = data.03, h = 8, n.fac = 4, fre = "quarter",
                                         all.per.sta = as.Date("1981-01-01"), all.per.end = as.Date("2017-10-01"),
                                         est.per.sta = as.Date("1981-01-01"), est.per.end = as.Date("2002-10-01"),
                                         pre.per.sta = as.Date("2002-10-01"), pre.per.end = as.Date("2017-10-01"),
                                         mis.val = TRUE, print = TRUE, log.file = log.file)
FVAR.POOS.r4.h8.02$RMSFE.POOS
```

```
## [1] 1.4786
```

## FAVAR Analyse No 03

Rekursive Schätzung der Faktoren auf der Grundlage von Variablen ohne fehlende Werte.


``` r
#..................................................
# A 3) Fair analysis based on variables without missing values ----

# A 3.1) h=1 ----

# dates data frame (date column is important!)
date.df <- data.frame(date = date,
                      year = format(date, "%Y"),
                      quarter = quarters(date),
                      month = format(date, "%m"),
                      day = format(date, "%d"))

# target variable (data frame)
GDP <- as.numeric(data.all.ts[,2])
h <- 1

data.01 <- data.frame(GDPGR_h = (400/h) * (log(GDP / lag_fun(GDP, h))))

# observed predictors (variables) (data frame)
data.02 <- data.frame(GDPGR_h_l1 = lag_fun(log(GDP / lag_fun(GDP, 1)), h),
                      GDPGR_h_l2 = lag_fun(log(GDP / lag_fun(GDP, 1)), h + 1),
                      CONST = 1)

# observed predictors for (fair) construction of unobserved factors (matrix/array)
data.03 <- data.all.ts[,which(colnames(data.all.ts) %in% paste0("macro.02.ts.V", seq(1,ncol(macro.02.ts))))]
colnames(data.03) <- paste0("V", seq(1,ncol(data.03)))

log.file <- here("11-session-03-03-df-modelle", "03-ergebnisse", "favar_03_h1_poos_diagnosen.txt")
FVAR.POOS.r4.h1.03 <- FVAR_POOS_function(date.df = date.df, data.01 = data.01, data.02 = data.02, data.03 = data.03, h = 1, n.fac = 4, fre = "quarter",
                                         all.per.sta = as.Date("1981-01-01"), all.per.end = as.Date("2017-10-01"),
                                         est.per.sta = as.Date("1981-01-01"), est.per.end = as.Date("2002-10-01"),
                                         pre.per.sta = as.Date("2002-10-01"), pre.per.end = as.Date("2017-10-01"),
                                         mis.val = FALSE, print = TRUE, log.file = log.file)
FVAR.POOS.r4.h1.03$RMSFE.POOS
```

```
## [1] 2.8809
```

``` r
# A 3.2) h=4 ----

# dates data frame (date column is important!)
date.df <- data.frame(date = date,
                      year = format(date, "%Y"),
                      quarter = quarters(date),
                      month = format(date, "%m"),
                      day = format(date, "%d"))

# target variable (data frame)
GDP <- as.numeric(data.all.ts[,2])
h <- 4

data.01 <- data.frame(GDPGR_h = (400/h) * (log(GDP / lag_fun(GDP, h))))

# observed predictors (variables) (data frame)
data.02 <- data.frame(GDPGR_h_l1 = lag_fun(log(GDP / lag_fun(GDP, 1)), h),
                      GDPGR_h_l2 = lag_fun(log(GDP / lag_fun(GDP, 1)), h + 1),
                      CONST = 1)

# observed predictors for (fair) construction of unobserved factors (matrix/array)
data.03 <- data.all.ts[,which(colnames(data.all.ts) %in% paste0("macro.02.ts.V", seq(1,ncol(macro.02.ts))))]
colnames(data.03) <- paste0("V", seq(1,ncol(data.03)))

log.file <- here("11-session-03-03-df-modelle", "03-ergebnisse", "favar_03_h4_poos_diagnosen.txt")
FVAR.POOS.r4.h4.03 <- FVAR_POOS_function(date.df = date.df, data.01 = data.01, data.02 = data.02, data.03 = data.03, h = 4, n.fac = 4, fre = "quarter",
                                         all.per.sta = as.Date("1981-01-01"), all.per.end = as.Date("2017-10-01"),
                                         est.per.sta = as.Date("1981-01-01"), est.per.end = as.Date("2002-10-01"),
                                         pre.per.sta = as.Date("2002-10-01"), pre.per.end = as.Date("2017-10-01"),
                                         mis.val = FALSE, print = TRUE, log.file = log.file)
FVAR.POOS.r4.h4.03$RMSFE.POOS
```

```
## [1] 2.331027
```

``` r
# A 3.3) h=8 ----

# dates data frame (date column is important!)
date.df <- data.frame(date = date,
                      year = format(date, "%Y"),
                      quarter = quarters(date),
                      month = format(date, "%m"),
                      day = format(date, "%d"))

# target variable (data frame)
GDP <- as.numeric(data.all.ts[,2])
h <- 8

data.01 <- data.frame(GDPGR_h = (400/h) * (log(GDP / lag_fun(GDP, h))))

# observed predictors (variables) (data frame)
data.02 <- data.frame(GDPGR_h_l1 = lag_fun(log(GDP / lag_fun(GDP, 1)), h),
                      GDPGR_h_l2 = lag_fun(log(GDP / lag_fun(GDP, 1)), h + 1),
                      CONST = 1)

# observed predictors for (fair) construction of unobserved factors (matrix/array)
data.03 <- data.all.ts[,which(colnames(data.all.ts) %in% paste0("macro.02.ts.V", seq(1,ncol(macro.02.ts))))]
colnames(data.03) <- paste0("V", seq(1,ncol(data.03)))

log.file <- here("11-session-03-03-df-modelle", "03-ergebnisse", "favar_03_h8_poos_diagnosen.txt")
FVAR.POOS.r4.h8.03 <- FVAR_POOS_function(date.df = date.df, data.01 = data.01, data.02 = data.02, data.03 = data.03, h = 8, n.fac = 4, fre = "quarter",
                                         all.per.sta = as.Date("1981-01-01"), all.per.end = as.Date("2017-10-01"),
                                         est.per.sta = as.Date("1981-01-01"), est.per.end = as.Date("2002-10-01"),
                                         pre.per.sta = as.Date("2002-10-01"), pre.per.end = as.Date("2017-10-01"),
                                         mis.val = FALSE, print = TRUE, log.file = log.file)
FVAR.POOS.r4.h8.03$RMSFE.POOS
```

```
## [1] 2.10754
```

## FAVAR Analyse No 04

Rekursive Schätzung der Faktoren auf der Grundlage aller Variablen


``` r
#..................................................
# A 4) Fair analysis based on all variables ----

# A 4.1) h=1 ----

# dates data frame (date column is important!)
date.df <- data.frame(date = date,
                      year = format(date, "%Y"),
                      quarter = quarters(date),
                      month = format(date, "%m"),
                      day = format(date, "%d"))

# target variable (data frame)
GDP <- as.numeric(data.all.ts[,2])
h <- 1

data.01 <- data.frame(GDPGR_h = (400/h) * (log(GDP / lag_fun(GDP, h))))

# observed predictors (variables) (data frame)
data.02 <- data.frame(GDPGR_h_l1 = lag_fun(log(GDP / lag_fun(GDP, 1)), h),
                      GDPGR_h_l2 = lag_fun(log(GDP / lag_fun(GDP, 1)), h + 1),
                      CONST = 1)

# observed predictors for (fair) construction of unobserved factors (matrix/array)
data.03 <- data.all.ts[,which(colnames(data.all.ts) %in% paste0("macro.02.ts.V", seq(1,ncol(macro.02.ts))))]
colnames(data.03) <- paste0("V", seq(1,ncol(data.03)))

log.file <- here("11-session-03-03-df-modelle", "03-ergebnisse", "favar_04_h1_poos_diagnosen.txt")
FVAR.POOS.r4.h1.04 <- FVAR_POOS_function(date.df = date.df, data.01 = data.01, data.02 = data.02, data.03 = data.03, h = 1, n.fac = 4, fre = "quarter",
                                         all.per.sta = as.Date("1981-01-01"), all.per.end = as.Date("2017-10-01"),
                                         est.per.sta = as.Date("1981-01-01"), est.per.end = as.Date("2002-10-01"),
                                         pre.per.sta = as.Date("2002-10-01"), pre.per.end = as.Date("2017-10-01"),
                                         mis.val = TRUE, print = TRUE, log.file = log.file)
FVAR.POOS.r4.h1.04$RMSFE.POOS
```

```
## [1] 2.876191
```

``` r
# A 4.2) h=4 ----

# dates data frame (date column is important!)
date.df <- data.frame(date = date,
                      year = format(date, "%Y"),
                      quarter = quarters(date),
                      month = format(date, "%m"),
                      day = format(date, "%d"))

# target variable (data frame)
GDP <- as.numeric(data.all.ts[,2])
h <- 4

data.01 <- data.frame(GDPGR_h = (400/h) * (log(GDP / lag_fun(GDP, h))))

# observed predictors (variables) (data frame)
data.02 <- data.frame(GDPGR_h_l1 = lag_fun(log(GDP / lag_fun(GDP, 1)), h),
                      GDPGR_h_l2 = lag_fun(log(GDP / lag_fun(GDP, 1)), h + 1),
                      CONST = 1)

# observed predictors for (fair) construction of unobserved factors (matrix/array)
data.03 <- data.all.ts[,which(colnames(data.all.ts) %in% paste0("macro.02.ts.V", seq(1,ncol(macro.02.ts))))]
colnames(data.03) <- paste0("V", seq(1,ncol(data.03)))

log.file <- here("11-session-03-03-df-modelle", "03-ergebnisse", "favar_04_h4_poos_diagnosen.txt")
FVAR.POOS.r4.h4.04 <- FVAR_POOS_function(date.df = date.df, data.01 = data.01, data.02 = data.02, data.03 = data.03, h = 4, n.fac = 4, fre = "quarter",
                                         all.per.sta = as.Date("1981-01-01"), all.per.end = as.Date("2017-10-01"),
                                         est.per.sta = as.Date("1981-01-01"), est.per.end = as.Date("2002-10-01"),
                                         pre.per.sta = as.Date("2002-10-01"), pre.per.end = as.Date("2017-10-01"),
                                         mis.val = TRUE, print = TRUE, log.file = log.file)
FVAR.POOS.r4.h4.04$RMSFE.POOS
```

```
## [1] 2.2812
```

``` r
# A 4.3) h=8 ----

# dates data frame (date column is important!)
date.df <- data.frame(date = date,
                      year = format(date, "%Y"),
                      quarter = quarters(date),
                      month = format(date, "%m"),
                      day = format(date, "%d"))

# target variable (data frame)
GDP <- as.numeric(data.all.ts[,2])
h <- 8

data.01 <- data.frame(GDPGR_h = (400/h) * (log(GDP / lag_fun(GDP, h))))

# observed predictors (variables) (data frame)
data.02 <- data.frame(GDPGR_h_l1 = lag_fun(log(GDP / lag_fun(GDP, 1)), h),
                      GDPGR_h_l2 = lag_fun(log(GDP / lag_fun(GDP, 1)), h + 1),
                      CONST = 1)

# observed predictors for (fair) construction of unobserved factors (matrix/array)
data.03 <- data.all.ts[,which(colnames(data.all.ts) %in% paste0("macro.02.ts.V", seq(1,ncol(macro.02.ts))))]
colnames(data.03) <- paste0("V", seq(1,ncol(data.03)))

log.file <- here("11-session-03-03-df-modelle", "03-ergebnisse", "favar_04_h8_poos_diagnosen.txt")
FVAR.POOS.r4.h8.04 <- FVAR_POOS_function(date.df = date.df, data.01 = data.01, data.02 = data.02, data.03 = data.03, h = 8, n.fac = 4, fre = "quarter",
                                         all.per.sta = as.Date("1981-01-01"), all.per.end = as.Date("2017-10-01"),
                                         est.per.sta = as.Date("1981-01-01"), est.per.end = as.Date("2002-10-01"),
                                         pre.per.sta = as.Date("2002-10-01"), pre.per.end = as.Date("2017-10-01"),
                                         mis.val = TRUE, print = TRUE, log.file = log.file)
FVAR.POOS.r4.h8.04$RMSFE.POOS
```

```
## [1] 1.987123
```

## Vergleich Ergebnisse


```
##                                    h1     h4     h8
## Non-recursive, no missing vars 2.0725 1.4113 1.4785
## Non-recursive, all vars        2.1434 1.4049 1.4786
## Recursive, no missing vars     2.8809 2.3310 2.1075
## Recursive, all vars            2.8762 2.2812 1.9871
```
