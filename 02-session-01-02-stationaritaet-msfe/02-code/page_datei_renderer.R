library(here)
library(knitr)

setwd(here("02-session-01-02-stationaritaet-msfe"))

knitr::opts_chunk$set(
  fig.path = "./03-ergebnisse/",
  dev="svg"
)

knit(
  input  = here("02-session-01-02-stationaritaet-msfe", "04-berichte", "hfwu_za_sose_26_01_02_muenzwurf_vorhersage.Rmd"),
  output = here("02-session-01-02-stationaritaet-msfe", "hfwu_za_sose_26_01_02_muenzwurf_vorhersage_page.md")
)

knit(
  input  = here("02-session-01-02-stationaritaet-msfe", "04-berichte", "hfwu_za_sose_26_01_02_vorhersage_loss_vergleich.Rmd"),
  output = here("02-session-01-02-stationaritaet-msfe", "hfwu_za_sose_26_01_02_vorhersage_loss_vergleich_page.md")
)
