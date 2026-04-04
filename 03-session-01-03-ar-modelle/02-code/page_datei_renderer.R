 
library(here)
library(knitr)

setwd(here("03-session-01-03-ar-modelle"))

knitr::opts_chunk$set(
  fig.path = "./03-ergebnisse/",
  dev="svg"
)

knit(
  input  = here("03-session-01-03-ar-modelle", "04-berichte", "hfwu_za_sose_26_01_03_ar_modelle_r_session.Rmd"),
  output = here("03-session-01-03-ar-modelle", "hfwu_za_sose_26_01_03_ar_modelle_r_session_page.md")
)
