 
library(here)
library(knitr)

setwd(here("04-session-01-04-adl-modelle"))

knitr::opts_chunk$set(
  fig.path = "./03-ergebnisse/",
  dev="svg"
)

knit(
  input  = here("04-session-01-04-adl-modelle", "04-berichte", "hfwu_za_sose_26_01_04_adl_modelle_r_session.Rmd"),
  output = here("04-session-01-04-adl-modelle", "hfwu_za_sose_26_01_04_adl_modelle_r_session_page.md")
)
