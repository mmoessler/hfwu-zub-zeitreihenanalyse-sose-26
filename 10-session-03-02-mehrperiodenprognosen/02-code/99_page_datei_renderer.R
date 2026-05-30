
library(here)
library(knitr)

setwd(here("10-session-03-02-mehrperiodenprognosen"))

knitr::opts_chunk$set(
  fig.path = "./03-ergebnisse/",
  dev="svg"
)

knit(
  input  = here("10-session-03-02-mehrperiodenprognosen", "04-berichte", "hfwu_za_sose_26_03_02_mehrperioden_prognosen_r_session.Rmd"),
  output = here("10-session-03-02-mehrperiodenprognosen", "hfwu_za_sose_26_03_02_mehrperioden_prognosen_r_session_page.md")
)
