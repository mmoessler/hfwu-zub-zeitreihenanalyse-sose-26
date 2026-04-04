library(here)
library(knitr)

setwd(here("01-session-01-01-einfuehrung"))

knitr::opts_chunk$set(
  fig.path = "./03-ergebnisse/",
  dev="svg"
)

knit(
  input  = here("00-session-kick-off", "04-berichte", "hfwu_za_sose_26_00_01_r_einfuehrung.Rmd"),
  output = here("00-session-kick-off", "hfwu_za_sose_26_00_01_r_einfuehrung_page.md")
)

knit(
  input  = here("00-session-kick-off", "04-berichte", "hfwu_za_sose_26_00_02_rmd_einfuehrung.Rmd"),
  output = here("00-session-kick-off", "hfwu_za_sose_26_00_02_rmd_einfuehrung_page.md")
)