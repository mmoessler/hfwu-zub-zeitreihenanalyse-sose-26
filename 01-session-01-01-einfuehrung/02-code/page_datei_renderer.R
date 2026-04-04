library(here)
library(knitr)

setwd(here("01-session-01-01-einfuehrung"))

knitr::opts_chunk$set(
  fig.path = "./03-ergebnisse/",
  dev="svg"
)

knit(
  input  = here("01-session-01-01-einfuehrung", "04-berichte", "hfwu_za_sose_26_01_00_makro_daten_aufbereitung.Rmd"),
  output = here("01-session-01-01-einfuehrung", "hfwu_za_sose_26_01_00_makro_daten_aufbereitung_page.md")
)

knit(
  input  = here("01-session-01-01-einfuehrung", "04-berichte", "hfwu_za_sose_26_01_01_einfuehrung_r_session.Rmd"),
  output = here("01-session-01-01-einfuehrung", "hfwu_za_sose_26_01_01_einfuehrung_r_session_page.md")
)