## -------------------------------------------------------------------------------------------------------------------------------------------
# Optionen Rendering
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
knitr::opts_chunk$set(echo = TRUE,
                      message = FALSE,
                      warning = FALSE)

# Säubere Umgebung
rm(list=ls())

# Lade Pakete
library(R.matlab)



#..................................................
# 1) Data processing: Factor_Data_1.m ----
# -> The matlab script Factor_Data_1.m processes the data from Excel to an matlab matrix

tmp <- readMat(con = rprojroot::find_rstudio_root_file("03-daten/datain.mat"))

bpdata_raw <- tmp$datain[1,1,1]$bpdata.raw
bpdata <- tmp$datain[2,1,1]$bpdata # use this for estimation
bpdata_noa <- tmp$datain[3,1,1]$bpdata.noa
bpnamevec <- tmp$datain[4,1,1]$bpnamevec
bplabvec.long <- tmp$datain[5,1,1]$bplabvec.long
bplabvec.short <- tmp$datain[6,1,1]$bplabvec.short
bpoutliervec <- tmp$datain[7,1,1]$bpoutliervec
bptcodevec <- tmp$datain[8,1,1]$bptcodevec
bpcatcode <- tmp$datain[9,1,1]$bpcatcode
bpinclcode <- tmp$datain[10,1,1]$bpinclcode # use this for estimation
calvec <- tmp$datain[11,1,1]$calvec
calds <- tmp$datain[12,1,1]$calds
dnobs <- tmp$datain[13,1,1]$dnobs

write.table(bpdata, file = rprojroot::find_rstudio_root_file("03-daten/us_all_macro_data.txt"), sep = ",", col.names = TRUE)
