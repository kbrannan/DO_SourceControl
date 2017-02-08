## general project path
chr.dir.general <- "M:/Models/Nutrients/UpperYaquinaRiver/HSPF/bacteria-sub-models/DO_SourceControl"

## intialize general input
source(paste0(chr.dir.general, "/general-input-for-testing.R"))

## load functions
source(paste0(chr.dir.general, "/get-cow-calf-lu-info.R"))


## get lu info
dt.lu <- get.cow.calf.lu(chr.dir.lu, chr.file.dbf.pasture, chr.file.dbf.forest)
