
library(data.table)

source(paste0(chr.dir.sub.models.cow.calf, "/", chr.file.R.cow.calf))


chr.input.cowcalf.tpl <- scan(file = paste0(chr.dir.sub.models.cow.calf,
                                            "/", chr.file.tpl.cow.calf),
                              sep = "\n", what = "character", quiet = TRUE)

df.input.cowcalf.tpl <- read.delim(file = paste0(chr.dir.sub.models.cow.calf,
                                                 "/", chr.file.tpl.cow.calf),
                               sep = ":", comment.char = "*", row.names = NULL,
                               header = FALSE, stringsAsFactors = FALSE)
names(df.input.cowcalf.tpl) <- c("par", "value")

dt.input.cowcalf.tpl <- data.table(df.input.cowcalf.tpl)
rm(df.input.cowcalf.tpl)

## populate parameter values that don't vary by sub-watershed
dt.input.cowcalf.tpl[par == "Watershed"]$value <- "Upper Yaquina River"
dt.input.cowcalf.tpl[par == "Date this Input File Created"]$value <- 
  format(Sys.Date(), "%Y-%m-%d")
dt.input.cowcalf.tpl[par == "MUTSIN Start Year"]$value <- "2015"
dt.input.cowcalf.tpl[par == "MUTSIN End Year"]$value <- "2016"
dt.input.cowcalf.tpl[par == "HSPF-Sup File Header Number for Pasture in MON-ACCUM Table"]$value <- 
  "10"
dt.input.cowcalf.tpl[par == "HSPF-Sup File Header Number for Pasture in MON-SQOLIM Table"]$value <- 
  "12"
dt.input.cowcalf.tpl[par == "HSPF-Sup File Header Number for Forest in MON-ACCUM Table"]$value <- 
  "11"
dt.input.cowcalf.tpl[par == "HSPF-Sup File Header Number for Forest in MON-SQOLIM Table"]$value <- 
  "13"
dt.input.cowcalf.tpl[par == "Average Stocking Density for Pasture in watershed (ac/Cow-Calf pair)"]$value <- 
  "10"
dt.input.cowcalf.tpl[par == "Fecal Coliform production by animal (org/(day-animal))"]$value <- 
  "1E+10"
dt.input.cowcalf.tpl[par == "SQOLIM multiplcation factor"]$value <- "9"
dt.input.cowcalf.tpl[par == "Percent of animals on pasture in and around streams"]$value <- 
  "10"
dt.input.cowcalf.tpl[par == "Percent of animals on forest in and around streams"]$value <- 
  "10"

## get sub watershed ids
vec.subs <- unique(dt.lu$sub.id)

## create list for input data.tables for sub-watersheds
lst.input.dt.cowcalf <- vector('list', length(vec.subs))

## loop through sub-watersheds and create input data.tables
for(ii in vec.subs) {
  dt.temp <- dt.input.cowcalf.tpl
  chr.lu.cow.calf <- "pasture"
  dt.temp[par == "Pasture Area in Watershed (ac)"]$value <- 
    as.character(dt.lu[sub.id == ii & lu == chr.lu.cow.calf]$area_ac)
  dt.temp[par == "Percent of pasture with stream access"]$value <- 
    sprintf(fmt = "%.2f",dt.lu[sub.id == ii & lu == chr.lu.cow.calf]$percent_ac)
  chr.lu.cow.calf <- "forest"
  dt.temp[par == "Forest Area in Watershed (ac)"]$value <- 
    as.character(dt.lu[sub.id == ii & lu == chr.lu.cow.calf]$area_ac)
  dt.temp[par == "Percent of forest with stream access"]$value <- 
    sprintf(fmt = "%.2f", dt.lu[sub.id == ii & lu == chr.lu.cow.calf]$percent_ac)
  lst.input.dt.cowcalf[[ii]] <- dt.temp
  rm(dt.temp)
}

## set names of the input data.tables
names(lst.input.dt.cowcalf) <- paste0("sub", sprintf(fmt = "%02i", vec.subs))

## create list of sub-model input file names
chr.files.input <- paste0(chr.dir.sub.models.cow.calf, "/",
                         names(lst.input.dt.cowcalf), "cowcalf.txt")


## write data.tables to input files
for(jj in 1:length(vec.subs)) {
  chr.input.temp.tpl <- chr.input.cowcalf.tpl
  int.rows.input.tpl <- grep("^\\*", chr.input.temp.tpl, invert = TRUE)
  for(kk in 1:length(lst.input.dt.cowcalf[[jj]]$par)) {
    chr.input.temp.tpl[int.rows.input.tpl[kk]] <- 
      paste0(lst.input.dt.cowcalf[[jj]][kk,], collapse = ": ")
  }
  tmp.file.input <- paste0(chr.dir.sub.models.cow.calf, "/",
                           names(lst.input.dt.cowcalf[jj]), "cowcalf.txt")
  cat(chr.input.temp.tpl, file = chr.files.input[kk], sep = "\n")
  rm(chr.input.temp.tpl, kk)  
}

lst.tmp <- lapply(chr.files.input, cow.calf)
lst.out <- lapply(lst.tmp, data.table)

str(junk)

junk <- lst.out[[1]]

junk[Month == "Jan", ]

junk[, names(junk)[-1 * 1:max(grep("pairs", names(junk)))]:=NULL]

int.pasture.onland <- c(4,5)

junk[ int.pasture.onland]

eval(parse(text = names(junk)[int.pasture.onland]))

junk[sum(pairs.OnPastureWOStreamAccess), by=Month]

str(junk[2,])

junk[, lapply(grep("pairs", names(junk)), sum), ]

junk[, lapply(.SD, add), by=Month, .SDcols=names(junk)[int.pasture.onland]]
