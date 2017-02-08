get.cow.calf.lu <- function(chr.dir.lu, chr.file.dbf.pasture, 
                            chr.file.dbf.forest) {
  ## load packages
  library(foreign)
  library(data.table)
 
  ## read tables from dbf files created by ArcModels
  df.lu.raw.pasture <- read.dbf(paste0(chr.dir.lu, "/", chr.file.dbf.pasture))
  df.lu.raw.forest <- read.dbf(paste0(chr.dir.lu, "/", chr.file.dbf.forest))
 
  ## asssemble a data table from the lu data.frames 
  dt.lu <- data.table(rbind(data.frame(sub.id = df.lu.raw.pasture[, "Sub_ID"], lu = "pasture", df.lu.raw.pasture[, c("area_ac", "rip_acc_ar", "percent_ac")],
                                       stringsAsFactors = FALSE),
                            data.frame(sub.id = df.lu.raw.forest[, "Sub_ID"], lu = "forest", df.lu.raw.forest[, c("area_ac", "rip_acc_ar", "percent_ac")],
                                       stringsAsFactors = FALSE)))
  ## return data table
  return(dt.lu)
}

