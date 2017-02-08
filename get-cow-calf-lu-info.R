get.cow.calf.lu <- function(chr.dir.lu, chr.file.dbf.pasture, 
                            chr.file.dbf.forest) {
  ## load packages
  library(foreign)
  library(data.table)
  
  ## path th the land use tables
  chr.dir.lu <- "M:/GIS/DOTMDL/UpperYaquinaRiver/SourceAssessment/riparian-access"
  
  ## pasture lu info file
  chr.file.dbf.pasture <- "pasture_rip_access_summary_general_model.dbf"
  
  ## forest lu info file
  chr.file.dbf.forest <- "forest_rip_access_summary_general_model.dbf"
  
  ## read tables
  df.lu.raw.pasture <- read.dbf(paste0(chr.dir.lu, "/", chr.file.dbf.pasture))
  df.lu.raw.forest <- read.dbf(paste0(chr.dir.lu, "/", chr.file.dbf.forest))
  
  dt.lu <- data.table(rbind(data.frame(sub.id = df.lu.raw.pasture[, "Sub_ID"], lu = "pasture", df.lu.raw.pasture[, c("area_ac", "rip_acc_ar", "percent_ac")],
                                       stringsAsFactors = FALSE),
                            data.frame(sub.id = df.lu.raw.forest[, "Sub_ID"], lu = "forest", df.lu.raw.forest[, c("area_ac", "rip_acc_ar", "percent_ac")],
                                       stringsAsFactors = FALSE)))
  return(dt.lu)
}

