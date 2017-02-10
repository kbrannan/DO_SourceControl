summarize.out <- function(dt.in) {
  df.in.0 <- melt(data.frame(dt.in), id.vars = "Month", 
                  variable.names = names(dt.in)[-1])
  
  df.in.1 <- data.frame(Month = df.in.0$Month, loc = NA, input = NA, 
                        value = df.in.0$value, variable = df.in.0$variable)
  
  df.in.1[grep("OnPasture", df.in.1$variable), "loc"] <- "pasture"
  df.in.1[grep("InForest", df.in.1$variable), "loc"] <- "forest"
  df.in.1[grep("InConfinement", df.in.1$variable), "loc"] <- "confinement"
  df.in.1[is.na(df.in.1$loc), "loc"] <- "none"
  df.in.1$loc <- factor(df.in.1$loc, 
                        levels = c("pasture", "forest", "confinement", "none"))
  
  
  df.in.1[grep("OnPasture", df.in.1$variable), "input"] <- "surface"
  df.in.1[grep("InForest", df.in.1$variable), "input"] <- "surface"
  df.in.1[grep("InConfinement", df.in.1$variable), "input"] <- "storage"
  df.in.1[grep("InStream", df.in.1$variable), "input"] <- "stream"
  df.in.1[is.na(df.in.1$input), "input"] <- "none"
  df.in.1$input <- factor(df.in.1$input, 
                          levels = c("surface", "stream", "storage", "none"))
  
  dt.out <- data.table(df.in.1)
  
  rm(df.in.0, df.in.1)
  
  ## summarize out loc and input by month
  dt.sum <- data.table(Month = dt.out[variable == "NumOfPairs", Month],
                       cat = "total-pairs",
                       value = dt.out[variable == "NumOfPairs", value])
  
  dt.temp <- data.table(Month = dt.out[variable == "AUvsTime", Month],
                        cat = "total-AU",
                        value = dt.out[variable == "AUvsTime", value])
  names(dt.temp) <- names(dt.sum)
  dt.sum <- rbind(dt.sum,dt.temp)
  rm(dt.temp)
  
  dt.temp <- dt.out[, lapply(.SD, sum), by=list(Month, input)][input == "stream", list(Month, input, value)]
  names(dt.temp) <- names(dt.sum)
  dt.sum <- rbind(dt.sum,dt.temp)
  rm(dt.temp)
  
  dt.temp <- dt.out[, lapply(.SD, sum), by=list(Month, input, loc)][input == "surface", list(Month, loc, value)]
  names(dt.temp) <- names(dt.sum)
  dt.sum <- rbind(dt.sum,dt.temp)
  rm(dt.temp)
  
  dt.temp <- dt.out[, lapply(.SD, sum), by=list(Month, input)][input == "storage", list(Month, loc, value)]
  names(dt.temp) <- names(dt.sum)
  dt.sum <- rbind(dt.sum,dt.temp)
  rm(dt.temp)

  dt.sum$cat <- gsub("confinement", "AU-confinement", dt.sum$cat)
  dt.sum$cat <- gsub("pasture", "AU-pasture", dt.sum$cat)
  dt.sum$cat <- gsub("forest", "AU-forest", dt.sum$cat)
  dt.sum$cat <- gsub("stream", "AU-stream", dt.sum$cat)
    
  return(dt.sum)
}