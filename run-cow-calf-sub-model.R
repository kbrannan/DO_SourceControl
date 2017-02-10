
library(data.table, quietly = TRUE)
library(reshape2, quietly = TRUE)
library(gtable, quietly = TRUE)
library(gridExtra, quietly = TRUE)

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

## run cow.calf model for all sub watersheds
lst.tmp <- lapply(chr.files.input, cow.calf)
## convert data.frames from cow.calf model to data.tables
lst.out <- lapply(lst.tmp, data.table)
## clean up
rm(lst.tmp)

## asssemble output for report to Tetra Tech
lst.summary.dt.cowcalf <- vector('list', length(vec.subs))
## set names of the summary data.tables
names(lst.summary.dt.cowcalf) <- paste0("sub", sprintf(fmt = "%02i", vec.subs))
for(ii in 1:length(vec.subs)) {
  dt.cur <- lst.out[[ii]]
  chr.drop.cols <- names(dt.cur)[(grep("(Bacteria)|(Accum\\.)|(Lim\\.)", names(dt.cur)))]
  dt.in <- dt.cur[ , !chr.drop.cols, with=FALSE]
  lst.summary.dt.cowcalf[[ii]] <- cbind(sub = ii, summarize.out(dt.in))
  rm(dt.cur, chr.drop.cols, dt.in)
}

dt.summary <- do.call(rbind, lst.summary.dt.cowcalf)

dt.sum <- dt.summary
dt.sum$cat <- gsub("^confinement", "AU-confinement", dt.sum$cat)
dt.sum$cat <- gsub("^pasture", "AU-pasture", dt.sum$cat)
dt.sum$cat <- gsub("^forest", "AU-forest", dt.sum$cat)
dt.sum$cat <- gsub("^stream", "AU-stream", dt.sum$cat)

dt.sum$cat <- factor(dt.sum$cat, 
                     levels = c("AU-pasture", "AU-forest","AU-stream",
                                "AU-confinement", "total-AU", "total-pairs"))

dt.summary <- dt.sum
rm(dt.sum)


write.csv(dt.summary, 
          file = paste0(chr.dir.sub.models.cow.calf, "/summary-out-cow-calf.csv"),
          row.names = FALSE)



maxrow = 12; 
int.n.rows <- nrow(dt.summary)
npages = ceiling(int.n.rows/maxrow)

pdf(file = paste0(chr.dir.sub.models.cow.calf, "/summary-out-cow-calf.pdf"),
    height = 8.5, width = 11, onefile = TRUE)


for (i in 1:npages) {
  idx = seq(1+((i-1)*maxrow), i*maxrow)
  idx <- idx[idx <= int.n.rows]
  grid.newpage()
  
  tmp.table <- tableGrob(
    dt.summary[idx, ], show.rownames = FALSE,
    gpar.coretext =gpar(fontsize=10),
    gpar.coltext=gpar(fontsize=10, fontface='bold'),
    y = unit(0.1, "npc"),
    vjust = 2
  )
  tmp.h <- grobHeight(tmp.table)
  tmp.w <- grobWidth(tmp.table)
  if(i == 1) {
    tmp.label <- "cow-calf AU across locations by sub-watershed and month"  
  } else {
    tmp.label <- "cow-calf AU across locations by sub-watershed and month (continued)"
  }
  tmp.title <- textGrob(label = tmp.label,
                        y=unit(0.5,"npc") + 0.5*tmp.h, 
                        vjust=0, gp=gpar(fontsize=20))
  tmp.gt <- gTree(children=gList(tmp.table, tmp.title)) 
  grid.draw(tmp.gt)
}

dev.off()

