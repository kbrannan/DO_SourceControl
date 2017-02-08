cow.calf <- function(chr.input.file) {
  
#   This function is the bacteria source-model cow-calf systems 
#   and generates input for HSPF. The specific outputs from 
#   this source model are loads from the cow-calf system to the land 
#   and directly to the stream. The load to the land is in the form 
#   of load/acre for each PLS in a sub-watershed that the source-model 
#   contributes to and the daily load to the stream. 
#   The input for the model is from an ASCII text file. For the input 
#   file. The symbol used for comments in the input file is "***". 
#   ":" is used to seperate parameter names or descriptions from values.
#   The function returns a data.frame with populations for different 
#   locations in the sub-watershed along with the associated bacteria loads.


  ## read input file
  df.input <- read.delim(chr.input.file, 
                         sep=":", comment.char="*", stringsAsFactors=FALSE, 
                         header=FALSE)
  names(df.input) <- c("parameter","value")

  
## set values for variables

## land use information
  ## pasture area
  lu.pasture.area  <- as.numeric(df.input$value[
    df.input$parameter == "Pasture Area in Watershed (ac)"])
  ## forest area
  lu.forest.area  <- as.numeric(df.input$value[
    df.input$parameter == "Forest Area in Watershed (ac)"])
  ## % pasture with stream access
  lu.pasture.w    <- as.numeric(df.input$value[
    df.input$parameter == "Percent of pasture with stream access"]) / 100
  ## % forest with stream access
  lu.forest.w     <- as.numeric(df.input$value[
    df.input$parameter == "Percent of forest with stream access"]) / 100
  
## animal management information
  ## cow-calf pair stocking density ac/pair
  amng.sd <- 
    as.numeric(df.input$value[
      df.input$parameter == 
        "Average Stocking Density for Pasture in watershed (ac/Cow-Calf pair)"]) 
  ## adjustment in cow size to account for calf growth in cow-calf pair
  amng.adj.size   <- as.numeric(strsplit(df.input$value[
    df.input$parameter == "Adjusted animal size"],",")[[1]]) 
  ## stocking schedule for pasture
  amng.in.pasture <- as.numeric(strsplit(df.input$value[
    df.input$parameter == "Pasture"],",")[[1]])
  ## stocking schedule for confinement
  amng.in.confine <- as.numeric(strsplit(df.input$value[
    df.input$parameter == "Confinement"],",")[[1]])
  ## stocking schedule for forest
  amng.in.forest  <- as.numeric(strsplit(df.input$value[
    df.input$parameter == "Forest"],",")[[1]])
  
## animal information
  ## bacteria production by cow #/(day-cow)
  ainfo.bac.prod <- as.numeric(
    df.input$value[df.input$parameter == 
                     "Fecal Coliform production by animal (org/(day-animal))"])
  ## SQOLIM factor > 1
  ainfo.sqolim.fac <- as.numeric(
    df.input$value[df.input$parameter == 
                     "SQOLIM multiplcation factor"])
  ## % of time cow-calf pair are in our around stream when on pasture
  ainfo.pasture.in.strm <- as.numeric(
    df.input$value[
      df.input$parameter == 
        "Percent of animals on pasture in and around streams"]) / 100
  ## % of time cow-calf pair are in our around stream when in forest
  ainfo.forest.in.strm  <- as.numeric(df.input$value[
    df.input$parameter == 
      "Percent of animals on forest in and around streams"]) / 100
  
## calculations
  ## pairs
  am.pairs     <- lu.pasture.area / amng.sd
  am.pairs.adj <- am.pairs * amng.adj.size
  
  ## pair location
  loc.pasture <- amng.in.pasture * am.pairs.adj
  loc.confine <- amng.in.confine * am.pairs.adj
  loc.forest  <- amng.in.forest * am.pairs.adj

  ## pair location with or without stream
  loc.pasture.w  <- lu.pasture.w * loc.pasture
  loc.pasture.wo <- (1 - lu.pasture.w) * loc.pasture
  loc.forest.w   <- lu.forest.w * loc.forest
  loc.forest.wo  <- (1 - lu.forest.w) * loc.forest

  ## pair location in stream or not 
  loc.pasture.w.strm <- loc.pasture.w * ainfo.pasture.in.strm
  loc.pasture.w.lnd  <- loc.pasture.w * (1 - ainfo.pasture.in.strm)
  loc.forest.w.strm   <- loc.forest.w * ainfo.forest.in.strm
  loc.forest.w.lnd    <- loc.forest.w * (1 - ainfo.forest.in.strm)

  ##
  ## bacteria load calculations
  bac.total.adj   <- am.pairs.adj * ainfo.bac.prod
  bac.total <- sum(bac.total.adj)
  bac.confine <- loc.confine * ainfo.bac.prod
  bac.pasture <- loc.pasture * ainfo.bac.prod
  bac.forest  <- loc.forest * ainfo.bac.prod
  bac.pasture.w.lnd  <- loc.pasture.w.lnd * ainfo.bac.prod
  bac.pasture.wo <- loc.pasture.wo * ainfo.bac.prod
  bac.pasture.w.strm  <- loc.pasture.w.strm * ainfo.bac.prod
  bac.forest.w.lnd   <- loc.forest.w.lnd * ainfo.bac.prod
  bac.forest.wo  <- loc.forest.wo * ainfo.bac.prod
  bac.forest.w.strm  <- loc.forest.w.strm * ainfo.bac.prod
  
  ##
  ## bacteria loads to end points (besides confinement)
  bac.pasture.lnd <- bac.pasture.wo + bac.pasture.w.lnd
  bac.forest.lnd  <- bac.forest.wo + bac.forest.w.lnd
  bac.strm <- bac.pasture.w.strm + bac.forest.w.strm

  ##
  ## SubModelOutput => df.output  
  df.output <- data.frame(
    Month = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep",
            "Oct","Nov","Dec"),
    NumOfPairs = am.pairs * rep(1,12),
    AUvsTime = am.pairs.adj,
    pairs.OnPastureWOStreamAccess = loc.pasture.wo,
    pairs.OnPastureWStreamAccess = loc.pasture.w.lnd,
    pairs.OnPastureInStream = loc.pasture.w.strm,
    pairs.InConfinementvsTime = loc.confine,
    pairs.InForestWOStreamAccess = loc.forest.wo,
    pairs.InForestWStreamAccess = loc.forest.w.lnd,
    pairs.InForestInStream = loc.forest.w.strm,

    Bacteria.OnPastureWOStreamAccess = bac.pasture.wo,
    Bacteria.OnPastureWStreamAccess = bac.pasture.w.lnd,
    Bacteria.OnPastureInStream = bac.pasture.w.strm,
    Bacteria.InConfinementvsTime = bac.confine,
    Bacteria.InForest = bac.forest.lnd,
    Bacteria.InForestInStream = bac.forest.w.strm,

    Bacteria.Instream = bac.strm,
    Accum.Pasture = bac.pasture.lnd / lu.pasture.area,
    Accum.Forest = bac.forest.lnd / lu.forest.area,
    
    Lim.Pasture = ainfo.sqolim.fac * bac.pasture.lnd / lu.pasture.area,
    Lim.Forest = ainfo.sqolim.fac * bac.forest.lnd / lu.forest.area,
    stringsAsFactors = FALSE)
    

  
  return(df.output)

}