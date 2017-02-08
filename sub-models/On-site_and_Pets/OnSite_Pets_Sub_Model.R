onsite_pets <- function(chr.input.file) {
  #   This function is the bacteria source-model on-site spetic systems 
  #   and pets. The modelgenerates input for input to HSPF. The specific 
  #   outputs from this source model are loads from the on-site systems 
  #   to land and directly to the stream, along with loads from pets to 
  #   land. The load to the land is in the form of load/acre for each 
  #   PLS in a sub-watershed that the source-model contributes to and 
  #   the hourly load to the stream in the form of a MUTSIN file. The 
  #   input for the model is from an ASCII text file. Use the text below
  #   as a template for the input file. The symbol used for comments in
  #   the input file is "***". The definitions for the symbols used in
  #   the template are: YYYY is four-digit year, MM two-digit month, 
  #   DD is the two-digit day, ## is an integer, #.# is a floating point 
  #   number, and #.#E+## is a number in scientific notation
  
  
## read input file
  df.input <- read.delim(chr.input.file, sep=":", comment.char="*",
                         stringsAsFactors=FALSE, header=FALSE)
  names(df.input) <- c("parameter","value(s)")
  
## set values for variables
  
## land use information
  ## developed area
  lu.RAOCUT.area  <- as.numeric(df.input$value[
    df.input$parameter == 
      "Residential/Agricultural Operration Area/Commercial/Urban/Transportation (ac)"])

## bacteria production rates
  onsite.bac.prod  <- as.numeric(df.input$value[
    df.input$parameter == "On-site systems (orgs/system-day)"])
  pets.bac.prod  <- as.numeric(df.input$value[
    df.input$parameter == "Pet (orgs/pet-day)"])
  all.SQLIMFactor  <- as.numeric(df.input$value[
    df.input$parameter == "SQOLIM multiplcation factor"])

## pet information
  pets.NumOfHH  <- as.numeric(df.input$value[
    df.input$parameter == "Number of House-Holds"])
  pets.PetsPerHH  <- as.numeric(df.input$value[
    df.input$parameter == "Pets per House-Hold"])

## On-site and structure Information
  onsite.NumNearStrmStrct  <- as.numeric(df.input$value[
    df.input$parameter == "Number of near-stream structures"])
  onsite.StrctPre1974      <- as.numeric(df.input$value[
    df.input$parameter == "Structures for house age pre-1974  (%)"])/100
  onsite.Strct1974to1986   <- as.numeric(df.input$value[
    df.input$parameter == "Structures for house age 1974-1986 (%)"])/100
  onsite.StrctPost1986     <- as.numeric(df.input$value[
    df.input$parameter == "Structures for house age post-1986 (%)"])/100
  onsite.FailRatePre1974      <- as.numeric(df.input$value[
    df.input$parameter == "Failure rate for house age pre-1974  (%)"])/100
  onsite.FailRate1974to1986   <- as.numeric(df.input$value[
    df.input$parameter == "Failure rate for house age 1974-1986 (%)"])/100
  onsite.FailRatePost1986     <- as.numeric(df.input$value[
    df.input$parameter == "Failure rate for house age post-1986 (%)"])/100
  onsite.percent.to.stream     <- as.numeric(df.input$value[
    df.input$parameter == "On-site Failure directly to stream (%)"])/100

### Calculations
  ### Pets
  pets.pop <- pets.NumOfHH * pets.PetsPerHH
  pets.bacteria.load <- pets.pop * pets.bac.prod
  Accum.RAOCUT <- pets.bacteria.load / lu.RAOCUT.area

  ### On-stie
  onsite.NearStrmStrctPre1974    <- onsite.NumNearStrmStrct * onsite.StrctPre1974
  onsite.NearStrmStrct1974to1986 <- onsite.NumNearStrmStrct * onsite.Strct1974to1986
  onsite.NearStrmStrctPost1986   <- onsite.NumNearStrmStrct * onsite.StrctPost1986
  onsite.NearStrmStrct <- onsite.NumNearStrmStrct
  onsite.NearStrmStrctFailurePre1974    <- onsite.NearStrmStrctPre1974 * onsite.FailRatePre1974
  onsite.NearStrmStrctFailure1974to1986 <- onsite.NearStrmStrct1974to1986 * onsite.FailRate1974to1986
  onsite.NearStrmStrctFailurePost1986   <- onsite.NearStrmStrctPost1986 * onsite.FailRatePost1986
  onsite.NearStrmStrctFailure <- onsite.NearStrmStrctFailurePre1974 + onsite.NearStrmStrctFailure1974to1986 + onsite.NearStrmStrctFailurePost1986
  ## calculate bacteria loads
  onsite.NearStrmStrctFailureInStream <- onsite.percent.to.stream * onsite.NearStrmStrctFailure
  onsite.NearStrmStrctFailurePre1974.load    <- onsite.NearStrmStrctFailurePre1974 * onsite.bac.prod 
  onsite.NearStrmStrctFailure1974to1986.load <- onsite.NearStrmStrctFailure1974to1986 * onsite.bac.prod 
  onsite.NearStrmStrctFailurePost1986.load   <- onsite.NearStrmStrctFailurePost1986 * onsite.bac.prod
  onsite.NearStrmStrctFailure.load <- onsite.NearStrmStrctFailurePre1974.load + onsite.NearStrmStrctFailure1974to1986.load + onsite.NearStrmStrctFailurePost1986.load
  ## adjust load for structures near stream that may not have toilet facilities
  onsite.NearStrmStrctFailure.to.stream.load <- onsite.percent.to.stream * onsite.NearStrmStrctFailure.load
  Accum.RAOCUT <- Accum.RAOCUT + (1- onsite.percent.to.stream) * onsite.NearStrmStrctFailure.load / lu.RAOCUT.area
  ##
  ### Assemble output data frame
  df.output <- data.frame(Month=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                               pop.pet.total=pets.pop,
					              		   num.onsite.NearStrmStrctPre1974=onsite.NearStrmStrctPre1974,
					              		   num.onsite.NearStrmStrct1974to1986=onsite.NearStrmStrct1974to1986,
				              			   num.onsite.NearStrmStrctPost1986=onsite.NearStrmStrctPost1986,
					              		   num.onsite.NearStrmStrct=onsite.NearStrmStrct,
							                 num.onsite.NearStrmStrctFailurePre1974=onsite.NearStrmStrctFailurePre1974,
							                 num.onsite.NearStrmStrctFailure1974to1986=onsite.NearStrmStrctFailure1974to1986,
						              	   num.onsite.NearStrmStrctFailurePost1986=onsite.NearStrmStrctFailurePost1986, 
							                 num.onsite.NearStrmStrctFailure=onsite.NearStrmStrctFailure,
					              		   num.onsite.NearStrmStrctFailureInStream=onsite.NearStrmStrctFailureInStream,
							                 Bacteria.pets.load=pets.bacteria.load,
					              		   Bacteria.onsite.NearStrmStrctFailurePre1974=onsite.NearStrmStrctFailurePre1974.load,
					              		   Bacteria.onsite.NearStrmStrctFailure1974to1986=onsite.NearStrmStrctFailure1974to1986.load,
					              		   Bacteria.onsite.NearStrmStrctFailurePost1986=onsite.NearStrmStrctFailurePost1986.load,
					              		   Bacteria.onsite.NearStrmStrctFailure=onsite.NearStrmStrctFailure.load,
					              		   Bacteria.onsite.NearStrmStrctFailure.to.stream.load=onsite.NearStrmStrctFailure.to.stream.load,
                               Accum.RAOCUT=Accum.RAOCUT,
					              		   Lim.RAOCUT=all.SQLIMFactor * Accum.RAOCUT,
                               stringsAsFactors=FALSE)
  ##
  ### return results
  return(df.output)
}