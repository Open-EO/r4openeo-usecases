message("global level udf script")

udf_setup = function(context) {
  if (!require("bfast")) {
    install.packages("bfast", quiet = TRUE)
  }
}

suppressWarnings(suppressMessages(library("bfast", quietly = T)))

udf_chunked = function(data, context) {
  pixels = unlist(data)
  dates = names(data)
  
  val = if(is.null(context) || is.null(context$val)) "breakpoint" else context$val
  level = if(is.null(context) || is.null(context$level) || length(context$val) == 0) c(0.001, 0.001) else context$level

  # error handling
  stopifnot(length(pixels) == length(dates)) 
  stopifnot(val %in% c("breakpoint", "magnitude"))

  # create ts object for bfast
  lsts = bfastts(pixels, dates, type = c("irregular"))
  
  # make sure there are enough observations
  if (sum(!is.na(lsts)) < 100){
    return(NA)
  }
    
  
  # run bfast
  res = tryCatch(
    {
      res = bfastmonitor(lsts, 
                         context$start_monitor, 
                         formula = response~harmon, 
                         order = 1, 
                         history = "all", 
                         level = level,
                         verbose = F)[[val]]
    }, 
    error = function(cond) {
      return(0)
    }
  )
  
  if(is.na(res)){
    return(0)
  }
  
  return(res) 
}

