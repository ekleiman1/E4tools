.onLoad <- function(libname = find.package("E4tools"), pkgname = "E4tools"){

  # CRAN Note avoidance
  if(getRversion() >= "2.15.1")
    utils::globalVariables(c("ts_time","EDA_HighLowPass","E4_serial"))
}



