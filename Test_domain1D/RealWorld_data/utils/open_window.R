open_window <- function() {
  os <- Sys.info()["sysname"]
  
  if (os == "Windows") {
    windows()
  } else if (os == "Darwin") { 
    quartz()
  } else { 
    x11()
  }
}
