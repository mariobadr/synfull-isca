get.average.time <- function(names, time) {
  time$Application <- as.character(time$Application)
  myvars <- time$Application %in% names
  
  my.time <- time[myvars,]
  
  df <- ddply(my.time, .(Application), summarize, Time.Taken = mean(Time.Taken))
  
  return (df) 
}

average.time.all <- function(fsys, tdep, synfull, synfull2) {
  df <- data.frame(Application = fsys$Application, 
                   Trace.Dependency = fsys$Time.Taken / tdep$Time.Taken, 
                   SynFull = fsys$Time.Taken / synfull$Time.Taken, 
                   SynFull_SS = fsys$Time.Taken / synfull2$Time.Taken)
  
  melt.df <- melt(df, id="Application")
  colnames(melt.df) <- c("Application", "Simulation", "Speed.Up")
  
  return(melt.df)
}