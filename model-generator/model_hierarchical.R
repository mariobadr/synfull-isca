source("clustering.R")
source("markov_chain.R")
source("model_micro.R")

library(fpc)
library(ff)
library(ffbase)
library(expm)

#Global variables
macro.cv <- data.frame() #Macro Clustering Vector
macro.cm <- "Total.Injection" #Macro Clustering Method
macro.res <- 500000

#generate.full.model("~/VirtualShared/fft.small.trace", "fft_asw.model", "asw", "Node.Injection", 500000, "L", "Node.Injection", 200, 32)

generate.full.model <- function(trace.file, #Full path and file name to trace file
                           model.file.name, #File name for output
                           macro.criterion, #Method to use for  number of clusters
                           macro.vector, #Clustering vector method at the macro scale
                           macro.resolution, #Resolution at the macro scale
                           micro.criterion, #Method to use for number of clusters
                           micro.vector, #Clustering vector method at the micro scale
                           micro.resolution, #Resolution at the micro scale
                           num.nodes #Number of nodes in the simulation
                           ) {
  options(scipen=999)
  
  nodes <- c(0:(num.nodes-1))
  assign("nodes", nodes, envir = .GlobalEnv)
  
  caches <- seq(0, 31, by=2)
  directories <- seq(1, 31, by=2)
  
  #Create empty clustering vector data frame
  if(macro.vector == "Total.Injection") {
    template.df <- data.frame(Total.Injection = numeric(0))
  } else if (macro.vector == "Node.Injection") {
    template.df <- as.data.frame(setNames(replicate(num.nodes,numeric(0), simplify = F), nodes))
  } else {
    stop("Unknown clustering method at macro level.")
  }
  assign("macro.cv", template.df, envir = .GlobalEnv)
  
  #Import the trace file and assign rows to bins
  print(paste("Importing trace:", trace.file))
  assign("macro.res", macro.resolution, envir = .GlobalEnv)
  trace <- read.table.ffdf(file=trace.file, transFUN=apply.bin)

  #Construct the clustering vector matrix according to each bin
  print("Creating vectors")
  assign("macro.cm", macro.vector, envir = .GlobalEnv)
  by(trace[,1:8], trace[,"bin"], append.macro.vector)
  
  print("Clustering Macro Level")
  clusters <- pamk(get("macro.cv", envir = .GlobalEnv), criterion=macro.criterion)
  
  #Setup File Output
  fileConn <- file(paste(model.file.name, sep=""), "w")
  on.exit(close(fileConn))
  
  # Write the header to the file
  print(paste("Modelling", clusters$nc, "clusters"))
  c(paste("HIER_CLASSES", clusters$nc), 
    paste("TIME_SPAN", macro.resolution)) -> header  
  writeLines(header, fileConn, sep="\n")
  
  markov <- markov.trans.1(clusters$pamobject$clustering)
  cat("HIER_MARKOV\n", file=fileConn)
  write.table(markov, col.names = F, row.names = F, file=fileConn)
  cat("END\n", file=fileConn)
  
  steady.markov <- markov %^% 500
  cat("HIER_MARKOV_STEADY\n", file=fileConn)
  write.table(steady.markov[1,], col.names = F, row.names = F, file=fileConn)
  cat("END\n", file=fileConn)
  
  #Output a model for each cluster
  num <- 1
  for(id in clusters$pamobject$id.med) {
    print(paste("Modelling cluster: ", num))
    cat(paste("HIER_BEGIN_ID ", num, "\n"), file=fileConn)
    
    #Create micro model
    start <- (id-1)*macro.resolution
    end <- (id)*macro.resolution
    test <- try(subset(trace, bin == id), silent = T)
    if(is.ffdf(test)) {
      test <- as.data.frame(test)
      generate.micro.model(test, 
                           micro.criterion, 
                           micro.vector, 
                           start, 
                           end, 
                           micro.resolution, 
                           caches,
                           directories, 
                           fileConn,
                           model.file.name,
                           F)
    } else {
      #use empty data frame
      print(0)
    }
    
    cat("END_HIER\n", file=fileConn)
    num <- num + 1
  }
}

apply.bin <- function(dat) {
  res <- get("macro.res", envir = .GlobalEnv)
  dat <- data.frame(dat, bin= floor(dat$V2 / res) + 1)
  
  return(dat)
}

append.macro.vector <- function(dat) {  
  cm <- get("macro.cm", envir = .GlobalEnv)
  
  #Get clustering vector from global scope
  cv <- get("macro.cv", envir = .GlobalEnv)
  
  #Add new vector depending on method
  if(cm == "Total.Injection") {
    cv[nrow(cv)+1,] <- create.ti.vector(dat)
  } else if (cm == "Node.Injection") {
    cv[nrow(cv)+1, ] <- create.ni.vector(dat)
  } else {
    stop("Unknown clustering method at macro level.")
  }
  
  #Update clustering vector at global scope
  assign("macro.cv", cv, envir = .GlobalEnv)
  
  return(dat)
}