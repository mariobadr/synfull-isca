source("clustering.R")

library(expm)

#generate.micro.model(trace_sub, "L", "Node.Injection", 0, 500000, 200, caches, directories, 0)

generate.micro.model <- function(data, #Data to be modelled
                                 criterion, #Method for how many clusters to use
                                 vector.type, #Type of vector to construct
                                 start.cycle,
                                 end.cycle,
                                 resolution, #Resolution
                                 caches, 
                                 directories,
                                 fileConn,
                                 filename,
                                 pdfOut
                                 ) {
  if(nrow(data) == 0 || ncol(data) == 0) {
    return()
  }
  
  if(resolution >= (end.cycle - start.cycle)) {
    stop("Resolution is larger than or equal to requested interval.")
  }

  trace <- data.frame(data, micro.bin = cut(data[,2], seq(start.cycle, 
                                                          end.cycle, 
                                                          resolution), labels=F))
  num.bins <- ceiling((end.cycle - start.cycle) / resolution)
  bins.df <- data.frame(micro.bin = c(1:(num.bins)))
  
  trace$micro.bin <- factor(trace$micro.bin, levels=bins.df$micro.bin) 
    
  # Get all initiating messages (i.e. messages from cores)
  init <- subset(trace, V3 %in% caches & V4 %in% directories & V6 == 1)
  row.names(init) <- NULL
  # Get all request messages (i.e. read, write, cache replacement, invalidate)
  req <- subset(trace, V6 == 1 & (V3 %in% caches & V4 %in% directories) 
                | (V7 == 4 & V3 %in% directories & V4 %in% caches))
  row.names(req) <- NULL
  # Get all reactive messages
  react <- trace[!trace$V1 %in% init$V1,]
  row.names(react) <- NULL
  
  clusters <- Do.Hclust(req, vector.type, bins.df)
    
  print("Determining number of clusters")
  #Determine number of clusters
  if(criterion == "L") {
    num.clusters <- L.Method(clusters)
  } else {
    stop("Unknown method for number of clusters at micro level")
  }
  print(num.clusters)
  
  class <- cutree(clusters, num.clusters)
  
  print("Setting up Markov")
  #Setup Markov Model
  markov <- markov.trans.1(class)
  steady.markov <- markov %^% 50
    
  class <- factor(class, levels=c(1:num.clusters))
  classified.bins <- data.frame(class, micro.bin = c(1:length(class)))
  init <- merge(init, classified.bins, by.x="micro.bin", by.y="micro.bin", sort=T)
  react <- merge(react, classified.bins, by="micro.bin", sort=T)
  
  #Spatial Injection Distribution
  wspat <- get.spatial.injection.by.class(subset(init, V6 == 1 & V7 == 0), caches)
  rspat <- get.spatial.injection.by.class(subset(init, V6 == 1 & V7 == 1), caches)
  cspat <- get.spatial.injection.by.class(subset(init, V6 == 1 & V7 == 2), caches)
  dspat <- get.spatial.injection.by.class(subset(init, V6 == 1 & V7 == 3), caches)
  
  print("Calculating flows")
  #Get flows for each message type
  wflows <- get.flows.by.class(subset(init, V6 == 1 & V7 == 0), caches, directories)
  rflows <- get.flows.by.class(subset(init, V6 == 1 & V7 == 1), caches, directories)
  cflows <- get.flows.by.class(subset(init, V6 == 1 & V7 == 2), caches, directories)
  dflows <- get.flows.by.class(subset(init, V6 == 1 & V7 == 3), caches, directories)
  #Invalidate and forwarded requests go from directories to caches
  iflows <- get.flows.by.class(subset(react, V6 == 1 & V7 == 4), directories, caches)
  fflows <- get.flows.by.class(subset(react, V6 == 1 & (V7 == 0 | V7 == 1)), directories, caches)
  
  #levelplot(xtabs(~V3+V4, trace), col.regions=gray(0:100/100))
  
  print("Calculating Volume Distributions")
  #Get volume distributions for each message type
  bin.comps <- get.bin.composition(init, classified.bins)
  volumes <- multi.xtabs(bin.comps, 
                         c("Write_Requests", 
                           "Read_Requests",
                           "Clean_CR",
                           "Dirty_CR"), 
                         "class"
                         )
    
  print("Calculating Forward Probabilities")
  #Get forward probabilities for reads/writes
  forward.prob.w <- get.fwd.prob(init, react, 0)
  forward.prob.r <- get.fwd.prob(init, react, 1)
  forward.prob <- merge(forward.prob.w, forward.prob.r, by="directory")
  
  print("Calculating Invalidate Volumes per directory")
  #Get invalidate probabilities for each directory
  inv.prob <- get.inv.vol(react, 15)
  
  print("Printing micro model to file")
  #fileConn <- file(file.name, "at")
  #on.exit(close(fileConn))
  
  # Write the header to the file
  c(paste("MEMORY", 1),
    paste("NUM_NODES", 32),
    paste("NUM_CLASSES", num.clusters),
    paste("RESOLUTION", resolution)
  ) -> header  
  writeLines(header, fileConn, sep="\n")
  
  cat("MARKOV\n", file=fileConn)
  write.table(markov, col.names = F, row.names = F, quote = F, file=fileConn)
  cat("END\n", file=fileConn)
    
  cat("MARKOV_STEADY\n", file=fileConn)
  write.table(steady.markov[1,], col.names = F, row.names = F, quote = F, file=fileConn)
  cat("END\n", file=fileConn)
  
  cat("WRITE_SPATIAL\n", file=fileConn)
  write.table(wspat, col.names = F, row.names = F, quote = F, file=fileConn)
  cat("END\n", file=fileConn)
  
  cat("READ_SPATIAL\n", file=fileConn)
  write.table(rspat, col.names = F, row.names = F, quote = F, file=fileConn)
  cat("END\n", file=fileConn)
  
  cat("CCR_SPATIAL\n", file=fileConn)
  write.table(cspat, col.names = F, row.names = F, quote = F, file=fileConn)
  cat("END\n", file=fileConn)
  
  cat("DCR_SPATIAL\n", file=fileConn)
  write.table(dspat, col.names = F, row.names = F, quote = F, file=fileConn)
  cat("END\n", file=fileConn)
  
  cat("WRITE_FLOWS\n", file=fileConn)
  write.table(wflows, col.names = F, row.names = F, quote = F, file=fileConn)
  cat("END\n", file=fileConn)
  
  cat("READ_FLOWS\n", file=fileConn)
  write.table(rflows, col.names = F, row.names = F, quote = F, file=fileConn)
  cat("END\n", file=fileConn)
  
  cat("CCR_FLOWS\n", file=fileConn)
  write.table(cflows, col.names = F, row.names = F, quote = F, file=fileConn)
  cat("END\n", file=fileConn)
  
  cat("DCR_FLOWS\n", file=fileConn)
  write.table(dflows, col.names = F, row.names = F, quote = F, file=fileConn)
  cat("END\n", file=fileConn)
  
  cat("WRITE_INJECTION\n", file=fileConn)
  write.table(volumes$Write_Requests, col.names = F, row.names = F, quote = F, file=fileConn)
  cat("END\n", file=fileConn)
  
  cat("READ_INJECTION\n", file=fileConn)
  write.table(volumes$Read_Requests, col.names = F, row.names = F, quote = F, file=fileConn)
  cat("END\n", file=fileConn)

  cat("CCR_INJECTION\n", file=fileConn)
  write.table(volumes$Clean_CR, col.names = F, row.names = F, quote = F, file=fileConn)
  cat("END\n", file=fileConn)

  cat("DCR_INJECTION\n", file=fileConn)
  write.table(volumes$Dirty_CR, col.names = F, row.names = F, quote = F, file=fileConn)
  cat("END\n", file=fileConn)
  
  cat("FORWARD_PROBABILITY\n", file=fileConn)
  write.table(forward.prob, col.names = F, row.names = F, quote = F, file=fileConn)
  cat("END\n", file=fileConn)
  
  cat("FORWARD_FLOWS\n", file=fileConn)
  write.table(fflows, col.names = F, row.names = F, quote=F, file=fileConn)
  cat("END\n", file=fileConn)
  
  cat("INVALIDATE_PROBABILITY\n", file=fileConn)
  write.table(inv.prob, col.names = F, row.names = F, quote = F, file=fileConn)
  cat("END\n", file=fileConn)
  
  cat("INVALIDATE_FLOWS\n", file=fileConn)
  write.table(iflows, col.names = F, row.names = F, quote = F, file=fileConn)
  cat("END\n", file=fileConn)
  
  if(pdfOut) {
    require(ggplot2)
    
    pdf(onefile=TRUE, file=paste(file.name, ".pdf"))
    bwtheme <- standard.theme("pdf", color=FALSE)
    
    barplot(t(markov), main="Markov Chain Probabilities", xlab="State")
    barplot(steady.markov[1,], main="Steady State Markov Probabilities", xlab="State", ylim=c(0:1))
    
    if(nrow(subset(init, V7 == 0)) > 0) {
      print(levelplot(wflows, main="Write Request Flows", xlab="Source", ylab="Destination", par.settings=bwtheme))
    }
    if(nrow(subset(init, V7 == 1)) > 0) {
      print(levelplot(rflows, main="Read Request Flows", xlab="Source", ylab="Destination", par.settings=bwtheme))
    }
    if(nrow(subset(init, V7 == 2)) > 0) {
      print(levelplot(cflows, main="Clean Cache Replacement Flows", xlab="Source", ylab="Destination", par.settings=bwtheme))   
    }
    if(nrow(subset(init, V7 == 3)) > 0) {
      print(levelplot(dflows, main="Dirty Cache Replacement Flows", xlab="Source", ylab="Destination", par.settings=bwtheme))
    }
    
    print(ggplot(data=as.data.frame(volumes$Write_Requests), aes(x=Write_Requests, y=Freq)) + geom_bar(stat = "identity") + facet_wrap(~class) + xlab("Number of Writes") + ylab("Occurences"))
    print(ggplot(data=as.data.frame(volumes$Read_Requests), aes(x=Read_Requests, y=Freq)) + geom_bar(stat = "identity") + facet_wrap(~class) + xlab("Number of Reads") + ylab("Occurences"))
    print(ggplot(data=as.data.frame(volumes$Clean_CR), aes(x=Clean_CR, y=Freq)) + geom_bar(stat = "identity") + facet_wrap(~class) + xlab("Number of Clean Cache Replacements") + ylab("Occurences"))
    print(ggplot(data=as.data.frame(volumes$Dirty_CR), aes(x=Dirty_CR, y=Freq)) + geom_bar(stat = "identity") + facet_wrap(~class) + xlab("Number of Dirty Cache Replacements") + ylab("Occurences"))
    #print(ggplot(data=forward.prob, aes(x=directory, y=forward)) + geom_bar(stat="identity") + ylab("Probability to Forward a Request") + ylim(0,1))
    
    if(nrow(subset(react, V6 == 1 & (V7 == 0 | V7 == 1))) > 0) {
      print(levelplot(fflows, xlab="Source", ylab="Destination", main="Forwarded Request Flows", par.settings=bwtheme))
    }
    #print(ggplot(data=inv.prob, aes(x=Var1, y=Freq)) + geom_bar(stat="identity") + facet_wrap(~V3 + class) + xlab("Number of Invalidates") + ylab("Occurences"))
    
    if(nrow(subset(react, V6 == 1 & V7 == 4)) > 0) {
      print(levelplot(iflows, xlab="Source", ylab="Destination", main="Invalidate Flows", par.settings=bwtheme))
    }
    dev.off()
  }
  
#   ggplot(data=melt(fwd.fft, "Directory"), 
#   aes(x=Directory, y=value, fill=variable)) + 
  #geom_bar(stat="identity", position=position_dodge()) + 
#  ylab("Probability to Forward a Request") + 
#    ylim(0,1) + 
#    theme_bw() + 
#    scale_x_continuous(breaks=directories, labels=directories,limits=c(0,31)) + 
#    scale_y_continuous(limits=c(0,0.6)) + 
#    theme(axis.line = element_line(colour = "black"), 
#   panel.grid.major = element_blank(),
#   panel.grid.minor = element_blank(),
#   panel.border = element_blank(),
#   panel.background = element_blank()) 
}

get.spatial.injection.by.class <- function(dat, l) {
  dat$V3 <- factor(dat$V3, levels=l)
  
  tab <- xtabs(~V3+class, dat)
  
  return(tab)
}

get.flows <- function(dat, src, dest) {
  dat$V3 <- factor(dat$V3, levels=src)
  dat$V4 <- factor(dat$V4, levels=dest)
  
  tab <- (xtabs(~V3+V4, dat))
  
  return(tab)  
}

get.flows.by.class <- function(dat, src, dest) {
  dat$V3 <- factor(dat$V3, levels=src)
  dat$V4 <- factor(dat$V4, levels=dest)
  
  tab <- (xtabs(~V3+V4+class, dat))
  
  return(tab)
}

get.bin.composition <- function(dat, classified.bins) {
  require(plyr)
  
  dat$micro.bin <- as.numeric(dat$micro.bin)
  bins <- ddply(dat, .(micro.bin), summarize, 
                Write_Requests = sum(V6 == 1 & V7 == 0), 
                Read_Requests = sum(V6 == 1 & V7 == 1),                
                Clean_CR = sum(V6 == 1 & V7 == 2),
                Dirty_CR = sum(V6 == 1 & V7 == 3))
  
  bins <- merge(classified.bins, bins, by="micro.bin", all=T)
  bins[is.na(bins)] <- 0
  
  return(bins)
}

get.fwd.prob.rw <- function(init, react) {
  #Get forward probabilities for reads/writes
  forward.prob.w <- get.fwd.prob(init, react, 0)
  forward.prob.r <- get.fwd.prob(init, react, 1)
  forward.prob <- merge(forward.prob.w, forward.prob.r, by="directory")
  colnames(forward.prob) <- c("Diretory", "Writes", "Reads")
  
  return(forward.prob)
}

get.fwd.prob <- function (init, react, var) {
  require(plyr)
  
  req.to.dir <- count(subset(init, V7 == var), c("V4"))
  for.by.dir <- count(subset(react, V6 == 1 & V7 == var), c("V3"))
  
  forward.prob <- merge(for.by.dir, req.to.dir, by.x = c("V3"), by.y = c("V4"), all=T)
  forward.prob[is.na(forward.prob)] <- 0
    
  result <- data.frame(directory = forward.prob$V3, 
                       forward = forward.prob$freq.x / forward.prob$freq.y)
  
  return(result)
}

get.inv.vol <- function(react, max.inv) {
  require(plyr)
  # Get the number of invalidates per directory. Include forwarded requests to
  # track when 0 invalidates are sent.
  inv.per.dir <- ddply(subset(react, V6 == 1 & (V7 == 4 | V7 == 0)), 
                       .(class, V3, V2, V8), function(df)nrow(df))
  inv.per.dir$V1 <- inv.per.dir$V1 - 1 # subtract forwarded request
    
  df <- data.frame(Var1 = c(0:max.inv))
  inv.vol <- ddply(inv.per.dir, 
                   .(class, V3), 
                   function(data) merge(df, as.data.frame(table(data$V1)), 
                                        by="Var1", 
                                        all.x=T)
                   )
  inv.vol[is.na(inv.vol)] <- 0
  
  return(inv.vol)  
}

multi.xtabs <- function(df, vars, group.var, ...) {
  sapply(simplify = FALSE,
         vars,
         function(var) {
           formula <- as.formula(paste("~", var, "+", group.var))
           xtabs(data = df, formula, ...)
         }
  )
}