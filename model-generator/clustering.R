Do.Hclust <- function(dat, vector.type, bins.df) {
  # Construct the feature vectors
  print("Constructing feature vectors")
  if(vector.type == "Total.Injection") {
    l <- by(dat[,1:8], dat[,"micro.bin"], create.ti.vector, simplify=F)
  } else if(vector.type == "Flow.Injection") {
    l <- by(dat[,1:8], dat[,"micro.bin"], create.f.vector, simplify=F)
  } else if (vector.type == "Node.Injection") {
    l <- by(dat[,1:8], dat[,"micro.bin"], create.ni.vector, simplify=F)
  } else if (vector.type == "NFlow.Injection") {
    l <- by(dat[,1:8], dat[,"micro.bin"], create.nf.vector, simplify=F)
  } else {
    stop("Unknown clustering method")
  }
  c.vectors <- do.call(rbind.data.frame, l)
    
  #Needs Optimization!!!
  print("Add vectors for missing bins")
  c.vectors <- merge(bins.df, c.vectors, by.y="row.names", by.x="micro.bin", all=T, sort=T)
  c.vectors[is.na(c.vectors)] <- 0
  c.vectors[,1] <- NULL
  
  print("Clustering vectors")
  # Calculate dissimilarity matrix
  d <- dist(c.vectors, method="euclidean")
  d <- d^2
  
  #Perform clustering
  clusters <- hclust(d, method="ward")
  
  return(clusters)
}

create.ti.vector <- function(dat) {
  return(nrow(dat))
}

create.ni.vector <- function(dat) {
  n <- get("nodes", envir = .GlobalEnv)
  f <- factor(dat$V3, levels=n)
  df <- as.data.frame(table(f), responseName="Freq", row.names=NULL)
    
  return (df$Freq)
}

create.f.vector <- function(dat) {
  n <- get("nodes", envir = .GlobalEnv)
  
  dat$V3 <- factor(dat$V3, levels=n)
  dat$V4 <- factor(dat$V4, levels=n)
  
  df <- as.data.frame(xtabs(~V3+V4, dat), responseName="Freq", row.names=NULL)
  
  return (df$Freq)
}

create.nf.vector <- function(dat) {
  n <- get("nodes", envir = .GlobalEnv)
  dimension <- 8
  
  row <- factor(floor(dat$V3 / 4), levels=0:3)
  col <- factor(dat$V4 %% 8, levels =0:7)
  
  df <- data.frame(dat, row, col)
  df <- as.data.frame(xtabs(~row+col, df), responseName="Freq", row.names=NULL)
  
  return(df$Freq)
}

L.Method <- function(clusters) {  
  eval <- cluster.merge.cost(clusters)
  
  b <- max(eval$num_clusters)
  c <- 3:(b-2)
  
  results <- NULL
  for(i in c) {
    L <- subset(eval, num_clusters <= i)
    R <- subset(eval, num_clusters > i)
    
    L.rmse <- sqrt(mean(residuals(lm(L[,2] ~ L[,1]))^2))
    R.rmse <- sqrt(mean(residuals(lm(R[,2] ~ R[,1]))^2))
    
    rmse <- ((i-1)/(b-1))*(L.rmse) + ((b-i)/(b-1))*(R.rmse)
    results <- c(results, rmse)
  }
  
  results <- data.frame(n = c, rmse = results)
  return(results[which(results$rmse == min(results$rmse)), 1])
}

# From: http://alumni.media.mit.edu/~tpminka/courses/36-350.2001/code/clus1.r
# Here, merging cost results from the sum of squares (a.k.a minimum variance),
# at least for ward's algorithm.
cluster.merge.cost <- function(h, k=1:30) {
  # height is the "value of the criterion associated with the clustering method"
  g <- rev(h$height)
  k <- k[k < length(g)]
  
  if(h$method != "single") {
    g <- -diff(g)
  }
  
  return(data.frame(num_clusters = k + 1, merge_cost = g[k]))
}
