getData <- function() {
  # Load the new CSV file into a data frame
  new_data <- read.csv("worldccnn.csv", stringsAsFactors = FALSE)
  
  # Convert the data frame to a matrix
  D <- as.matrix(new_data)
  
  # Save the data frame to a new CSV file without quotes
  write.csv(new_data, "world_noquotes.csv", quote = FALSE, row.names = FALSE)
  
  return(D)
}


tourLength <- function(tour, distMatrix) {
     tour <- c(tour, tour[1])             #e.g. convert A,B,C to A,B,C,A. Thus the tour finishes where it started.
     route <- embed(tour, 2)[,2:1]        #converts the tour into a matrix of trips. i.e. 
                                          # 
     tourlength <- sum(distMatrix[route]) #tour length must be minimised
     return(tourlength)                   #however, GA package only maximises. So 1/tourlength can be maximised. 
}

tspFitness <- function(tour, ...){       #... allows passing some unspecified arguments to the function, which can be passed on further. 
     return (1/tourLength(tour, ...))    #Since the tour length must be minimsed, 1/tourlength can be maximised. 
                                         #We convert it into a maximisation problem because the GA package can only maximise. 
}

plotTSPSolution<-function(solution){
  data <- read.csv("worldccnn.csv")
  mds <- cmdscale(data)
  x <- mds[, 1]
  y <- -mds[, 2]
  plot(x, y, type = "n", asp = 1, xlab = "", ylab = "")
  abline(h = pretty(range(x), 10), v = pretty(range(y), 10),
            col = "light gray")
  tour <- solution[1, ]
  tour <- c(tour, tour[1])
  n <- length(tour)
  arrows(x[tour[-n]], y[tour[-n]], x[tour[-1]], y[tour[-1]],
            length = 0.15, angle = 25, col = "black", lwd = 2)
  text(x, y, labels(new_data), cex=0.8)
}
#result <- runGA(problem = "tsp")
#bestsolution <- getBestSolution()
# plotTSPSolution(bestsolution)
