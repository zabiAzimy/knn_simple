# loading the required library
library(FNN)


# data generation
Time <- 0:23 
Temp <- c(12.2, 11.9, 10.9, 10.1, 9.8, 9.2, 8.9, 9.3, 11.0, 12.9, 15.0, 17.5,
          19.6, 20.5, 21.4, 21.8, 22.6, 23.1, 22.9, 21.7, 19.4, 17.9, 16.8,
          15.2) 

Xgrid <- data.frame(Time = seq(0,23.9, by = 0.05))

Xgrid

# performing knn for regression 
# with k = 1
knn_out_1 <- knn.reg(Time, test = Xgrid, y = Temp, k = 1)

# with k = 5
knn_out_2 <- knn.reg(Time, test = Xgrid, y = Temp, k = 5)

# visualizing the model 
plot(Time, Temp, main = "1 nearest neighbor")
lines(Xgrid[, 1], knn_out_1$pred, col = 2)

# let's make the visualization for the knn with k = 5
plot(Time, Temp, main = "5 Nearest Neighbors")
lines(Xgrid[, 1], knn_out_2$pred, col = 3, lwd = 2)
