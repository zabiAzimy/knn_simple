## Mean squared error and cross validation
# We have generated a sample data here
fitdata <- as.data.frame( matrix(c(1,87,42, 6,73,43, 7,66,44, 15,62,54, 12,68,45,
                                   4,92,46, 12,60,50, 13,70,46, 14,71,54, 10,64,
                                   47), byrow=TRUE,ncol=3))


# Using the above simulated data, perfrom a knn learning model
# given names to the data frame columns
names(fitdata) <- c("fitness","weight","lungvol") 

# view the small simulated dataset
fitdata

# let's now fit a KNN model 
X <- fitdata[, c("weight", "lungvol")]
# X contains the training data to which we fit the knn model
X
# Look at the arguments given to the knn.reg 
knnr_out_1 <- knn.reg(X, y = fitdata$fitness, k = 1, test = X)

# let's look at the predictions of knnr_out_1
knnr_out_1$pred

# look at the training data 
fitdata$fitness

# Now let's calculate the mean squared error
mean(fitdata$fitness - knnr_out_1$pred) ^ 2

# looking at the above output, looks like we have done a great job
# but let's not jump to conclusion this quickly
# Choosing the model only based on the lowest MSE is not a good idea


# Cross validation comes into action now
# we are again working the same 1 dimensional data from section 1
# performing knn with one explanatory variable
x <- 1:20
y <- x + rnorm(length(x), mean = 10)

# the explanatory variable should be stored in a data frame
Xgrid <- data.frame(X)
Xgrid

# Compare the output of knn.reg with and without the test argument
knnr_out_a_1 <- knn.reg(x, y = y, k = 1)  # Without test argument

knnr_out_b_1 <- knn.reg(x, y = y, test = Xgrid, k =1)  # with test argument

# Combining the predictions of both the above models
round(cbind(x, y, 
            pred_wo_test = knnr_out_a_1$pred,
            pred_w_test = knnr_out_b_1$pred), digits = 2)

# When the test argument is not specified, The algorithm excludes the current 
# value of x when determining the k nearest neighbors to x

# for different values of K, we now compute MSE (Mean squared Error)
k_vec <- 1:10
# We are considering the following values for the K
k_vec

# we define a vector containing NA which will be replaced with MSE values
mse_vec <- rep(NA, length(k_vec))

# Run a loop which will compute MSE for different values of k
for (k in k_vec) {
  knnr_out_k <- knn.reg(x, y = y, k = k)
  mse_vec[k] <- mean((y - knnr_out_k$pred) ^ 2)
}

# plotting the MSE values against the K values
plot(k_vec, mse_vec)

# looking at the above plot, we see that K as 2 and 4 gives the lowest MSE 

# let's now do this for the fitness data
k_vec <- 1 : (nrow(fitdata) - 1)
mse_vec <- rep(NA, length(k_vec))
for(k in k_vec){ 
  knnr_out_k <- knn.reg(X, y = fitdata$fitness, k = k) 
  mse_vec[k] <- mean((fitdata$fitness- knnr_out_k$pred) ^ 2) 
  }

# plot the k values against the mse computed
# and looking at the plot, we understand that 2 is the best value for K
plot(k_vec, mse_vec)












# Simulated data
# Simulated data
fitdata <- as.data.frame( matrix(c(1,87,42, 6,73,43, 7,66,44, 15,62,54, 12,68,45,
                                   4,92,46, 12,60,50, 13,70,46, 14,71,54, 10,64,
                                   47), byrow=TRUE,ncol=3))

# Assign column names
names(fitdata) <- c("fitness","weight","lungvol") 

# Explanatory variables
X <- fitdata[, c("weight", "lungvol")]

# Define the grid for predictions
m1 <- seq(55, 95, length=20) # weight taking values between 55-95
m2 <- seq(40, 55, length=20) # lung volume takes values between 40-55

# Create a grid for the input variables
Xgrid <- expand.grid(weight = m1, lungvol = m2)

# Fit the KNN model and make predictions on the grid
library(FNN)
knnr_out_2 <- knn.reg(X, y = fitdata$fitness, k = 2, test = Xgrid)
preds <- matrix(knnr_out_2$pred, nrow = length(m1), ncol = length(m2))

# Plot with better angles and color
res <- persp(m1, m2, preds, border = grey(0.6),
             xlab = "Weight", ylab = "Lungvol", zlab = "Fitness",
             theta = 30, phi = 30, col = "lightblue")



tt<-cbind(Xgrid, preds) 

with(tt, plot3d(lungvol, weight, knnr_out_2$pred, col = "grey80", size = 3,
                zlab = "fitness")) 
with(fitdata, plot3d(lungvol, weight, fitness, add = TRUE, size = 2, col = "red", type = "s"))





