# load the required library

library(FNN)

# this library is used for 3d visualization
library(rgl)

# performing knn with one explanatory variable
x <- 1:20
y <- x + rnorm(length(x), mean = 10)

# the explanatory variable should be stored in a data frame
Xgrid <- data.frame(x)

# The function which is part of FNN library for knn is called knn.reg
# so let's use it 
knnr_out <- knn.reg(x, y = y, k = 1, test = Xgrid)
round(cbind(x, y, fitted_1 = knnr_out$pred), digits = 2)


# Now we are performing knn with k = 3
knnr_out_1 <- knn.reg(x, y, k =3, test = Xgrid)
round(cbind(x, y, fitted_1 = knnr_out_1$pred), digits = 2)

# let's plot the model
plot(x, y)
lines(Xgrid[, 1], knnr_out$pred, col = "red", lwd = 2)


# let's for the sake of completeness, perform a linear regression
lm_out <- lm(y ~ x)

# checking the summary of lm_out
summary(lm_out)

# scatter plot of x against y
plot(x, y)

# Add the regression line
abline(lm_out, col = "red", lwd = 2.5)


# let's again perform knn, now with k = 19
knnr_out_19 <- knn.reg(x, y = y, test = Xgrid, k = 19)
lines(Xgrid[, 1], knnr_out_19$pred, col = 3, lwd = 3)

# Having the k = 19 we get a very underfit model. 









