# load the library/ies
library(rgl)

# We have generated a sample data here
fitdata <- as.data.frame( matrix(c(1,87,42, 6,73,43, 7,66,44, 15,62,54, 12,68,45,
                                   4,92,46, 12,60,50, 13,70,46, 14,71,54, 10,64,
                                   47), byrow=TRUE,ncol=3))
# check the head of fitdata dataframe
head(fitdata)

# check the dimension of the X data matrix
dim(X)

# given names to the data frame columns
names(fitdata) <- c("fitness","weight","lungvol") 

# checking the summary for each column
summary(fitdata)


# performing a very simple linear regression
lm_fitness <- lm(fitness ~ weight + lungvol, data = fitdata)

# checking the summary of the above linear model
summary(lm_fitness)

# let's plot the two variables 

m1<-seq(55,95,length=20) # weight taking values between 55-95
m2<-seq(40, 55,length=20) # lung volume takes values between 40-55

# expanding the Xgrid
Xgrid <- expand.grid(weight = m1, lungvol = m2)
Xgrid
pred.grid<-predict(lm_fitness,newdata=Xgrid)
tt<-cbind(Xgrid, pred.grid) 
# check what tt contains
tt

# This is the perspective plot
res<-persp(m1, m2, matrix(pred.grid,nrow=length(m1)),
           border=grey(0.6), xlab="Weight",ylab="LungVolume",
           zlab="fitness", theta=0, phi=15)
# Plotting the points 
points(trans3d(fitdata$weight,fitdata$lungvol,fitdata$fitness,pmat=res), 
       pch=16,col=(2:3)[1.5+.5*sign(lm_fitness$residuals)])


# Creat the 3D plots
with(tt, plot3d(lungvol, weight, pred.grid, col = "grey80", size = 3, zlab = "fitness"))
with(fitdata, plot3d(lungvol, weight, fitness, add = TRUE, size = 1,
                     col = (2:3)[1.5 + 0.5 * sign(lm_fitness$residuals)], type = "s"))


# let's now fit a KNN model 
X <- fitdata[, c("weight", "lungvol")]
knnr_out_1 <- knn.reg(X, y = fitdata$fitness, k = 1, test = X)

# let's find the length of knnr_out_1$prediction
length(knnr_out_1$pred)
length(m1)
length(m2)

res <- persp(m1, m2, matrix(knnr_out_1$pred, nrow = length(m1)),
             border = grey(0.6), xlab = "Weight",
              ylab = "Lungvol", zlab = "fitness",
             theta = 15, phi = 15)

points(trans3d(fitdata$weight, fitdata$lungvol, fitdata$fitness, pmat = res),
       pch = 16, col = c(2:3)[1.5 + 0.5 * sign(lm_fitness$residuals)])


with(tt, plot3d(lungvol, weight, knnr_out_1$pred, col = "grey80", size = 3, zlab = "fitness"))
with(fitdata, plot3d(lungvol, weight, fitness, add = TRUE, size = 2, col = "red", type = "s"))



# the code copied from ChatGPT
# Ensure that knnr_out_1$pred has the correct length
z_matrix <- matrix(knnr_out_1$pred, nrow = length(m1), ncol = length(m2))

# Plot the perspective plot
res <- persp(m1, m2, z_matrix,
             border = grey(0.6), xlab = "Weight",
             ylab = "Lungvol", zlab = "Fitness",
             theta = 15, phi = 15)

# The above code needs to fixed really well
# There are a lot of mess -- Clean it up!





