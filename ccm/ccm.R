## ----setup, echo=FALSE, warning=FALSE, message=F-------------------------
library(knitr)#; library(zoo); library(xts)
library(plotly)
library(rEDM)
library(ggplot2)#; library(cowplot)
library(dplyr); library(tidyr)


opts_chunk$set(fig.align = 'center',
               fig.show = 'hold',
               fig.height = 5,
               warning = FALSE, message = FALSE, error = FALSE, echo=FALSE)
options(formatR.arrow = TRUE,width = 90)###, cache=TRUE)

## ----call vignette, echo=TRUE, eval=FALSE--------------------------------
## vignette("rEDM-tutorial", package="rEDM")

## ----generate data, echo=TRUE--------------------------------------------

## Two vectors to store data
X <- c()
Y <- c()
## Initial values
X[1] <- 0.1
Y[1] <- 0.1
## Iterate the dynamics 150 time steps
for(i in 2:150){
  X[i] <- 3.77*X[i-1]*(1-X[i-1])
  Y[i] <- 3.82*Y[i-1]*(1-Y[i-1]-0.05*X[i-1])
}
XY<-as.data.frame(cbind(X,Y))
  

## ----plot 1st time series------------------------------------------------
par(cex=1.1,lwd=2)
  plot(20:50,X[20:50],type="b", pch=18, col="blue",ylim=c(min(X,Y),max(X,Y)),main='Two Species',xlab = 'time',ylab='Population')
  lines(20:50,Y[20:50],pch=19, col="red", type="b",lty=2,lwd=2)
  legend(x = "bottomright", legend = c("X", "Y"),lty=c(1,2),pch=c(18,19) ,col = c("blue", "red"), inset = 0.02,lwd=2)
  # save data for future use
  write.csv(XY, file = "data_unidirectional.csv",row.names=F)
  
  

## ----plot correlation----------------------------------------------------
fit<-lm(Y ~ X)
plot(X,Y,main='Correlation (X,Y)')
abline(fit$coefficients[1],fit$coefficients[2])
legend(x = "bottomleft", legend = paste('r =',round(cor(X,Y)*100)/100),inset = 0.02,col = 'black',lty = 1)


## ----optimal embeddings--------------------------------------------------
options(warn = -1)
E_star_X<-which.max(simplex(X,silent=T)$rho)
print(paste('E*(X) =',E_star_X))

E_star_Y<-which.max(simplex(Y,silent=T)$rho)
print(paste('E*(Y) =',E_star_Y))


## ----load make_block, eval=FALSE-----------------------------------------
## source("https://raw.githubusercontent.com/mathbio/edmTutorials/master/utilities/make_block.R")

## ----MX X_xmap_Y---------------------------------------------------------

Shadow_MXY<-make_block(XY,max_lag = 2)
Shadow_MX<-Shadow_MXY[,2:3]
Shadow_MY<-Shadow_MXY[,4:5]
focal_point<-50
dist.matrix_X <- as.matrix(dist(Shadow_MX, upper=TRUE))
dist.matrix_Y <- as.matrix(dist(Shadow_MY, upper=TRUE))
neigb_X <- order(dist.matrix_X[focal_point,])[2:4]
neigb_Y <- order(dist.matrix_Y[focal_point,])[2:4]
color=rgb(0,0,0,alpha=0.1)
plot(Shadow_MX[,2],Shadow_MX[,1],col = color, pch=16,main='MX',xlab='x(t)',ylab='x(t+1)')
points(Shadow_MX[focal_point,2],Shadow_MX[focal_point,1],col='red',cex=2.5, pch = 1, lwd = 4)
points(Shadow_MX[neigb_X[1],2],Shadow_MX[neigb_X[1],1],pch=16,col='blue')
points(Shadow_MX[neigb_X[2],2],Shadow_MX[neigb_X[2],1],pch=16,col='blue')
points(Shadow_MX[neigb_X[3],2],Shadow_MX[neigb_X[3],1],pch=16,col='blue')

plot(Shadow_MX[,2],Shadow_MX[,1],col = color, pch=16,xlim=c(Shadow_MX[focal_point,2]-0.05,Shadow_MX[focal_point,2]+0.05),ylim=c(Shadow_MX[focal_point,1]-0.1,Shadow_MX[focal_point,1]+0.1),main='MX (Zoom in)',xlab='x(t)',ylab='x(t+1)')
points(Shadow_MX[focal_point,2],Shadow_MX[focal_point,1],pch=16,col='red',cex=1.5)
points(Shadow_MX[neigb_X[1],2],Shadow_MX[neigb_X[1],1],pch=16,col='blue',cex=1.5)
points(Shadow_MX[neigb_X[2],2],Shadow_MX[neigb_X[2],1],pch=16,col='blue',cex=1.5)
points(Shadow_MX[neigb_X[3],2],Shadow_MX[neigb_X[3],1],pch=16,col='blue',cex=1.5)




## ----MY X_xmap_Y---------------------------------------------------------
plot(Shadow_MY[,2],Shadow_MY[,1],col = color, pch=16,main='MY',xlab='y(t)',ylab='y(t+1)')
points(Shadow_MY[focal_point,2],Shadow_MY[focal_point,1],pch=16,col='red')
points(Shadow_MY[neigb_X[1],2],Shadow_MY[neigb_X[1],1],pch=16,col='blue')
points(Shadow_MY[neigb_X[2],2],Shadow_MY[neigb_X[2],1],pch=16,col='blue')
points(Shadow_MY[neigb_X[3],2],Shadow_MY[neigb_X[3],1],pch=16,col='blue')

## ----MY - Y_xmap_X-------------------------------------------------------

color=rgb(0,0,0,alpha=0.1)
plot(Shadow_MY[,2],Shadow_MY[,1],col = color, pch=16,main='MX',xlab='x(t)',ylab='x(t+1)')
points(Shadow_MY[focal_point,2],Shadow_MY[focal_point,1],col='red',cex=2.5, pch = 1, lwd = 4)
points(Shadow_MY[neigb_Y[1],2],Shadow_MY[neigb_Y[1],1],pch=16,col='blue')
points(Shadow_MY[neigb_Y[2],2],Shadow_MY[neigb_Y[2],1],pch=16,col='blue')
points(Shadow_MY[neigb_Y[3],2],Shadow_MY[neigb_Y[3],1],pch=16,col='blue')

plot(Shadow_MY[,2],Shadow_MY[,1],col = color, pch=16,xlim=c(Shadow_MY[focal_point,2]-0.1,Shadow_MY[focal_point,2]+0.1),ylim=c(Shadow_MY[focal_point,1]-0.1,Shadow_MY[focal_point,1]+0.1),main='MY (Zoom in)',xlab='x(t)',ylab='x(t+1)')
points(Shadow_MY[focal_point,2],Shadow_MY[focal_point,1],pch=16,col='red',cex=1.5)
points(Shadow_MY[neigb_Y[1],2],Shadow_MY[neigb_Y[1],1],pch=16,col='blue',cex=1.5)
points(Shadow_MY[neigb_Y[2],2],Shadow_MY[neigb_Y[2],1],pch=16,col='blue',cex=1.5)
points(Shadow_MY[neigb_Y[3],2],Shadow_MY[neigb_Y[3],1],pch=16,col='blue',cex=1.5)




## ----MX Y_xmap_X---------------------------------------------------------
plot(Shadow_MX[,2],Shadow_MX[,1],col = color, pch=16,main='MX',xlab='y(t)',ylab='y(t+1)')
points(Shadow_MX[focal_point,2],Shadow_MX[focal_point,1],pch=16,col='red')
points(Shadow_MX[neigb_Y[1],2],Shadow_MX[neigb_Y[1],1],pch=16,col='blue')
points(Shadow_MX[neigb_Y[2],2],Shadow_MX[neigb_Y[2],1],pch=16,col='blue')
points(Shadow_MX[neigb_Y[3],2],Shadow_MX[neigb_Y[3],1],pch=16,col='blue')

## ----estimating X from Y-------------------------------------------------
lib<-lib <- c(1, NROW(Shadow_MXY))
block_lnlp_output <- block_lnlp(Shadow_MXY, lib = lib, pred = lib, columns = c("X",
 "Y","Y_1"), target_column = "X", stats_only = FALSE, first_column_time = TRUE)
observed <- block_lnlp_output$model_output[[1]]$obs
predicted <- block_lnlp_output$model_output[[1]]$pred

fit_YX<-lm(predicted ~ observed)
plot_range <- range(c(observed, predicted), na.rm = TRUE)
plot(observed, predicted, xlim = plot_range, ylim = plot_range, xlab = "Observed",
ylab = "Predicted")
abline(fit_YX$coefficients[1],fit_YX$coefficients[2])
legend(x = "bottomright", legend = paste('r =',round(cor(observed,predicted)*100)/100),inset = 0.02,col = 'black',lty = 1)




## ----estimating Y from X-------------------------------------------------
lib<-lib <- c(1, NROW(Shadow_MXY))
block_lnlp_output <- block_lnlp(Shadow_MXY, lib = lib, pred = lib, columns = c("X",
 "X","X_1"), target_column = "Y", stats_only = FALSE, first_column_time = TRUE)
observed <- block_lnlp_output$model_output[[1]]$obs
predicted <- block_lnlp_output$model_output[[1]]$pred

fit_YX<-lm(predicted ~ observed)
plot_range <- range(c(observed, predicted), na.rm = TRUE)
plot(observed, predicted, xlim = plot_range, ylim = plot_range, xlab = "Observed",
ylab = "Predicted")
abline(fit_YX$coefficients[1],fit_YX$coefficients[2])
legend(x = "bottomleft", legend = paste('r =',round(cor(observed,predicted)*100)/100),inset = 0.02,col = 'black',lty = 1)



## ----convergent----------------------------------------------------------
  # cross map from X to Y
  X_xmap_Y<- ccm(XY, E = 2, lib_column = "X", target_column = "Y", lib_sizes = seq(10, 150, by = 10), num_samples = 100, random_libs = TRUE, replace = TRUE)
  # cross map from Y to X
  Y_xmap_X<- ccm(XY, E = 2, lib_column = "Y", target_column = "X", lib_sizes = seq(10, 150, by = 10), num_samples = 100, random_libs = TRUE, replace = TRUE)
  
  #mean values
  X_xmap_Y_means <- ccm_means(X_xmap_Y)
  Y_xmap_X_means <- ccm_means(Y_xmap_X)
  
  #plot graphs
  plot(X_xmap_Y_means$lib_size, pmax(0, X_xmap_Y_means$rho), type = "l", col = "red",main='Two Species', xlab = "Library Size", ylab = "Cross Map Skill (rho)", ylim = c(0,1))
  lines(Y_xmap_X_means$lib_size, pmax(0, Y_xmap_X_means$rho), col = "blue")
  legend(x = "topleft", legend = c("X_xmap_Y", "Y_xmap_X"), col = c("red", "blue"), cex=1.1,lwd=2, inset = 0.02)
  

## ----ex3-----------------------------------------------------------------
data(sardine_anchovy_sst)

