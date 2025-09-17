Provides tools to test for spatial dependence in canonical variates, compute the influence of a certain canonical variate in different locations, and compute the influence of neighbors towards the global CCA coefficient

Sample Code
library(GWCCA)
library(readxl)
library(CCP)
library(CCA)
library(spdep)

data <- read_excel(file.choose())

#Split data into X and Y
X <- data[,c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x11","x12","x13","x14","x15")]
Y <- data[,c("y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","y13","y15")]

#Scale X and Y
X.std <- scale(X)
Y.std <- scale(Y)

#Perform global cca (cca for all data)
cca.result <- cc(X.std,Y.std)

#Canonical variates per observation
U1 <- as.matrix(X.std) %*% cca.result$xcoef[,1]
V1 <- as.matrix(Y.std) %*% cca.result$ycoef[,1]

#Aggregate per location
loc_id <- rep(1:4, times = c(15,15,15,15))
coords <- data.frame(
  lon = c(106.8, 107.6, 110.4, 120.3),
  lat = c(-6.2, -6.9, -7.8, -7.5)
)

#Make weight matrix
nb <- knn2nb(knearneigh(coords, k=2))
lw <- nb2listw(nb, style="W")
lw_mat <- listw2mat(lw)

#Try out the functions from GWCCA!
#Moran's test for spatial dependence in canonical variate U1 V1
cca_moran(U1,V1,loc_id,lw)

#Find out how strong is the influence of U1 and V1 in different locations
cca_variates_local(U1,V1,loc_id,lw_mat)

#Find out how strong is the neighbor's influence from the global cca coefficients
global_cor <- cca.result$cor
rho_local(global_cor[1],lw_mat)
