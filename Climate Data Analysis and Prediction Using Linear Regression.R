#Loading libraries
library(tidyverse)
library(readr)
library(ggplot2)
install.packages("scatterplot3d")
library(scatterplot3d)

#Loading the dataset

clim <- read.csv("https://userpage.fu-berlin.de/soga/data/raw-data/Climfrance.csv",sep = ";")
#Check the structure of the dataset
str(clim)
#View the dataset
view(clim)

#The altitude and p_mean variables are characters and should be converted to integers
#Removing the comma and converting altitude to int
clim$altitude <- as.numeric(gsub(",","",clim$altitude))

#Removing the comma and converting p_mean to int
clim$p_mean <- as.numeric(gsub(",","",clim$p_mean))


#Getting France map 
France  <- map_data("france")
#Plotting the maps with the climate locations
ggplot() +geom_polygon(data = France,aes(x = long, y = lat, group = group),
 colour = "red", fill = "blue") +geom_point(data = clim,
aes(x = lon, y = lat), alpha = 0.5) + theme_bw() +
labs( title= "Map of France with dots representing the 36 climate sites", x = "Longitude", y="Latitude") +coord_map()

#Ecercise 1
climfrar <- clim[1:34,]

mean_annual_temp <- lm(t_mean ~ altitude + lat + lon, data= climfrar)
summary(mean_annual_temp )
#The test shows that the mean annual temperature in France depends 
#significantly on altitude and latitude with p-values of 3.17×10⁻⁸
#and 1.24×10⁻¹⁰ respectively

#Based on the p-value, the latitude and altitude are significant
#and the longitude is not a significant variable

#Exercise 2

mean_annual_temp_2 <- lm(t_mean ~ altitude + lat, data = climfrar)


#predicting values for mont-ven and pic-du-midi

new_data1 <- list(altitude = 1212, lat = 44.16)
new_data2 <- list(altitude = 2860, lat = 42.93)

# Prediction 
pred_temp_ven <- predict(mean_annual_temp_2, newdata = new_data1, interval = "p", level = 0.95)
pred_temp_ven
# Prediction for the second set of values
pred_temp_pic <- predict(mean_annual_temp_2, newdata = new_data2, interval = "p", level = 0.95)

pred_temp_pic


#The predicted mean for Mont-Ventoux is 6.17°C
#with a 95-prediction-interval of [3.79°C,8.54°C]
#Since the measured mean is 3.6°C
#our model is not accurate enough to reproduce the temperature for Mont-Ventoux.
#The predicted mean for Pic-du-midi is −3.45°C with a 95 -prediction-interval of [−8.35°C,1.45°C]
# So, our prediction still covers the measured mean of −1.2°C

#Exercise 3

scatter_3d <- with(climfrar, scatterplot3d(altitude, 
    lat, t_mean,  pch = 16, highlight.3d = TRUE, angle = 45,))
scatter_3d$plane3d(mean_annual_temp_2)

#From the 3D scatter plot we see that most of the 
#point data is very well approximated by the plane except
#for data points with low altitude and low latitude. 
#This indicates that the linear model does not fit all of the data well

summary(mean_annual_temp_2)

#The adjusted R² value is 0.82
#That means that roughly 82% of the variance found in the mean annual temperature
#can be explained by the variables latitude and altitude.
#The model with longitude explained more variation than this model.
#The Residual Standard Error is a measure of the quality of a linear
#regression fit. On average, the actual mean temperature can deviate 
#from the regression plane by 0.73°C. Given that the mean of all data
#points is 11.68°C, the percentage error is 6.2%. 
#The model is statistically significant with a p-value of 1.268e-12














