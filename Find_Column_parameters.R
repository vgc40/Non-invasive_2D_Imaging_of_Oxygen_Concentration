setwd("C:/Users/gara009/OneDrive - PNNL/Documents/GitHub/Non-invasive_2D_Imaging_of_Oxygen_Concentration/")
rm(list=ls());graphics.off()

# Install packages
#install.packages("raster"); install.packages("rgdal");install.packages("tmap");install.packages('Kendall')

# Load libraries
library(raster); library(rgdal);library(tmap);library(Kendall); 
library(plotly); library(crayon)

graphics.off()

# This code is intended to help the user find the values for the necessary input parameters needed in the Input_col_parameters.csv

#################################################################
# Read in input calibration curve parameters and clamp location per column
#NOTE: The user must pre-populate column and the calibration values into the input file before running this code. Follow instructions in the first row of input file. 

#################################################################
# Set up working directory and manually identify Column number

Column.number = 1
input.path = "C:/Users/gara009/OneDrive - PNNL/Documents/GitHub/Non-invasive_2D_Imaging_of_Oxygen_Concentration/Input"

##############################################################
# Read in input calibration curve parameters and clamp location per column

input.files = list.files(path = input.path, pattern = ".csv", full.names = T)
input = read.csv(input.files[1], stringsAsFactors = F, skip = 1)

# Subset to the column that you are working with

input.column = subset(input, input$column == Column.number)

Ro = input.column$Ro
a = input.column$a
Ksv = input.column$Ksv

##############################################################
# Read in the images
# If the column or positions of the clamps did not move through the duration of the experiment, then we only need to load one image to find the column parameters.
#############################################################

img.path = ("C:/Users/gara009/OneDrive - PNNL/Documents/GitHub/Non-invasive_2D_Imaging_of_Oxygen_Concentration/Images/output")

red.imgs = list.files(path = img.path, pattern = "R.tif", full.names = T) # 
green.imgs = list.files(path = img.path,pattern = "G1.tif", full.names = T)

red.img = raster(red.imgs[1])
plot(red.img)
green.img = raster(green.imgs[1])
plot(green.img)

# Perform the basic image processing to visualize the image better

subtraction = red.img - green.img
#plot(subtraction)

R = subtraction/green.img
#plot(R)

# Check out min and max value in the pixels to be removed as outlier
pixel.min.value = R[which.min(R)]
pixel.max.value = R[which.max(R)]

# # reassign cells with outlier values of Inf to NA
DOmax = 12
f <- function(x) {
  #x[x == Inf] <- NA
  x[x <= (a+(1-a)*(1/(1+Ksv*DOmax)))*Ro] <- NA #Fixed to have a max DO value of 10
  x[x > (Ro*1.05)] <- NA #Provided a 5% buffer above the Ro
  x}

R.clean = calc(R, f)
pixel.min.value.clean = R.clean[which.min(R.clean)]
pixel.max.value.clean = R.clean[which.max(R.clean)]

# Apply the calibration curve

# result = (Ro - R)/(Ksv*(R-Ro*a))
result = (Ro - R.clean)/(Ksv*(R.clean-Ro*a))

# Set some plotting parameters to quickly visualize the column

cuts = seq(0,DOmax, by = 0.5)
# Set color scale for plot
col1 = colorRampPalette(c('#313695', '#4575b4','#74add1','#abd9e9','#e0f3f8','#ffffbf','#fee090','#fdae61','#f46d43','#d73027','#a50026'))

plot(result, breaks=cuts, col = col1((length(cuts)-1)), zlim = c(0,DOmax),xlim=c(xmin(result),xmax(result)), ylim=c(ymin(result), ymax(result)))

#################################################################
#Re-size the image to cm
###############################################################
# Manually extent of the raster to until the figure matches the length of the column. These are some of the parameters to optimize with this code and to save in the input file
##############################################################
pixel.min.result = result[which.min(result)]
pixel.max.result = result[which.max(result)]

x.min.cm = -5 #Manually edit value in cm
x.max.cm = 36 #Manually edit value in cm

col.len = extent(x.min.cm,x.max.cm,0, ((x.max.cm-x.min.cm)*nrow(result))/ncol(result)) 

result2 = setExtent(result,col.len)

plot(result2, breaks=cuts, col = col1((length(cuts)-1)), zlim = c(0,DOmax),xlim=c(xmin(result2),xmax(result2)), ylim=c(ymin(result2), ymax(result2)))

#xmin amd xmax are successfully optimized when the length inside of the column spans the length of the optode. 

## Make sure to transfer the optimized xmin and xmax values to x.min.in.cm and x.max.in.cm cells in the Input file

#####################################################
#Examine if your column needs to be slightly rotated. Manually edit the angle based on what is needed
######################################################
#Run fucntion
rotate <- function(x, angle=0, resolution=res(x)) {
  y <- x; crs(y) <- "+proj=aeqd +ellps=sphere +lat_0=90 +lon_0=0"
  projectRaster(y, res=resolution,
                crs=paste0("+proj=aeqd +ellps=sphere +lat_0=90 +lon_0=", -angle))
}

# Manually adjust angle. 

 rotated.image = rotate(result2, -0.1) # Manually edit the number here to change the angle
 
 plot(rotated.image, breaks=cuts, col = col1((length(cuts)-1)), zlim = c(0,DOmax),xlim=c(xmin(rotated.image),xmax(rotated.image)), ylim=c(ymin(rotated.image), ymax(rotated.image)))
 
 # Once satisfied with the rotated image, copy the angle in the rot.num cell in the Input file
 
 
################################################################
 # Optimize x1,x2 and y1,y2 coordinates to crop the column image only to the optode portion
################################################################

crop.extent = extent(0,30,5,14) # Optimize this values in cm the first to values are on the x axis and the second ones ar in the y axis

result.cropped = crop(rotated.image,crop.extent)

plot(result.cropped, breaks=cuts, col = col1((length(cuts)-1)), zlim = c(0,DOmax),xlim=c(xmin(result.cropped),xmax(result.cropped)),ylim=c(ymin(result.cropped), ymax(result.cropped)))

# Once satisfied with the crop extent, make sure to copy the values in the crop.extent.x1, crop.extent.x2, crop.extent.y1, crop.extent.y2 cells in the Input file

#################################################################
# Deterimine clamp locations

img.matrix = as.matrix(result.cropped)

x.in.cm = seq(xres(result.cropped),(ncol(result.cropped)*xres(result.cropped)), by = xres(result.cropped))

do = rowMeans(t(img.matrix), na.rm = TRUE) # Calculates the mean DO values along the length of the column

data = na.omit(as.data.frame(cbind(x.in.cm,do)))
plot_ly(data = data, x = ~x.in.cm,y = ~do)

# Manually zoom in the plot and determine the x positions where the DO signal starts to get noisy or out of the expected trend.This number determines the beginning of the clamp. 

# Once satisfied with the selected range place the x values in the clamp1, clamp2, clamp3, clamp4, clamp5 cells of the Input file depending on how many 

# If no clamp available fill the cell value with 0
