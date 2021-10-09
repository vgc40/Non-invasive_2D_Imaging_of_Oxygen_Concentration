rm(list=ls());graphics.off()

home.path = ""
setwd (home.path)

# Install packages
#install.packages("raster"); install.packages("rgdal");install.packages("tmap");install.packages('Kendall')

# Load libraries
library(raster); library(rgdal);library(tmap);library(Kendall); 
library(plotly); library(crayon)

graphics.off()

#######################################################################

# Set by manually by the user. Write down the column number you will be working with. Ensure that the folders selected below exist in the home directory

Column.number = 1
Column.folder = "Column_1"
Column.phase = "DI"  # Phase options are DI, Injection and Sampling. This can be replaced with treatment
DOmax = 10 #Max DO value that is expected after calibration values.

# Use the variables below to trim the first and last couple of cm of the column to reduce edge effects.

remove.data.start = 2 #in cm. Remove data the first XX cm. Use 0 if no data needs to be removed.
remove.data.end = 28 # in cm Remove data starting ## cm. Use 30 (or the length of the column) is no data needs to be removed.

clamp.threshold = 1.85 # Approx thickness of the clamp and a safety factor

column.path = paste(home.path,Column.folder, Column.phase, "Example_Images","output", sep = "/")

#Changing working directory so it is easy to import images 

setwd(column.path)

###########################################################################
# Defining paths. Make sure the folders in the path exist in the Column Folder and Treatment
###############################
plots.path = paste0(home.path,Column.folder,"/", Column.phase, "/Plots_out")
processed.img.path = paste0(home.path,Column.folder,"/", Column.phase, "/Images_processed")
img.path = paste0(home.path,Column.folder,"/", Column.phase, "/Images_out")
crop.path = paste0(home.path,Column.folder,"/", Column.phase, "/Images_cropped")
matrix.path = paste0(home.path,Column.folder,"/", Column.phase, "/Matrix")
input.path = paste0(home.path,"Input")
plots.path.clean = paste0(home.path,Column.folder,"/",Column.phase, "/Plots_clean")
img.path.clean = paste0(home.path,Column.folder, "/",Column.phase, "/Images_clean")
###############################

#############
# Read in input calibration curve parameters and clamp location per column
#NOTE: The user must pre-populate column number as well as the calibration values into the input file before running this code. 

#The user must run the Find_Column_parameters.R code to populate all the cells required in the Input file. 

#Follow instructions in the first row of input file. 

#############
# Read in input calibration curve parameters and clamp location per column

input.files = list.files(path = input.path, pattern = ".csv")
input.need = input.files[grep(pattern = Column.phase, x = input.files)]
input = read.csv(paste0(input.path,"/",input.need), stringsAsFactors = F, skip = 1)

# Subset to the column that you are working with

input.column = subset(input, input$column == Column.number)

Ro = input.column$Ro
a = input.column$a
Ksv = input.column$Ksv

rot.num = input.column$rot.num
# Input starting x-axis values for the clamps. 

clamp1 = input.column$clamp1 # in cm 
clamp2 =input.column$clamp2  # in cm
clamp3 = input.column$clamp3  #in cm
clamp4 = input.column$clamp4  #in cm
clamp5 = input.column$clamp5  #in cm

##############################################################
# Read in the images
#############################################################

red.img = list.files(pattern = "R.tif") 
green.img = list.files(pattern = "G1.tif")

# Checking consisency across number of images
if (length(red.img)!= length(green.img)){
  cat(red("Error: Number of red and green images don't match", "Please check!\n"))
}else{
for (i in 1:length(red.img)){

  graphics.off()
  
red.imgs = raster(red.img[i])
#plot(red.imgs)
green.imgs = raster(green.img[i])
#plot(green.imgs)
name = gsub("_R.tif","",red.img[i])

# Perform the basic image processing, subtraction and division 

subtraction = red.imgs - green.imgs
#plot(subtraction)

R = subtraction/green.imgs
#plot(R)

# Check out min and max value in the pixels to be removed as outlier
pixel.min.value = R[which.min(R)]
pixel.max.value = R[which.max(R)]


# # reassign cells with outlier values of Inf to NA
f <- function(x) {
  #x[x == Inf] <- NA
  x[x <= (a+(1-a)*(1/(1+Ksv*DOmax)))*Ro] <- NA #Fixed to have a max DO value of 10
  x[x > (Ro*1.05)] <- NA #Provided a 5% buffer above the Ro
  x}

R.clean = calc(R, f)
pixel.min.value.clean = R.clean[which.min(R.clean)]
pixel.max.value.clean = R.clean[which.max(R.clean)]

# Apply the calibration curve

result = (Ro - R.clean)/(Ksv*(R.clean-Ro*a))
#plot(result)

cuts = seq(0,DOmax, by = 0.5)

#This is a color blind friendly palette
col1 = colorRampPalette(c('#313695', '#4575b4','#74add1','#abd9e9','#e0f3f8','#ffffbf','#fee090','#fdae61','#f46d43','#d73027','#a50026'))
                        

#plot with defined breaks

pixel.min.result = result[which.min(result)]
pixel.max.result = result[which.max(result)]


# Set extent of the raster to the length of the column
x.min.cm = input.column$x.min.in.cm #in cm
x.max.cm = input.column$x.max.in.cm #in cm
col.len = extent(x.min.cm,x.max.cm,0, ((x.max.cm-x.min.cm)*nrow(result))/ncol(result)) #These numbers were manually adjusted to get the 30 cm in the Find_Column_parameter.R code

result2 = setExtent(result,col.len)

# Rotate image
rotate <- function(x, angle=0, resolution=res(x)) {
  y <- x; crs(y) <- "+proj=aeqd +ellps=sphere +lat_0=90 +lon_0=0"
  projectRaster(y, res=resolution,
                crs=paste0("+proj=aeqd +ellps=sphere +lat_0=90 +lon_0=", -angle))
}

 rotated.image = rotate(result2, rot.num)

# Crop images

crop.extent = extent(input.column$crop.extent.x1,input.column$crop.extent.x2,input.column$crop.extent.y1,input.column$crop.extent.y2)

result.cropped = crop(rotated.image,crop.extent)

pdf(paste0(crop.path,"/",name,"_",Column.folder,"_", Column.phase,"_Cropped",".pdf"))
plot(result.cropped, breaks=cuts, col = col1((length(cuts)-1)), zlim = c(0,DOmax),xlim=c(xmin(result.cropped),xmax(result.cropped)),ylim=c(ymin(result.cropped), ymax(result.cropped)))
dev.off()


# Crop only the area of interest. Removing 1 cm on the edges to reduce border effects 

crop.extent = extent(input.column$crop.extent.x1,input.column$crop.extent.x2,(input.column$crop.extent.y1+1),(input.column$crop.extent.y2-1)) #Area of interest
cropped.interest = crop(result.cropped,crop.extent)


### Remove clamps

clamp1.x.in.cm = seq(clamp1,(clamp1+clamp.threshold), by = xres(cropped.interest))

clamp2.x.in.cm = seq(clamp2,(clamp2+clamp.threshold), by = xres(cropped.interest))

clamp3.x.in.cm = seq(clamp3,(clamp3+clamp.threshold), by = xres(cropped.interest))

clamps.y.in.cm = seq(ymin(cropped.interest),ymax(cropped.interest), by = yres(cropped.interest))

clamp1.extent = extent(clamp1.x.in.cm[1],clamp1.x.in.cm[length(clamp1.x.in.cm)],clamps.y.in.cm[1],clamps.y.in.cm[length(clamps.y.in.cm)]) 

clamp2.extent = extent(clamp2.x.in.cm[1],clamp2.x.in.cm[length(clamp2.x.in.cm)],clamps.y.in.cm[1],clamps.y.in.cm[length(clamps.y.in.cm)]) 

clamp3.extent = extent(clamp3.x.in.cm[1],clamp3.x.in.cm[length(clamp3.x.in.cm)],clamps.y.in.cm[1],clamps.y.in.cm[length(clamps.y.in.cm)]) 

if (clamp4 >0){
clamp4.x.in.cm = seq(clamp4,(clamp4+clamp.threshold), by = xres(cropped.interest))

clamp4.extent = extent(clamp4.x.in.cm[1],clamp4.x.in.cm[length(clamp4.x.in.cm)],clamps.y.in.cm[1],clamps.y.in.cm[length(clamps.y.in.cm)])
cropped.interest[cellsFromExtent(cropped.interest,clamp4.extent)] = NA
}

if (clamp5 >0){
  clamp5.x.in.cm = seq(clamp5,(clamp5+clamp.threshold), by = xres(cropped.interest))
  
  clamp5.extent = extent(clamp5.x.in.cm[1],clamp5.x.in.cm[length(clamp5.x.in.cm)],clamps.y.in.cm[1],clamps.y.in.cm[length(clamps.y.in.cm)])
  cropped.interest[cellsFromExtent(cropped.interest,clamp5.extent)] = NA
}

# Make pixel values of the clamps as NA

cropped.interest[cellsFromExtent(cropped.interest,clamp1.extent)] = NA

cropped.interest[cellsFromExtent(cropped.interest,clamp2.extent)] = NA

cropped.interest[cellsFromExtent(cropped.interest,clamp3.extent)] = NA

# Plot area of interest again 
pdf(paste0(img.path,"/",name,"_",Column.folder,"_", Column.phase,"_Processed",".pdf"))
plot(cropped.interest, breaks=cuts, col = col1((length(cuts)-1)), zlim = c(0,DOmax),xlim=c(xmin(cropped.interest),xmax(cropped.interest)),xlab="Column Length (cm)", yaxt = "n") # This is to not print the y axis
dev.off()

# Extract pixel values

img.matrix = as.matrix(cropped.interest)

x.in.cm = seq(xres(cropped.interest),(ncol(cropped.interest)*xres(cropped.interest)), by = xres(cropped.interest))

y.in.cm = seq(yres(cropped.interest),(nrow(cropped.interest)*yres(cropped.interest)), by = yres(cropped.interest))

Mean.DO.mg.per.L= rowMeans(t(img.matrix), na.rm = TRUE) #col means gives row means. We are calculating the mean of the columns

data = as.data.frame(cbind(x.in.cm,Mean.DO.mg.per.L))

write.csv(data,paste0(matrix.path,"/",name,"_Mean_DO_cm","_",Column.folder,"_",Column.phase,".csv"), row.names = F)

pdf(paste0(plots.path,"/",name,"_",Column.folder,"_",Column.phase, ".pdf"))
plot(data$x.in.cm,data$Mean.DO.mg.per.L, xlab="Column Length (cm)",ylab="DO mg/L",cex.axis=1,cex.lab=1,col=c("black"),pch=19, ylim = c(0,DOmax))
title(name)
dev.off()

######################################################
# Repeating the profile plots removing the first couple of cm at the beginning and at the end

# Removing data at the start
data.start = subset(data, data$x.in.cm >=  remove.data.start)
# Removing data at the end
data.end = subset(data.start, data.start <= remove.data.end )

# Making the plots
write.csv(data.end,paste0(matrix.path,"/",name,"_Mean_DO_cm","_",Column.folder,"_",Column.phase,".csv"), row.names = F)

pdf(paste0(plots.path.clean,"/",name,"_",Column.folder,"_",Column.phase,"no_regression_clean", ".pdf"))
plot(data.end$x.in.cm,data.end$Mean.DO.mg.per.L, xlab="Column Length (cm)",ylab="DO mg/L",cex.axis=1,cex.lab=1,col=c("black"),pch=19, ylim = c(0,DOmax))
title(name)
dev.off()

######################################################
# Also re-print cropped figure after removing cm from start to end

crop.extent.clean = extent(remove.data.start,remove.data.end,input.column$crop.extent.y1,input.column$crop.extent.y2)

result.cropped.clean = crop(cropped.interest,crop.extent.clean)

# Plot area of interest again 
pdf(paste0(img.path.clean,"/",name,"_",Column.folder,"_", Column.phase,"_Processed_Clean",".pdf"))
plot(result.cropped.clean, breaks=cuts, col = col1((length(cuts)-1)), zlim = c(0,DOmax),xlim=c(xmin(result.cropped.clean),xmax(result.cropped.clean)),xlab="Column Length (cm)", yaxt = "n") # This is to not print the y axis
dev.off()

rm(data)
rm(name); rm(rotated.image)
}
}

