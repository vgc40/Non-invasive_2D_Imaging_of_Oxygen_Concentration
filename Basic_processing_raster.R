setwd("C:/Users/gara009/OneDrive - PNNL/Documents/GitHub/Non-invasive_2D_Imaging_of_Oxygen_Concentration/")
rm(list=ls());graphics.off()

# Install packages
#install.packages("raster"); install.packages("rgdal");install.packages("tmap");install.packages('Kendall')

# Load libraries
library(raster); library(rgdal);library(tmap);library(Kendall); 
library(plotly); library(crayon)

graphics.off()

#######################################################################

# Set by manually by the user. Write down the column number you will be working with

Column.number = 0
Column.folder = "Column_0"
Column.phase = "DI"  # Phase options are DI, Injection and Sampling
Folder = "/No_regressions" # folder for profiles with no regression
DOmax = 10 #Max DO value that is expected Bump it up to 12-15 absolute MAx
remove.data.start = 2 #in cm. Remove data the first XX cm
remove.data.end = 28 # in cm Remove data starting ## cm


column.path = paste(getwd(),Column.folder, Column.phase, sep = "/")

#Changing working directory so it is easy to import images 

setwd(column.path)
#getwd() #checking

###########################################################################
# Defining paths
###############################
plots.path = paste("/Users/gara009/OneDrive - PNNL/Documents/Column experiments/Methods_test/Plots.out", Column.folder,Column.phase, sep = "/")
rates.path = paste0("/Users/gara009/OneDrive - PNNL/Documents/Column experiments/Methods_test/Rates.out/", Column.folder)
processed.img.path = paste("/Users/gara009/OneDrive - PNNL/Documents/Column experiments/Methods_test/Processed.out", Column.folder,Column.phase, sep = "/")
img.path = paste("/Users/gara009/OneDrive - PNNL/Documents/Column experiments/Methods_test/Img.out",Column.folder,Column.phase, sep = "/")
crop.path = paste("/Users/gara009/OneDrive - PNNL/Documents/Column experiments/Methods_test/Crop.out",Column.folder,Column.phase, sep = "/")
matrix.path = paste("/Users/gara009/OneDrive - PNNL/Documents/Column experiments/Methods_test/Matrix.out",Column.folder,Column.phase, sep = "/")
input.path = "/Users/gara009/OneDrive - PNNL/Documents/Column experiments/Methods_test/Input"
plots.path.clean = paste("/Users/gara009/OneDrive - PNNL/Documents/Column experiments/Methods_test/Plots.out.clean", Column.folder,Column.phase, sep = "/")
img.path.clean = paste("/Users/gara009/OneDrive - PNNL/Documents/Column experiments/Methods_test/Img.out.clean",Column.folder,Column.phase, sep = "/")
###############################

#############
# Read in input calibration curve parameters and clamp location per column
#NOTE: The user must pre-populate column and tube number as well as the calibration values into the input file before running this code. Follow instructions in the first row of input file. 

#############################################################################
# Set up working directory and manually identify Column number

Column.number = 0
input.path = "/Users/gara009/OneDrive - PNNL/Documents/GitHub/Non-invasive_2D_Imaging_of_Oxygen_Concentration/Input"

#############
# Read in input calibration curve parameters and clamp location per column

input.files = list.files(path = input.path, pattern = ".csv", full.names = T)
input = read.csv(input.files[1], stringsAsFactors = F, skip = 1)

# Subset to the column that you are working with

input.column = subset(input, input$column == Column.number)

Ro = input.column$Ro
a = input.column$a
Ksv = input.column$Ksv



input.files = list.files(path = input.path, pattern = ".csv")
input.need = input.files[grep(pattern = Column.phase, x = input.files)]
input = read.csv(paste0(input.path,"/",input.need), stringsAsFactors = F)

# Subset to the column that you are working with

input.column = subset(input, input$column == Column.number)

Ro = input.column$Ro
a = input.column$a
Ksv = input.column$Ksv

# Input starting x-axis values for the clamps. 

clamp1 = input.column$clamp1 # in cm 
clamp2 =input.column$clamp2  # in cm
clamp3 = input.column$clamp3  #in cm
clamp4 = input.column$clamp4  #in cm
clamp5 = input.column$clamp5  #in cm
clamp.threshold = 1.85 # Approx thickness of the clamp and a safety factor
# Make it higher than 1.5 to be safe
##############################################################
# Read in the images
#############################################################

red.img = list.files(pattern = "R.tif") # Add a line that says to run images every hour rather than 5 min
green.img = list.files(pattern = "G1.tif")

# Checking consisency across number of images
if (length(red.img)!= length(green.img)){
  cat(red("Error: Number of red and green images don't match", "Please check!\n"))
}else{
for (i in 1:length(red.img)){
#for (i in 1:69){
#for (i in 70:length(red.img)){    

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

# result = (Ro - R)/(Ksv*(R-Ro*a))
result = (Ro - R.clean)/(Ksv*(R.clean-Ro*a))
#plot(result)


#cuts = seq(0,9.5, by = 0.5)
cuts = seq(0,DOmax, by = 0.5)

#col1 = colorRampPalette(c("black","purple","blue","green","yellow","orange","red"), interpolate= "linear") 

#This is a color friendly palette
col1 = colorRampPalette(c('#313695', '#4575b4','#74add1','#abd9e9','#e0f3f8','#ffffbf','#fee090','#fdae61','#f46d43','#d73027','#a50026'))
                        


#plot with defined breaks
#plot(result, breaks=cuts, col = col1((length(cuts)-1)), zlim = c(0,DOmax),xlim=c(xmin(result),xmax(result)), ylim=c(ymin(result), ymax(result)))

pixel.min.result = result[which.min(result)]
pixel.max.result = result[which.max(result)]


# Set extent of the raster to the length of the column
x.min.cm = input.column$x.min.in.cm #in cm
x.max.cm = input.column$x.max.in.cm #in cm
col.len = extent(x.min.cm,x.max.cm,0, ((x.max.cm-x.min.cm)*nrow(result))/ncol(result)) #These numbers were manually adjusted to get the 30 cm of the column, hopefully
result2 = setExtent(result,col.len)

#plot(result2, breaks=cuts, col = col1((length(cuts)-1)), zlim = c(0,DOmax),xlim=c(xmin(result2),xmax(result2)), ylim=c(0, ymax(result2)))
     #, yaxt = "n")

#####################################################
#Examine if your column needs to be slightly rotated if not comment from #A to A# out and uncomment #C
######################################################
#A Rotate image
rotate <- function(x, angle=0, resolution=res(x)) {
  y <- x; crs(y) <- "+proj=aeqd +ellps=sphere +lat_0=90 +lon_0=0"
  projectRaster(y, res=resolution,
                crs=paste0("+proj=aeqd +ellps=sphere +lat_0=90 +lon_0=", -angle))
}

 rotated.image = rotate(result2, -0.2)
 #plot(rotated.image,  breaks=cuts, col = col1((length(cuts)-1)), zlim = c(0,DOmax))
#B

# #C 
# rotated.image = result2
# # end

crop.extent = extent(input.column$crop.extent.x1,input.column$crop.extent.x2,input.column$crop.extent.y1,input.column$crop.extent.y2)
#crop.extent = extent(0,30,5,12)

result.cropped = crop(rotated.image,crop.extent)

pdf(paste0(crop.path,"/",name,"_",Column.folder,"_", Column.phase,"_Cropped",".pdf"))
plot(result.cropped, breaks=cuts, col = col1((length(cuts)-1)), zlim = c(0,DOmax),xlim=c(xmin(result.cropped),xmax(result.cropped)),ylim=c(ymin(result.cropped), ymax(result.cropped)))
dev.off()


# Crop only the area of interest. Removing 1 cm on the edges to reduce border effects 

crop.extent = extent(input.column$crop.extent.x1,input.column$crop.extent.x2,(input.column$crop.extent.y1+1),(input.column$crop.extent.y2-1)) #Area of interest
cropped.interest = crop(result.cropped,crop.extent)

#plot(cropped.interest, breaks=cuts, col = col1((length(cuts)-1)), zlim = c(0,DOmax),xlim=c(xmin(cropped.interest),xmax(cropped.interest)),ylim=c(ymin(cropped.interest), ymax(cropped.interest)))
#, interpolate = TRUE)


### Remove clamps

# ###################################################
# ##### TEST ONLY. Comment out and delete once all of the clamp locations have been figured out.
# 
# img.matrix = as.matrix(cropped.interest)
# 
# x.in.cm = seq(xres(cropped.interest),(ncol(cropped.interest)*xres(cropped.interest)), by = xres(cropped.interest))
# 
# do = rowMeans(t(img.matrix), na.rm = TRUE)
# 
# data = na.omit(as.data.frame(cbind(x.in.cm,do)))
# plot_ly(data = data, x = ~x.in.cm,y = ~do)

##################################################
#### End of test
############################################

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


# Create a smoothing window for raster layer 
#smallest spatial feature that I want to resolve, Let's say a mm \then we could look into 
#Calculate focal ("moving window") values for the neighborhood of focal cells using a matrix of weights, perhaps in combination with a function.

#window = matrix(1/9,nrow = 3, ncol = 3)
#test = focal(cropped.interest, window)

## or

## Disaggregate a RasterLayer to create a new RasterLayer with a higher resolution (smaller cells). The values in the new RasterLayer are the same as in the larger original cells unless you specify method="bilinear", in which case values are locally interpolated (using the resample function). The number in the function represents the integer. amount of disaggregation expressed as number of cells (horizontally and vertically).

#test = disaggregate(cropped.interest,5,method = "bilinear")

# png(paste0(img.path,"/",name,"_Processed",".png"))
# plot(test, breaks=cuts, col = col1((length(cuts)-1)), zlim = c(0,DOmax),xlab="Column Length (cm)", yaxt = "n") # This is to not print the y axis
# dev.off()


# Extract pixel values

# Idea, we could extract all the values in the width of the column and take an average of the values to get the profile going 

img.matrix = as.matrix(cropped.interest)
#img.matrix = as.matrix(test) # After smoothing. Looks terrible, check with Matt

x.in.cm = seq(xres(cropped.interest),(ncol(cropped.interest)*xres(cropped.interest)), by = xres(cropped.interest))

y.in.cm = seq(yres(cropped.interest),(nrow(cropped.interest)*yres(cropped.interest)), by = yres(cropped.interest))

Mean.DO.mg.per.L= rowMeans(t(img.matrix), na.rm = TRUE) #col means gives row means. We are calculating the mean of the columns

data = as.data.frame(cbind(x.in.cm,Mean.DO.mg.per.L))

write.csv(data,paste0(matrix.path,"/",name,"_Mean_DO_cm","_",Column.folder,"_",Column.phase,".csv"), row.names = F)

fit=lm(data$Mean.DO.mg.per.L~data$x.in.cm)
u=fit$coefficients
b=u[[1]] #Intercept
c=u[[2]] #rate mg/L min
r=summary(fit)$r.squared
p=summary(fit)$coefficients[4]  

pdf(paste0(plots.path,"/",name,"_",Column.folder,"_",Column.phase, ".pdf"))
plot(data$x.in.cm,data$Mean.DO.mg.per.L, xlab="Column Length (cm)",ylab="DO mg/L",cex.axis=1,cex.lab=1,col=c("black"),pch=19, ylim = c(0,DOmax))
abline(b, c)
legend("topright", bty="n", legend=paste("R2 =", format(summary(fit)$adj.r.squared, digits=4)))
legend("topleft", bty="n", legend=paste("p-value = ", format(summary(fit)$coefficients[4], digits=4)))
title(name)
dev.off()


pdf(paste0(plots.path,Folder,"/",name,"_",Column.folder,"_",Column.phase,"no_regression", ".pdf"))
plot(data$x.in.cm,data$Mean.DO.mg.per.L, xlab="Column Length (cm)",ylab="DO mg/L",cex.axis=1,cex.lab=1,col=c("black"),pch=19, ylim = c(0,DOmax))
title(name)
dev.off()

m <- matrix(NA, ncol = 6, nrow = 1)
rate=data.frame(m)
names(rate)[1]="ID" ;names(rate)[2]= "slope"; names(rate)[3]="rate.mg.per.L.per.cm"; names(rate)[4]="R2" ; names(rate)[5]="pvalue"; names(rate)[6]="phase"

rate$slope = as.numeric(c)
rate$rate.mg.per.L.per.cm=as.numeric(abs(c)) #in mg O2/L cm
rate$ID=name
rate$R2=as.numeric(abs(r))
rate$pvalue=p
rate$phase = Column.phase

write.csv(rate,paste0(rates.path,"/",name,"_Rate","_",Column.folder,"_",Column.phase,".csv"), row.names = F)
writeRaster(cropped.interest,paste0(processed.img.path,"/",name,"_Processed","_",Column.folder, "_",Column.phase,".tiff"), overwrite = T)

rm(rate)

######################################################
# Repeating the profile plots removing the first couple of cm at the beginning and at the end

# Removing data at the start
data.start = subset(data, data$x.in.cm >=  remove.data.start)
# Removing data at the end
data.end = subset(data.start, data.start <= remove.data.end )

# Making the plots
write.csv(data.end,paste0(matrix.path,"/",name,"_Mean_DO_cm","_",Column.folder,"_",Column.phase,".csv"), row.names = F)

fit=lm(data.end$Mean.DO.mg.per.L~data.end$x.in.cm)
u=fit$coefficients
b=u[[1]] #Intercept
c=u[[2]] #rate mg/L min
r=summary(fit)$r.squared
p=summary(fit)$coefficients[4]  

pdf(paste0(plots.path.clean,"/",name,"_",Column.folder,"_",Column.phase,"_clean", ".pdf"))
plot(data.end$x.in.cm,data.end$Mean.DO.mg.per.L, xlab="Column Length (cm)",ylab="DO mg/L",cex.axis=1,cex.lab=1,col=c("black"),pch=19, ylim = c(0,DOmax))
abline(b, c)
legend("topright", bty="n", legend=paste("R2 =", format(summary(fit)$adj.r.squared, digits=4)))
legend("topleft", bty="n", legend=paste("p-value = ", format(summary(fit)$coefficients[4], digits=4)))
title(name)
dev.off()


pdf(paste0(plots.path.clean,Folder,"/",name,"_",Column.folder,"_",Column.phase,"no_regression_clean", ".pdf"))
plot(data.end$x.in.cm,data.end$Mean.DO.mg.per.L, xlab="Column Length (cm)",ylab="DO mg/L",cex.axis=1,cex.lab=1,col=c("black"),pch=19, ylim = c(0,DOmax))
title(name)
dev.off()

m <- matrix(NA, ncol = 6, nrow = 1)
rate=data.frame(m)
names(rate)[1]="ID" ;names(rate)[2]= "slope"; names(rate)[3]="rate.mg.per.L.per.cm"; names(rate)[4]="R2" ; names(rate)[5]="pvalue"; names(rate)[6]="phase"

rate$slope = as.numeric(c)
rate$rate.mg.per.L.per.cm=as.numeric(abs(c)) #in mg O2/L cm
rate$ID=name
rate$R2=as.numeric(abs(r))
rate$pvalue=p
rate$phase = Column.phase

write.csv(rate,paste0(rates.path,"/Clean","/",name,"_Rate_Clean","_",Column.folder,"_",Column.phase,".csv"), row.names = F)

######################################################
# Also re-print cropped figure after removing cm from start to end

crop.extent.clean = extent(remove.data.start,remove.data.end,input.column$crop.extent.y1,input.column$crop.extent.y2)
#crop.extent = extent(0,30,5,12)

result.cropped.clean = crop(cropped.interest,crop.extent.clean)
# Plot area of interest again 
pdf(paste0(img.path.clean,"/",name,"_",Column.folder,"_", Column.phase,"_Processed_Clean",".pdf"))
plot(result.cropped.clean, breaks=cuts, col = col1((length(cuts)-1)), zlim = c(0,DOmax),xlim=c(xmin(result.cropped.clean),xmax(result.cropped.clean)),xlab="Column Length (cm)", yaxt = "n") # This is to not print the y axis
dev.off()


rm(data)
rm(name); rm(rotated.image)
}
}

