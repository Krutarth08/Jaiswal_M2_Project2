#Print name 
print("Plotting Basics : Krutarth Jaiswal")

#Install Packages
install.packages(c("plyr", "FSA", "FSAdata", "magrittr", "dplyr", "plotrix", "ggplot2","moments"))

#Import Libraries
library(FSAdata)
library(plyr)
library(FSA)
library(magrittr)
library(dplyr)
library(plotrix)
library(ggplot2)
library(moments)

#Load the BullTroutRML2 dataset
data <- BullTroutRML2
data

#Print the first and last 3 records from the dataset
headtail(data,n=3)

#Filter out all records except those from Harrison Lake
filtered_data <- filter(data,data["lake"]=="Harrison")
filtered_data

#Display the first and last 3 records from the filtered dataset
head(filtered_data,3)
tail(filtered_data,3)

#Display the structure of the filtered dataset
str(filtered_data)

# Display the summary of the filtered dataset and save it as <t>
t <- summary(filtered_data)
t

#Create a scatterplot for “age” (y variable) and “fl” (x variable)
plot(filtered_data$fl,filtered_data$age,xlab="Fork Length(mm)", ylab ="Age(yrs)",
     xlim = c(0,500), ylim = c(0,15),
     main = "Plot 1: Harrison Lake Trout",pch = 20) #pch = point shapes (http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r) 
``

#Plot an “Age” histogram
hist(filtered_data$age, xlab = "Age (yrs)", ylab = "Frequency",ylim = c(0,15), 
     main = "Plot 2: Harrison Fish Age Distribution", col.main = "cadetblue", col = "cadetblue")




#Create an overdense plot(Scatter plot)
era1 <- as.factor(era)
as.numeric(era1)

plot(filtered_data$fl,filtered_data$age,xlab="Fork Length(mm)", ylab ="Age(yrs)",
     xlim = c(0,500), ylim = c(0,15),
     main = "Plot 3: Harrison Density Shaded by Era",pch = as.numeric(era1)+18,col="Black" )

#Create a new object called “tmp” that includes the first 3 and last 3 records of the whole data set
tmp <- headtail(data,n=3L)
tmp

#Display the “era” column in the new “tmp” object
tmp["era"]

#Create a pchs vector with the argument values for + and x. Then create a cols vector with the two elements “red” and “gray60”
pchs <- c("+","x")
pchs
cols <- c("red","gray60")

#Convert the tmp object values to numeric values. Then create a numeric numEra object from the tmp$era object
numEra <- as.numeric(tmp$era)
numEra

#Associate the cols vector with the tmp era values
cols[tmp$era]

#Scatter plot with symbols and color 
plot(age~fl,data=filtered_data,xlab="Fork Length(mm)", ylab ="Age(yrs)",
     xlim = c(0,500), ylim = c(0,15),
     main = "Plot 4: Symbol & Color by Era",pch = pchs,col = cols)

#Scatter plot with dashed regression line
plot(filtered_data$fl,filtered_data$age,xlab="Fork Length(mm)", ylab ="Age(yrs)",
     xlim = c(0,500), ylim = c(0,15),
     main = "Plot 5: Regression Overlay",pch = pchs,col = cols)
abline(lm(age~fl,data=data),col="cadetblue",lty="dashed",lwd=2)

#Scatter plot with legend and regression line
plot(filtered_data$fl,filtered_data$age,xlab="Fork Length(mm)", ylab ="Age(yrs)",
     xlim = c(0,500), ylim = c(0,15), 
     main = "Plot 6: Legend Overlay",pch = pchs,col = cols)
abline(lm(age~fl,data=data),col="cadetblue",lty=1,lwd=2)
legend("topleft",inset=.05, legend = levels(data$era) ,pch = pchs, col=cols, cex=0.8,box.lty=0)



