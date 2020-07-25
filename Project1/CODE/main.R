library(tabulizer)
library(reshape)
library(ggplot2)

##########################################################################
location <- "/home/samroadie/Desktop/DA_Lab/CRS-2016.pdf"
urban<- extract_areas(location,pages=16,output="data.frame",header = F)
urban <- as.data.frame(urban)
colnames(urban) = c("Districts","Birth_reg","Birth_rate","Death_reg","Death_rate","Reg_infant_death","Still_birth_reg","Still_birth_rate")
urban
write.csv(urban,"/home/samroadie/Desktop/DA_Lab/urban.csv", row.names = FALSE)
urban <- read.csv("/home/samroadie/Desktop/DA_Lab/urban.csv", header = TRUE)
urban
#####################################################################################
rural<- extract_areas(location,pages=17,output="data.frame",header = F)
rural <- as.data.frame(rural)
rural
colnames(rural) = c("Districts","Birth_reg","Birth_rate","Death_reg","Death_rate","Reg_infant_death","Still_birth_reg","Still_birth_rate")
rural
write.csv(rural,"/home/samroadie/Desktop/DA_Lab/rural.csv", row.names = FALSE)
rural <- read.csv("/home/samroadie/Desktop/DA_Lab/rural.csv", header = TRUE)
rural
##########################################################################################
df2 <- extract_areas(location,pages=11,output="data.frame",header = F)
df2= df2[[1]]
df2 <- as.data.frame(df2)
colnames(df2) = c('year','nve_l','nve_s','nve_d','c_b','c_d','per_b','per_d')
write.csv(df2,"/home/samroadie/Desktop/DA_Lab/data.csv", row.names = FALSE)
data <- read.csv("/home/samroadie/Desktop/DA_Lab/data1.csv", sep = ",", header = TRUE)
data = tail(data,-11)
df2 = data
#NOTATION
#nve_l = no. of vital events registered for live birth
#nve_s = no. of vital events registered for still birth
#nve_d = no. of vital events registered for death

#c_b = crs vital rates for birth
#c_d = crs vital rates for death

#per_b = percentage of crs rates to srs rates birth
#per_d = percentage of crs rates to srs rates death


# Find Min 
min(data$nve_l)
min(data$nve_s)
min(data$nve_d)
min(data$c_b)
min(data$c_d)
min(data$per_b)
min(data$per_d)

# Find Max
max(data$nve_l)
max(data$nve_s)
max(data$nve_d)
max(data$c_b)
max(data$c_d)
max(data$per_b)
max(data$per_d)

# Find Mean.
mean(data$nve_l)
mean(data$nve_s)
mean(data$nve_d)
mean(data$c_b)
mean(data$c_d)
mean(data$per_b)
mean(data$per_d)

#Find Median
median(data$nve_l)
median(data$nve_s)
median(data$nve_d)
median(data$c_b)
median(data$c_d)
median(data$per_b)
median(data$per_d)

#Find Mode
mode(data$nve_l)
mode(data$nve_s)
mode(data$nve_d)
mode(data$c_b)
mode(data$c_d)
mode(data$per_b)
mode(data$per_d)

#Find Variance
var(data$nve_l)
var(data$nve_s)
var(data$nve_d)
var(data$c_b)
var(data$c_d)
var(data$per_b)
var(data$per_d)

#Find Standard Deviation
sd(data$nve_l)
sd(data$nve_s)
sd(data$nve_d)
sd(data$c_b)
sd(data$c_d)
sd(data$per_b)
sd(data$per_d)

#Find IQR
IQR(data$nve_l)
IQR(data$nve_s)
IQR(data$nve_d)
IQR(data$c_b)
IQR(data$c_d)
IQR(data$per_b)
IQR(data$per_d)

# CHECKING OUTLIER IN IN ALL DATASETS
out1 = boxplot(data)$out
out1
out2 = boxplot(rural)$out
out2
out3 = boxplot(urban)$out
out3
#################################################################################
## PLOTS OF VITAL EVENTS REGISTERED FROM 2011 TO 2016

# PLOT 1 - BAR PLOT OF Percentage of CRS rates to SRS rate for birth and death

mg <- data.frame(data$year,data$per_b,data$per_d)
colnames(mg) <- c("year","percentage_birth_crs/srs","percentage_death_crs/srs")
mg <- melt(mg,id.vars = "year")
ggplot(mg,aes(x = year , y = value,fill = variable))+ylab("Birth Rate") + geom_bar(stat = "identity",position = "dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle(" BAR PLOT OF Percentage of CRS rates to SRS rate for birth and death")

#PLOT 2 - Line Graph of Number of vital events registered still births VS Year

ggplot(data,aes(x = year,y = nve_s)) + geom_line(color = "blue") + ggtitle("Number of vital events registered still births VS Year")

#PLOT 3
#scatter plot of Number of vital events registered death VS Year

with(df2,plot(year,nve_d,main = "Number of vital events registered death VS Year"))

#PLOT 4 

boxplot(data$c_b , data$c_d, names = c(" crs vital rates for birth"," crs vital rates for death"))

## PLOTS FOR DISTRICTWISE REGISTERED BIRTHS, DEATHS, INFANT DEATHS, STILL BIRTHS AND RATES RURAL-2016

#PLOT 1 BOX PLOT OF STILL BIRTH RATE IN RURAL(HAS OUTLIER)

boxplot(rural$Still_birth_rate,main = "Still_birth_rate in rural")

#PLOT 2 PIE CHART OF Contribution to Reg_infant_death in urban

pie(urban$Reg_infant_death,urban$Reg_infant_death, main = "Contribution to Reg_infant_death in urban", radius =1,cex=0.6,col = rainbow(length(urban$Districts)))
legend("topright",urban$Districts , cex = 0.8,legend= urban$Districts,fill = rainbow(length(urban$Districts)))

#PLOT 3 Comparative plot for urban and rural

ubru <- data.frame(urban$Birth_rate,rural$Birth_rate,urban$Districts)
colnames(ubru) <- c("Urban","Rural","Districts")
mubru <- melt(ubru,id.vars = "Districts")
ggplot(mubru,aes(x = Districts , y = value,fill = variable))+ylab("Birth Rate") + geom_bar(stat = "identity",position = "dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))



urban



  
    
  
