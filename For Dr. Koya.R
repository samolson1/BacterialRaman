#set up the files to be brought in (set working directory first)
setwd("/Users/samuelolson/Desktop/Spectral Data")
pacman::p_load(dplyr,reshape2,ggplot2,caret,openxlsx,tidyverse,randomForest)

###################
###################

#for July 15th
#compile files into R, change date as needed for each
filelist.a = list.files(path="/Users/samuelolson/Desktop/Spectral Data/Bacteria/MRSA/July 15th, 2019/SeraphSpline", pattern = ".*.txt", full.names = T)
datalist.a = lapply(filelist.a, FUN=read.table, header=F)

#set up the dataframes
big.df.a <- as.data.frame(datalist.a) #big.df assembles dataframe of all spectral information (200 shots)
sml.df.a <- big.df.a[ , -seq(1, ncol(big.df.a), 2)] #small.df assembles dataframe with only the Y values (every other column deleted)
new.df.a <- data.frame(big.df.a[[1]], sml.df.a) #new.df is a dataframe with one column on the X values (first colums), followed by all Y values
avg.df.a <- data.frame(big.df.a[[1]], rowMeans(sml.df.a)) #avg.df is a two column dataframe with the X values and the row means of all Y values
  names(avg.df.a) <- c("X", "Y")
mltn.df.a <- melt(new.df.a, id.vars = 1) #mltn.df is essentially just new.df but in long format so that it can be plotted more easily
  names(mltn.df.a) <- c("X", "SAMPLE", "Y")

#draw plots
#this one draws the averaged line laid on top of all 200 shots
#adjust x and y limits and breaks as needed
avgall.pl.a <- ggplot() +
  theme_light() + 
  geom_line(mltn.df.a, mapping = aes(x = X, y = Y), color = "grey20", size = 0.009) +
  geom_line(avg.df.a, mapping = aes(x = X, y = Y), color = "blue", size = 0.3) +
  scale_x_continuous(name = "wavenumber", limits = c(500,1800), breaks = seq(500, 1800, by = 100)) +
  scale_y_continuous(name = "intensity", limits = c(0,19000), breaks = seq(0,19000, by = 1000))
print(avgall.pl.a)

###################
###################

#for July 12th, 2019
filelist.b = list.files(path = "/Users/samuelolson/Desktop/Spectral Data/Bacteria/MRSA/July 12th, 2019/SeraphSpline", pattern = ".*.txt", full.names = T)
datalist.b = lapply(filelist.b, FUN = read.table, header = F)
big.df.b <- as.data.frame(datalist.b) 
sml.df.b <- big.df.b[ , -seq(1, ncol(big.df.b), 2)] 
new.df.b <- data.frame(big.df.b[[1]], sml.df.b) 
avg.df.b <- data.frame(big.df.b[[1]], rowMeans(sml.df.b)) 
  names(avg.df.b) <- c("X", "Y")
mltn.df.b <- melt(new.df.b, id.vars = 1)
  names(mltn.df.b) <- c("X", "SAMPLE", "Y")
#plot B
avgall.pl.b <- ggplot() +
    theme_light() + 
    geom_line(mltn.df.b, mapping = aes(x = X, y = Y), color = "grey20", size = 0.009) +
    geom_line(avg.df.b, mapping = aes(x = X, y = Y), color = "blue", size = 0.3) +
    scale_x_continuous(name = "wavenumber", limits = c(500,1800), breaks = seq(500, 1800, by = 100)) +
    scale_y_continuous(name = "intensity", limits = c(0,15000), breaks = seq(0,15000, by = 1000))
print(avgall.pl.b)


###################
###################

#for July 9th, 2019
filelist.c = list.files(path = "/Users/samuelolson/Desktop/Spectral Data/Bacteria/MRSA/July 9th, 2019/SeraphSpline", pattern = ".*.txt", full.names = T)
datalist.c = lapply(filelist.c, FUN = read.table, header = F)
big.df.c <- as.data.frame(datalist.c) 
sml.df.c <- big.df.c[ , -seq(1, ncol(big.df.c), 2)] 
new.df.c <- data.frame(big.df.c[[1]], sml.df.c) 
avg.df.c <- data.frame(big.df.c[[1]], rowMeans(sml.df.c)) 
names(avg.df.c) <- c("X", "Y")
mltn.df.c <- melt(new.df.c, id.vars = 1)
names(mltn.df.c) <- c("X", "SAMPLE", "Y")
#plot C
avgall.pl.c <- ggplot() +
  theme_light() + 
  geom_line(mltn.df.c, mapping = aes(x = X, y = Y), color = "grey20", size = 0.009) +
  geom_line(avg.df.c, mapping = aes(x = X, y = Y), color = "blue", size = 0.3) +
  scale_x_continuous(name = "wavenumber", limits = c(500,1800), breaks = seq(500, 1800, by = 100)) +
  scale_y_continuous(name = "intensity", limits = c(0,15000), breaks = seq(0,15000, by = 1000))
print(avgall.pl.c)

###################
###################

#for July 8th, 2019
filelist.D = list.files(path = "/Users/samuelolson/Desktop/Spectral Data/Bacteria/MRSA/July 8th, 2019/SeraphSpline", pattern = ".*.txt", full.names = T)
datalist.D = lapply(filelist.D, FUN = read.table, header = F)
big.df.D <- as.data.frame(datalist.D) 
sml.df.D <- big.df.D[ , -seq(1, ncol(big.df.D), 2)] 
new.df.D <- data.frame(big.df.D[[1]], sml.df.D) 
avg.df.D <- data.frame(big.df.D[[1]], rowMeans(sml.df.D)) 
names(avg.df.D) <- c("X", "Y")
mltn.df.D <- melt(new.df.D, id.vars = 1)
names(mltn.df.D) <- c("X", "SAMPLE", "Y")
#plot D
avgall.pl.D <- ggplot() +
  theme_light() + 
  geom_line(mltn.df.D, mapping = aes(x = X, y = Y), color = "grey20", size = 0.009) +
  geom_line(avg.df.D, mapping = aes(x = X, y = Y), color = "blue", size = 0.3) +
  scale_x_continuous(name = "wavenumber", limits = c(500,1800), breaks = seq(500, 1800, by = 100)) +
  scale_y_continuous(name = "intensity", limits = c(0,15000), breaks = seq(0,15000, by = 1000))
print(avgall.pl.D)

###################
###################

#for March 27th, 2019 (SRO did not run - full conditions unknown)
filelist.E = list.files(path = "/Users/samuelolson/Desktop/Spectral Data/Bacteria/Old MRSA/MRSA4330_4750uLPlus250uLWater_AlCoated_032719/SeraphSpline", pattern = ".*.txt", full.names = T)
datalist.E = lapply(filelist.E, FUN = read.table, header = F)
big.df.E <- as.data.frame(datalist.E) 
sml.df.E <- big.df.E[ , -seq(1, ncol(big.df.E), 2)] 
new.df.E <- data.frame(big.df.E[[1]], sml.df.E) 
avg.df.E <- data.frame(big.df.E[[1]], rowMeans(sml.df.E)) 
names(avg.df.E) <- c("X", "Y")
mltn.df.E <- melt(new.df.E, id.vars = 1)
names(mltn.df.E) <- c("X", "SAMPLE", "Y")
#plot E
avgall.pl.E <- ggplot() +
  theme_light() + 
  geom_line(mltn.df.E, mapping = aes(x = X, y = Y), color = "grey20", size = 0.009) +
  geom_line(avg.df.E, mapping = aes(x = X, y = Y), color = "blue", size = 0.3) +
  scale_x_continuous(name = "wavenumber", limits = c(500,1800), breaks = seq(500, 1800, by = 100)) +
  scale_y_continuous(name = "intensity", limits = c(0,15000), breaks = seq(0,15000, by = 1000))
print(avgall.pl.E)

###################
###################

#for March 29th, 2019 (SRO did not run - full conditions unknown)
filelist.F = list.files(path = "/Users/samuelolson/Desktop/Spectral Data/Bacteria/Old MRSA/MRSA43300_4750uLPlus250uLPBS_AlCoated_032919/SeraphSpline", pattern = ".*.txt", full.names = T)
datalist.F = lapply(filelist.F, FUN = read.table, header = F)
big.df.F <- as.data.frame(datalist.F) 
sml.df.F <- big.df.F[ , -seq(1, ncol(big.df.F), 2)] 
new.df.F <- data.frame(big.df.F[[1]], sml.df.F) 
avg.df.F <- data.frame(big.df.F[[1]], rowMeans(sml.df.F)) 
names(avg.df.F) <- c("X", "Y")
mltn.df.F <- melt(new.df.F, id.vars = 1)
names(mltn.df.F) <- c("X", "SAMPLE", "Y")
#plot F
avgall.pl.F <- ggplot() +
  theme_light() + 
  geom_line(mltn.df.F, mapping = aes(x = X, y = Y), color = "grey20", size = 0.009) +
  geom_line(avg.df.F, mapping = aes(x = X, y = Y), color = "blue", size = 0.3) +
  scale_x_continuous(name = "wavenumber", limits = c(500,1800), breaks = seq(500, 1800, by = 100)) +
  scale_y_continuous(name = "intensity", limits = c(0,15000), breaks = seq(0,15000, by = 1000))
print(avgall.pl.F)

###################
###################

mrph.df <- read.csv("SamsData_Normalized_Morphology.csv") #uploads the file into a data frame
names(mrph.df) = gsub(pattern = "X", replacement = "", x = names(mrph.df)) #remove the X that sits in front of the numbers in the top row
trimmed.df  <- subset(mrph.df, grp == 4 | grp == 6 | grp == 7 | grp == 8 | grp == 11) #selects the groups of interest based on the group number you assigned in Excel
#for this data set, the control (water only) is 11, then 4,6,7,and 8 are all different trials of MRSA

set.seed(21) #set point of randomization

#defining the 70/30 train/test split
inTraining <- createDataPartition(trimmed.df$grp, p = 0.70, list = FALSE)
training <- trimmed.df[inTraining, ]
testing <- trimmed.df[-inTraining, ]

#clean up the data to use in the random forest. this needs dplyr package
cleanshave <- trimmed.df%>%
  mutate_if(is.numeric, as.factor)

#build random forest
data.imputed <- rfImpute(grp ~ ., data = cleanshave, iter = 6)
model <- randomForest(grp ~ ., data = data.imputed, proximity = TRUE)