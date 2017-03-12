df <- read.csv("../../sensorData.csv", stringsAsFactors=F)

serhan = df[df$statusId %in% c("131", "132") & df$sensorName == 'Linear Acceleration',]
sinan = df[df$statusId %in% c("133", "134") & df$sensorName == 'Linear Acceleration',]
#sinanda 135 de var, esit olsun diye dahil etmedim.

total = df[df$statusId %in% c("131","132","133", "134","135") & df$sensorName == 'Linear Acceleration',]

#serhan
serhanValues <- serhan$value
filteredSerhanValues = gsub("\\[|\\]", "", serhanValues)
finalSerhan = structure(data.frame(matrix(unlist(strsplit(filteredSerhanValues,",")),length(filteredSerhanValues),3,T)), names=c("X","Y","Z")) 

#sinan
sinanValues <- sinan$value
filteredSinanValues = gsub("\\[|\\]", "", sinanValues)
finalSinan = structure(data.frame(matrix(unlist(strsplit(filteredSinanValues,",")),length(filteredSinanValues),3,T)), names=c("X","Y","Z")) 

#total
totalValues <- total$value
filteredTotalValues = gsub("\\[|\\]", "", totalValues)
finalTotal = structure(data.frame(matrix(unlist(strsplit(filteredTotalValues,",")),length(filteredTotalValues),3,T)), names=c("X","Y","Z")) 


#prepare data for serhan
preDataX = data.matrix(finalSerhan$X, rownames.force = NA)
preDataY = data.matrix(finalSerhan$Y, rownames.force = NA)
preDataZ = data.matrix(finalSerhan$Z, rownames.force = NA)
magn = sqrt(as.numeric(preDataX)^2 + as.numeric(preDataY)^2 + as.numeric(preDataZ)^2)
finalSerhan["Magnitude"] = magn

#prepare data for sinan
preDataX = data.matrix(finalSinan$X, rownames.force = NA)
preDataY = data.matrix(finalSinan$Y, rownames.force = NA)
preDataZ = data.matrix(finalSinan$Z, rownames.force = NA)
magn = sqrt(as.numeric(preDataX)^2 + as.numeric(preDataY)^2 + as.numeric(preDataZ)^2)
finalSinan["Magnitude"] = magn

#summary
summary(finalSerhan)
var(finalTotal$Magnitude)


#newFinalDf = cbind(final70DFMerve,mag = (as.numeric(as.character(final70DFMerve$X))^2 + (as.numeric(as.character(final70DFMerve$Y))^2)))

#two histograms on the same figure
hist(finalSerhan$Magnitude, col=rgb(1,0,0,0.5),xlab="Linear Accelerometer Magnitude", ylim=c(0,25000))
hist(finalSinan$Magnitude, col=rgb(0,0,1,0.5), add=T, ylim=c(0,0,25000))

#two histograms on different figures
hist(finalSerhan$Magnitude, col=rgb(1,0,0,0.5),xlab="Linear Accelerometer Magnitude of Total",ylim=c(0,25000))
hist(finalSinan$Magnitude, col=rgb(1,1,0,0.5),xlab="Linear Accelerometer Magnitude of Serhan",ylim=c(0,25000))

#density Serhan
hist(finalSerhan$Magnitude, col=rgb(1,0,0,0.5),xlab="Linear Accelerometer Magnitude",prob=T)
meanSerhan = mean(finalSerhan$Magnitude, na.rm = TRUE)
#curve
curve(dnorm(x, mean=meanSerhan, sd=sd(finalSerhan$Magnitude)), add=TRUE)

#density Sinan
hist(finalSinan$Magnitude, col=rgb(1,0,0,0.5),xlab="Linear Accelerometer Magnitude",prob=T)
meanSinan = mean(finalSinan$Magnitude, na.rm = TRUE)
#curve
curve(dnorm(x, mean=meanSinan, sd=sd(finalSinan$Magnitude)), add=TRUE)

#ECDF of Both Sinan And Serhan
plot(ecdf(finalSerhan$Magnitude), col=rgb(0,0,1,1/4),xlim=c(1,10)) 
plot(ecdf(finalSinan$Magnitude), col=rgb(1,0,0,1/4),xlim=c(1,10),add=T)

#QQNORM
qqnorm(finalSerhan$Magnitude)
qqnorm(finalSinan$Magnitude)

#BoxPlot
boxplot(finalSerhan$Magnitude, main="Serhan", ylim=c(0,5))
summary(finalSerhan$Magnitude)

boxplot(finalSinan$Magnitude, main="Sinan", ylim=c(0,5))
summary(finalSinan$Magnitude)

#with par (all in one)
par(mfrow=c(2,2))
hist(finalSerhan$Magnitude, col=rgb(1,0,0,0.5),xlab="Linear Accelerometer Magnitude of Total",ylim=c(0,25000))
hist(finalSinan$Magnitude, col=rgb(1,1,0,0.5),xlab="Linear Accelerometer Magnitude of Serhan",ylim=c(0,25000))
plot(ecdf(finalSerhan$Magnitude), col=rgb(0,0,1,1/4),xlim=c(1,10)) 
plot(ecdf(finalSinan$Magnitude), col=rgb(1,0,0,1/4),xlim=c(1,10))

#boxplot array
boxplot(finalSerhan$Magnitude, finalSinan$Magnitude, names=c("Serhan","Sinan"))

#stripchart
#lengthleri ayni olmadigindan hata veriyor.
stripchart(finalSerhan$Magnitude ~ finalSinan$Magnitude,method="jitter", jitter=0.3)

#standard deviation
sd(finalSerhan$Magnitude)
sd(finalSinan$Magnitude)



