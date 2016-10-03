##################################################################
############## Figura #05 ########################################
##################################################################

library(tidyr)
Web <- "https://www.dropbox.com/s/r0fxlta0tyqkwax/datatraining.txt?dl=1"
datosWeb <- read.table(Web, sep=",")  # o puede escribirse la dirección directamente dentro
head(datosWeb)  # un vistazo para ver cómo son los datos

matrix <- separate(datosWeb,date,c("day","hour"),sep=" ",convert=TRUE)
vector <- as.Date(datosWeb[,1])
days <- weekdays(vector)
weekStatus <- rep(1,length(days))
for(i in 1:length(days))
{
  if(days[i]== "sábado" || days[i]== "domingo")
  {
    weekStatus[i] <- 0
  }
  
}

matrix <- separate(matrix,hour,c("hora","minuto","segundo"),sep=":",convert=TRUE)
NSM <- rep(0,length(days))
hora <- matrix[,2]
minuto <- matrix[,3]
segundo <- matrix[,4]
num<-0
for(i in 1:length(days))
{
  num<- hora[i]*3600 + minuto[i]*60 + segundo[i]
  NSM[i] <- num
}

finalMatrix <- cbind(matrix[,5], matrix[,6], matrix[,7], matrix[,8], matrix[,9], NSM,weekStatus,matrix[,10])
colnames(finalMatrix) <- c("Temperature","Humidity","Light","CO2","HumidityRatio","NSM","WeekStatus","Occupancy")
finalMatrix <- as.data.frame(finalMatrix)

head(finalMatrix)

#########################################################################################
#########################################################################################
#http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software

correlationMatrix <- cor(finalMatrix[,1:7])
round(correlationMatrix, 2)

library("Hmisc")
pMatrix <- rcorr(as.matrix(finalMatrix[,1:7]))
pMatrix

library(corrplot)
corrplot(correlationMatrix, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)


