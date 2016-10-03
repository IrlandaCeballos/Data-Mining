##################################################################
############## Figura #03 ########################################
##################################################################
Web <- "https://www.dropbox.com/s/r0fxlta0tyqkwax/datatraining.txt?dl=1"
datatesting <- read.table(Web,header=TRUE,sep=",")
head(datatesting)

#setwd("C:/Users/Irlanda Ceballos/Desktop/DISCO DURO/UNIVERSIDAD/Magíster en Gestión de Operaciones/12vo Semestre MGO/Minería de datos/Control #03")
#datatesting <- read.table("datatest.txt",header=TRUE,sep=",")
#head(datatesting)

library(ggplot2)
library(grid)
library(gridExtra)
library(scales)

str(datatesting)
datatesting$Occupancy  <- as.factor(datatesting$Occupancy)
datatesting$date <- as.POSIXct(datatesting$date,tz="UTC") 
datatesting$Occupancy  <- as.factor(datatesting$Occupancy)
str(datatesting)
prop.table(table(datatesting$Occupancy))
summary(datatesting)

# Creación de las 6 series de tiempo

pushViewport(viewport(layout = grid.layout(6, 1)))

plot1 <- ggplot(datatesting,aes(date))+geom_line(color="Red",aes(y=Temperature))+ylab("Temperature")+xlab("Time")+
  scale_x_datetime(breaks=date_breaks("60 min"),labels=date_format("%H:%M"),
                   limits=as.POSIXct(c("2015-02-05 8:00","2015-02-06 9:00"),tz="GMT"))+
  theme(axis.text.x=element_text(angle=90,hjust=1))        

plot2 <- ggplot(datatesting,aes(date))+geom_line(color="Blue",aes(y=Humidity))+ylab("Humidity")+xlab("Time")+
  scale_x_datetime(breaks=date_breaks("60 min"),labels=date_format("%H:%M"),
                   limits=as.POSIXct(c("2015-02-05 8:00","2015-02-06 9:00"),tz="GMT"))+
  theme(axis.text.x=element_text(angle=90,hjust=1))        

plot3 <- ggplot(datatesting,aes(date))+geom_line(color="deepskyblue1",aes(y=HumidityRatio))+ylab("HumidityRatio")+xlab("Time")+
  scale_x_datetime(breaks=date_breaks("60 min"),labels=date_format("%H:%M"),
                   limits=as.POSIXct(c("2015-02-05 8:00","2015-02-06 9:00"),tz="GMT"))+
  theme(axis.text.x=element_text(angle=90,hjust=1))        

plot4 <- ggplot(datatesting,aes(date))+geom_line(color="Green",aes(y=CO2))+ylab("CO2 (ppm)")+xlab("Time")+
  scale_x_datetime(breaks=date_breaks("60 min"),labels=date_format("%H:%M"),
                   limits=as.POSIXct(c("2015-02-05 8:00","2015-02-06 9:00"),tz="GMT"))+
  theme(axis.text.x=element_text(angle=90,hjust=1))        

plot5 <- ggplot(datatesting,aes(date))+geom_line(color="gold4",aes(y=Light))+ylab("Light (Lux)")+xlab("Time")+
  scale_x_datetime(breaks=date_breaks("60 min"),labels=date_format("%H:%M"),
                   limits=as.POSIXct(c("2015-02-05 8:00","2015-02-06 9:00"),tz="GMT"))+
  theme(axis.text.x=element_text(angle=90,hjust=1))        

plot6 <- ggplot(datatesting,aes(date))+geom_line(color="Black",aes(y=as.numeric(Occupancy)))+ylab("Occupancy")+xlab("Time")+
  scale_x_datetime(breaks=date_breaks("60 min"),labels=date_format("%H:%M"),
                   limits=as.POSIXct(c("2015-02-05 8:00","2015-02-06 9:00"),tz="GMT"))+
  theme(axis.text.x=element_text(angle=90,hjust=1))  

print(plot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot2, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(plot3, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(plot4, vp = viewport(layout.pos.row = 4, layout.pos.col = 1))
print(plot5, vp = viewport(layout.pos.row = 5, layout.pos.col = 1))
print(plot6, vp = viewport(layout.pos.row = 6, layout.pos.col = 1))

plot1 <- ggplot_gtable(ggplot_build(plot1))
plot2 <- ggplot_gtable(ggplot_build(plot2))
plot3 <- ggplot_gtable(ggplot_build(plot3))
plot4 <- ggplot_gtable(ggplot_build(plot4))
plot5 <- ggplot_gtable(ggplot_build(plot5))
plot6 <- ggplot_gtable(ggplot_build(plot6))

maxWidth = unit.pmax(plot1$widths[2:3],plot2$widths[2:3],plot3$widths[2:3], plot4$widths[2:3],plot5$widths[2:3],plot6$widths[2:3])

plot1$widths[2:3] <- maxWidth
plot2$widths[2:3] <- maxWidth
plot3$widths[2:3] <- maxWidth
plot4$widths[2:3] <- maxWidth
plot5$widths[2:3] <- maxWidth
plot6$widths[2:3] <- maxWidth

grid.arrange(plot1, plot2,plot3, plot4,plot5, plot6,ncol=1)
# Run the commented line if you want to safe the plot in a png file
# png(file="occupancy2.png",width=1250,height=1200,res=75)
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6,ncol=1)
