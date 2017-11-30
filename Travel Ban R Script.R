#Data on the Rocks
#February 2017 2/13/17
#Travel Ban

#REQUIRED PACKAGES
library(xlsx) #package to import xls files directly
library(pastecs) #summary stats function stat.desc
library(ggplot2)

#CUSTOM FUNCTIONS
Sp.Desc <- function(data)
{
  print(t(stat.desc(data))[,-c(2,3,6,7,11,14)])
}

DoR.Theme <- function(axis.text.size=16, axis.title.size=16, title.size=20, legend.position="none")
{
  theme(panel.grid.major = element_line(colour="grey90"), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line.x=element_line(colour="black"), axis.line.y=element_line(colour="black"),
        axis.title.x=element_text(colour = "black", size=axis.title.size), axis.title.y=element_text(colour = "black", size=axis.title.size),
        axis.text.x=element_text(colour = "black", size=axis.text.size), axis.text.y=element_text(colour = "black", size=axis.text.size),
        plot.title=element_text(colour = "black",size=title.size, face="bold", hjust=.5),
        axis.ticks=element_line(colour="black"), legend.position = legend.position, legend.key=element_blank())
}



#Immigration Time Trends from Banned Countries
#Census data downloaded from:
#http://www.migrationpolicy.org/programs/data-hub/us-immigration-trends#source

Immigrant <- data.frame(Year=c(1980,1990,2000,2010,2015), Im.Tot=c( 188629, 308469, 505234, 737096, 875661))
Immigrant$Year.2 <- Immigrant$Year^2

Im.Trend <- lm(Im.Tot ~ Year + Year.2, data=Immigrant)
summary(Im.Trend)
#summary(Im.Trend)$coefficients[,"Estimate"]

#Get predicted yearly values
Im.Pred <- data.frame(Year=seq(1980,2016,1))
Im.Pred$Year.2 <- Im.Pred$Year^2
Im.Pred$Im.Tot.P <- predict(Im.Trend, Im.Pred)
Im.Pred

#check predicted values fit
ggplot(data=Im.Pred, aes(x=Year, y=Im.Tot.P)) + geom_point() + 
  geom_point(data=Immigrant, aes(x=Year, y=Im.Tot), size=4, colour="red") + DoR.Theme()
#looks good

#Look at yearly change
#Lag by 1
Im.Pred$Im.Tot.P.Lag <- NA
for(i in 2:dim(Im.Pred)[1]){
  Im.Pred$Im.Tot.P.Lag[i] <- Im.Pred$Im.Tot.P[i-1]
}
Im.Pred

#Compute change
Im.Pred$Im.New <- Im.Pred$Im.Tot.P - Im.Pred$Im.Tot.P.Lag
Im.Pred

#Look at time trend of change
ggplot(data=Im.Pred, aes(x=Year, y=Im.New)) + geom_point() + DoR.Theme()
#By definition linear effect

#Now compute the number of doctors
#Total number of doctors in America from banned countries: 8243
#Average length of a doctor's career is 35 years. 
#98.79% of the immigrants from these nations came post 1980 so can basically just assume that all the doctors came after 1980
#Need to distribute new doctors over the years.
#Total immigrants in 2015 was 872597.9 for 8243 doctors
#ratio of total immigrants to doctors
Dr.Ratio <- 8243 / (Im.Pred$Im.Tot.P[36] - Im.Pred$Im.Tot.P[1]) #assuming all working doctors moved hear post 1980 so subtract out those pre-1980
Dr.Ratio

#Dr. Immigrants per year
Im.Pred$Dr.New <- Im.Pred$Im.New * Dr.Ratio
Im.Pred
#check sum
sum(Im.Pred$Dr.New, na.rm=T) - Im.Pred$Dr.New[37] #should be 8243 (Need to subtract out 2016)

#Total immigrant Doctors
Im.Pred$Dr.Tot <- NA
for (i in 2:dim(Im.Pred)[1]){
  Im.Pred$Dr.Tot[i] <- sum(Im.Pred$Dr.New[1:i], na.rm=T)
}
Im.Pred

#Years working
Im.Pred$Yrs.Work <- 2016 - Im.Pred$Year
Im.Pred

#Copute QALY by year
Im.Pred$Dr.QALY <- Im.Pred$Dr.New * Im.Pred$Yrs.Work * 17 #Num new doctors that year * years worked in US * QALY per doctor per year
Im.Pred

Dr.QALY.Tot <- sum(Im.Pred$Dr.QALY, na.rm=T)
Dr.QALY.Tot

#If ban had been implemented in 2000
#subset dataframe to 2000-2016
Im.Pred.post2000 <- subset(Im.Pred, Year>=2000)
Im.Pred.post2000$Dr.Tot <- Im.Pred.post2000$Dr.Tot - Im.Pred$Dr.Tot[20] #Need to subtract out the 1999 Dr.Total number to get tota number excluded
Im.Pred.post2000

Dr.QALY.Tot.post2000 <- sum(Im.Pred.post2000$Dr.QALY, na.rm=T)
Dr.QALY.Tot.post2000

#Terrorism Data
Terrorists <- data.frame(Year=c(2000,2006,2016), Terrorists=c(0,1,3))

#GRAPHING DATA----
Im.Plot.line <- ggplot(data=Im.Pred.post2000, aes(x=Year, y=Dr.Tot)) + 
  geom_line(size=2, colour="#668cff") + 
  annotate("text", label="Doctors", x=2008.5, y=3600, colour="#668cff", size=6, fontface="bold", hjust=0) + 
  geom_line(data=Terrorists, aes(x=Year, y=Terrorists), size=2, colour="#cc0000") +
  annotate("text", label="Terrorists", x=2013.2, y=250, colour="#cc0000", size=6, fontface="bold", hjust=.5, vjust=.5) + 
  scale_x_continuous(limits=c(2000,2017), breaks=seq(2000,2016,2), expand = c(0,0)) +
  scale_y_continuous("Banned Immigrants", limits=c(0,5200), breaks=seq(1000,5000,1000), expand = c(0,0)) +
  ggtitle("Terrorists vs. Doctors Banned") +
  DoR.Theme()
Im.Plot.line

ggsave(Im.Plot.line, filename="Im.Plot.line.png", width = 8, height=7, dpi=500)

#EXPORT DATA TO POST----
write.csv(Im.Pred, file="Immigration Data.csv", row.names=F)
write.csv(Im.Pred.post2000, file="Immigration Data Post 2000.csv", row.names=F)


#EXTRA----
#Plot the number of new doctors versus new terrorists

Im.Plot <- ggplot(data=Im.Pred.post2000, aes(x=Year, y=Dr.Tot)) + 
  geom_area(fill="#668cff") + 
  annotate("text", label="Doctors", x=2008.5, y=3600, colour="#668cff", size=6, fontface="bold", hjust=0) + 
  geom_area(data=Terrorists, aes(x=Year, y=Terrorists), fill="#cc0000") +
  annotate("rect", fill="white", xmin = 2011.4, xmax = 2015, ymin = 175, ymax = 500) +
  annotate("text", label="Terrorists", x=2013.2, y=350, colour="#cc0000", size=6, fontface="bold", hjust=.5, vjust=.5) + 
  scale_x_continuous(limits=c(2000,2017), breaks=seq(2000,2016,2), expand = c(0,0)) +
  scale_y_continuous("Banned Immigrants", limits=c(0,5200), breaks=seq(1000,5000,1000), expand = c(0,0)) +
  ggtitle("Terrorists vs. Doctors Banned") +
  DoR.Theme()
Im.Plot

ggsave(Im.Plot, filename="Im.Plot.png", width = 8, height=7, dpi=500)

#get rid of terrorists label
Im.Plot.NoT <- ggplot(data=Im.Pred.post2000, aes(x=Year, y=Dr.Tot)) + 
  geom_area(fill="#668cff") + 
  annotate("text", label="Doctors", x=2008.5, y=3600, colour="#668cff", size=6, fontface="bold", hjust=0) + 
  geom_area(data=Terrorists, aes(x=Year, y=Terrorists), fill="#cc0000") +
  scale_x_continuous(limits=c(2000,2017), breaks=seq(2000,2016,2), expand = c(0,0)) +
  scale_y_continuous("Banned Immigrants", limits=c(0,5200), breaks=seq(1000,5000,1000), expand = c(0,0)) +
  ggtitle("Terrorists vs. Doctors Banned") +
  DoR.Theme()
Im.Plot.NoT

ggsave(Im.Plot.NoT, filename="Im.Plot.NoT.png", width = 8, height=7, dpi=500)

#Plot QALY -- DOESNT WORK----
#QALY plot too large to visualize, even when the doctor effect is divided into lives
#Just plot new doctors. 
#QALY too big a difference to visualize
#An average american lifespan is about 44 QALY based on https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3828687/
#Divide QALY effect by 44: 647499/44 = 14715.89
#Terrorism box dimensions and location 
T.x <- .1 
T.y <- T.x #4 for the difference in x-y axes, and 2/7 for the final image dimensions
T.loc <- c(0, 0)

#Doctor box dimensions
D.Rect <- 3 #times as high as wide
D.x <- sqrt(14715.89)*T.x / D.Rect #average quality adjusted life expectancy
D.y <- sqrt(14715.89)*T.y * D.Rect
D.loc <- c(.5, 0)

#boxes for QALY
QALY.Plot <- ggplot() + 
  annotate("rect", fill="#cc0000", xmin = T.loc[1], xmax = T.loc[1]+T.x, ymin = T.loc[2], ymax = T.loc[2]+T.y) +
  annotate("rect", fill="#668cff", xmin = D.loc[1], xmax = D.loc[1]+D.x, ymin = D.loc[2], ymax = D.loc[2]+D.y) +
  #scale_x_continuous(limits=c(0,25)) +
  #scale_y_continuous(limits=c(0,100)) +
  DoR.Theme() + theme(axis.title.x=element_blank(),  axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
                      axis.title.y=element_blank(),  axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.line.y=element_blank())
QALY.Plot

ggsave(QALY.Plot, filename="QALY.Plot.png", width = 2, height=7, dpi=500)

