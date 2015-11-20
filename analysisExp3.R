#################################################
##  R analysis script for Experiment 3 of 
##  Faulkenberry, Cruise, Lavro, & Shaki (in press),
##  to appear in Acta Psychologica
####################################################

library(ggplot2)

rawData<-read.table("leftTrajectoriesExp3.csv",sep=",",header=TRUE)

# clean up data
dataStep3<-subset(rawData,subset=error!=1) # remove errors

meanRT<-mean(dataStep3$RT)
sdRT<-sd(dataStep3$RT)
dataLeft<-subset(dataStep3,subset=RT<meanRT+3*sdRT & RT>meanRT-3*sdRT) # remove 3 SD outliers
attach(dataLeft)

rawData<-read.table("rightTrajectoriesExp3.csv",sep=",",header=TRUE)
# clean up data
dataStep3<-subset(rawData,subset=error!=1) # remove errors

meanRT<-mean(dataStep3$RT)
sdRT<-sd(dataStep3$RT)
dataRight<-subset(dataStep3,subset=RT<meanRT+3*sdRT & RT>meanRT-3*sdRT) # remove 3 SD outliers
attach(dataRight)

# plot hand trajectories

dataLeftCongruent<-subset(dataLeft,condition==1)
dataLeftIncongruent<-subset(dataLeft,condition==2)
dataRightCongruent<-subset(dataRight,condition==1)
dataRightIncongruent<-subset(dataRight,condition==2)


xCoords=rep(0,404)
yCoords=rep(0,404)
side=rep(0,404)
condition=rep(0,404)

for (i in 1:101){
  xCoords[i]=mean(dataLeftCongruent[,i+22])
  yCoords[i]=mean(dataLeftCongruent[,i+123])
  side[i]="left"
  condition[i]="congruent"
    
  xCoords[i+101]=mean(dataLeftIncongruent[,i+22])
  yCoords[i+101]=mean(dataLeftIncongruent[,i+123])
  side[i+101]="left"
  condition[i+101]="incongruent"
  
  xCoords[i+202]=mean(dataRightCongruent[,i+22])
  yCoords[i+202]=mean(dataRightCongruent[,i+123])
  side[i+202]="right"
  condition[i+202]="congruent"
  
  xCoords[i+303]=mean(dataRightIncongruent[,i+22])
  yCoords[i+303]=mean(dataRightCongruent[,i+123])
  side[i+303]="right"
  condition[i+303]="incongruent"
}

library("ggplot2")
trajectoryData=data.frame(xCoords,yCoords,side,condition)
plot=ggplot(trajectoryData,aes(x=xCoords,y=yCoords,group=condition))+xlim(-1,1)+ylim(0,1.5)
paths=geom_path(aes(linetype=condition),size=1.3)
labels=labs(x="x-coordinates",y="y-coordinates")
faceting=facet_grid(.~side)
stripFormat=theme(strip.text=element_text(face="bold",size=rel(1.5)))
legendFormat=theme(legend.title=element_text(face="bold",size=rel(1.5)),legend.text=element_text(size=rel(1.5)))
axesFormat=theme(axis.title=element_text(size=rel(1.4)))


basePlot=plot+paths+labels+faceting+stripFormat+legendFormat+axesFormat
basePlot+labs(colour="Condition")+theme(legend.position=c(0.5,0.5))+theme(legend.background=element_rect(fill="white",colour="black"))

# notes: export as 954 x 461


# find out when x-coordinates differ significantly
# x variables go from 23rd column to 124th column

# left trajectories

for (i in 23:123){
  test=t.test(dataLeftCongruent[,i],dataLeftIncongruent[,i])
  cat(sprintf('X_%i, p=%f \n',i-22,test$p.value))
}

# differed from 26th to 84th timestep

# right trajectories

for (i in 23:123){
  test=t.test(dataRightCongruent[,i],dataRightIncongruent[,i])
  cat(sprintf('X_%i, p=%f \n',i-22,test$p.value))
}

# differed from 26th to 90th timestep


library(reshape)

# PERFORMANCE MEASURES
# RT
# left side
agg=aggregate(RT~subject+condition,data=dataLeft,FUN="mean") # RT performance data aggregated by subject
t.test(agg$RT[agg$condition==1],agg$RT[agg$condition==2],paired=TRUE)
mean(agg$RT[agg$condition==1])
mean(agg$RT[agg$condition==2])

m=mean(agg$RT[agg$condition==2]-agg$RT[agg$condition==1])
s=sd(agg$RT[agg$condition==2]-agg$RT[agg$condition==1])
m/s



# right side
agg=aggregate(RT~subject+condition,data=dataRight,FUN="mean") # RT performance data aggregated by subject
t.test(agg$RT[agg$condition==1],agg$RT[agg$condition==2],paired=TRUE)
mean(agg$RT[agg$condition==1])
mean(agg$RT[agg$condition==2])

m=mean(agg$RT[agg$condition==2]-agg$RT[agg$condition==1])
s=sd(agg$RT[agg$condition==2]-agg$RT[agg$condition==1])
m/s


# init
# left side
agg=aggregate(init.time~subject+condition,data=dataLeft,FUN="mean") # RT performance data aggregated by subject
t.test(agg$init.time[agg$condition==1],agg$init.time[agg$condition==2],paired=TRUE)
mean(agg$init.time[agg$condition==1])
mean(agg$init.time[agg$condition==2])

m=mean(agg$init.time[agg$condition==2]-agg$init.time[agg$condition==1])
s=sd(agg$init.time[agg$condition==2]-agg$init.time[agg$condition==1])
m/s

# init
# right side
agg=aggregate(init.time~subject+condition,data=dataRight,FUN="mean") # RT performance data aggregated by subject
t.test(agg$init.time[agg$condition==1],agg$init.time[agg$condition==2],paired=TRUE)
mean(agg$init.time[agg$condition==1])
mean(agg$init.time[agg$condition==2])

m=mean(agg$init.time[agg$condition==2]-agg$init.time[agg$condition==1])
s=sd(agg$init.time[agg$condition==2]-agg$init.time[agg$condition==1])
m/s


# movement duration
# left side
agg=aggregate(RT-init.time~subject+condition,data=dataLeft,FUN="mean") # RT performance data aggregated by subject
names(agg)<-c("subject","condition","duration")
t.test(agg$duration[agg$condition==1],agg$duration[agg$condition==2],paired=TRUE)
mean(agg$duration[agg$condition==1])
mean(agg$duration[agg$condition==2])

m=mean(agg$duration[agg$condition==2]-agg$duration[agg$condition==1])
s=sd(agg$duration[agg$condition==2]-agg$duration[agg$condition==1])
m/s




# right side
agg=aggregate(RT-init.time~subject+condition,data=dataRight,FUN="mean") # RT performance data aggregated by subject
names(agg)<-c("subject","condition","duration")
t.test(agg$duration[agg$condition==1],agg$duration[agg$condition==2],paired=TRUE)
mean(agg$duration[agg$condition==1])
mean(agg$duration[agg$condition==2])

m=mean(agg$duration[agg$condition==2]-agg$duration[agg$condition==1])
s=sd(agg$duration[agg$condition==2]-agg$duration[agg$condition==1])
m/s


# AUC
# left side
agg=aggregate(AUC~subject+condition,data=dataLeft,FUN="mean") # RT performance data aggregated by subject
t.test(agg$AUC[agg$condition==1],agg$AUC[agg$condition==2],paired=TRUE)
mean(agg$AUC[agg$condition==1])
mean(agg$AUC[agg$condition==2])

m=mean(agg$AUC[agg$condition==2]-agg$AUC[agg$condition==1])
s=sd(agg$AUC[agg$condition==2]-agg$AUC[agg$condition==1])
m/s



# right side
# left side
agg=aggregate(AUC~subject+condition,data=dataRight,FUN="mean") # RT performance data aggregated by subject
t.test(agg$AUC[agg$condition==1],agg$AUC[agg$condition==2],paired=TRUE)
mean(agg$AUC[agg$condition==1])
mean(agg$AUC[agg$condition==2])

m=mean(agg$AUC[agg$condition==2]-agg$AUC[agg$condition==1])
s=sd(agg$AUC[agg$condition==2]-agg$AUC[agg$condition==1])
m/s



RTcongruentLeft=rep(0,51)
RTincongruentLeft=rep(0,51)
for (i in 1:41){
  RTcongruentLeft[i]<-mean(dataLeft$RT[dataLeft$subject==i & dataLeft$condition==1])
  RTincongruentLeft[i]<-mean(dataLeft$RT[dataLeft$subject==i & dataLeft$condition==2])
}

for (i in 43:52){
  RTcongruentLeft[i-1]<-mean(dataLeft$RT[dataLeft$subject==i & dataLeft$condition==1])
  RTincongruentLeft[i-1]<-mean(dataLeft$RT[dataLeft$subject==i & dataLeft$condition==2])
}

mean(RTcongruentLeft)
mean(RTincongruentLeft)
t.test(RTcongruentLeft,RTincongruentLeft,paired=TRUE)

m=mean(RTincongruentLeft-RTcongruentLeft)
s=sd(RTincongruentLeft-RTcongruentLeft)
m/s

InitcongruentLeft=rep(0,51)
InitincongruentLeft=rep(0,51)
for (i in 1:41){
  InitcongruentLeft[i]<-mean(dataLeft$init[dataLeft$subject==i & dataLeft$condition==1])
  InitincongruentLeft[i]<-mean(dataLeft$init[dataLeft$subject==i & dataLeft$condition==2])
}

for (i in 43:52){
  InitcongruentLeft[i-1]<-mean(dataLeft$init[dataLeft$subject==i & dataLeft$condition==1])
  InitincongruentLeft[i-1]<-mean(dataLeft$init[dataLeft$subject==i & dataLeft$condition==2])
}

m=mean(InitincongruentLeft-InitcongruentLeft)
s=sd(InitincongruentLeft-InitcongruentLeft)
m/s

mean(InitcongruentLeft)
mean(InitincongruentLeft)
t.test(InitcongruentLeft,InitincongruentLeft,paired=TRUE)


AUCcongruentLeft=rep(0,51)
AUCincongruentLeft=rep(0,51)
for (i in 1:41){
  AUCcongruentLeft[i]<-mean(dataLeft$AUC_2[dataLeft$subject==i & dataLeft$condition==1])
  AUCincongruentLeft[i]<-mean(dataLeft$AUC_2[dataLeft$subject==i & dataLeft$condition==2])
}

for (i in 43:52){
  AUCcongruentLeft[i-1]<-mean(dataLeft$AUC_2[dataLeft$subject==i & dataLeft$condition==1])
  AUCincongruentLeft[i-1]<-mean(dataLeft$AUC_2[dataLeft$subject==i & dataLeft$condition==2])
}

m=mean(AUCincongruentLeft-AUCcongruentLeft)
s=sd(AUCincongruentLeft-AUCcongruentLeft)
m/s

t.test(AUCcongruentLeft,AUCincongruentLeft,paired=TRUE)


# right side

RTcongruentRight=rep(0,51)
RTincongruentRight=rep(0,51)
for (i in 1:41){
  RTcongruentRight[i]<-mean(dataRight$RT[dataRight$subject==i & dataRight$condition==1])
  RTincongruentRight[i]<-mean(dataRight$RT[dataRight$subject==i & dataRight$condition==2])
}

for (i in 43:52){
  RTcongruentRight[i-1]<-mean(dataRight$RT[dataRight$subject==i & dataRight$condition==1])
  RTincongruentRight[i-1]<-mean(dataRight$RT[dataRight$subject==i & dataRight$condition==2])
}

m=mean(RTincongruentRight-RTcongruentRight)
s=sd(RTincongruentRight-RTcongruentRight)
m/s

mean(RTcongruentRight)
mean(RTincongruentRight)
t.test(RTcongruentRight,RTincongruentRight,paired=TRUE)


InitcongruentRight=rep(0,51)
InitincongruentRight=rep(0,51)
for (i in 1:41){
  InitcongruentRight[i]<-mean(dataRight$init[dataRight$subject==i & dataRight$condition==1])
  InitincongruentRight[i]<-mean(dataRight$init[dataRight$subject==i & dataRight$condition==2])
}

for (i in 43:52){
  InitcongruentRight[i-1]<-mean(dataRight$init[dataRight$subject==i & dataRight$condition==1])
  InitincongruentRight[i-1]<-mean(dataRight$init[dataRight$subject==i & dataRight$condition==2])
}

m=mean(InitincongruentRight-InitcongruentRight)
s=sd(InitincongruentRight-InitcongruentRight)
m/s

mean(InitcongruentRight)
mean(InitincongruentRight)
t.test(InitcongruentRight,InitincongruentRight,paired=TRUE)


AUCcongruentRight=rep(0,51)
AUCincongruentRight=rep(0,51)
for (i in 1:41){
  AUCcongruentRight[i]<-mean(dataRight$AUC_1[dataRight$subject==i & dataRight$condition==1])
  AUCincongruentRight[i]<-mean(dataRight$AUC_1[dataRight$subject==i & dataRight$condition==2])
}

for (i in 43:52){
  AUCcongruentRight[i-1]<-mean(dataRight$AUC_1[dataRight$subject==i & dataRight$condition==1])
  AUCincongruentRight[i-1]<-mean(dataRight$AUC_1[dataRight$subject==i & dataRight$condition==2])
}

m=mean(AUCincongruentRight-AUCcongruentRight)
s=sd(AUCincongruentRight-AUCcongruentRight)
m/s


mean(AUCcongruentRight)
mean(AUCincongruentRight)
t.test(AUCcongruentRight,AUCincongruentRight,paired=TRUE)

