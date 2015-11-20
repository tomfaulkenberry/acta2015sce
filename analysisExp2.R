#################################################
##  R analysis script for Experiment 2 of 
##  Faulkenberry, Cruise, Lavro, & Shaki (in press),
##  to appear in Acta Psychologica
## 
##  be sure to execute code at bottom of script FIRST
##  in order to load various plotting functions
####################################################

library(ggplot2)

rawData<-read.table("leftTrajectoriesExp2.csv",sep=",",header=TRUE)

# clean up data
dataStep3<-subset(rawData,subset=error!=1) # remove errors

meanRT<-mean(dataStep3$RT)
sdRT<-sd(dataStep3$RT)
dataLeft<-subset(dataStep3,subset=RT<meanRT+3*sdRT & RT>meanRT-3*sdRT) # remove 3 SD outliers
attach(dataLeft)

rawData<-read.table("rightTrajectoriesExp2.csv",sep=",",header=TRUE)
# clean up data
dataStep3<-subset(rawData,subset=error!=1) # remove errors

meanRT<-mean(dataStep3$RT)
sdRT<-sd(dataStep3$RT)
dataRight<-subset(dataStep3,subset=RT<meanRT+3*sdRT & RT>meanRT-3*sdRT) # remove 3 SD outliers
attach(dataRight)

# plot hand trajectories

dataLeftCongruent1<-subset(dataLeft,condition==1 & distance==1)
dataLeftCongruent2<-subset(dataLeft,condition==1 & distance==2)
dataLeftCongruent3<-subset(dataLeft,condition==1 & distance==3)
dataLeftCongruent4<-subset(dataLeft,condition==1 & distance==4)

dataLeftIncongruent1<-subset(dataLeft,condition==2 & distance==1)
dataLeftIncongruent2<-subset(dataLeft,condition==2 & distance==2)
dataLeftIncongruent3<-subset(dataLeft,condition==2 & distance==3)
dataLeftIncongruent4<-subset(dataLeft,condition==2 & distance==4)

dataRightCongruent1<-subset(dataRight,condition==1 & distance==1)
dataRightCongruent2<-subset(dataRight,condition==1 & distance==2)
dataRightCongruent3<-subset(dataRight,condition==1 & distance==3)
dataRightCongruent4<-subset(dataRight,condition==1 & distance==4)

dataRightIncongruent1<-subset(dataRight,condition==2 & distance==1)
dataRightIncongruent2<-subset(dataRight,condition==2 & distance==2)
dataRightIncongruent3<-subset(dataRight,condition==2 & distance==3)
dataRightIncongruent4<-subset(dataRight,condition==2 & distance==4)

xCoords=rep(0,1616)
yCoords=rep(0,1616)
side=rep(0,1616)
condition=rep(0,1616)
distance=rep(0,1616)

for (i in 1:101){
  xCoords[i]=mean(dataLeftCongruent1[,i+23])
  yCoords[i]=mean(dataLeftCongruent1[,i+124])
  side[i]="left"
  condition[i]="congruent"
  distance[i]="1"
  
  xCoords[i+101]=mean(dataLeftCongruent2[,i+23])
  yCoords[i+101]=mean(dataLeftCongruent2[,i+124])
  side[i+101]="left"
  condition[i+101]="congruent"
  distance[i+101]="2"
  
  xCoords[i+202]=mean(dataLeftCongruent3[,i+23])
  yCoords[i+202]=mean(dataLeftCongruent3[,i+124])
  side[i+202]="left"
  condition[i+202]="congruent"
  distance[i+202]="3"
  
  xCoords[i+303]=mean(dataLeftCongruent4[,i+23])
  yCoords[i+303]=mean(dataLeftCongruent4[,i+124])
  side[i+303]="left"
  condition[i+303]="congruent"
  distance[i+303]="4"
    
  xCoords[i+404]=mean(dataLeftIncongruent1[,i+23])
  yCoords[i+404]=mean(dataLeftIncongruent1[,i+124])
  side[i+404]="left"
  condition[i+404]="incongruent"
  distance[i+404]="1"
  
  xCoords[i+505]=mean(dataLeftIncongruent2[,i+23])
  yCoords[i+505]=mean(dataLeftIncongruent2[,i+124])
  side[i+505]="left"
  condition[i+505]="incongruent"
  distance[i+505]="2"
  
  xCoords[i+606]=mean(dataLeftIncongruent3[,i+23])
  yCoords[i+606]=mean(dataLeftIncongruent3[,i+124])
  side[i+606]="left"
  condition[i+606]="incongruent"
  distance[i+606]="3"
  
  xCoords[i+707]=mean(dataLeftIncongruent4[,i+23])
  yCoords[i+707]=mean(dataLeftIncongruent4[,i+124])
  side[i+707]="left"
  condition[i+707]="incongruent"
  distance[i+707]="4"
      
  xCoords[i+808]=mean(dataRightCongruent1[,i+23])
  yCoords[i+808]=mean(dataRightCongruent1[,i+124])
  side[i+808]="right"
  condition[i+808]="congruent"
  distance[i+808]="1"
  
  xCoords[i+909]=mean(dataRightCongruent2[,i+23])
  yCoords[i+909]=mean(dataRightCongruent2[,i+124])
  side[i+909]="right"
  condition[i+909]="congruent"
  distance[i+909]="2"
  
  xCoords[i+1010]=mean(dataRightCongruent3[,i+23])
  yCoords[i+1010]=mean(dataRightCongruent3[,i+124])
  side[i+1010]="right"
  condition[i+1010]="congruent"
  distance[i+1010]="3"
  
  xCoords[i+1111]=mean(dataRightCongruent4[,i+23])
  yCoords[i+1111]=mean(dataRightCongruent4[,i+124])
  side[i+1111]="right"
  condition[i+1111]="congruent"
  distance[i+1111]="4"
        
  xCoords[i+1212]=mean(dataRightIncongruent1[,i+23])
  yCoords[i+1212]=mean(dataRightIncongruent1[,i+124])
  side[i+1212]="right"
  condition[i+1212]="incongruent"
  distance[i+1212]="1"
  
  xCoords[i+1313]=mean(dataRightIncongruent2[,i+23])
  yCoords[i+1313]=mean(dataRightIncongruent2[,i+124])
  side[i+1313]="right"
  condition[i+1313]="incongruent"
  distance[i+1313]="2"
  
  xCoords[i+1414]=mean(dataRightIncongruent3[,i+23])
  yCoords[i+1414]=mean(dataRightIncongruent3[,i+124])
  side[i+1414]="right"
  condition[i+1414]="incongruent"
  distance[i+1414]="3"
  
  xCoords[i+1515]=mean(dataRightIncongruent4[,i+23])
  yCoords[i+1515]=mean(dataRightIncongruent4[,i+124])
  side[i+1515]="right"
  condition[i+1515]="incongruent"
  distance[i+1515]="4"
}

trajectoryData=data.frame(xCoords,yCoords,side,condition,distance)
plot=ggplot(trajectoryData,aes(x=xCoords,y=yCoords,group=condition))+xlim(-1,1)+ylim(0,1.5)
paths=geom_path(aes(linetype=condition),size=1.3)
labels=labs(x="x-coordinates",y="y-coordinates")
faceting=facet_grid(distance~side)
stripFormat=theme(strip.text=element_text(face="bold",size=rel(1.5)))
legendFormat=theme(legend.title=element_text(face="bold",size=rel(1.5)),legend.text=element_text(size=rel(1.5)))
axesFormat=theme(axis.title=element_text(size=rel(1.4)))

basePlot=plot+paths+labels+faceting+stripFormat+legendFormat+axesFormat
basePlot+labs(colour="Condition")+theme(legend.position=c(0.5,0.5))+theme(legend.background=element_rect(fill="white",colour="black"))

# notes: export as 700 x 800



library(reshape)

# PERFORMANCE MEASURES
# RT
# left side
agg=aggregate(RT~subject+condition+distance,data=dataLeft,FUN="mean") # RT performance data aggregated by subject
RT.aov=aov(RT~as.factor(condition)*as.factor(distance)+Error(as.factor(subject)/(as.factor(condition)*as.factor(distance))),data=agg)
summary(RT.aov)
print(model.tables(RT.aov,"means"),digits=6)
write.table(cast(agg,subject~condition+distance),"~/Dropbox/experiments/SCEexp2/data/rtLeft.csv",sep=",") # use this for separate SPSS analysis

# plot line graph (be sure to execute code at bottom to define "summarySEwithin" function)  Note: error bars defined in Morey 2008
summary=summarySEwithin(agg,measurevar="RT",withinvars=c("condition","distance"),idvar="subject")
summary$condition=c("congruent","congruent","congruent","congruent","incongruent","incongruent","incongruent","incongruent")
ggplot(summary,aes(x=distance,y=RT,shape=condition))+geom_line(aes(group=condition,linetype=condition))+geom_point(size=4)+geom_errorbar(width=0.1,aes(ymin=RT-ci,ymax=RT+ci))+labs(x="Numerical distance",y="Mean RT (ms)")+theme(legend.title=element_text(face="bold",size=rel(1.3)),legend.text=element_text(size=rel(1.3)))+theme(axis.title=element_text(face="bold",size=rel(1.3)))+theme(axis.text.x=element_text(size=rel(1.3)))+theme(axis.text.y=element_text(size=rel(1.3)))
  
# right side
agg=aggregate(RT~subject+condition+distance,data=dataRight,FUN="mean") # RT performance data aggregated by subject
RT.aov=aov(RT~as.factor(condition)*as.factor(distance)+Error(as.factor(subject)/(as.factor(condition)*as.factor(distance))),data=agg)
summary(RT.aov)
print(model.tables(RT.aov,"means"),digits=6)
write.table(cast(agg,subject~condition+distance),"~/Dropbox/experiments/SCEexp2/data/rt.csv",sep=",")  # use this for separate SPSS analysis


# plot line graph (be sure to execute code at bottom to define "summarySEwithin" function)  Note: error bars defined in Morey 2008
summary=summarySEwithin(agg,measurevar="RT",withinvars=c("condition","distance"),idvar="subject")
summary$condition=c("congruent","congruent","congruent","congruent","incongruent","incongruent","incongruent","incongruent")
ggplot(summary,aes(x=distance,y=RT,shape=condition))+geom_line(aes(group=condition,linetype=condition))+geom_point(size=4)+geom_errorbar(width=0.1,aes(ymin=RT-ci,ymax=RT+ci))+labs(x="Numerical distance",y="Mean RT (ms)")+theme(legend.title=element_text(face="bold",size=rel(1.3)),legend.text=element_text(size=rel(1.3)))+theme(axis.title=element_text(face="bold",size=rel(1.3)))+theme(axis.text.x=element_text(size=rel(1.3)))+theme(axis.text.y=element_text(size=rel(1.3)))


# MT
# left side
agg=aggregate(RT-init.time~subject+condition+distance,data=dataLeft,FUN="mean") # RT performance data aggregated by subject
names(agg)<-c("subject","condition","distance","duration")
MT.aov=aov(duration~as.factor(condition)*as.factor(distance)+Error(as.factor(subject)/(as.factor(condition)*as.factor(distance))),data=agg)
summary(MT.aov)
print(model.tables(MT.aov,"means"),digits=6)
write.table(cast(agg,subject~condition+distance),"~/Dropbox/experiments/SCEexp2/data/mtLeft.csv",sep=",") # use this for separate SPSS analysis

# plot line graph (be sure to execute code at bottom to define "summarySEwithin" function)  Note: error bars defined in Morey 2008
summary=summarySEwithin(agg,measurevar="duration",withinvars=c("condition","distance"),idvar="subject")
summary$condition=c("congruent","congruent","congruent","congruent","incongruent","incongruent","incongruent","incongruent")
ggplot(summary,aes(x=distance,y=duration,shape=condition))+geom_line(aes(group=condition,linetype=condition))+geom_point(size=4)+geom_errorbar(width=0.1,aes(ymin=duration-ci,ymax=duration+ci))+labs(x="Numerical distance",y="Mean MT (ms)")+theme(legend.title=element_text(face="bold",size=rel(1.3)),legend.text=element_text(size=rel(1.3)))+theme(axis.title=element_text(face="bold",size=rel(1.3)))+theme(axis.text.x=element_text(size=rel(1.3)))+theme(axis.text.y=element_text(size=rel(1.3)))

# right side
agg=aggregate(RT-init.time~subject+condition+distance,data=dataRight,FUN="mean") # RT performance data aggregated by subject
names(agg)<-c("subject","condition","distance","duration")
MT.aov=aov(duration~as.factor(condition)*as.factor(distance)+Error(as.factor(subject)/(as.factor(condition)*as.factor(distance))),data=agg)
summary(MT.aov)
print(model.tables(MT.aov,"means"),digits=6)
write.table(cast(agg,subject~condition+distance),"~/Dropbox/experiments/SCEexp2/data/mtRight.csv",sep=",") # use this for separate SPSS analysis

# plot line graph (be sure to execute code at bottom to define "summarySEwithin" function)  Note: error bars defined in Morey 2008
summary=summarySEwithin(agg,measurevar="duration",withinvars=c("condition","distance"),idvar="subject")
summary$condition=c("congruent","congruent","congruent","congruent","incongruent","incongruent","incongruent","incongruent")
ggplot(summary,aes(x=distance,y=duration,shape=condition))+geom_line(aes(group=condition,linetype=condition))+geom_point(size=4)+geom_errorbar(width=0.1,aes(ymin=duration-ci,ymax=duration+ci))+labs(x="Numerical distance",y="Mean MT (ms)")+theme(legend.title=element_text(face="bold",size=rel(1.3)),legend.text=element_text(size=rel(1.3)))+theme(axis.title=element_text(face="bold",size=rel(1.3)))+theme(axis.text.x=element_text(size=rel(1.3)))+theme(axis.text.y=element_text(size=rel(1.3)))


# Init
# left side
agg=aggregate(init.time~subject+condition+distance,data=dataLeft,FUN="mean") # RT performance data aggregated by subject
init.aov=aov(init.time~as.factor(condition)*as.factor(distance)+Error(as.factor(subject)/(as.factor(condition)*as.factor(distance))),data=agg)
summary(init.aov)
print(model.tables(init.aov,"means"),digits=6)

# right side
agg=aggregate(init.time~subject+condition+distance,data=dataRight,FUN="mean") # RT performance data aggregated by subject
init.aov=aov(init.time~as.factor(condition)*as.factor(distance)+Error(as.factor(subject)/(as.factor(condition)*as.factor(distance))),data=agg)
summary(init.aov)
print(model.tables(init.aov,"means"),digits=6)

# AUC

# left side
agg=aggregate(AUC~subject+condition+distance,data=dataLeft,FUN="mean") # RT performance data aggregated by subject
AUC.aov=aov(AUC~as.factor(condition)*as.factor(distance)+Error(as.factor(subject)/(as.factor(condition)*as.factor(distance))),data=agg)
summary(AUC.aov)
print(model.tables(AUC.aov,"means"),digits=6)
write.table(cast(agg,subject~condition+distance),"~/Dropbox/experiments/SCEexp2/data/aucLeft.csv",sep=",")  # use this for separate SPSS analysis



# right side
agg=aggregate(AUC~subject+condition+distance,data=dataRight,FUN="mean") # RT performance data aggregated by subject
AUC.aov=aov(AUC~as.factor(condition)*as.factor(distance)+Error(as.factor(subject)/(as.factor(condition)*as.factor(distance))),data=agg)
summary(AUC.aov)
print(model.tables(AUC.aov,"means"),digits=6)
write.table(cast(agg,subject~condition+distance),"~/Dropbox/experiments/SCEexp2/data/aucRight.csv",sep=",")  # use this for separate SPSS analysis




# density plot of AUC by condition

# build Condition vector
Condition=c(rep(0,length(data$condition)))
for(i in 1:length(data$condition)){
  if(data$condition[i]==1){
    Condition[i]="Congruent"
  }
  else{
    Condition[i]="Incongruent"
  }
}

# convert AUC to z-scores by participant
attach(dataLeft)
z.AUC.left=c(rep(0,length(dataLeft$subject)))
z.AUC.conditionLeft=rep(0,length(dataLeft$subject))
for(i in 1:length(dataLeft$subject)){
  z.AUC.left[i]=(AUC[i]-mean(AUC[subject==subject[i]]))/sd(AUC[subject==subject[i]])
  z.AUC.conditionLeft[i]=dataLeft$condition[i]
}


attach(dataRight)
z.AUC.right=c(rep(0,length(dataRight$subject)))
z.AUC.conditionRight=rep(0,length(dataRight$subject))
for(i in 1:length(dataRight$subject)){
  z.AUC.right[i]=(AUC[i]-mean(AUC[subject==subject[i]]))/sd(AUC[subject==subject[i]])
  z.AUC.conditionRight[i]=dataRight$condition[i]
}

z.AUC=c(z.AUC.left,z.AUC.right)
z.condition=c(z.AUC.conditionLeft,z.AUC.conditionRight)

dataFull=data.frame(z.AUC,z.condition)
incongruentTrials=dataFull$z.AUC[dataFull$z.condition==2]

# plotting
basePlot=ggplot(NULL,aes(x=incongruentTrials))+geom_histogram(binwidth=0.4,fill="white",colour="black")
labels=labs(x="Area under curve (AUC)",y="Frequency")
axesFormat=theme(axis.title=element_text(size=rel(1.4)))

basePlot+labels+axesFormat

# note: export as 600 x 400

# assess bimodality of AUC values in the "Incongruent" condition

library("diptest")
dip.test(incongruentTrials)

# Bimodality coefficient (SAS)

library("moments")

s=skewness(incongruentTrials)
k=kurtosis(incongruentTrials)
n=length(incongruentTrials)

BC=(s^2+1)/(k+(3*(n-1)^2)/((n-2)*(n-3)))
BC





# helper function for error bars above

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


## Summarizes data, handling within-subjects variables by removing inter-subject variability.
## It will still work if there are no within-S variables.
## Gives count, un-normed mean, normed mean (with same between-group mean),
##   standard deviation, standard error of the mean, and confidence interval.
## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   withinvars: a vector containing names of columns that are within-subjects variables
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                       FUN=is.factor, FUN.VALUE=logical(1))
  
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  
  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")
  
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor
  
  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}

## Norms the data within specified groups in a data frame; it normalizes each
## subject (identified by idvar) so that they have the same mean, within each group
## specified by betweenvars.
##   data: a data frame.
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   na.rm: a boolean that indicates whether to ignore NA's
normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  require(plyr)
  
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  
  # Remove this subject mean column
  data$subjMean <- NULL
  
  return(data)
}
 