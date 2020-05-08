########################
#  title: "Plotting Metabolic Data --- Bandsma"
#author: "cb"
#######################
#Pull up ggplot2 --- and other R addition package needed for plotting
require(ggplot2)
require(reshape2)
require(matrixStats)
library(grid)
require(gdata)

setwd("C:\\Users\\Celine\\Dropbox\\Bandsma.Lab\\Project_LongTermEffectsSAM_Paper\\5. Data\\")
choose.files()

# pull in data
data<-read.csv(file="C:\\Users\\Celine\\Dropbox\\Bandsma.Lab\\Project_LongTermEffectsSAM_Paper\\5. Data\\2016-04-21_Prasad_AllExp_FULLsetMERGED.csv", header=TRUE, row.names=1)
head(data)
colnames(data)


# pull in cage information file
info<-read.csv(file= "C:\\Users\\Celine\\Dropbox\\Bandsma.Lab\\Project_LongTermEffectsSAM_Paper\\5. Data\\MetCage animal information_FINAL.csv", header=TRUE, row.names=1)
head(info)
colnames(info)


#TAKE OUT GIRLS!!!
info<-info[info$Sex=="Male",]
info$Sex<-droplevels(info$Sex)
info$Sex

#add new column with cage id by merging Xperiment and cage number
info$CageID<-paste0(info$X_no,info$cage_no)
colnames(info)
#set as factor
info$CageID<-factor(info$CageID)
info$CageID

#select all 
info$Diet_TimePoint
summary(info$Diet_TimePoint)

#FOR EACH group: make a list of all the animal that were in them and add _RER tag
#this list will be used to select the right columns for calculating the average and plotting

#Group1 4 perc
perc4_CageID.list<-info$CageID[info$Diet_TimePoint=="4perc"]
perc4_CageID.list<-paste0("RER_",perc4_CageID.list)
perc4_CageID.list

#Group2 4perc_RC
perc4_RC_CageID.list<-info$CageID[info$Diet_TimePoint=="4perc_RC"]
perc4_RC_CageID.list<-paste0("RER_",perc4_RC_CageID.list)
perc4_RC_CageID.list

#Group3 4perc_RC_HFHC
perc4_RC_HFHC_CageID.list<-info$CageID[info$Diet_TimePoint=="4perc_RC_HFHC"]
perc4_RC_HFHC_CageID.list<-paste0("RER_",perc4_RC_HFHC_CageID.list)
perc4_RC_HFHC_CageID.list

#Group4 4perc_RC_RC
perc4_RC_RC_CageID.list<-info$CageID[info$Diet_TimePoint=="4perc_RC_RC"]
perc4_RC_RC_CageID.list<-paste0("RER_",perc4_RC_RC_CageID.list)
perc4_RC_RC_CageID.list


#lets do the same for 18percent
#Group1 18 perc
perc18_CageID.list<-info$CageID[info$Diet_TimePoint=="18perc"]
perc18_CageID.list<-paste0("RER_",perc18_CageID.list)
perc18_CageID.list

#Group2 18perc_RC
perc18_RC_CageID.list<-info$CageID[info$Diet_TimePoint=="18perc_RC"]
perc18_RC_CageID.list<-paste0("RER_",perc18_RC_CageID.list)
perc18_RC_CageID.list

#Group3 18perc_RC_HFHC
perc18_RC_HFHC_CageID.list<-info$CageID[info$Diet_TimePoint=="18perc_RC_HFHC"]
perc18_RC_HFHC_CageID.list<-paste0("RER_",perc18_RC_HFHC_CageID.list)
perc18_RC_HFHC_CageID.list

#Group18 18perc_RC_RC
perc18_RC_RC_CageID.list<-info$CageID[info$Diet_TimePoint=="18perc_RC_RC"]
perc18_RC_RC_CageID.list<-paste0("RER_",perc18_RC_RC_CageID.list)
perc18_RC_RC_CageID.list




#now lets pull from main matrix ALL RER columns 
#first create a list of columns to pull
colnames(data)
RER.data<-grep("RER_",colnames(data))
RER.data
# pull the columns
RER.data<-data[,RER.data]
colnames(RER.data)

# now we are going to make a subset of the RER data for each group 
# so that we can average each group together and retrieve the standard deviations
info$Sex
colnames(info)
summary(info$Diet_TimePoint)
summary(info$End_Group)


RER.data.4p<-RER.data[,colnames(RER.data) %in% perc4_CageID.list]
head(RER.data.4p)

RER.data.4p_RC<-RER.data[,colnames(RER.data) %in% perc4_RC_CageID.list]
head(RER.data.4p_RC)

RER.data.4p_RC_HFHC<-RER.data[,colnames(RER.data) %in% perc4_RC_HFHC_CageID.list]
head(RER.data.4p_RC_HFHC)

RER.data.4p_RC_RC<-RER.data[,colnames(RER.data) %in% perc4_RC_RC_CageID.list]
head(RER.data.4p_RC_RC)

# 18 percent diet
RER.data.18p<-RER.data[,colnames(RER.data) %in% perc18_CageID.list]
head(RER.data.18p)

RER.data.18p_RC<-RER.data[,colnames(RER.data) %in% perc18_RC_CageID.list]
head(RER.data.18p_RC)

RER.data.18p_RC_HFHC<-RER.data[,colnames(RER.data) %in% perc18_RC_HFHC_CageID.list]
head(RER.data.18p_RC_HFHC)

RER.data.18p_RC_RC<-RER.data[,colnames(RER.data) %in% perc18_RC_RC_CageID.list]
head(RER.data.18p_RC_RC)


#now I want to plot each group together to check for outliers etc

# cages group 4 perc
head(RER.data.4p)
RER.data.4p$id<-1:225
head(RER.data.4p)

m.RER.data.4p<-melt(RER.data.4p, id="id")
colnames(m.RER.data.4p)
head(m.RER.data.4p)

p<-ggplot(aes(x=id,y=value, colour=variable,group=variable),data=m.RER.data.4p)
p+geom_line()+
  labs(title="RER.data.4p")
ggsave(file="RER.data.4p.svg")


p<-ggplot(aes(x=id,y=value),data=m.RER.data.4p)
p+stat_summary(geom="line", fun.y="mean", na.rm=TRUE)+
  stat_smooth()+
labs(title="RER.data.4p_smooth")
ggsave(file="RER.data.4p_smooth.svg")


# cages group 4 perc_RC
head(RER.data.4p_RC)
RER.data.4p_RC$id<-1:225
head(RER.data.4p_RC)

m.RER.data.4p_RC<-melt(RER.data.4p_RC, id="id")
colnames(m.RER.data.4p_RC)
head(m.RER.data.4p_RC)

p<-ggplot(aes(x=id,y=value, colour=variable,group=variable),data=m.RER.data.4p_RC)
p+geom_line()+
labs(title="RER.data.4p_RC")
ggsave(file="RER.data.4p_RC.svg")

p<-ggplot(aes(x=id,y=value),data=m.RER.data.4p_RC)
p+stat_summary(geom="line", fun.y="mean", na.rm=TRUE)+
  stat_smooth()+
  labs(title="RER.data.4p_RC_smooth")
ggsave(file="RER.data.4p_RC_smooth.svg")

# cages group 4 perc_RC_RC
head(RER.data.4p_RC_RC)
RER.data.4p_RC_RC$id<-1:225
head(RER.data.4p_RC_RC)

m.RER.data.4p_RC_RC<-melt(RER.data.4p_RC_RC, id="id")
colnames(m.RER.data.4p_RC_RC)
head(m.RER.data.4p_RC_RC)

p<-ggplot(aes(x=id,y=value, colour=variable,group=variable),data=m.RER.data.4p_RC_RC)
p+geom_line()+
  labs(title="RER.data.4p_RC_RC")
ggsave(file="RER.data.4p_RC_RC.svg")

p<-ggplot(aes(x=id,y=value),data=m.RER.data.4p_RC_RC)
p+stat_summary(geom="line", fun.y="mean", na.rm=TRUE)+
  stat_smooth()+
labs(title="RER.data.4p_RC_RC_smooth")
ggsave(file="RER.data.4p_RC_RC_smooth.svg")

# cages group 4 perc_RC_HFHC
head(RER.data.4p_RC_HFHC)
RER.data.4p_RC_HFHC$id<-1:225
head(RER.data.4p_RC_HFHC)

m.RER.data.4p_RC_HFHC<-melt(RER.data.4p_RC_HFHC, id="id")
colnames(m.RER.data.4p_RC_HFHC)
head(m.RER.data.4p_RC_HFHC)

p<-ggplot(aes(x=id,y=value, colour=variable,group=variable),data=m.RER.data.4p_RC_HFHC)
p+geom_line()+
  labs(title="RER.data.4p_RC_HFHC")
ggsave(file="RER.data.4p_RC_HFHC.svg")

p<-ggplot(aes(x=id,y=value),data=m.RER.data.4p_RC_HFHC)
p+stat_summary(geom="line", fun.y="mean", na.rm=TRUE)+
  stat_smooth()+
  labs(title="RER.data.4p_RC_HFHC_smooth")
ggsave(file="RER.data.4p_RC_HFHC_smooth.svg")


##########################  NOW for 18perc
# cages group 18 perc
head(RER.data.18p)
RER.data.18p$id<-1:225
head(RER.data.18p)

m.RER.data.18p<-melt(RER.data.18p, id="id")
colnames(m.RER.data.18p)
head(m.RER.data.18p)

p<-ggplot(aes(x=id,y=value, colour=variable,group=variable),data=m.RER.data.18p)
p+geom_line()+
  labs(title="RER.data.18p")
ggsave(file="RER.data.18p.svg")

p<-ggplot(aes(x=id,y=value),data=m.RER.data.18p)
p+stat_summary(geom="line", fun.y="mean", na.rm=TRUE)+
  stat_smooth()+
  labs(title="RER.data.18p_smooth")
ggsave(file="RER.data.18p_smooth.svg")


# cages group 18 perc_RC
head(RER.data.18p_RC)
RER.data.18p_RC$id<-1:225
head(RER.data.18p_RC)

m.RER.data.18p_RC<-melt(RER.data.18p_RC, id="id")
colnames(m.RER.data.18p_RC)
head(m.RER.data.18p_RC)

p<-ggplot(aes(x=id,y=value, colour=variable,group=variable),data=m.RER.data.18p_RC)
p+geom_line()+
  labs(title="RER.data.18p_RC")
ggsave(file="RER.data.18p_RC.svg")

p<-ggplot(aes(x=id,y=value),data=m.RER.data.18p_RC)
p+stat_summary(geom="line", fun.y="mean", na.rm=TRUE)+
  stat_smooth()+
  labs(title="RER.data.18p_RC_smooth")
ggsave(file="RER.data.18p_RC_smooth.svg")


# cages group 18 perc_RC_RC
head(RER.data.18p_RC_RC)
RER.data.18p_RC_RC$id<-1:225
head(RER.data.18p_RC_RC)

m.RER.data.18p_RC_RC<-melt(RER.data.18p_RC_RC, id="id")
colnames(m.RER.data.18p_RC_RC)
head(m.RER.data.18p_RC_RC)

p<-ggplot(aes(x=id,y=value, colour=variable,group=variable),data=m.RER.data.18p_RC_RC)
p+geom_line()+
  labs(title="RER.data.18p_RC_RC")
ggsave(file="RER.data.18p_RC_RC.svg")
  

p<-ggplot(aes(x=id,y=value),data=m.RER.data.18p_RC_RC)
p+stat_summary(geom="line", fun.y="mean", na.rm=TRUE)+
  stat_smooth()+
  labs(title="RER.data.18p_RC_RC_smooth")
ggsave(file="RER.data.18p_RC_RC_smooth.svg")


# cages group 18 perc_RC_HFHC
head(RER.data.18p_RC_HFHC)
RER.data.18p_RC_HFHC$id<-1:225
head(RER.data.18p_RC_HFHC)

m.RER.data.18p_RC_HFHC<-melt(RER.data.18p_RC_HFHC, id="id")
colnames(m.RER.data.18p_RC_HFHC)
head(m.RER.data.18p_RC_HFHC)

p<-ggplot(aes(x=id,y=value, colour=variable,group=variable),data=m.RER.data.18p_RC_HFHC)
p+geom_line()+
  labs(title="RER.data.18p_RC_HFHC")
ggsave(file="RER.data.18p_RC_HFHC.svg")

p<-ggplot(aes(x=id,y=value),data=m.RER.data.18p_RC_HFHC)
p+stat_summary(geom="line", fun.y="mean", na.rm=TRUE)+
  stat_smooth()+
  labs(title="RER.data.18p_RC_HFHC_smooth")
ggsave(file="RER.data.18p_RC_HFHC_smooth.svg")




############################################
############################################
#calculate the means and standard deviations
plot(RER.data.4p)


W16.4p_HFHC<-c(2,7,"red", "G1")
W16.18p_HFHC<-c(3,6,"orange'", "G2")
W15.4p_RC<-c(1, "grey", "G3")
W15.18p_RC<-c(8,"black", "G4")
W8.4p_HFHC<-c(4,"green", "G5")
W8.4p_RC<-c(5,"purple", "G6")




###[Note and Considerations]
the ***lights off /on cycles need to be checked*** 
  do we want to average the 2 days into 1 full 24 hours cycle, it would take out some noise, and make any real signal cleaner (in theory).

***
  Let's start
here we calculate the range of values to set the limits for y

```{r,echo=TRUE}

range(data$RER_C1)
range(data$RER_C2)
range(data$RER_C3)
range(data$RER_C4)
range(data$RER_C5)
range(data$RER_C6)
range(data$RER_C7)
range(data$RER_C8)
```

Now, let's establish the active phase of the animals, which intervals are lights-on and which are lights-off.
```{r,echo=TRUE}
data[,1:2]


#lights off- 19 - 55 
#lights on - 56 - 98

#lights off: 99 -135;
#lights on 136 - 178; 

#lights off 179 - 215
#lights on 216 - 224
```

Now let's plot every cage individually
And the ligths on and off windows (green boxes indicate ACTIVE period)

```{r,echo=TRUE}
#C1
x1=data$INTERVAL
y1=data$RER_C1

p1<-ggplot(data=data,aes(x=x1,y=y1))+
geom_line(color="grey")+
scale_y_continuous(limits=c(0.7,1))+
annotate("rect", xmin= 19, xmax = 55, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
annotate("rect", xmin= 99, xmax = 135, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
annotate("rect", xmin= 179, xmax = 215, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
labs(title="RER_C1 15Weeks.4%_RC ", x="Interval",y="RER")+
theme(title=element_text(size=14),axis.text = element_text(size=11),legend.position="none")+
theme_bw()
print(p1)

#C2 
x2=data$INTERVAL
y2=data$RER_C2

p2<-ggplot(data=data,aes(x=x2,y=y2))+
geom_line(color="red")+
scale_y_continuous(limits=c(0.7,1))+
annotate("rect", xmin= 19, xmax = 55, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
annotate("rect", xmin= 99, xmax = 135, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
annotate("rect", xmin= 179, xmax = 215, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
labs(title="RER_C2 16Weeks.4%_HFHC ", x="Interval",y="RER")+
theme(title=element_text(size=14),axis.text = element_text(size=11),legend.position="none")+
theme_bw()
print(p2)


#C3 
x3=data$INTERVAL
y3=data$RER_C3

p3<-ggplot(data=data,aes(x=x3,y=y3))+
geom_line(color="blue")+
scale_y_continuous(limits=c(0.7,1))+
annotate("rect", xmin= 19, xmax = 55, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
annotate("rect", xmin= 99, xmax = 135, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
annotate("rect", xmin= 179, xmax = 215, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
labs(title="RER_C3 16Weeks.18%_HFHC ", x="Interval",y="RER")+
theme(title=element_text(size=14),axis.text = element_text(size=11),legend.position="none")+
theme_bw()
print(p3)

#C4  
x4=data$INTERVAL
y4=data$RER_C4

p4<-ggplot(data=data,aes(x=x4,y=y4))+
geom_line(color="blue")+
scale_y_continuous(limits=c(0.7,1))+
annotate("rect", xmin= 19, xmax = 55, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
annotate("rect", xmin= 99, xmax = 135, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
annotate("rect", xmin= 179, xmax = 215, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
labs(title="RER_C4 8Weeks.4%_HFHC ", x="Interval",y="RER")+
theme(title=element_text(size=14),axis.text = element_text(size=11),legend.position="none")+
theme_bw()
print(p4)

#C5  
x5=data$INTERVAL
y5=data$RER_C5

p5<-ggplot(data=data,aes(x=x5,y=y5))+
geom_line(color="purple")+
scale_y_continuous(limits=c(0.7,1))+
annotate("rect", xmin= 19, xmax = 55, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
annotate("rect", xmin= 99, xmax = 135, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
annotate("rect", xmin= 179, xmax = 215, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
labs(title="RER_C5 8Weeks.4%_RC", x="Interval",y="RER")+
theme(title=element_text(size=14),axis.text = element_text(size=11),legend.position="none")+
theme_bw()
print(p5)

#C6  
x6=data$INTERVAL
y6=data$RER_C6

p6<-ggplot(data=data,aes(x=x6,y=y6))+
geom_line(color="orange")+
scale_y_continuous(limits=c(0.7,1))+
annotate("rect", xmin= 19, xmax = 55, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
annotate("rect", xmin= 99, xmax = 135, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
annotate("rect", xmin= 179, xmax = 215, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
labs(title="RER_C6 16Weeks.18%_HFHC", x="Interval",y="RER")+
theme(title=element_text(size=14),axis.text = element_text(size=11),legend.position="none")+
theme_bw()
print(p6)

#C7  
x7=data$INTERVAL
y7=data$RER_C7

p7<-ggplot(data=data,aes(x=x7,y=y7))+
geom_line(color="red")+
scale_y_continuous(limits=c(0.7,1))+
labs(title="RER_C7 16Weeks.4%_HFHC", x="Interval",y="RER")+
annotate("rect", xmin= 19, xmax = 55, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
annotate("rect", xmin= 99, xmax = 135, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
annotate("rect", xmin= 179, xmax = 215, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
theme(title=element_text(size=14),axis.text = element_text(size=11),legend.position="none")+
theme_bw()
print(p7)

#C8  
x8=data$INTERVAL
y8=data$RER_C8

p8<-ggplot(data=data,aes(x=x8,y=y8))+
geom_line(color="blue")+
scale_y_continuous(limits=c(0.7,1))+
labs(title="RER_C8 15Weeks.18%_RC", x="Interval",y="RER")+
annotate("rect", xmin= 19, xmax = 55, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
annotate("rect", xmin= 99, xmax = 135, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
annotate("rect", xmin= 179, xmax = 215, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
theme(title=element_text(size=14),axis.text = element_text(size=11),legend.position="none")+
theme_bw()
print(p8)

multiplot(p4,p2,p7,p3,p6,p5,p1,p8, cols=2)

```

***
Now let's plot them together: to check for outliers within the groups

This is the RER for W4.4perc and in orange W4.4perc_4Wr
```{r,echo=TRUE}

x=data$INTERVAL
y=data$RER_C1
p<-ggplot(data=data,aes(x=x,y=y))
p+
  geom_line(colour="red")+
  geom_line(y=data$RER_C2, colour="red")+
  geom_line(y=data$RER_C3, colour="blue")+
  geom_line(y=data$RER_C4, colour="blue")+
  geom_line(y=data$RER_C5, colour="purple")+
  geom_line(y=data$RER_C6, colour="orange")+
  geom_line(y=data$RER_C7, colour="red")+
  geom_line(y=data$RER_C8, colour="blue")+
  scale_y_continuous(limits=c(0.65,1.1))+
  annotate("rect", xmin= 19, xmax = 55, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
  annotate("rect", xmin= 99, xmax = 135, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
  annotate("rect", xmin= 179, xmax = 215, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
  labs(title="RER", x="Interval",y="RER")+
  theme(title=element_text(size=14),axis.text = element_text(size=11))+
  theme_bw()
```

so could be a difference in the animals after 4Week recovery, they show the "average" 0.85-0.8 RER mixed substrait usage... whereas the respiratory exchange ratio in the 4weeks@4% are much closer to 1. 

It would worth looking at if the variability is greater in the RER of  4weeks@4% during the animals resting phase as compared to their active phase.

***
  This is the RER for W4.18perc and in puple W4.18perc_4Wr
```{r,echo=TRUE}

x=data$INTERVAL
y=data$RER_C1
p<-ggplot(data=data,aes(x=x,y=y))
p+
  #geom_line(colour="red")+
  #geom_line(y=data$RER_C2, colour="red")+
  geom_line(y=data$RER_C3, colour="blue")+
  geom_line(y=data$RER_C4, colour="blue")+
  geom_line(y=data$RER_C5, colour="purple")+
  #geom_line(y=data$RER_C6, colour="orange")+
  #geom_line(y=data$RER_C7, colour="red")+
  geom_line(y=data$RER_C8, colour="blue")+
  scale_y_continuous(limits=c(0.65,1.1))+
  annotate("rect", xmin= 19, xmax = 55, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
  annotate("rect", xmin= 99, xmax = 135, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
  annotate("rect", xmin= 169.3, xmax = 183, ymin = 0.65, ymax = Inf, fill="green",alpha=0.2)+
  labs(title="RER - C1-8", x="Interval",y="RER")+
  theme(title=element_text(size=14),axis.text = element_text(size=11))+
  theme_bw()
```
so, ... one of the animal hovers quite high at ~1, whereas the 2 others seem to have lower and maybe more variable RER.
our 4Week@18%_recovered seems to cluster with the 4Week@18%, but has oscillation? just one animal hard to say anything

****
  Okay, lets plot the row means of each group.
To do RER first lets make a new data set with only the RER data

```{r,echo=TRUE}
RER<-data[,matchcols(data, with=c("RER"))]
colnames(RER)
```

Let's group the cages and average them across their row values
calculate the row mean of each of the 4 Groups
```{r,echo=TRUE}
# Cage 1,2,7:W4.4perc
G1<-rowMeans(RER[,c(2,7)], na.rm=TRUE)

#cage 3, 4,8: W4.18perc
G2<-rowMeans(RER[,c(3,6)], na.rm=TRUE)

#cage 5 :W4.18perc_4Wr  
G3<-RER[,c(1)]

# cage 6: W4.4perc_4Wr  
G4<-RER[,c(8)]

# cage 6: W4.4perc_4Wr  
G5<-RER[,c(4)]

# cage 6: W4.4perc_4Wr  
G6<-RER[,c(5)]


```

***
So plotting the average for each group

```{r,echo=TRUE}
x=data$INTERVAL
y=G1
p<-ggplot(data=data,aes(x=x,y=y))+
geom_line(colour="red")+
geom_line(y=G2, colour="orange")+
geom_line(y=G3, colour="grey")+
geom_line(y=G4, colour="black")+
geom_line(y=G5, colour="green")+
geom_line(y=G6, colour="purple")+
annotate("rect", xmin= 19, xmax = 55, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
annotate("rect", xmin= 99, xmax = 135, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
annotate("rect", xmin= 179, xmax = 215, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
scale_y_continuous(limits=c(0.65,1.1))+
labs(title="RER", x="Interval",y="RER")+
scale_colour_discrete(name  ="Group", labels=c("W4.4perc", "W4.18perc", "W4.4perc_4Wr", "W4.18perc_4Wr"))+
theme(title=element_text(size=14),axis.text = element_text(size=11))+
theme_bw()
print(p)
```


Okay, now lets plot the same graph and loose the 2 groups that consist of just one animal, just to see better... 
```{r,echo=TRUE}
x=data$INTERVAL
y=W4.4perc
p<-ggplot(data=data,aes(x=x,y=y))+
geom_line(colour="red")+
geom_line(y=W4.18perc, colour="blue")+
annotate("rect", xmin= 19, xmax = 55, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
annotate("rect", xmin= 99, xmax = 135, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
annotate("rect", xmin= 169.3, xmax = 183, ymin = 0.65, ymax = Inf, fill="green",alpha=0.2)+
scale_y_continuous(limits=c(0.65,1.1))+
labs(title="RER", x="Interval",y="RER")+
scale_colour_discrete(name  ="Group", labels=c("W4.4perc", "W4.18perc"))+
theme(title=element_text(size=14),axis.text = element_text(size=11))+
theme_bw()
print(p)
```

So there "might" be something : in the active period of the animals
were the RER could be higher in the 4 week diet at 4%. The RER of the animals on 4 week diet at 4% or 18% is very high, and pretty close to pure glucose use (~1), more so in the day. Generally the RER would hover around 0.85-0.8 which we see in the 2 "4 week recovered animals" ... however, these recovered animals are of a different age, ... and this needs to be considered. I am not sure of the details here. 

So, it is possible that having a higher respiratory exchange ratio (ratio between the amount of oxygen (O2) consumed and carbon dioxide (CO2) produced) during the day in these animals would be functionning on a slightly higher ratio of glucose.

***
Some notes from:
Speakman, John R. “Measuring Energy Metabolism in the Mouse – Theoretical, Practical, and Analytical Considerations.” Frontiers in Physiology 4 (March 14, 2013). doi:10.3389/fphys.2013.00034.

Theoretically total daily energy expenditure is comprised of several different components: basal or resting expenditure, physical activity, thermoregulation, and the thermic effect of food. 
There are two fundamentally different ways to measure energy metabolism. The end product of all metabolic activity is either heat or work. Since work also ultimately appears as heat, one way is to measure the heat produced directly by the animal. This is called direct calorimetry. Technical difficulties in measuring directly heat expenditure,.. 
The alternative is to not measure the heat directly but rather measure components of the metabolic process that generate the heat, and hence infer its production indirectly. This has become known as indirect calorimetry, or, because respiratory gases are used, respirometry.
Fortunately measuring oxygen and CO2 gases can be performed with great accuracy: much better accuracy than for the small amounts of heat involved. Moreover, these compounds are not normally stored in the body to any great extent – unlike heat. The only downside of this approach is that animals do not always metabolize glucose, and when they change to burning other substrates the equation changes. However the equation changes in a systematic way depending on the substrate being used. This can be diagnosed from the measured ratio of oxygen consumption to CO2 production (called the respiratory exchange ratio: RER). 
The actual substrate oxidation at the tissue level is called the respiratory quotient (RQ). RQ is reflected in the RER but because of lags in the body they are not directly equivalent over short timescales. If we can work out how much nitrogen has been produced via the urine to calculate protein oxidation, then we can work out the other substrate oxidations from the RER (Weir, 1949), and very accurately calculate the energy expenditure. In fact, not correcting for differences in protein oxidation induces only a small error, ***unless protein oxidation exceeds 15% (Even and Nadkarni, 2012)*** and most people ignore this effect, using only the oxygen consumption combined with the estimated RQ from simultaneous measurements of CO2 production.

[note] are we expecting high protein oxidation... and how would that affect the data

If both CO2 and O2 are measured then the resultant oxygen consumption can be converted into an energy expenditure measurement using the inferred substrate utilization (RQ) from the measured RER, assuming negligible protein oxidation has occurred.
Unless there is good reason to expect the animal is metabolizing exclusively fat, or a known diet with a given composition (food quotient) then ***generally the unknown RQ is assumed to be 0.8 or 0.85, as values between 0.7 (pure fat oxidation) and 1.0 (pure carbohydrate oxidation)*** minimize the error in the assumption. 

***
Now lets plot the data but with the loess smoother function, this smooths the data, and takes a lot of the variablity out, and gives a better view of the general "mean"

```{r,echo=TRUE}
x=data$INTERVAL
y=G1
p<-ggplot(data=data,aes(x=x,y=y))
p+geom_smooth(aes(y=G1), colour="red")+
geom_smooth(aes(y=G2), colour="orange")+
geom_smooth(aes(y=G3), colour="grey")+
geom_smooth(aes(y=G4), colour="black")+
geom_smooth(aes(y=G5), colour="green")+
geom_smooth(aes(y=G6), colour="purple")+
scale_y_continuous(limits=c(0.65,1.1))+
annotate("rect", xmin= 19, xmax = 55, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
annotate("rect", xmin= 99, xmax = 135, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
annotate("rect", xmin= 179, xmax = 215, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
labs(title="RER - Loess Smoothed", x="Interval",y="RER")+
theme(title=element_text(size=14),axis.text = element_text(size=11))+
theme_bw()
```

Now lets plot both the loess smooth curves over the raw data just to 
see.

```{r,echo=TRUE}
x=data$INTERVAL
y=W4.4perc
p<-ggplot(data=data,aes(x=x,y=y))
p+geom_line(colour="red")+
geom_line(y=W4.18perc, colour="blue")+
geom_line(y=W4.4perc_4Wr, colour="orange")+
geom_line(y=W4.18perc_4Wr, colour="purple")+
stat_smooth(aes(y=W4.4perc), colour="red")+
stat_smooth(aes(y=W4.18perc), colour="blue")+
stat_smooth(aes(y=W4.4perc_4Wr), colour="orange")+
stat_smooth(aes(y=W4.18perc_4Wr), colour="purple")+
scale_y_continuous(limits=c(0.65,1.1))+
annotate("rect", xmin= 19, xmax = 55, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
annotate("rect", xmin= 99, xmax = 135, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
annotate("rect", xmin= 169.3, xmax = 183, ymin = 0.65, ymax = Inf, fill="green",alpha=0.2)+
labs(title="RER - Raw data and Loess Curve", x="Interval",y="RER")+
theme(title=element_text(size=14),axis.text = element_text(size=11))
```


Now lets try and use the 3point moving average instead of the loess smooth function.

Lets' average each point with the point before it, and the point after it...  this smooths out the curve and reduces noise

```{r,echo=TRUE}
G1.ma<-filter(G1,sides=2,rep(1/3,3)) # 3point moving average
G2.ma<-filter(G2,sides=2,rep(1/3,3)) # 3point moving average
G3.ma<-filter(G3,sides=2,rep(1/3,3)) # 3point moving average
G4.ma<-filter(G4,sides=2,rep(1/3,3)) # 3point moving average
G5.ma<-filter(G5,sides=2,rep(1/3,3)) # 3point moving average
G6.ma<-filter(G6,sides=2,rep(1/3,3)) # 3point moving average
```

Plotting the moving average
```{r,echo=TRUE}
x=data$INTERVAL
y=G1.ma
p<-ggplot(data=data,aes(x=x,y=y))
p+geom_line(colour="red")+
  geom_line(y=G2.ma, colour="orange")+
  # geom_line(y=G3.ma, colour="grey")+
  #geom_line(y=G4.ma, colour="black")+
  # geom_line(y=G5.ma, colour="green")+
  #geom_line(y=G6.ma, colour="purple")+
  scale_y_continuous(limits=c(0.7,1))+
  annotate("rect", xmin= 19, xmax = 55, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
  annotate("rect", xmin= 99, xmax = 135, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
  annotate("rect", xmin= 179, xmax = 215, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
  labs(title="RER - 3 point moving average", x="Interval",y="RER")+
  theme(title=element_text(size=14),axis.text = element_text(size=11))+
  theme_bw()
```

again lets re do the same plot without the 2 groups containing 1 animal
```{r,echo=TRUE}
x=data$INTERVAL
y=W4.4perc.ma
p<-ggplot(data=data,aes(x=x,y=y))
p+geom_line(colour="red")+
  geom_line(y=W4.18perc, colour="blue")+
  #geom_line(y=W4.4perc_4Wr, colour="orange")+
  #geom_line(y=W4.18perc_4Wr, colour="purple")+
  scale_y_continuous(limits=c(0.65,1.1))+
  annotate("rect", xmin= 19, xmax = 55, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
  annotate("rect", xmin= 99, xmax = 135, ymin = 0.7, ymax = Inf, fill="green",alpha=0.2)+
  annotate("rect", xmin= 169.3, xmax = 183, ymin = 0.65, ymax = Inf, fill="green",alpha=0.2)+
  labs(title="RER - 3 point moving average", x="Interval",y="RER")+
  theme(title=element_text(size=14),axis.text = element_text(size=11))+
  theme_bw()
```


