########################
###
### Metabolic Cage Experiments | Bandsmatiks
### MERGING raw Data
###
########################

### author: CBD, Guanlan

# make list of needed packages
list.of.packages <- c("dplyr","dbplyr","readxl", "tidyverse","tibble", "lubridate","mgsub","DT","here")
# list any missing packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if any packages missing --> install
if(length(new.packages) > 0) {install.packages(new.packages,dependencies = TRUE)}
# load all packages
lapply(list.of.packages, require, character.only = TRUE)


### Put all files in one directory: the folder must contain only the files that will be merged

### IMPORTING all files from directory
### read paths using "here" so that different users can use same code more easily
### create a list of the files from your target directory 
file_list <- list.files(path=here("1-Data","ImportData_forR"))
file_list

### Set up working directory, aka you tell the computer in which folder all the files are
#choose.files()

#replace the path with the location of your files on your computer
setwd("C:\\Users\\Celine\\Dropbox\\Bandsma.Lab\\Project_LongTermEffectsSAM_Paper\\5. Data\\")
## or
#setwd(path=here("1-Data","ImportData_forR"))

#list all files within that folder that finishes with .CSV (comma seperated values)
list.files(pattern="*.csv")


### this is a loop function to load and clean the files individually [Note this loop function is to be adapted to metabolic cage data]
#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
dataset <- data.frame()
for (i in 1:length(file_list[1:94])){
  temp_data <- read_excel(here("1-Data","ImportData_forR", file_list[i]), range = cell_cols("A:N")) #read in files with specify columns
  temp_data <- select(temp_data,c("Timestamp (YYYY-MM-DDThh:mm:ss)","Event Type","Patient Info", starts_with("Glucose Value (mmol"),"Duration (hh:mm:ss)"))
  temp_data$record_id<-rep(unique(temp_data$"Patient Info")[1], dim(temp_data)[1])
  temp_data$record_id<-mgsub(as.character(temp_data$record_id), pattern=c("-"), c(""))
  temp_data<-temp_data[-c(1:9),]
  temp_data<- temp_data %>% rename(Timestamp = `Timestamp (YYYY-MM-DDThh:mm:ss)`,
         Event_Type = `Event Type`,
         Glucose_Value = starts_with("Glucose Value (mmol"),
         Duration = `Duration (hh:mm:ss)`)
  temp_data$Date <- as.Date(temp_data$Timestamp)
  temp_data$Time <- format(as.POSIXlt(strptime(temp_data$Timestamp,format = "%Y-%m-%d %H:%M:%S")),format="%H:%M")
  temp_data <- select(temp_data,c("record_id","Event_Type", "Glucose_Value","Date","Time"))
  dataset <- rbind(dataset, temp_data) # for each iteration, bind the new data to the building dataset
}


## add in index of record count
head(dataset)
dataset$Index<-1:dim(dataset)[1]

##reorder columns
colnames(dataset)
dataset<-dataset[,c(6,1:5)]
head(dataset)

write.csv(dataset, file=paste0(Sys.Date(),"_Glucose_FullCombinedData.csv"))



### this code is each file being loaded individually and needs to be replaced by the above code
#############################################
#read in the files for each cage
#cage 1
X1<-read.csv("DataSet1_MERGE_C1-8_2015-05-12_mice_4-18_percDiet.175.49.csv", header=TRUE)
X1$TimeAll<-c(1:175)

X2<-read.csv("DataSet2_MERGE_C1-8_2015-05-19_HFHC_mice.213.49.csv", header=TRUE)
X2$TimeAll<-c(1:213)

X3<-read.csv("DataSet3_MERGE_C1-8_2015-05-25_4% LPD and HFHC.211.49.csv", header=TRUE)
X3$TimeAll<-c(1:211)

X4<-read.csv("DataSet4_MERGE_C1-8_2015-06-02_4 week recovery mice batch 6.145.49.csv", header=TRUE)
X4$TimeAll<-c(1:145)

X5<-read.csv("DataSet5_MERGE_C1-8_2015-06-22_8 week HFHC mice batch 4.211.49.csv", header=TRUE)
X5$TimeAll<-c(1:211)

X6<-read.csv("DataSet6_MERGE_C1-8_2015-08-14 (14 wk HFHC Batch 4, 13 wk RC batch 5, and 8 wk HFHC batch 7).210.49.csv", header=TRUE)
X6$TimeAll<-c(1:210)

X7<-read.csv("DataSet7_MERGE_C1-8_2015-09-22-Batch 6 (15 week HFHC mice) and batch 7 (14 week HFHC mice).213.49.csv", header=TRUE)
X7$TimeAll<-c(1:213)

X8<-read.csv("DataSet8_MERGE_C1-8_2015-10-05-Batch 8 (15 week HFHC mice) and batch 10 (4 week Recovery Diet mice following 4% LPD).219.49.csv", header=TRUE)
X8$TimeAll<-c(1:219)

X9<-read.csv("DataSet9_MERGE_C1-8_2015-10-19-Batch 12 (4 week on 4% and 18% diet mice).225.49.csv", header=TRUE)
X9$TimeAll<-c(1:225)

X10<-read.csv("DataSet10_MERGE_C1-8_2015-10-30-Batch 9 (14 wk HFHC mice).212.49.csv", header=TRUE)
X10$TimeAll<-c(1:212)

X11<-read.csv("DataSet11_MERGE_C1-8_2015-11-02-Batch 13 (4 week on 4% and 18% diet mice).224.49.csv", header=TRUE)
X11$TimeAll<-c(1:224)

X12<-read.csv("DataSet12_MERGE_C1-8_2015-11-09-Batch 11 (4 wk Recovery Diet following 18% Protein Diet).216.49.csv", header=TRUE)
X12$TimeAll<-c(1:216)



###############
####### MERging of EXPERIMENTS

Fullset<-merge(X1,X2, by="TimeAll",all=TRUE)

Fullset<-merge(Fullset,X3, by="TimeAll",all=TRUE)
Fullset<-merge(Fullset,X4, by="TimeAll",all=TRUE)
Fullset<-merge(Fullset,X5, by="TimeAll",all=TRUE)
Fullset<-merge(Fullset,X6, by="TimeAll",all=TRUE)
Fullset<-merge(Fullset,X7, by="TimeAll",all=TRUE)
Fullset<-merge(Fullset,X8, by="TimeAll",all=TRUE)
Fullset<-merge(Fullset,X9, by="TimeAll",all=TRUE)
Fullset<-merge(Fullset,X10, by="TimeAll",all=TRUE)
Fullset<-merge(Fullset,X11, by="TimeAll",all=TRUE)
Fullset<-merge(Fullset,X12, by="TimeAll",all=TRUE)

#### check head of file
colnames(Fullset)
Fullset
head(Fullset)
tail(Fullset)

write.csv(Fullset, file="2016-04-21_Prasad_AllExp_FULLsetMERGED.csv")
