###Data Cleaning Scrip
##09.18.2017
#P.Hoover

#Load Libraries
library(dplyr)
library(xlsx)
library(tidyr)
library(reshape2)

#Medication Tables
meds<-read.xlsx("Vestib_Export_08102017.xlsx",header=TRUE,sheetName="export_Vestib_SOT")
save(meds,file="Meds_cleaning.RData")

meds_split<-meds[,c(1,6)]
meds_split$Medication<-sub("PM "," PM:",meds_split$Medication)
meds_split$Medication<-sub("PM:"," PM:",meds_split$Medication)
meds_split$Medication<-sub("This morning:","AM:",meds_split$Medication)
meds_split$Medication<-sub("Today:","AM:",meds_split$Medication)
meds_split$Medication<-sub("Yesterday"," PM",meds_split$Medication)
meds_split$Medication<-sub("AM ","AM:",meds_split$Medication)
meds_split$Medication<-sub("Last night:"," PM:",meds_split$Medication)
meds_split$Medication<-sub("Taken this morning:","AM:",meds_split$Medication)
meds_split$Medication<-sub("This :"," PM:",meds_split$Medication)
meds_split$Medication<-sub("Medications taken this morning:","AM:",meds_split$Medication)
meds_split$Medication<-sub("Last night","PM:",meds_split$Medication)
meds_split$Medication<-sub("taken last night","PM:",meds_split$Medication)
meds_split$Medication<-sub("Last evening","PM:",meds_split$Medication)
meds_split$Medication<-sub("This Morning","AM:",meds_split$Medication)
meds_split$Medication<-sub("Taken this am:","AM:",meds_split$Medication)

meds_split<-meds_split%>%
  separate(Medication,c("AM","PM"),sep="PM")

meds_split$AM<-sub("AM:","",meds_split$AM)
meds_split$PM<-sub(":","",meds_split$PM)

AM<-meds_split%>%
  separate(AM,paste0("AM",1:20),sep=",",remove=FALSE)%>%
  select(-c(AM,PM))%>%
  gather(Time,value=Meds,-ID)%>%
  filter(!Meds=="NA")%>%
  mutate(Time="AM")

PM<-meds_split%>%
  separate(PM,paste0("PM",1:20),sep=",",remove=FALSE)%>%
  select(-c(AM,PM))%>%
  gather(Time, value=Meds,-ID)%>%
  filter(!Meds=="NA")%>%
  mutate(Time="PM")

medications<-rbind(PM,AM)
medications$Meds<-tolower(medications$Meds)

View(table(medications$Meds))

medications$Meds<-gsub("sm denies","",medications$Meds)
medications$Meds<-sub("and","",medications$Meds)
medications$Meds<-gsub("none","",medications$Meds)
medications$Meds<-gsub("taking ","",medications$Meds)
medications$Meds<-gsub("no medications","",medications$Meds)

#write.xlsx(medications,file="Medications.xlsx")

##Reading in Entire Speadsheet
library(XLConnect)

workbook<-loadWorkbook("C:/Users/Peter.Hoover/Documents/R/Vestibular/Validity_Project/Vestib_Export_08102017_trim.xlsx")
sheet_names<-getSheets(workbook)
names(sheet_names)<-sheet_names

workbook_list<-lapply(sheet_names,function(.sheet){
readWorksheet(object=workbook,.sheet)
  })

#save(workbook_list,file="DataList.RData")

#Merge Files and Tests
load(file="DataList.RData")

mymerge<-function(x,y){
  merge(x,y,all=TRUE,by="ID")
}

#Survey Consolidation
ABC<-workbook_list$export_ABC%>%
  filter(ABC_Description=="Admission")
DHI<-workbook_list$export_DHI%>%
  filter(DHI_Description=="Admission")
GAD<-workbook_list$export_GAD7%>%
  filter(GAD_Description=="Admission")
NSI<-workbook_list$export_NSI%>%
  filter(NSI_Description=="Admission")
PCLM<-workbook_list$export_PCLM%>%
  filter(PCLM_Description=="Admission")
PHQ<-workbook_list$export_PHQ9%>%
  filter(PHQ_Description=="Admission")

Surveys<-Reduce(mymerge, list(ABC,DHI,GAD,NSI,PCLM,PHQ))
#save(Surveys,file="Surveys.RData")

##Injury Cleaning
injury<-workbook_list$export_Injury
injury_counts<-injury%>%
  group_by(ID)%>%
  summarise(LOC=sum(Injury_LOC=="Yes"),
            AOC=sum(Injury_Alteration=="Yes"),
            PTA=sum(Injury_PTA=="Yes"))

##DoT 
SOT<-workbook_list$export_Vestib_SOT
SOT<-SOT[,-c(4:12,15:17)]

#History
Hx<-workbook_list$export_Vestib_History
Hx<-Hx[,c(1,3,5,7,9,11)]
Hx[,c(2:6)]<-ifelse(!is.na(Hx[,c(2:6)]),"NA","X")

#Vestib Testing
Testing<-workbook_list$export_Vestib_Combined

Testing<-Testing[,grep("\\_Abnormal|\\ID",names(Testing))]
Testing$Spon_Gaze<-ifelse(Testing$Vestibular_Oculomotor_Spontaneous_Nystagmus_Abnormal=="C"|Testing$Vestibular_Oculomotor_Gaze_Center_Abnormal=="C","C","X")
Testing$GazeRL<-ifelse(Testing$Vestibular_Oculomotor_Gaze_Left_Abnormal=="C"& Testing$Vestibular_Oculomotor_Gaze_Right_Abnormal=="C","C","NA")
Testing$GazeR<-ifelse(Testing$Vestibular_Oculomotor_Gaze_Right_Abnormal=="C"& !Testing$Vestibular_Oculomotor_Gaze_Left_Abnormal=="C",Testing$Vestibular_Oculomotor_Gaze_Right_Abnormal,"NA")
Testing$GazeL<-ifelse(Testing$Vestibular_Oculomotor_Gaze_Left_Abnormal=="C"& !Testing$Vestibular_Oculomotor_Gaze_Right_Abnormal=="C",Testing$Vestibular_Oculomotor_Gaze_Left_Abnormal,"NA")

Testing$Total_C<-rowSums(Testing[,c(3:7,11:34)]=="C")

#save(Testing,file="Vestib_Testing.RData")

Testing$na<-rowSums(is.na(Testing))
Testing<-Testing%>%
  filter(!na==34)

mymerge.x<-function(x,y){
  merge(x,y,all.x=TRUE,by="ID")
}

demo<-workbook_list$export_Demographics


final_df<-Reduce(mymerge.x,list(Testing,SOT,Hx,injury_counts,Surveys,demo))
save(final_df,file="final_df.RData")


##UPDATE 10.25.17 MEDICATIONS##
pt_meds<-read.xlsx("Control_Medications.xlsx",header=TRUE,sheetIndex = 1)
ct_meds<-read.xlsx("Control_Medications.xlsx",header=TRUE,sheetIndex = 2)

pt_meds<-merge(pt_meds,ct_meds,by="Meds",all.x=TRUE)
save(pt_meds,file="pt_meds.RData")


