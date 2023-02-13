rm(list = ls())
library(lubridate)
setwd("C:/Users/p1160248/Desktop/newCode")

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("C:/Users/p1160248/Desktop/newCode/VVdrivers.R")

df <- read.csv("C:/Users/p1160248/Desktop/Project_Obstruction Urethral/1nov2022ProjetAI_dxW.csv", sep = ";", encoding="UTF-8", header=TRUE)
ST<-c('azotemia', 'azotémie','acute kidney injury', 'aki', 'pyelonephritis','pyélonéphrite','insuffisance rénale', 'renal insufficiency','ckd','maladie rénale', 'maladie rénale chronique')
HT<-c('examen normal vaccination','vaccination')

data<-df[!(df$DX_CODEE %in% c('Examen Normal Pré-Opératoire', 'Examen Normal','Examen Normal Vaccination','Vaccination','Voir Plus Bas' , 'Diagnostic(S) Primaire(S)','Codage De Diagnostic','Diagnostic(S) Secondaire(S)', 'Diagnostic ouvert')),]
data2<-df[df$DX_CODEE %in% c('Examen Normal Pré-Opératoire', 'Examen Normal','Examen Normal Vaccination','Vaccination','Voir Plus Bas' , 'Diagnostic(S) Primaire(S)','Codage De Diagnostic','Diagnostic(S) Secondaire(S)', 'Diagnostic ouvert'), ]


# co<-Disease_CodComm(ST, df)
# s_file<- "fUOF.csv"
# s_dir <- "D:/WIAAgrosante/Dunn-Pablo/newCode"
# print_Diseasefile(co,s_dir, s_file)

co<-Healthy_CodComm(HT, df)
s_file<- "fUOF_healthy.csv"
s_dir <- "C:/Users/p1160248/Desktop/newCode"
print_Diseasefile(co,s_dir, s_file)



s_file1="1nov2022ProjetAI_patientDemographicsW.csv"
s_dir1="C:/Users/p1160248/Desktop/Project_Obstruction Urethral"
obj_demo<-DemmoPatients(co, s_dir1, s_file1)

s_file<- "demoPatients.csv"
s_dir <- "C:/Users/p1160248/Desktop/newCode"
print_ParamDemoDisease(obj_demo, s_dir, s_file)

s_file1="1nov2022ProjetAI_signesVitauxW.csv"
s_dir1="C:/Users/p1160248/Desktop/Project_Obstruction Urethral"
co2<-SVPatients(co, s_dir1, s_file1)

s_file<- "SVPatients.csv"
s_dir <- "C:/Users/p1160248/Desktop/newCode"
print_ParamSVDisease(co2, s_dir, s_file)

objAll<-all_Disease_CodComm(data, data2) 

s_file1b="requetes-items-1b.csv"  ##
s_dirb="C:/Users/p1160248/Desktop/request-extracts"
# ReqPatients2(co2, s_dirb, s_file1b, 1)
co1b<-ReqPatients(co2, s_dirb, s_file1b, sv=TRUE, comm=FALSE) ##

s_file<- "Req1_Patients.csv" ##
s_dir <- "C:/Users/p1160248/Desktop/newCode" ##
print_ParamDisease(co1b, s_dir, s_file)   ##

#ReqPatientsSuperAll(s_dirb, s_file1b, 1)
ReqPatientsAll(objAll, s_dirb, s_file1b, 1)

s_file2b="requetes-items-2b.csv"
#ReqPatients2(co2, s_dirb, s_file2b, 2)
co2b<-ReqPatients(co1b, s_dirb, s_file2b, sv=TRUE, comm=FALSE) ##

s_file<- "Req2_Patients.csv"  ##
s_dir <- "C:/Users/p1160248/Desktop/newCode" ##
print_ParamDisease(co2b, s_dir, s_file)          ##

#ReqPatientsSuperAll(s_dirb, s_file2b, 2)
ReqPatientsAll(objAll, s_dirb, s_file2b, 2)

s_file3b="requetes-items-3b.csv"
#ReqPatients2(co2, s_dirb, s_file3b, 3)
co3b<-ReqPatients(co2b, s_dirb, s_file3b, sv=TRUE, comm=FALSE) ##

s_file<- "Req3_Patients.csv"  ##
s_dir <- "C:/Users/p1160248/Desktop/newCode"  ##
print_ParamDisease(co3b, s_dir, s_file)    ##

#ReqPatientsSuperAll(s_dirb, s_file3b, 3)
ReqPatientsAll(objAll, s_dirb, s_file3b, 3)

s_file4b="requetes-items-4b.csv"
#ReqPatients2(co2, s_dirb, s_file4b, 4)
co4b<-ReqPatients(co3b, s_dirb, s_file4b, sv=TRUE, comm=FALSE) ##

s_file<- "Req4_Patients.csv"  ##
s_dir <- "C:/Users/p1160248/Desktop/newCode" ##
print_ParamDisease(co4b, s_dir, s_file)  ##

#ReqPatientsSuperAll(s_dirb, s_file4b, 4)
ReqPatientsAll(objAll, s_dirb, s_file4b, 4)

s_file5b="requetes-items-5b.csv"
#ReqPatients2(co2, s_dirb, s_file5b, 5)
co5b<-ReqPatients(co4b, s_dirb, s_file5b, sv=TRUE, comm=FALSE) ##

s_file<- "Req5_Patients.csv"  ##
s_dir <- "C:/Users/p1160248/Desktop/newCode" ##
print_ParamDisease(co5b, s_dir, s_file)  ##

#ReqPatientsSuperAll(s_dirb, s_file5b, 5)
ReqPatientsAll(objAll, s_dirb, s_file5b, 5)
