s_dir <- "C:/Users/p1160248/Desktop/newCode"
s_file <- "VVClasses.R"
s_path <- paste(s_dir,"/", s_file, sep = "")
setwd(s_dir)
source(s_file)

Disease_Codee <- function (ST, df, ii) {
  return( ( ( grepl(ST[1], tolower(df$DX_CODEE[ii])) || grepl(ST[2], tolower(df$DX_CODEE[ii])) ||grepl(ST[3], tolower(df$DX_CODEE[ii]))||grepl(ST[4], tolower(df$DX_CODEE[ii]))||grepl(ST[5], tolower(df$DX_CODEE[ii]))||grepl(ST[6], tolower(df$DX_CODEE[ii]))||grepl(ST[7], tolower(df$DX_CODEE[ii]))||grepl(ST[8], tolower(df$DX_CODEE[ii]))||grepl(ST[9], tolower(df$DX_CODEE[ii]))||grepl(ST[10], tolower(df$DX_CODEE[ii])) ) && (df$TYPE_DX[ii]=="Definitive"))  )
}

Healthy_Codee<- function (HT, df, ii) {
  return( ( ( grepl(HT[1], tolower(df$DX_CODEE[ii])) || grepl(HT[2], tolower(df$DX_CODEE[ii]))  ) && (df$TYPE_DX[ii]=="Definitive"))  )
}

Disease_Comment <- function (ST, df, ii) {
  return(  ( grepl(ST[1], tolower(df$DX_COMMENT[ii])) || grepl(ST[2], tolower(df$DX_COMMENT[ii])) ||grepl(ST[3], tolower(df$DX_COMMENT[ii]))||grepl(ST[4], tolower(df$DX_COMMENT[ii]))||grepl(ST[5], tolower(df$DX_COMMENT[ii]))||grepl(ST[6], tolower(df$DX_COMMENT[ii]))||grepl(ST[7], tolower(df$DX_COMMENT[ii]))||grepl(ST[8], tolower(df$DX_COMMENT[ii]))||grepl(ST[9], tolower(df$DX_COMMENT[ii]))||grepl(ST[10], tolower(df$DX_COMMENT[ii])) )&&   (   ( (!grepl(ST[1], tolower(df$DX_CODEE[ii]))) && (!grepl(ST[2], tolower(df$DX_CODEE[ii])))&&(!grepl(ST[3], tolower(df$DX_CODEE[ii]))) && (!grepl(ST[4], tolower(df$DX_CODEE[ii]))) && (!grepl(ST[5], tolower(df$DX_CODEE[ii]))) && (!grepl(ST[6], tolower(df$DX_CODEE[ii]))) && (!grepl(ST[7], tolower(df$DX_CODEE[ii]))) && (!grepl(ST[8], tolower(df$DX_CODEE[ii]))) && (!grepl(ST[9], tolower(df$DX_CODEE[ii]))) && (!grepl(ST[10], tolower(df$DX_CODEE[ii]))) ) && (df$TYPE_DX[ii]=="Definitive") )  )
}

Healthy_Comment<- function (HT, df, ii) {
  return(  ( grepl(HT[1], tolower(df$DX_COMMENT[ii])) || grepl(HT[2], tolower(df$DX_COMMENT[ii])) )&& (   ( (!grepl(HT[1], tolower(df$DX_CODEE[ii]))) && (!grepl(HT[2], tolower(df$DX_CODEE[ii])))) && (df$TYPE_DX[ii]=="Definitive") )  )
}

#hema HÉMATOCRITE, HEMATOCRITE, HCT, HEMATOCRIT
PCV<- function (hema, df, ii) {
  return(  ( grepl(hema[1], tolower(df$NOM_RESULTATS[ii])) || grepl(hema[2], tolower(df$NOM_RESULTATS[ii])) ||grepl(hema[3], tolower(df$NOM_RESULTATS[ii]))||grepl(hema[4], tolower(df$NOM_RESULTATS[ii])) ||grepl(hema[5], tolower(df$NOM_RESULTATS[ii]))||grepl(hema[6], tolower(df$NOM_RESULTATS[ii])))  )
  
}

# CRÉATININE (UMOL/L), CRÉATININE (U=MMOL/L), CREATININE (UMOL/L), CREATININE (UMONL/L), CREA (UMOL/L), CRÉATININE (MMOL/L)
crea<- function (creat, df, ii) {
  return(  ( grepl(creat[1], tolower(df$NOM_RESULTATS[ii])) || grepl(creat[2], tolower(df$NOM_RESULTATS[ii])) ||grepl(creat[3], tolower(df$NOM_RESULTATS[ii]))||grepl(creat[4], tolower(df$NOM_RESULTATS[ii]))||grepl(creat[5], tolower(df$NOM_RESULTATS[ii]))||grepl(creat[6], tolower(df$NOM_RESULTATS[ii])) )  )
  
}

# urea, uree   URÉE(MMOL/L)    (note: Values in mg/dL should be converted to mmol/L)
#Urée (mmol/L)
# UREA (MMOL/L)
uree<- function (ure, df, ii) {
  return(  ( grepl(ure[1], tolower(df$NOM_RESULTATS[ii])) || grepl(ure[2], tolower(df$NOM_RESULTATS[ii])) || grepl(ure[3], tolower(df$NOM_RESULTATS[ii]))))
  
}

# BUN (mg/dL) , RÉSULTAT BUN (MG/DL)
BUN<- function (bu, df, ii) {
  return(  ( grepl(bu[1], tolower(df$NOM_RESULTATS[ii])) || grepl(bu[2], tolower(df$NOM_RESULTATS[ii])) ))
  
}
# ICA (MMOL/L), CALCIUM IONISÉ (MMOL/L),  CA++ (MMOL/L)
ICA<- function (ca, df, ii) {
  return(  ( grepl(ca[1], tolower(df$NOM_RESULTATS[ii])) || grepl(ca[2], tolower(df$NOM_RESULTATS[ii]))|| grepl(ca[3], tolower(df$NOM_RESULTATS[ii]))))
  
}
# NA (MMOL/L), NA+ (mmol/L),SODIUM (MMOL/L)
NAp<- function (n, df, ii) {
  return(  ( grepl(n[1], tolower(df$NOM_RESULTATS[ii])) || grepl(n[2], tolower(df$NOM_RESULTATS[ii]))|| grepl(n[3], tolower(df$NOM_RESULTATS[ii]))))
  
}
# K (MMOL/L), K+ (mmol/L), POTASSIUM (MMOL/L)
Kp<- function (k, df, ii) {
  return(  ( grepl(k[1], tolower(df$NOM_RESULTATS[ii])) || grepl(k[2], tolower(df$NOM_RESULTATS[ii]))|| grepl(k[3], tolower(df$NOM_RESULTATS[ii]))))
  
}
# HC03 (MMOL/L), HCO3 (22-28 MMOL/L),HC03
hco<- function (h, df, ii) {
  return(  ( grepl(h[1], tolower(df$NOM_RESULTATS[ii])) || grepl(h[2], tolower(df$NOM_RESULTATS[ii]))|| grepl(h[3], tolower(df$NOM_RESULTATS[ii])) || grepl(h[4], tolower(df$NOM_RESULTATS[ii]))  ))
  
}
# BLOOD PH (7.35-7.45), BLOOD PH (7,35-7,45), PH, PH SANGUIN (7.35-7.45), PH Sanguin (7.35-7.45)
php<- function (ph, df, ii) {
  return(  ( grepl(ph[1], tolower(df$NOM_RESULTATS[ii])) || grepl(ph[2], tolower(df$NOM_RESULTATS[ii]))|| grepl(ph[3], tolower(df$NOM_RESULTATS[ii]))|| grepl(ph[4], tolower(df$NOM_RESULTATS[ii]))|| grepl(ph[5], tolower(df$NOM_RESULTATS[ii]))      ))
  
}

 all_Disease_CodComm <- function (data, data2) {
   cobj<-c()
   for(i in 1:length(data)){
         ob<-SV_Disease$new(name=data$DX_CODEE[i], date=data$DERNIERE_MODIF_LE[i], ID_patient=data$UID_NUMERO_PATIENT[i], ID_visite=data$UID_VISITE[i])
         cobj<-append(cobj, ob)
          }
   for(i in 1:length(data2)){
         ob<-SV_Disease$new(name=data2$DX_COMMENT[i], date=data2$DERNIERE_MODIF_LE[i], ID_patient=data2$UID_NUMERO_PATIENT[i], ID_visite=data2$UID_VISITE[i])
         cobj<-append(cobj, ob)
          }

   return( cobj)
 }

Disease_CodComm <- function (ST, df) {
  cobj<-c()
  for(ii in 1:length(df$DX_CODEE)){
    
    if(!is.na(df$DX_CODEE[ii])) {
      
      if (Disease_Codee(ST, df, ii) )
      {
        ob<-SV_Disease$new(name=df$DX_CODEE[ii], date=df$DERNIERE_MODIF_LE[ii], ID_patient=df$UID_NUMERO_PATIENT[ii], ID_visite=df$UID_VISITE[ii])
        cobj<-append(cobj, ob)
        
      } }
  }
  for(ii in 1:length(df$DX_CODEE)){
    if (Disease_Comment(ST, df, ii) )
    {
      cobj<-append(cobj, SV_Disease$new(name=df$DX_COMMENT[ii], date=df$DERNIERE_MODIF_LE[ii], ID_patient=df$UID_NUMERO_PATIENT[ii], ID_visite=df$UID_VISITE[ii]))
    }
  }
  return( cobj)
}

Healthy_CodComm <- function (HT, df) {
  cobj<-c()
  #print(length(df$DX_CODEE))
  for(ii in 1:length(df$DX_CODEE)){
    
    if(!is.na(df$DX_CODEE[ii])) {
      #print(ii)
      if (Healthy_Codee(HT, df, ii) )
      {
        ob<-SV_Disease$new(name=df$DX_CODEE[ii], date=df$DERNIERE_MODIF_LE[ii], ID_patient=df$UID_NUMERO_PATIENT[ii], ID_visite=df$UID_VISITE[ii])
        cobj<-append(cobj, ob)
        
        
      } }
  }
  for(ii in 1:length(df$DX_CODEE)){
    if (Healthy_Comment(HT, df, ii) )
    {
      cobj<-append(cobj, SV_Disease$new(name=df$DX_COMMENT[ii], date=df$DERNIERE_MODIF_LE[ii], ID_patient=df$UID_NUMERO_PATIENT[ii], ID_visite=df$UID_VISITE[ii]))
    }
  }
  return( cobj)
}

#s_file1="1nov2022ProjetAI_patientDemographicsW.csv"
#s_dir1="D:/WIAAgrosante/Dunn-Pablo/Project_Obstruction Urethral"
DemmoPatients<- function (disease_obj, s_dir1, s_file1) {
  s_path1 <- paste(s_dir1,"/", s_file1, sep = "")
  df<- read.csv(s_path1, sep = ";", encoding="UTF-8", header=TRUE)
  cobj<-c()
  for(i in 1:length(disease_obj)){
    for(ii in 1:length(df$UID_NUMERO_PATIENT)){
      if (disease_obj[[i]]$get_patient()==df$UID_NUMERO_PATIENT[ii])
      {
        cobj<-append(cobj, Demo$new(Race=df$RACE[ii], Sexe=df$SEXE[ii], Couleur=df$COULEUR[ii], date=disease_obj[[i]]$get_date(), dateN=df$DATE_NAISSANCE[ii], ID_patient=df$UID_NUMERO_PATIENT[ii]))
        
      }
    }
  }
  return(cobj)
}

s_file1="1nov2022ProjetAI_signesVitauxW.csv"
s_dir1="D:/WIAAgrosante/Dunn-Pablo/Project_Obstruction Urethral"
SVPatients<- function (disease_obj, s_dir1, s_file1) {
  s_path <- paste(s_dir1,"/", s_file1, sep = "")
  print( s_path)
  
  df<- read.csv(s_path, sep = ";", encoding="UTF-8", header=TRUE)
  cobj<-c()
  
  for(ii in 1:length(disease_obj)){
    for(i in 1:length(df$UID_NUMERO_PATIENT)){
      
      if((!is.na(df$UID_NUMERO_PATIENT[i])&&(!is.na(df$UID_VISITE[i]))) ) {
        if ((disease_obj[[ii]]$get_patient()==df$UID_NUMERO_PATIENT[i])&&(disease_obj[[ii]]$get_visite()==df$UID_VISITE[i]))
        {
          
          dt=as.numeric(difftime( ymd(substring(disease_obj[[ii]]$get_date(), 1, 10)), ymd(substring(df$ENTREE_LE[i],1,10)), units = "days"))
          
          if ((dt<160)&&(dt >-20)){
            disease_obj[[ii]]$set_SVdate(df$ENTREE_LE[i])
            disease_obj[[ii]]$set_svisite(df$UID_VISITE[i])
            disease_obj[[ii]]$set_pulse(df$POULS[i])
            disease_obj[[ii]]$set_respiration(df$RESPIRATION[i])
            disease_obj[[ii]]$set_weight(df$POIDS_G[i])
            print(dt)
            #print(disease_obj[[ii]]$get_name())
            cobj<-append(cobj, disease_obj[[ii]])
          }
        }
      }
    }
  }
  return(cobj)
}

#
# s_file1b="requetes-items-1b.csv"
# s_dir1b="D:/WIAAgrosante/Dunn-Pablo/request-extracts"
ReqPatients<- function (robj, s_dir1, s_file1, sv=TRUE, comm=TRUE) {
  s_path <- paste(s_dir1,"/", s_file1, sep = "")
  df<- read.csv(s_path, sep = ";", encoding="UTF-8", header=TRUE)
  
  hema <-tolower(c("HÉMATOCRITE", "HEMATOCRITE", "*VIA HCT", "HCT (%)", "HCT (%PCV)", "HEMATOCRIT"))
  creat <-tolower(c("CRÉATININE (UMOL/L)", "CRÉATININE (U=MMOL/L)", "CREATININE (UMOL/L)", "CREATININE (UMONL/L)", "CREA (UMOL/L)","CRÉATININE (MMOL/L)"))
  ure <-tolower(c("URÉE(MMOL/L)","Urée (mmol/L)", "UREA (MMOL/L)"))
  bu <-tolower(c("BUN (mg/dL)" , "RÉSULTAT BUN (MG/DL)"))
  ca <-tolower(c("ICA (MMOL/L)", "CALCIUM IONISÉ (MMOL/L)",  "CA\\+\\+ (MMOL/L)"))
  n <-tolower(c("NA (MMOL/L)","NA\\+ (mmol/L)","SODIUM (MMOL/L)"))
  k <-tolower(c("K (MMOL/L)", "K\\+ (mmol/L)", "POTASSIUM (MMOL/L)"))
  h<-tolower(c( "HC03 (MMOL/L)", "HCO3 (22-28 MMOL/L)","HC03", "HCO3 (MMOL/L)" ))
  ph<-tolower(c("BLOOD PH (7.35-7.45)", "BLOOD PH (7,35-7,45)", "PH", "PH SANGUIN (7.35-7.45)", "PH Sanguin (7.35-7.45)"))
  
  for(ii in 1:length(robj)){
    for(i in 1:length(df$UID_NUMERO_PATIENT)){
      
      if ((!is.na(df$UID_NUMERO_PATIENT[i])&&(!is.na(df$UID_VISITE[i]))) ) { #((!is.na(df$UID_NUMERO_PATIENT[i])&&(!is.na(df$UID_VISITE[i]))) ) {
        
        vis<-ifelse(sv,robj[[ii]]$get_visite(), robj[[ii]]$get_svisite())
        if ((robj[[ii]]$get_patient()==df$UID_NUMERO_PATIENT[i])&&(vis==df$UID_VISITE[i])) # ((robj[[ii]]$get_patient()==df$UID_NUMERO_PATIENT[i])&&(vis==df$UID_VISITE[i]))
        {
          #print(robj[[ii]]$get_patient())
          if(PCV(hema, df, i)&&length(df$OBSERVATIONS[i])>0) {
            robj[[ii]]$set_pcv(ifelse(comm,df$OBSERVATIONS[i],df$COMMENTAIRES_RESULTATS[i]))
          } else if(crea(creat, df, i)&&length(df$OBSERVATIONS[i])>0) {
            robj[[ii]]$set_pcv(ifelse(comm,df$OBSERVATIONS[i],df$COMMENTAIRES_RESULTATS[i]))
          } else if (uree(ure, df, i)&&length(df$OBSERVATIONS[i])>0)  {
            robj[[ii]]$set_ure(ifelse(comm,df$OBSERVATIONS[i],df$COMMENTAIRES_RESULTATS[i]))
          } else  if (BUN(bu, df, i)&&length(df$OBSERVATIONS[i])>0) { #*2.80112
            robj[[ii]]$set_ure(ifelse(comm,df$OBSERVATIONS[i],df$COMMENTAIRES_RESULTATS[i]))
          } else if (ICA(ca, df, i)&&length(df$OBSERVATIONS[i])>0) {
            robj[[ii]]$set_iCa(ifelse(comm,df$OBSERVATIONS[i],df$COMMENTAIRES_RESULTATS[i]))
          } else if (NAp(n, df, i)&&length(df$OBSERVATIONS[i])>0) {
            robj[[ii]]$set_N(ifelse(comm,df$OBSERVATIONS[i],df$COMMENTAIRES_RESULTATS[i]))
          } else  if (Kp(k, df, i)&&length(df$OBSERVATIONS[i])>0) {
            robj[[ii]]$set_K(ifelse(comm,df$OBSERVATIONS[i],df$COMMENTAIRES_RESULTATS[i]))
          } else  if ( hco(h, df, i)&&length(df$OBSERVATIONS[i])>0) {
            robj[[ii]]$set_HCO3(ifelse(comm,df$OBSERVATIONS[i],df$COMMENTAIRES_RESULTATS[i]))
          } else  if (php(ph, df, i)&&length(df$OBSERVATIONS[i])>0) {
            robj[[ii]]$set_pH(ifelse(comm,df$OBSERVATIONS[i],df$COMMENTAIRES_RESULTATS[i]))
          }
        }
      }
    }
  }
  return(robj)
}

ReqPatients2<- function (robj, s_dir1, s_file1, d, sv=TRUE, comm=TRUE) {
  s_path <- paste(s_dir1,"/", s_file1, sep = "")
  df<- read.csv(s_path, sep = ";", encoding="UTF-8", header=TRUE)
  
  hema <-tolower(c("HÉMATOCRITE", "HEMATOCRITE", "*VIA HCT", "HCT (%)", "HCT (%PCV)", "HEMATOCRIT"))
  creat <-tolower(c("CRÉATININE (UMOL/L)", "CRÉATININE (U=MMOL/L)", "CREATININE (UMOL/L)", "CREATININE (UMONL/L)", "CREA (UMOL/L)","CRÉATININE (MMOL/L)"))
  ure <-tolower(c("URÉE(MMOL/L)","Urée (mmol/L)", "UREA (MMOL/L)"))
  bu <-tolower(c("BUN (mg/dL)" , "RÉSULTAT BUN (MG/DL)"))
  icap <-tolower(c("ICA (MMOL/L)", "CALCIUM IONISÉ (MMOL/L)", "CA\\+\\+ (MMOL/L)"))
  n <-tolower(c("NA (MMOL/L)","NA\\+ (mmol/L)","SODIUM (MMOL/L)"))
  kplus <-tolower(c("K (MMOL/L)", "K\\+ (mmol/L)", "POTASSIUM (MMOL/L)"))
  h<-tolower(c( "HC03 (MMOL/L)", "HCO3 (22-28 MMOL/L)","HC03", "HCO3 (MMOL/L)" ))
  ph<-tolower(c("BLOOD PH (7.35-7.45)", "BLOOD PH (7,35-7,45)", "PH", "PH SANGUIN (7.35-7.45)", "PH Sanguin (7.35-7.45)"))
  cpcv<-c()
  cpcv2<-c()
  cpat<-c()
  ccrea<-c()
  ccrea2<-c()
  crpat<-c()
  cure<-c()
  cure2<-c()
  cupat<-c()
  cbun<-c() #*2.80112
  cbun2<-c()
  bupat<-c()
  ca<-c()
  ca2<-c()
  cat<-c()
  na<-c()
  na2<-c()
  cnat<-c()
  k<-c()
  k2<-c()
  ckat<-c()
  hc<-c()
  hc2<-c()
  chat<-c()
  phc<-c()
  phc2<-c()
  phat<-c()
  
  vpat<-c()
  vrpat<-c()
  vupat<-c()
  vunpat<-c()
  vat<-c()
  vnat<-c()
  vkat<-c()
  vhat<-c()
  pvat<-c()
  
  for(ii in 1:length(robj)){
    for(i in 1:length(df$UID_NUMERO_PATIENT)){
      
      if(!is.na(df$UID_NUMERO_PATIENT[i])) { # ((!is.na(df$UID_NUMERO_PATIENT[i])&&(!is.na(df$UID_VISITE[i]))) ) {
        
        vis<-ifelse(sv,robj[[ii]]$get_visite(), robj[[ii]]$get_svisite())
        if (robj[[ii]]$get_patient()==df$UID_NUMERO_PATIENT[i]) # ((robj[[ii]]$get_patient()==df$UID_NUMERO_PATIENT[i])&&(vis==df$UID_VISITE[i]))
        {
          #print(robj[[ii]]$get_patient())
          #print(ca[1])
          #print(df$NOM_RESULTATS[i])
          if(PCV(hema, df, i)) {
            cpcv<-c(cpcv, df$OBSERVATIONS[i])
            cpcv2<-c(cpcv2, df$COMMENTAIRES_RESULTATS[i])
            cpat<-c(cpat, df$UID_NUMERO_PATIENT[i])
            vpat<-c(vpat,df$UID_VISITE[i]) #vpat vrpat vupat vunpat vat
          }                    #vnat, vkat, vhat, pvat,
          else if(crea(creat, df, i)) {
            ccrea<-c(ccrea, df$OBSERVATIONS[i])
            ccrea2<-c(ccrea2, df$COMMENTAIRES_RESULTATS[i])
            crpat<-c(crpat, df$UID_NUMERO_PATIENT[i])
            vrpat<-c(vrpat,df$UID_VISITE[i])
            
          } else if (uree(ure, df, i))  {
            cure<-c(cure, df$OBSERVATIONS[i])
            cure2<-c(cure2, df$COMMENTAIRES_RESULTATS[i])
            cupat<-c(cupat, df$UID_NUMERO_PATIENT[i])
            vupat<-c(vupat,df$UID_VISITE[i])
          } else  if (BUN(bu, df, i)) {
            cbun<-c(cbun, df$OBSERVATIONS[i]) #*2.80112
            cbun2<-c(cbun2, df$COMMENTAIRES_RESULTATS[i])
            bupat<-c(bupat, df$UID_NUMERO_PATIENT[i])
            vunpat<-c(vunpat,df$UID_VISITE[i])
          } else if (ICA(icap, df, i)) {
            ca<-c(ca, df$OBSERVATIONS[i])
            ca2<-c(ca2, df$COMMENTAIRES_RESULTATS[i])
            cat<-c(cat, df$UID_NUMERO_PATIENT[i])
            vat<-c(vat,df$UID_VISITE[i])
          } else if (NAp(n, df, i)) {
            na<-c(na, df$OBSERVATIONS[i])
            na2<-c(na2, df$COMMENTAIRES_RESULTATS[i])
            cnat<-c(cnat, df$UID_NUMERO_PATIENT[i])
            vnat<-c(vnat,df$UID_VISITE[i])
          } else  if (Kp(kplus, df, i)) {
            k<-c(k, df$OBSERVATIONS[i])
            k2<-c(k2, df$COMMENTAIRES_RESULTATS[i])
            ckat<-c(ckat, df$UID_NUMERO_PATIENT[i])
            vkat<-c(vkat,df$UID_VISITE[i])
          } else  if ( hco(h, df, i)) {
            hc<-c(hc, df$OBSERVATIONS[i])
            hc2<-c(hc2, df$COMMENTAIRES_RESULTATS[i])
            chat<-c(chat, df$UID_NUMERO_PATIENT[i])
            vhat<-c(vhat,df$UID_VISITE[i])
          } else  if (php(ph, df, i)) {
            phc<-c(phc, df$OBSERVATIONS[i])
            phc2<-c(phc2, df$COMMENTAIRES_RESULTATS[i])
            phat<-c(phat, df$UID_NUMERO_PATIENT[i])
            pvat<-c(pvat,df$UID_VISITE[i])
          }
        }
      }
    }
  }
  s_dir<- "C:/Users/p1160248/Desktop/newCode"
  df <- data.frame(cpat,vpat , cpcv, cpcv2)
  # names(df) <- c("Pat","pcv","pcv2")
  #vpat vrpat vupat vunpat vat
  #vnat, vkat, vhat, pvat,
  s_path <- paste(s_dir,"/","pcv",d, ".csv", sep = "")
  write.csv(df, s_path,row.names=FALSE)
  
  df <- data.frame(crpat,vrpat , ccrea, ccrea2)
  #names(df) <- c("Pat","crea","crea2")
  s_path <- paste(s_dir,"/","creatinine",d, ".csv", sep = "")
  write.csv(df, s_path,row.names=FALSE)
  
  df <- data.frame(cupat,vupat, cure, cure2)
  #names(df) <- c("Pat","ure","ure2")
  s_path <- paste(s_dir,"/","uree",d, ".csv", sep = "")
  write.csv(df, s_path,row.names=FALSE)
  
  df <- data.frame(bupat,vunpat, cbun, cbun2)
  #names(df) <- c("Pat","bun","bun2")
  s_path <- paste(s_dir,"/","bun",d, ".csv", sep = "")
  write.csv(df, s_path,row.names=FALSE)
  
  df <- data.frame(cat,vat, ca, ca2)
  # names(df) <- c("Pat","ca","ca2")
  s_path <- paste(s_dir,"/","ca",d, ".csv", sep = "")
  write.csv(df, s_path,row.names=FALSE)
  
  df <- data.frame(cnat,vnat, na, ca2)
  #names(df) <- c("Pat","na","na2")
  s_path <- paste(s_dir,"/","na",d, ".csv", sep = "")
  write.csv(df, s_path,row.names=FALSE)
  
  df <- data.frame(ckat,vkat, k, k2)
  #names(df) <- c("Pat","k","k2")
  s_path <- paste(s_dir,"/","K",d, ".csv", sep = "")
  write.csv(df, s_path,row.names=FALSE)
  
  df <- data.frame(chat,vhat, hc, hc2)
  #names(df) <- c("Pat","hco3","hco3_2")
  s_path <- paste(s_dir,"/","HCO3",d, ".csv", sep = "")
  write.csv(df, s_path,row.names=FALSE)
  
  df <- data.frame(phat,pvat,phc, phc2)
  #names(df) <- c("Pat","ph","ph_2")
  s_path <- paste(s_dir,"/","ph",d, ".csv", sep = "")
  write.csv(df, s_path,row.names=FALSE)
  
}

print_Diseasefile<- function (obj, s_dir, s_file ) {
  name<-c()
  date<-c()
  patient<-c()
  visite<-c()
  for(i in 1:length(obj)) {
    o<-obj[[i]]$get_name()
    name<-append(name,o)
    date<-append(date,obj[[i]]$get_date())
    patient<-append(patient,obj[[i]]$get_patient())
    visite<-append(visite,obj[[i]]$get_visite())
  }
  df <- data.frame(name,date, patient, visite)
  names(df) <- c("Name","Date","ID_patient","ID_visite")
  s_path <- paste(s_dir,"/", s_file, sep = "")
  # ec<-file(s_path, encoding="UTF-8")
  write.csv(df, s_path,row.names=FALSE)
}
print_ParamDemoDisease<- function (obj_demo, s_dir, s_file ) {
  s_pathr <- paste(s_dir,"/", s_file, sep = "")
  
  breed<-c()
  sex<-c()
  patient_demo<-c()
  color<-c()
  age<-c()
  for(i in 1:length(obj_demo)) {
    
    breed<-append(breed,obj_demo[[i]]$get_breed())
    sex<-append(sex,obj_demo[[i]]$get_sex())
    patient_demo<-append(patient_demo, obj_demo[[i]]$get_patient())
    color<-append(color, obj_demo[[i]]$get_color())
    age<-append(age,  obj_demo[[i]]$get_age())
  }
  
  df_demo <- data.frame(patient_demo, breed, sex, color, age)
  names(df_demo) <- c("ID_patient", "breed", "sex", "color", "age")
  write.csv(df_demo, s_pathr,row.names=FALSE)
  
  
}

print_ParamSVDisease<- function (obj, s_dir, s_file) {
  
  svisite<-c()
  SVdate<-c()
  pulse<-c()
  respiration<-c()
  weight<-c()
  for(i in 1:length(obj)) {
    svisite<-append(svisite, obj[[i]]$get_svisite())
    SVdate<-append(SVdate, obj[[i]]$get_SVdate())
    pulse<-append(pulse, obj[[i]]$get_pulse())
    respiration <-append(respiration, obj[[i]]$get_respiration())
    weight<-append(weight,  obj[[i]]$get_weight())
  }
  print(length(svisite))
  print(length(SVdate))
  print(length(pulse))
  print(length(respiration))
  print(length(weight))
  
  df <- data.frame(svisite, SVdate, pulse, respiration, weight)
  names(df) <- c("ID_svisite", "SV_Date", "Pulse", "Respiration", "Weight")
  s_path <- paste(s_dir,"/", s_file, sep = "")
  write.csv(df, s_path,row.names=FALSE)
  
  
}

print_ParamDisease<- function (obj, s_dir, s_file ) {
  
  name<-c()
  date<-c()
  patient<-c()
  visite<-c()
  
  svisite<-c()
  SVdate<-c()
  pulse<-c()
  respiration<-c()
  weight<-c()
  
  pcv<-c()
  cre<-c()
  ure<-c()
  iCa<-c()
  N<-c()
  K<-c()
  pH<-c()
  HCO3<-c()
  
  for(i in 1:length(obj)) {
    # print(obj[[i]]$get_name())
    name<-append(name,obj[[i]]$get_name())
    date<-append(date,obj[[i]]$get_date())
    patient<-append(patient,obj[[i]]$get_patient())
    visite<-append(visite,obj[[i]]$get_visite())
    
    svisite<-append(svisite, obj[[i]]$get_svisite())
    SVdate<-append(SVdate, obj[[i]]$get_SVdate())
    pulse<-append(pulse, obj[[i]]$get_pulse())
    respiration <-append(respiration, obj[[i]]$get_respiration())
    weight<-append(weight,  obj[[i]]$get_weight())
    
    pcv<-append(pcv, obj[[i]]$get_pcv())
    cre<-append(cre, obj[[i]]$get_cre())
    ure<-append(ure, obj[[i]]$get_ure())
    iCa<-append(iCa, obj[[i]]$get_iCa())
    N<-append(N,obj[[i]]$get_N())
    K<-append(K,obj[[i]]$get_K())
    pH<-append(pH,obj[[i]]$get_pH())
    HCO3<-append(HCO3,obj[[i]]$get_HCO3())
  }
  
  
  df <- data.frame(name, date, patient, visite, svisite, SVdate, pulse, respiration, weight, pcv, cre, ure, iCa, N, K, pH, HCO3)
  names(df) <- c("name","date" , "ID_patient", "ID_visite","ID_svisite", "SV_Date", "Pulse", "Respiration", "Weight", "Pcv", "Cre", "Ure", "ICa", "N", "K", "PH", "HCO3")
  s_path <- paste(s_dir,"/", s_file, sep = "")
  write.csv(df, s_path,row.names=FALSE)
  
  
}

ReqPatientsAll<- function (robj, s_dir1, s_file1, d) {
  s_path <- paste(s_dir1,"/", s_file1, sep = "")
  df<- read.csv(s_path, sep = ";", encoding="UTF-8", header=TRUE)
  
  cpres<-c()
  cppres<-c()
  cpcv<-c()
  cpcv2<-c()
  cpat<-c()
  vpat<-c()
  
  for(ii in 1:length(robj)){
    for(i in 1:length(df$UID_NUMERO_PATIENT)){
      
      if(!is.na(df$UID_NUMERO_PATIENT[i])) { # ((!is.na(df$UID_NUMERO_PATIENT[i])&&(!is.na(df$UID_VISITE[i]))) ) {
        
        vis<-ifelse(sv,robj[[ii]]$get_visite(), robj[[ii]]$get_svisite())
        if ((robj[[ii]]$get_patient()==df$UID_NUMERO_PATIENT[i])&&(0<length(df$OBSERVATIONS[i])||0<length(df$COMMENTAIRES_RESULTATS[i]))) # ((robj[[ii]]$get_patient()==df$UID_NUMERO_PATIENT[i])&&(vis==df$UID_VISITE[i]))
        {
            
            cppres<-c(cppres, robj[[ii]]$get_name())
            cpres<-c(cpres, df$NOM_RESULTATS[i]) 
            cpcv<-c(cpcv, df$OBSERVATIONS[i])
            cpcv2<-c(cpcv2, df$COMMENTAIRES_RESULTATS[i])
            cpat<-c(cpat, df$UID_NUMERO_PATIENT[i])
            vpat<-c(vpat,df$UID_VISITE[i]) #vpat vrpat vupat vunpat vat
          
        }
      }
    }
  }
  s_dir<- "C:/Users/p1160248/Desktop/newCode"
  df <- data.frame(cpat,vpat, cppres, cpres, cpcv, cpcv2)
  names(df) <- c("Pat","visitePat","nom", "nom_resultats", "observations","commentaires_res")
 
  s_path <- paste(s_dir,"/","all", d, ".csv", sep = "")
  write.csv(df, s_path,row.names=FALSE)
  
}

ReqPatientsSuperAll<- function (s_dir1, s_file1, d) {
  s_path <- paste(s_dir1,"/", s_file1, sep = "")
  df<- read.csv(s_path, sep = ";", encoding="UTF-8", header=TRUE)
  
  cpres<-c()
  cppres<-c()
  cpcv<-c()
  cpcv2<-c()
  cpat<-c()
  vpat<-c()
  
    for(i in 1:length(df$UID_NUMERO_PATIENT)){
      
      if(!is.na(df$UID_NUMERO_PATIENT[i])) { # ((!is.na(df$UID_NUMERO_PATIENT[i])&&(!is.na(df$UID_VISITE[i]))) ) {
        
        if ((0<length(df$OBSERVATIONS[i])||0<length(df$COMMENTAIRES_RESULTATS[i]))) # ((robj[[ii]]$get_patient()==df$UID_NUMERO_PATIENT[i])&&(vis==df$UID_VISITE[i]))
        {
          
       
          cpres<-c(cpres, df$NOM_RESULTATS[i]) 
          cpcv<-c(cpcv, df$OBSERVATIONS[i])
          cpcv2<-c(cpcv2, df$COMMENTAIRES_RESULTATS[i])
          cpat<-c(cpat, df$UID_NUMERO_PATIENT[i])
          vpat<-c(vpat,df$UID_VISITE[i]) #vpat vrpat vupat vunpat vat
          
        }
      }
    }
  
  s_dir<- "C:/Users/p1160248/Desktop/newCode"
  df <- data.frame(cpat,vpat, cpres, cpcv, cpcv2)
  names(df) <- c("Pat","visitePat","nom_resultats", "observations","commentaires_res")
  
  s_path <- paste(s_dir,"/","superall", d, ".csv", sep = "")
  write.csv(df, s_path,row.names=FALSE)
  
}


 all_Disease_CodComm <- function (data, data2) {
   cobj<-c()
   for(i in 1:length(data)){
         ob<-SV_Disease$new(name=data$DX_CODEE[i], date=data$DERNIERE_MODIF_LE[i], ID_patient=data$UID_NUMERO_PATIENT[i], ID_visite=data$UID_VISITE[i])
         cobj<-append(cobj, ob)
          }
   for(i in 1:length(data2)){
         ob<-SV_Disease$new(name=data2$DX_COMMENT[i], date=data2$DERNIERE_MODIF_LE[i], ID_patient=data2$UID_NUMERO_PATIENT[i], ID_visite=data2$UID_VISITE[i])
         cobj<-append(cobj, ob)
          }

   return( cobj)
 }