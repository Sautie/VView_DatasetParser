rm(list = ls())
#setwd("C:/Users/p1160248/Desktop/newCode")
print(getwd())

library(R6)

Disease <- R6Class(
  classname = "Disease",
  public = list(
    # name=NULL,
    # date=NULL,
    # ID_patient=NULL,
    # ID_visite=NULL,
    initialize = function(name, date, ID_patient, ID_visite) {
      stopifnot(is.character(name),length(name)==1)
      stopifnot(is.character(date),length(date)==1)
      stopifnot(is.numeric(ID_patient),length(ID_patient)==1)
      stopifnot(is.numeric(ID_visite),length(ID_visite)==1)
      private$name <- name
      private$date <- date
      private$ID_patient <-ID_patient
      private$ID_visite <- ID_visite
    },
    set_name = function(name) {
      private$name = name
    },
    set_date = function(date) {
      private$date = date
    },
    set_patient = function(ID_patient) {
      private$ID_patient = ID_patient
    },
    set_visite= function(ID_visite) {
      private$ID_visite=ID_visite
    },
    
    get_name = function() {
      return(private$name)
    },
    get_date = function() {
      return(private$date)
    },
    get_patient= function() {
      private$ID_patient
    },
    get_visite= function() {
      private$ID_visite
    },
    print = function(...) {
      cat("Diseaswe: \n")
      cat("  Name: ", private$name, "\n", sep = "")
      cat("  Date:  ", private$date, "\n", sep = "")
      cat("  ID_Patient: ", private$ID_patient, "\n", sep = "")
      cat("  ID_visite  ", private$ID_visite, "\n", sep = "")
      invisible(self)
    }
  ),
  private = list(
    name = NULL,
    date = NULL,
    ID_patient = NULL,
    ID_visite=NULL
  )
)

SV_Disease <- R6Class(
  classname = "SV_Disease",
  inherit = Disease,
  public = list(
    initialize = function(name=NA, date=NA, ID_patient=NA, ID_visite=NA, ID_SVisite=NA,  SVdate=NA, pulse=NA, respiration=NA, weight=NA, pcv=NA, cre=NA, ure=NA, iCa=NA, N=NA, K=NA, pH=NA, HCO3=NA, Cl=NA, TProt=NA, BE=NA, EPOC=NA, Lactate=NA, Glucose=NA, BloodPressure=NA){
      super$initialize(name, date, ID_patient, ID_visite)
      
      private$ID_SVisite = ID_SVisite
      private$SVdate = SVdate
      private$pulse = pulse
      private$respiration = respiration
      private$weight = weight
      private$pcv = pcv
      private$cre = cre
      private$ure = ure
      private$iCa=iCa
      private$N = N
      private$K = K
      private$pH= pH
      private$HCO3 = HCO3
      private$Cl = Cl
      private$TProt = TProt
      private$BE = BE
      private$EPOC = EPOC
      private$Lactate = Lactate
      private$Glucose = Glucose
      private$BloodPressure = BloodPressure
    },
    
    set_svisite = function(ID_SVisite) {
      private$ID_SVisite = ID_SVisite
    },
    set_SVdate = function(date) {
      private$SVdate = date
    },
    set_pulse = function(pulse) {
      private$pulse = pulse
    },
    set_respiration= function(respiration) {
      private$respiration = respiration
    },
    set_weight= function(weight) {
      private$weight = weight
    },
    set_pcv = function(pcv) {
      private$pcv =  pcv
    },
    set_cre = function(cre) {
      private$cre =  cre
    },
    set_ure = function(ure) {
      private$ure =  ure
    },
    
    set_iCa= function(iCa) {
      private$iCa =  iCa
    },
    set_N= function(N) {
      private$N =  N
    },
    set_K= function(K) {
      private$K =  K
    },
    set_pH= function(pH) {
      private$pH =  pH
    },
    set_HCO3= function(HCO3) {
      private$HCO3=  HCO3
    },
    set_Cl= function(Cl) {
      private$Cl=  Cl
    },
    set_TProt= function(TProt) {
      private$TProt=  TProt
    },
    set_BE= function(BE) {
      private$BE=  BE
    },
    set_EPOC= function(EPOC) {
      private$EPOC=EPOC
    },
    set_Lactate= function(Lactate) {
      private$Lactate=  Lactate
    },
    set_Glucose= function(Glucose) {
      private$Glucose=  Glucose
    },
    set_BloodPressure= function(BloodPressure) {
      private$BloodPressure= BloodPressure
    },
    
    get_svisite = function() {
      private$ID_SVisite
    },
    get_SVdate = function() {
      private$SVdate
    },
    get_pulse = function() {
      private$pulse
    },
    get_respiration= function() {
      private$respiration
    },
    get_weight= function() {
      private$weight
    },
    
    get_pcv = function() {
      private$pcv
    },
    get_cre = function() {
      private$cre
    },
    get_ure = function() {
      private$ure
    },
    get_iCa= function() {
      private$iCa
    },
    get_N= function(N) {
      private$N
    },
    get_K= function(K) {
      private$K
    },
    get_pH= function(pH) {
      private$pH
    },
    get_HCO3= function(HCO3) {
      private$HCO3
    },
    get_Cl= function(Cl) {
      private$Cl
    },
    get_TProt= function(TProt) {
      private$TProt
    },
    get_BE= function(BE) {
      private$BE
    },
    get_EPOC= function(EPOC) {
      private$EPOC
    },
    get_Lactate= function(Lactate) {
      private$Lactate
    },
    get_Glucose= function(Glucose) {
      private$Glucose
    },
    get_BloodPressure= function(BloodPressure) {
      private$BloodPressure
    },
    
    print = function(...) {
      cat("Rq: \n")
      cat("  Name: ", private$name, "\n", sep = "")
      cat("  Date:  ", private$date, "\n", sep = "")
      cat("  ID_Patient: ", private$ID_patient, "\n", sep = "")
      cat("  ID_visite  ", private$ID_visite, "\n", sep = "")
      cat("  svVisite: ", private$ID_SVisite, "\n", sep = "")
      cat("  Date:  ", private$SVdate, "\n", sep = "")
      cat("  Pulse: ", private$pulse, "\n", sep = "")
      cat("  Respiration:  ", private$respiration, "\n", sep = "")
      cat("  Weight:  ", private$weight, "\n", sep = "")
      cat("  PCV:  ", private$pcv, "\n", sep = "")
      cat("  creatinine:  ", private$cre, "\n", sep = "")
      cat("  urea:  ", private$ure, "\n", sep = "")
      cat("  iCa:  ", private$iCa, "\n", sep = "")
      cat("  PCV:  ", private$pcv, "\n", sep = "")
      cat("  Na:  ", private$N, "\n", sep = "")
      cat("  K:  ", private$K, "\n", sep = "")
      cat("  PH:  ", private$pH, "\n", sep = "")
      cat("  HCO3:  ", private$HCO3, "\n", sep = "")
      cat("  Cl:  ", private$Cl, "\n", sep = "")
      cat("  Tot_Prot:  ", private$TProt, "\n", sep = "")
      cat("  BE:  ", private$BE, "\n", sep = "")
      cat("  EPOC:  ", private$EPOC, "\n", sep = "")
      cat("  Lactatos:  ", private$Lactate , "\n", sep = "")
      cat("  Tot_Prot:  ", private$TProt, "\n", sep = "")
      cat("  Glucose:  ", private$Glucose, "\n", sep = "")
      cat("  BloodPressure:  ", private$BloodPressure, "\n", sep = "")
      #invisible(self)
    }
  ),
  
  private = list(
    ID_SVisite = NA,
    SVdate=NA,
    pulse= NA,
    respiration= NA,
    weight= NA,
    pcv =  NA,
    cre= NA,
    ure=NA,
    iCa= NA,
    N= NA,
    K= NA,
    pH= NA,
    HCO3= NA,
    Cl= NA,
    TProt= NA,
    BE= NA,
    EPOC= NA,
    Lactate= NA,
    Glucose= NA,
    BloodPressure= NA
  )
)

Demo <- R6Class(
  classname = "Demo",
  public = list(
    initialize = function(Race, Sexe=NA, Couleur=NA, date=NA, dateN=NA, ID_patient=NA) {
      # stopifnot(is.character(Race),length(Race)==1)
      # stopifnot(is.character(Sexe),length(Sexe)==1)
      # stopifnot(is.character(Couleur),length(Couleur)==1)
      # stopifnot(is.character(dateN),length(dateN)==1)
      # stopifnot(is.character(date),length(date)==1)
      # stopifnot(is.numeric(ID_patient),length(ID_patient)==1)
      
      private$Race = Race
      private$Sexe = Sexe
      private$Couleur = Couleur
      private$age=as.numeric(difftime( ymd(substring(date,1,10)), ymd(substring(dateN,1,10)), units = "days"))/ 365.25
      private$ID_patient= ID_patient
      
    },
    set_breed = function(Race) {
      private$Race = Race
    },
    set_age = function(date, dateN) {
      private$age = as.numeric(difftime( ymd(date), ymd(substring(dateN,1,10)), units = "days"))/ 365.25
    },
    set_Sexe = function(Sexe) {
      private$Sexe = Sexe
    },
    set_patient = function(ID_patient) {
      private$ID_patient = ID_patient
    },
    set_color = function(Couleur) {
      private$Couleur = Couleur
    },
    get_breed = function() {
      private$Race
    },
    get_sex = function() {
      private$Sexe
    },
    get_patient= function() {
      private$ID_patient
    },
    get_color= function() {
      private$Couleur
    },
    get_age= function() {
      private$age
    },
    
    print = function(...) {
      cat("Patient: \n")
      cat("  Patient: ", private$ID_patient, "\n", sep = "")
      cat("  Age  ", private$age, "\n", sep = "")
      cat("  Breed:  ", private$Race, "\n", sep = "")
      cat("  Sex: ", private$Sexe, "\n", sep = "")
      cat("  Color  ", private$Couleur, "\n", sep = "")
    }
  ),
  private = list(
    Race=NULL,
    Sexe=NULL,
    Couleur=NULL,
    age=NULL,
    ID_patient=NULL
  )
)

# s1 =SV_Disease$new(name="t", date="t", ID_patient="t", ID_visite="t")
# x1$get_date("5432")
# x1$get_name()
