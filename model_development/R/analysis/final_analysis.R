# ACMI stuff
# install.packages("bigrquery")
# install.packages("tidyverse")   = Needed for API calls
#install.packages("riskRegression")
#install.packages("data.table")

library(dplyr)
library(bigrquery)
library (MASS)
library("pROC")
library("ggstance")
library("MLeval") #ML eval for cross validated roc curve
library("survival")
library("rms")
library("lubridate") #to make start stop dates
library("riskRegression") #for PredcitCox (basline hazard estimation)
library("data.table") #for formatting
library("survminer") #to get ggq plot wor


################################################################################
# Tables have all been built in build_sql_2.R
# Table used for modelling is yhcr-prd-phm-bia-core.CY_MYSPACE_KB.full_v1_Cox_D_F_Update
# The code in this scripts runs Time dependent Cox regression models
#The plan for the modelling is to fit a model with known predictors of falls/ delirium
#and then compare this model to a model with the same predictors in addition to 89 
#AC medications
################################################################################

# 23/6/21

# Kate Best - https://orcid.org/0000-0002-4663-7141

################################################################################
# Dates - format YYYY-MM-DD
project_start_date <- "2019-01-01"
project_end_date <- "2019-12-31"

################################################################################
#Pre-requisites
#Read in list of 110 AC drugs -UPDATE CSV TO SAVE ADDING CHLORPENAMINE and minus INDACATEROL here
ACB_meds <- read.csv("selected_ac_medications.csv", header = FALSE, sep = ",")
ACB_meds <- unlist(ACB_meds)


#Remove 11 drugs prescribed nationally <2500 per month in OpenPrescribing.net
AC_National_remove<-c("Alprazolam", "Clozapine" ,"Hydromorphone", "Naloxone" , "Trimipramine", "Fluvoxamine",
                      "Doxepin", "Prazosin", "Eslicarbazepine", "Dicycloverine", "Diphenhydramine")

ac_medications <-  ACB_meds [! ACB_meds %in% AC_National_remove]
print(paste0(length(ac_medications), " ac medications loaded."))

eFI_deficits <- read.csv("eFI deficits.csv", header = FALSE, sep = ",")
eFI_deficits <- unlist(eFI_deficits)

############################
#Read in data from bigquery#
############################
Cox_data <- bq_table_download("yhcr-prd-phm-bia-core.CY_MYSPACE_KB.full_v1_Cox_D_F_Update" )

############################
#Clean/ format data        #
############################
#Make composite variable of Delirium and Falls
table(Cox_data$Delirium, Cox_data$Falls_Outcome)
Cox_data$D_or_F<-0
Cox_data$D_or_F[Cox_data$Delirium==1 | Cox_data$Falls_Outcome==1 ]= 1
Cox_data$D_or_F[Cox_data$Delirium==0 & Cox_data$Falls_Outcome==0 ]= 0
table(Cox_data$D_or_F)

#Restrict to 65-95 only
Cox_data <- subset(Cox_data, AgeJan19 < 96) 

#Recode Age
Cox_data$Age_Cat<-"null"
Cox_data$Age_Cat[Cox_data$AgeJan19 <75]= "65-74"
Cox_data$Age_Cat[Cox_data$AgeJan19 >74 & Cox_data$AgeJan19 <85]= "75-84"
Cox_data$Age_Cat[Cox_data$AgeJan19 >84 ]= "85-95"
sum(is.na(Cox_data$AgeJan19))  # check missing values for Age_Cat (none!)
table(Cox_data$AgeJan19)

#Centre age around median (74 years)
summary(Cox_data$AgeJan19)
Cox_data$Age<-Cox_data$AgeJan19-74

#Create Age^2 and Age^3 for modelling non-linear association
Cox_data$Age2<-Cox_data$Age*Cox_data$Age
Cox_data$Age3<-Cox_data$Age2*Cox_data$Age

#Recode Sex
Cox_data$GENDER<-recode_factor(Cox_data$gender_concept_id, '8507' ="Male", '8532' ="Female", .default = NA_character_)
table(Cox_data$GENDER)
sum(is.na(Cox_data$GENDER))

#Make sure Month is a date not factor & make new Start variable that is a copy of month
class(Cox_data$Month)
Cox_data$Start <- as.Date(Cox_data$Month, "%y-%m-%d")

#Need to make 'stop date' that is the date at start of the next month
Cox_data$Stop <- (Cox_data$Start %m+% months(1))

#Make variable for first date of study so can work out start/ stop days as opposed to dates
Cox_data$Start1<- difftime(Cox_data$Start ,project_start_date , units = c("days"))
Cox_data$Stop1<- difftime(Cox_data$Stop ,project_start_date , units = c("days"))

#Make data with only complete sex
Complete_Cox_data<-Cox_data[!is.na(Cox_data$GENDER),]
Complete_Cox_data<- Complete_Cox_data[order(Complete_Cox_data$person_ID, Complete_Cox_data$Month),] #order by ID, month

#Make eFI score
#Note make original (36 deficits and new one which is 36- 7 comorbidities - Fragility fracture)
eFI_deficits <- read.csv("eFI deficits.csv", header = FALSE, sep = ",")
eFI_deficits <- unlist(eFI_deficits)

eFI_deficits_Subset <-  eFI_deficits [! eFI_deficits %in% c("Arthritis", "Falls",  "Hearing_impairment"
                                                                      , "Memory___cognitive_problems", "Osteporosis", "Parkinsonism___tremor" ,
                                                                      "Urinary_incontinence", "Visual_impairment")] 

Complete_Cox_data$eFI_score<-(rowSums(Complete_Cox_data[,c(eFI_deficits_Subset)]))/28

#Make frailty categories
Complete_Cox_data$eFI_Cat<-0
Complete_Cox_data$eFI_Cat[Complete_Cox_data$eFI_score <0.12]= 0
Complete_Cox_data$eFI_Cat[Complete_Cox_data$eFI_score >=0.12 & Complete_Cox_data$eFI_score <0.24]= 1
Complete_Cox_data$eFI_Cat[Complete_Cox_data$eFI_score >=0.24 & Complete_Cox_data$eFI_score <0.36]= 2
Complete_Cox_data$eFI_Cat[Complete_Cox_data$eFI_score >=0.36 ]= 3
table(Complete_Cox_data$eFI_Cat)
Complete_Cox_data$eFI_Cat<-as.factor(Complete_Cox_data$eFI_Cat)

##########################
#Calculate drug frequency#
##########################
#No of months prescribed
Drug_Freq<-colSums(Complete_Cox_data[,c(ac_medications)])
Drug_Freq<-as.matrix(Drug_Freq)

#No of different AC_drugs (calculated for each month, neglecting person)
Complete_Cox_data$AnyAC<-rowSums(Complete_Cox_data[,c(ac_medications)])
No_AC<-as.data.frame(table(Complete_Cox_data$AnyAC))

#No of people prescribed to
AC_only<-filter(Complete_Cox_data, AnyAC!=0) #Filter only those rows with any AC med CHECK NUMBERS
AC_only <- AC_only[,c(-2,-3,-4,-5,-(116:168))] #Just keep the drugs variables and person id

Drug_Freq_Person <- AC_only %>%   #Make one row per person that has 1 if drug prescribed i any month
  group_by(person_ID) %>%
  summarise_each(funs(max))

Drug_Freq_Person<-colSums(Drug_Freq_Person[,c(ac_medications)])


#Calculate no of outcomes for each drug NOTE this is for rows(months) not people
Outcomes <- subset(Complete_Cox_data, D_F>0 ) 
Drug_Outcomes<-as.matrix(colSums(Outcomes[,c(ac_medications)]))
Drug_Outcomes_Table<-as.data.frame(cbind(Drug_Freq, Drug_Freq_Person, Drug_Outcomes))
Drug_Outcomes_Table$PersonPrev<-Drug_Outcomes/Drug_Freq_Person*100
Drug_Outcomes_Table$RowsPrev<-Drug_Outcomes/Drug_Freq*100

#No of people prescribed an AC med during 2019
AC <- Complete_Cox_data[,c(-2,-3,-4,-5,-(116:168))] #Just keep the drugs variables and person id
AC_No <- AC %>%   #Make one row per person that has 1 if drug prescribed i any month
  group_by(person_ID) %>%
 summarise_all(max)
  
AC_No$ACno<-rowSums(AC_No[,c(ac_medications)])
AC_NO<-as.data.frame(table(AC_No$ACno))


#No of people prescribed an AC med during 2019 according to outcomes
AC_No_Outcome <- Outcomes[,c(-2,-3,-4,-5,-(116:168))] 
AC_No_Outcome <- AC_No_Outcome %>%   #Make one row per person that has 1 if drug prescribed i any month
  group_by(person_ID) %>%
  summarise_all(max)

AC_No_Outcome$ACno<-rowSums(AC_No_Outcome[,c(ac_medications)])
AC_NO_OUTCOME<-as.data.frame(table(AC_No_Outcome$ACno))


##########################
#Descriptive stats#
##########################
Descriptive<-filter(Complete_Cox_data,Month=='2019-01-01') #Filter only first month for descriptive stats

table(Descriptive$GENDER)
summary(Descriptive$GENDER)

summary(Descriptive$AgeJan19)
table(Descriptive$Age_Cat)

AgeSex<-as.data.frame(table(Descriptive$Age_Cat, Descriptive$GENDER))

table(Descriptive$eFI_Cat)

table(Descriptive$Parkinsonism___tremor)
table(Descriptive$Memory___cognitive_problems)
table(Descriptive$Visual_impairment)
table(Descriptive$Hearing_impairment)
table(Descriptive$Falls)
table(Descriptive$Urinary_incontinence)
table(Descriptive$Osteoporosis)
table(Descriptive$Arthritis)

table(Outcomes$Age_Cat,Outcomes$GENDER)

################################
#Examine functional form of Age#
################################
#Plot martingale resids of null model against Age, Age2 and Age3
Age_Cox<-coxph(Surv(Start1, Stop1, D_or_F)~ Age + Age2 + Age3  , data = Complete_Cox_data)
ggcoxfunctional(Age_Cox, data = Complete_Cox_data) #looks non-linear

#Plot martingale residuals of model fitted for age
age.ph <- coxph( Surv(Start1, Stop1, D_or_F) ~ Age ,  Complete_Cox_data, method="breslow")
Complete_Cox_data$resid<- residuals(age.ph, type="martingale", data=Complete_Cox_data)
plot(Complete_Cox_data$Age, Complete_Cox_data$resid, xlab="Age",ylab="Martingale Residuals")
plot(lowess(Complete_Cox_data$Age, Complete_Cox_data$resid))

#Fit model adjusted for Age, Age^2 and Age^3 & look at Wald Statistics
Age_Cox1<-coxph(Surv(Start1, Stop1, D_or_F)~ Age   , data = Complete_Cox_data)
Age_Cox2<-coxph(Surv(Start1, Stop1, D_or_F)~ GENDER + Age + Age2  , data = Complete_Cox_data)
Age_Cox3<-coxph(Surv(Start1, Stop1, D_or_F)~ GENDER + Age + Age2  + Age3 , data = Complete_Cox_data)

#Can see that Age^2 is significant. When adding cubic term, Age^2 goes non-significant so choose the 
#Quadratic model going forwards
summary(Age_Cox1)
summary(Age_Cox2)
summary(Age_Cox3)

################################
#Examine functional form of eFI#
################################
eFI_Cox<-coxph(Surv(Start1, Stop1, D_or_F)~ eFI_score  , data = Complete_Cox_data)
ggcoxfunctional(eFI_Cox, data = Complete_Cox_data) #looks non-linear

#######################
# Fit  models         #
#######################

#Make list of AC drugs that are not prescribed in 2019 (Promazine), or have 0 events (row 1) or 1 events (row 2). 
#Note Levomepromazine only removed once PRNs excluded
AC_remove <- c("Promazine","Propantheline_bromide","Levocetirizine","Sulpiride",
               "Diamorphine","Tapentadol","Oxcarbazepine","Diphenhydramine","Levomepromazine", "Methocarbamol","Nefopam","Chlorpromazine")

#Make list of AC drugs excluding those listed above
ac_medications_subset <-  ac_medications [! ac_medications %in% AC_remove]

#Make strings of drugs (with + between) that can be added into the models
AC_string_Subset <-paste(ac_medications_subset, collapse=' + ')


#Plan is to first fit existing prognostic model consisting of age, sex, clinical predictors.
#We will then compare this to the model of these factors in addition to AC meds

#First fit model of existing model (all predictors but without AC meds); this will be used for comparison
Model_comparison<-cph(Surv(Start1, Stop1, D_or_F) ~  Age + Age2  + GENDER + Memory___cognitive_problems + Visual_impairment + Hearing_impairment + Parkinsonism___tremor +  
             Falls + Urinary_incontinence  + Osteoporosis + Arthritis  , data=Complete_Cox_data, surv=T, x=T, y=T,  time.inc=365)

#Time dependent Cox adjusted for all drugs and all predictors
Model_full<-cph(Surv(Start1, Stop1, D_or_F) ~  Age + Age2  + GENDER + Memory___cognitive_problems + Visual_impairment + Hearing_impairment  +
               Parkinsonism___tremor +  Falls + Urinary_incontinence + Osteoporosis + Arthritis  +Alverine + Amantadine + Amisulpride + Amitriptyline + Aripiprazole + Atenolol + Atropine + Baclofen + Bendroflumethiazide + Betamethasone + Bumetanide + Buprenorphine + Captopril + Carbamazepine + Cetirizine + Chlorphenamine + Cimetidine + Cinnarizine + Citalopram + Clomipramine + Clonazepam + Clonidine + Clobazam + Codeine + Colchicine + Cyclizine + Darifenacin + Desloratadine + Dexamethasone + Diazepam + Digoxin + Dihydrocodeine + Dipyridamole + Dosulepin + Doxazosin + Escitalopram + Fentanyl + Fesoterodine + Fexofenadine + Fluoxetine + Flupenthixol + Furosemide + Glycopyrronium_bromide + Haloperidol + Hydralazine + Hydrochlorothiazide + Hydrocortisone + Hydroxyzine + Hyoscine + Imipramine  + Indapamide + Indoramin + Ipratropium + Isosorbide + Levomepromazine + Lofepramine + Loperamide + Loratadine + Lorazepam + Mebeverine + Metoprolol + Midazolam + Mirtazapine + Morphine + Nifedipine + Nortriptyline + Olanzapine + Oxybutynin + Oxycodone + Paroxetine + Prednisolone + Prochlorperazine + Procyclidine + Promethazine + Quetiapine + Ranitidine + Risperidone + Solifenacin + Temazepam +  Theophylline + Tiotropium_bromide + Tolterodine + Tramadol + Trazodone + Trihexyphenidyl + Trospium_chloride + Umeclidinium + Venlafaxine + Warfarin, data=Complete_Cox_data, surv=T, x=T, y=T,  time.inc=365)

#Produce HRs and 95%CI
Results_Model_comparison<-cbind(exp(Model_comparison$coefficients),exp(confint(Model_comparison)))
Results_Model_full<-cbind(exp(Model_full$coefficients),exp(confint(Model_full)))

#Model fit stats (R^2 and concordance)
Model_comparison$stats
Model_full$stats

concordance(Model_comparison) 
concordance(Model_full) 

#cross validated concordance statistic to measure discrimination
#(from the rms package: https://www.rdocumentation.org/packages/rms/versions/6.1-1/topics/validate)
#Note the bootstrapping would be preferrable, but this crashes R
cv_Model_comparison <- validate(Model_comparison, method = "crossvalidation", B = 5) 
cv_Model_comparison
CV_C_Model_comparison <- (cv_Model_comparison[1, 5] + 1)/2
CV_C_Model_comparison


cv_Model_full <- validate(Model_full, method = "crossvalidation", B = 5) 
cv_Model_full
CV_C_Model_full <- (cv_Model_full[1, 5] + 1)/2
CV_C_Model_full


#calibration 5-fold (won't run due to divergence)
#calibrate_Model<-calibrate(Model, method="crossvalidation", B=5) 
#calibrate_Model_4<-calibrate(Model_4, method="crossvalidation", B=5) 

#Check colinearity
vif(Model_comparison)  #All looks reasonable with VIF<2 for every predictor except for age and age^2
vif(Model_full) #All looks reasonable with VIF<2 for every predictor except for age and age^2

#Check proportional Hazards
cox.zph(Model_comparison, transform="km", global=TRUE) 
cox.zph(Model_full, transform="km", global=TRUE) 


########################################
# Estimate baseline hazard    #
########################################
#Estimate baseline hazard at each month. Centered sets all covariates to zero (i.e. no drugs, sex=0, age=74)
Base_haz_comparison<-as.data.table(predictCox(Model_comparison, times=c(31, 59,90, 120, 151, 181, 212, 243, 273, 304, 334, 365),
                                     type="hazard", centered=FALSE))


Base_haz_full<-as.data.table(predictCox(Model_full, times=c(31, 59,90, 120, 151, 181, 212, 243, 273, 304, 334, 365),
                                     type="hazard", centered=FALSE))

########################################
# Additional models for sensitivity     #
########################################
#For sensitivity also fit drug only model, age/ gender model , full model - eFI
Model_1<-cph(Surv(Start1, Stop1, D_or_F) ~Alverine + Amantadine + Amisulpride + Amitriptyline + Aripiprazole + Atenolol + Atropine + Baclofen + Bendroflumethiazide + Betamethasone + Bumetanide + Buprenorphine + Captopril + Carbamazepine + Cetirizine + Chlorphenamine + Cimetidine + Cinnarizine + Citalopram + Clomipramine + Clonazepam + Clonidine + Clobazam + Codeine + Colchicine + Cyclizine + Darifenacin + Desloratadine + Dexamethasone + Diazepam + Digoxin + Dihydrocodeine + Dipyridamole + Dosulepin + Doxazosin + Escitalopram + Fentanyl + Fesoterodine + Fexofenadine + Fluoxetine + Flupenthixol + Furosemide + Glycopyrronium_bromide + Haloperidol + Hydralazine + Hydrochlorothiazide + Hydrocortisone + Hydroxyzine + Hyoscine + Imipramine  + Indapamide + Indoramin + Ipratropium + Isosorbide + Levomepromazine + Lofepramine + Loperamide + Loratadine + Lorazepam + Mebeverine + Metoprolol + Midazolam + Mirtazapine + Morphine + Nifedipine + Nortriptyline + Olanzapine + Oxybutynin + Oxycodone + Paroxetine + Prednisolone + Prochlorperazine + Procyclidine + Promethazine + Quetiapine + Ranitidine + Risperidone + Solifenacin + Temazepam + Theophylline + Tiotropium_bromide + Tolterodine + Tramadol + Trazodone + Trihexyphenidyl + Trospium_chloride + Umeclidinium + Venlafaxine + Warfarin , data=Complete_Cox_data, surv=T, x=T, y=T,  time.inc=365)
Model_2<-cph(Surv(Start1, Stop1, D_or_F) ~ Age + Age2 + GENDER + Alverine + Amantadine + Amisulpride + Amitriptyline + Aripiprazole + Atenolol + Atropine + Baclofen + Bendroflumethiazide + Betamethasone + Bumetanide + Buprenorphine + Captopril + Carbamazepine + Cetirizine + Chlorphenamine + Cimetidine + Cinnarizine + Citalopram + Clomipramine + Clonazepam + Clonidine + Clobazam + Codeine + Colchicine + Cyclizine + Darifenacin + Desloratadine + Dexamethasone + Diazepam + Digoxin + Dihydrocodeine + Dipyridamole + Dosulepin + Doxazosin + Escitalopram + Fentanyl + Fesoterodine + Fexofenadine + Fluoxetine + Flupenthixol + Furosemide + Glycopyrronium_bromide + Haloperidol + Hydralazine + Hydrochlorothiazide + Hydrocortisone + Hydroxyzine + Hyoscine + Imipramine  + Indapamide + Indoramin + Ipratropium + Isosorbide + Levomepromazine + Lofepramine + Loperamide + Loratadine + Lorazepam + Mebeverine + Metoprolol + Midazolam + Mirtazapine + Morphine + Nifedipine + Nortriptyline + Olanzapine + Oxybutynin + Oxycodone + Paroxetine + Prednisolone + Prochlorperazine + Procyclidine + Promethazine + Quetiapine + Ranitidine + Risperidone + Solifenacin + Temazepam + Theophylline +Tiotropium_bromide + Tolterodine + Tramadol + Trazodone + Trihexyphenidyl + Trospium_chloride + Umeclidinium + Venlafaxine + Warfarin, data=Complete_Cox_data, surv=T, x=T, y=T,  time.inc=365)

Results_Model_1<-cbind(exp(Model_1$coefficients),exp(confint(Model_1)))
Results_Model_2<-cbind(exp(Model_2$coefficients),exp(confint(Model_2)))


Model_1$stats
Model_2$stats

concordance(Model_1) 
concordance(Model_2) 


#Check colinearity
vif(Model_1)  #All looks reasonable with VIF<2 for every predictor
vif(Model_2) #All looks reasonable with VIF<2 for every predictor

#Check proportional Hazards
cox.zph(Model_1, transform="km", global=TRUE) 
cox.zph(Model_2, transform="km", global=TRUE) 

cv_Model_1 <- validate(Model_1, method = "crossvalidation", B = 5) 
cv_Model_1
CV_C_Model_1 <- (cv_Model_1[1, 5] + 1)/2
CV_C_Model_1

cv_Model_2 <- validate(Model_2, method = "crossvalidation", B = 5) 
cv_Model_2
CV_C_Model_2 <- (cv_Model_2[1, 5] + 1)/2
CV_C_Model_2

#Estimate baseline hazard at each month. Centered sets all covariates to zero (i.e. no drugs, sex=0, age=74)
Base_haz_1<-as.data.table(predictCox(Model_1, times=c(31, 59,90, 120, 151, 181, 212, 243, 273, 304, 334, 365),
                                              type="hazard", centered=FALSE))


Base_haz_2<-as.data.table(predictCox(Model_2, times=c(31, 59,90, 120, 151, 181, 212, 243, 273, 304, 334, 365),
                                        type="hazard", centered=FALSE))

########################################
# Create Index based on ACB scores     #
########################################
#First run ACB on the subset of drugs included in our index (n=89 minus Alprazolam, Lorazepam, Clonazepam, Clobazam, Midazolam, Temazepam and Trimipramine
#which were not in ACB)
#Then run on ACB meds that we had in the original list (n=110 minus Alprazolam, Lorazepam, Clonazepam, Clobazam, Midazolam, Temazepam and Trimipramine
#which were not in ACB)
#Make new data set
ACBdata<-Complete_Cox_data
#ACBdata$Amantadine<-recode(ACBdata$Amantadine,'1'=2, '0'=0) # code to recode single drug

#ACB scores each drug 1 to 3
#Those with a 1 in already that are scored 1 can be left alone (e.g. Alverine)
#Those with a 1 in already that are scored 2 or 3 must be recoded
#Those with a 1 in already that aren't in original ACB must be recoded to 0

ACBdata<-ACBdata %>% 
  mutate_at(vars(Amantadine, Carbamazepine, Eslicarbazepine, Nefopam, Oxcarbazepine), recode, '1'=2, '0'=0) %>%
  mutate_at(vars(Amitriptyline, Atropine, Chlorpromazine, Chlorphenamine,  Clomipramine, Clozapine, Darifenacin,
                 Dicycloverine, Diphenhydramine,Dosulepin, Doxepin, Fesoterodine, Glycopyrronium_bromide, Hydroxyzine,
                 Hyoscine, Imipramine, Levomepromazine, Lofepramine,  Methocarbamol,Nortriptyline, Olanzapine, Oxybutynin,
                 Paroxetine, Procyclidine, Promazine,Promethazine, Propantheline_bromide, Quetiapine, Solifenacin, 
                 Tolterodine, Trihexyphenidyl, Trospium_chloride), recode, '1'=3, '0'=0) %>%
  mutate_at(vars(Alprazolam, Lorazepam, Clonazepam, Clobazam, Midazolam, Temazepam, Trimipramine) , recode, '1'=0, '0'=0)


##Calculate ACB score for each row that is the sum of the drug scores, use 3 different AC_lists
#Note that thiis not complete ACB as we excluded some from the list right at the beginning so they are not in our dataset
ACBdata$ACBscore_1<-rowSums(ACBdata[,c(ACB_meds)])
ACBdata$ACBscore_2<-rowSums(ACBdata[,c(ac_medications)])
ACBdata$ACBscore_3<-rowSums(ACBdata[,c(ac_medications_subset)])

table(ACBdata$ACBscore_1)
table(ACBdata$ACBscore_2)
table(ACBdata$ACBscore_3)

#Run time dependent Cox using score as predictor
Model_ACB1<-cph(Surv(Start1, Stop1, D_or_F) ~ Age + Age2 +  GENDER + Memory___cognitive_problems 
                + Visual_impairment + Hearing_impairment + Osteoporosis + Urinary_incontinence + Parkinsonism___tremor +  Falls + Arthritis + ACBscore_1 , data=ACBdata, surv=T, x=T, y=T,  time.inc=365)

Model_ACB2<-cph(Surv(Start1, Stop1, D_or_F) ~ Age + Age2 +  GENDER + Memory___cognitive_problems 
                      + Visual_impairment + Hearing_impairment + Osteoporosis + Urinary_incontinence + Parkinsonism___tremor +  Falls + Arthritis + ACBscore_2 , data=ACBdata, surv=T, x=T, y=T,  time.inc=365)

Model_ACB3<-cph(Surv(Start1, Stop1, D_or_F) ~ Age + Age2 + GENDER + Memory___cognitive_problems 
                      + Visual_impairment + Hearing_impairment + + Osteoporosis + Urinary_incontinence + Parkinsonism___tremor +  Falls + Arthritis + ACBscore_3 , data=ACBdata, surv=T, x=T, y=T,  time.inc=365)


#Produce HRs and 95%CI
Results_Model_ACB1<-cbind(exp(Model_ACB1$coefficients),exp(confint(Model_ACB1)))
Results_Model_ACB2<-cbind(exp(Model_ACB2$coefficients),exp(confint(Model_ACB2)))
Results_Model_ACB3<-cbind(exp(Model_ACB3$coefficients),exp(confint(Model_ACB3)))

#fit train concordance
concordance(Model_ACB1)
concordance(Model_ACB2) 
concordance(Model_ACB3) 

#Stats
Model_ACB1$stats
Model_ACB2$stats
Model_ACB3$stats

#cross validated concordance statistic to measure discrimination (AUROC)
#(from the rms package: https://www.rdocumentation.org/packages/rms/versions/6.1-1/topics/validate)
cv_Model_ACB1 <- validate(Model_ACB1, method = "crossvalidation", B = 5) 
cv_Model_ACB1
CV_C_Model_ACB1 <- (cv_Model_ACB1[1, 5] + 1)/2
CV_C_Model_ACB1

cv_Model_ACB2 <- validate(Model_ACB2, method = "crossvalidation", B = 5) 
cv_Model_ACB2
CV_C_Model_ACB2 <- (cv_Model_ACB2[1, 5] + 1)/2
CV_C_Model_ACB2

cv_Model_ACB3 <- validate(Model_ACB3, method = "crossvalidation", B = 5) 
cv_Model_ACB3
CV_C_Model_ACB3 <- (cv_Model_ACB3[1, 5] + 1)/2
CV_C_Model_ACB3

#Check proportional Hazards
cox.zph(Model_ACB1, transform="km", global=TRUE) 
cox.zph(Model_ACB2, transform="km", global=TRUE) 
cox.zph(Model_ACB3, transform="km", global=TRUE) 

#calibration 5-fold
calibrate_Model_ACB2<-calibrate(Model_ACB2, method="crossvalidation", B=5) 
calibrate_Model_ACB3<-calibrate(Model_ACB3, method="crossvalidation", B=5) 

Base_haz_ACB1<-as.data.table(predictCox(Model_ACB1, times=c(31, 59,90, 120, 151, 181, 212, 243, 273, 304, 334, 365),
                                        type="hazard", centered=FALSE))


########################################
# Excluding PRNs          #
########################################
Cox_data <- bq_table_download("yhcr-prd-phm-bia-core.CY_MYSPACE_KB.full_v1_Cox_D_F_Update_PRN" )
#Make composite variable of Delirium and Falls
table(Cox_data$Delirium, Cox_data$Falls)
Cox_data$D_or_F<-0
Cox_data$D_or_F[Cox_data$Delirium==1 | Cox_data$Falls==1 ]= 1
Cox_data$D_or_F[Cox_data$Delirium==0 & Cox_data$Falls==0 ]= 0
table(Cox_data$D_or_F)

#Restrict to 65-95 only
Cox_data <- subset(Cox_data, AgeJan19 < 96) 

#Centre age around median (74 years)
summary(Cox_data$AgeJan19)
Cox_data$Age<-Cox_data$AgeJan19-74

#Create Age^2 and Age^3 for modelling non-linear association
Cox_data$Age2<-Cox_data$Age*Cox_data$Age
Cox_data$Age3<-Cox_data$Age2*Cox_data$Age

#Recode Sex
Cox_data$GENDER<-recode_factor(Cox_data$gender_concept_id, '8507' ="Male", '8532' ="Female", .default = NA_character_)
table(Cox_data$GENDER)
sum(is.na(Cox_data$GENDER))

#Recode Age into 3 categories for descriptive stats
Cox_data$Age_Cat<-"null"
Cox_data$Age_Cat[Cox_data$AgeJan19 <75]= "65-74"
Cox_data$Age_Cat[Cox_data$AgeJan19 >74 & Cox_data$AgeJan19 <85]= "75-84"
Cox_data$Age_Cat[Cox_data$AgeJan19 >84 ]= "85-95"
sum(is.na(Cox_data$Age_Cat))  # check missing values for Age_Cat (none!)

#Make sure Month is a date not factor & make new Start variable that is a copy of month
class(Cox_data$Month)
Cox_data$Start <- as.Date(Cox_data$Month, "%y-%m-%d")

#Need to make 'stop date' that is the date at start of the next month
Cox_data$Stop <- (Cox_data$Start %m+% months(1))

#Make variable for first date of study so can work out start/ stop days as opposed to dates
Cox_data$Start1<- difftime(Cox_data$Start ,project_start_date , units = c("days"))
Cox_data$Stop1<- difftime(Cox_data$Stop ,project_start_date , units = c("days"))

#Make data with only complete sex (sex missing in 4%)
Complete_Cox_data<-Cox_data[!is.na(Cox_data$GENDER),]
Complete_Cox_data<- Complete_Cox_data[order(Complete_Cox_data$person_ID, Complete_Cox_data$Month),] #order by ID, month


#Time dependent Cox adjusted for all drugs and all predictors
#Not including eFI score as CV crashes with eFI in
Model_full<-cph(Surv(Start1, Stop1, D_or_F) ~  Age + Age2  + GENDER + Memory___cognitive_problems + Visual_impairment + Hearing_impairment  +
                  Parkinsonism___tremor +  Hist_Falls + Urinary_incontinence + Osteoporosis + Arthritis  +Alverine + Amantadine + Amisulpride + Amitriptyline + Aripiprazole + Atenolol + Atropine + Baclofen + Bendroflumethiazide + Betamethasone + Bumetanide + Buprenorphine + Captopril + Carbamazepine + Cetirizine + Chlorphenamine + Cimetidine + Cinnarizine + Citalopram + Clomipramine + Clonazepam + Clonidine + Clobazam + Codeine + Colchicine + Cyclizine + Darifenacin + Desloratadine + Dexamethasone + Diazepam + Digoxin + Dihydrocodeine + Dipyridamole + Dosulepin + Doxazosin + Escitalopram + Fentanyl + Fesoterodine + Fexofenadine + Fluoxetine + Flupenthixol + Furosemide + Glycopyrronium_bromide + Haloperidol + Hydralazine + Hydrochlorothiazide + Hydrocortisone + Hydroxyzine + Hyoscine + Imipramine  + Indapamide + Indoramin + Ipratropium + Isosorbide + Levomepromazine + Lofepramine + Loperamide + Loratadine + Lorazepam + Mebeverine + Metoprolol + Midazolam + Mirtazapine + Morphine + Nifedipine + Nortriptyline + Olanzapine + Oxybutynin + Oxycodone + Paroxetine + Prednisolone + Prochlorperazine + Procyclidine + Promethazine + Quetiapine + Ranitidine + Risperidone + Solifenacin + Temazepam +  Theophylline + Tiotropium_bromide + Tolterodine + Tramadol + Trazodone + Trihexyphenidyl + Trospium_chloride + Umeclidinium + Venlafaxine + Warfarin, data=Complete_Cox_data, surv=T, x=T, y=T,  time.inc=365)

#Produce HRs and 95%CI
Results_Model_full<-cbind(exp(Model_full$coefficients),exp(confint(Model_full)))

#Model fit stats (R^2 and concordance)
Model_full$stats

concordance(Model_full) 

#cross validated concordance statistic to measure discrimination
#(from the rms package: https://www.rdocumentation.org/packages/rms/versions/6.1-1/topics/validate)
#Note the bootstrapping would be preferrable, but this crashes R
cv_Model_full <- validate(Model_full, method = "crossvalidation", B = 5) 
cv_Model_full
CV_C_Model_full <- (cv_Model_full[1, 5] + 1)/2
CV_C_Model_full


#calibration 5-fold (won't run!)
#calibrate_Model<-calibrate(Model, method="crossvalidation", B=5) 
#calibrate_Model_4<-calibrate(Model_4, method="crossvalidation", B=5) #Divergence in 5 samples

#plot(cal, add=TRUE)

#Check colinearity
vif(Model_full) #All looks reasonable with VIF<2 for every predictor

#Check proportional Hazards
cox.zph(Model_full, transform="km", global=TRUE) 



########################################
# Incorporate Dose of meds     # 
# Note this analysis has not yet been run#
########################################
Cox_data_dose <- bq_table_download("yhcr-prd-phm-bia-core.CY_MYSPACE_KB.full_v1_Cox_D_F_Update_Dose" )

#Make composite variable of Delirium and Falls
table(Cox_data_dose$Delirium, Cox_data_dose$Falls_Outcome)
Cox_data_dose$D_or_F<-0
Cox_data_dose$D_or_F[Cox_data_dose$Delirium==1 | Cox_data_dose$Falls_Outcome==1 ]= 1
Cox_data_dose$D_or_F[Cox_data_dose$Delirium==0 & Cox_data_dose$Falls_Outcome==0 ]= 0
table(Cox_data_dose$D_or_F)

#Restrict to 65-95 only
Cox_data_dose<- subset(Cox_data_dose$AgeJan19 < 96) 

#Centre age
Cox_data_dose$Age<-Cox_data_dose$AgeJan19-74

#Recode Sex
Cox_data_dose$GENDER<-recode_factor(Cox_data_dose$gender_concept_id, '8507' ="Male", '8532' ="Female", .default = NA_character_)
table(Cox_data_dose$GENDER)
sum(is.na(Cox_data_dose$GENDER))

#Make sure Month is a date not factor & make new Start variable that is a copy of month
class(Cox_data_dose$Month)
Cox_data_dose$Start <- as.Date(Cox_data_dose$Month, "%y-%m-%d")

#Need to make 'stop date' that is the date at start of the next month
Cox_data_dose$Stop <- (Cox_data_dose$Start %m+% months(1))

#Make variable for first date of study so can work out start/ stop days as opposed to dates
Cox_data_dose$Start1<- difftime(Cox_data_dose$Start ,project_start_date , units = c("days"))
Cox_data_dose$Stop1<- difftime(Cox_data_dose$Stop ,project_start_date , units = c("days"))

#Make data with only complete sex
Complete_dose<-Cox_data_dose[!is.na(Cox_data$GENDER),]
Complete_dose<- Complete_dose[order(Complete_dose$person_ID, Complete_dose$Month),] #order by ID, month

#Time dependent Cox adjusted for all drugs and all predictors
Model_1_dose<-cph(Surv(Start1, Stop1, D_or_F) ~Age + Age2  + GENDER + Memory___cognitive_problems + Visual_impairment + Hearing_impairment  +
                    Parkinsonism___tremor +  Falls + Urinary_incontinence + Osteoporosis + Arthritis + Dose_Alverine + Dose_Amantadine + Dose_Amisulpride + .... +
           , data=Complete_dose, surv=T, x=T, y=T,  time.inc=365)

