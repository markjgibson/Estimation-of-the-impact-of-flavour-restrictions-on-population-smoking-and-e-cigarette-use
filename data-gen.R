#############
# READ DATA #
#############

#install.packages("read.spss")
library(foreign)
setwd(FilePath)
ASH_Y21<-read.spss("ASH21Y.sav", use.value.labels = TRUE, to.data.frame = TRUE)
ASH_A21<-read.spss("ASH21A_AV.sav", use.value.labels = TRUE, to.data.frame = TRUE)
ASH_Y19<-read.spss("ASH19Y.sav", use.value.labels = TRUE, to.data.frame = TRUE)
ASH_A19<-read.spss("ASH19A.sav", use.value.labels = TRUE, to.data.frame = TRUE)
STS_22<-read.spss("STSJan-April22.sav", use.value.labels = TRUE, to.data.frame = TRUE)
NYTS_21<-read.csv("NYTS21.csv")

#########################
# SPLIT STS DATA BY AGE #
#########################

STS_A22<-subset(STS_22, actage>20)
print(nrow(STS_A22))
STS_Y22<-subset(STS_22, actage<21)
print(nrow(STS_Y22))

#################################
# EXCLUDE BASED ON SOCIAL GRADE #
#################################

ASH_Y21_c2de<-subset(ASH_Y21, profile_socialgrade_cie_yks__2=="Yes")
ASH_A21_c2de<-subset(ASH_A21, profile_socialgrade_cie=="C2" | profile_socialgrade_cie=="D" | profile_socialgrade_cie=="E")
ASH_Y19_c2de<-subset(ASH_Y19, profile_socialgrade_cie_yks__2=="Yes")
ASH_A19_c2de<-subset(ASH_A19, profile_socialgrade_ciew=="C2" | profile_socialgrade_ciew=="D" | profile_socialgrade_ciew=="E")
STS_A22_c2de<-subset(STS_A22, sgz=="C2" | sgz=="D" | sgz=="E")
STS_Y22_c2de<-subset(STS_Y22, sgz=="C2" | sgz=="D" | sgz=="E")

######################
# CREATE UK FUNCTION #
######################

analyse <- function(grade){
  
  #Write output to file
  sink(paste("ASH-STS", grade, ".txt", sep=""))
  
  #Put dataframes in new variable to be used in function
  ASHY21<-get(paste("ASH_Y21", grade, sep=""))
  ASHA21<-get(paste("ASH_A21", grade, sep=""))
  ASHY19<-get(paste("ASH_Y19", grade, sep=""))
  ASHA19<-get(paste("ASH_A19", grade, sep=""))
  STSY22<-get(paste("STS_Y22", grade, sep=""))
  STSA22<-get(paste("STS_A22", grade, sep=""))
  
  ###########################################################
  # A. TO WHAT EXTENT DO FLAVOURS DRAW IN NON_SMOKING YOUTH #
  ###########################################################
  print("## TO WHAT EXTENT DO FLAVOURS DRAW IN NON_SMOKING YOUTH ##")
  
  #------------------#
  # b. Never-Smokers #
  #------------------#
  print("# Non-Smokers #")
  #ASH base is all
  
  #Get number and weight of Never Smoker subset... 
  #ASH
  ASH_Y21_NS<-subset(ASHY21, B1=="I have never smoked cigarettes, not even a puff or two")
  print("ASH Never-Smokers 2021")
  NS<-print(nrow(ASH_Y21_NS))
  print("Weight")
  NSW<-print(mean(ASH_Y21_NS$weight))
  #STS
  STS_Y22_NS<-subset(STSY22, q632a1=="I have never been a smoker (i.e. smoked for a year or more)")
  print("STS Never-Smokers 2022")
  NS2<-print(nrow(STS_Y22_NS))
  print("Weight")
  NSW2<-mean(STS_Y22_NS$weight_gb)
  if(is.na(NSW2)){NSW2<-0}
  print(NSW2)
  
  #Get number and weight of Smokers
  #ASH
  ASH_Y21_S<-subset(ASHY21, B1!="I have never smoked cigarettes, not even a puff or two")
  ASH_Y21_S<-subset(ASH_Y21_S, B1!="Don't want to say")
  print("ASH Smokers 2021")
  S<-print(nrow(ASH_Y21_S))
  print("Weight")
  SW<-print(mean(ASH_Y21_S$weight))
  #STS
  STS_Y22_S<-subset(STSY22, q632a1!="I have never been a smoker (i.e. smoked for a year or more)")
  STS_Y22_S<-subset(STS_Y22_S, q632a1!="Don't Know")
  print("STS Smokers 2022")
  S2<-print(nrow(STS_Y22_S))
  print("Weight")
  SW2<-mean(STS_Y22_S$weight_gb)
  if(is.na(SW2)){SW2<-0}
  print(SW2)
  
  #Calculate percentage
  WNS<-(NS*NSW)+(NS2*NSW2)
  WS<-(S*SW)+(S2*SW2) 
  print("A.b. %")
  print(WNS/(WNS+WS)*100)
  print("A.b. N")
  print(NS+NS2+S+S2)
  
  #--------------------------#
  # c. Non-Smokers who Vaped #
  #--------------------------#
  print("# Non-Smokers who Vaped #")
  #ASH base for Vaping Q is all those who have heard of e-cigs not all, so we want to include NAs
  
  #Get number and weight of Ever-Vapers
  #ASH
  EvVap<-c("I have only tried an e-cigarette once or twice", "I use e-cigarettes sometimes, but no more than once a month",
           "I use e-cigarettes more than once a month, but less than once a week", "I use e-cigarettes more than once a week but not every day",
           "I use e-cigarettes every day", "I used e-cigarettes in the past but no longer do")
  ASH_Y21_NSEV<-subset(ASH_Y21_NS, C4b %in% EvVap | is.na(ASH_Y21_NS$C4b))
  print("ASH Never-Smokers-Ever-Vapers 2021")
  EV<-print(nrow(ASH_Y21_NSEV))
  print("Weight")
  EVW<-print(mean(ASH_Y21_NSEV$weight))
  #STS
  STS_Y22_NSEV<-subset(STS_Y22_NS, imw184_12 == "Electronic cigarette")
  print("STS Never-Smokers-Ever-Vapers 2022")
  EV2<-print(nrow(STS_Y22_NSEV))
  print("Weight")
  EVW2<-mean(STS_Y22_NSEV$weight_gb)
  if(is.na(EVW2)){EVW2<-0}
  print(EVW2)
  
  #Get number and weight of Never-Smoker-Never-Vapers
  #ASH
  ASH_Y21_NSNV<-subset(ASH_Y21_NS, C4b=="I have never used an e-cigarette")
  print("ASH Never-Smokers-Never-Vapers 2021")
  NV<-print(nrow(ASH_Y21_NSNV))
  print("Weight")
  NVW<-print(mean(ASH_Y21_NSNV$weight))
  #STS
  STS_Y22_NSNV<-subset(STS_Y22_NS, imw184_12=="no Electronic cigarette")
  print("STS Never-Smokers-Never-Vapers 2022")
  NV2<-print(nrow(STS_Y22_NSNV))
  print("Weight")
  NVW2<-mean(STS_Y22_NSNV$weight_gb)
  if(is.na(NVW2)){NVW2<-0}
  print(NVW2)
  
  #Calculatepercentage
  WEV<-(EV*EVW)+(EV2*EVW2)
  WNV<-(NV*NVW)+(NV2*NVW2)
  print("A.c. %")
  print(WEV/(WEV+WNV)*100)
  print("A.c. N")
  print(EV+EV2+NV+NV2)
  
  #-----------------------------#
  # d. Vape Because of Flavours #
  #-----------------------------#
  print("# Vape Because of Flavours #")
  #ASH base is all who tried E-cigs

  #Get Non-Smokers Ever-Vapers
  #ASH
  ASH_Y19_NS<-subset(ASHY19, B1=="I have never smoked cigarettes, not even a puff or two")
  ASH_Y19_NSEV<-subset(ASH_Y19_NS, C4b!="I have never used an e-cigarette")
  print("Non-Smokers Ever-Vapers 2019")
  print(nrow(ASH_Y19_NSEV))

  #Get number and weight of Like-Flavours
  #ASH
  ASH_Y19_LF<-subset(ASH_Y19_NSEV, C7b=="I like the flavours")
  print("ASH Like-Flavours 2019")
  LF<-print(nrow(ASH_Y19_LF))
  print("Weight")
  LFW<-print(mean(ASH_Y19_LF$weight))
  #STS
  STS_Y22_LF<-subset(STS_Y22_NSEV, yvp12=="I like the flavours")
  print("STS Like-Flavours 2022")
  LF2<-print(nrow(STS_Y22_LF))
  print("Weight")
  LFW2<-mean(STS_Y22_LF$weight_gb)
  if(is.na(LFW2)){LFW2<-0}
  print(LFW2)
  
  #Get weight of Not-Like-Flavours
  #ASH
  ASH_Y19_NLF<-subset(ASH_Y19_NSEV, C7b!="Don't know")
  ASH_Y19_NLF<-subset(ASH_Y19_NLF, C7b!="I like the flavours")
  print("ASH Not 2019")
  NLF<-print(nrow(ASH_Y19_NLF))
  print("Weight")
  NLFW<-print(mean(ASH_Y19_NLF$weight))
  #STS
  STS_Y22_NLF<-subset(STS_Y22_NSEV, yvp12=="no I like the flavours")
  print("STS Not 2022")
  NLF2<-print(nrow(STS_Y22_NLF))
  print("Weight")
  NLFW2<-mean(STS_Y22_NLF$weight_gb)
  if(is.na(NLFW2)){NLFW2<-0}
  print(NLFW2)
  
  #Calculate percentage
  WLF<-(LF*LFW)+(LF2*LFW2)
  WNLF<-(NLF*NLFW)+(NLF2*NLFW2)
  print("A.d. %")
  print(WLF/(WLF+WNLF)*100)
  print("A.d. N")
  print(LF+LF2+NLF+NLF2)
  
  #######################################################
  # B. HOW MANY NON-SMOKERS WHO VAPE SUBSEQUENTLY SMOKE #
  #######################################################
  print("## HOW MANY NON-SMOKERS WHO VAPE SUBSEQUENTLY SMOKE ##")
  
  #-------------------------------------#
  # b. Non-Smokers who Vape, then Smoke #
  #-------------------------------------#
  print("# Non-Smokers who Vape, then Smoke #")
  #ASH base is all who tried E-cigs
  
  #Get number and weight of Then-Smoke
  #ASH
  ASH_Y21_TS<-subset(ASHY21, C5=="I tried using an e-cigarette before I first tried smoking a real cigarette")
  print("ASH Non-smoking Vapers who Then-Smoke 2021")
  TS<-print(nrow(ASH_Y21_TS))
  print("Weight")
  TSW<-print(mean(ASH_Y21_TS$weight))
  #STS
  STS_Y22_TS<-subset(STSY22, yvp2=="I tried using an e-cigarette before I first tried smoking a regular cigarette")
  print("STS Non-smoking Vapers who Then-Smoke 2022")
  TS2<-print(nrow(STS_Y22_TS))
  print("Weight")
  TSW2<-mean(STS_Y22_TS$weight_gb)
  if(is.na(TSW2)){TSW2<-0}
  print(TSW2)
  
  #Get number and weight of Then-Don't
  #ASH
  ASH_Y21_TD<-subset(ASHY21, C5=="I have never smoked a real cigarette but have tried an e-cigarette")
  print("ASH Non-smoking Vapers who Then-Don't 2021")
  TD<-print(nrow(ASH_Y21_TD))
  print("Weight")
  TDW<-print(mean(ASH_Y21_TD$weight))
  #STS
  STS_Y22_TD<-subset(STSY22, yvp2=="I have never smoked a real cigarette but have tried an e-cigarette")
  print("STS Non-smoking Vapers who Then-Don't 2022")
  TD2<-print(nrow(STS_Y22_TD))
  print("Weight")
  TDW2<-mean(STS_Y22_TD$weight_gb)
  if(is.na(TDW2)){TDW2<-0}
  print(TDW2)
  
  #Calculate percentage
  WTS<-(TS*TSW)+(TS2*TSW2)
  WTD<-(TD*TDW)+(TD2*TDW2)
  print("B.b. %")
  print(WTS/(WTS+WTD)*100)
  print("B.b. N")
  print(TS+TS2+TD+TD2)
  
  ###########################################################
  # C. TO WHAT EXTENT DO FLAVOURS DRAW IN ADULT SMOKERS WHO #
  #   WOULD QUIT SMOKING USING E-CIGARETTES AS A RESOURCE   #
  ###########################################################
  print("## TO WHAT EXTENT DO FLAVOURS DRAW IN ADULT SMOKERS WHO WOULD QUIT SMOKING USING E-CIGARETTES AS A RESOURCE ##")
 
  #-------------------------------------------------------#
  # b. Smokers who quit last year because of E-Cigarettes #
  #-------------------------------------------------------#
  
  print("# Smokers who quit last year because of E-cigarettes #")
  
  #In current smoker (plus recent quiters) subset...
  #STS
  STS_A22_S<-subset(STSA22, q632a1!="Don't Know")
  STS_A22_S<-subset(STS_A22_S, q632a1!="I have never been a smoker (i.e. smoked for a year or more)")
  STS_A22_S<-subset(STS_A22_S, q632a1!="stopped smoking completely more than a year ago")
  print("STS Smokers + Recent Quiters 2022")
  print(nrow(STS_A22_S))
  
  #Get number and weight of Ex-smoke E-cig quitters
  #STS
  STS_A22_EQ<-subset(STS_A22_S, q632a1=="I have stopped smoking completely in the last year" & X.q632b1k=="Electronic cigarette")
  print("STS e-cig-quitters-yearly 2022")
  EQ<-print(nrow(STS_A22_EQ))
  print("Weight")
  EQW<-print(mean(STS_A22_EQ$weight_gb))
  
  #Get number and weight of everyone else
  #STS
  STS_A22_EE<-subset(STS_A22_S, !(q632a1=="I have stopped smoking completely in the last year" & X.q632b1k=="Electronic cigarette"))
  print("STS Everyone Else 2022")
  EE<-print(nrow(STS_A22_EE))
  print("Weight")
  EEW<-print(mean(STS_A22_EE$weight_gb))
  
  #Calculate percentage
  WEQ<-EQ*EQW
  WEE<-EE*EEW
  print("C.b %")
  print(WEQ/(WEQ+WEE)*100)
  print("c.b. N")
  print(EQ+EE)
  
  #-----------------------------------------------------------#
  # c. Smokers who would not quit/smoke more without flavours #
  #-----------------------------------------------------------#
  print("# Smokers who would not quit/smoke more without flavours #")
  #ASH base is all flavoured E-cig users

  #In Smoker subset...
  #ASH
  ASH_A19_S<-subset(ASHA19, s2_edited2016=="I smoke but I don't smoke every day" | s2_edited2016=="I smoke every day")
  print("ASH Smokers 2019")
  print(nrow(ASH_A19_S))
  #STS
  STS_A22_S<-subset(STSA22, q632a1!="Don't Know")
  STS_A22_S<-subset(STS_A22_S, q632a1!="I have never been a smoker (i.e. smoked for a year or more)")
  print("STS Smokers 2022")
  print(nrow(STS_A22_S))
  
  #Get number and weight of Smoke-Mores
  #ASH
  ASH_A19_SM<-subset(ASH_A19_S, Q6_NEW2019=="I would smoke more tobacco" | Q6_NEW2019=="I would go back to smoking tobacco")
  print("ASH Smoke-Mores 2019")
  SM<-print(nrow(ASH_A19_SM))
  print("Weight")
  SMW<-print(mean(ASH_A19_SM$weight))
  #STS
  STS_A22_SM<-subset(STS_A22_S, yvp33=="I would smoke more tobacco" | yvp34=="I would go back to smoking tobacco")
  print("STS Smoke-Mores 2022")
  SM2<-print(nrow(STS_A22_SM))
  print("Weight")
  SMW2<-print(mean(STS_A22_SM$weight_gb))
  
  #Get number and weight of everyone else
  #ASH
  EvEl<-c("I would still try to get flavours", "I would stop vaping/ using an e-cigarette", "I would make my own e-liquid", "I would use unflavoured e-liquids/ cartridges", "Other")
  ASH_A19_EE<-subset(ASH_A19_S, Q6_NEW2019 %in% EvEl)
  print("ASH Everyone Else 2019")
  EE<-print(nrow(ASH_A19_EE))
  print("Weight")
  EEW<-print(mean(ASH_A19_EE$weight))
  #STS
  STS_A22_EE<-subset(STS_A22_S, yvp33=="no I would smoke more tobacco" & yvp34=="no I would go back to smoking tobacco")
  print("STS Everyone Else 2022")
  EE2<-print(nrow(STS_A22_EE))
  print("Weight")
  EEW2<-print(mean(STS_A22_EE$weight_gb))
  
  #Calculate percentage
  WSM<-(SM*SMW)+(SM2*SMW2)
  WEE<-(EE*EEW)+(EE2*EEW2)
  print("C.c %")
  print(WSM/(WSM+WEE)*100)
  print("C.c. N")
  print(SM+SM2+EE+EE2)
  
  ##################################################
  # D. HOW MANY CURRENT ADULT VAPERS WOULD RELAPSE #
  #    TO SMOKING IF FLAVOURS WERE UNAVAILABLE     #
  ##################################################
  print("## HOW MANY CURRENT ADULT VAPERS WOULD RELAPSE TO SMOKING IF FLAVOURS WERE UNAVAILABLE ##")
    
  #------------------------#
  # b. Ex-smokers who vape #
  #------------------------#
  print("# Ex-smokers who vape #")
  #ASH base is all flavoured E-cig users

  #In Ex-Smoker subset...
  #ASH
  ASH_A21_XS<-subset(ASHA21, s2_edited2016=="I used to smoke but I have given up now")
  print("ASH Ex-Smokers 2021")
  print(nrow(ASH_A21_XS))
  #STS
  STS_A22_XS<-subset(STSA22, q632a1=="I have stopped smoking completely in the last year" | q632a1=="I stopped smoking completely more than a year ago")
  print("STS Ex-Smokers 2022")
  print(nrow(STS_A22_XS))
  
  #Get number and weight of Ex-Smoker-Current_vaper
  #ASH
  ASH_A21_CV<-subset(ASH_A21_XS, q3=="I have tried e-cigarettes and still use them")
  print("ASH Vapers 2021")
  CV<-print(nrow(ASH_A21_CV))
  print("Weight")
  CVW<-print(mean(ASH_A21_CV$weight))
  #STS
  STS_A22_CV<-subset(STS_A22_XS, qimw866=="Electronic cigarette")
  print("STS Vapers 2022")
  CV2<-print(nrow(STS_A22_CV))
  print("Weight")
  CVW2<-print(mean(STS_A22_CV$weight_gb))
  
  #Get weight of Ex-Smoker-Non-Vaper
  #ASH
  NonVape<-c("I have never heard of e-cigarettes and have never tried them", "I have heard of e-cigarettes but have never tried them", "I have tried e-cigarettes but do not use them (anymore)")
  ASH_A21_NoV<-subset(ASH_A21_XS, q3 %in% NonVape)
  print("ASH Non-Vapers 2021")
  NoV<-print(nrow(ASH_A21_NoV))
  print("Weight")
  NoVW<-print(mean(ASH_A21_NoV$weight))
  #STS
  STS_A22_NoV<-subset(STS_A22_XS, qimw866=="no Electronic cigarette")
  print("STS Non-Vapers 2022")
  NoV2<-print(nrow(STS_A22_NoV))
  print("Weight")
  NoVW2<-print(mean(STS_A22_NoV$weight_gb))
  
  #Calculate percentage
  WCV<-(CV*CVW)+(CV2*CVW2) 
  WNoV<-(NoV*NoVW)+(NoV2*NoVW2)
  print("D.b. %")
  print(WCV/(WCV+WNoV)*100)
  print("D.b. N")
  print(CV+CV2+NoV+NoV2)
  
  #--------------------------------------------------#
  # c. Ex-Smokers who would relapse without flavours #
  #--------------------------------------------------#
  print("# Ex-Smokers who would relapse without flavours #")
  #ASH base is all flavoured E-cig users

  #In Ex-Smoker subset... 
  ASH_A19_XS<-subset(ASHA19, s2_edited2016=="I used to smoke but I have given up now")
  print("Ex-Smoker 2019")
  print(nrow(ASH_A19_XS))
  
  #Get number and weight of Smoke-Insteads
  #ASH
  ASH_A19_SI<-subset(ASH_A19_XS, Q6_NEW2019=="I would smoke more tobacco" | Q6_NEW2019=="I would go back to smoking tobacco")
  print("ASH Smoke-Insteads 2019")
  SI<-print(nrow(ASH_A19_SI))
  print("Weight")
  SIW<-print(mean(ASH_A19_SI$weight))
  #STS
  STS_S22_SI<-subset(STS_A22_XS, yvp33=="I would smoke more tobacco" | yvp34=="I would go back to smoking tobacco")
  print("STS Smoke-Insteads 2022")
  SI2<-print(nrow(STS_S22_SI))
  print("Weight")
  SIW2<-print(mean(STS_S22_SI$weight_gb))
  
  #Get weight of everyone else
  #ASH
  ASH_A19_EE<-subset(ASH_A19_XS, Q6_NEW2019 %in% EvEl)
  print("ASH Everyone Else 2012")
  EE<-print(nrow(ASH_A19_EE))
  print("Weight")
  EEW<-print(mean(ASH_A19_EE$weight))
  #STS
  STS_A22_EE<-subset(STS_A22_XS, yvp33=="no I would smoke more tobacco" & yvp34=="no I would go back to smoking tobacco")
  print("STS Everyone Else 2022")
  EE2<-print(nrow(STS_A22_EE))
  print("Weight")
  EEW2<-print(mean(STS_A22_EE$weight_gb))
  
  #Calculate percentage
  WSI<-(SI*SIW)+(SI2*SIW2) 
  WEE<-(EE*EEW)+(EE2*EEW2)
  print("D.c. %")
  print(WSI/(WSI+WEE)*100)
  print("D.c. N")
  print(SI+SI2+EE+EE2)
  
  #Stop writing
  sink()
}

#Run for general Population
grade<-""
analyse(grade)

#Run for low social grade
grade<-"_c2de"
analyse(grade)

#######
# USA #
#######

sink("NYTS.txt")
print("## USA - NYTS ##")

#-------------------#
# A.b Never Smokers #
#-------------------#
print("# Never Smokers #")

#Get number and weight of Never Smoker subset... 
NYTS_21_NS<-subset(NYTS_21, QN35==2)
print("Never-Smokers 2021")
NS<-print(nrow(NYTS_21_NS))
print("Weight")
NSW<-print(mean(NYTS_21_NS$finwgt))

#Get number and weight of Smokers
NYTS_21_S<-subset(NYTS_21, QN35==1)
print("Smokers 2021")
S<-print(nrow(NYTS_21_S))
print("Weight")
SW<-print(mean(NYTS_21_S$finwgt))

#Calculate percentage
WNS<-NS*NSW
WS<-S*SW 
print("A.b. %")
print(WNS/(WNS+WS)*100)
print("A.b. N")
print(NS+S)

#-----------------------------#
# A.c Never-Smokers who vaped #
#-----------------------------#
print("# Non-Smokers who Vaped #")

#Get number and weight of Vape subset... 
NYTS_21_NSEV<-subset(NYTS_21_NS, QN6==1)
print("Never-Smokers-Ever-Vapers 2021")
NSEV<-print(nrow(NYTS_21_NSEV))
print("Weight")
NSEVW<-print(mean(NYTS_21_NSEV$finwgt))

#Get number and weight of Never-Smokers-Never-Vapers
NYTS_21_NSNV<-subset(NYTS_21_NS, QN6==2)
print("Never-Smokers-Never-Vapers 2021")
NSNV<-print(nrow(NYTS_21_NSNV))
print("Weight")
NSNVW<-print(mean(NYTS_21_NSNV$finwgt))

#Calculate percentage
WNSEV<-NSEV*NSEVW
WNSNV<-NSNV*NSNVW 
print("A.c. %")
print(WNSEV/(WNSEV+WNSNV)*100)
print("A.c. N")
print(NSEV+NSNV)

#-------------------------------#
# A.d. Vape Because of Flavours #
#-------------------------------#
print("# Vape Because of Flavours #")

#Get number and weight of Like Flavours subset... 
NYTS_21_LF<-subset(NYTS_21_NSEV, QN11H==1)
print("Like Flavours 2021")
LF<-print(nrow(NYTS_21_LF))
print("Weight")
LFW<-print(mean(NYTS_21_LF$finwgt))

#Get number and weight of Everyone Else
NYTS_21_EE<-subset(NYTS_21_NSEV, is.na(QN11H))
print("Everyone Else 2021")
EE<-print(nrow(NYTS_21_EE))
print("Weight")
EEW<-print(mean(NYTS_21_EE$finwgt))

#Calculate percentage
WLF<-LF*LFW
WEE<-EE*EEW 
print("A.d. %")
print(WLF/(WLF+WEE)*100)
print("A.d. N")
print(LF+EE)

sink()
