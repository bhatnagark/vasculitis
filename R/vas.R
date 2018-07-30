#' @title Merge the 5 files and gives one in-line CSV and one binded file.
#'
#' @description takes the address of files from the user using prompt and pass these address to another function which cleans and binds the file.The computation time can be high depending upon the size of the file. Be patient.
#' @param d address of the directory
#' @param f1 name/address of the biopsy csv
#' @param f2 name/address of the treatment file
#' @param f3 name/address of the demographic file
#' @param f4 name/aadress of the encounter file
#' @param f5 name/address of the complication file
#' @return NULL



#This Function takes the file path of the files as an arguement and returns the merged in-line file and a binded CSV
myfunction<-function(d,f1,f2,f3,f4,f5){
  #install.packages("dplyr")
  #library(dplyr)
  #install.packages("gtools")
  #library(gtools)

  #user defined directory
  setwd(dir=d)
  set.seed(101)

  #read biopsy file
  data_1 <- read.csv(f1,header = FALSE)
  names(data_1) <- as.matrix(data_1[1, ])
  data_1 <- data_1[-1, ]
  data_1[] <- lapply(data_1, function(x) type.convert(as.character(x)))

  #read complication file
  data_2<-read.csv(f2)
  names(data_2) <- as.matrix(data_2[1, ])
  data_2 <- data_2[-1, ]
  data_2[] <- lapply(data_2, function(x) type.convert(as.character(x)))

  #read demographic file
  data_3<-read.csv(f3)
  names(data_3) <- as.matrix(data_3[1, ])
  data_3 <- data_3[-1, ]
  data_3[] <- lapply(data_3, function(x) type.convert(as.character(x)))

  #read encounter file
  data_4<-read.csv(f4)
  names(data_4) <- as.matrix(data_4[1, ])
  data_4 <- data_4[-1, ]
  data_4[] <- lapply(data_4, function(x) type.convert(as.character(x)))

  #read treatment file
  data_5<-read.csv(f5, header = FALSE)
  names(data_5) <- as.matrix(data_5[1, ])
  data_5 <- data_5[-1, ]
  data_5[] <- lapply(data_5, function(x) type.convert(as.character(x)))

  df<-merge(data_1, data_2, by = "Patient Id")
  df1<-merge(df, data_3, by = "Patient Id")
  df2<-merge(df1, data_4, by = "Patient Id")
  df3<-merge(df2,data_5, by = "Patient Id")

  write.csv(df3, file = "mergedT.csv")
  merged_inline= read.csv("mergedT.csv", header = TRUE, check.names = F, as.is=TRUE)
  missing_df<-merged_inline[,3572:3629]
  merged_inline1<-merge(merged_inline,missing_df,by=0)
  merged_inline1<-merged_inline1[,c(3:3550,5359:5416,3551:5358)]
  write.csv(merged_inline1, file = "mergedf.csv")
  merged_inline<-read.csv("mergedF.csv", header = TRUE, check.names = F, as.is=TRUE)

  colnames(merged_inline[721])
  colnames(merged_inline[5102])

  encounters_inline= merged_inline[,721:5160]
  pID= merged_inline[2]
  encounters= data.frame(pID, encounters_inline, check.names = F)

  for (i in 1:4441){
    temp= names(encounters[i])
    substr(temp, 1,12)
    #wildcard search for Treamtment Na to remove Umlaut character
    if (grepl(glob2rx("Treatment Na*"),substr(temp, 1,12))){
      names(encounters)[i]= "Treatment Naive (Never on Immunosuppression)"
    }
  }


  encountersPID= encounters[,-(2:4)]
  encountersNPID= encounters[,-(1:4)]

  #######  Final Loop - encounter_inline to segmented by date #########################################
  patients= encountersNPID[encountersPID$"Patient Id"==1,]
  patients1 = patients
  encountersComplete= patients1[0,1:103]

  l <- list()
  h=1
  for(j in 1:4437){
    # LenName= nchar(names(encountersNPID[j]))
    ColName= substr(colnames(encountersNPID[j]),1,13)
    if(ColName=="Date Of Visit"){
      l[[h]] = j-1
      h=h+1
    }
  }


  for(q in 1:682){
    print(q)
    patients= encountersNPID[encountersPID$"Patient Id"==pID[q,],]
    patients1 = patients
    i=1
    n=1
    lines= patients1[0,1:103]

    for(j in 1:4437){

      if(j %in% l){
        n=n+1
        i=i+1
        # encountersComplete= rbind(encountersComplete, lines)
      }
      for(m in 1:103){
        LenName= nchar(names(lines[m]))
        ColName= substr(colnames(patients1[j]),1,LenName)
        if(ColName == names(lines[m])){
          lines[i,m] = patients1[,j]
          break
        }
      }
    }
    encountersComplete= rbind(encountersComplete, lines)
  }


  ##########################    BIOPSY   ############################################

  biopsy_inline= merged_inline[,2:87]
  biopsy_inline

  pID= merged_inline[2]

  for (i in 1:86){
    temp= names(biopsy_inline[i])
    # substr(temp, 1,40)
    #wildcard search for Treamtment Na to remove Umlaut character
    if (grepl(glob2rx("Number Glomeruli with active vasculitic*"),substr(temp, 1,39))){
      names(biopsy_inline)[i]= "Number Glomeruli with active vasculitic lesions(thrombosis + tuft disruption + cells in Bowman's space/crescents))"
    }
  }

  for (i in 1:86){
    temp= names(biopsy_inline[i])
    # substr(temp, 1,40)
    #wildcard search for Treamtment Na to remove Umlaut character
    if (grepl(glob2rx("Site of biopsy - othe*"),substr(temp, 1,21))){
      names(biopsy_inline)[i]= "Site of other biopsy"
    }
  }

  #######  Final Loop - biopsy_inline to segmented by date #########################################
  # m=2
  biopsyPID= biopsy_inline
  biopsyNPID= biopsy_inline[,-(1)]

  biop= biopsy_inline[biopsyPID$"Patient Id"==1,]
  biop1 = biop
  biopsyComplete= biop1[0,1:17]


  for(q in 1:682){
    biop= biopsyNPID[biopsyPID$"Patient Id"==pID[q,],]
    biop1 = biop
    i=1
    n=1
    lines= biop1[0,1:17]

    for(j in 1:85){
      for(m in 1:17){
        LenName= nchar(names(lines[m]))
        ColName= substr(colnames(biop1[j]),1,LenName)
        if(ColName == names(lines[m])){
          lines[i,m] = biop1[,j]
          print(j)
          #print(m)
          break
        }
      }
      if(j==17*n){
        n=n+1
        i=i+1
      }
    }
    biopsyComplete= rbind(biopsyComplete, lines)
  }

  ############################################################################################



  ################################    TREAMTMENT   ###########################################

  treatment_inline= merged_inline[,88:503]
  treatment_inline

  pID= merged_inline[2]
  treatment_inline= data.frame(pID, treatment_inline, check.names = F)


  ##########      Continuous Medication     ############

  continuous_medication= treatment_inline[,1:239]

  #######  Final Loop - to get the data segmented by date ###################################
  continuous_medicationPID= continuous_medication
  continuous_medicationNPID= continuous_medication[,-(1)]

  c_med= continuous_medicationNPID[continuous_medicationPID$"Patient Id"==1,]
  c_med1 = c_med
  c_med_complete= c_med1[0,1:7]

  for(q in 1:682){
    c_med= continuous_medicationNPID[continuous_medicationPID$"Patient Id"==pID[q,],]
    c_med1 = c_med
    i=1
    n=1
    lines= c_med1[0,1:7]

    for(j in 1:238){
      for(m in 1:7){
        LenName= nchar(names(lines[m]))
        ColName= substr(colnames(c_med1[j]),1,LenName)
        if(ColName == names(lines[m])){
          lines[i,m] = c_med1[,j]
          print(j)
          break
        }
      }
      if(j==7*n){
        n=n+1
        i=i+1
      }
    }
    c_med_complete= rbind(c_med_complete, lines)
  }

  ##########################################

  ##########      Therapy       ############

  ivtherapy= treatment_inline[,240:359]
  pID= merged_inline[2]
  ivtherapy= data.frame(pID, ivtherapy, check.names = F)

  for (i in 1:121){
    temp= names(ivtherapy[i])
    if (grepl(glob2rx("IV therapy - othe*"),substr(temp, 1,17))){
      names(ivtherapy)[i]= "Other IV Therapy"
    }
  }

  ivtherapyPID= ivtherapy
  ivtherapyNPID= ivtherapy[,-(1)]

  iv_t= ivtherapyNPID[ivtherapyPID$"Patient Id"==1,]
  iv_t1 = iv_t
  iv_t_complete= iv_t1[0,1:5]

  for(q in 1:682){
    iv_t= ivtherapyNPID[ivtherapyPID$"Patient Id"==pID[q,],]
    iv_t1 = iv_t
    i=1
    n=1
    lines= iv_t1[0,1:5]

    for(j in 1:120){
      for(m in 1:5){
        LenName= nchar(names(lines[m]))
        ColName= substr(colnames(iv_t1[j]),1,LenName)
        if(ColName == names(lines[m])){
          lines[i,m] = iv_t1[,j]
          print(j)
          break
        }
      }
      if(j==5*n){
        n=n+1
        i=i+1
      }
    }
    iv_t_complete= rbind(iv_t_complete, lines)
  }

  ##########################################

  #########      Plasma Exchange     #######

  plexchange= treatment_inline[,360:413]
  pID= merged_inline[2]
  plexchange= data.frame(pID, plexchange, check.names = F)

  plexchangePID= plexchange
  plexchangeNPID= plexchange[,-(1)]

  pl_ex= plexchangeNPID[plexchangePID$"Patient Id"==1,]
  pl_ex1 = pl_ex
  pl_ex_complete= iv_t1[0,1:3]

  for(q in 1:682){
    pl_ex= plexchangeNPID[plexchangePID$"Patient Id"==pID[q,],]
    pl_ex1 = pl_ex
    i=1
    n=1
    lines= pl_ex1[0,1:3]

    for(j in 1:54){
      for(m in 1:5){
        LenName= nchar(names(lines[m]))
        ColName= substr(colnames(pl_ex1[j]),1,LenName)
        if(ColName == names(lines[m])){
          lines[i,m] = pl_ex1[,j]
          print(j)
          break
        }
      }
      if(j==3*n){
        n=n+1
        i=i+1
      }
    }
    pl_ex_complete= rbind(pl_ex_complete, lines)
  }

  ############################################################################################

  ######  Add the two columns from treatment - Donor Type and Date of transplant

  ############################################################################################


  ##############################      Complications     ######################################


  complications_inline= merged_inline[,5161:5415]
  pID= merged_inline[2]
  complications_inline= data.frame(pID, complications_inline, check.names = F)


  complicationsPID= complications_inline
  complicationsNPID= complications_inline[,-(1)]

  comp= complicationsNPID[complicationsPID$"Patient Id"==1,]
  comp1 = comp
  comp_complete= comp1[0,1:11]


  for(q in 1:682){
    print(q)
    comp= complicationsNPID[complicationsPID$"Patient Id"==pID[q,],]
    comp1 = comp
    i=1
    n=1
    lines= comp1[0,1:11]

    for(j in 1:255){
      for(m in 1:11){
        LenName= nchar(names(lines[m]))
        ColName= substr(colnames(comp1[j]),1,LenName)
        # substr(colnames(patients1[104]), 1, 13)
        # print(ColName)
        if(ColName == names(lines[m])){
          lines[i,m] = comp1[,j]
          break
        }
      }
      if(j==11*n){
        n=n+1
        i=i+1
      }
    }
    comp_complete= rbind(comp_complete, lines)
  }

  ############################################################################################


  ############################       DEMOGRAPHICS      #######################################

  demographics_inline= merged_inline[,504:720]
  pID= merged_inline[2]
  # demographics_inline= data.frame(pID, demographics_inline, check.names = F)
  colAdd = c("PatientID", "Label")
  demographics_inline[ , colAdd] <- NA
  demographics_inline["Label"]= "Demographics"
  demographics_inline["PatientID"]=pID
  demographicsComplete1= demographics_inline[c(218:219,1:217)]

  ############################################################################################


  ####################       FINAL EXCEL        ##############################################

  pID= merged_inline[2]

  ###  ENCOUNTERS
  divEncounter= nrow(encountersComplete)/682
  encountersComplete1=encountersComplete
  colAdd = c("PatientID", "Label")
  encountersComplete1[ , colAdd] <- NA
  encountersComplete1["Label"]= "Encounters"
  encountersComplete1= encountersComplete1[c(104:105,1:103)]
  n=1
  rangeEncounter= nrow(encountersComplete)
  for(i in 1:rangeEncounter){
    encountersComplete1[i,1]= pID[n,1]
    if(i==n*divEncounter){
      n=n+1
    }
  }

  ###  BIOPSY
  divBiopsy= nrow(biopsyComplete)/nrow(biopsy_inline)
  biopsyComplete1=biopsyComplete
  colAdd = c("PatientID", "Label")
  biopsyComplete1[ , colAdd] <- NA
  biopsyComplete1["Label"]= "Biopsy"
  biopsyComplete1= biopsyComplete1[c(18:19,1:17)]
  n=1
  rangeBiopsy= nrow(biopsyComplete1)
  for(i in 1:rangeBiopsy){
    biopsyComplete1[i,1]= pID[n,1]
    if(i==n*divBiopsy){
      n=n+1
    }
  }

  ###  ContinuousMedication
  div_c_med= nrow(c_med_complete)/nrow(continuous_medication)
  c_med_complete1=c_med_complete
  colAdd = c("PatientID", "Label")
  c_med_complete1[ , colAdd] <- NA
  c_med_complete1["Label"]= "Continuous_Medication"
  c_med_complete1= c_med_complete1[c(8:9,1:7)]
  n=1
  rangeCmed= nrow(c_med_complete)
  for(i in 1:rangeCmed){
    c_med_complete1[i,1]= pID[n,1]
    if(i==n*div_c_med){
      n=n+1
    }
  }

  ###  Therapy
  ivt_div= nrow(iv_t_complete)/nrow(ivtherapy)
  iv_t_complete1=iv_t_complete
  colAdd = c("PatientID", "Label")
  iv_t_complete1[ , colAdd] <- NA
  iv_t_complete1["Label"]= "IV_Therapy"
  iv_t_complete1= iv_t_complete1[c(6:7,1:5)]
  n=1
  rangeCmed= nrow(c_med_complete)
  for(i in 1:rangeCmed){
    iv_t_complete1[i,1]= pID[n,1]
    if(i==n*ivt_div){
      n=n+1
    }
  }

  ###  PlasmaExchange
  pl_ex_div= nrow(pl_ex_complete)/nrow(plexchange)
  pl_ex_complete1=pl_ex_complete
  colAdd = c("PatientID", "Label")
  pl_ex_complete1[ , colAdd] <- NA
  pl_ex_complete1["Label"]= "Plasma_Exchange"
  pl_ex_complete1= pl_ex_complete1[c(4:5,1:3)]
  n=1
  rangePlex= nrow(pl_ex_complete)
  for(i in 1:rangePlex){
    pl_ex_complete1[i,1]= pID[n,1]
    if(i==n*pl_ex_div){
      n=n+1
    }
  }

  ###  Complications
  divComplications= nrow(comp_complete)/nrow(complications_inline)
  comp_complete1=comp_complete
  colAdd = c("PatientID", "Label")
  comp_complete1[ , colAdd] <- NA
  comp_complete1["Label"]= "Complications"
  comp_complete1= comp_complete1[c(12:13,1:11)]
  n=1
  rangeComp= nrow(comp_complete)
  for(i in 1:rangeComp){
    comp_complete1[i,1]= pID[n,1]
    if(i==n*divComplications){
      n=n+1
    }
  }

  ###  Changing Dates to Common Dates
  names(encountersComplete1)[3]= "Date"
  names(biopsyComplete1)[3]= "Date"
  names(c_med_complete1)[7]= "Date"
  names(iv_t_complete1)[5]= "Date"
  names(pl_ex_complete1)[4]= "Date"
  names(comp_complete1)[3]= "Date"
  names(demographicsComplete1)[205]= "Date"

  getwd()

  ############################################################################################
  # File Intergrity check
  write.csv(encountersComplete1, file = "enc.csv")
  write.csv(biopsyComplete1, file = "bio.csv")
  write.csv(c_med_complete1, file = "c_med.csv")
  write.csv(iv_t_complete1, file = "iv.csv")
  write.csv(pl_ex_complete1, file = "plex.csv")
  write.csv(comp_complete1, file = "comp.csv")
  write.csv(demographicsComplete1, file = "demo.csv")
  ############################################################################################

  temp1<-smartbind(encountersComplete1,biopsyComplete1,c_med_complete1,iv_t_complete1,pl_ex_complete1,demographicsComplete1,comp_complete1)
  temp1 <- temp1[!(temp1$Date == ""),]
  temp1 <- temp1[!(is.na(temp1$PatientID)),]
  temp1 <- temp1[!(temp1$PatientID=="NA"),]
  N <- 2
  for(i in 1:N){
    temp1<-temp1[-1]
    i=i+1
  }
  write.csv(temp1,file = "bindf.csv")


}

#This function takes the address of the directory and files and passess the address to the main function
address<-function(){
  d<-readline(prompt = "enter the working directory.All merged file be saved at this location--")
  f1<-readline(prompt = "enter the path of the biopsy file--")
  f2<-readline(prompt = "enter the path of the treatment file--")
  f3<-readline(prompt = "enter the path of the demograohic file--")
  f4<-readline(prompt = "enter the path of the encounter file--")
  f5<-readline(prompt = "enter the path of the complication file--")

  #function invocation with path address as arguement
  myfunction(d,f1,f2,f3,f4,f5)

}

#address()
