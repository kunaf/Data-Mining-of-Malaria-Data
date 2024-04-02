#import libraries
library(arules)
library(arulesViz)
library(dplyr)

# rename dataframe
MalariaData <- Malaria_data_Translated

# set N/A fields to 0
MalariaData[is.na(MalariaData)] = 0

#round up figures to 1 decimal place
is.num <- sapply(MalariaData, is.numeric)
MalariaData[is.num] <- lapply(MalariaData[is.num], round, 1)

#drop column total confirmed cases of severe malaria cuz it affected the data set
MalariaDaata <- select(MalariaData, 1:25, 27:58)
View(MalDaata)

#remove outlier values
i <-9 #begin from column 9
while(i<(dim(MalariaData)[2])){
  MalariaData <- filter(MalariaData, MalariaData[i]<=100)
  i<-i+1
}

# categorize the data
MalariaData[["Suspected cases of simple malaria (< 5 years old) p" ]] <-
  cut(MalariaData[["Suspected cases of simple malaria (< 5 years old) p" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[["Suspected cases of simple malaria (total) p"  ]] <-
  cut(MalariaData[["Suspected cases of simple malaria (total) p"  ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[["Suspected cases of severe malaria (>=5 years old) p"   ]] <-
  cut(MalariaData[["Suspected cases of severe malaria (>=5 years old) p"   ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[[ "Suspected cases of severe malaria (< 5 years old) p"  ]] <-
  cut(MalariaData[[ "Suspected cases of severe malaria (< 5 years old) p"  ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[["Suspected cases of simple malaria simple (5>=5 years old p)"]] <-
  cut(MalariaData[["Suspected cases of simple malaria simple (5>=5 years old p)"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[["Suspected cases of severe malaria P" ]] <-
  cut(MalariaData[["Suspected cases of severe malaria P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[["Suspected cases of malaria (>=5 years old) p" ]] <-
  cut(MalariaData[["Suspected cases of malaria (>=5 years old) p" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[[ "Suspected cases of malaria (< 5 years old) p" ]] <-
  cut(MalariaData[[ "Suspected cases of malaria (< 5 years old) p" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[["Suspected cases of malaria (pregnant women) P"  ]] <-
  cut(MalariaData[["Suspected cases of malaria (pregnant women) P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
View(MalariaData)
MalariaData[["Suspected cases of severe malaria (pregnant women) p" ]] <-
  cut(MalariaData[["Suspected cases of severe malaria (pregnant women) p"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[["Suspected cases of malaria P" ]] <-
  cut(MalariaData[["Suspected cases of malaria P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[[ "Confirmed cases of simple malaria (< 5 years old) P"]] <-
  cut(MalariaData[[ "Confirmed cases of simple malaria (< 5 years old) P"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[["Confirmed cases of simple malaria >=5 years old) P" ]] <-
  cut(MalariaData[[ "Confirmed cases of simple malaria >=5 years old) P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[["Confirmed cases of severe malaria (< 5 years old) P"]] <-
  cut(MalariaData[["Confirmed cases of severe malaria (< 5 years old) P"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[["Confirmed cases of severe malaria (>=5 years old) P" ]] <-
  cut(MalariaData[["Confirmed cases of severe malaria (>=5 years old) P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
colnames(MalariaData)
MalariaData[["Confirmed cases of simple malaria P" ]] <-
  cut(MalariaData[["Confirmed cases of simple malaria P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[[ "Confirmed cases of severe malaria (pregnant women) P" ]] <-
  cut(MalariaData[[ "Confirmed cases of severe malaria (pregnant women) P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[["Confirmed malaria cases (< 5 years old) P" ]] <-
  cut(MalariaData[["Confirmed malaria cases (< 5 years old) P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[["Confirmed malaria cases >=5 years old) P" ]] <-
  cut(MalariaData[["Confirmed malaria cases >=5 years old) P"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[[ "Confirmed malaria cases (pregnant women)  P" ]] <-
  cut(MalariaData[[ "Confirmed malaria cases (pregnant women)  P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[["Confirmed malaria cases P"]] <-
  cut(MalariaData[["Confirmed malaria cases P"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[[ "Number of RDTs performed (< 5 years old)  P" ]] <-
  cut(MalariaData[[ "Number of RDTs performed (< 5 years old)  P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[["Number of RDTs performed (>=5 years old) P"]] <-
  cut(MalariaData[["Number of RDTs performed (>=5 years old) P"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[[ "Number of RDTs performed (pregnant women) P"]] <-
  cut(MalariaData[[ "Number of RDTs performed (pregnant women) P"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[["Number of RDTs performed P" ]] <-
  cut(MalariaData[["Number of RDTs performed P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
colnames(MalariaData)
MalariaData[[ "Number of positive RDTs(< 5 years old) P" ]] <-
  cut(MalariaData[[ "Number of positive RDTs(< 5 years old) P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[["Number of positive RDTs(5>=5 years old) P" ]] <-
  cut(MalariaData[["Number of positive RDTs(5>=5 years old) P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[["Number of positive RDTs(pregnant women) P"]] <-
  cut(MalariaData[["Number of positive RDTs(pregnant women) P"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[[ "Number of positive RDTs P" ]] <-
  cut(MalariaData[[ "Number of positive RDTs P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[[ "Number of EGs carried out (< 5 years old) P"]] <-
  cut(MalariaData[[ "Number of EGs carried out (< 5 years old) P"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[[ "Number of EGs carried out (>=5 years old) P" ]] <-
  cut(MalariaData[[ "Number of EGs carried out (>=5 years old) P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[["Number of EGs carried out  P"  ]] <-
  cut(MalariaData[["Number of EGs carried out  P"  ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[["Number of EGs carried out (pregnant women) P" ]] <-
  cut(MalariaData[["Number of EGs carried out (pregnant women) P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[["Number of positive EGs (< 5 years old) P"]] <-
  cut(MalariaData[["Number of positive EGs (< 5 years old) P"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[[ "Number of positive EGs P" ]] <-
  cut(MalariaData[[ "Number of positive EGs P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[[ "Number of deaths from all causes P"  ]] <-
  cut(MalariaData[[ "Number of deaths from all causes P"  ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[["Number of deaths from all causes (>=5 years old) P" ]] <-
  cut(MalariaData[["Number of deaths from all causes (>=5 years old) P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[[ "Number of positive EGs (pregnant women) P" ]] <-
  cut(MalariaData[[ "Number of positive EGs (pregnant women) P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[["Number of positive EGs (>=5 years old) P" ]] <-
  cut(MalariaData[["Number of positive EGs (>=5 years old) P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[["Number of deaths from all causes (< 5 years old) P" ]] <-
  cut(MalariaData[["Number of deaths from all causes (< 5 years old) P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
MalariaData[["Number of deaths from all causes (pregnant women) P"]] <-
  cut(MalariaData[["Number of deaths from all causes (pregnant women) P"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)

MalariaData[["Number of malaria deaths(< 5 years old) P"]] <-
      cut(MalariaData[["Number of malaria deaths(< 5 years old) P"]],
           breaks = c(0,25,50,75,101),
           labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
            right = FALSE)
MalariaData[["Number of malaria deaths(>=5 years old) P" ]] <-
       cut(MalariaData[["Number of malaria deaths(>=5 years old) P" ]],
           breaks = c(0,25,50,75,101),
            +         labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
            +         right = FALSE)
MalariaData[["Number of malaria deaths P"]] <-
       cut(MalariaData[["Number of malaria deaths P"]],
            +         breaks = c(0,25,50,75,101),
            +         labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
            +         right = FALSE)
MalariaData[["Suspected malaria cases (>=5 years old) P"]] <-
  +     cut(MalariaData[["Suspected malaria cases (>=5 years old) P"]],
            +         breaks = c(0,25,50,75,101),
            +         labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
            +         right = FALSE)
MalariaData[["Number of malaria deaths(pregnant women) P"]] <-
  +     cut(MalariaData[["Number of malaria deaths(pregnant women) P"]],
            +         breaks = c(0,25,50,75,101),
            +         labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
            +         right = FALSE)
MalariaData[["Suspected malaria cases (< 5 years old) P"]] <-
  +     cut(MalariaData[["Suspected malaria cases (< 5 years old) P"]],
            +         breaks = c(0,25,50,75,101),
            +         labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
            +         right = FALSE)
MalariaData[["Suspected malaria cases (pregnant women) P"]] <-
  +     cut(MalariaData[["Suspected malaria cases (pregnant women) P"]],
            +         breaks = c(0,25,50,75,101),
            +         labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
            +         right = FALSE)
MalariaData[["Suspected malaria cases P"]] <-
  +     cut(MalariaData[["Suspected malaria cases P"]],
            +         breaks = c(0,25,50,75,101),
            +         labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
            +         right = FALSE)
#remove columns region and consultations
MalariaData <- select(MalariaData, 1, 3:4, 9:57)

#create transactions file
MalariaData <- as(MalariaData, "transactions")

#run apriori algorithm limits len of rules with maxlen and minlen
MalariaData_rules <- apriori(MalariaData, parameter = list(support = 0.01, confidence = 0.8, minlen = 1, maxlen = 3))

MalariaData_rules <- apriori(MalariaData, parameter = list(support = 0.3, confidence = 0.8))

#no limit to lenght of rules
MalariaData_rules <- apriori(MalariaData, parameter = list(support = 0.4, confidence = 0.8))

#convert rules to dataframe object
MalariaData_rulesDF <- DATAFRAME(MalariaData_rules, setStart='', setEnd='', separate = TRUE)

#filter rules wrt suspected malaria cases on the rhs
SuspectedCases_rules <- subset(MalariaData, subset=rhs %pin% 'Suspected cases')

# filter rules with lift > 1.5
filtered_rules_lift <- subset(rules, subset = lift > 1.5)

# write rules to a csv file
write(filtered_rules_lift, file = "data.csv")

# filter redundant rules
nonr_rules <- filtered_rules_lift[!is.redundant(filtered_rules_lift)] 

#rmove redundant rules does not remove any rules after reducing with maxlen
#filter data wrt to maxlen (size per row) and get a set of 1000 rules
#maxlen = 2
rules_maxlen2 <- subset(MalData_rules, subset = (size(MalData_rules) == 2))
#maxlen = 3
rules_maxlen3 <- subset(MalData_rules, subset = (size(MalData_rules) == 3))
rules3_top <- subset(rules_maxlen3, subset = lift > 1.1)
#maxlen = 4
rules_maxlen4 <- subset(MalData_rules, subset = (size(MalData_rules) == 4))
rules4_top <- subset(rules_maxlen4, subset = lift > 1.15)
#maxlen = 5
rules_maxlen5 <- subset(MalData_rules, subset = (size(MalData_rules) == 5))
rules5_top <- subset(rules_maxlen5, subset = lift > 1.6)
#maxlen = 6
rules_maxlen6 <- subset(MalData_rules, subset = (size(MalData_rules) == 6))
rules6_top <- subset(rules_maxlen6, subset = lift > 1.8)
#maxlen = 7
rules_maxlen7 <- subset(MalData_rules, subset = (size(MalData_rules) == 7))
rules7_top <- subset(rules_maxlen7, subset = lift > 1.85)
#maxlen = 8
rules_maxlen8 <- subset(MalData_rules, subset = (size(MalData_rules) == 8))
rules8_top <- subset(rules_maxlen8, subset = lift > 1.85)
#maxlen = 9
rules_maxlen9 <- subset(MalData_rules, subset = (size(MalData_rules) == 9))
rules9_top <- subset(rules_maxlen9, subset = lift > 1.5)
                              
filt <- subset(Confirmed_size3, subset=!lhs %pin% 'Region')

Akwaya = filter(MalariaDataBackup, District == "Akwaya")

#filter rules wrt to value on the rhs
#deaths
Deaths <- subset(MalariaData, subset=rhs %pin% 'Deaths')
subDeaths <- Deaths_size2[quality(Deaths_size2)$support > 0.7]#set support > 0.7
#Confirmedcases
Confirmed_size4 <- subset(rules_maxlen4, subset=rhs %pin% 'Confirmed cases')

#District
D_Limbe = filter(MalariaDataBackup, District == "Limbe")
D_Tiko = filter(MalariaDataBackup, District == "Tiko")
 D_Konye = filter(MalariaDataBackup, District == "Konye")
 D_Kumba = filter(MalariaDataBackup, District == "Kumba")
 D_Mundemba = filter(MalariaDataBackup, District == "Mundemba")
 D_Mbonge = filter(MalariaDataBackup, District == "Mbonge")
 D_Tombel = filter(MalariaDataBackup, District == "Tombel")
 D_Nguti = filter(MalariaDataBackup, District == "Nguti")
D_Wabane = filter(MalariaDataBackup, District == "Wabane")
 D_Eyumodjock = filter(MalariaDataBackup, District == "Eyumodjock")
D_Mamfe = filter(MalariaDataBackup, District == "Mamfe")
D_Fontem = filter(MalariaDataBackup, District == "Fontem")
D_Buea = filter(MalariaDataBackup, District == "Buea")
D_EkondoTiti = filter(MalariaDataBackup, District == "Ekondo Titi")
D_Bakassi = filter(MalariaDataBackup, District == "Bakassi")
D_Bangem = filter(MalariaDataBackup, District == "Bangem")
D_Muyuka = filter(MalariaDataBackup, District == "Muyuka")
D_Akwaya = filter(MalariaDataBackup, District == "Akwaya")

Bakassi <- as(Bakassi, "transactions")

#tests


library(sythpop)
#Generate Synthetic Data
sds_default <- syn(Malaria_data_Translated) #general

my.seed <- 17914709
Malaria_sds <- syn(Malaria_data_Test_Translated_, method = "ctree", m = 10, seed = my.seed)


#export synthetic data file to csv
write.syn(object, filename, 
          filetype = c("SPSS", "Stata", "SAS", "csv", "tab", "rda", "RData", "txt"), 
          convert.factors = "numeric", data.labels = NULL, save.complete = TRUE, 
          extended.info = TRUE, ...)

#compare origiinal dataset with syntheti dataset
compare(sds_default, Malaria_data_Test_Translated_)

#how large should the synthetic data be 
# rename dataframe
SynthMalariaData <- Synth_10636

# set N/A fields to 0
SynthMalariaData[is.na(SynthMalariaData)] = 0

#round up figures to 1 decimal place
is.num <- sapply(SynthMalariaData, is.numeric)
SynthMalariaData[is.num] <- lapply(SynthMalariaData[is.num], round, 2)

#drop column total confirmed cases of severe malaria cuz it affected the data set
SynthMalariaData <- select(SynthMalariaData, 1:27, 29:60)
View(SynthMalariaData)

#remove outlier values
j <-9 #begin from column 9
while(j<(dim(SynthMalariaData)[2])){
  SynthMalariaData <- filter(SynthMalariaData, SynthMalariaData[j]<=100)
  j<-j+1
}

# categorize the data
SynthMalariaData[["Suspected cases of simple malaria (< 5 years old) p" ]] <-
  cut(SynthMalariaData[["Suspected cases of simple malaria (< 5 years old) p" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Suspected cases of simple malaria (total) p"  ]] <-
  cut(SynthMalariaData[["Suspected cases of simple malaria (total) p"  ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Suspected cases of severe malaria (>=5 years old) p"   ]] <-
  cut(SynthMalariaData[["Suspected cases of severe malaria (>=5 years old) p"   ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[[ "Suspected cases of severe malaria (< 5 years old) p"  ]] <-
  cut(SynthMalariaData[[ "Suspected cases of severe malaria (< 5 years old) p"  ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Suspected cases of simple malaria simple (5>=5 years old p)"]] <-
  cut(SynthMalariaData[["Suspected cases of simple malaria simple (5>=5 years old p)"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Suspected cases of severe malaria P" ]] <-
  cut(SynthMalariaData[["Suspected cases of severe malaria P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Suspected cases of malaria (>=5 years old) p" ]] <-
  cut(SynthMalariaData[["Suspected cases of malaria (>=5 years old) p" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[[ "Suspected cases of malaria (< 5 years old) p" ]] <-
  cut(SynthMalariaData[[ "Suspected cases of malaria (< 5 years old) p" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Suspected cases of malaria (pregnant women) P"  ]] <-
  cut(SynthMalariaData[["Suspected cases of malaria (pregnant women) P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
View(SynthMalariaData)
SynthMalariaData[["Suspected cases of severe malaria (pregnant women) p" ]] <-
  cut(SynthMalariaData[["Suspected cases of severe malaria (pregnant women) p"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Suspected cases of malaria P" ]] <-
  cut(SynthMalariaData[["Suspected cases of malaria P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[[ "Confirmed cases of simple malaria (< 5 years old) P"]] <-
  cut(SynthMalariaData[[ "Confirmed cases of simple malaria (< 5 years old) P"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Confirmed cases of simple malaria >=5 years old) P" ]] <-
  cut(SynthMalariaData[[ "Confirmed cases of simple malaria >=5 years old) P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Confirmed cases of severe malaria (< 5 years old) P"]] <-
  cut(SynthMalariaData[["Confirmed cases of severe malaria (< 5 years old) P"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Confirmed cases of severe malaria (>=5 years old) P" ]] <-
  cut(SynthMalariaData[["Confirmed cases of severe malaria (>=5 years old) P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
colnames(SynthMalariaData)
SynthMalariaData[["Confirmed cases of simple malaria P" ]] <-
  cut(SynthMalariaData[["Confirmed cases of simple malaria P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[[ "Confirmed cases of severe malaria (pregnant women) P" ]] <-
  cut(SynthMalariaData[[ "Confirmed cases of severe malaria (pregnant women) P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Confirmed malaria cases (< 5 years old) P" ]] <-
  cut(SynthMalariaData[["Confirmed malaria cases (< 5 years old) P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Confirmed malaria cases >=5 years old) P" ]] <-
  cut(SynthMalariaData[["Confirmed malaria cases >=5 years old) P"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[[ "Confirmed malaria cases (pregnant women)  P" ]] <-
  cut(SynthMalariaData[[ "Confirmed malaria cases (pregnant women)  P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Confirmed malaria cases P"]] <-
  cut(SynthMalariaData[["Confirmed malaria cases P"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[[ "Number of RDTs performed (< 5 years old)  P" ]] <-
  cut(SynthMalariaData[[ "Number of RDTs performed (< 5 years old)  P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Number of RDTs performed (>=5 years old) P"]] <-
  cut(SynthMalariaData[["Number of RDTs performed (>=5 years old) P"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[[ "Number of RDTs performed (pregnant women) P"]] <-
  cut(SynthMalariaData[[ "Number of RDTs performed (pregnant women) P"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Number of RDTs performed P" ]] <-
  cut(SynthMalariaData[["Number of RDTs performed P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
colnames(SynthMalariaData)
SynthMalariaData[[ "Number of positive RDTs(< 5 years old) P" ]] <-
  cut(SynthMalariaData[[ "Number of positive RDTs(< 5 years old) P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Number of positive RDTs(5>=5 years old) P" ]] <-
  cut(SynthMalariaData[["Number of positive RDTs(5>=5 years old) P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Number of positive RDTs(pregnant women) P"]] <-
  cut(SynthMalariaData[["Number of positive RDTs(pregnant women) P"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[[ "Number of positive RDTs P" ]] <-
  cut(SynthMalariaData[[ "Number of positive RDTs P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[[ "Number of EGs carried out (< 5 years old) P"]] <-
  cut(SynthMalariaData[[ "Number of EGs carried out (< 5 years old) P"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[[ "Number of EGs carried out (>=5 years old) P" ]] <-
  cut(SynthMalariaData[[ "Number of EGs carried out (>=5 years old) P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Number of EGs carried out  P"  ]] <-
  cut(SynthMalariaData[["Number of EGs carried out  P"  ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Number of EGs carried out (pregnant women) P" ]] <-
  cut(SynthMalariaData[["Number of EGs carried out (pregnant women) P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Number of positive EGs (< 5 years old) P"]] <-
  cut(SynthMalariaData[["Number of positive EGs (< 5 years old) P"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[[ "Number of positive EGs P" ]] <-
  cut(SynthMalariaData[[ "Number of positive EGs P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[[ "Number of deaths from all causes P"  ]] <-
  cut(SynthMalariaData[[ "Number of deaths from all causes P"  ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Number of deaths from all causes (>=5 years old) P" ]] <-
  cut(SynthMalariaData[["Number of deaths from all causes (>=5 years old) P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[[ "Number of positive EGs (pregnant women) P" ]] <-
  cut(SynthMalariaData[[ "Number of positive EGs (pregnant women) P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Number of positive EGs (>=5 years old) P" ]] <-
  cut(SynthMalariaData[["Number of positive EGs (>=5 years old) P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Number of deaths from all causes (< 5 years old) P" ]] <-
  cut(SynthMalariaData[["Number of deaths from all causes (< 5 years old) P" ]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Number of deaths from all causes (pregnant women) P"]] <-
  cut(SynthMalariaData[["Number of deaths from all causes (pregnant women) P"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)

SynthMalariaData[["Number of malaria deaths(< 5 years old) P"]] <-
  cut(SynthMalariaData[["Number of malaria deaths(< 5 years old) P"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)

SynthMalariaData[["Number of malaria deaths(>=5 years old) P" ]] <-
  cut(SynthMalariaData[["Number of malaria deaths(>=5 years old) P" ]],
      breaks = c(0,25,50,75,101),
          labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
          right = FALSE)
SynthMalariaData[["Number of malaria deaths P"]] <-
  cut(SynthMalariaData[["Number of malaria deaths P"]],
      breaks = c(0,25,50,75,101),
     labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Suspected malaria cases (>=5 years old) P"]] <-
      cut(SynthMalariaData[["Suspected malaria cases (>=5 years old) P"]],
               breaks = c(0,25,50,75,101),
               labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
               right = FALSE)
SynthMalariaData[["Number of malaria deaths(pregnant women) P"]] <-
  cut(SynthMalariaData[["Number of malaria deaths(pregnant women) P"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Number of malaria deaths P"]] <-
  cut(SynthMalariaData[["Number of malaria deaths P"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Suspected malaria cases (< 5 years old) P"]] <-
  cut(SynthMalariaData[["Suspected malaria cases (< 5 years old) P"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Suspected malaria cases (pregnant women) P"]] <-
  cut(SynthMalariaData[["Suspected malaria cases (pregnant women) P"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Suspected malaria cases P"]] <-
  cut(SynthMalariaData[["Suspected malaria cases P"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Suspected malaria cases (>=5 years old) P"]] <-
  cut(SynthMalariaData[["Suspected malaria cases (>=5 years old) P"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)
SynthMalariaData[["Confirmed cases of severe malaria P"]] <-
  cut(SynthMalariaData[["Confirmed cases of severe malaria P"]],
      breaks = c(0,25,50,75,101),
      labels = c("MalariaCat1", "MalariaCat2", "MalariaCat3", "MalariaCat4"),
      right = FALSE)

#remove columns region and consultations
SynthMalariaData <- select(SynthMalariaData, 3:4, 9:58)

#select columns confirmed cases and deaths
SynthConfDeaths <- select(SynthMalariaData, 1:2, 14:24, 45:48)

#select cols confirmed cases and suspected cases
SynthConfSusp <-  select(SynthMalariaData, 1:2, 3:24)

#select cols tests and deaths
SYnthTestDeaths <-  select(SynthMalariaData, 1:2, 25:40, 45:48)

#create transactions file
SynthMalariaData <- as(SynthMalariaData, "transactions")

SynthConfDeaths <- as(SynthConfDeaths, "transactions")

SynthConfSusp <- as(SynthConfSusp, "transactions")

SYnthTestDeaths <- as(SYnthTestDeaths, "transactions")

SYnthTestDeaths_rules <- apriori(SYnthTestDeaths, parameter = list(support = 0.01, confidence = 0.03, maxlen = 4))

SynthConfSusp_rules <- apriori(SynthConfSusp, parameter = list(support = 0.01, confidence = 0.03, minlen = 2, maxlen = 4))

SynthConfDeaths_rules <- apriori(SynthConfDeaths, parameter = list(support = 0.01, confidence = 0.03, maxlen = 6))

WabaneDeaths <- subset(MalariaData_rules3, subset=rhs %pin% 'malaria deaths')
> WabaneDeaths <- subset(WabaneDeaths, subset=lhs %pin% 'Wabane')
> View(WabaneDeaths)


BakassiTest = subset(BakassiTest, subset = support > 0.02)
> BakassiTestDeaths = subset(BakassiTest, subset = rhs %pin% 'Number of malaria deaths')
> BakassiTestDeaths = subset(BakassiTestDeaths, subset = lhs %pin% 'Confirmed')

BakassiTestDeathsA = BakassiTestDeaths[c(2,7,11,14,18,21,25,32,33,40,41,42,46)]


SynthConfDeaths_rulesAthreeN = SynthConfDeaths_rulesAthree[c(1,5,9,11,14,20,22,28,29,33,39,43,46,52,54,60,61,65,71,75,78,84,86,92,95,99,102,108,110,116,119,124,127,130,135,138,144,146,152,153,157,160,164,167,170,175,179,182,18,190,196,198,201,205,212,215,218,223,227,230,236,238,244,247,251,255,257,261,268,271,274,279,282,288,290,296,298,303,305,309,313,317,324,327,330,335,339,342,348,350,356,357,361,368,371,374,379,383,386,392,394,400,403,405,409,416,420,423,426,431,435,438,444,446,452,455,457,461,466,470,472,475,478,483,487,490,496,498,504,507,509,513,520,521,525,532,535,538,543,550,556,558,564,567,571,573,577,582,585,589,596,599,602,607,611,614,620,622,628,630,635,637,641,648,651,654,659,663,666,672,674,680,682,687,689,693,700,701,705,712,715,718,723,727,730,736,738,744,777,781,787,790,795,799,802,808,810,816,819,822,825,829,833,837,844,847,851,855,858,864,866,872)]

SynthConfDeaths_rulesAtwo = SynthConfDeaths_rulesAone[c(1,2,11,16,20,23,26,30,36,37,41,47,50,55,59,62,67,69,73,80,81,85,92,95,98,103,107,110,116,118,124)]

sync; echo 1 > /proc/sys/vm/drop_caches

# sync; echo 2 > /proc/sys/vm/drop_caches

# sync; echo 3 > /proc/sys/vm/drop_caches 


