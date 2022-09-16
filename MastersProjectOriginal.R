pacman::p_load(plyr,dplyr,lavaan, tidyLPA, mclust)
#Importing spss dataset
data = Hmisc::spss.get("~/Documents/MastersProject/NeuralMechanisms_SharedData.sav",use.value.labels = F)
#Creating project dataset with ID, sex, age, Ethnicity, Education, & Income
#project = data.frame(data[c(1,236,3,7,28,29)])
project = data.frame(data[c('Physics.ID', 'Sex.1', 'D1.1', 'D5.1',  'D27.1', 'D4.1')]) 
#Rownames are now participant ID
colnames(project) = c('ID', 'Sex', 'Age', 'Ethnicity','Income', 'GPA');rownames(project) = project$ID

#All three anxiety measures are being placed into their own dataframe
#Science scale is the only scale that has some non-science related items.
#Remove non science questions in items
science = data.frame(data[c(167:210)]); science  = science  %>% select(-starts_with("NScA"))
math = data.frame(data[c(211:235)])
beck = data.frame(data[c(51:71)])

#Importing Years
behave = read.csv("~/Documents/MastersProject/BehavioralData.csv");rownames(behave) = behave$Enrollment.ID
a = project[!(rownames(project) %in% rownames(behave)),] 
temp = matrix(nrow = nrow(a), ncol = ncol(behave)) %>% data.frame()
rownames(temp) = rownames(a);temp[,1] = as.numeric(rownames(a));colnames(temp) = colnames(behave)
behave = rbind(behave, temp);behave = behave[(rownames(project)),]
#Strategy: there is a discrepancy in the participation IDs in the behavioral and fmri data.
#So, to ensure that the correct student is being assigned their fmri or behavioral values,
#their numerical IDs are assigned to rownames and the rownames are sutracted from each other for 
#both datasets. If a series of zeroes is produces, then everything is fine.
which(as.numeric(rownames(project)) - as.numeric(rownames(behave)) > 0)
project$Years = behave$Strt.Level

which(beck[1:ncol(beck)] > 3, arr.ind = T)
which(science[1:ncol(science)] > 4, arr.ind = T)
which(math[1:ncol(math)] > 4, arr.ind = T)

which(beck[1:ncol(beck)] < 0, arr.ind = T)
which(science[1:ncol(science)] < 0, arr.ind = T)
which(math[1:ncol(math)] < 0, arr.ind = T)

#Mean substitution
for(i in 1:ncol(science)) { 
  science[ , i][is.na(science[ , i])] <- mean(science[ , i], na.rm = TRUE)}
for(i in 1:ncol(beck)) { 
  beck[ , i][is.na(beck[ , i])] <- mean(beck[ , i], na.rm = TRUE)}
for(i in 1:ncol(math)) { 
  math[ , i][is.na(math[ , i])] <- mean(math[ , i], na.rm = TRUE)}


which(beck[1:ncol(beck)] > 3, arr.ind = T)
which(science[1:ncol(science)] > 4, arr.ind = T)
which(math[1:ncol(math)] > 4, arr.ind = T)

which(beck[1:ncol(beck)] < 0, arr.ind = T)
which(science[1:ncol(science)] < 0, arr.ind = T)
which(math[1:ncol(math)] < 0, arr.ind = T)

#z-scored
project$SA= rowSums(science[1:ncol(science)])  %>% scale(center = T, scale = T) %>% as.numeric()
project$BK= rowSums(beck[1:ncol(beck)])  %>% scale(center = T, scale = T) %>% as.numeric()
project$MA= rowSums(math[1:ncol(math)])  %>% scale(center = T, scale = T) %>% as.numeric()

which(beck[1:ncol(beck)] > 3, arr.ind = T)
which(science[1:ncol(science)] > 4, arr.ind = T)
which(math[1:ncol(math)] > 4, arr.ind = T)

which(is.na(beck))
which(is.na(math))
which(is.na(science))

#Using normalized scale data for tidyLPA.
project %>% select(SA, MA)  %>%estimate_profiles(1:4) 

project %>% select(SA, MA)  %>%estimate_profiles(1:4) %>% get_fit() 


project%>% select(SA, MA)  %>% estimate_profiles(4) %>% get_estimates()

#Compare solutions.

project %>% select(SA, MA) %>% estimate_profiles(1:4) %>% compare_solutions()


#Graphing results.
project %>% select(SA, MA) %>% estimate_profiles(4) %>% plot_profiles( sd = F,ci = 0.95)

tab = project%>% select(SA, MA)  %>% estimate_profiles(4) %>% get_data()

tab = tab[5:8]

#Filtering Individuals w/ a cprob > 0.7. There are four groups in total.

sum(tab[,1]>=0.7) #n = 8
sum(tab[,2]>=0.7) #n = 73
sum(tab[,3]>=0.7) #n = 27
sum(tab[,4]>=0.7) #n = 5

g1 = which(tab[,1]>= 0.7)
g2 = which(tab[,2]>= 0.7)
g3 = which(tab[,3]>=0.7)
g4 = which(tab[,4]>=0.7)

#Turning demographic data into factors to rename later in the script.
project[c(2,4:5,7)]  = project[c(2,4:5,7)] %>% sapply(function(x) as.factor(x))

#Determining which individuals/rows are in which group for the Low STEM Anxiety group and the High Math Anxiety.
#Including demographic variables for grouped participants
g2dem = project[g2, 2:10]
g3dem = project[g3, 2:10]
g2dem['Anxiety.Profiles'] = rep('Low Anxiety', sum(tab[,2]>0.7)) # CI low for both
g3dem['Anxiety.Profiles'] = rep('High Math Anxiety',sum(tab[,3]>0.7)) # Confidence intervals for math greater than zero, touches for science

#Generating one data table
ndata = rbind(g2dem,g3dem)

#Importing tsv files containing the connectivity information.
fcidanvan = read.table(file = '~/Documents/MastersProject/fci_dan-van_network-connectivity.tsv', sep = '\t', header = TRUE)
fcidandef =  read.table(file = '~/Documents/MastersProject/fci_dan-def_network-connectivity.tsv', sep = '\t', header = TRUE)
fcivandef =  read.table(file = '~/Documents/MastersProject/fci_van-def_network-connectivity.tsv', sep = '\t', header = TRUE)

fcidanvan = fcidanvan[(fcidanvan$X.1=='1'),]
fcidandef = fcidandef[(fcidandef$X.1=='1'),]
fcivandef = fcivandef[(fcivandef$X.1=='1'),]

fcidandefscene = fcidandef[(fcidandef$X.2=='physicsXscenario'),] 
colnames(fcidandefscene) = c('ID', 'Session', 'Condition','dandefscene')
fcidandefq = fcidandef[(fcidandef$X.2=='physicsXquestion'),]
colnames(fcidandefq) = c('ID', 'Session', 'Condition','dandefq')
fcidandefans = fcidandef[(fcidandef$X.2=='physicsXanswers'),]
colnames(fcidandefans) = c('ID', 'Session', 'Condition','dandefans')

fcidanvanscene = fcidanvan[(fcidanvan$X.2=='physicsXscenario'),]
colnames(fcidanvanscene) = c('ID', 'Session', 'Condition','danvanscene')
fcidanvanq = fcidanvan[(fcidanvan$X.2=='physicsXquestion'),]
colnames(fcidanvanq) = c('ID', 'Session', 'Condition','danvanq')
fcidanvanans = fcidanvan[(fcidanvan$X.2=='physicsXanswers'),]
colnames(fcidanvanans) = c('ID', 'Session', 'Condition','danvanans')

fcivandefscene = fcivandef[(fcivandef$X.2=='physicsXscenario'),]
colnames(fcivandefscene) = c('ID', 'Session', 'Condition','vandefscene')
fcivandefq = fcivandef[(fcivandef$X.2=='physicsXquestion'),]
colnames(fcivandefq) = c('ID', 'Session', 'Condition','vandefq')
fcivandefans = fcivandef[(fcivandef$X.2=='physicsXanswers'),]
colnames(fcivandefans) = c('ID', 'Session', 'Condition','vandefans')

pkdanvan =  read.table(file = '~/Documents/MastersProject/reas_dan-van_network-connectivity.tsv', sep = '\t', header = TRUE)
pkdandef = read.table(file = '~/Documents/MastersProject/reas_dan-def_network-connectivity.tsv', sep = '\t', header = TRUE)
pkvandef = read.table(file = '~/Documents/MastersProject/reas_van-def_network-connectivity.tsv', sep = '\t', header = TRUE)

pkdanvan = pkdanvan[(pkdanvan$X.2 =='Reasoning'),]
pkdandef= pkdandef[(pkdandef$X.2 =='Reasoning'),]
pkvandef = pkvandef[(pkvandef$X.2 =='Reasoning'),]

colnames(pkdanvan) = c('ID', 'Session', 'Condition','pkdanvan')
colnames(pkdandef) = c('ID', 'Session', 'Condition','pkdandef')
colnames(pkvandef) = c('ID', 'Session', 'Condition','pkvandef')

pkdanvan = pkdanvan[(pkdanvan$Session =='1'),]
pkdandef= pkdandef[(pkdandef$Session =='1'),]
pkvandef = pkvandef[(pkvandef$Session  =='1'),]

#Creating one dataframe containing all connectivity information
t1= data.frame(fcidandefscene[c(1,2,4)],fcidandefq[4], fcidandefans[4], fcidanvanscene[4], fcidanvanq[4], fcidanvanans[4], fcivandefscene[4], fcivandefq[4], fcivandefans[4])
t1 = data.frame(t1,pkdanvan[4], pkdandef[4], pkvandef[4]);rownames(t1) = t1$ID

#Ensuring the ID's for the connectivity data match the ID's for the dataframe containing the demographic information
rownames(t1) = t1$ID
matchnames = rownames(ndata)
x = ndata[!(rownames(ndata) %in% rownames(t1)),]
temp = matrix(nrow = nrow(x), ncol = ncol(t1));temp = data.frame(temp);rownames(temp) = rownames(x);temp$X1 = c(rownames(x))
colnames(temp) = colnames(t1)
t1 = rbind(t1, temp);t1 = t1[rownames(ndata),];t1 = data.frame(t1, ndata)
which(as.numeric(rownames(ndata)) - as.numeric(rownames(t1)) > 0)

#Renaming demographic variables
t1$Sex = t1$Sex %>% sapply(function(x) revalue(x,c( "1" = "0", "2"="1"))) %>% sapply(function(x) as.numeric(as.character(x)))
t1$Ethnicity = t1$Ethnicity %>% sapply(function(x) revalue(x,c( "1" = "0", "2"="1"))) %>%sapply(function(x) as.numeric(as.character(x)))
t1['Anxiety.Profiles'] = t1['Anxiety.Profiles'] %>% sapply(function(x) revalue(x,c( "Low Anxiety" = "0", "High Math Anxiety"="1"))) %>%sapply(function(x) as.numeric(as.character(x)))
t1$Income= t1$Income %>% sapply(function(x) revalue(x,c('1' = '0', '2' = '1', '3' = '2', '4'='3', '5'='4', '6'='5'))) %>% sapply(function(x) as.numeric(as.character(x)))
t1$Years= t1$Years %>% sapply(function(x) revalue(x,c('10' = '0', '20' = '1', '30' = '2', '40'='3'))) %>% sapply(function(x) as.numeric(as.character(x)))

#Using lavaan to create regression equations
library(lavaan)
mod = '
pkdanvan ~Anxiety.Profiles + Sex + Years + Income + Age + Ethnicity  + GPA + BK


pkdanvan~~pkdanvan

pkdanvan~1

'
model = lavaan(mod, data = t1)
summary(model, fit.measures = T)

mod = '

pkdandef ~Anxiety.Profiles + Sex + Years + Income + Age + Ethnicity  + GPA + BK


pkdandef~~pkdandef

pkdandef~1

'
model = lavaan(mod, data = t1)
summary(model, fit.measures = T)


mod = '

pkvandef ~Anxiety.Profiles + Sex + Years + Income + Age + Ethnicity  + GPA + BK


pkvandef~~pkvandef

pkvandef~1

'

model = lavaan(mod, data = t1)
summary(model, fit.measures = T)

summary(lm(danvanans~Anxiety.Profiles+ Sex + Years + Income + Age + Ethnicity  + GPA + BK  , data = t1))

mod = '

danvanscene ~Anxiety.Profiles+ Sex + Years + Income + Age + Ethnicity  + GPA + BK
danvanq~Anxiety.Profiles+ Sex + Years + Income + Age + Ethnicity  + GPA + BK
danvanans~Anxiety.Profiles + Sex + Years + Income + Age + Ethnicity  + GPA + BK

danvanscene~~danvanscene
danvanq~~danvanq
danvanans~~danvanans

danvanscene~~danvanq + danvanans
danvanq~~danvanans

danvanscene~1
danvanq~1
danvanans~1
'

model = lavaan(mod, data = t1)
summary(model, fit.measures = T)


mod = '

dandefscene ~Anxiety.Profiles + Sex + Years + Income + Age + Ethnicity  + GPA + BK
dandefq~Anxiety.Profiles + Sex + Years + Income + Age + Ethnicity  + GPA + BK
dandefans~Anxiety.Profiles + Sex + Years + Income + Age + Ethnicity  + GPA + BK

dandefscene~~dandefscene
dandefq~~dandefq
dandefans~~dandefans

dandefscene~~dandefq + dandefans
dandefq~~dandefans

dandefscene~1
dandefq~1
dandefans~1
'
model = lavaan(mod, data = t1)
summary(model, fit.measures = T)


mod = '

dandefscene ~1
dandefq~1
dandefans~1

dandefscene~~dandefscene
dandefq~~dandefq
dandefans~~dandefans

dandefscene~~dandefq + dandefans
dandefq~~dandefans


'
model = lavaan(mod, data = t1)
summary(model, fit.measures = T)




mod = '

vandefscene ~Anxiety.Profiles + Sex + Years + Income + Age + Ethnicity  + GPA + BK
vandefq~Anxiety.Profiles + Sex + Years + Income + Age + Ethnicity  + GPA +  BK
vandefans~Anxiety.Profiles+ Sex + Years + Income + Age + Ethnicity  + GPA +  BK

vandefscene~~vandefscene
vandefq~~vandefq
vandefans~~vandefans

vandefscene~~vandefq + vandefans
vandefq~~vandefans

vandefscene~1
vandefq~1
vandefans~1
'
model = lavaan(mod, data = t1)
summary(model, fit.measures = T)

#Importing and manipulating data for within network connectivity

library(stringr)
#DAN within network connectivity for the FCI task
node = read.table(file = '~/Documents/MastersProject/fci_dan-van_node-connectivity.tsv', sep = '\t', header = TRUE)
#These numbers corresponds to Dan nodes: 109-136=28; 313-335; 23 = 51; 2601 -51 = 2550
#deletes every row that is not Session = 1
dan = node[(node$X=='1'),]
#Need row with labels
dan=rbind(node[1,], dan[2:nrow(dan),])
danb = dan[-c(1:3)]
#Current format is of the are numbers that correspond with regions.
#Currently, every column name has an X in front of the number.
#The next code removes the X and allows the column names to be treated as numbers.
#I can select colnames from certain regions.
danb = danb %>% rename_all(~stringr::str_replace(., regex("^x", ignore_case = TRUE), ""))
left = which(as.numeric(colnames(danb))>= 109 & as.numeric(colnames(danb))<137)
right =  which(as.numeric(colnames(danb))>= 313 & as.numeric(colnames(danb))<336)
combined = c(left, right)  
danb = danb[,combined]
#First row corresponds to region labels. This code selects all labels related to dan.
left = which(danb[1,]>= 109 & danb[1,]<137)
right =  which(danb[1,]>= 313 & danb[1,]<336)
combined = c(left, right)  
danb = danb[,combined]  
danb2 = danb[-c(1),]
del = colMeans(danb2, na.rm = T)
del2 = which(del==1)
danb2 = danb2[,-c(del2)]
dancheck = danb2[!duplicated(as.list(danb2))]
withindan = rowMeans(danb2)
danb[c(2:nrow(danb)),2] - danb2[1]


#These numbers corresponds to Dan nodes: 109-136=28; 313-335; 23 = 51; 2601 -51 = 2550
#DAN within network connectivity for the PK task

nodepk = read.table(file = '~/Documents/MastersProject/reas_dan-van_node-connectivity.tsv', sep = '\t', header = TRUE)
danpk = nodepk[(nodepk$X=='1'),]
danpk=rbind(nodepk[1,], danpk[2:nrow(danpk),])
danpkb = danpk[-c(1:3)]
danpkb = danpkb %>% rename_all(~ stringr::str_replace(., regex("^x", ignore_case = TRUE), ""))
left = which(as.numeric(colnames(danpkb))>= 109 & as.numeric(colnames(danpkb))<137)
right =  which(as.numeric(colnames(danpkb))>= 313 & as.numeric(colnames(danpkb))<336)
combined = c(left, right)  
danpkb = danpkb[,combined]
left = which(danpkb[1,]>= 109 & danpkb[1,]<137)
right =  which(danpkb[1,]>= 313 & danpkb[1,]<336)
combined = c(left, right)  
danpkb = danpkb[,combined]  
danpkb2 = danpkb[-c(1),]
del = colMeans(danpkb2, na.rm = T)
del2 = which(del==1)
danpkb2 = danpkb2[,-c(del2)]
danpkcheck = danpkb2[!duplicated(as.list(danpkb2))]
withindanpk = rowMeans(danpkb2)
danpkb[c(2:nrow(danpkb)),2] - danpkb2[1]


#These numbers corresponds to VAN nodes: 85-108= 24; 285-312 = 28 = 2704 - 52 = 2652
#VAN within network connectivity for tthe FCI task

van = node[(node$X=='1'),]
van=rbind(node[1,], van[2:nrow(van),])
vanb = van[-c(1:3)]
vanb = vanb %>% rename_all(~ stringr::str_replace(., regex("^x", ignore_case = TRUE), ""))
left = which(as.numeric(colnames(vanb))>= 85 & as.numeric(colnames(vanb))<109)
right =  which(as.numeric(colnames(vanb))>= 285 & as.numeric(colnames(vanb))<313)
combined = c(left, right)  
vanb = vanb[,combined]
left = which(vanb[1,]>= 85 & vanb[1,]<109)
right =  which(vanb[1,]>= 285 &vanb[1,]<313)
combined = c(left, right)  
vanb = vanb[,combined]
vanb2 = vanb[-c(1),]
del = colMeans(vanb2, na.rm = T)
del2 = which(del==1)
vanb2 = vanb2[,-c(del2)]
vancheck = vanb2[!duplicated(as.list(vanb2))]
withinvan = rowMeans(vanb2) 
vanb[c(2:nrow(vanb)),2] - vanb2[1]

#These numbers corresponds to VAN nodes: 85-108= 24; 285-312 = 28 = 2704 - 52 = 2652
# VAN Within network connectivity for the PK task
vanpk = nodepk[(nodepk$X=='1'),]
vanpk=rbind(nodepk[1,], vanpk[2:nrow(vanpk),])
vanpkb = vanpk[-c(1:3)]
vanpkb = vanpkb %>% rename_all(~ stringr::str_replace(., regex("^x", ignore_case = TRUE), ""))
left = which(vanpkb[1,]>= 85 & vanpkb[1,]<109)
right =  which(vanpkb[1,]>= 285 & vanpkb[1,]<313)
combined = c(left, right)  
vanpkb = vanpkb[,combined]
left = which(as.numeric(colnames(vanpkb))>= 85 & as.numeric(colnames(vanpkb))<109)
right = which(as.numeric(colnames(vanpkb))>= 285 & as.numeric(colnames(vanpkb))<313)
combined = c(left, right)  
vanpkb = vanpkb[,combined]
vanpkb2 = vanpkb[-c(1),]
del = colMeans(vanpkb2, na.rm = T)
del2 = which(del==1)
vanpkb2 = vanpkb2[,-c(del2)]
vanpkcheck = vanpkb2[!duplicated(as.list(vanpkb2))]
withinvanpk = rowMeans(vanpkb2) 
vanpkb[c(2:nrow(vanpkb)),2] - vanpkb2[1]



#These numbers corresponds to DMN nodes: 1-41 = 41; 201-236 = 36; 77 = 5929 - 77 = 5852
#DMN within network connectivity for the FCI task
node2 = read.table(file = '~/Documents/MastersProject/fci_dan-def_node-connectivity.tsv', sep = '\t', header = TRUE)
default = node2[(node2$X=='1'),]
default=rbind(node2[1,], default[2:nrow(default),])
defaultb = default[-c(1:3)]
defaultb = defaultb %>% rename_all(~ stringr::str_replace(., regex("^x", ignore_case = TRUE), ""))
left = which(as.numeric(colnames(defaultb))> 1 & as.numeric(colnames(defaultb))<42)
right =  which(as.numeric(colnames(defaultb))>= 201 & as.numeric(colnames(defaultb))<237)
combined = c(left, right)  
defaultb = defaultb[,combined]
left = which(defaultb[1,]>= 1 & defaultb[1,]<42)
right =  which(defaultb[1,]>= 201 & defaultb[1,]<237)
combined = c(left, right)  
defaultb = defaultb[,combined]  
defaultb2 = defaultb[-c(1),]
del = colMeans(defaultb2, na.rm = T)
del2 = which(del==1)
defaultb2 = defaultb2[,-c(del2)]
defaultcheck = defaultb2[!duplicated(as.list(defaultb2))]
withindef = rowMeans(defaultb2)
defaultb[c(2:nrow(defaultb)),2] - defaultb2[1]


#These numbers corresponds to DMN nodes: 1-41 = 41; 201-236 = 36; 77 = 5929 - 77 = 5852
#DMN within network connectivity for the PK task

nodepk2 = read.table(file = '~/Documents/MastersProject/reas_dan-def_node-connectivity.tsv', sep = '\t', header = TRUE)
defaultpk = nodepk2[(nodepk2$X=='1'),]
defaultpk=rbind(nodepk2[1,], defaultpk[2:nrow(defaultpk),])
defaultpkb = defaultpk[-c(1:3)]
defaultpkb = defaultpkb %>% rename_all(~ stringr::str_replace(., regex("^x", ignore_case = TRUE), ""))
left = which(defaultpkb[1,]>= 1 & defaultpkb[1,]<42)
right =  which(defaultpkb[1,]>= 201 & defaultpkb[1,]<237)
combined = c(left, right)  
defaultpkb = defaultpkb[,combined] 
left = which(as.numeric(colnames(defaultpkb))> 1 & as.numeric(colnames(defaultpkb))<42)
right =  which(as.numeric(colnames(defaultpkb))>= 201 & as.numeric(colnames(defaultpkb))<237)
combined = c(left, right)  
defaultpkb = defaultpkb[,combined]
defaultpkb2 = defaultpkb[-c(1),]
del = colMeans(defaultpkb2, na.rm = T)
del2 = which(del==1)
defaultpkb2 = defaultpkb2[,-c(del2)]
defaultpkcheck = defaultpkb2[!duplicated(as.list(defaultpkb2))]
withindefpk = rowMeans(defaultpkb2)
defaultpkb[c(2:nrow(defaultpkb)),2] - defaultpkb2[1]

withindan = c(NA, withindan)
withinvan= c(NA, withinvan)
withindef = c(NA, withindef)
withindanpk = c(NA, withindanpk)
withinvanpk= c(NA, withinvanpk)
withindefpk= c(NA, withindefpk)
dan$withindan = withindan
van$withinvan = withinvan
danpk$withindanpk = withindanpk
vanpk$withinvanpk = withinvanpk
default$withindef = withindef
defaultpk$withindefpk = withindefpk

withindanscene = dan[(dan$X.1=='physicsXscenario'),]
withindanq = dan[(dan$X.1=='physicsXquestion'),]
withindanans = dan[(dan$X.1=='physicsXanswers'),]

withinvanscene = van[(van$X.1=='physicsXscenario'),]
withinvanq = van[(van$X.1=='physicsXquestion'),]
withinvanans = van[(van$X.1=='physicsXanswers'),]

withinvanpk = vanpk[(vanpk$X.1=='Reasoning'),]
withindanpk = danpk[(danpk$X.1=='Reasoning'),]

withindefscene = default[(default$X.1=='physicsXscenario'),]
withindefq = default[(default$X.1=='physicsXquestion'),]
withindefans = default[(default$X.1=='physicsXanswers'),]

withindefpk = defaultpk[(defaultpk$X.1=='Reasoning'),]

#Creating single dataframe for the within-network connectivity measures
within = data.frame(withindanscene[1],withindanscene[c('withindan')], withindanq['withindan'], withindanans['withindan'], withindanpk['withindanpk'],withinvanscene['withinvan'], withinvanq['withinvan'], withinvanans['withinvan'], withinvanpk['withinvanpk'], 
                    withindefscene['withindef'], withindefq['withindef'], withindefans['withindef'], withindefpk['withindefpk'])


colnames(within) = c('ID', 'withindanscene', 'withindanq', 'withindanans', 'withindanpk','withinvanscene', 'withinvanq', 'withinvanans', 'withinvanpk',
                    'withindefscene', 'withindefq', 'withindefans', 'withindefpk')

rownames(within) = within$ID
matchnames = rownames(ndata)
x = ndata[!(rownames(ndata) %in% rownames(within)),]
temp = matrix(nrow = nrow(x), ncol = ncol(within))
temp = data.frame(temp)
rownames(temp) = rownames(x)
temp$X1 = as.numeric(rownames(x))
colnames(temp) = colnames(within)
within = within[(rownames(within) %in% matchnames),]
within = rbind(within, temp)
within=within[rownames(ndata),]
within = data.frame(within, t1)
as.numeric(rownames(ndata)) - as.numeric(rownames(within))
as.numeric(rownames(t1)) - as.numeric(rownames(within))
rownames(withindanans) = withindanans$label
rownames(withinvanans) = withinvanans$label
as.numeric(rownames(withindanans)) - as.numeric(rownames(withinvanans))

#Using lavaan for regression
library(lavaan)

mod = '

withinvanscene ~Anxiety.Profiles + Sex + Years + Income + Age + Ethnicity  + GPA + BK
withinvanq~Anxiety.Profiles + Sex + Years + Income + Age + Ethnicity  + GPA + BK
withinvanans~Anxiety.Profiles + Sex + Years + Income + Age + Ethnicity  + GPA + BK

withinvanscene~~withinvanscene
withinvanq~~withinvanq
withinvanans~~withinvanans

withinvanscene~~withinvanq + withinvanans
withinvanq~~withinvanans

withinvanscene~1
withinvanq~1
withinvanans~1
'
#model = lavaan(mod, data = within, se = 'bootstrap', bootstrap = 1000)

model = lavaan(mod, data = within)
summary(model, fit.measures = T)


mod = '

withindanscene ~Anxiety.Profiles + Sex + Years + Income + Age + Ethnicity  + GPA + BK
withindanq~Anxiety.Profiles + Sex + Years + Income + Age + Ethnicity  + GPA + BK
withindanans~Anxiety.Profiles+ Sex + Years + Income + Age + Ethnicity  + GPA + BK

withindanscene~~withindanscene
withindanq~~withindanq
withindanans~~withindanans

withindanscene~~withindanq + withindanans
withindanq~~withindanans

withindanscene~1
withindanq~1
withindanans~1
'
model = lavaan(mod, data = within)
summary(model, fit.measures = T)


mod = '

withindefscene ~Anxiety.Profiles + Sex + Years + Income + Age + Ethnicity  + GPA + BK
withindefq~Anxiety.Profiles + Sex + Years + Income + Age + Ethnicity  + GPA + BK
withindefans~Anxiety.Profiles+ Sex + Years + Income + Age + Ethnicity  + GPA + BK

withindefscene~~withindefscene
withindefq~~withindefq
withindefans~~withindefans

withindefscene~~withindefq +withindefans
withindefq~~withindefans

withindefscene~1
withindefq~1
withindefans~1
'

model = lavaan(mod, data = within)
summary(model, fit.measures = T)

mod = '

withindefscene ~Anxiety.Profiles + Sex + Years + Income + Age + Ethnicity  + GPA + BK
withindefq~Anxiety.Profiles + Sex + Years + Income + Age + Ethnicity  + GPA + BK
withindefans~Anxiety.Profiles+ Sex + Years + Income + Age + Ethnicity  + GPA + BK

withindefscene~~withindefscene
withindefq~~withindefq
withindefans~~withindefans

withindefscene~1
withindefq~1
withindefans~1
'

model= lavaan(mod, data = within)
summary(model, fit.measures = T)



mod = '

withinvanpk ~Anxiety.Profiles + Sex + Years + Income + Age + Ethnicity  + GPA + BK

withinvanpk~~withinvanpk


withinvanpk~1

'

model = lavaan(mod, data = within)
summary(model, fit.measures = T)

mod = '

withindefpk ~Anxiety.Profiles + Sex + Years + Income + Age + Ethnicity  + GPA + BK

withindefpk~~withindefpk


withindefpk~1

'

model = lavaan(mod, data = within)
summary(model, fit.measures = T)


mod = '

withindanpk ~Anxiety.Profiles + Sex + Years + Income + Age + Ethnicity  + GPA + BK

withindanpk~~withindanpk


withindanpk~1

'

model = lavaan(mod, data = within)
summary(model, fit.measures = T)

#Testing demographic differences

# No difference for income 
a = tabulate(factor(g3dem$Income))
b = tabulate(factor(g2dem$Income))
c = rbind(a,b)
chisq.test(c,simulate.p.value=TRUE, B=1e6)

a = tabulate(factor(g3dem$Sex))
b = tabulate(factor(g2dem$Sex))
c = rbind(a,b)
chisq.test(c,simulate.p.value=TRUE , B=1e6)

t1 = na.omit(g3dem$Years)
t2 = na.omit(g2dem$Years)
a = tabulate(factor(t1))
b = tabulate(factor(t2))
c = rbind(a,b)
chisq.test(c,simulate.p.value=TRUE , B=1e6)

t.test(g3dem$Age,g2dem$Age)

table(is.na(g3dem$BK))
table(is.na(g2dem$BK))

table(is.na(g3dem$GPA))
table(is.na(g2dem$GPA))

t1 = na.omit(g3dem$GPA)
t2 = na.omit(g2dem$GPA)

t.test(g3dem$GPA,na.omit(g2dem$GPA))
t.test(t1,t2)

pkacc = read.delim("~/Documents/MastersProject/reas_accuracy_by_gender_pre.txt", header = T, sep = "\t", dec = ".")
rownames(pkacc) =pkacc$Subject
x = ndata[!(rownames(ndata) %in% rownames(pkacc)),]
temp = matrix(nrow = nrow(x), ncol = ncol(pkacc))
temp = data.frame(temp)
rownames(temp) = rownames(x)
temp$X1 = as.numeric(rownames(temp))
colnames(temp) = colnames(pkacc)
pkacc = rbind(pkacc, temp)
pkacc = pkacc[(rownames(pkacc) %in% matchnames),]
pkacc = pkacc[rownames(ndata),]
which(as.numeric(rownames(pkacc)) - as.numeric(rownames(ndata)) > 0 )



fciacc = read.delim(file = '~/Documents/MastersProject/fci_accuracy_rt.txt', header = TRUE, sep = "\t", dec = ".")
fciacc = fciacc[c(fciacc$Session=='session-0'),]
rownames(fciacc) = fciacc$Subject
x = ndata[!(rownames(ndata) %in% rownames(fciacc)),]
temp = matrix(nrow = nrow(x), ncol = ncol(fciacc))
temp = data.frame(temp)
rownames(temp) = rownames(x)
temp$X1 = c(105, 576, 501)
colnames(temp) = colnames(fciacc)
fciacc = rbind(fciacc, temp)
fciacc = fciacc[(rownames(fciacc) %in% matchnames),]
fciacc = fciacc[rownames(ndata),]

which(as.numeric(rownames(fciacc)) - as.numeric(rownames(ndata)) > 0)

fciacc$groups = ndata$Anxiety.Profiles
g1 = fciacc[fciacc$groups == 'Low Anxiety',]
g2 = fciacc[fciacc$groups == 'High Math Anxiety',]

t.test(g1$Mean.FCI.Accuracy,g2$Mean.FCI.Accuracy)

g1 = na.omit(g1)
g2 = na.omit(g2)
t.test(g1$Mean.FCI.Accuracy,g2$Mean.FCI.Accuracy)


pkacc$groups = ndata$Anxiety.Profiles
g1 = pkacc[pkacc$groups == 'Low Anxiety',]
g2 = pkacc[pkacc$groups == 'High Math Anxiety',]
t.test(g1$MeanReasoningAccuracy,g2$MeanReasoningAccuracy)

g1 = na.omit(g1)
g2 = na.omit(g2)
t.test(g1$MeanReasoningAccuracy,g2$MeanReasoningAccuracy)



