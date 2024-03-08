#This script that I made includes the LPA and regression analysis for between- & within-network dan, van, and dmn connectivity.

pacman::p_load(plyr,dplyr,lavaan, tidyLPA, mclust)
#Importing spss dataset
data = Hmisc::spss.get("~/NeuralMechanisms_SharedData.sav",use.value.labels = F)
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

#Importing csv file containing enrollment years.
behave = read.csv("~/BehavioralData.csv");rownames(behave) = behave$Enrollment.ID
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

#Compare solutions of the 1,2,3, and 4 profile FCI model to select the best one.

project %>% select(SA, MA) %>% estimate_profiles(1:4) %>% compare_solutions()


#Graphing results.
project %>% select(SA, MA) %>% estimate_profiles(4) %>% plot_profiles( sd = F,ci = 0.95)

tab = project%>% select(SA, MA)  %>% estimate_profiles(4) %>% get_data()

tab = tab[5:8]

#Filtering Individuals w/ a cprob > 0.7. There are four groups in total.

sum(tab[,1]>=0.7) 
sum(tab[,2]>=0.7) 
sum(tab[,3]>=0.7) 
sum(tab[,4]>=0.7) 

g1 = which(tab[,1]>= 0.7)
g2 = which(tab[,2]>= 0.7)
g3 = which(tab[,3]>=0.7)
g4 = which(tab[,4]>=0.7)

#Turning demographic data into factors to rename later in the script.
project[c(2,4:5,7)]  = project[c(2,4:5,7)] %>% sapply(function(x) as.factor(x))

#Determining which individuals/rows are in which group for the Low STEM Anxiety group and the High Math Anxiety.
#Including demographic variables for grouped participants
Low_STEM_Anxiety_dem = project[g2, 2:10]
High_Math_Anxiety_dem = project[g3, 2:10]
Low_STEM_Anxiety_dem['Anxiety.Profiles'] = rep('Low STEM Anxiety', sum(tab[,2]>0.7)) # CI low for both
High_Math_Anxiety_dem['Anxiety.Profiles'] = rep('High Math Anxiety',sum(tab[,3]>0.7)) # Confidence intervals for math greater than zero, touches for science

#Generating one data table for all participants in the Low STEM anxiety and High Math Anxiety groups.
ndata = rbind(Low_STEM_Anxiety_dem,High_Math_Anxiety_dem)

#Importing tsv files containing the connectivity information.
fcidanvan = read.table(file = '~/fci_dan-van_network-connectivity.tsv', sep = '\t', header = TRUE)
fcidandef =  read.table(file = '~/fci_dan-def_network-connectivity.tsv', sep = '\t', header = TRUE)
fcivandef =  read.table(file = '~/fci_van-def_network-connectivity.tsv', sep = '\t', header = TRUE)

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

fcivandmnscene = fcivandmn[(fcivandmn$X.2=='physicsXscenario'),]
colnames(fcivandmnscene) = c('ID', 'Session', 'Condition','vandmnscene')
fcivandmnq = fcivandmn[(fcivandmn$X.2=='physicsXquestion'),]
colnames(fcivandmnq) = c('ID', 'Session', 'Condition','vandmnq')
fcivandmnans = fcivandmn[(fcivandmn$X.2=='physicsXanswers'),]
colnames(fcivandmnans) = c('ID', 'Session', 'Condition','vandmnans')


#Creating one dataframe containing all connectivity information
between_network= data.frame(fcidandmnscene[c(1,2,4)],fcidandmnq[4], fcidandmnans[4], fcidanvanscene[4], fcidanvanq[4], fcidanvanans[4], fcivandmnscene[4], fcivandmnq[4], fcivandmnans[4])
rownames(between_network) = between_network$ID

#Ensuring the ID's for the connectivity data match the ID's for the dataframe containing the demographic information
rownames(between_network) = between_network$ID
matchnames = rownames(ndata)
x = ndata[!(rownames(ndata) %in% rownames(between_network)),]
temp = matrix(nrow = nrow(x), ncol = ncol(between_network));temp = data.frame(temp);rownames(temp) = rownames(x);temp$X1 = c(rownames(x))
colnames(temp) = colnames(between_network)
between_network = rbind(between_network, temp);between_network = between_network[rownames(ndata),];between_network = data.frame(between_network, ndata)
which(as.numeric(rownames(ndata)) - as.numeric(rownames(between_network)) > 0)

#Renaming demographic variables
between_network$Sex = between_network$Sex %>% sapply(function(x) revalue(x,c( "1" = "0", "2"="1"))) %>% sapply(function(x) as.numeric(as.character(x)))
between_network$Ethnicity = between_network$Ethnicity %>% sapply(function(x) revalue(x,c( "1" = "0", "2"="1"))) %>%sapply(function(x) as.numeric(as.character(x)))
between_network['Anxiety.Profiles'] = between_network['Anxiety.Profiles'] %>% sapply(function(x) revalue(x,c( "Low Anxiety" = "0", "High Math Anxiety"="1"))) %>%sapply(function(x) as.numeric(as.character(x)))
between_network$Income= between_network$Income %>% sapply(function(x) revalue(x,c('1' = '0', '2' = '1', '3' = '2', '4'='3', '5'='4', '6'='5'))) %>% sapply(function(x) as.numeric(as.character(x)))
between_network$Years= between_network$Years %>% sapply(function(x) revalue(x,c('10' = '0', '20' = '1', '30' = '2', '40'='3'))) %>% sapply(function(x) as.numeric(as.character(x)))

#Using lavaan to create regression equations
library(lavaan)

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

model = lavaan(mod, data = between_network)
summary(model, fit.measures = T)


mod = '

dandmnscene ~Anxiety.Profiles + Sex + Years + Income + Age + Ethnicity  + GPA + BK
dandmnq~Anxiety.Profiles + Sex + Years + Income + Age + Ethnicity  + GPA + BK
dandmnans~Anxiety.Profiles + Sex + Years + Income + Age + Ethnicity  + GPA + BK

dandmnscene~~dandmnscene
dandmnq~~dandmnq
dandmnans~~dandmnans

dandmnscene~~dandmnq + dandmnans
dandmnq~~dandmnans

dandmnscene~1
dandmnq~1
dandmnans~1
'
model = lavaan(mod, data = between_network)
summary(model, fit.measures = T)


mod = '

vandmnscene ~Anxiety.Profiles + Sex + Years + Income + Age + Ethnicity  + GPA + BK
vandmnq~Anxiety.Profiles + Sex + Years + Income + Age + Ethnicity  + GPA +  BK
vandmnans~Anxiety.Profiles+ Sex + Years + Income + Age + Ethnicity  + GPA +  BK

vandmnscene~~vandmnscene
vandmnq~~vandmnq
vandmnans~~vandmnans

vandmnscene~~vandmnq + vandmnans
vandmnq~~vandmnans

vandmnscene~1
vandmnq~1
vandmnans~1
'
model = lavaan(mod, data = between_network)
summary(model, fit.measures = T)

#Importing and manipulating data for within network connectivity

library(stringr)
#DAN within network connectivity for the FCI task
dan_van_nodes = read.table(file = '~/fci_dan-van_node-connectivity.tsv', sep = '\t', header = TRUE)
#These numbers corresponds to Dan nodes: 109-136=28; 313-335; 23 = 51; 2601 -51 = 2550
#deletes every row that is not Session = 1
dan_van_nodes = dan_van_nodes[(dan_van_nodes$X=='1'),]
#Column names are the nodes and the first row states the node that it is connected to  
dan_van_nodes=rbind(dan_van_nodes[1,], dan_van_nodes[2:nrow(dan_van_nodes),])
#Exclude columns 1:3 since they only contain ID and session information.                                                                                                                                         
dan_van_nodes_reduced = dan_van_nodes[-c(1:3)]
#Current format is of the are numbers that correspond with regions.
#Column names are the nodes and the first row states the node that it is connected to.                                                                                                                                          
#Currently, every column name has an X in front of the number.
#The next code removes the X and allows the column names to be treated as numbers.
#I can select colnames from certain regions.
dan_van_nodes_reduced = dan_van_nodes_reduced %>% rename_all(~stringr::str_replace(., regex("^x", ignore_case = TRUE), ""))
#Extracting DAN nodes based on column information                                                                                                                                          
left =  which(as.numeric(colnames(dan_van_nodes_reduced))>= 109 & as.numeric(colnames(dan_van_nodes_reduced))<137)                                                                                                                                          
right =  which(as.numeric(colnames(dan_van_nodes_reduced))>= 313 & as.numeric(colnames(dan_van_nodes_reduced))<336)
combined = c(left, right)  
dan_van_nodes_reduced = dan_van_nodes_reduced[,combined]
#Extracting DAN nodes based on row information    
left = which(dan_van_nodes_reduced[1,]>= 109 & dan_van_nodes_reduced[1,]<137)
right =  which(dan_van_nodes_reduced[1,]>= 313 & dan_van_nodes_reduced[1,]<336)
combined = c(left, right)  
dan_nodes = dan_van_nodes_reduced[,combined]  
#Removes first row containing node information                                                                                                                                           
dan_nodes = dan_nodes[-c(1),]
delete = colMeans(dan_nodes, na.rm = T)
#Remove the columns of nodes that are correlated to themselves. These columns contains 1.                                                                                                                                           
delete = which(delete==1)
dan_nodes = dan_nodes[,-c(delete)]
withindan_fci = rowMeans(dan_nodes)

#These numbers corresponds to VAN nodes: 85-108= 24; 285-312 = 28 = 2704 - 52 = 2652
#VAN within network connectivity for the FCI task
dan_van_nodes_reduced = dan_van_nodes[-c(1:3)]
dan_van_nodes_reduced  = dan_van_nodes_reduced %>% rename_all(~ stringr::str_replace(., regex("^x", ignore_case = TRUE), ""))
left = which(as.numeric(colnames(dan_van_nodes_reduced ))>= 85 & as.numeric(colnames(dan_van_nodes_reduced ))<109)
right =  which(as.numeric(colnames(dan_van_nodes_reduced ))>= 285 & as.numeric(colnames(dan_van_nodes_reduced ))<313)
combined = c(left, right)  
dan_van_nodes_reduced  = dan_van_nodes_reduced[,combined]
left = which(dan_van_nodes_reduced[1,]>= 85 & dan_van_nodes_reduced[1,]<109)
right =  which(dan_van_nodes_reduced[1,]>= 285 &dan_van_nodes_reduced[1,]<313)
combined = c(left, right)  
van_nodes = dan_van_nodes_reduced[,combined]
van_nodes = van_nodes[-c(1),]
delete = colMeans(van_nodes, na.rm = T)
delete = which(delete==1)
van_nodes = van_nodes[,-c(van_nodes)]
withinvan_fci = rowMeans(van_nodes) 

#These numbers corresponds to DMN nodes: 1-41 = 41; 201-236 = 36; 77 = 5929 - 77 = 5852
#DMN within network connectivity for the FCI task
dan_dmn_nodes = read.table(file = '~/fci_dan-dmn_node-connectivity.tsv', sep = '\t', header = TRUE)
dan_dmn_nodes = dan_dmn_nodes[(dan_dmn_nodes$X=='1'),]
dan_dmn_nodes=rbind(dan_dmn_nodes[1,], dan_dmn_nodes[2:nrow(dan_dmn_nodes),])
dan_dmn_nodes_reduced = dan_dmn_nodes[-c(1:3)]
dan_dmn_nodes_reduced  = dan_dmn_nodes_reduced  %>% rename_all(~ stringr::str_replace(., regex("^x", ignore_case = TRUE), ""))
left = which(as.numeric(colnames(dan_dmn_nodes_reduced ))> 1 & as.numeric(colnames(dan_dmn_nodes_reduced ))<42)
right =  which(as.numeric(colnames(dan_dmn_nodes_reduced ))>= 201 & as.numeric(colnames(dan_dmn_nodes_reduced ))<237)
combined = c(left, right)  
dan_dmn_nodes_reduced  = dan_dmn_nodes_reduced[,combined]
left = which(dan_dmn_nodes_reduced[1,]>= 1 & dan_dmn_nodes_reduced[1,]<42)
right =  which(dan_dmn_nodes_reduced[1,]>= 201 & dan_dmn_nodes_reduced[1,]<237)
combined = c(left, right)  
dmn_nodes = dan_dmn_nodes_reduced[,combined]  
dmn_nodes= dmn_nodes[-c(1),]
delete = colMeans(dmn_nodes, na.rm = T)
delete = which(delete==1)
dmn_nodes = dmn_nodes[,-c(delete)]
withindmn_fci = rowMeans(dmn_nodes)


#Adding NA so that the vectors have the same length as the original data frame
withindan_fci = c(NA, withindan_fci)
withinvan_fci = c(NA, withinvan_fci)
withindmn_fci = c(NA, withindmn_fci)
dan_van_nodes$withindan_fci = withindan_fci
dan_van_nodes$withinvan_fci = withinvan_fci
dan_dmn_nodes$withindmn_fci = withindmn_fci

withinscene = dan_van_nodes[(dan_van_nodes$X.1=='physicsXscenario'),]
withinq = dan_van_nodes[(dan_van_nodes$X.1=='physicsXquestion'),]
withinans = dan_van_nodes[(dan_van_nodes$X.1=='physicsXanswers'),]
withindmnscene = dan_dmn_nodes[(dan_dmn_nodes$X.1=='physicsXscenario'),]
withindmnq = dan_dmn_nodes[(dan_dmn_nodes$X.1=='physicsXquestion'),]
withindmnans = dan_dmn_nodes[(dan_dmn_nodes$X.1=='physicsXanswers'),]

#Creating single dataframe for the within-network connectivity measures
within = data.frame(withindanscene[1],withinscene[c('withindan_fci')], withinq['withindan_fci'], withinans['withindan_fci'],withinscene['withinvan_fci'], withinq['withinvan_fci'], withinans['withinvan_fci'],  
                    withindmnscene['withindmn_fci'], withindmnq['withindmn_fci'], withindmnans['withindmn_fci'])

#Changing names of the columns
colnames(within) = c('ID', 'withindanscene', 'withindanq', 'withindanans', 'withinvanscene', 'withinvanq', 'withinvanans',
                    'withindmnscene', 'withindmnq', 'withindmnans')

within = data.frame(within, between)

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

withindmnscene ~Anxiety.Profiles + Sex + Years + Income + Age + Ethnicity  + GPA + BK
withindmnq~Anxiety.Profiles + Sex + Years + Income + Age + Ethnicity  + GPA + BK
withindmnans~Anxiety.Profiles+ Sex + Years + Income + Age + Ethnicity  + GPA + BK

withindmnscene~~withindmnscene
withindmnq~~withindmnq
withindmnans~~withindmnans

withindmnscene~~withindmnq +withindmnans
withindmnq~~withindmnans

withindmnscene~1
withindmnq~1
withindmnans~1
'

model = lavaan(mod, data = within)
summary(model, fit.measures = T)



