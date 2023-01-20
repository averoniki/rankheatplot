# Example: Alzheimer's Dementia Safety Data
# Install and load R library netmeta.
# install.packages("netmeta")

library(readxl)
library(netmeta)

## for rank-heat plot function
library(readxl)
library(fields)
library(RColorBrewer)
library(circlize)
###

setwd("/Users/argie/Library/CloudStorage/OneDrive-UniversityofToronto/Sharon/Rank-heat plot extension")

## Import the dataset
# Outomes 1:7 (it should be 7 circles)
# Max 10 treatments (it should be 10 radii)
Falls <- read_excel("Safety AD Data RCT.xlsx","Falls")
Nausea <- read_excel("Safety AD Data RCT.xlsx","Nausea")
Bradycardia <- read_excel("Safety AD Data RCT.xlsx","Bradycardia")
Diarrhea <- read_excel("Safety AD Data RCT.xlsx","Diarrhea")
Headache <- read_excel("Safety AD Data RCT.xlsx","Headache")
SAE <- read_excel("Safety AD Data RCT.xlsx","SAE")
Vomiting <- read_excel("Safety AD Data RCT.xlsx","Vomiting")

View(Vomiting)

# transform arm to contrast level data to run the netmeta R function
# In this example we have binary data and select the OR as a treatment effect 
# this information will be provided by the user (i.e., arm-level/contrast-level [e.g., r,n vs TE,seTE],type of data [e.g., continuous vs binary], effect size [e.g., OR vs SMD])
Falls.pair <- pairwise(treat = trt, event = r, n = n, studlab = study, data=Falls, sm="OR")
Falls.pair

Nausea.pair <- pairwise(treat = trt, event = r, n = n, studlab = study, data=Nausea, sm="OR")
Bradycardia.pair <- pairwise(treat = trt, event = r, n = n, studlab = study, data=Bradycardia, sm="OR")
Diarrhea.pair <- pairwise(treat = trt, event = r, n = n, studlab = study, data=Diarrhea, sm="OR")
Headache.pair <- pairwise(treat = trt, event = r, n = n, studlab = study, data=Headache, sm="OR")
SAE.pair <- pairwise(treat = trt, event = r, n = n, studlab = study, data=SAE, sm="OR")
Vomiting.pair <- pairwise(treat = trt, event = r, n = n, studlab = study, data=Vomiting, sm="OR")

# Run the NMA model
# should ask the user to provide the reference treatment "ref"
# also should ask which model to use random or common across outcomes to present in the rank-heat plot
# here I chose random=T and common=F for all outcomes
# in the case of random=T, then method.tau should be chosen by the user (REML should be the default)
res.Falls <- netmeta(Falls.pair, sm="OR", ref="PLAC", common = FALSE, random= TRUE, method.tau = "REML")

res.Nausea <- netmeta(Nausea.pair, sm="OR", ref="PLAC", common = FALSE, random= TRUE, method.tau = "REML")
res.Bradycardia <- netmeta(Bradycardia.pair, sm="OR", ref="PLAC", common = FALSE, random= TRUE, method.tau = "REML")
res.Diarrhea <- netmeta(Diarrhea.pair, sm="OR", ref="PLAC", common = FALSE, random= TRUE, method.tau = "REML")
res.Headache <- netmeta(Headache.pair, sm="OR", ref="PLAC", common = FALSE, random= TRUE, method.tau = "REML")
res.SAE <- netmeta(SAE.pair, sm="OR", ref="PLAC", common = FALSE, random= TRUE, method.tau = "REML")
res.Vomiting <- netmeta(Vomiting.pair, sm="OR", ref="PLAC", common = FALSE, random= TRUE, method.tau = "REML")

# You can create a network plot by typing
#netgraph(res.Falls)

# The network should be connected - get more information about the connectivity of the network
# if the number of subnetworks >1 then the user should update the data 
netconnection(treat1, treat2, studlab, data = res.Falls)

netconnection(treat1, treat2, studlab, data = res.Nausea)
netconnection(treat1, treat2, studlab, data = res.Bradycardia)
netconnection(treat1, treat2, studlab, data = res.Diarrhea)
netconnection(treat1, treat2, studlab, data = res.Headache)
netconnection(treat1, treat2, studlab, data = res.SAE)
netconnection(treat1, treat2, studlab, data = res.Vomiting)


# Get the P-scores/SUCRAs for the rank-heat plot
# ask the user whether P-score or SUCRA should be presented (e.g., method = "SUCRA", nsim = 100))
# should ask the user to clarify if small outcome values are good/bad
Pscores.Falls<-netrank(res.Falls,method = "P-score",small.values="good", common = FALSE, random= TRUE)

Pscores.Nausea<-netrank(res.Nausea,method = "P-score", small.values="good", common = FALSE, random= TRUE)
Pscores.Bradycardia<-netrank(res.Bradycardia,method = "P-score",small.values="good", common = FALSE, random= TRUE)
Pscores.Diarrhea<-netrank(res.Diarrhea,method = "P-score",small.values="good", common = FALSE, random= TRUE)
Pscores.Headache<-netrank(res.Headache,method = "P-score",small.values="good", common = FALSE, random= TRUE)
Pscores.SAE<-netrank(res.SAE,method = "P-score",small.values="good", common = FALSE, random= TRUE)
Pscores.Vomiting<-netrank(res.Vomiting,method = "P-score",small.values="good", common = FALSE, random= TRUE)

# Use R package writexl to save league tables
library(writexl)

# note I chose Pscore.random for the random effects model
# choose Pscore.fixed for the common effect model
#rankheat.names<-unique(names(c(Pscores.Falls$Pscore.random,Pscores.Nausea$Pscore.random,Pscores.Bradycardia$Pscore.random,Pscores.Diarrhea$Pscore.random,Pscores.Headache$Pscore.random,Pscores.SAE$Pscore.random,Pscores.Vomiting$Pscore.random)))
#test<-do.call(rbind.data.frame, list(Pscores.Falls$Pscore.random,Pscores.Nausea$Pscore.random,Pscores.Bradycardia$Pscore.random,Pscores.Diarrhea$Pscore.random,Pscores.Headache$Pscore.random,Pscores.SAE$Pscore.random,Pscores.Vomiting$Pscore.random))
#colnames(test)<-rankheat.names
#rownames(test)<-c("Falls","Nausea","Bradycardia","Diarrhea","Headache","SAE","Vomiting")
n.out<-7
n.trt<-9
# create a dataframe for each outcome (maybe create a "for" loop across outcomes?)
rank.Falls<-data.frame(Pscores.Falls$Pscore.random)
fall.names<-row.names(rank.Falls)
rank.Falls<-data.frame(matrix(Pscores.Falls$Pscore.random),fall.names)
names(rank.Falls)<-c("Falls","Treatment")

rank.Nausea<-data.frame(Pscores.Nausea$Pscore.random)
nausea.names<-row.names(rank.Nausea)
rank.Nausea<-data.frame(matrix(Pscores.Nausea$Pscore.random),nausea.names)
names(rank.Nausea)<-c("Nausea","Treatment")

rank.Bradycardia<-data.frame(Pscores.Bradycardia$Pscore.random)
bradycardia.names<-row.names(rank.Bradycardia)
rank.Bradycardia<-data.frame(matrix(Pscores.Bradycardia$Pscore.random),bradycardia.names)
names(rank.Bradycardia)<-c("Bradycardia","Treatment")

rank.Diarrhea<-data.frame(Pscores.Diarrhea$Pscore.random)
diarrhea.names<-row.names(rank.Diarrhea)
rank.Diarrhea<-data.frame(matrix(Pscores.Diarrhea$Pscore.random),diarrhea.names)
names(rank.Diarrhea)<-c("Diarrhea","Treatment")

rank.Headache<-data.frame(Pscores.Headache$Pscore.random)
headache.names<-row.names(rank.Headache)
rank.Headache<-data.frame(matrix(Pscores.Headache$Pscore.random),headache.names)
names(rank.Headache)<-c("Headache","Treatment")

rank.SAE<-data.frame(Pscores.SAE$Pscore.random)
sae.names<-row.names(rank.SAE)
rank.SAE<-data.frame(matrix(Pscores.SAE$Pscore.random),sae.names)
names(rank.SAE)<-c("SAE","Treatment")

rank.Vomiting<-data.frame(Pscores.Vomiting$Pscore.random)
vomiting.names<-row.names(rank.Vomiting)
rank.Vomiting<-data.frame(matrix(Pscores.Vomiting$Pscore.random),vomiting.names)
names(rank.Vomiting)<-c("Vomiting","Treatment")

# merge all data for rankheat plot data
rankheat.data<-Reduce(function(x,y) merge(x,y, by="Treatment", all=TRUE),
                      list(rank.Falls, rank.Nausea, rank.Bradycardia, rank.Diarrhea, rank.Headache, rank.SAE, rank.Vomiting))

# League table from random effects model
write_xlsx(rankheat.data, path = "Safety AD Pscores.xlsx")


# Note: in the rank-heat plot it would be good to have the option to provide a certain order of the treatments and outcomes according to the user's preference

# Run the rank-heat plot function with the data above
rankheatplot(rankheat.data, format="percentage")
  
############################## STOP HERE ##################################
### AAV: To prepare similar rscript for continuous, and contrast -level data
# Example data Senn 2013
data(Senn2013)

# header of data
head(Senn2013)

# NMA model can run directly for contrast-level data as shown below
res2 <- netmeta(TE, seTE, treat1, treat2, studlab, data = Senn2013)


