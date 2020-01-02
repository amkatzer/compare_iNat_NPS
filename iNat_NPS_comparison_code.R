#Contents:
#1: Working directory & package library
#2: Data import (NPSpecies & iNaturalist)
#3: Direct Matches to NPS
#4: Matches to NPS synonyms
#5: Matches to ITIS
#6: Make dataframe with raw results
#7: Take raw results dataframe and pull out all of the unique entries
#8: Make table 1
#9: Make table 2
#10: Make table 3
#11: Make table 4
#12: Make graph

#1----------------------------------------------------------------------------------------
setwd("C:/Users/amkat/Desktop/VSFS/compare_iNat_NPS/")
library(rinat)
library(taxize)
library(stringr)
library(tibble)
library("rbison")
#devtools::install_github("ropensci/rbison")
library(tidyr)
library(stringr)
library(dplyr)
library(data.table)
#2----------------------------------------------------------------------------------------
#Specify park:
park <- c("LEWI")
options(stringsAsFactors = FALSE)

#First, import data
NPS_data <- read.csv("./NPSpecies_Checklist_LEWI_20190516093957.csv", header=T, skipNul = T, stringsAsFactors=F)
NPS_data2 <- word(NPS_data$Scientific.name, start=1, end=2, sep=" ")

#Get iNaturalist data through an API
#get_inat_obs_project is from the rinat package

iNat_data <- get_inat_obs_project("2016-national-parks-bioblitz-lewis-and-clark", type="observations")
as.character(iNat_data$Scientific.name) #this prints the data?

#Create new data frame with only the RG & iconic taxa
RG_iNat_data <- iNat_data[iNat_data$Quality.grade == "research",]

RG_iNat_data_iconic <- RG_iNat_data[RG_iNat_data$Iconic.taxon.name == "Plantae" | RG_iNat_data$Iconic.taxon.name == "Aves" | RG_iNat_data$Iconic.taxon.name == "Mammalia" | RG_iNat_data$Iconic.taxon.name == "Reptilia" | RG_iNat_data$Iconic.taxon.name == "Amphibia",]

#Chop off subspecies names. Genus only names are assigned NA. Returns a vector, not a dataframe
RG_iNat_data2 <- word(RG_iNat_data_iconic$Scientific.name, start=1, end=2, sep=" ") 

#Make new dataframe with only the RG, non-iconic taxa will be stored. This will be used in table 4.
RG_iNat_not_iconic <- RG_iNat_data[RG_iNat_data$Iconic.taxon.name != "Plantae" & RG_iNat_data$Iconic.taxon.name != "Aves" & RG_iNat_data$Iconic.taxon.name != "Mammalia" & RG_iNat_data$Iconic.taxon.name != "Reptilia" & RG_iNat_data$Iconic.taxon.name != "Amphibia",]

#3----------------------------------------------------------------------------------------
#Use a loop to compare the iNat entries to the NPS entries.
iNat_NPS_matches <- vector(mode="logical", length=0)

for (i in 1:length(RG_iNat_data2)){
  iNat_NPS_matches[i] <- RG_iNat_data2[i] %in% NPS_data2
}

#4----------------------------------------------------------------------------------------
#Use a loop to compare the iNat entries to the NPS synonym entries (1st in every entry)
NPS_data3 <- word(NPS_data$Synonyms, start=1, end=2, sep= " ")
iNat_NPSsynonym_matches <- vector(mode="logical", length=0)

for (i in 1:length(RG_iNat_data2)){
  iNat_NPSsynonym_matches[i] <- RG_iNat_data2[i] %in% NPS_data3
}

#5----------------------------------------------------------------------------------------
#Get synonyms for all of the NPSpecies entries from ITIS

#input table so we do not have to download the synonyms: code from Kelsey Cooper (Indiana State)
syn_nums <- read.table('synonym_links', sep ="|", header = FALSE, dec =".", stringsAsFactors = FALSE)
sci_names <- read.table('taxonomic_units', sep ="|", header = FALSE, dec =".", fill=T, stringsAsFactors = FALSE)
sci_names <- sci_names[c('V1','V26')]
setnames(sci_names, old=c("V1","V26"), new=c("TSN", "Scientific.Name"))
setnames(syn_nums, old=c("V1","V2", "V3"), new=c("TSN", "SYN.TSN", "Date"))
NPS_itis_synonyms_df <- merge(syn_nums, sci_names, by='TSN', all.x=T)
NPS_itis_synonyms_df <- merge(NPS_itis_synonyms_df, sci_names, by.x='SYN.TSN', by.y='TSN', all.x=T)
NPS_itis_synonyms_df <- NPS_itis_synonyms_df[-3]
NPS_itis_synonyms_df <- NPS_itis_synonyms_df[,c(1, 4, 2, 3)]
setnames(NPS_itis_synonyms_df, old=c("SYN.TSN","TSN", "Scientific.Name.x", "Scientific.Name.y"), new=c("TSN", "SYN.TSN", "SYN.Scientific.Name", "Scientific.Name"))

NPS_synonyms <- data.frame(Scientific.name=character(), Synonym=character(), stringsAsFactors = FALSE)

for (i in 1:length(NPS_data$Scientific.name)) {
    placeholderdf <- NPS_itis_synonyms_df[grepl(NPS_data$Scientific.name[i], NPS_itis_synonyms_df$Scientific.Name),] 
    placeholderdf2 <- NPS_itis_synonyms_df[grepl(NPS_data$Scientific.name[i], NPS_itis_synonyms_df$SYN.Scientific.Name),]
    NPS_synonyms <- rbind(NPS_synonyms, placeholderdf, placeholderdf2)
}


#use the df of synonyms
iNat_itis_synonym_matches <- vector(mode="logical", length=0)
iNat_itis_nonsyn_matches <- vector(mode="logical", length=0)

for (i in 1:length(RG_iNat_data2)) {
  iNat_itis_synonym_matches[i] <- RG_iNat_data2[i] %in% NPS_synonyms$SYN.Scientific.Name
  iNat_itis_nonsyn_matches[i] <- RG_iNat_data2[i] %in% NPS_synonyms$Scientific.Name
}

#6----------------------------------------------------------------------------------------
#Make a new dataframe with the raw results
data <- cbind(RG_iNat_data2, RG_iNat_data_iconic$Iconic.taxon.name, iNat_NPS_matches, iNat_NPSsynonym_matches, iNat_itis_synonym_matches, iNat_itis_nonsyn_matches, RG_iNat_data_iconic$Url)
data <- as.data.frame(na.omit(data))
colnames(data)[1] <- c("Scientific_name")
colnames(data)[2] <- c("Iconic_taxa")
colnames(data)[7] <- c("iNaturalist_URL")
data <- data[order(data$Iconic_taxa, data$Scientific_name),]

#OUtput the raw data into a csv file. 
rownames(data) <- c()
write.csv(data, file=paste(park,"_data.csv", sep=""))
data <- data[,-7]
#7----------------------------------------------------------------------------------------
#For all the tables, need to remove duplicate species entries
data2 <- unique(data)

#8----------------------------------------------------------------------------------------
#Table 1: All data; tells which falses are synonyms (to NPS or itis) and which are new

data2[,"Match NPSpecies"] <- c("")
for (i in 1:length(data2$Scientific_name)){
  if (data2[i,3]==TRUE){
    data2[i,7] <- c("Match")
  } else if (data2[i,4]==TRUE & data2[i,3]==FALSE) {
  data2[i,7] <- c("NPSpecies synonym match")
  } else if (data2[i,5] == TRUE & data2[i,3] == FALSE & data2[i,4] == FALSE) {
    data2[i,7] <- c("ITIS Match")
  } else if (data2[i,6] ==TRUE & data2[i,3] == FALSE & data2[i,4] == FALSE){
    data2[i,7] <- c("ITIS Match")
  } else {
    data2[i,7] <- c("No Match")
  }
}

table1 <- data2[,c(1,2,7)]

table1 <- add_row(.data=table1, Scientific_name=RG_iNat_not_iconic$Scientific.name, Iconic_taxa=RG_iNat_not_iconic$Iconic.taxon.name)
table1['iNaturalist_Entry_count'] <- c("")

for (i in 1:length(table1$Scientific_name)){
  if (table1[i, 2] != "Amphibia" & table1[i, 2] != "Aves" & table1[i, 2] != "Mammalia" &
      table1[i, 2] != "Plantae" & table1[i, 2] != "Reptilia"){
    table1[i,3] <- c("Table 4")
  } 
}

table1 <- table1[order(table1$Iconic_taxa, table1$Scientific_name),]
table1 <- unique(table1)


#Add in the counts for entries (number of entries per species)
for (i in 1:length(table1$Scientific_name)) {
  if (table1$Scientific_name[i] %in% data2$Scientific_name){
  table1$iNaturalist_Entry_count[i] <- length(which(data$Scientific_name == table1$Scientific_name[i]))
  } else {
    table1$iNaturalist_Entry_count[i] <- length(which(RG_iNat_not_iconic ==table1$Scientific_name[i]))
  }
}

rownames(table1) <- c()
write.csv(table1, paste(park, "_table1.csv", sep=""))

#9----------------------------------------------------------------------------------------
#Table 2: All Synonym data and what the taxonomic update is (itis only)

#1. set up table to have 4 columns
table2 <- table1[table1$`Match NPSpecies` =="ITIS Match",]
table2 <- table2[,c(2,1)]
colnames(table2)[colnames(table2)=="Scientific_name"] <- "iNat_entry" #Giving some weird warning message that is annoying
table2$iNat_entry <- as.character(table2$iNat_entry)
table2[,"NPSpecies_name"] <- c("") 
table2[,"Accepted_TSN"] <- c("")
table2[,"NPS_entry_TSN"] <- c("")

for (i in 1:length(table2$iNat_entry)) {
  if (table2$iNat_entry[i] %in% NPS_synonyms$SYN.Scientific.Name == TRUE){
    table2$Accepted_TSN[i] <- NPS_synonyms$TSN[match(table2$iNat_entry[i], NPS_synonyms$SYN.Scientific.Name)]
    table2$NPS_entry_TSN[i] <- NPS_synonyms$SYN.TSN[match(table2$iNat_entry[i], NPS_synonyms$SYN.Scientific.Name)]
    table2$NPSpecies_name[i] <- NPS_synonyms$Scientific.Name[match(table2$iNat_entry[i], NPS_synonyms$SYN.Scientific.Name)]
  } else if (table2$iNat_entry[i] %in% NPS_synonyms$Scientific.Name == TRUE) {
    table2$Accepted_TSN[i] <- NPS_synonyms$TSN[match(table2$iNat_entry[i], NPS_synonyms$Scientific.Name)]
    table2$NPS_entry_TSN[i] <- NPS_synonyms$SYN.TSN[match(table2$iNat_entry[i], NPS_synonyms$Scientific.Name)]
    table2$NPSpecies_name[i] <- NPS_synonyms$SYN.Scientific.Name[match(table2$iNat_entry[i], NPS_synonyms$Scientific.Name)]
    }
  }

rownames(table2) <- c()
write.csv(table2, paste(park,"_table2.csv", sep=""))

#10----------------------------------------------------------------------------------------
#Table 3: All potentially new species
#columns: taxa, scientific name, common name, BISON occurrences, invasive
table3 <- table1[table1$`Match NPSpecies`=="No Match",]
table3 <- table3[,c(2:1)]
table3[,"BISON"] <- c("")
table3[,"Invasive.Record"] <- c("")

#Invasive record 
inv_list <- read.csv("https://raw.githubusercontent.com/KelseyDCooper/USGS-NPS-App/master/invasives_list.csv")
for (i in 1:length(table3$Scientific_name)){
  if (table3$Scientific_name[i] %in% inv_list$Scientific.Name){
    table3$Invasive.Record[i] <- c("Yes")
  } else {
    table3$Invasive.Record[i] <- c("No")
  }
}

#Add in BISON entries. Best to do this last due to the vast amounts of data (list of states) that isn't easily visible in R dataframe.
#Takes a bit of time.

long.name <- c(state.name, "Alberta Canada", "American Samoa", "British Columbia Canada", "Commonwealth of the Northern Mariana Islands",
               "District of Columbia", "Guam", "Manitoba Canada", "New Brunswick Canada", 
               "Newfoundland and Labrador Canada", "Northwest Territories Canada", 
               "Nova Scotia Canada", "Nunavut Canada", "Ontario Canada", "Prince Edward Island Canada",
               "Puerto Rico", "Quebec Canada", "Saskatchewan Canada", "United States Virgin Islands", 
               "Yukon Canada", "Alaska EEZ", "American Samoa EEZ", "Hawaii EEZ", "Howland and Baker Island EEZ",
               "Jarvis Island EEZ", "Johnston Atoll EEZ", "Northern Mariana Islands and Guam EEZ", 
               "Palmyra Atoll EEZ", "Puerto Rico EEZ", "US Atlantic EEZ", "US Pacific EEZ",
               "US Virgin Islands EEZ", "Wake Island EEZ")
short.name <- c(state.abb, "AB CAN", "AS", "BC CAN", "CNMI", "DC", "GU", "MB CAN", "NB CAN",
                "NL CAN", "NT CAN", "NS CAN", "NU CAN", "ON CAN", "PE CAN", "PR", "QC CAN",
                "SK CAN", "USVI", "YT CAN", "AK EEZ", "AS EEZ", "HI EEZ", "Howland-Baker EEZ",
                "Jarvis EEZ", "Johnston Atoll EEZ", "N MP-GU EEZ", "Palmyra Atoll EEZ", "PR EEZ", "US Atlantic EEZ", "US Pacific EEZ",
                "USVI EEZ", "Wake EEZ")
all.states <- as.data.frame(cbind(long.name, short.name), stringsAsFactors = FALSE)

for (i in 1:length(table3$Scientific_name)){
  out <- bison(species=table3$Scientific_name[i], count=1)
  if (is.null(out$states) == FALSE){
    state.counts <- as.data.frame(cbind(out$states[,"record_id"]))
    for (k in 1:length(state.counts$V1)){
      state.counts$V2[k] <- all.states$short.name[match(state.counts$V1[k], all.states$long.name)]
    }
    states <- paste(state.counts$V2, out$states[,'total'])
    states <- paste(states, collapse="; ")
    table3[i, 'BISON'] <- states
  } else {
    table3[i, 'BISON'] <- "No data available"
  }
}

#export table
rownames(table3) <- c()
write.csv(table3, paste(park, "_table3.csv", sep=""))
#11----------------------------------------------------------------------------------------
#Table 4
table4 <- RG_iNat_not_iconic[,c(5,3,4)]
table4 <- table4[order(table4$Iconic.taxon.name, table4$Scientific.name),]
table4 <- unique(table4)

rownames(table4) <- c()
write.csv(table4, paste(park, "_table4.csv", sep=""))

#12----------------------------------------------------------------------------------------
#Graph

table1a <- data2[,c(1,2,7)]

counts2 <- table(table1a$`Match NPSpecies`, table1a$Iconic_taxa) 
counts3 <- counts2[,-4] #delete the plants column from the table
colnames(counts3) <- c("Amph", "Aves", "Mamm", "Rept")
#give us y axis maximums
dfcounts <- as.data.frame(counts2)
amphibiasum <- sum(dfcounts$Freq[dfcounts$Var2 == "Amphibia"])
avessum <- sum(dfcounts$Freq[dfcounts$Var2 == "Aves"])
mammaliasum <- sum(dfcounts$Freq[dfcounts$Var2 == "Mammalia"])
plantaesum <- sum(dfcounts$Freq[dfcounts$Var2 == "Plantae"])
reptiliasum <- sum(dfcounts$Freq[dfcounts$Var2 == "Reptilia"])

all_high <- max(rbind(amphibiasum, avessum, mammaliasum, plantaesum, reptiliasum)) +20
sub_high <- max(rbind(amphibiasum, avessum, mammaliasum, reptiliasum)) +20

#Time to graph!
pdf(paste(park,"_figure1.pdf", sep=""), width=7, height=8)

#par(fig=c(0,1,0,1))
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)

barplot(counts2, ylim=c(0,all_high), xlab="Taxa", col=c("cadetblue1", "seashell","lightsalmon"), 
        ylab="Number of Species")

legend("right", inset=c(-0.35,0), legend = rownames(counts2) , fill = c("cadetblue1", "seashell","lightsalmon","mediumorchid4"))

par(fig=c(0.75,1,0.75,1), mar=c(1,1,1,1), new=TRUE)
#par(fig=c(0.1,0.5,0.5,1), new=TRUE)
barplot(counts3, ylim=c(0,sub_high),col=c("cadetblue1", "seashell","lightsalmon", "mediumorchid4"), cex.names=0.45)



dev.off()







