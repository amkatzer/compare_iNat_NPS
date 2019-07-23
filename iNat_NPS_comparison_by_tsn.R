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
setwd("C:/Users/amkat/Desktop/VSFS")
library(rinat)
library(taxize)
library(stringr)
library(tibble)
library("rbison")
#devtools::install_github("ropensci/rbison")

#2----------------------------------------------------------------------------------------
#First, import data
#You must remake the header line in the csv file for this to work. 
#Completely delete line 1 and make new headers in excel. Save as a csv.
NPS_data <- read.csv("./LEWI/NPSpecies_Checklist_LEWI_20190516093957.csv", header=T, skipNul = T)
#NPS_data2 <- word(NPS_data$Scientific.name, start=1, end=2, sep=" ")

#Get iNaturalist data through an API so we don't have to deal with downloading it
#get_inat_obs_project is from the rinat package

iNat_data <- get_inat_obs_project("2016-national-parks-bioblitz-lewis-and-clark", type="observations")

#Create new data frame with only the RG & iconic taxa
RG_iNat_data <- iNat_data[iNat_data$Quality.grade == "research",]

RG_iNat_data[,"tsn"] <- c("")
for(q in 1:length(RG_iNat_data$Scientific.name)){
  temp_tsn <- as.tsn(get_tsn(RG_iNat_data$Scientific.name[q]))
  RG_iNat_data$tsn[q] <- as.integer(temp_tsn)
}

RG_iNat_data_iconic <- RG_iNat_data[RG_iNat_data$Iconic.taxon.name == "Plantae" 
                                    | RG_iNat_data$Iconic.taxon.name == "Aves" 
                                    | RG_iNat_data$Iconic.taxon.name == "Mammalia" 
                                    | RG_iNat_data$Iconic.taxon.name == "Reptilia" 
                                    | RG_iNat_data$Iconic.taxon.name == "Amphibia",]

#Make new dataframe with only the RG, non-iconic taxa will be stored. This will be used in table 4.
RG_iNat_not_iconic <- RG_iNat_data[RG_iNat_data$Iconic.taxon.name != "Plantae" 
                                   & RG_iNat_data$Iconic.taxon.name != "Aves" 
                                   & RG_iNat_data$Iconic.taxon.name != "Mammalia" 
                                   & RG_iNat_data$Iconic.taxon.name != "Reptilia" 
                                   & RG_iNat_data$Iconic.taxon.name != "Amphibia",]

#3----------------------------------------------------------------------------------------
#Use a loop to compare the iNat entries to the NPS entries.
iNat_NPS_matches <- vector(mode="logical", length=0)

for (i in 1:length(RG_iNat_data)) {
  iNat_NPS_matches[i] <- RG_iNat_data$tsn[i] %in% NPS_data$TSN
}

#4----------------------------------------------------------------------------------------
#WAY TO DO THIS BY TSN??? Currently none are listed in the NPS species lists
#May be able to do this by getting tsn for each synonym listed

#Use a loop to compare the iNat entries to the NPS synonym entries
NPS_data3 <- word(NPS_data$Synonyms, start=1, end=2, sep= " ")
iNat_NPSsynonym_matches <- vector(mode="logical", length=0)

for (i in 1:length(RG_iNat_data2)){
  iNat_NPSsynonym_matches[i] <- RG_iNat_data2[i] %in% NPS_data3
}

#5----------------------------------------------------------------------------------------
#Get synonyms for all of the NPSpecies entries from ITIS
#This can take a while depending on the number of NPS entries

NPS_itis_synonyms <- vector(length=0)

#for (j in 1:length(NPS_data$Scientific.name)){
#  NPS_itis_synonyms[j] <- synonyms(NPS_data$Scientific.name[j], db='itis', rows = 1) #synonyms function is from the taxize package; rows=1 is taking the first row from every entry. This may not be the best way, but it is the best way to automate
#}

#Take out all of the columns besides the synonym names 
#NPS_itis_synonyms2 <- lapply(NPS_itis_synonyms, function(x) x[(names(x) %in% c("syn_name"))])
#NPS_itis_synonyms3 <- unlist(NPS_itis_synonyms2)
#NPS_itis_synonyms4 <- word(NPS_itis_synonyms3, start=1, end=2, sep=" ")

#Compare the iNat entries to the synonym names
#iNat_itis_synonym_matches <- vector(mode="logical", length=0)

#for (i in 1:length(RG_iNat_data2)){
#  iNat_itis_synonym_matches[i] <- RG_iNat_data2[i] %in% NPS_itis_synonyms4
#}


#6----------------------------------------------------------------------------------------
#Make a new dataframe with the raw results
data <- cbind(RG_iNat_data2, RG_iNat_data_iconic$Iconic.taxon.name, iNat_NPS_matches, iNat_NPSsynonym_matches, iNat_itis_synonym_matches)
data <- as.data.frame(na.omit(data))
colnames(data)[1] <- c("Scientific_name")
colnames(data)[2] <- c("Iconic_taxa")
data <- data[order(data$Iconic_taxa, data$Scientific_name),]

#OUtput the raw data into a csv file. 
write.csv(data, file="LEWI_data.csv")

#7----------------------------------------------------------------------------------------
#For all the tables, need to remove duplicate species entries
data2 <- unique(data)

#8----------------------------------------------------------------------------------------
#Table 1: All data; tells which falses are synonyms (to NPS or itis) and which are new
#need to append the non-iconic taxa into the table.

data2[,"Match NPSpecies"] <- c("")
for (i in 1:length(data2$Scientific_name)){
  if (data2[i,3]==TRUE){
    data2[i,6] <- c("Match")
  } else if (data2[i,4]==TRUE & data2[i,3]==FALSE) {
    data2[i,6] <- c("NPSpecies synonym match")
  } else if (data2[i,5] == TRUE & data2[i,3] == FALSE & data2[i,4] == FALSE) {
    data2[i,6] <- c("ITIS Match")
  } else {
    data2[i,6] <- c("No Match")
  }
}

table1 <- data2[,c(1,2,6)]

table1 <- add_row(.data=table1, Scientific_name=RG_iNat_not_iconic$Scientific.name, Iconic_taxa=RG_iNat_not_iconic$Iconic.taxon.name)

for (i in 1:length(table1$Scientific_name)){
  if (table1[i, 2] != "Amphibia" & table1[i, 2] != "Aves" & table1[i, 2] != "Mammalia" &
      table1[i, 2] != "Plantae" & table1[i, 2] != "Reptilia"){
    table1[i,3] <- c("Table 4")
  } 
}

table1 <- table1[order(table1$Iconic_taxa, table1$Scientific_name),]
table1 <- unique(table1)
#The above function is not working correctly. Need to figure out how to get the iconic taxa to be in alphabetical order
#It isn't a ordered factor either

write.csv(table1, "LEWI_table1.csv")

#9----------------------------------------------------------------------------------------
#Table 2: All Synonym data and what the taxonomic update is (itis only)

#1. set up table to have 4 columns
table2 <- data2[data2$`Match NPSpecies`=="ITIS Match",]
table2 <- table2[,c(2,1)]
colnames(table2)[colnames(table2)=="Scientific_name"] <- "iNat_entry"
table2[,"NPSpecies_name"] <- c("") 
table2[,"Accepted_TSN"] <- c("")
table2[,"Accepted_name"] <- c("")
table2[,"Reference"] <- c("")

tempsynonym <- synonyms(table2$Scientific_name, db="itis")#find iNat synonyms in ITIS
tempsyndf <- as.data.frame(tempsynonym[[1]]) #practice code to see if the numbers will pull out the species name. This works.
tempmatches <- tempsyndf$syn_name %in% table2$Scientific.name
tempmatches2 <- cbind(tempsyndf$syn_name, tempsyndf$syn_author, tempmatches)




for (k in 1:length(table2$Scientific_name)){
  tempsyndf <- as.data.frame(tempsynonym[[k]]) #Pull out dataframe from list
  tempmatches <- tempsyndf$syn_name %in% NPS_data$Scientific.name #compare entries with NPSpecies;problem line??
  tempmatches2 <- cbind(tempsyndf$syn_name, tempsyndf$syn_author, tempmatches) #Make new df
  for (m in 1:length(tempmatches2[,1])){
    if (tempmatches2[m,3] == TRUE) {
      table2[k, 3] <- c(tempmatches2$syn_name)
      table2[k,4] <- c(tempmatches2$syn_author)
    }
  }
}

table2





#10----------------------------------------------------------------------------------------
#Table 3: All potentially new species
#columns: taxa, scientific name, common name, BISON occurrences, iNat observation, invasive?
table3 <- table1[table1$`Match NPSpecies`=="No Match",]
table3 <- table3[,c(2:1)]
table3[,"BISON"] <- c("")
table3[,"iNaturalist.Entry"] <- c("")
table3[,"Invasive.Record"] <- c("")

#Add in iNaturalist entry
for (i in 1:length(table3$Scientific_name)){
  if (table3[i,"iNaturalist.Entry"]==""){
    if (RG_iNat_data_iconic == table3[i,"Scientific_name"]){ #This line is the problem. 
      #Error about the == comparing to an entry (incompatible methods). 
      #I don't think %in% is right though here.
      table3[i,"iNaturalist.Entry"] <- c(RG_iNat_data_iconic[i,"Url"])}
  } 
}


#Add in Invasive record
for (i in 1:length(table3$Scientific_name)){
  
}


#Add in BISON entries. Best to do this last due to the vast amounts of data (list of states) that isn't easily visible in R dataframe.
#Takes a bit of time.
for (i in 1:length(table3$Scientific_name)){
  out <- bison(species=table3[i,"Scientific_name"], count=5)
  states <- paste(out$states[,'record_id'], out$states[,'total'])
  states <- paste(states, collapse="; ")
  table3[i,'BISON'] <- states
}

write.csv(table3, "LEWI_table3.csv")
#11----------------------------------------------------------------------------------------
#Table 4
table4 <- RG_iNat_not_iconic[,c(5,3,4)]
table4 <- table4[order(table4$Iconic.taxon.name, table4$Scientific.name),]
table4 <- unique(table4)

write.csv(table4, "LEWI_table4.csv")

#12----------------------------------------------------------------------------------------
#Graph - need to introduce variables for the axes that can be easily changed at the top to automize better. Also need to change if there will
#be 4 or 3 colors for the graph (NPS synonym being added in)
table1a <- data2[,c(1,2,6)]

#No Plants stacked barplot
counts2 <- table(table1a$`Match NPSpecies`, table1a$Iconic_taxa) #not really sure what the plants part is still in here.
counts3 <- counts2[,-4] #delete the plants column from the table

pdf("LEWI_figure1.pdf", width=7, height=8)

par(fig=c(0,1,0,1))
barplot(counts2, ylim=c(0,300), xlab="Taxa", col=c("cadetblue1", "seashell","lightsalmon"), 
        ylab="Number of Species")
legend("topright", inset=0.02, legend = rownames(counts2) , fill = c("cadetblue1", "seashell","lightsalmon","mediumorchid4"))

par(fig=c(0.1,0.5,0.5,1), new=TRUE)
barplot(counts3, ylim=c(0,70),col=c("cadetblue1", "seashell","lightsalmon", "mediumorchid4"), cex.names=0.45)

dev.off()









