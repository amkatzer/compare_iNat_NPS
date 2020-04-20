#this code is meant to identify the species not previously identified in the parks where there were less than 50 (or other specified number) in the park

library(stringr) #use str_extract from this package

table3 <- read.csv("ACAD_table3.csv", stringsAsFactors = FALSE)
parkname <- "ACAD"
park_states <- read.csv("nps_boundary.csv", stringsAsFactors = FALSE)

BISON_in_state <- rep(NA, length(table3$Scientific_name))
speciesundercutoff <- data.frame(table3$Scientific_name, table3$Invasive.Record, BISON_in_state, stringsAsFactors = FALSE)

cutoff <- 50 #put in the cutoff amount that you want for entries in the state

for (i in 1:length(table3$Scientific_name)){
  numberinBISON <- unlist(strsplit(table3$Occurences.in.BISON[i], split=";")) #splits up the BISON occurrences from table 3
  numberinBISON <- gsub("[[:space:]]", "", numberinBISON) #remove white spaces
  parkstate <- park_states$STATE[park_states$UNIT_CODE == parkname] #get park state from the database
  statedata <- as.integer(str_extract(numberinBISON[pmatch(parkstate, numberinBISON)], "[0-9]+$")) 
  if (statedata <= cutoff) {
    speciesundercutoff$BISON_in_state[i] <- statedata
  }
}

speciesundercutoff <- na.omit(speciesundercutoff[speciesundercutoff$BISON_in_state != "NA",])
speciesundercutoff


