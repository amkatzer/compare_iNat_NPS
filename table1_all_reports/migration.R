#this code is meant to identify the species not previously identified in the parks where there were less than 50 (or other specified number) in the park

library(stringr) #use str_extract from this package

table3 <- read.csv("ACAD_table3.csv", stringsAsFactors = FALSE)

park_states <- read.csv("nps_boundary.csv", stringsAsFactors = FALSE)

for (i in 1:length(table3$Scientific_name)){
  numberinBISON <- unlist(strsplit(table3$Occurences.in.BISON[i], split=";")) #splits up the BISON occurrences from table 3
  #tabnumberinBISON <- data.frame(unlist(strsplit(numberinBISON, split=" [0-9]+$")), unlist(strsplit(numberinBISON, split=" [0-9]+$"))[2])
  parkstate <- park_states$STATE[park_states$UNIT_CODE == "ACAD"] #get park state from the database
  statedata <- str_extract(numberinBISON[pmatch(parkstate, numberinBISON)==TRUE], "[0-9]+$") #struggling to get a match between the park state and BISON
}


