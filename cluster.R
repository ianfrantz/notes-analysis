#The following script creates the text clusters for analysis
library (dplyr)

#Load Noble Data
df <- read.csv("generalnotes.csv", stringsAsFactors = FALSE, header = FALSE)

#Create only dataframe with needed information
notes <- select(df, V1, V2)

#Using aggregate to create notes as a tidy dataset
notes <- aggregate(V2 ~ V1, data = notes, paste, collapse = ", ")

#Convert to lowercase
notes$V2 <- tolower(notes$V2)

#Aggregated lookup table 
lookuptable <- aggregate(V1~ V2, data = df, paste, collapse = ", ")

#######-----Intialize "cluster" function-----

newfile <- data_frame()

cluster <- function(matchwords) {
  newfile <<- notes[grep(paste(matchwords, collapse = "|"), notes$V2),]
  rowlocation <- which (notes$V1 %in% newfile$V1)
  notes <<- notes[-rowlocation,]
}


#CLUSTER: Furniture Sales
cluster (c("furniture","sale"))
activiation.notes <- newfile

#CLUSTER: Refund
cluster (c("refund"))
payment.notes <- newfile 

#CLUSTER: Invoice
cluster  (c("invoice"))
pin.notes <- newfile 

#CLUSTER: Return
cluster  (c("return", "merch"))
fee.notes <- newfile 

#CLUSTER: Delivery Problem
cluster  (c("delivery","problem"))
web.notes <- newfile 
