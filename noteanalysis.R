library (dplyr)

#Load notes into dataframe
df <- read.csv("generalnotes.csv", stringsAsFactors = FALSE, header = FALSE)

#Create only dataframe with needed information
notes <- select(df, V1, V2)

#Using aggregate to create notes as a tidy dataset
notes <- aggregate(V2 ~ V1, data = notes, paste, collapse = ", ")

#-----Start subsetting data based on matching words-----
#Creating refund related notes and apply them across notes dataframe
matchwords <- c("refund", "damaged")
refund.notes <- notes[grep(paste(matchwords, collapse = "|"), notes$V2),]

#Match logical note numbers with row id:
rowlocation <- which (notes$V1 %in% refund.notes$V1)

#Remove subsetted data from original data.frame and preserving the data.frame structure
notes <- notes[-rowlocation,]


#Same four components for a new group of matching words.
matchwords <- c("recommend")
recommend.notes <- notes[grep(paste(matchwords, collapse = "|"), notes$V2),]
rowlocation <- which (notes$V1 %in% recommend.notes$V1)
notes <- notes[-rowlocation,]