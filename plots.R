#Create plots
library(ggplot2)
library (purrr)


#Find all data.frames in global and isolate clusternames
clusternames <- names(which(unlist(eapply(.GlobalEnv,is.data.frame)))) 
x <- grep(".notes", clusternames)
clusternames <- clusternames[x]

#Create number of data observations vector(dataobs) using map to iterate over clusternames into function nrow
dataobs <- map(clusternames, .f = function(x) {
  x <- get(x)
  if ("data.frame" %in% class(x)) {
    nrow(x)
  }
})

#Build the results data.frame
clusternames <- gsub(".notes","",clusternames)
results <- data.frame(Clusters = clusternames)
data <- unlist(dataobs)
results <- cbind(results, Data = data)
results <- results[order(results$Data, decreasing = TRUE), ]


#Create "resultsplot" vector with core plotting
resultsplot <- ggplot(results, aes(x = Clusters, y = Data, fill = Data)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = results$Clusters) +
  scale_fill_gradient(high = "red", low = "green") 

#Manual Entries for Titles, Subtitles and Captions.
plot1title <- readline("What should the of the plot be? ")
plot1subtitle <- readline("Add a subtitle? ")
plot1caption <- readline("Add a caption? ")

#Add Labeling for Aug 2017 
resultsplot + labs(title = plot1title,
                   subtitle = plot1subtitle,
                   caption = plot1caption)

                   #subtitle = "Total Notes: 15
                   #Increase in: 20% complaints",
                   

#Plotting two different data sources
ggplot(NULL, aes(x = Clusters, y = Data, group = Data)) + 
  geom_point(data = milestoneresults, size = 5, color = "red") +
  geom_point(data = indigoresults, size = 3, color = "blue") + 
  scale_y_continuous(limits = c(0,15000))
