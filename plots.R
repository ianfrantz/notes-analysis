#Create plots
library(ggplot2)
library (purrr)

#######-----Start Plots-----
#Step 1 - Put clusters and results into data.frame
#Step 2 - Build class? / levels?
#Step 3 - geom_histogram

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


#Plot the results
ggplot(results, aes(x = Clusters, y = Data, fill = Data)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = results$Clusters) +
  scale_fill_gradient(high = "red", low = "green") +
  labs(title = "Payment Phone Calls (4/1/17 - 6/31/17)",
       subtitle = "Total Notes: 26579", 
       caption = "4917 uncategorized payment notes remain but removed from plot")

#Plotting more than one result
ggplot(NULL, aes(x = Clusters, y = Data, group = Data)) + 
  geom_point(data = milestoneresults, size = 5, color = "red") +
  geom_point(data = indigoresults, size = 3, color = "blue") + 
  scale_y_continuous(limits = c(0,15000))


