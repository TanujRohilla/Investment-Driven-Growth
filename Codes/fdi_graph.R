library(ggplot2)
fdi <- read.csv("/home/krishna/Documents/DM/Investment-Driven-Growth/CSV/FDI.csv",header = TRUE)
sectors <- fdi[,1] #First Column contains name of Industry
fdi <- fdi[,-1]
time_series <- seq(2000,2016,by = 1)

i = 1

all.plots <- vector(nrow(fdi),mode = 'list') #Storing all the Plots in a single List

#Plotting Histogram for each Sectors in the Gross Domestic Product DataSet
for(i in 1:nrow(fdi)){
  fdi_per_industry <- fdi[i,]
  plot_data <- data.frame(years = time_series,fdi = as.numeric(fdi_per_industry))
  graph <- ggplot(plot_data,aes(years,fdi)) + geom_bar(stat = "identity",colour = "black",fill = "red") +
    ggtitle(sectors[i]) + xlab("Time Series") + ylab("FDI (in US Dollars (millions))")
  plot(graph)
  all.plots[[i]] <- recordPlot() 
}

pdf("/home/krishna/Documents/DM/Investment-Driven-Growth/Output/Histogram_FDI.pdf",onefile = TRUE)
for(plot in all.plots){
  replayPlot(plot)
}
graphics.off()