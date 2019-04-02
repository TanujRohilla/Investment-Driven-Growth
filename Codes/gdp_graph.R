library(ggplot2)
gross_domestic_product <- read.csv(file = "/home/krishna/Documents/DM/Investment-Driven-Growth/CSV/GDP_Graphs.csv",header = TRUE)
industries <- gross_domestic_product[,1] #First Column contains name of Industry
gross_domestic_product <- gross_domestic_product[,-1]
time_series <- seq(1950,2013,by = 1)

all.plots <- vector(nrow(gross_domestic_product),mode = 'list') #Storing all the Plots in a single List

#Plotting Histogram for each Industries in the Gross Domestic Product DataSet
for(i in 1:nrow(gross_domestic_product)){
  gdp_per_industry <- gross_domestic_product[i,]
  plot_data <- data.frame(years = time_series,gdp = as.numeric(gdp_per_industry))
  graph <- ggplot(plot_data,aes(years,gdp)) + geom_bar(stat = "identity",colour = "black",fill = "red") +
            ggtitle(industries[i]) + xlab("Time Series") + ylab("GDP (in crores)")
  plot(graph)
  all.plots[[i]] <- recordPlot() 
}

pdf("/home/krishna/Documents/DM/Investment-Driven-Growth/Output/Histogram_GDP.pdf",onefile = TRUE)
for(plot in all.plots){
  replayPlot(plot)
}
graphics.off()