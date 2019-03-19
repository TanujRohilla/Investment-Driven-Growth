library(ggplot2)
gdp_rate <- read.csv("/home/krishna/Documents/DM/FDI/CSV/GDP_Rate.csv",header = TRUE)
industries <- gdp_rate[,1] #First Column contains name of Industry
gdp_rate <- gdp_rate[,-1]
time_series <- seq(1951,2013,by = 1)

all.plots <- vector(nrow(gross_domestic_product),mode = 'list') #Storing all the Plots in a single List

#Plotting Time Series of Gross Domestic Product Rate for each Industries in the DataSet
for(i in 1:nrow(gdp_rate)){
  gdp_per_industry <- gdp_rate[i,]
  plot_data <- data.frame(years = time_series,gdp = as.numeric(gdp_per_industry))
  graph <- ggplot(plot_data,aes(years,gdp)) + geom_line() + 
            ggtitle(industries[i]) + xlab("Time Series") + ylab("GDP Rate")
  plot(graph)
  all.plots[[i]] <- recordPlot() 
}

pdf("/home/krishna/Documents/DM/FDI/Output/TimeSeries_GDP_Rate.pdf",onefile = TRUE)
for(plot in all.plots){
  replayPlot(plot)
}
graphics.off()