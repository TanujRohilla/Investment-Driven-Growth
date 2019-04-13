library(ggplot2)

model_creation <- function(data){
  
  resultant_data <- c()
  time_series <- seq(1950,2013,by = 1)
  industries_gdp <- data[,1]
  data <- data[,-1]
  x <- seq(1,length(time_series),by = 1)
  
  all.plots <- vector(nrow(data),mode = 'list') #Storing all the Plots in a single List
  
  industry <- 6
  for(industry in 1:nrow(data)){
    gdp <- unlist(data[industry,])
    y <- log(gdp)
    
    model <- lm(y ~ x)
    #print(predict(model,data.frame(x = 1)))
    prediction <- t(unlist(lapply(x, function(x){predict(model,data.frame(x = x))})))
    #plot(x = time_series,y = exp(prediction),type = "l")
    graph_data <- as.data.frame(t(rbind(exp(prediction),gdp)))
    graph <- ggplot(graph_data,aes(time_series)) + geom_line(aes(y = V1,color = "Prediction")) + geom_line(aes(y=gdp,color = "Actual")) + xlab("Time Series") + ylab("GDP (in crores)") + ggtitle(industries_gdp[industry])
    plot(graph)
    all.plots[[industry]] <- recordPlot() 
  }
  pdf("/home/krishna/Documents/DM/Investment-Driven-Growth/Output/GDP_Time_Series.pdf",onefile = TRUE)
  for(plot in all.plots){
    replayPlot(plot)
  }
  graphics.off()
}

gdp_rate <- read.csv("/home/krishna/Documents/DM/Investment-Driven-Growth/CSV/GDP_Graphs.csv",header = TRUE)
model_creation(gdp_rate)