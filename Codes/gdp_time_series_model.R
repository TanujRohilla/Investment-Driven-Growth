library(ggplot2)
library(xlsx)

model_graph_creation <- function(gdp_per_industry,industry_name)
{ 
  gdp_per_industry <- unlist(gdp_per_industry)
  time_series <- seq(1950,2013,by = 1)
  x <- seq(1,length(time_series),by = 1)
  y <- log(gdp_per_industry)
  #Model Creation & Prediction
  model <- lm(y ~ x)
  prediction <- t(unlist(lapply(x, function(x){predict(model,data.frame(x = x))})))
  graph_data <- as.data.frame(t(rbind(exp(prediction),gdp_per_industry)))

  #Graph Creation & Cofficients Extraction
  graph <- ggplot(graph_data,aes(time_series)) + geom_line(aes(y = V1,color = "Prediction")) + geom_line(aes(y=gdp_per_industry,color = "Actual")) + xlab("Time Series") + ylab("GDP (in crores)") + ggtitle(industry_name)
  coff <- as.data.frame(summary(model)$coefficients[,1])  
  row.names(coff) <- c("Intercept","Time")
  colnames(coff) <- "Cofficients"
  
  
  #Future Predictions
  predict_x <- c(65,66,67,68)  #Predict for years 2014-15,2015-16,2016-17,2017-18
  future_predictions <- as.data.frame(exp(unlist(lapply(predict_x, function(predict_x){predict(model,data.frame(x = predict_x))}))))
  row.names(future_predictions) <- c("2014-15","2015-16","2016-17","2017-18")
  colnames(future_predictions) <- "GDP"
  
  data <- list(time_series_graph = graph,cofficient = coff,prediction = future_predictions)
  return(data)
}

gdp_data <- read.csv("/home/krishna/Documents/DM/Investment-Driven-Growth/CSV/GDP_Graphs.csv",header = TRUE)
industries_gdp <- gdp_data[,1]
gdp_data <- gdp_data[,-1]

#all.plots <- vector(nrow(gdp_data),mode = 'list') #Storing all the Plots in a single List
gdp_predicted_file <- "/home/krishna/Documents/DM/Investment-Driven-Growth/Output/GDP_Predicted.csv"
gdp_model_file <- "/home/krishna/Documents/DM/Investment-Driven-Growth/Output/GDP_Time_Series_Model.csv"

predicted_gdp <- data.frame()[1:4,]
model_gdp <- data.frame()[1:2,]
for(industry in 1:nrow(gdp_data)){
  info <- model_graph_creation(gdp_data[industry,],industries_gdp[industry])
  
  #Plotting & Recording Data
  plot(info$time_series_graph)
  all.plots[[industry]] <- recordPlot()
  
  predicted_gdp <- cbind(predicted_gdp,info$prediction)
  model_gdp <- cbind(model_gdp,info$cofficient)
}

#Writing Prediction to CSV File
predicted_gdp <- t(predicted_gdp)
row.names(predicted_gdp) <- industries_gdp
colnames(predicted_gdp) <- c("2014-15","2015-16","2016-17","2017-18")
write.csv(predicted_gdp,file = gdp_predicted_file)

#Writing Model Cofficients to CSV File
model_gdp <- t(model_gdp)
row.names(model_gdp) <- industries_gdp
colnames(model_gdp) <- c("Intercept","Time")
write.csv(model_gdp,file = gdp_model_file)

#Output Graphs to PDF
pdf("/home/krishna/Documents/DM/Investment-Driven-Growth/Output/GDP_Time_Series_Graph.pdf",onefile = TRUE)
for(plot in all.plots){
  replayPlot(plot)
}
graphics.off()