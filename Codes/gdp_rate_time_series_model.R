detrending_data <- function(data){
  
  resultant_data <- c()
  time_series <- seq(1950,2013,by = 1)
  industries_gdp <- data[,1]
  data <- data[,-1]
  x <- seq(1,length(time_series),by = 1)
  
  industry <- 6
  #for(industry in 1:nrow(data)){
    y <- unlist(data[industry,])
    model <- lm(y ~ x)
    print(summary(model))
    #print(predict(model,data.frame(x = 1)))
    #prediction <- t(unlist(lapply(x, function(x){predict(model,data.frame(x = x))})))
    #resultant_data <- rbind(resultant_data,prediction)
    #View(rbind(prediction,y))
   #}
}

gdp_rate <- read.csv("/home/krishna/Documents/DM/Investment-Driven-Growth/CSV/GDP_Graphs.csv",header = TRUE)
detrending_data(gdp_rate)