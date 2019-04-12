model_creation <- function(data){
  
  resultant_data <- c()
  time_series <- seq(1950,2013,by = 1)
  industries_gdp <- data[,1]
  data <- data[,-1]
  x <- seq(1,length(time_series),by = 1)
  
  industry <- 6
  #for(industry in 1:nrow(data)){
    temp <- unlist(data[industry,])
    View(rbind(x,temp))
    y <- log(temp)
    
    model <- lm(y ~ x)
    print(summary(model))
    #print(predict(model,data.frame(x = 1)))
    prediction <- t(unlist(lapply(x, function(x){predict(model,data.frame(x = x))})))
    resultant_data <- rbind(resultant_data,prediction)
    plot(x = time_series,y = exp(prediction),type = "l")
    View(rbind(exp(prediction),temp))
   #}
}

gdp_rate <- read.csv("/home/krishna/Documents/DM/Investment-Driven-Growth/CSV/GDP_Graphs.csv",header = TRUE)
model_creation(gdp_rate)