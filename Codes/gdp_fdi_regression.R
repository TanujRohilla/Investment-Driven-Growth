find_linear_relation <- function(gdp,fdi_values){
  combined_data <- (cbind(y = gdp,x = fdi_values))
  combined_data <- as.data.frame(scale(combined_data))
  #View(combined_data)
  
  linear_model <- lm(y~.,data = combined_data)
  View(linear_model)
  print(summary(linear_model))
}


fdi <- read.csv("/home/krishna/Documents/DM/Investment-Driven-Growth/CSV/FDI_Selected(Matched).csv",header = TRUE)
gdp <- read.csv("/home/krishna/Documents/DM/Investment-Driven-Growth/CSV/GDP_Selected.csv",header = TRUE)

industries_gdp <- gdp[,1]
gdp <- gdp[,-1]
sectors_fdi <- fdi[,1]
fdi <- fdi[,-1]

fdi <- t(fdi)
gdp <- t(gdp)

industry <- 8
#for(industry in 1:nrow(gdp)){
  fdi_relevant_sectors <- fdi[,industry == fdi[1,]]
  find_linear_relation(gdp[,industry],fdi_relevant_sectors[-1,])
#}