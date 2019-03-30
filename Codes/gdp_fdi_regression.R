library(xlsx)

find_linear_relation <- function(gdp,fdi_values){
  combined_data <- (cbind(y = gdp,x = fdi_values))
  
  standard_deviation <- apply(combined_data, 2,sd)  #Finding Standard Deviation of Each Column
  combined_data <- as.data.frame(scale(combined_data))
  
  linear_model <- lm(y~.,data = combined_data)
  data <- list(coff = summary(linear_model)$coefficients[,1],sd = standard_deviation)
  return(data)
}

fdi <- read.csv("/home/krishna/Documents/DM/Investment-Driven-Growth/CSV/FDI_Selected(Matched).csv",header = TRUE)
gdp <- read.csv("/home/krishna/Documents/DM/Investment-Driven-Growth/CSV/GDP_Selected.csv",header = TRUE)

industries_gdp <- gdp[,1]
gdp <- gdp[,-1]
sectors_fdi <- fdi[,1]
fdi <- fdi[,-1]

fdi <- t(fdi)
gdp <- t(gdp)

file_name <- "/home/krishna/Documents/DM/Investment-Driven-Growth/Output/GDP_FDI_Model.xlsx"

for(industry in 1:length(industries_gdp)){
  fdi_relevant_sectors <- data.frame(fdi[,industry == fdi[1,]])
  info <- find_linear_relation(gdp[,industry],fdi_relevant_sectors[-1,])
  length(info$coff) = length(info$sd) #To setup NA values from unknown Cofficients
  data <- cbind(paste("",sectors_fdi[industry == fdi[1,]]),info$coff[-1],info$sd[-1])
  #row.names(data) <- seq(from = 3,to = nrow(data)+2)
  temp <- (rbind(c(paste("",industries_gdp[industry]),0,info$sd[1]),c("Intercept",info$coff[1],0)))
  data <- data.frame(rbind(temp,data))
  colnames(data) <- c("Sectors","Cofficient","Standard Deviation")
  #View(data)
  write.xlsx(data,file = file_name,append=TRUE,sheetName = paste("",industries_gdp[industry]),row.names = FALSE)
}