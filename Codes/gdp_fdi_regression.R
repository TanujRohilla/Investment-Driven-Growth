library(xlsx)

find_linear_relation <- function(gdp,fdi_values){
  combined_data <- (cbind(y = gdp,x = fdi_values))
  
  standard_deviation <- apply(combined_data, 2,sd)  #Finding Standard Deviation of Each Column
  combined_data <- as.data.frame(scale(combined_data))
  
  linear_model <- lm(y~.,data = combined_data)
  coff <- c(NA,summary(linear_model)$coefficients[,1])
  sd <- c(standard_deviation[1],NA,standard_deviation[2:length(standard_deviation)])
  data <- list(cofficient = coff,standard_deviation = sd)
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

industry <- 3
for(industry in 1:length(industries_gdp)){
  fdi_relevant_sectors <- data.frame(fdi[,industry == fdi[1,]])
  info <- find_linear_relation(gdp[,industry],fdi_relevant_sectors[-1,])
  
  length(info$cofficient) = length(info$standard_deviation) #To setup NA values from unknown Cofficients
  actual_cofficient <- (info$cofficient*info$standard_deviation[1])/info$standard_deviation

  sectors <- c(paste("",industries_gdp[industry]),"Intercept",paste("",sectors_fdi[industry == fdi[1,]]))
  data <- data.frame(sectors,info$cofficient,actual_cofficient,info$standard_deviation,stringsAsFactors = FALSE)
  colnames(data) <- c("Sectors","Estimated Cofficient","Actual Cofficients","Standard Deviation")
  
  write.xlsx(data,file = file_name,append=TRUE,sheetName = paste("",industries_gdp[industry]),row.names = FALSE)
}