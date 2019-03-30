library(ggplot2)
time_series <- seq(2000,2013,by = 1)

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
  total_fdi <- rowSums(fdi_relevant_sectors[-1,])
  #graph_data <- data.frame(years = time_series, values = cbind(gdp[,industry],total_fdi))
  graph_data <- rbind(gdp[,industry],total_fdi)
  colnames(graph_data) <- time_series
  rownames(graph_data) <- c("GDP","FDI")
  View(graph_data)
  #graph <- ggplot(graph_data,aes(years,graph_data[,2])) + geom_bar(stat = "identity",colour = "black",fill = "red")
  barplot(graph_data,col=colors()[c(23,89)],border = "black",beside=T,legend=rownames(graph_data),font.lab=2, space = 0.04, font.axis = 2)
#}