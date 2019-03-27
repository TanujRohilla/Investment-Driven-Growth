fdi <- read.csv("/home/krishna/Documents/DM/Investment-Driven-Growth/CSV/FDI.csv",header = TRUE)
industries <- fdi[,1] #First Column contains name of Industry
fdi <- fdi[,-1]

exchange_rate <- read.csv("/home/krishna/Documents/DM/Investment-Driven-Growth/CSV/Exchange_Rate.csv",header = TRUE)
new_fdi <- data.frame()

#Changing Units
for(i in 1:nrow(fdi)){
  new_fdi <- rbind(new_fdi,fdi[i,]*exchange_rate[,2]*0.1) #1 Million = 0.1 Crores
}

new_fdi <- cbind(industries,new_fdi)
column_names <- c("Sector","2000-01","2001-02","2002-03","2003-04","2004-05","2005-06","2006-07","2007-08","2008-09","2009-10","2010-11","2011-12","2012-13","2013-14","2014-15","2015-16","2016-17")
colnames(new_fdi) <- column_names

write.csv(new_fdi,file = "/home/krishna/Documents/DM/Investment-Driven-Growth/CSV/FDI_INR.csv",row.names = FALSE)