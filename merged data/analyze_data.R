
library(dplyr)
library(tidyr)
library(ggplot2)
merged.all = read.table("merged.table.csv", sep='\t', header=T)


#pcadata = merged.all %>% subset(Year=="2011/12") 

pcadata = merged.all %>% subset(!duplicated(paste(Local.authority, Year))) 
row.names(pcadata) = paste(pcadata$Local.authority, pcadata$Year)
pcadata.matrix = scale(na.omit(pcadata %>% select(-Local.authority, -County, -Year, -Applications)))

# Principal Component
#biplot(princomp(na.omit(merged.all %>% select(-Local.authority, -County, -Year, -Applications, -applications.succeed, -applications.declined, -TotalGranted))))

pdf("pca_analysis.pdf")
biplot(princomp(pcadata.matrix))


dev.off()

