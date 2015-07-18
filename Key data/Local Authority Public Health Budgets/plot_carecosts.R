library(ggplot2)
library(tidyr)
library(dplyr)

care.costs = read.delim("care_costs_by_district.csv", sep=",")
pdf("care_costs.pdf")

# plot comparison residential vs at home cost
care.costs %>% gather(variable, cost, -District, -Postcode) %>% group_by(variable) %>% ggplot(aes(x=variable, y= cost)) + geom_boxplot() + ggtitle("Cost of Residential Care vs At Home Care")

# plot comparison of residential vs at home by the first three letters of postcode
care.costs %>% gather(variable, cost, -District, -Postcode) %>% separate(Postcode, into=c("postcode1", "postcode2"), sep=" ") %>% group_by(variable) %>% ggplot(aes(x=variable, y= cost)) + geom_boxplot() + ggtitle("Cost of Residential Care vs At Home Care") + facet_wrap(~postcode1)



dev.off()

