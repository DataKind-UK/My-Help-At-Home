
local.authority = read.table("local_authority_to_homes_agencies_hospitals.csv", header=T, sep=',')

# Plotting Distribution of 
local.authority%>% gather(variable, number, -Local.Authority) %>% ggplot(aes(x=variable, y=number)) + geom_boxplot()
local.authority%>% gather(variable, number, -Local.Authority) %>% group_by(variable) %>% summarise(sum(number))




care.local = local.authority %>% left_join(care.costs, by=c("Local.Authority"="District"))

# Plotting Pairs graph between care.costs and local.authority, to see if there is correlation. Requires care.local df
pairs(local_and_costs %>% select(-Local.Authority, -Postcode))


