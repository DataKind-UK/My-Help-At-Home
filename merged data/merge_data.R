library(dplyr)
library(tidyr)
library(ggplot2)


authority.mapping = read.delim("raw/ASCOF Area Mapping MANY TO ONE RELATIONSHIPS.csv", sep=",") %>% select(CASSR.Code: County)

census = read.delim("raw/LTCensusDataClean.csv", sep=",")

census.filtered = census %>% 
    select(Local.authority, ends_with("2011")) %>% 
    select(Local.authority, Usual.residents.2011, Sex.ratio.2011, Median.age.2011, starts_with("Percentage") , starts_with("Average."), starts_with("Population.density")) %>%
    subset(Local.authority %in% authority.mapping$Local.authority) %>% 
    mutate(Local.authority = 
        ifelse(grepl(" and Chester", Local.authority), "Chester",
        ifelse(Local.authority == "Kingston upon Hull, City of UA", "Kingston upon Thames",
        as.character(Local.authority)))) %>%

    subset(complete.cases(.))   # 326 Local Authorities!

census.filtered.all = rbind(census.filtered %>% mutate(Year="2010/2011"), census.filtered %>% mutate(Year="2011/2012"), census.filtered %>% mutate(Year="2012/2013"), census.filtered %>% mutate(Year="2013/2014"), census.filtered %>% mutate(Year="2014/2015")) 
census.filtered.all %>% write.table("clean/census.clean.csv", row.names=F)



total_agencies.raw = read.table("raw/local_authority_to_homes_agencies_hospitals.csv", header=T, sep=',')

total_agencies = total_agencies.raw %>% 
    mutate(Local.Authority=
        ifelse(Local.Authority=="Bristol, City of",     "Bristol", 
        ifelse(Local.Authority=="Cheshire West and Chester UA",     "Chester", 
        ifelse(grepl("West and Chester", Local.Authority),     "Chester", 
        ifelse(Local.Authority=="County Durham",     "Durham", 
        ifelse(Local.Authority=="Herefordshire, County of",     "Herefordshire", 
        ifelse(Local.Authority=="Kingston upon Hull, City of",     "Kingston upon Thames", 
        ifelse(Local.Authority=="Medway",     "Medway Towns", 
        ifelse(Local.Authority=="Telford and Wrekin",     "Telford and the Wrekin",
        as.character(Local.Authority)))))))))) 


# intersect total_agencies with Authority Codes, divide each County by number of Authorities
total_agencies = total_agencies %>% 
    left_join(authority.mapping, by=c("Local.Authority"="CASSR.Name")) %>% 
    group_by(Local.Authority) %>% 
    mutate_each(funs(./n()), Homes:Hospitals) %>% 
#    select (  Local.authority=Local.Authority, Homes, Agencies, Hospitals)  
    select (  Local.authority, Homes, Agencies, Hospitals)  

total_agencies = rbind (
    total_agencies %>% mutate(Year="2010/2011"),
    total_agencies %>% mutate(Year="2011/2012"),
    total_agencies %>% mutate(Year="2012/2013"),
    total_agencies %>% mutate(Year="2013/2014"),
    total_agencies %>% mutate(Year="2014/2015")
    )

total_agencies %>% write.table("clean/total.agencies.clean.csv", row.names=F)


##census.filtered %>% left_join(total_agencies) %>% subset(!complete.cases(.)) %>% print
#merged.data = census.filtered %>% left_join(total_agencies) %>% subset(complete.cases(.)) 
#merged.data %>% nrow %>% print





grants.raw = read.table("raw/health_grants.csv", sep=";", header=T) %>% subset(Location!="England")

#grants %>% left_join(authority.mapping, by=c("Location"="CASSR.Name")) %>% subset(is.na(Local.authority)) %>% select(Location) %>% print.data.frame(row.names=F)
grants = grants.raw %>% 
    left_join(authority.mapping, by=c("Location"="CASSR.Name")) %>% 
    group_by(Location, Year) %>% 
    mutate(TotalGranted=1000*TotalGranted/n())  %>%
    select(-CASSR.Code)
grants %>% write.table("clean/grants.clean.csv", row.names=F)




# Quality Data
quality = read.table("raw/QualityTbl.csv", sep=",", header=T) %>% 
    select(-CASSR.NAME) %>% 
    left_join(authority.mapping, by=c("CASSR.CODE"="CASSR.Code")) %>%
    select(Local.authority, Year=FISCAL.YEAR, QUALITY) 

quality %>% write.table("clean/QualityTbl.clean", row.names=F)

#    group_by(CASSR.CODE, FISCAL.YEAR) %>% mutate(QUALITY=as.numeric(QUALITY)/n()) 


cost.raw = read.table("raw/CostTable.csv", sep=',', header=T) %>%
    subset(!grepl("England", X1314)) %>% 
    separate(X1314, into=c("CASSR.Code", "CASSR.Name"), sep="-", extra="merge")  


cost = cost.raw %>% 
    mutate(CASSR.Code=as.integer(CASSR.Code)) %>%  
    left_join(authority.mapping, by="CASSR.Code")  %>%
    select(Local.authority, Year=Fiscal.Year, cost)


cost %>% write.table("clean/average_costs.clean", row.names=F)



# Ratings
ratings = read.table("raw/Ratings2.csv", header=T, sep=",") %>%
    select(CASSR.Code, CASSR.Name, Outcome, Year=End.Year) %>% 
    left_join(authority.mapping) 
ratings %>% write.table("clean/ratings.clean.csv", row.names=F)



ratings = ratings %>% left_join(authority.mapping) 


app.total  = read.table("raw/total_aps_la_year.csv", header=F, col.names=c("Location", "Year", "Applications"), sep=",") %>%
    left_join(authority.mapping, by=c("Location"="CASSR.Name")) %>% 
    group_by(Location, Year) %>% 
    mutate(Applications = Applications/n())  %>% 
    select(Local.authority, Year, Applications) 
#    mutate(Local.authority=gsub(" UA", "", Local.authority))

#applications.accepted  = read.table("raw/PublicHealthGrantsToLocalAuthoritiesPerYear.csv", header=T, sep=";") %>%
#    left_join(authority.mapping, by=c("Location"="CASSR.Name")) %>% 
#    group_by(Location, Year) %>% 
#    mutate_each(funs(./n()), TotalGranted)  %>%
#    select(Local.authority, BaseLinePerHead, TotalGranted)

app.succ = read.table("raw/total_succeed_la_year.csv", header=F, sep=",", col.names=c("CASSR.Name", "Year", "applications.succeed")) %>% 
    left_join(authority.mapping, by=c("CASSR.Name")) %>% 
    group_by(CASSR.Name) %>% 
    mutate(applications.succeed/n()) %>% 
    ungroup %>% 
    select(Local.authority, Year, applications.succeed)

app.declined = read.table("raw/total_declined_la_year.csv", header=F, sep=",", col.names=c("CASSR.Name", "Year", "applications.declined")) %>% 
    left_join(authority.mapping, by=c("CASSR.Name")) %>% 
    group_by(CASSR.Name) %>% 
    mutate(applications.declined/n()) %>% 
    ungroup %>% 
    select(Local.authority, Year, applications.declined)



applications = app.total %>% 
    left_join(app.succ) %>% 
    left_join(app.declined) %>%
    ungroup %>% 
    mutate(Year=gsub ("(\\d\\d)(\\d\\d)", "20\\1/\\2", as.character(Year), perl=T))



app.total %>% write.table("clean/applications.csv", row.names=F)


health = read.table("raw/2011_health_provision.csv", sep=",", header=T)


c1t = read.table("raw/C1cTable.csv", sep=",", header=T) %>% left_join(authority.mapping, by=c("CASSR.NAME"="CASSR.Code") ) %>% select(Local.authority, Year=FISCAL.YEAR, X2C1C)
c2t = read.table("raw/C2cTable.csv", sep=",", header=T) %>% left_join(authority.mapping, by=c("CASSR.NAME"="CASSR.Code") ) %>% select(Local.authority, Year=FISCAL.YEAR, X2c2)


merged.all = census.filtered %>% 
    left_join(cost) %>% 
    left_join(quality) %>% 
    left_join(applications) %>% 
    left_join(applications.accepted) %>%
    left_join(health, by=c("Local.authority"="LA_name")) %>%
    left_join(total_agencies %>% select(-Local.Authority, -Year) %>% mutate(Local.authority = as.character(Local.authority)) %>% distinct) %>%
    left_join(c1t) %>%
    left_join(c2t) %>% 
    subset(Year %in% c("2011/12", "2012/13", "2013/14"))  %>%
    mutate_each(funs(as.numeric(gsub(",", ".", .))), -Local.authority, -Year) %>%select(-Location)  %>%
    select(-Local.Authority) %>%
    select(Local.authority, Year, matches(".")) %>%
    unique


merged.all %>% write.table("merged.table.csv", row.names=F, sep="\t")
merged.all %>% glimpse
 
