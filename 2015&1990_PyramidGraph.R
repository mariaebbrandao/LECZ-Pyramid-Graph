setwd("/Users/mb/Documents/Baruch_IO_Master/Baruch_Graduate/Research Assistant/Deborah Balk/LECZ_Pyramid/Script/New")
#setwd("P:/WRI_LECZ2019/Analysis/Graphs and Maps/LECZ_MERIT_v1/LECZ Pyramid Graph/script/Combined 2015 & 1990")

# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("ggrepel")
# install.packages("scales")
# install.packages("forcats")
# install.packages("ggtext")
# install.packages("ggpubr")
# install.packages("cowplot")
library(cowplot)
library(ggpubr)
library(ggtext)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)
library(forcats)
library(grid)

# Uncomment lines to generate Graph for Countries that have < 100K persons OR are < 1000 km2 
# Lines: 156, 389 - 393, 483

# Uncomment lines to generate Graph for Countries that have > 100K persons AND are > 1000km2 
# Lines: 157, 396 - 400, 485

#-----------------------------------------------------------------------
# 1990 Data wrangling 
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Started with 1990 Data wrangling to get countries from 1990 to match 2015 top 50 countries  
# Used 1990 data to overlay graph in order to compare 2015 to 1990 
# Towns and Semi-Dense Areas, Cities, Rural

#data1990<-read_csv("P:/WRI_LECZ2019/Analysis/Graphs and Maps/LECZ_MERIT_v1/LECZ Pyramid Graph/data/Hasim_PrcCalculated_LECZ_stats_with_share_calculated.csv")
#delta1990<-read_csv("P:/WRI_LECZ2019/Analysis/Graphs and Maps/LECZ_MERIT_v1/LECZ Pyramid Graph/data/LECZ_delta_merit_GHSSMOD_v2 copy 2.csv")
data1990<-read_csv("/Users/mb/Documents/Baruch_IO_Master/Baruch_Graduate/Research Assistant/Deborah Balk/LECZ_Pyramid/Data/NewLECZdata/New LECZ Data To Use/Hasim_PrcCalculated_LECZ_stats_with_share_calculated.csv")
delta1990<-read_csv("/Users/mb/Documents/Baruch_IO_Master/Baruch_Graduate/Research Assistant/Deborah Balk/LECZ_Pyramid/Data/NewLECZdata/New LECZ Data To Use/Delta/LECZ_delta_merit_GHSSMOD_v2 copy.csv")

#Countries with deltas 
delta1990<-delta1990 %>% select("iso") %>% distinct()  #dropped duplicates
delta1990$delta=TRUE #create a new variable

#Merge the delta dataset to small 1990
data1990<-left_join(data1990,delta1990, by = c("iso"))

#Towns and Semi-Dense Areas, Cities, Rural
#Selecting variables I need
meritunder10m1990<-data1990%>% 
  select("iso","lecz","CountryName","UNRegion","ContinentName","LandLock","SmallIsland",
         "UrbanRural_Class","GHSSMOD_ghspop_1990","Pop10m_1990","Prc_Pop10m_1990","Prc_RuralPop10m_1990",
         "Prc_UrbanClusterPop10m_1990","Prc_UrbanCenterPop10m_1990","UrbanCenterPop10m_1990","RuralPop10m_1990",
         "UrbanClusterPop10m_1990","landArea_2015", "delta") %>% 
  group_by(iso) %>% mutate(CountryTotalPop=sum(GHSSMOD_ghspop_1990))%>% #addition to get total population per country
  filter(lecz!="Over 10 Meters") %>% filter(iso!="and") # Filtering out lecz over 10m # removed country AND- Andorra as there is no usable information 

# Test to see if observations are the same
meritunder10m1990 <-meritunder10m1990 %>% group_by(CountryName) %>% mutate(Testmean=mean(Pop10m_1990))
table(meritunder10m1990$Pop10m_1990==meritunder10m1990$Testmean) # Observations match

# Dropping duplicate observations
meritunder10m1990 <-meritunder10m1990 %>% filter(lecz!="0 to 5 Meters")

## Relable "6 to 10 Meters" to "Under10m"
meritunder10m1990 <-meritunder10m1990 %>% mutate(lecz=ifelse(lecz=="6 to 10 Meters","Under10m", lecz) )

### NA Manipulation
#Prc_RuralPop10m_1990
table(is.na(meritunder10m1990$Prc_RuralPop10m_1990)) # Tells amount of NA in variable
meritunder10m1990$Prc_RuralPop10m_1990_isna<- is.na(meritunder10m1990$Prc_RuralPop10m_1990) #Creates new variable to have record of NA
meritunder10m1990$Prc_RuralPop10m_1990[is.na(meritunder10m1990$Prc_RuralPop10m_1990)] <- 0

#Prc_UrbanClusterPop10m_1990
table(is.na(meritunder10m1990$Prc_UrbanClusterPop10m_1990)) # Tells amount of NA in variable
meritunder10m1990$Prc_UrbanClusterPop10m_1990_isna<- is.na(meritunder10m1990$Prc_UrbanClusterPop10m_1990) #Creates new variable to have record of NA
meritunder10m1990$Prc_UrbanClusterPop10m_1990[is.na(meritunder10m1990$Prc_UrbanClusterPop10m_1990)] <- 0

#Prc_UrbanCenterPop10m_1990
table(is.na(meritunder10m1990$Prc_UrbanCenterPop10m_1990)) # Tells amount of NA in variable
meritunder10m1990$Prc_UrbanCenterPop10m_1990_isna<- is.na(meritunder10m1990$Prc_UrbanCenterPop10m_1990) #Creates new variable to have record of NA
meritunder10m1990$Prc_UrbanCenterPop10m_1990[is.na(meritunder10m1990$Prc_UrbanCenterPop10m_1990)] <- 0

#Prc_Pop10m_1990
table(is.na(meritunder10m1990$Prc_Pop10m_1990)) # Tells amount of NA in variable
meritunder10m1990$Prc_Pop10m_1990_isna<- is.na(meritunder10m1990$Prc_Pop10m_1990) #Creates new variable to have record of NA
meritunder10m1990$Prc_Pop10m_1990[is.na(meritunder10m1990$Prc_Pop10m_1990)] <- 0

#delta
table(is.na(meritunder10m1990$delta)) # Tells amount of NA in variable
meritunder10m1990$delta_isna<- is.na(meritunder10m1990$delta) #Creates new variable to have record of NA
meritunder10m1990$delta[is.na(meritunder10m1990$delta)]<-0
#...End of NA manipulation

## Created new variable Prc_under10m_ghspop_1990 to add rows with corresponding Urban Rural Classes to the percentage values which were on a column 
meritunder10m1990$Prc_under10m_ghspop_1990<-ifelse(meritunder10m1990$UrbanRural_Class=="Quasi-Urban", meritunder10m1990$Prc_UrbanClusterPop10m_1990,
                                               ifelse(meritunder10m1990$UrbanRural_Class=="Urban", meritunder10m1990$Prc_UrbanCenterPop10m_1990,
                                                      meritunder10m1990$Prc_RuralPop10m_1990))

# Dropping variables (ex:Testmean as test is completed)
meritunder10m1990= subset(meritunder10m1990, select = -c(Testmean, GHSSMOD_ghspop_1990, Prc_Pop10m_1990_isna,Prc_UrbanCenterPop10m_1990_isna,Prc_RuralPop10m_1990_isna,Prc_UrbanClusterPop10m_1990_isna, Prc_RuralPop10m_1990,
                                                 Prc_UrbanClusterPop10m_1990,Prc_UrbanCenterPop10m_1990,UrbanCenterPop10m_1990,RuralPop10m_1990,UrbanClusterPop10m_1990,delta_isna) )

# Rename variables to have 1990
meritunder10m1990<- meritunder10m1990 %>% rename(lecz1990=lecz,
                                                 CountryName1990=CountryName,
                                                 UNRegion1990=UNRegion,
                                                 ContinentName1990=ContinentName,
                                                 LandLock1990=LandLock,
                                                 SmallIsland1990=SmallIsland,
                                                 CountryTotalPop1990=CountryTotalPop,
                                                 delta1990=delta)

# Reversed Rural percentage to negative numbers, so it can be on the opposite side of the graph
meritunder10m1990<-meritunder10m1990 %>% mutate(Prc_under10m_ghspop_1990=case_when(UrbanRural_Class=="Rural"~ Prc_under10m_ghspop_1990*-1,
                                                                           TRUE~ as.numeric(Prc_under10m_ghspop_1990)))


# Multiple Percentage by 100 to adjust decimal 
meritunder10m1990<-meritunder10m1990 %>% mutate(Prc_under10m_ghspop_1990=(Prc_under10m_ghspop_1990*100))%>% mutate(Prc_Pop10m_1990=(Prc_Pop10m_1990*100))

# Divide population by 1000 
meritunder10m1990<-meritunder10m1990 %>% mutate(Pop10m_1990=(Pop10m_1990/1000))

# Creating a new variable CountryName1990ds where if smallisland ==true we add * behind country name, delta ==true we add + behind country name
meritunder10m1990$CountryName1990ds= ifelse(meritunder10m1990$SmallIsland1990==TRUE & meritunder10m1990$delta1990==TRUE, paste("*+",meritunder10m1990$CountryName1990, sep=" "),
                                    ifelse(meritunder10m1990$SmallIsland1990==TRUE & meritunder10m1990$delta1990==FALSE, paste("*", meritunder10m1990$CountryName1990, sep=" "),
                                           ifelse(meritunder10m1990$delta1990==TRUE & meritunder10m1990$SmallIsland1990==FALSE, paste("+",meritunder10m1990$CountryName1990, sep=" "),
                                                  meritunder10m1990$CountryName1990)))

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 2015 Data wrangling 
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#data2015<-read_csv("P:/WRI_LECZ2019/Analysis/Graphs and Maps/LECZ_MERIT_v1/LECZ Pyramid Graph/data/Hasim_PrcCalculated_LECZ_stats_with_share_calculated.csv")
#delta2015<-read_csv("P:/WRI_LECZ2019/Analysis/Graphs and Maps/LECZ_MERIT_v1/LECZ Pyramid Graph/data/LECZ_delta_merit_GHSSMOD_v2 copy 2.csv")

data2015<-read_csv("/Users/mb/Documents/Baruch_IO_Master/Baruch_Graduate/Research Assistant/Deborah Balk/LECZ_Pyramid/Data/NewLECZdata/New LECZ Data To Use/Hasim_PrcCalculated_LECZ_stats_with_share_calculated.csv")
delta2015<-read_csv("/Users/mb/Documents/Baruch_IO_Master/Baruch_Graduate/Research Assistant/Deborah Balk/LECZ_Pyramid/Data/NewLECZdata/New LECZ Data To Use/Delta/LECZ_delta_merit_GHSSMOD_v2 copy.csv")

#Countries with deltas 
delta2015<-delta2015 %>% select("iso") %>% distinct()  #dropped duplicates
delta2015$delta=TRUE #create a new variable

#Merge the delta dataset to meritunder10m
data2015<-left_join(data2015,delta2015, by = c("iso"))

#Selecting variables I need
meritunder10m2015<-data2015%>% 
  select("iso","lecz","CountryName","UNRegion","ContinentName","LandLock","SmallIsland",
         "UrbanRural_Class","GHSSMOD_ghspop_2015","Pop10m_2015","Prc_Pop10m_2015","Prc_RuralPop10m_2015",
         "Prc_UrbanClusterPop10m_2015","Prc_UrbanCenterPop10m_2015","UrbanCenterPop10m_2015","RuralPop10m_2015",
         "UrbanClusterPop10m_2015","landArea_2015", "delta") %>% 
  group_by(iso) %>% mutate(CountryTotalPop=sum(GHSSMOD_ghspop_2015))%>% #addition to get total population per country
  filter((CountryTotalPop <= 100000) | (landArea_2015 <=1000)) %>% # Filtering In Countries that have < 100K persons or are < 1000 km2 
  #filter((CountryTotalPop >= 100000) & (landArea_2015>=1000)) %>%  # Filtering In Countries that have > 100K persons and are > 1000km2 
  filter(lecz!="Over 10 Meters")%>% filter(iso!="and") # Filtering out lecz over 10m # removed country AND- Andorra as there is no usable information 

# # Test to see number of countries in dataset 
# test<-meritunder10m2015 %>%select("iso")
# test <-distinct(test)
# dim(test) #There are 63 countries

# Test to see if observations are the same
meritunder10m2015 <-meritunder10m2015 %>% group_by(CountryName) %>% mutate(Testmean=mean(Pop10m_2015))
table(meritunder10m2015$Pop10m_2015==meritunder10m2015$Testmean) # Observations match

# Dropping duplicate observations
meritunder10m2015 <-meritunder10m2015 %>% filter(lecz!="0 to 5 Meters")

## Relable "6 to 10 Meters" to "Under10m"
meritunder10m2015 <-meritunder10m2015 %>% mutate(lecz=ifelse(lecz=="6 to 10 Meters","Under10m", lecz) )

### NA Manipulation
#Prc_RuralPop10m_2015
table(is.na(meritunder10m2015$Prc_RuralPop10m_2015)) # Tells amount of NA in variable
meritunder10m2015$Prc_RuralPop10m_2015_isna<- is.na(meritunder10m2015$Prc_RuralPop10m_2015) #Creates new variable to have record of NA
meritunder10m2015$Prc_RuralPop10m_2015[is.na(meritunder10m2015$Prc_RuralPop10m_2015)] <- 0

#Prc_UrbanClusterPop10m_2015
table(is.na(meritunder10m2015$Prc_UrbanClusterPop10m_2015)) # Tells amount of NA in variable
meritunder10m2015$Prc_UrbanClusterPop10m_2015_isna<- is.na(meritunder10m2015$Prc_UrbanClusterPop10m_2015) #Creates new variable to have record of NA
meritunder10m2015$Prc_UrbanClusterPop10m_2015[is.na(meritunder10m2015$Prc_UrbanClusterPop10m_2015)] <- 0

#Prc_UrbanCenterPop10m_2015
table(is.na(meritunder10m2015$Prc_UrbanCenterPop10m_2015)) # Tells amount of NA in variable
meritunder10m2015$Prc_UrbanCenterPop10m_2015_isna<- is.na(meritunder10m2015$Prc_UrbanCenterPop10m_2015) #Creates new variable to have record of NA
meritunder10m2015$Prc_UrbanCenterPop10m_2015[is.na(meritunder10m2015$Prc_UrbanCenterPop10m_2015)] <- 0

#Prc_Pop10m_2015
table(is.na(meritunder10m2015$Prc_Pop10m_2015)) # Tells amount of NA in variable
meritunder10m2015$Prc_Pop10m_2015_isna<- is.na(meritunder10m2015$Prc_Pop10m_2015) #Creates new variable to have record of NA
meritunder10m2015$Prc_Pop10m_2015[is.na(meritunder10m2015$Prc_Pop10m_2015)] <- 0

#delta
table(is.na(meritunder10m2015$delta)) # Tells amount of NA in variable
meritunder10m2015$delta_isna<- is.na(meritunder10m2015$delta) #Creates new variable to have record of NA
meritunder10m2015$delta[is.na(meritunder10m2015$delta)]<-0
#...End of NA manipulation

## Created new variable Prc_under10m_ghspop_2015 to add rows with corresponding Urban Rural Classes to the percentage values which were on a column 
meritunder10m2015$Prc_under10m_ghspop_2015<-ifelse(meritunder10m2015$UrbanRural_Class=="Quasi-Urban", meritunder10m2015$Prc_UrbanClusterPop10m_2015,
                                                   ifelse(meritunder10m2015$UrbanRural_Class=="Urban", meritunder10m2015$Prc_UrbanCenterPop10m_2015,
                                                          meritunder10m2015$Prc_RuralPop10m_2015))

# Dropping variables (ex:Testmean as test is completed)
meritunder10m2015= subset(meritunder10m2015, select = -c(Testmean, GHSSMOD_ghspop_2015, Prc_Pop10m_2015_isna,Prc_UrbanCenterPop10m_2015_isna,Prc_RuralPop10m_2015_isna,Prc_UrbanClusterPop10m_2015_isna, Prc_RuralPop10m_2015,
                                                         Prc_UrbanClusterPop10m_2015,Prc_UrbanCenterPop10m_2015,UrbanCenterPop10m_2015,RuralPop10m_2015,UrbanClusterPop10m_2015, delta_isna) )

# Rename variables to have 2015
meritunder10m2015<- meritunder10m2015 %>% rename(lecz2015=lecz,
                                                 CountryName2015=CountryName,
                                                 UNRegion2015=UNRegion,
                                                 ContinentName2015=ContinentName,
                                                 LandLock2015=LandLock,
                                                 SmallIsland2015=SmallIsland,
                                                 CountryTotalPop2015=CountryTotalPop,
                                                 delta2015=delta)
                                                 

# Reversed Rural percentage to negative numbers, so it can be on the opposite side of the graph
meritunder10m2015<-meritunder10m2015 %>% mutate(Prc_under10m_ghspop_2015=case_when(UrbanRural_Class=="Rural"~ Prc_under10m_ghspop_2015*-1,
                                                                                   TRUE~ as.numeric(Prc_under10m_ghspop_2015)))


# Multiple Percentage by 100 to adjust decimal 
meritunder10m2015<-meritunder10m2015 %>% mutate(Prc_under10m_ghspop_2015=(Prc_under10m_ghspop_2015*100))%>% mutate(Prc_Pop10m_2015=(Prc_Pop10m_2015*100))

# Divide population by 1000 
meritunder10m2015<-meritunder10m2015 %>% mutate(Pop10m_2015=(Pop10m_2015/1000))


# Creating a new variable CountryName2015ds where if smallisland ==true we add * behind country name, delta ==true we add + behind country name
meritunder10m2015$CountryName2015ds= ifelse(meritunder10m2015$SmallIsland2015==TRUE & meritunder10m2015$delta2015==TRUE, paste("*+",meritunder10m2015$CountryName2015, sep=" "),
                                        ifelse(meritunder10m2015$SmallIsland2015==TRUE & meritunder10m2015$delta2015==FALSE, paste("*", meritunder10m2015$CountryName2015, sep=" "),
                                               ifelse(meritunder10m2015$delta2015==TRUE & meritunder10m2015$SmallIsland2015==FALSE, paste("+",meritunder10m2015$CountryName2015, sep=" "),
                                                      meritunder10m2015$CountryName2015)))
# Arrange data in descending 
meritunder10m2015<- meritunder10m2015 %>% arrange(desc(Prc_Pop10m_2015)) 

# Create subset data to have top 50 countries only
# Selected 1:150 because observations are in sets of 2
top50countries2015<-(top50countries2015<- meritunder10m2015[1:150, ])

# Check if there are 50 countries
test<-top50countries2015 %>%select("iso")
test <-distinct(test)
dim(test)

# Merge 1990 to 2015 to pick out the top 50 countries from 2015 in the 1990 data set, and drop all other countries in 1990 that are not part of 2015
top50countries20151990<- left_join(top50countries2015,meritunder10m1990, by = c("iso","UrbanRural_Class"))

#Check if there are 50 countries
# test2<-top50countries20151990 %>%select("iso")
# test2 <-distinct(test2)
# dim(test2)

top50countries20151990<- top50countries20151990 %>% arrange(desc(Prc_Pop10m_2015)) 

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------            
# 2015 Plot and Dimensions 
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------            

#Bar plot & dimensions
#To order by population = aes(x=fct_reorder(CountryName,Pop10m_2015)
#To order by percentage = aes(x=fct_reorder(CountryName,Prc_under10m_ghspop_2015)


graph2015<-ggplot(top50countries20151990,
                  aes(x= fct_reorder(CountryName2015ds, Prc_Pop10m_2015, .desc = FALSE),
                      y=Prc_under10m_ghspop_2015))+
  
  geom_bar(data=subset(top50countries20151990, UrbanRural_Class=="Rural"),
           stat="identity",alpha=1, width = .4,
           position = "dodge",
           aes(fill=factor(UrbanRural_Class, levels=c("Rural"))))+
  
  geom_bar(data=subset(top50countries20151990,UrbanRural_Class=="Quasi-Urban" | UrbanRural_Class=="Urban" ),
           stat="identity",alpha=1, width = .7,
           position = "dodge",
           aes(fill=factor(UrbanRural_Class, levels=c("Quasi-Urban","Urban"))))+ 
  
  #Changes colors to red and orange
  scale_fill_manual(breaks = c("Rural","Quasi-Urban", "Urban"), values = c("chartreuse3","gold","red3"),labels=c("Rural","Towns and Semi-Dense Areas","Cities"))+
  geom_hline(yintercept=0, colour="white", lwd=1.5) +guides( color=FALSE)+ #this is the white line in between/dividing Rural and Urban bars 
  
  
  #Left number labeling on graph (Population under 10m= Pop10m_2015)
  geom_text_repel(
    data=subset(top50countries20151990, UrbanRural_Class=="Rural"),
    aes(label=number(Pop10m_2015,accuracy = 1L, big.mark = ",")),
    colour="blue3",
    size=18,
    direction= "y",
    nudge_y = -200,
    segment.size = 0,
    segment.color = "transparent",
    force=0,
    box.padding = 1.5 #adds space between numbers and country names
  )+
  
  geom_text_repel(
    data=subset(top50countries20151990, UrbanRural_Class=="Quasi-Urban"),
    aes(label=number(Pop10m_2015,accuracy = 1L, big.mark = ",")),
    colour="blue3",
    size=18,
    direction= "y",
    nudge_y = -300,
    segment.size = 0,
    segment.color = "transparent",
    force=0,
    box.padding = 1.5 #adds space between numbers and country names
  )+
  
  geom_text_repel(
    data=subset(top50countries20151990, UrbanRural_Class=="Urban"),
    aes(label=number(Pop10m_2015,accuracy = 1L, big.mark = ",")),
    colour="blue3",
    size=18,
    direction= "y",
    nudge_y = -300,
    segment.size = 0,
    segment.color = "transparent",
    force=0,
    box.padding = 1.5 #adds space between numbers and country names
  )+
  
  #Right number labeling on graph (Percentage of Total Population under 10m= Prc_Pop10m_2015)
  geom_text_repel(
    data=subset(top50countries20151990, UrbanRural_Class=="Rural"),
    aes(label=round(Prc_Pop10m_2015, digits = 1)),
    colour="blue3",
    size=18,
    direction= "y",
    nudge_y = 300,
    segment.size = 0,
    segment.color = "transparent",
    force=0
  )+
  
  geom_text_repel(
    data=subset(top50countries20151990, UrbanRural_Class=="Urban"),
    aes(label=round(Prc_Pop10m_2015, digits = 1)),
    colour="blue3",
    size=18,
    direction= "y",
    nudge_y = 300,
    segment.size = 0,
    segment.color = "transparent",
    force=0
  )+
  
  geom_text_repel(
    data=subset(top50countries20151990, UrbanRural_Class=="Quasi-Urban"),
    aes(label=round(Prc_Pop10m_2015, digits = 1)),
    colour="blue3",
    size=18,
    direction= "y",
    nudge_y = 300,
    segment.size = 0,
    segment.color = "transparent",
    force=0
  )+
  
  #Plot labels   
  labs(y=" 
       % of population in 10m LECZ",
       x=" ",
       fill= " ",
       subtitle= #play with dashes as you switch from graph with more or less to adjust the spacing on the top titles
         "<b> 
       <span style='color:blue;'> Population (1000)</span>
       <span style='color:white;'>__________________________________________________________________________</span> 
       <span style='color:blue;'>% of Total Population </span>
       <br>
       <span style='color:white;'>_</span>
       <span style='color:blue;'> in 10m LECZ</span> 
       <span style='color:white;'>___________________________</span>
       <span style='color:black;'> -Rural-</span>
       <span style='color:white;'>____________________________</span>
       <span style='color:black;'>-Urban-</span>
       <span style='color:white;'>______________________</span>
       <span style='color:blue;'> in 10m LECZ</span></b>",
  #---------------  
  # Uncomment for Countries that have < 100K persons OR are < 1000km2
  
        caption ="\n Top 50 Countries with Less than 100k Persons or 1000 sqkm Ordered by Percentage of Total Population Under 10m, 2015
                Countries are sorted by percentage of total population in 10m LECZ
                                                                                                                                                     Black lines represent 1990 data set
                                                                                                                                                                            +Countries with Delta
                                                                                                                                                                               *Small Island States ")+
#---------------  
  # Uncomment for Countries that have > 100K persons and are > 1000km2

#caption ="\n Top 50 Countries with More than 100k Persons and 1000 sqkm Ordered by Percentage of Total Population Under 10m, 2015
 #                Countries are sorted by percentage of total population in 10m LECZ
  #                                                                                                                                                    Black lines represent 1990 data set
   #                                                                                                                                                                          +Countries with Delta
    #                                                                                                                                                                            *Small Island States ")+
#-----------  
  scale_y_continuous(breaks = c(-100,-80,-60,-40,-20,0,20,40,60,80,100),
                     labels= c("100%","80%","60%","40%","20%","0", "20%","40%","60%","80%","100%"))+  
  
  scale_x_discrete(labels = function(y) str_wrap(y, width = 38.9))+ # adjusts the spacing between long names by placing a word on the bottom of the other 
  
  coord_flip(ylim=c(-100 , 100))+
  
  theme_minimal()+
  
  theme(
    plot.title=element_text(color="grey7", size=50),
    plot.subtitle  = element_markdown( size = 50),
    axis.title.x = element_text(colour = "grey7",face="bold",size=50),
    axis.title.y = element_text(colour = "grey7",face="bold",size=50, lineheight = .9), #lineheight adds space
    axis.text.y = element_text(colour = "black",face="bold",size=50),
    #axis.text.y = element_text(colour = colorvec2,face="bold",size=50), In case we use the vectors for color, lines 118-124
    axis.text.x = element_text(color = "black",face="bold",size=50,vjust = -.5), #vjust adds space
    panel.border = element_rect(colour = "black", fill=NA, size=0.3),
    plot.caption=element_text(color="black",hjust = -0.2, face="bold", size=50,margin = margin(0,5,40,5)), 
    legend.spacing.x = unit(1.5,"cm"),
    legend.margin = margin(20,5,40,5), #margin add space around the words
    legend.key.height  = unit(2,"cm"),
    legend.key.width  = unit(2,"cm"), 
    legend.text=element_text(size=50),
    legend.position="bottom")

# set the dimensions (and gets rid of grid viewport error) 
dev.new(width = 500, height = 500, unit = "in")

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------            
#1990 Plot and Dimensions 
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------            
names(top50countries20151990)

graph1990<-ggplot(top50countries20151990,
                  aes(x= fct_reorder(CountryName1990ds, Prc_Pop10m_2015, .desc = FALSE),
                      y=Prc_under10m_ghspop_1990))+
  
  geom_bar(data=subset(top50countries20151990, UrbanRural_Class=="Rural"),
           stat="identity",alpha=1, width = .04,
           position = position_dodge(width = 0.7), #add space between grouped bars (within a group, like Quasi-Urban & Urban)
           aes(fill=factor(UrbanRural_Class, levels=c("Rural"))))+
  
  geom_bar(data=subset(top50countries20151990,
                       UrbanRural_Class=="Quasi-Urban" | UrbanRural_Class=="Urban" ),
           stat="identity",alpha=1, width = .1,
           position = position_dodge(width = 0.7), #add space between grouped bars (within a group, like Quasi-Urban & Urban)
           aes(fill=factor(UrbanRural_Class, levels=c("Quasi-Urban","Urban"))))+ 
  
  #Changes colors to red and orange
  scale_fill_manual(breaks = c("Rural","Quasi-Urban", "Urban"), values = c("black","black","black"),labels=c("Rural","Towns and Semi-Dense Areas","Cities"))+
  geom_hline(yintercept=0, colour="white", lwd=1.5) +guides( color=FALSE)+
  
  #flips graph coordination
  coord_flip(ylim=c(-100, 100))+
  
  theme_transparent()+
  
  theme(legend.position="none")+
  
  # set the dimensions (and gets rid of grid viewport error) 
  dev.new(width = 500, height = 500, unit = "in")

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------            
# Combining/Overlaying the graphs 2015 and 1990
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------            

# Make the plotting area the same size - (area inside the x and y axis) to be the same for multiple graphs
aligned_plots <- align_plots(graph2015, graph1990, align="hv", axis = "tblr")

#Place the plots into the same figure
test1<-ggdraw()+
  draw_plot(aligned_plots[[1]]) +
  draw_plot(aligned_plots[[2]])


#setwd("P:/WRI_LECZ2019/Analysis/Graphs and Maps/LECZ_MERIT_v1/LECZ Pyramid Graph/output_graph/2015&1990_Combined")
setwd("/Users/mb/Documents/Baruch_IO_Master/Baruch_Graduate/Research Assistant/Deborah Balk/LECZ_Pyramid/Graphical Output/New Data Set/MoreThan_100K people or are More Than 1000km2")

# To save Graph with Countries that have < 100K persons OR are < 1000km2
ggsave(test1, filename=paste("Less_Top50Countries" ,".pdf"),  width = 55, height = 80, dpi=200, limitsize = FALSE)

# To save Graph with Countries that have > 100K persons And are > 1000km2
#ggsave(test1, filename=paste("More_Top50Countries" ,".pdf"),  width = 55, height = 80, dpi=200, limitsize = FALSE)

