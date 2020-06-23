library(acs)
library(leaflet)
library(tigris)
library(tidyverse)
library(rgdal)

api.key.install("a6aada134f2ac38827cb802c68d35ddaf8401a7f")
lookup_code("VA", "Arlington") # 05000US51013
geo <- geo.make(state=51, county=13, tract="*", block.group = "*")

arl_block <- block_groups("51", "13")
plot(arl_block)

# Tables: B19001, B03002, Table B25024

# Population B01003
pop <- acs.fetch(endyear = 2018, geography = geo, table.number = "B01003", col.names = "pretty")
pop@geography$county <- as.character("51")
pop@geography$county <- as.character("013")

pop_df <- data.frame(paste0(pop@geography$state,
                            pop@geography$county, 
                            pop@geography$tract,
                            pop@geography$blockgroup), 
                     pop@estimate)
colnames(pop_df)[1] <- "GEOID"



### INCOME
income <- acs.fetch(endyear = 2018, geography = geo, table.number = "B19001", col.names = "pretty")
income@geography$county <- as.character("51")
income@geography$county <- as.character("013")

income_df <- data.frame(paste0(income@geography$state,
                               income@geography$county, 
                               income@geography$tract,
                               income@geography$blockgroup), 
                        income@estimate)
income_df$Inc.under75k <-  rowSums(income_df[,3:13])
income_df$Inc.75to200k <-  rowSums(income_df[,14:17])
income_df$Inc.over200k <-  income_df[,18]

income_df <- select(income_df, c(1, 2, 19:21))
colnames(income_df) <- c("GEOID", "Total", "Inc.under75k", "Inc.75to200k", "Inc.over200k")
rownames(income_df)<-1:nrow(income_df)




# Hispanic 
Hispanic <- acs.fetch(endyear = 2018, geography = geo, table.number = "B03002", col.names = "pretty")
Hispanic@geography$county <- as.character("51")
Hispanic@geography$county <- as.character("013")

Hispanic_df <- data.frame(paste0(Hispanic@geography$state,
                                 Hispanic@geography$county, 
                                 Hispanic@geography$tract,
                                 Hispanic@geography$blockgroup), 
                          Hispanic@estimate)
Hispanic_df <- select(Hispanic_df, c(1, 2, 13))
colnames(Hispanic_df)[1] <- "GEOID"
rownames(Hispanic_df)<-1:nrow(Hispanic_df)


# Race Table B02001
Race <- acs.fetch(endyear = 2018, geography = geo, table.number = "B02001", col.names = "pretty")
Race@geography$county <- as.character("51")
Race@geography$county <- as.character("013")

Race_df <- data.frame(paste0(Race@geography$state,
                             Race@geography$county, 
                             Race@geography$tract,
                             Race@geography$blockgroup), 
                      Race@estimate)
Race_df$nonwhite <- rowSums(Race_df[,4:11])
Race_df$BIPOC <- rowSums(Race_df[,c(4, 5, 7)])
colnames(Race_df)[1] <- "GEOID"


# Type of Home Table B25024
Home <- acs.fetch(endyear = 2018, geography = geo, table.number = "B25024", col.names = "pretty")
Home@geography$county <- as.character("51")
Home@geography$county <- as.character("013")

Home_df <- data.frame(paste0(Home@geography$state,
                             Home@geography$county, 
                             Home@geography$tract,
                             Home@geography$blockgroup), 
                      Home@estimate)
Home_df$SFH <- rowSums(Home_df[,3:4])
Home_df$multifamily <- rowSums(Home_df[,5:10])

colnames(Home_df)[1] <- "GEOID"



# Merge Data
m1 <- merge(pop_df, income_df, by = "GEOID")
m2 <- merge(m1, Hispanic_df, by = "GEOID")
m3 <- merge(m2, Race_df, by = "GEOID")
m4 <- merge(m3, Home_df, by = "GEOID")

m4$Pct.multifamily <- 100 * (m4$multifamily / m4$"Units.in.Structure..Total.")
m4$Pct.under75k <- 100 * (m4$Inc.under75k / m4$"Total.Population..Total")
m4$Pct.75to200k <- 100 * (m4$Inc.75to200k / m4$"Total.Population..Total") 
m4$Pct.over200k <- 100 * (m4$Inc.over200k / m4$"Total.Population..Total")
m4$Pct.Hispanic <- 100 * (m4$"Hispanic.or.Latino.by.Race..Hispanic.or.Latino."/ m4$"Total.Population..Total")
m4$Pct.nonwhite <- 100 * (m4$nonwhite / m4$"Total.Population..Total")
m4$Pct.BIPOC <- 100 * (m4$BIPOC / m4$"Total.Population..Total")
m5 <- select(m4, c(1, 2, 34:40))
m5 <- m5[ !(m5$GEOID %in% c(510139802001, 510139801001)), ]



arl_merged <- geo_join(arl_block, m5, "GEOID", "GEOID")
writeOGR(arl_merged, "/Users/alexhendel/Desktop/Arlington GIS/R Code Files", 
         "Arlington", driver="ESRI Shapefile", overwrite_layer=TRUE, check_exists=TRUE)


# Leaflet
pal <- colorQuantile("Greens", NULL, n = 4)
popup <- paste0("Percent Multifamily: ", as.character(arl_merged$Pct.multifamily))

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = arl_merged, 
              fillColor = ~pal(arl_merged$Pct.multifamily),
              fillOpacity = 0.7,
              weight = 0.2,
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = arl_merged$Pct.multifamily, 
            position = "bottomright", 
            title = "Percent Multifamily")

