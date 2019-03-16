library(rgdal)
library(geojsonio)
library(shiny)
library(readr)
library(plyr)
library(dplyr)
library(plotly)
library(tidyr)
library(threejs)
library(geosphere)
library(leaflet)
library(RColorBrewer)
library(readr)
library(leaflet.extras)
library(shinyjs)
library(rsconnect)
library(devtools)
library(ggthemes)
library(ggplot2)
library(mapview)
library(scales)
library(leaflet.minicharts)
library(shinydashboard)
library(manipulateWidget)
library(visNetwork)
library(igraph)
library(treemap)
library(viridis)
library(corrplot)
library(shinyBS)
library(shinyWidgets)
library(chorddiag)

SentCase <- function(InputString){
  InputString <-
    paste(toupper(substring(InputString,1,1)),tolower(substring(InputString,2)),
          sep="")
}

ProperCase <- function(InputString){
  sapply(lapply(strsplit(InputString," "), SentCase), paste, collapse=" ")
}

migrant <- read_csv("data/migrant_master5.csv")
remittance_totals <- read_csv("data/remittance_totals.csv")
worldshapes_data <- geojsonio::geojson_read("worldjson/countries.geo.json", what = "sp")
worldshapes <- worldshapes_data
migrant_remittance<-read_csv("data/migrant_remittance_v4.csv")
similar_countries <-read_csv("data/SimilarCountries.csv")
bilateral_bal <- read.csv("data/Bilateralbalance.csv")

colnames(migrant) <- c("year", "destination_region", "destination_sub_region", "destination_country", "destination_lat","destination_lon","destination_income_level", "destination_region_type", "destination_country_type", "origin_region","origin_sub_region","origin_country","origin_lat","origin_lon","origin_income_level","origin_region_type","origin_country_type", "gender","flag","count","destination_country_code","origin_country_code","remittance","log_remittance","migration_percentage","remittance_percentage","origin_gdppc","destination_gdppc","destination_gdp","origin_gdp","destination_population","origin_population","remittance_as_origin_gdp_pct","remittance_as_destination_gdp_pct","total_remittance_as_origin_gdp_pct")

migrant$remittance = as.numeric(migrant$remittance)
migrant$remittance_percentage = as.numeric(migrant$remittance_percentage)
migrant$log_remittance = as.numeric(migrant$log_remittance)
migrant$migration_percentage = as.numeric(migrant$migration_percentage)

migrant_data <- migrant

migrant1 <- subset(migrant, flag == "Stock" & gender == "Total")

migrant_gender <- subset(migrant, flag == "Stock" & gender != "Total")

migrant1$destination_region[migrant1$destination_region == "Latin America and the Carribean"] <- "Latin America"

migrant1$origin_region[migrant1$origin_region == "Latin America and the Carribean"] <- "Latin America"

colnames(migrant_data) <- c("year","destination_region","destination_sub_region","destination_country","destination_latitude","destination_longitude","destination_income_level","destination_region_type","destination_country_type","origin_region","origin_sub_region","origin_country","origin_latitude","origin_longitude","origin_income_level","origin_region_type","origin_country_type","gender","flag","count","destination_country_code","origin_country_code","remittance","log_remittance","migration_percentage","remittance_percentage","origin_gdppc","destination_gdppc","destination_gdp","origin_gdp","destination_population","origin_population","remittance_as_origin_gdp_pct","remittance_as_destination_gdp_pct","total_remittance_as_origin_gdp_pct")

migrant_income_dest_group = migrant1 %>% 
  dplyr::group_by(destination_income_level,year) %>% 
  dplyr::summarise(percent_income_dest = sum(count)) %>% 
  dplyr::filter(destination_income_level != "N/A") %>% 
  dplyr::filter(year!= 2017) %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(percent_income_dest = round((percent_income_dest/sum(percent_income_dest))*100,2))

migrant_income_orig_group = migrant1 %>% 
  dplyr::group_by(origin_income_level,year) %>% 
  dplyr::summarise(percent_income_orig = sum(count)) %>% 
  dplyr::filter(origin_income_level != "N/A") %>% 
  dplyr::filter(year!= 2017) %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(percent_income_orig = round((percent_income_orig/sum(percent_income_orig))*100,2))

migrant_type_dest_group = migrant1 %>% 
  dplyr::group_by(destination_country_type,year) %>% 
  dplyr::summarise(percent_type_dest = sum(count)) %>% 
  dplyr::filter(destination_country_type != "N/A") %>% 
  dplyr::filter(year!= 2017) %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(percent_type_dest = round((percent_type_dest/sum(percent_type_dest))*100,2))

migrant_type_orig_group = migrant1 %>% 
  dplyr::group_by(origin_country_type,year) %>% 
  dplyr::summarise(percent_type_orig = sum(count)) %>% 
  dplyr::filter(origin_country_type != "N/A") %>% 
  dplyr::filter(year!= 2017) %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(percent_type_orig = round((percent_type_orig/sum(percent_type_orig))*100,2))

migrant_income_dest_group <- ddply(migrant_income_dest_group, .(year),
                                   transform, pos = cumsum(percent_income_dest) - (0.5 * percent_income_dest))

migrant_income_orig_group <- ddply(migrant_income_orig_group, .(year),
                                   transform, pos = cumsum(percent_income_orig) - (0.5 * percent_income_orig))

migrant_type_dest_group <- ddply(migrant_type_dest_group, .(year),
                                 transform, pos = cumsum(percent_type_dest) - (0.5 * percent_type_dest))

migrant_type_orig_group <- ddply(migrant_type_orig_group, .(year),
                                 transform, pos = cumsum(percent_type_orig) - (0.5 * percent_type_orig))

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

plotlyfont <- list(
  family = "mono",
  size = 12,
  color = 'black')

col3 <- colorRampPalette(c("red","white","blue4"))

remittance_totals$year = as.numeric(remittance_totals$year)

migrant_continent = migrant1 %>% 
  dplyr::filter(!(year %in% c(1990,1995,2000,2005,2010))) %>% 
  dplyr::mutate(remittance = if_else(is.na(remittance), 0, remittance)) %>% 
  dplyr::group_by(origin_region,year) %>% 
  dplyr::summarise(net_remittance = sum(remittance)) %>% 
  dplyr::group_by(origin_region) %>% 
  dplyr::mutate(pct_change = (net_remittance/lag(net_remittance) - 1) * 100) %>% 
  dplyr::filter(year == 2017) 

migrant_continent[migrant_continent$pct_change < 0,"posneg"] = "Negative"
migrant_continent[migrant_continent$pct_change > 0,"posneg"] = "Positive"

similar_countries_dev = similar_countries %>% 
  filter(type == "More developed")
similar_countries_dev_comb = combn(similar_countries_dev$code,2,FUN=paste, collapse='-')
similar_countries_dev_df = data.frame(similar_countries_dev_comb)
colnames(similar_countries_dev_df) <- c("combination")
similar_countries_dev_df$i = substr(similar_countries_dev_df$combination,1,3)
similar_countries_dev_df$j = substr(similar_countries_dev_df$combination,5,8)

similar_countries_less = similar_countries %>% 
  filter(type == "Less developed")
similar_countries_less_comb = combn(similar_countries_less$code,2,FUN=paste, collapse='-')
similar_countries_less_df = data.frame(similar_countries_less_comb)
colnames(similar_countries_less_df) <- c("combination")
similar_countries_less_df$i = substr(similar_countries_less_df$combination,1,3)
similar_countries_less_df$j = substr(similar_countries_less_df$combination,5,8)

similar_countries_least = similar_countries %>% 
  filter(type == "Least developed")
similar_countries_least_comb = combn(similar_countries_least$code,2,FUN=paste, collapse='-')
similar_countries_least_df = data.frame(similar_countries_least_comb)
colnames(similar_countries_least_df) <- c("combination")
similar_countries_least_df$i = substr(similar_countries_least_df$combination,1,3)
similar_countries_least_df$j = substr(similar_countries_least_df$combination,5,8)

bilateral_bal[bilateral_bal$Region %in% c("Europe","Africa","Asia","LA","Northern America","Oceania"),"mig_type"] = "Within continent"

bilateral_bal[!(bilateral_bal$Region %in% c("Europe","Africa","Asia","LA","Northern America","Oceania")),"mig_type"] = "Cross continent"

f <- list(
  family = "Mono",
  size = 15,
  color = "#000000"
)
x <- list(
  title = "Log migrant stock from i->j, Mij",
  titlefont = f
)
y <- list(
  title = "Log migrant stock from j->i, Mji",
  titlefont = f
)

remove_diags <- function(mat, rm.lower = TRUE, ...) {
  diag(mat) <- 0
  if (isTRUE(rm.lower)) mat[lower.tri(mat)] <- 0
  mat
}

options(scipen=200)