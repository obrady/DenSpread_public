################################################################################
# Plots maps of sources of invasions of key cities and overall major contributors
################################################################################

# Oliver Brady
# 14 July 2022

rm(list = ls())

require(sf)
require(sp)
require(tmap)
require(rgdal)
require(raster)
require(ggplot2)
require(exactextractr)
require(ggrepel)
require(ggsn)
require(RColorBrewer)
require(tidyverse)
library(igraph)


# working directory
setwd("/Users/eideobra/Dropbox/08_Serotype_spread/")
# setwd("/Users/Vinyas/OneDrive/08_Serotype_spread/")

country <- "Brazil"

# admin files
if(country == "Brazil"){
  # summary details of invaded cities
  Big_cities <- read.csv("Data/Intermediate_datasets/Brazil_big_cities.csv")
  Big_cities_future <- read.csv("Predictions/Bra-Fig4-Top10FutureInfected-GAULsJan24.csv")
  #Big_cities <- rbind(Big_cities, Big_cities_future)
  
  # load in detailed matrices for cities (past and future respectively)
  load("Data/Intermediate_datasets/Retrospective_pred/Big_city_sources.RData")
  load("Predictions/Bra-Fig4-Top10FutureInfected-MobMatricesJan24.RData")
  # process future list of consistency
  Big_city_con_list_future <- list()
  for(i in 1:length(bigCityMobilityMatrices)){Big_city_con_list_future[[i]] = bigCityMobilityMatrices[[i]][[1]]}
  
  # load in normalisation values -step required here to make movement covariates comparable
  load("Data/Intermediate_datasets/Retrospective_pred/Covariate_summaries_maxlink.RData")
  
  # admin
  admin0 <- st_read(dsn = "Reference/Admin0(2011)/admin0.shp", layer = "admin0")
  admin0 <- admin0[admin0$COUNTRY_ID %in% c("BRA", "PRY", "ARG", "URY"), ]
  admin1 <- st_read(dsn = "Reference/Admin1(2011)/admin1.shp", layer = "admin1")
  admin2 <- st_read(dsn = "Reference/Admin2(2011)/admin2.shp", layer = "admin2")
  # trim to Brazil
  admin1 = admin1[admin1$COUNTRY_ID == "BRA", ]
  admin2 = admin2[admin2$COUNTRY_ID == "BRA", ]
  
  # add a name to big cities then combine
  Big_cities_future$Name = admin2$NAME[match(Big_cities_future$GAUL, admin2$GAUL_CODE)]
  Big_cities_future$Year = Big_cities_future$YearInfected
  Big_cities = rbind(Big_cities[, c("Name", "GAUL", "Year")],
                     Big_cities_future[, c("Name", "GAUL", "Year")])
  
  # load original connectivity matrices for relevant movement variables
  # c) Radiation model
  load("Data/Intermediate_datasets/Radiation_matrix_Brazil.RData")
  # D) Adjacency model
  load("Data/Intermediate_datasets/dist_mat_Adjacency_Brazil.RData")
  # E) flight data
  load("Data/Intermediate_datasets/Brazil_air_move_mat_ad2.RData")
  # F) Worldpop internal migration flows (state level)
  load("Data/Intermediate_datasets/Brazil_Migration_mat.RData")
  # G) MAP friction surface- travel time between biggest city in each municipality
  load("Data/Intermediate_datasets/Brazil_Friction_mat.RData")
  # Friction transformation (emphasises accessible places)
  dist_mat_Friction = 1 / (dist_mat_Friction + 1)
  
  # load in original spread data and future projections and convert to a table of estimated invasion year
  #bra_OD <- read.csv("Data/Brazil_mun_dat_cleaned_thresholded.csv")
  dat_bra <- read.csv("Predictions/Bra-Fig1-ContemporaryPredsJan24.csv")
  dat_bra = dat_bra[!is.na(dat_bra$YearPredicted), ]
  FP_Bra <- read.csv("Predictions/Bra-Fig3-FuturePredsJan24.csv")
  FP_Bra = FP_Bra[!(FP_Bra$GAUL %in% dat_bra$GAUL), ]
  MunArrival <- data.frame(GAUL = c(dat_bra$GAUL, FP_Bra$GAUL),
                           YearInvaded = c(dat_bra$YearPredicted, FP_Bra$YearInfected))
  
}

if(country == "Mexico"){
  # summary details of invaded cities
  Big_cities <- read.csv("Data/Intermediate_datasets/Mexico_big_cities.csv")
  Big_cities_future <- read.csv("Predictions/Mex-Fig4-Top10FutureInfected-GAULsJan24.csv")
  #Big_cities <- rbind(Big_cities, Big_cities_future)
  
  # load in detailed matrices for cities (past and future respectively)
  load("Data/Intermediate_datasets/Retrospective_pred/Big_city_sources_Mexico.RData")
  load("Predictions/Mex-Fig4-Top10FutureInfected-MobMatricesJan24.RData")
  # process future list of consistency
  Big_city_con_list_future <- list()
  for(i in 1:length(bigCityMobilityMatrices)){Big_city_con_list_future[[i]] = bigCityMobilityMatrices[[i]][[1]]}
  
  
  # load in normalisation values -step required here to make movement covariates comparable
  load("Data/Intermediate_datasets/Retrospective_pred/Covariate_summaries_Mexico_maxlink.RData")
  
  # admin shapefile
  admin0 <- st_read(dsn = "Reference/Admin0(2011)/admin0.shp", layer = "admin0")
  admin0 <- admin0[admin0$COUNTRY_ID %in% c("MEX", "USA", "GTM", "BLZ", "HND"), ]
  
  admin1 = st_read(dsn = "Data/Mexico_dat/MEX_shapefiles/mex_admbnda_adm1_govmex_20200618.shp", layer = "mex_admbnda_adm1_govmex_20200618")
  admin2 = st_read(dsn = "Data/Mexico_dat/MEX_shapefiles/mex_admbnda_adm2_govmex_20200618.shp", layer = "mex_admbnda_adm2_govmex_20200618")
  admin2$GAUL_CODE = as.numeric(gsub("MX", "", admin2$ADM2_PCODE))
  admin2$NAME = admin2$ADM2_ES
  
  Big_cities_future$Name = admin2$NAME[match(Big_cities_future$GAUL, admin2$GAUL_CODE)]
  Big_cities_future$Year = Big_cities_future$YearInfected
  Big_cities = rbind(Big_cities[, c("Name", "GAUL", "Year")],
                     Big_cities_future[, c("Name", "GAUL", "Year")])
  
  # reformat big cities names
  Big_cities$Name[Big_cities$Name == "CULIACAN"] = "Culiacán"
  Big_cities$Name[Big_cities$Name == "LEON"] = "León"
  Big_cities$Name[Big_cities$Name == "ZAPOPAN"] = "Zapopan"
  Big_cities$Name[Big_cities$Name == "MONTERREY"] = "Monterrey"
  Big_cities$Name[Big_cities$Name == "MEXICALI"] = "Mexicali"
  
  # load original connectivity matrices for relevant movement variables
  # c) Radiation model
  load("Data/Intermediate_datasets/Radiation_matrix_Mexico.RData")
  # D) Adjacency model
  load("Data/Intermediate_datasets/dist_mat_Adjacency_Mexico.RData")
  # E) flight data
  load("Data/Intermediate_datasets/Mexico_air_move_mat_ad2.RData")
  # F) Worldpop internal migration flows (state level)
  load("Data/Intermediate_datasets/Mexico_Migration_mat.RData")
  # G) MAP friction surface- travel time between biggest cities in each municiaplity
  load("Data/Intermediate_datasets/Mexico_Friction_mat.RData")
  # Friction transformation (emphasises accessible places)
  dist_mat_Friction = 1 / (dist_mat_Friction + 1)
  
  # load in original spread data and future projections and convert to a table of estimated invasion year
  #mex_OD <- read.csv("Data/Mexico_mun_dat_cleaned_thresholded.csv")
  dat_mex <- read.csv("Predictions/Mex-Fig1-ContemporaryPredsJan24.csv")
  dat_mex = dat_mex[!is.na(dat_mex$YearPredicted), ]
  FP_Mex <- read.csv("Predictions/Mex-Fig3-FuturePredsJan24.csv")
  FP_Mex = FP_Mex[!(FP_Mex$GAUL %in% dat_mex$GAUL), ]
  MunArrival <- data.frame(GAUL = c(dat_mex$GAUL, FP_Mex$GAUL),
                           YearInvaded = c(dat_mex$YearPredicted, FP_Mex$YearInfected))
  
}



#summarises the connectivity with the invasion city by different types of movement
source.summariser <- function(agg_mob){
  # scaling values for comparability
  # i 1:5 is radiation, adjacency, air, migration and friction
  #for(i in 1:5){
  #  # normalise relative to values observed in all municipalities in the maximum node connectivity
  #  # in spread_dat
  #  # i.e. minus the mean and divide by the range
  #  agg_mob[i, ] = (agg_mob[i, ] - vals_summary[1, i]) / 
  #    (vals_summary[4, i] - vals_summary[3, i])
  #}
  
  # alternative ranking-based approach
  #agg_mob[1, ] = rank(c(agg_mob[1, ], dist_mat_Rad_tv), na.last = "keep")[1:ncol(agg_mob)]
  #agg_mob[2, ] = rank(c(agg_mob[2, ], dist_mat_Adjacency_tv), na.last = "keep")[1:ncol(agg_mob)]
  #agg_mob[3, ] = rank(c(agg_mob[3, ], dist_mat_Air_tv), na.last = "keep")[1:ncol(agg_mob)]
  #agg_mob[4, ] = rank(c(agg_mob[4, ], dist_mat_Migration_tv), na.last = "keep")[1:ncol(agg_mob)]
  #agg_mob[5, ] = rank(c(agg_mob[5, ], dist_mat_Friction_tv), na.last = "keep")[1:ncol(agg_mob)]
  
  # select relevant sources depending on country
  if(country == "Brazil"){
  #  # order is "Rad", "Adj", "Air", "Migration", "Friction"
  #  agg_mob = agg_mob[c(1, 2, 5), ]
  #  mov_opts <- c("Rad", "Adj", "Friction")
  mov_opts <- c("Rad", "Adj", "Air", "Migration", "Friction")
  }
  if(country == "Mexico"){
    # order is "Rad", "Adj", "Air", "Migration", "Friction"
    #agg_mob = agg_mob[c(1, 2, 3, 5), ]
    mov_opts <- c("Rad", "Adj", "Air", "Migration", "Friction")
  }
  
  # aggregate mpobility across all types
  #agg_mob_t <- apply(agg_mob, 2, sum)
  
  # top 10 sources
  #rankr <- data.frame(mob_flux = agg_mob_t, GAUL = admin2$GAUL_CODE)
  #od <- order(rankr$mob_flux, decreasing = T)
  #top10 <- rankr[od, ]
  #top10 <- top10[1:10, ]
  
  # and dominant type of movement from that municipality
  #col_match = match(top10$GAUL, admin2$GAUL_CODE)
  #mov_type = sapply(col_match, function(x) which.max(agg_mob[, x]))
  #top10$Type = mov_opts[mov_type]
  
  # return the top connected municipality for each movement type
  top10 <- data.frame(mob_flux = 1,
                      GAUL = admin2$GAUL_CODE[apply(agg_mob, 1, which.max)],
                      Type = mov_opts)
  # remove adjacency then add again as can be 0 can be multiple municipalities
  if(max(agg_mob[2, ], na.rm = T) == 0){
    # if no adjacency cells then remove adjacency link
    top10 = top10[top10$Type != "Adj", ]
  }else{
    top10 = top10[top10$Type != "Adj", ]
    top10 = rbind(data.frame(mob_flux = 1,
                             GAUL = na.omit(admin2$GAUL_CODE[agg_mob[2, ] == 1]),
                             Type = "Adj"),
                  top10)
  }
  return(top10)
}


# function for all the plotting detail
network.mapper <- function(sources, destination, c_limits, invaded){
  
  # nodes
  source_nodes <- data.frame(id = 1:nrow(sources),
                             st_coordinates(st_centroid(sources)),
                             name = sources$NAME)
  # delete lines from adjacent municipalities
  #sources_away <- sources[sources$Source_type != "Adj", ]
  #source_nodes = source_nodes[source_nodes$name %in% sources_away$NAME, ]
  sources_away <- sources
  
  # edges
  source_nodes_S <- source_nodes[source_nodes$name != destination, ]
  source_nodes_S$weights = sources_away$Sources[!is.na(sources_away$Sources)]
  source_nodes_S$category = sources_away$Source_type[!is.na(sources_away$Sources)]
  source_nodes_D <- source_nodes[source_nodes$name == destination, ]
  
  source_edges <- tibble(data.frame(from = source_nodes_S$id,
                                    to = source_nodes_D$id,
                                    weight = source_nodes_S$weights,
                                    category = source_nodes_S$category))
  source_edges$category = as.factor(source_edges$category)
  
  # graph
  g <- graph_from_data_frame(source_edges, directed = TRUE, vertices = source_nodes)
  
  edges_for_plot <- source_edges %>%
    inner_join(source_nodes %>% select(id, X, Y), by = c('from' = 'id')) %>%
    rename(x = X, y = Y) %>%
    inner_join(source_nodes %>% select(id, X, Y), by = c('to' = 'id')) %>%
    rename(xend = X, yend = Y)
  source_nodes$weight = degree(g)
  
  # color palette
  mov_col <- data.frame(type = c("Adj", "Air", "Friction", "Migration", "Rad"),
                        col = brewer.pal(5, name = "Accent"))
  mov_col_lines = mov_col[mov_col$type %in% source_nodes_S$category, ]
  mov_col_fill = mov_col[mov_col$type %in% sources$Source_type, ]
  
  # background shapefile
  fortaleza <- geom_sf(data = sources[sources$Source_type == destination, ], 
                       fill = "black", lwd = 0.2)
  mun_SF <- sources[sources$Source_type != destination, ]
  mun_SF$category = mun_SF$Source_type
  mun_shapes <- geom_sf(data = mun_SF,
                        aes(fill = category),
                        lwd = 0.2, alpha = 0.75,
                        show.legend = FALSE)
  #state_shapes <- geom_sf(data = admin1, fill = rgb(150/255,150/255,150/255))
  state_shapes <- geom_sf(data = admin1, lwd = 0.2)
  country_shapes <- geom_sf(data = admin0, lwd = 0.2)
  invaded_shapes <- geom_sf(data = invaded, lwd = 0, fill = rgb(255/255,150/255,150/255, 0.5))
  
  
  # coordinate limits
  mapcoords <- coord_sf(xlim = c_limits[1:2], ylim = c_limits[3:4])
  
  # map theme
  maptheme <- theme(panel.grid = element_blank()) +
    theme(axis.text = element_blank()) +
    theme(axis.ticks = element_blank()) +
    theme(axis.title = element_blank()) +
    theme(legend.position = "bottom") +
    theme(panel.grid = element_blank()) +
    #theme(panel.background = element_rect(fill = "white")) +
    theme(panel.background = element_rect(fill = rgb(173/255,216/255,230/255))) +
    theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))
  
  # split into left and right spiral arrows
  edges_for_plot$weight = 0.5
  edges_for_plot1 = edges_for_plot[edges_for_plot$category %in% c("Adj"), ]
  edges_for_plot2 = edges_for_plot[edges_for_plot$category %in% c("Friction"), ]
  edges_for_plot3 = edges_for_plot[edges_for_plot$category %in% c("Rad"), ]
  edges_for_plot4 = edges_for_plot[edges_for_plot$category %in% c("Migration"), ]
  edges_for_plot5 = edges_for_plot[edges_for_plot$category %in% c("Air"), ]
  
  
  g1 <- ggplot(source_nodes) + 
    country_shapes + state_shapes + invaded_shapes + fortaleza + mun_shapes +
    geom_curve(aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                   color = category, size = weight),
               data = edges_for_plot1, curvature = 0.33, lineend = "round",
               alpha = 0.75, show.legend = FALSE) +
    geom_curve(aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                   color = category, size = weight),
               data = edges_for_plot2, curvature = 0.1, lineend = "round",
               alpha = 0.75, show.legend = FALSE) +
    geom_curve(aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                   color = category, size = weight),
               data = edges_for_plot3, curvature = -0.1, lineend = "round",
               alpha = 0.75, show.legend = FALSE) +
    geom_curve(aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                   color = category, size = weight),
               data = edges_for_plot4, curvature = -0.05, lineend = "round",
               alpha = 0.75, show.legend = FALSE) +
    geom_curve(aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                   color = category, size = weight),
               data = edges_for_plot5, curvature = 0.05, lineend = "round",
               alpha = 0.75, show.legend = FALSE) +
    scale_size_continuous(guide = "none", range = c(0.3, 3)) + # scale for edge widths
    scale_color_manual(values = mov_col_lines$col) +
    scale_fill_manual(values = mov_col_fill$col) +
    mapcoords + maptheme
  
  return(g1)
}


# calculate the min and max of each movement covariate to put flux values in context
#vals_summary <- data.frame(Rad = log(c(min(dist_mat_Rad), max(dist_mat_Rad)) + 1),
#                           Adj = c(min(dist_mat_Adjacency), max(dist_mat_Adjacency)),
#                           Air = log(c(min(dist_mat_Air), max(dist_mat_Air)) + 1),
#                           Mig = log(c(min(dist_mat_Migration), max(dist_mat_Migration)) + 1),
#                           Fric = log(c(min(dist_mat_Friction), max(dist_mat_Friction)) + 1))

# and all values in vectorised form for ranking approach
#dist_mat_Rad_tv = log(as.vector(dist_mat_Rad) + 1)
#dist_mat_Adjacency_tv = as.vector(dist_mat_Adjacency)
#dist_mat_Air_tv = log(as.vector(dist_mat_Air) + 1)
#dist_mat_Migration_tv = log(as.vector(dist_mat_Migration) + 1)
#dist_mat_Friction_tv = log(as.vector(dist_mat_Friction) + 1)

# vals summary simplification to just connectivity variables
#vals_summary = vals_summary[, 15:19]


#extract top10 sources from city con list and future list
top10s <- lapply(Big_city_con_list, source.summariser)
names(top10s) <- names(Big_city_con_list)

top10s_future <- lapply(Big_city_con_list_future, source.summariser)
names(top10s_future) <- unlist(lapply(Big_city_con_list_future, function(x) attributes(x)$names[1]))

top10s <- c(top10s, top10s_future)

# past cities (Brazil) are: "Fortaleza", "Belo Horizonte", "Brasilia", "Sao Paulo", "Salvador"
# future cities (Brazil) are: "Nova Iguacu", "Porto Alegre", "Curitiba", "Joinville", "Florianopolis", "Caxias Do Sul", "Pelotas" 

# trim to specific cities in mexico- Monterrey, Zapopan (Guadalajara), Tijuana and Nezahualcóyotl (CDMX suburbs)
if(country == "Mexico"){
  top10s = top10s[c(2, 3, 9, 12)]
}

# trim to specific cities in Brazil- Brasilia, Sao Paulo, Salvador, Curitiba and Porto Alegre
if(country == "Brazil"){
  top10s = top10s[c(3:5, 7, 8)]
}


# make maps city at a time
# loop through cities
for(i in 1:length(top10s)){
  
  # initialise vector
  admin2$Sources = admin2$Source_type = NA
  
  # label invasion city
  city_dets <- data.frame(GAUL = as.numeric(names(top10s)[i]),
                          NAME = Big_cities$Name[match(as.numeric(names(top10s)[i]), Big_cities$GAUL)],
                          YEAR = Big_cities$Year[match(as.numeric(names(top10s)[i]), Big_cities$GAUL)])
  # character correction for Sao Paulo and Brasilia
  if(city_dets$NAME == "S\\'e3o Paulo"){city_dets$NAME = "Sao Paulo"}
  if(city_dets$NAME == "BrasÌlia"){city_dets$NAME = "Brasilia"}
  if(city_dets$NAME == "Quer\x8etaro"){city_dets$NAME = "Querétaro"}
  if(city_dets$NAME == "Naucalpan de Juarez"){city_dets$NAME = "Naucalpan de Juárez"}
  
  admin2$Source_type[admin2$GAUL_CODE == city_dets$GAUL] = city_dets$NAME
  
  # now loop though top10 sources and assign movement types to each source
  sources <- admin2[admin2$GAUL_CODE == city_dets$GAUL, ]
  
  for(k in 1:nrow(top10s[[i]])){
    sources = rbind(sources, admin2[admin2$GAUL_CODE == top10s[[i]][k, 2], ])
  }
  # Add type and weight
  sources$Source_type = c(city_dets$NAME, top10s[[i]][, 3])
  sources$Sources = c(NA, top10s[[i]][, 1])
  
  # background of all invaded municipalities
  invaded <- admin2[admin2$GAUL_CODE %in% MunArrival$GAUL[MunArrival$YearInvaded < city_dets$YEAR], ]
  
  ### arrows section
  destination = city_dets$NAME
  if(country == "Brazil"){
    c_limits = list(c(-49.44647, -43.14981, -23.08072, -14.53519),
                    c(-46.91761, -43.14981, -24.31051, -22.77694),
                    c(-39.86854, -37.63538, -14.39564, -11.35698),
                    c(-52.00000, -46.14981, -30.27303, -23.0),
                    c(-49.30569, -46.14981, -26.27303, -23.02882))[[i]]
  }else{
    c_limits = list(c(-101.0,  -87.0,   17.5,   26.5),
                    c(-105.5, -100,   18.5,   22.5),
                    c(-117.26157, -108.00000,   23.08479,   32.57095),
                    c(-103.26157,  -96.74054,   18.68408,   22.57095))[[i]]
  }
  # automated extent detection (top 3 non adjacent sources in scope)
  all_source = admin2[!is.na(admin2$Source_type), ]
  all_source = all_source[order(all_source$Sources, decreasing = T), ] # sort most to least connected
  
  # calculate figure limits
  #c_limits <- as.vector(extent(all_source[all_source$Source_type != "Air", ]))
  #c_limits <- as.vector(extent(all_source))
  
  # remove adjacency from the line drawing
  #all_source = all_source[!(all_source$Source_type %in% "Adj"), ]
  
  
  g1 <- network.mapper(sources, destination, c_limits, invaded)
  
  # saving
  p_filename <- paste0("Plots/Jan24/Source_type_", city_dets$NAME, ".png")
  ggsave(filename = p_filename, g1)
  
  # source data saving
  if(country == "Mexico"){
    invaded <- as.data.frame(invaded)[, c("ADM2_ES", "ADM2_PCODE", "GAUL_CODE")]
    invaded$Source_type = "Other"
    sources = as.data.frame(sources)[, c("ADM2_ES", "ADM2_PCODE", "GAUL_CODE", "Source_type")]
    invaded = invaded[!(invaded$GAUL_CODE %in% sources$GAUL_CODE), ]
    all_sources = rbind(invaded, sources)
  }else{
    invaded <- as.data.frame(invaded)[, c("NAME", "GAUL_CODE")]
    invaded$Source_type = "Other"
    sources = as.data.frame(sources)[, c("NAME", "GAUL_CODE", "Source_type")]
    invaded = invaded[!(invaded$GAUL_CODE %in% sources$GAUL_CODE), ]
    all_sources = rbind(invaded, sources)
  }
  
  if(city_dets$NAME == "Brasilia"){write.csv(all_sources, file = "Figure_source_data/SD_Fig_4A.csv")}
  if(city_dets$NAME == "Sao Paulo"){write.csv(all_sources, file = "Figure_source_data/SD_Fig_4B.csv")}
  if(city_dets$NAME == "Porto Alegre"){write.csv(all_sources, file = "Figure_source_data/SD_Fig_4C.csv")}
  if(city_dets$NAME == "Curitiba"){write.csv(all_sources, file = "Figure_source_data/SD_Fig_4D.csv")}
  
  if(city_dets$NAME == "Monterrey"){write.csv(all_sources, file = "Figure_source_data/SD_Fig_5A.csv")}
  if(city_dets$NAME == "Zapopan"){write.csv(all_sources, file = "Figure_source_data/SD_Fig_5B.csv")}
  if(city_dets$NAME == "Tijuana"){write.csv(all_sources, file = "Figure_source_data/SD_Fig_5C.csv")}
  if(city_dets$NAME == "Nezahualcóyotl"){write.csv(all_sources, file = "Figure_source_data/SD_Fig_5D.csv")}
}


# national map for reference
if(country == "Mexico"){
  admin0 = admin0[admin0$COUNTRY_ID == "MEX", ]
} else{
  admin0 = admin0[admin0$COUNTRY_ID == "BRA", ]
}
state_shapes <- geom_sf(data = admin1, lwd = 0.2)
country_shapes <- geom_sf(data = admin0, lwd = 0.2)
# map theme
maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) +
  #theme(panel.background = element_rect(fill = rgb(173/255,216/255,230/255))) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))
g2 <- ggplot(admin0) + 
  country_shapes + state_shapes + maptheme
ggsave(filename = paste0("Plots/Jan24/Ref_map_", country, ".png"), g2)

