# compiles human movement matrices between municiaplities (admin 2) in Brazil

rm(list = ls())


require(raster)
require(rgdal)
require(rgeos)
require(geosphere)
require(movement)
require(mgcv)

setwd("/Users/eideobra/Dropbox/08_Serotype_spread/")


## admin units
Brazil <- readOGR(dsn = "Reference/Admin2(2011)/admin2.shp", layer = "admin2")
Brazil = Brazil[Brazil$COUNTRY_ID == "BRA", ]
mun_info <- read.csv("Reference/GAUL_IBGE_conversion_full.csv")
Brazil$Pop = mun_info$IBGE_MUN_POP[match(Brazil$GAUL_CODE, mun_info$GAUL_CODE)]

Mexico = readOGR(dsn = "Data/Mexico_dat/MEX_shapefiles/mex_admbnda_adm2_govmex_20200618.shp", layer = "mex_admbnda_adm2_govmex_20200618")
mun_info2 <- read.csv("Data/Mexico_dat/mex_popp_govmex.csv")
mun_info2$pobtot = as.numeric(gsub(",", "", mun_info2$pobtot))
mun_info2 <- aggregate(pobtot ~ adm2code, data = mun_info2, FUN = sum)

Mexico$GAUL_CODE = as.numeric(gsub("MX", "", Mexico$ADM2_PCODE))
Mexico$Pop = mun_info2$pobtot[match(Mexico$ADM2_PCODE, mun_info2$adm2code)]
# choose country
admin2 = Brazil

## municipality info



## Human movement matrices
# A) simple great circle distance for now
coords <- gCentroid(admin2,byid=TRUE)
dist_mat_GC <- distm(coords)

# B) gravity model
location_data <- data.frame(location = admin2$GAUL_CODE,
                            population = admin2$Pop,
                            x = coordinates(coords)[, 1],
                            y = coordinates(coords)[, 2])
location_data  <- as.location_dataframe(location_data)

# currently using Portugal parameterisation from Kraemer Ebola paper
predicted_flux  <- predict(gravity(theta = 0.01, alpha = 0.44, beta = 0.4, gamma = 1.81), 
                           location_data, symmetric = TRUE)
dist_mat_Grav <- as.matrix(round(predicted_flux$movement_matrix))



# C) radiation model
predicted_flux  <- predict(originalRadiation(theta = 0.9), 
                           location_data, 
                           symmetric = TRUE)
dist_mat_Rad <- as.matrix(round(predicted_flux$movement_matrix))



# D) adjacency model
require(spdep)
adj_list = poly2nb(admin2, queen=TRUE, row.names = admin2$GAUL_CODE)
dist_mat_Adjacency = matrix(0, nrow = nrow(admin2), ncol = nrow(admin2))
for(i in 1:length(adj_list)){
  dist_mat_Adjacency[i, adj_list[[i]]] = 1
  dist_mat_Adjacency[adj_list[[i]], i] = 1 
}

if(nrow(admin2) == nrow(Brazil)){
  save(location_data, file = "Data/Intermediate_datasets/Brazil_location_data.RData")
  save(dist_mat_Grav, file = "Data/Intermediate_datasets/Gravity_matrix_Brazil.RData")
  save(dist_mat_Rad, file = "Data/Intermediate_datasets/Radiation_matrix_Brazil.RData")
  save(dist_mat_Adjacency, file = "Data/Intermediate_datasets/dist_mat_Adjacency_Brazil.RData")
  save(dist_mat_GC, file = "Data/Intermediate_datasets/dist_mat_GC_Brazil.RData")
}
if(nrow(admin2) == nrow(Mexico)){
  save(location_data, file = "Data/Intermediate_datasets/Mexico_location_data.RData")
  save(dist_mat_Grav, file = "Data/Intermediate_datasets/Gravity_matrix_Mexico.RData")
  save(dist_mat_Rad, file = "Data/Intermediate_datasets/Radiation_matrix_Mexico.RData")
  save(dist_mat_Adjacency, file = "Data/Intermediate_datasets/dist_mat_Adjacency_Mexico.RData")
  save(dist_mat_GC, file = "Data/Intermediate_datasets/dist_mat_GC_Mexico.RData")
}



# E) flight data
#airports <- read.csv("Covariates/Connectivity/Flights/list of cities geocoded.csv")
airports <- read.csv("Covariates/Connectivity/Flights/global_airport_codes.csv")
# remove airports with missing country
airports = airports[!is.na(airports$iso_country), ]



if(nrow(admin2) == nrow(Brazil)){
    
    # subset airports
    airports = airports[airports$iso_country == "BR", ]
    
    # load in flight data
    fd <- read.csv("Covariates/Connectivity/Flights/Annual_2009_2019/BrazilAirportToAirport2009_2019.csv")
    
    # check all flight data airport codes are in airports database
    #all(fd$origAirportCode %in% airports$iata_code) # TRUE
    # subset airports to just those that we have flight data for
    airports = airports[airports$iata_code %in% c(fd$origAirportCode, fd$destAirportCode), ]
    # sort airport codes alphabetically
    airports = airports[order(airports$iata_code), ]
    
    ### aggregate flight data over multiple years
    # generate combined origin destination variable
    fd$OD = apply(fd[, c("origAirportCode", "destAirportCode")], 1, paste, collapse = "")
    fd_sum <- aggregate(totalVol ~ OD, data = fd, FUN = sum)
    # now split origina and destination back out
    fd_sum$origin = sapply(fd_sum$OD, substr, start = 1, stop = 3)
    fd_sum$destination = sapply(fd_sum$OD, substr, start = 4, stop = 6)
    
    
    ### Airport catchment areas
    # use Huff model to estimate airport catchment areas
    # Sj = airport attractiveness = outbound traffic
    Sj = aggregate(totalVol ~ origAirportCode, data = fd, FUN = "sum")
    # specific exception for Brazil where CCI only has inbound traffic
    Sj = rbind(Sj, data.frame(origAirportCode = "CCI",
                              totalVol = fd$totalVol[fd$destAirportCode == "CCI"]))
    # then re-sort on alphabetical order
    Sj = Sj[order(Sj$origAirportCode), ]
    
    # Dij = distance between municipality centroid and each airport
    Dij = distm(as.data.frame(coords), cbind(airports$Longitude, airports$Latitude))
    # impose max distance catchment area by changing all distances above this to very far away
    dist_thresh <- 500000 # 500km
    Dij[Dij > dist_thresh] = 999999999
    # distance exponent - assign 2 based on https://reader.elsevier.com/reader/sd/pii/S1877584520300587?token=07871C5033501E848E5CEFCBCBF015E4752C1B4444D726FE555E1464E5539194ACE025C0B50960363F7023C95604E787&originRegion=eu-west-1&originCreation=20220818145833
    beta = 2
    
    
    # loop through each municipality and calculate Pij for all airports
    Pij <- matrix(NA, nrow = nrow(Dij), ncol = nrow(Sj))
    for(i in 1:nrow(Dij)){
      for(j in 1:nrow(Sj)){
        Pij[i, j] = (Sj[j, 2] / (Dij[i, j] ^ beta)) / sum(Sj[j, 2] / (Dij[, j] ^ beta))
      }
    }
    # sum municipality values to 1- i.e. given person has chosen to travel what airport do they choose?
    NPij <- t(apply(Pij, 1, function(x) x / sum(x)))
    
    # now multiply by resident population in each municipality to get a (relative) expectation of visitors
    # per airport
    PNPij <- NPij * admin2$Pop
    
    # now normalise by airport to decide what proportion of passenger volume should be 
    # assigned to each municipality
    NPNPij <- apply(PNPij, 2, function(x) x / sum(x))
    
    ## assigning airport traffic volumes to municipality movement matrices
    # go airport to airport connection
    # redistribute passengers at origin and destination airports by NDPij
    
    # set up municipality movement matrix
    dist_mat_Air = matrix(0, nrow = nrow(admin2), ncol = nrow(admin2))
    
    # main loop
    for(i in 1:nrow(fd_sum)){
      # origin and destination airports
      ori <- fd_sum$origin[i]
      dest <- fd_sum$destination[i]
      # catchment areas for origin and destination airports
      ori_c <- NPNPij[, match(ori, Sj$origAirportCode)]
      dest_c <- NPNPij[, match(dest, Sj$origAirportCode)]
      # passengers from origin
      ori_p <- ori_c * fd_sum$totalVol[i]
      # assign these to destinations in a big matrix
      travel_mat <- t(sapply(ori_p, function(x) x * dest_c))
      # set diagonals to 0 to avoid destination being the same as origin
      diag(travel_mat) = 0
      # then add to the higher level matrix
      dist_mat_Air = dist_mat_Air + travel_mat
    }
    
    save(dist_mat_Air, file = "Data/Intermediate_datasets/Brazil_air_move_mat_ad2.RData")
}



# now Mexico version
if(nrow(admin2) == nrow(Mexico)){
  
  # subset airports
  airports = airports[airports$iso_country == "MX", ]
  #airports$airportCode = as.character(airports$airportCode)
  # sort airport codes alphabetically
  airports = airports[order(airports$iata_code), ]
  
  # load in flight data
  fd <- read.csv("Covariates/Connectivity/Flights/Annual_2009_2019/MexicoAirportToAirport2009_2019.csv")
  
  # check all flight data airport codes are in airports database
  #all(fd$origAirportCode %in% airports$iata_code) # TRUE
  # subset airports to just those that we have flight data for
  airports = airports[airports$iata_code %in% fd$origAirportCode, ]
  
  ### aggregate flight data over multiple years
  # generate combined origin destination variable
  fd$OD = apply(fd[, c("origAirportCode", "destAirportCode")], 1, paste, collapse = "")
  fd_sum <- aggregate(totalVol ~ OD, data = fd, FUN = sum)
  # now split origina and destination back out
  fd_sum$origin = sapply(fd_sum$OD, substr, start = 1, stop = 3)
  fd_sum$destination = sapply(fd_sum$OD, substr, start = 4, stop = 6)
  
  
  ### Airport catchment areas
  # use Huff model to estimate airport catchment areas
  # Sj = airport attractiveness = outbound traffic
  Sj = aggregate(totalVol ~ origAirportCode, data = fd, FUN = "sum")
  
  # Dij = distance between municipality centroid and each airport
  Dij = distm(as.data.frame(coords), cbind(airports$Longitude, airports$Latitude))
  # impose max distance catchment area by changing all distances above this to very far away
  dist_thresh <- 500000 # 500km
  Dij[Dij > dist_thresh] = 999999999
  # distance exponent - assign 2 based on https://reader.elsevier.com/reader/sd/pii/S1877584520300587?token=07871C5033501E848E5CEFCBCBF015E4752C1B4444D726FE555E1464E5539194ACE025C0B50960363F7023C95604E787&originRegion=eu-west-1&originCreation=20220818145833
  beta = 2
  
  
  # loop through each municipality and calculate Pij for all airports
  Pij <- matrix(NA, nrow = nrow(Dij), ncol = nrow(Sj))
  for(i in 1:nrow(Dij)){
    for(j in 1:nrow(Sj)){
      Pij[i, j] = (Sj[j, 2] / (Dij[i, j] ^ beta)) / sum(Sj[j, 2] / (Dij[, j] ^ beta))
    }
  }
  # sum municipality valeus to 1- i.e. given person has chosen to travel what airport do they choose?
  NPij <- t(apply(Pij, 1, function(x) x / sum(x)))
  
  # now multiply by resident population in each municipality to get a (relative) expectation of visitors
  # per airport
  PNPij <- NPij * admin2$Pop
  
  # now normalise by airport to decide what proportion of passenger volume should be 
  # assigned to each muncipality
  NPNPij <- apply(PNPij, 2, function(x) x / sum(x))
  
  ## assigning airport traffic volumes to municipality movement matrices
  # go airport to airport connection
  # redistribute passengers at origin and destination airports by NDPij
  
  # set up municipality movement matrix
  dist_mat_Air = matrix(0, nrow = nrow(admin2), ncol = nrow(admin2))
  
  # main loop
  for(i in 1:nrow(fd_sum)){
    # origin and destination airports
    ori <- fd_sum$origin[i]
    dest <- fd_sum$destination[i]
    # catchment areas for origin and destination airports
    ori_c <- NPNPij[, match(ori, Sj$origAirportCode)]
    dest_c <- NPNPij[, match(dest, Sj$origAirportCode)]
    # passengers from origin
    ori_p <- ori_c * fd_sum$totalVol[i]
    # assign these to destinations in a big matrix
    travel_mat <- t(sapply(ori_p, function(x) x * dest_c))
    # set diagonals to 0 to avoid destination being the same as origin
    diag(travel_mat) = 0
    # then add to the higher level matrix
    dist_mat_Air = dist_mat_Air + travel_mat
  }
  
  save(dist_mat_Air, file = "Data/Intermediate_datasets/Mexico_air_move_mat_ad2.RData")
}











# F) Worldpop internal migration flows (state level)

if(nrow(admin2) == nrow(Brazil)){
  # load in raw data
  wpmig <- read.csv("Covariates/Connectivity/Worldpop_migraton/BRA_5yrs_InternalMigFlows_2010.csv")
  # load in admin 1 shapefiles
  admin1 <- readOGR(dsn = "Reference/Admin1(2011)/admin1.shp", layer = "admin1")
  admin1 = admin1[admin1$COUNTRY_ID == "BRA", ]
  # work out proportion of the state-wide population that is in each admin2
  admin2[is.na(admin2$Pop)] = 1
  state_pop = aggregate(Pop~ PARENT_ID, data = admin2, FUN = sum)
  admin2$SatePop_prop <- admin2$Pop / state_pop$Pop[match(admin2$PARENT_ID, state_pop$PARENT_ID)]
  
}
if(nrow(admin2) == nrow(Mexico)){
  # load in raw data
  wpmig <- read.csv("Covariates/Connectivity/Worldpop_migraton/MEX_5yrs_InternalMigFlows_2010.csv")
  # load in admin 1 shapefiles
  admin1 <- readOGR(dsn = "Data/Mexico_dat/MEX_shapefiles/mex_admbnda_adm1_govmex_20200618.shp", 
                    layer = "mex_admbnda_adm1_govmex_20200618")
  admin1$GAUL_CODE = as.numeric(gsub("MX", "", admin1$ADM1_PCODE))
  admin2$PARENT_ID = as.numeric(gsub("MX", "", admin2$ADM1_PCODE))
  
  # work out proportion of the state-wide population that is in each admin2
  state_pop = aggregate(Pop~ PARENT_ID, data = admin2, FUN = sum)
  admin2$SatePop_prop <- admin2$Pop / state_pop$Pop[match(admin2$PARENT_ID, state_pop$PARENT_ID)]
}


## Node (I) numbers to GAUL codes
# tabel of unique latlongs
node_tab <- data.frame(uniquecombs(wpmig[, c("LONTO", "LATTO")]),
                       GAUL = as.numeric(as.character(extract(admin1, uniquecombs(wpmig[, c("LONTO", "LATTO")]))$GAUL_CODE)))

wpmig$GAUL_I <- node_tab$GAUL[match(wpmig$LONFR, node_tab$LONTO)]
wpmig$GAUL_J <- node_tab$GAUL[match(wpmig$LONTO, node_tab$LONTO)]



# now to movement matrix at admin2 level
dist_mat_Migration = matrix(0, nrow = nrow(admin2), ncol = nrow(admin2))
for(i in 1:nrow(admin2)){
  home = admin2$PARENT_ID[i]
  home_prop = admin2$SatePop_prop[i]
  for(j in 1:nrow(admin2)){
    away = admin2$PARENT_ID[j]
    #away_prop = admin2$SatePop_prop[j]
    if((home != away) & (home %in% wpmig$GAUL_I) & (away %in% wpmig$GAUL_J)){
      dist_mat_Migration[i, j] = wpmig$PrdMIG[(wpmig$GAUL_I == home) & (wpmig$GAUL_J == away)] * home_prop
    }
  }
}




if(nrow(admin2) == nrow(Brazil)){
  save(dist_mat_Migration, file = "Data/Intermediate_datasets/Brazil_Migration_mat.RData")
}
if(nrow(admin2) == nrow(Mexico)){
  save(dist_mat_Migration, file = "Data/Intermediate_datasets/Mexico_Migration_mat.RData")
}







# G) MAP friction surface- travel time between biggest city in each municipality
if(nrow(admin2) == nrow(Brazil)){
  dist_mat_Friction <- read.csv("Covariates/Connectivity/MAP_Accessibility/Brazil_accessibility_matrix.csv")
  # remove first column (just contains GAUL codes)
  dist_mat_Friction = dist_mat_Friction[, 2:ncol(dist_mat_Friction)]
  dist_mat_Friction = as.matrix(dist_mat_Friction)
  # there are 9 municipalities for which the algorithm didn't work- suspect v. long distances to islands
  # this leads to 9 NAs in most columns and 5510 NAs in 9 colummns- need to fix these to an arbitrary high value
  # highest travel time in the dataset = 8617.3
  for(i in 1:ncol(dist_mat_Friction)){
    if(all(is.na(dist_mat_Friction[, i]))){
      dist_mat_Friction[, i] = 8617.3
    }else{
      dist_mat_Friction[is.na(dist_mat_Friction[, i]), i] = max(dist_mat_Friction[!is.na(dist_mat_Friction[, i]), i])
    }
  }
  
  # save
  save(dist_mat_Friction, file = "Data/Intermediate_datasets/Brazil_Friction_mat.RData")
}

if(nrow(admin2) == nrow(Mexico)){
  dist_mat_Friction <- read.csv("Covariates/Connectivity/MAP_Accessibility/Mexico_accessibility_matrix.csv")
  # remove first column (just contains GAUL codes)
  dist_mat_Friction = dist_mat_Friction[, 2:ncol(dist_mat_Friction)]
  dist_mat_Friction = as.matrix(dist_mat_Friction)
  # save
  save(dist_mat_Friction, file = "Data/Intermediate_datasets/Mexico_Friction_mat.RData")
}



############################
# movement projection
############################

# time periods for data:
# population: Brazil = 2010, Mexico = 2010
# flights = 2009-2019 average
# Radiation = same as population (2010)
# migration = 2010 (5 year average)
# Friction = 2015

# load in UN population projections for Brazil and Mexico
# https://population.un.org/wpp/Download/
pop <- read.csv("/Users/eideobra/Dropbox/08_Serotype_spread/Reference/UN_population_projections.csv")


# load in OECD GDP long term projections for Brazil and Mexico
# https://data.oecd.org/gdp/gdp-long-term-forecast.htm
gdp <- read.csv("/Users/eideobra/Dropbox/08_Serotype_spread/Reference/OECD_gdp_projections.csv")

# restrict to relevant date range then look at relative changes over time
pop = pop[pop$Year %in% (1980:2040), ]
gdp = gdp[gdp$Year %in% (1980:2040), ]

# values relative to 2010
pop_br <- pop[pop$Country == "Brazil", ]
pop_mx <- pop[pop$Country == "Mexico", ]
gdp_br <- gdp[gdp$Country == "Brazil", ]
gdp_mx <- gdp[gdp$Country == "Mexico", ]

pop_br$Pop = pop_br$Pop / pop_br$Pop[pop_br$Year == 2010]
pop_mx$Pop = pop_mx$Pop / pop_mx$Pop[pop_mx$Year == 2010]

gdp_br$GDP = gdp_br$GDP / gdp_br$GDP[gdp_br$Year == 2010]
gdp_mx$GDP = gdp_mx$GDP / gdp_mx$GDP[gdp_mx$Year == 2010]

# linear extrapolation for GDP values 1980-1989
gdp_mod_br = lm(GDP ~ Year, data = gdp_br)
gdp_br = rbind(data.frame(Country = "Brazil",
                          Year = 1980:1989,
                          GDP = predict(gdp_mod_br, newdata = data.frame(Year = 1980:1989))),
               gdp_br)

gdp_mod_mx = lm(GDP ~ Year, data = gdp_mx)
gdp_mx = rbind(data.frame(Country = "Mexico",
                          Year = 1980:1989,
                          GDP = predict(gdp_mod_mx, newdata = data.frame(Year = 1980:1989))),
               gdp_mx)



# inspect trends:
summary(pop_br$Pop);summary(gdp_br$GDP)
summary(pop_mx$Pop);summary(gdp_mx$GDP)

plot(gdp_br$Year, gdp_br$GDP, type = "l")
lines(pop_br$Year, pop_br$Pop)
abline(v = 2010)

plot(gdp_mx$Year, gdp_mx$GDP, type = "l")
lines(pop_mx$Year, pop_mx$Pop)
abline(v = 2010)

# assume growing in travel is an equal weighted average of changes in population and GDP
br_mov_change = data.frame(Year = 1980:2040,
                            Mult = 0.5 * (pop_br$Pop + gdp_br$GDP))

mx_mov_change = data.frame(Year = 1980:2040,
                            Mult = 0.5 * (pop_mx$Pop + gdp_mx$GDP))

# save multipliers to be used in Fit_Eval_data_prep
write.csv(br_mov_change, file = "Covariates/Future_projections/predictions/Movement_Brazil.csv")
write.csv(mx_mov_change, file = "Covariates/Future_projections/predictions/Movement_Mexico.csv")




























#############################
# movement data visualisation
#############################

require(sp)
require(tmap)
require(sf)
require(maps)
require(ggplot2)
require(gridExtra)

size.scale <- function(x, maxwidth){
  size_vec = x^(1/2)
  size_vec = size_vec - min(size_vec)
  if(min(size_vec) != max(size_vec)){size_vec = size_vec / max(size_vec)}
  size_vec = size_vec + 0.05
  size_vec = size_vec * maxwidth
  return(size_vec)
}
mov.mat.plot <- function(f_mat, f_level, n2 = 10000, inverse = FALSE, maxwidth = 5){
  
  # subset to focus geographic level
  if(f_level == "cities"){
    f_index = admin2$Pop >= 100000
    f_mat = f_mat[f_index, f_index]
  }
  
  if(f_level == "towns"){
    f_index = (admin2$Pop < 100000) & (admin2$Pop >= 10000)
    f_mat = f_mat[f_index, f_index]
  }
  
  if(f_level == "villages"){
    f_index = admin2$Pop < 10000
    f_mat = f_mat[f_index, f_index]
  }
  
  if(f_level == "all"){
    f_index = rep(TRUE, nrow(admin2))
  }
  
  # delete duplicated connections
  # - does not scale well with large matrices
  for(i in 1:(ncol(f_mat) - 1)){
    f_mat[(i + 1):nrow(f_mat), i] = NA
  }
  
  cpoints <- data.frame(GAUL = admin2$GAUL_CODE,
                        centroid(admin2))
  
  l_dat = data.frame(from = as.vector(sapply(admin2$GAUL_CODE[f_index], rep, times = sum(f_index))),
                     to = rep(admin2$GAUL_CODE[f_index], sum(f_index)),
                     flow = as.vector(f_mat))
  l_dat$from.x = cpoints$X1[match(l_dat$from, cpoints$GAUL)]
  l_dat$from.y = cpoints$X2[match(l_dat$from, cpoints$GAUL)]
  l_dat$to.x = cpoints$X1[match(l_dat$to, cpoints$GAUL)]
  l_dat$to.y = cpoints$X2[match(l_dat$to, cpoints$GAUL)]
  
  
  
  # remove self connections
  l_dat = l_dat[l_dat$flow != 0, ]
  
  # delete duplicated connections
  l_dat = l_dat[!is.na(l_dat$flow), ]
  
  # if looking at great circle distance
  if(inverse){l_dat$flow = 1 / l_dat$flow}
  
  # point size proprotional to total traffic volume to all destinations
  pointSize = aggregate(l_dat$flow, by = list(l_dat$to), FUN = sum)
  pointSize = data.frame(GAUL = pointSize[, 1],
                         size = size.scale(pointSize$x, maxwidth))
  pointSize = pointSize$size[match(cpoints$GAUL[f_index], pointSize$GAUL)]
  pointSize[is.na(pointSize)] = 1
  
  # take top n2 routes
  l_dat = l_dat[order(l_dat$flow, decreasing = TRUE), ]
  if(nrow(l_dat) > n2){l_dat = l_dat[1:n2, ]}
  
  # size vector for lines
  size_vec = size.scale(l_dat$flow, maxwidth)
  
  # color by biggest hubs (in terms of flow)
  colvec = data.frame(hub_GAUL = names(table(as.numeric(as.character(l_dat$to)))),
                      count = as.numeric(table(as.numeric(as.character(l_dat$to)))))
  #colvec = aggregate(l_dat$flow, by = list(l_dat$to), FUN = sum)
  colvec = colvec[order(colvec$count, decreasing = T), ]
  colvec$AssignCol = c(rainbow(10, alpha = 0.5), rep(rgb(255/255,248/255,220/255, 0.25), nrow(colvec) - 10))
  colvec2 = colvec$AssignCol[match(l_dat$to, colvec$hub_GAUL)]
  
  p1 <- ggplot() +
    geom_polygon(data = admin2, aes(x = long, y = lat, group = group), fill = "darkgrey") +
    #geom_point(data = cpoints, aes(x = X1, y = X2), size = 0.1, color = "white") +
    geom_point(data = cpoints[f_index, ], aes(x = X1, y = X2), size = pointSize, color = rgb(0,0,0,0.5), shape = 1) +
    geom_curve(data = l_dat, aes(x = from.x, y = from.y, xend = to.x, yend = to.y), col = colvec2, 
               size = size_vec, curvature = .2) +
    geom_point(data = cpoints[f_index, ], aes(x = X1, y = X2), size = pointSize, color = rgb(0,0,0,0.1), shape = 1) +
    theme_dark()
  return(p1)
}

# A) great circle distance
p0 <- mov.mat.plot(dist_mat_GC, "all", inverse = TRUE)
p1 <- mov.mat.plot(dist_mat_GC, "cities", inverse = TRUE)
p2 <- mov.mat.plot(dist_mat_GC, "towns", inverse = TRUE)
p3 <- mov.mat.plot(dist_mat_GC, "villages", inverse = TRUE)

if(nrow(admin2) == nrow(Brazil)){
  pdf(file = "Plots/Viz_Brazil_mov_GC.pdf", width = 20, height = 20)
  grid.arrange(p0, p1, p2, p3, ncol = 2, nrow= 2);dev.off()
}
if(nrow(admin2) == nrow(Mexico)){
  pdf(file = "Plots/Viz_Mexico_mov_GC.pdf", width = 20, height = 20)
  grid.arrange(p0, p1, p2, p3, ncol = 2, nrow= 2);dev.off()
}



# B) gravity model
p0 <- mov.mat.plot(dist_mat_Grav, "all")
p1 <- mov.mat.plot(dist_mat_Grav, "cities")
p2 <- mov.mat.plot(dist_mat_Grav, "towns")
p3 <- mov.mat.plot(dist_mat_Grav, "villages")

if(nrow(admin2) == nrow(Brazil)){
  pdf(file = "Plots/Viz_Brazil_mov_Grav.pdf", width = 20, height = 20)
  grid.arrange(p0, p1, p2, p3, ncol = 2, nrow= 2);dev.off()
}
if(nrow(admin2) == nrow(Mexico)){
  pdf(file = "Plots/Viz_Mexico_mov_Grav.pdf", width = 20, height = 20)
  grid.arrange(p0, p1, p2, p3, ncol = 2, nrow= 2);dev.off()
}


# C) radiation model
p0 <- mov.mat.plot(dist_mat_Rad, "all")
p1 <- mov.mat.plot(dist_mat_Rad, "cities")
p2 <- mov.mat.plot(dist_mat_Rad, "towns")
p3 <- mov.mat.plot(dist_mat_Rad, "villages")

if(nrow(admin2) == nrow(Brazil)){
  pdf(file = "Plots/Viz_Brazil_mov_Rad.pdf", width = 20, height = 20)
  grid.arrange(p0, p1, p2, p3, ncol = 2, nrow= 2);dev.off()
}
if(nrow(admin2) == nrow(Mexico)){
  pdf(file = "Plots/Viz_Mexico_mov_Rad.pdf", width = 20, height = 20)
  grid.arrange(p0, p1, p2, p3, ncol = 2, nrow= 2);dev.off()
}

# D) Adjacency
p0 <- mov.mat.plot(dist_mat_Adjacency, "all")
p1 <- mov.mat.plot(dist_mat_Adjacency, "cities")
p2 <- mov.mat.plot(dist_mat_Adjacency, "towns")
p3 <- mov.mat.plot(dist_mat_Adjacency, "villages")

if(nrow(admin2) == nrow(Brazil)){
  pdf(file = "Plots/Viz_Brazil_mov_Adjacency.pdf", width = 20, height = 20)
  grid.arrange(p0, p1, p2, p3, ncol = 2, nrow= 2);dev.off()
}
if(nrow(admin2) == nrow(Mexico)){
  pdf(file = "Plots/Viz_Mexico_mov_Adjacency.pdf", width = 20, height = 20)
  grid.arrange(p0, p1, p2, p3, ncol = 2, nrow= 2);dev.off()
}

# E) flight data
p0 <- mov.mat.plot(dist_mat_Air, "all")
p1 <- mov.mat.plot(dist_mat_Air, "cities")
p2 <- mov.mat.plot(dist_mat_Air, "towns")
p3 <- mov.mat.plot(dist_mat_Air, "villages")

if(nrow(admin2) == nrow(Brazil)){
  pdf(file = "Plots/Viz_Brazil_mov_Air.pdf", width = 20, height = 20)
  grid.arrange(p0, p1, p2, p3, ncol = 2, nrow= 2);dev.off()
}
if(nrow(admin2) == nrow(Mexico)){
  pdf(file = "Plots/Viz_Mexico_mov_Air.pdf", width = 20, height = 20)
  grid.arrange(p0, p1, p2, p3, ncol = 2, nrow= 2);dev.off()
}

# F) Worldpop internal migration flows (state level)
p0 <- mov.mat.plot(dist_mat_Migration, "all")
p1 <- mov.mat.plot(dist_mat_Migration, "cities")
p2 <- mov.mat.plot(dist_mat_Migration, "towns")
p3 <- mov.mat.plot(dist_mat_Migration, "villages")

if(nrow(admin2) == nrow(Brazil)){
  pdf(file = "Plots/Viz_Brazil_mov_Migration.pdf", width = 20, height = 20)
  grid.arrange(p0, p1, p2, p3, ncol = 2, nrow= 2);dev.off()
}
if(nrow(admin2) == nrow(Mexico)){
  pdf(file = "Plots/Viz_Mexico_mov_Migration.pdf", width = 20, height = 20)
  grid.arrange(p0, p1, p2, p3, ncol = 2, nrow= 2);dev.off()
}

# E) MAP friction surface
p0 <- mov.mat.plot(dist_mat_Friction, "all", inverse = TRUE)
p1 <- mov.mat.plot(dist_mat_Friction, "cities", inverse = TRUE)
p2 <- mov.mat.plot(dist_mat_Friction, "towns", inverse = TRUE)
p3 <- mov.mat.plot(dist_mat_Friction, "villages", inverse = TRUE)

if(nrow(admin2) == nrow(Brazil)){
  pdf(file = "Plots/Viz_Brazil_mov_Friction.pdf", width = 20, height = 20)
  grid.arrange(p0, p1, p2, p3, ncol = 2, nrow= 2);dev.off()
}
if(nrow(admin2) == nrow(Mexico)){
  pdf(file = "Plots/Viz_Mexico_mov_Friction.pdf", width = 20, height = 20)
  grid.arrange(p0, p1, p2, p3, ncol = 2, nrow= 2);dev.off()
}



## key results:
# Gravity sends to such all to the big cities in the South East, but also has quite a lot of  local connectivity in medium size towns
# but very limited for rural areas

# Radiation has lots more mixing with big cities over longer distances and suggests much more connectivity in the northwest
# than Gravity, but does still have some local structure, especially at town and village level- lots of regional connectivity

# Air = Lots of links to northwest and northern hubs but nothing to more rural areas. Lots of connectivity dispersed over many 
# municiaplity pairs in the southeast.

# Migration = just lots of connectivity between areas in the Southeast.

# Friction = mostly short distance highly structured movement. At city level some regional connectivity, but few long distance jumps





