### cleans and reformats SINAN data from Brazil for the spread model
# also performs some basic summaries of the data such as number and percentage of 
# admin units colonised over time
# and does some analysis to support the assumption of continual presence of dengue
# after first arrival

# source of data:
# 2001-2006: http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sinanwin/cnv/denguebr.def
# 2007-2012: http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sinannet/cnv/denguebr.def
# 2013-: http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sinannet/cnv/denguebbr.def
# doing an extraction where lines = municipality of residence ("Município de residência") and columns = month of 1st symptoms ("Mês 1º Sintoma(s)")
# "clean" file names just involve .txt. -> .csv conversion, stripping unnecessary file headers and converting "-" to "0"s

rm(list = ls())

require(rgdal)
require(raster)
require(ggplot2)

setwd("/Users/eideobra/Dropbox/08_Serotype_spread/")

# IBGE municipality and state lookup file
IBGE_ref <- read.csv("Reference/GAUL_IBGE_conversion_full.csv")

state_files <- list.files("Data/Brazil_mun_dat")
# subset to just clean versions
state_files = state_files[grepl("_clean.csv", state_files)]

state_list = c("AC",
               "AL",
               "AP",
               "AM",
               "BA",
               "CE",
               "DF",
               "ES",
               "GO",
               "MA",
               "MT",
               "MS",
               "MG",
               "PA",
               "PB",
               "PR",
               "PE",
               "PI",
               "RJ",
               "RN",
               "RS",
               "RO",
               "RR",
               "SC",
               "SP",
               "SE",
               "TO")


for(i in 1:length(state_files)){
  f1 <- read.csv(paste0("Data/Brazil_mun_dat/", state_files[i]))
  f1$State = state_list[i]
  # split numeric and text municipality name
  if(any(is.na(as.numeric(sapply(f1$Municipality, function(x) strsplit(as.character(x), " ")[[1]][1]))))){break}
  f1$IBGE = as.numeric(sapply(f1$Municipality, function(x) strsplit(as.character(x), " ")[[1]][1]))
  f1$Mun_name = as.character(sapply(f1$Municipality, function(x) paste(strsplit(as.character(x), " ")[[1]][2:length(strsplit(as.character(x), " ")[[1]])],
                                                                       collapse = " ")))
  if(i == 1){bdat = f1}else{
    bdat = rbind(bdat, f1)
  }
}

# remove cases in unknown areas
bdat = bdat[bdat$IBGE != 0, ]
bdat = bdat[!grepl("ignorado", bdat[, 1]), ]

# quick summary stats:
# number of unique reports per year
#plot(aggregate(bdat$IBGE, by = list(bdat$Year), FUN = length)) # noisy



#####
# post 2012 data sources
#####


# nationwide files
nation_files <- list.files("Data/Brazil_mun_dat/2013_onwards/")
# subset to just clean versions
nation_files = nation_files[grepl("_clean.csv", nation_files)]

# loop through years, edit and bind together
for(i in 1:length(nation_files)){
  f1 <- read.csv(paste0("Data/Brazil_mun_dat/2013_onwards/", nation_files[i]))
  # add year
  f1$Year = i + 2012
  # split numeric and text municipality name
  if(any(is.na(as.numeric(sapply(f1$Municipality, function(x) strsplit(as.character(x), " ")[[1]][1]))))){break}
  IBGE_hold = as.numeric(sapply(f1$Municipality, function(x) strsplit(as.character(x), " ")[[1]][1]))
  f1$State = IBGE_ref$IBGE_STATE_CODE[match(IBGE_hold, IBGE_ref$IBGE_CODE)]
  f1$IBGE = IBGE_hold
  f1$Mun_name = as.character(sapply(f1$Municipality, function(x) paste(strsplit(as.character(x), " ")[[1]][2:length(strsplit(as.character(x), " ")[[1]])],
                                                                       collapse = " ")))
  if(i == 1){ndat = f1}else{
    ndat = rbind(ndat, f1)
  }
}

# remove cases in unknown areas
ndat = ndat[ndat$IBGE != 0, ]
ndat = ndat[!grepl("ignorado", ndat[, 1]), ]


# checking 2013 data from state extracts and nationwide extracts (nationwide should be more contemporary)
# total cases:
sum(colSums(bdat[bdat$Year == 2013, 2:13], na.rm = T)) # 74,573
sum(colSums(ndat[ndat$Year == 2013, 2:13], na.rm = T)) # vs 1,432,703 - a clear winner

bdat = bdat[!(bdat$Year == 2013), ]

# join datasets
bdat = rbind(bdat, ndat)






###########################
# matching to shapefiles
###########################

# lookup table to convert IBGE (brazilian) admin unit codes to international (GAUL) standards
conv_tab <- read.csv("Reference/GAUL_IBGE_conversion_full.csv")
# check if all IBGE codes match
table(bdat$IBGE %in% conv_tab$IBGE_CODE)
# 99% are there, so assign them GAUL codes
bdat$GAUL <- conv_tab$GAUL_CODE[match(bdat$IBGE, conv_tab$IBGE_CODE)]


# those that don't match:
miss_areas = unique(bdat[!(bdat$IBGE %in% conv_tab$IBGE_CODE), c(15, 16, 17)]) # 86 of them

# save them -> geoposition them in google maps then reload in and extract based on admin2 shapefiles
write.csv(miss_areas, file = "Data/No_match_IBGE.csv")
# load back in corrected results
miss_areas_gr <- read.csv("Data/No_match_IBGE_GR.csv")

# extract new GAULs from global Admin 2 shapefiles
admin2 <- readOGR(dsn = "Reference/Admin2(2011)/admin2.shp", layer = "admin2")
admin2 = admin2[admin2$COUNTRY_ID == "BRA", ]
miss_areas_gr$gaul = as.numeric(as.character(raster::extract(admin2, cbind(miss_areas_gr$Long, miss_areas_gr$Lat))$GAUL_CODE))
bdat$GAUL[!(bdat$IBGE %in% conv_tab$IBGE_CODE)] = miss_areas_gr$gaul[match(bdat$IBGE[!(bdat$IBGE %in% conv_tab$IBGE_CODE)],
                                                                           miss_areas_gr$IBGE)]


# combining multiple matches for GAUL code
bdat$rowID = 1:nrow(bdat)


for(i in 1:length(unique(bdat$GAUL))){
  f_dat = bdat[bdat$GAUL == unique(bdat$GAUL)[i], ]
  if(length(unique(f_dat$IBGE)) > 1){
    keep_rows = f_dat$rowID[f_dat$IBGE == f_dat$IBGE[1]]
    delete_rows = f_dat$rowID[f_dat$IBGE != f_dat$IBGE[1]]
    for(k in 1:length(delete_rows)){
      # if its duplicated at the level of year then add cases together
      # find year match
      year_match = match(bdat$Year[bdat$rowID == delete_rows[k]], bdat$Year[bdat$rowID %in% keep_rows])
      if(!is.na(year_match)){
        bdat[match(keep_rows[year_match], bdat$rowID), 2:13] = bdat[match(keep_rows[year_match], bdat$rowID), 2:13] + bdat[match(delete_rows[k], bdat$rowID), 2:13]
        bdat = bdat[bdat$rowID != delete_rows[k], ]
      }else{
        # else add a new row to the bottom of the df with one identifier
        template = bdat[match(keep_rows[1], bdat$rowID), ]
        template[, 2:13] = bdat[match(delete_rows[k], bdat$rowID), 2:13]
        # new rowID and year
        template$Year = bdat$Year[match(delete_rows[k], bdat$rowID)]
        if(is.na(template$Year)){print(paste("error at step", i, k))}
        template$rowID = max(bdat$rowID) + 1
        bdat = rbind(bdat, template)
        bdat = bdat[bdat$rowID != delete_rows[k], ]
      }
    }
  }
}

# check no more duplication of gaul codes
length(unique(bdat$IBGE)) == length(unique(bdat$GAUL))

# check for duplications of year within a municipality (rare but present)
Y_dup <- table(bdat$IBGE, bdat$Year)
Y_dup = Y_dup[apply(Y_dup, 1, function(x) any(x > 1)), ]
# convert to a list with the duplicated years for each mun
Y_dup_list = apply(Y_dup, 1, function(x) names(x[x > 1]))

for(i in 1:length(Y_dup_list)){
  # next loop for duplicated years (some municipalities have more than one year duplicated)
  for(j in 1:length(Y_dup_list[[i]])){
    # find the duplicated rows
    dr <- bdat[(bdat$IBGE == as.numeric(names(Y_dup_list)[i])) & (bdat$Year == as.numeric(Y_dup_list[[i]][j])),  "rowID"]
    # now combine and delete duplicated rows
    bdat[bdat$rowID == dr[1], 2:13] = bdat[bdat$rowID == dr[1], 2:13] + bdat[bdat$rowID == dr[2:length(dr)], 2:13]
    bdat = bdat[!(bdat$rowID %in% dr[2:length(dr)]), ]
  }
}

# quick chekc that it has worked (should == FALSE)
any(table(bdat$IBGE, bdat$Year) > 1)


# reset row IDs
bdat$rowID = 1:nrow(bdat)

# add region of Brazil
bdat$Region = conv_tab$Region[match(bdat$GAUL, conv_tab$GAUL_CODE)]

# final small ad-hoc edit to replace NAs (strange coding) in 2002 with 0s
bdat$Out[is.na(bdat$Out)] = 0

# save compiled dataset
write.csv(bdat, file = "Data/Brazil_mun_dat_cleaned.csv")













# number of new areas reporting each yuear (assume dengue sticks around once introduced)
bdat$Total <- rowSums(bdat[, c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", 
                               "Jul", "Ago", "Set", "Out", "Nov", "Dez")], na.rm = T)
# add incidence
mun_info <- read.csv("Reference/GAUL_IBGE_conversion_full.csv")
bdat$pop = mun_info$IBGE_MUN_POP[match(bdat$GAUL, mun_info$GAUL_CODE)]
bdat$Total_incid = bdat$Total / bdat$pop
# remove NAs
bdat = bdat[!is.na(bdat$GAUL), ]

# threshold options
Tcase_opts <- data.frame(Thresh = seq(1,100, 1), # 1-100 cases,
                         Num_start = NA,  #number invaded in 2001
                         Num_end = NA,
                         Avg_persistence = NA) # years Thresh exceeded after arrival
Tincid_opts <- data.frame(Thresh = seq(1,100, 1) / 10000, # 1-100 cases per 10,000
                          Num_start = NA,
                          Num_end = NA,
                          Avg_persistence = NA)

# start and end numbers
bdat_start = bdat[bdat$Year == 2001, ]
u_GAULs = unique(bdat$GAUL)

for(i in 1:nrow(Tcase_opts)){
  # start
  Tcase_opts$Num_start[i] = sum(bdat_start$Total >=  Tcase_opts$Thresh[i])
  Tincid_opts$Num_start[i] = sum(bdat_start$Total_incid >=  Tincid_opts$Thresh[i])
  
  # end
  Tcase_opts$Num_end[i] = length(unique(bdat$GAUL[bdat$Total >=  Tcase_opts$Thresh[i]]))
  Tincid_opts$Num_end[i] = length(unique(bdat$GAUL[bdat$Total_incid >=  Tincid_opts$Thresh[i]]))
  
  # persistence
  
  # work out invasion year
  case_inv <- bdat[bdat$Total >= Tcase_opts$Thresh[i], ]
  inv_year = aggregate(Year ~ GAUL, data = case_inv, FUN = min)
  inv_year$Year = inv_year$Year - min(inv_year$Year) + 1
  
  # table presence of any dengue cases (> 0) by year
  case_tab <- table(bdat$GAUL, bdat$Year)
  
  # persistence in years since invasion
  pers_list = sapply(inv_year$Year, function(x) seq(x, ncol(case_tab), 1))
  pers = rep(NA, length(pers_list))
  for(k in 1:length(pers_list)){
    rec <- case_tab[match(inv_year$GAUL[k], row.names(case_tab)), pers_list[[k]]]
    # remove invasion year
    rec = rec[-1]
    pers[k] = sum(rec) / length(rec)
  }
  Tcase_opts$Avg_persistence[i] = mean(pers, na.rm =T)
  
  # for incidence now
  # work out invasion year
  incid_inv <- bdat[bdat$Total_incid >= Tincid_opts$Thresh[i], ]
  inv_year = aggregate(Year ~ GAUL, data = incid_inv, FUN = min)
  inv_year$Year = inv_year$Year - min(inv_year$Year) + 1
  
  # table presence of any dengue cases (> 0) by year
  incid_tab <- table(bdat$GAUL, bdat$Year)
  
  # persistence in years since invasion
  pers_list = sapply(inv_year$Year, function(x) seq(x, ncol(incid_tab), 1))
  pers = rep(NA, length(pers_list))
  for(k in 1:length(pers_list)){
    rec <- incid_tab[match(inv_year$GAUL[k], row.names(incid_tab)), pers_list[[k]]]
    # remove invasion year
    rec = rec[-1]
    pers[k] = sum(rec) / length(rec)
  }
  Tincid_opts$Avg_persistence[i] = mean(pers, na.rm =T)
}


# calcualte number invaded over the timeframe
Tcase_opts$Num_invaded = Tcase_opts$Num_end - Tcase_opts$Num_start
Tincid_opts$Num_invaded = Tincid_opts$Num_end - Tincid_opts$Num_start

# turn number invaded to percentage
Tcase_opts$Perc_invaded = Tcase_opts$Num_invaded / max(Tcase_opts$Num_end)
Tincid_opts$Perc_invaded = Tincid_opts$Num_invaded / max(Tcase_opts$Num_end)

# composite score
Tcase_opts$Index = sqrt((1 - Tcase_opts$Avg_persistence)^2 + (1 - Tcase_opts$Perc_invaded)^2)
Tincid_opts$Index = sqrt((1 - Tincid_opts$Avg_persistence)^2 + (1 - Tincid_opts$Perc_invaded)^2)

# identify best
Tcase_opts$best = c("black", "red")[(Tcase_opts$Index == min(Tcase_opts$Index)) + 1]
Tincid_opts$best = c("black", "red")[(Tincid_opts$Index == min(Tincid_opts$Index)) + 1]

par(mfrow = c(1,2))
plot(Tcase_opts$Perc_invaded, Tcase_opts$Avg_persistence,xlim = c(0.5, 0.7), ylim = c(0.6, 1),
     col = Tcase_opts$best)
plot(Tincid_opts$Perc_invaded, Tincid_opts$Avg_persistence, xlim = c(0.5, 0.7), ylim = c(0.6, 1),
     col = Tcase_opts$best)

p1 <- ggplot(Tcase_opts, aes(x = Perc_invaded * 100, y = Avg_persistence * 100, color = Thresh)) + 
  geom_point() +
  labs(color = "Threshold \n (cases per year)") +
  xlab("% of dengue-reporting municipalities invaded 2001-2019") +
  ylab("% of years dengue is reported after invasion") +
  geom_hline(yintercept = Tcase_opts$Avg_persistence[Tcase_opts$best == "red"] * 100, 
             color = "red", linetype = "dashed")+
  geom_vline(xintercept = Tcase_opts$Perc_invaded[Tcase_opts$best == "red"] * 100, 
             color = "red", linetype = "dashed") +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100))

p2 <- ggplot(Tincid_opts, aes(x = Perc_invaded * 100, y = Avg_persistence * 100, color = Thresh * 10000)) + 
  geom_point() +
  labs(color = "Threshold \n (cases per 10,000 \n per year)") +
  xlab("% of dengue-reporting municipalities invaded 2001-2019") +
  ylab("% of years dengue is reported after invasion") +
  geom_hline(yintercept = Tincid_opts$Avg_persistence[Tincid_opts$best == "red"] * 100, 
             color = "red", linetype = "dashed")+
  geom_vline(xintercept = Tincid_opts$Perc_invaded[Tincid_opts$best == "red"] * 100, 
             color = "red", linetype = "dashed") +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100))


# incidence based threshold better than case-based threshold
# best option:
Tincid_opts[Tincid_opts$best == "red", ]
Bra_thesh = Tincid_opts$Thresh[Tincid_opts$best == "red"]

# 0.002 = 20 cases per 10,000 people
# gives average persistence of 0.837 and a percent invaded of 0.695
# (percent invaded is % of municipalities reporting any dengue at any point)


ggsave(filename = "Plots/SI_Brazil_Invasion_threshold_cases.pdf", p1, width = 8, height = 5)
ggsave(filename = "Plots/SI_Brazil_Invasion_threshold_incidence.pdf", p2, width = 8, height = 5)

# save new version of the spread data with the threshold already applied and subsetted to only first invasion of each municipality
# apply threshold
bdat = bdat[bdat$Total_incid >= Bra_thesh, ]

# universal formatting
bdat$year = bdat$Year
bdat$GAUL_A2 = bdat$GAUL

# deleting records after first year of arrival
bdat = bdat[order(bdat$year), ]
bdat = bdat[!duplicated(bdat$GAUL_A2), ]

# save
write.csv(bdat, "Data/Brazil_mun_dat_cleaned_thresholded.csv")


# identify invasion years for 5 biggest cities that were observed to be invaded for War games maps
bdat_order <- bdat[order(bdat$pop, decreasing = T), ]
bdat_order <- bdat_order[bdat_order$year > min(bdat_order$year), ]
# Sao Paulo 2014
# Salvador 2015
# Brasilia 2010
# Fortaleza 2003
# Belo Horizonte 2007
brazil_big_cities <- data.frame(GAUL = bdat_order$GAUL_A2[1:5], 
                                Name = bdat_order$Mun_name[1:5], 
                                Year = bdat_order$year[1:5])
# save big cities for later loading in
write.csv(brazil_big_cities, file = "Data/Intermediate_datasets/Brazil_big_cities.csv")









################################
# plots of spread data in Brazil
################################


# admin shapefile
admin0 <- readOGR(dsn = "Reference/Admin0(2011)/admin0.shp", layer = "admin0")
admin1 <- readOGR(dsn = "Reference/Admin1(2011)/admin1.shp", layer = "admin1")
admin2 <- readOGR(dsn = "Reference/Admin2(2011)/admin2.shp", layer = "admin2")
## trim to Brazil
admin0 = admin0[admin0$NAME == "Brazil", ]
admin1 = admin1[admin1$COUNTRY_ID == "BRA", ]
admin2 = admin2[admin2$COUNTRY_ID == "BRA", ]

# apply threshold
bdat_thresh <- bdat[bdat$Total_incid >= Bra_thesh, ]


## collapse to first admin2 report
first_intro = aggregate(bdat_thresh$Year, by = list(bdat_thresh$GAUL), FUN = min)

# assign back to shapefile
admin2$Den_intro_report <- NA
for(i in 1:nrow(first_intro)){
  admin2$Den_intro_report[match(first_intro$Group.1[i], admin2$GAUL_CODE)] = first_intro$x[i]
}

# admin0 without Brazil for basemmaps
admin0All <- readOGR(dsn = "Reference/Admin0(2011)/admin0.shp", layer = "admin0")
admin0_NoBRA = admin0All[admin0All$NAME != "Brazil", ]

# plotting
g1 <- tm_shape(admin2) +
  tm_polygons("Den_intro_report", border.alpha = 0, title = "First year dengue reported",
              palette = "Reds", n = 10, legend.reverse = TRUE,
              textNA = "Dengue absent as of 2020",
              colorNA = "lightgrey") +
  tm_shape(admin0_NoBRA) +
  tm_fill(col = "darkgrey") +
  tm_shape(admin0All) +
  tm_borders() +
  tm_layout(bg.color = "lightblue",
            legend.format=list(fun=function(x) formatC(x, digits=0, format="d"))) +
  tm_legend(bg.color = "white", frame = T)
tmap_save(g1, file = "Plots/Den_introDate_Brazil_program.pdf")

# cumulative infected municipalities over time
muns_year <- table(bdat_thresh$Year)
muns_year <- data.frame(Year = as.numeric(names(muns_year)),
                        Municipalities = cumsum(muns_year))
#muns_year$Year = as.factor(muns_year$Year)

p1 <- ggplot(muns_year, aes(x = Year, y = Municipalities)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Cumulative municipalities", limits = c(0, nrow(admin2)), n.breaks = 10) +
  scale_x_continuous(n.breaks = 10) +
  theme_bw()

p1
ggsave(filename = "Plots/Cumulative_muns_infected_Brazil.pdf", height = 3, width = 4, p1)


