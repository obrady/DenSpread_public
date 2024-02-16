### cleans and reformats SINAVE data from Mexico for the spread model
# also performs some basic summaries of the data such as number and percentage of 
# admin units colonised over time
# and does some analysis to support the assumption of continual presence of dengue
# after first arrival

# source of data:
# Sent direct from felpe Colon after FOI request
# data has changing definitons:
# 2000-2008: DHF only
# 2009-2015 DF + DHF
# 2016-2019 dengue (warnign signs) + severe dengue + non severe dengue
# all figures in this dataset are the sum of al these categories

# new data direct from Mexico MoH for 1995-1999
# contains "confirmed cases" - assume DF.

rm(list = ls())

require(rgdal)
require(raster)
require(sf)
require(tmap)
require(ggplot2)
require(wktmo)

setwd("/Users/eideobra/Dropbox/08_Serotype_spread/")

# load in data raw
Mdat <- read.csv("Data/Mexico_dat/Mexico_DHF_D_dat.csv")
Mdat_pre2000 <- read.csv("Data/Mexico_dat/Mexico_D_dat_95_2000.csv")

# format pre-2000 data and merge with post 2000 data
# amalgamate all to be annual data
Mdat$Anncases = rowSums(Mdat[, 4:ncol(Mdat)])
Mdat = data.frame(Mdat[, c("Municipality", "State", "Year", "Anncases")])
Mdat_pre2000$Anncases = rowSums(Mdat_pre2000[, 4:ncol(Mdat_pre2000)])
Mdat_pre2000 = data.frame(Mdat_pre2000[, c("Municipality_name", "State", "Year", "Anncases")])
names(Mdat_pre2000)[1] = "Municipality"
# preliminary trim to areas that only report >= 5 cases a year due to significant challenges with string matching
Mdat_pre2000 = Mdat_pre2000[Mdat_pre2000$Anncases >= 5, ] # trims from 1346 to 93 areas
Mdat = rbind(Mdat_pre2000, Mdat)


# remove cases in unknown areas (removes 36 rows and 4 rows respectively)
Mdat = Mdat[Mdat[, 1] != "(en blanco)", ]



# ok load in Shapefiles
#admin1 <- readOGR(dsn = "Reference/Admin1(2011)/admin1.shp", layer = "admin1")
#admin2 <- readOGR(dsn = "Reference/Admin2(2011)/admin2.shp", layer = "admin2")
#admin1 = admin1[admin1$COUNTRY_ID == "MEX", ]
#admin2 = admin2[admin2$COUNTRY_ID == "MEX", ]
# now going with shapefiles downloaded from HDx 
# (https://data.humdata.org/dataset/mexican-administrative-level-0-country-1-estado-and-2-municipio-boundary-polygons)
admin1 <- readOGR(dsn = "Data/Mexico_dat/MEX_shapefiles/mex_admbnda_adm1_govmex_20200618.shp", layer = "mex_admbnda_adm1_govmex_20200618")
admin2 <- readOGR(dsn = "Data/Mexico_dat/MEX_shapefiles/mex_admbnda_adm2_govmex_20200618.shp", layer = "mex_admbnda_adm2_govmex_20200618")


### State check ###

# ok trim trailing white space issue
Mdat[, 1] = as.character(Mdat[, 1])
Mdat[, 2] = as.character(Mdat[, 2])
Mdat[, 1] = trimws(Mdat[, 1], "right")
Mdat[, 2] = trimws(Mdat[, 2], "right")

sc1 <- sort(unique(Mdat$State))
sc2 <- toupper(as.character(sort(unique(admin1$ADM1_ES))))
state_comp <- data.frame(data = sc1,
                         shapefile = c(sc2, rep("NA", length(sc1) - length(sc2))))
state_comp

# specific manual corrections




Mdat[Mdat[, 2] %in% c("BCS"), 2] = "BAJA CALIFORNIA SUR"
Mdat[Mdat[, 2] %in% c("MICHOACAN", "MICHOACÁN"), 2] = "MICHOACÁN DE OCAMPO"
Mdat[Mdat[, 2] %in% c("ESTADO DE MEXICO", "MEXICO"), 2] = "MÉXICO"
Mdat[Mdat[, 2] %in% c("COAHUILA"), 2] = "COAHUILA DE ZARAGOZA"
Mdat[Mdat[, 2] %in% c("CAMPECHEAS"), 2] = "CAMPECHE"
Mdat[Mdat[, 2] %in% c("NUAVO LEON", "NUEVO LEON", "NUEVO LEÓN"), 2] = "NUEVO LEÓN"
Mdat[Mdat[, 2] %in% c("ORIZABA"), 2] = "VERACRUZ"
Mdat[Mdat[, 2] %in% c("QUERETARO"), 2] = "QUERÉTARO DE ARTEAGA"
Mdat[Mdat[, 2] %in% c("SAN  LUIS POTOSI", "SAN LUIS POTOSI"), 2] = "SAN LUIS POTOSÍ"
Mdat[Mdat[, 2] %in% c("TAMPICO"), 2] = "TAMAULIPAS"
Mdat[Mdat[, 2] %in% c("TAMULIPAS"), 2] = "TAMAULIPAS"
Mdat[Mdat[, 2] %in% c("URSULO GALVAN"), 2] = "VERACRUZ"
Mdat[Mdat[, 2] %in% c("VERACRUZ"), 2] = "VERACRUZ DE IGNACIO DE LA LLAVE"
Mdat[Mdat[, 2] %in% c("YUCATAN"), 2] = "YUCATÁN"

table(Mdat[, 2] %in% sc2)



### Municipality check ###

require(stringi)

# load in state abbreviations table (improves string matching)
abb_tab <- read.csv("Data/Mexico_dat/State_abbreviations.csv")
admin1$ADM1_ABB <- abb_tab$State_abb[match(admin1$ADM1_ES, abb_tab$State_name)]
admin2$ADM1_ABB <- abb_tab$State_abb[match(admin2$ADM1_ES, abb_tab$State_name)]

# combined municipality and state name
Mdat$ADM1_ABB = admin2$ADM1_ABB[match(Mdat$State, toupper(admin2$ADM1_ES))]
Mdat$MunDetailed = apply(Mdat[, c("Municipality", "ADM1_ABB")], 1, function(x) paste(stri_trans_general(x[1], "Latin-ASCII"),
                                                                                       x[2], sep = " /-/ "))
admin2$MunDetailed = apply(as.data.frame(admin2)[, c("ADM2_ES", "ADM1_ABB")], 1, function(x) paste(stri_trans_general(toupper(x[1]), "Latin-ASCII"), 
                                                                                             toupper(x[2]), 
                                                                                             sep = " /-/ "))

Data_Mnames <- unique(Mdat$MunDetailed)
Shape_Mnames <- unique(admin2$MunDetailed)
table(Data_Mnames %in% Shape_Mnames)

# ok separate into those with exact matches and those without
Mun_lookuptab = data.frame(Data_name = Data_Mnames[Data_Mnames %in% Shape_Mnames],
                           Shape_name = Shape_Mnames[match(Data_Mnames[Data_Mnames %in% Shape_Mnames],
                                                           Shape_Mnames)])
Data_Mnames = Data_Mnames[!(Data_Mnames %in% Mun_lookuptab$Data_name)]
original_Data_Mnames = Data_Mnames

# manual editing to fix state abbreviations in the municipality text strings
# and reversing LA, LAS, LOS and EL to always prefix the municipality
# spelling out abbreviations: CD. -> CUIDAD, COL. -> Colonia
#write.csv(Data_Mnames, file = "Data/Mexico_dat/Municipality_name_edits.csv")
# laod back in and see how many fixed
manual_correct <- read.csv("Data/Mexico_dat/Municipality_name_edits_corrected.csv")
Data_Mnames = manual_correct$correction
table(Data_Mnames %in% Shape_Mnames) # fixes another 110

Mun_lookuptab = rbind(Mun_lookuptab, data.frame(Data_name = original_Data_Mnames[Data_Mnames %in% Shape_Mnames],
                                                Shape_name = Shape_Mnames[match(Data_Mnames[Data_Mnames %in% Shape_Mnames],
                                                                                Shape_Mnames)]))
Data_Mnames = original_Data_Mnames[!(Data_Mnames %in% Shape_Mnames)]



# ok going to need a fuzzy matching algorithm
require(stringdist)

d <- expand.grid(Data_Mnames, Shape_Mnames) # Distance matrix in long form
names(d) <- c("D_name","S_name")
d$dist <- stringdist(d$D_name,d$S_name, method="jw") # String edit distance (use your favorite function here)

# inspect differences
d = d[order(d$dist), ]
#d[400:500, ]
# now matching algorithm
Data_Mnames <- data.frame(Data_Name = Data_Mnames,
                          Shape_match_any = "NA",
                          Shape_match_In_state = "NA",
                          Match_dist_any = NA,
                          Match_dist_In_state = NA,
                          stringsAsFactors = FALSE)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

for(i in 1:nrow(Data_Mnames)){
  subset = d[d$D_name == Data_Mnames$Data_Name[i], ]
  subset$statematch = sapply(as.character(subset$D_name), substrRight, n = 3) == 
    sapply(as.character(subset$S_name), substrRight, n = 3)
  
  Data_Mnames$Shape_match_any[i] = as.character(subset$S_name[1])
  Data_Mnames$Match_dist_any[i] = subset$dist[1]
  
  subset = subset[subset$statematch, ]
  Data_Mnames$Shape_match_In_state[i] = as.character(subset$S_name[1])
  Data_Mnames$Match_dist_In_state[i] = subset$dist[1]
  
}

Data_Mnames = Data_Mnames[order(Data_Mnames$Match_dist_any), ]
# now write for manual inspection
#write.csv(Data_Mnames, file = "Data/Mexico_dat/Municipality_name_edits_afterFuzzy.csv")

# ok load back in after manual checking
Data_Mnames <- read.csv("Data/Mexico_dat/Municipality_name_edits_afterFuzzy_processed.csv")

# records to be manually deleted because no match can be found or because regional summaries 
# have crept through
rec_del <- c("GUERRERO /-/ GRO", "YUCATAN /-/ YUC", "OAXACA /-/ OAX", "COLOSIO /-/ GRO", "SIN DATO /-/ YUC",
             "NAYARIT /-/ NAY", "LA.CARDENAS /-/ MIC", "COSTA CHICA /-/ GRO", "POTRERO NUEVO /-/ VER", " /-/ CAM",
             "SAN SENOBIO /-/ VER", "LAGUNA DEL ROBLE /-/ VER", "STA. MARIA HCO /-/ OAX",
             "S M JAL DEL MARQUEZ /-/ OAX", "S.R.E. /-/ GRO", "ROSARIO /ESCUINAPA /-/ SIN", "SIN DATOS /-/ SON",
             "LA UNION REYES /-/ VER", "SE DESCONOCE /-/ TAM", "SE DESCONOCE /-/ OAX", "JUAN R.E. /-/ GRO",
             "CIUDAD VENUSTIANO CARRANZA /-/ JAL", "ALARCON RAMIREZ RITA /-/ VER", "UNION /-/ MIC")


Data_Mnames$FINAL_LINK <- as.character(Data_Mnames$Shape_match_any)
Data_Mnames$FINAL_LINK[!(Data_Mnames$Choose_bestMatch)] = NA

geo_locs <- Data_Mnames[!is.na(Data_Mnames$Lat), ]
coordinates(geo_locs) <- ~ Long + Lat
proj4string(geo_locs) <- proj4string(admin2)
geo_match  = over(geo_locs, admin2)
Data_Mnames$FINAL_LINK[!is.na(Data_Mnames$Lat)] = geo_match$MunDetailed

Data_Mnames$FINAL_LINK[(Data_Mnames$Data_Name %in% rec_del)] = NA

# ok now append to the municipality lookup table
Mun_lookuptab = rbind(Mun_lookuptab, data.frame(Data_name = Data_Mnames$Data_Name,
                                                Shape_name = Data_Mnames$FINAL_LINK))
#Mun_lookuptab = Mun_lookuptab[!is.na(Mun_lookuptab$Shape_name), ]
# now save
#save(Mun_lookuptab, file = "Data/Mexico_dat/Municipality_lookup_table.csv")

# reconciliation
admin2$GAUL_CODE = as.numeric(gsub("MX", "", admin2$ADM2_PCODE))

# exact matches
Mdat$GAUL_CODE = NA
Mdat$GAUL_CODE[Mdat$MunDetailed %in% admin2$MunDetailed] = admin2$GAUL_CODE[match(Mdat$MunDetailed[Mdat$MunDetailed %in% admin2$MunDetailed],
                                                                                  admin2$MunDetailed)]
summary(Mdat$GAUL_CODE)

# lookup table matches
shape_match = Mun_lookuptab$Shape_name[match(Mdat$MunDetailed[is.na(Mdat$GAUL_CODE)],
                                             Mun_lookuptab$Data_name)]
Mdat$GAUL_CODE[is.na(Mdat$GAUL_CODE)] = admin2$GAUL_CODE[match(shape_match, admin2$MunDetailed)]

summary(Mdat$GAUL_CODE)

stillMissing = sort(unique(Mdat$MunDetailed[is.na(Mdat$GAUL_CODE)]))

# 23 of them are supposed to be NAs what about the final 13?
table(stillMissing %in% rec_del)

stillMissing = stillMissing[!(stillMissing %in% rec_del)]

# ok add back in the commas
Mdat$MunDetailed[Mdat$MunDetailed == "NAYAR EL /-/ NAY"] = "NAYAR, EL /-/ NAY"
Mdat$MunDetailed[Mdat$MunDetailed == "UNION LA /-/ GRO"] = "UNION, LA /-/ GRO"
Mdat$MunDetailed[Mdat$MunDetailed == "CD DEL CARMEN /-/ CAM"] = "CARMEN /-/ CAM"
Mdat$MunDetailed[Mdat$MunDetailed == "NAYAR, EL /-/ NAY"] = Mun_lookuptab$Shape_name[match("NAYAR, EL /-/ NAY",
                                                                               Mun_lookuptab$Data_name)]
Mdat$MunDetailed[Mdat$MunDetailed == "UNION, LA /-/ GRO"] = Mun_lookuptab$Shape_name[match("UNION, LA /-/ GRO",
                                                                                         Mun_lookuptab$Data_name)]
Mdat$GAUL_CODE[Mdat$MunDetailed == "DEL NAYAR /-/ NAY"] = admin2$GAUL_CODE[match("DEL NAYAR /-/ NAY",
                                                                                 admin2$MunDetailed)]
Mdat$GAUL_CODE[Mdat$MunDetailed == "LA UNION DE ISIDORO MONTES DE OCA /-/ GRO"] = admin2$GAUL_CODE[match("LA UNION DE ISIDORO MONTES DE OCA /-/ GRO",
                                                                                 admin2$MunDetailed)]



# ok now delete the ones without geographic matches
Mdat = Mdat[!is.na(Mdat$GAUL_CODE), ]

# now aggregation of province data

U_Years = unique(Mdat$Year)

for(i in 1:length(U_Years)){
  f_dat = Mdat[Mdat$Year == U_Years[i], ]
  if(any(duplicated(f_dat$GAUL_CODE))){
    DUPS = f_dat$GAUL_CODE[duplicated(f_dat$GAUL_CODE)]
    for(k in 1:length(DUPS)){
      # case addition
      Mdat[(Mdat$GAUL_CODE == DUPS[k]) & (Mdat$Year == U_Years[i]), ][1, 4] = sum(Mdat[(Mdat$GAUL_CODE == DUPS[k]) & (Mdat$Year == U_Years[i]), ][1, 4],
                                                                                  Mdat[(Mdat$GAUL_CODE == DUPS[k]) & (Mdat$Year == U_Years[i]), ][-1, 4])
      # row deletion
      rowCand = (1:nrow(Mdat))[(Mdat$GAUL_CODE == DUPS[k]) & (Mdat$Year == U_Years[i])]
      rowCand = rowCand[2:length(rowCand)]
      Mdat = Mdat[!((1:nrow(Mdat)) %in% rowCand), ]
    }
  }
}


# final formatting
Mdat = data.frame(Municipality = Mdat$Municipality,
                  Ann_cases = Mdat[, 4],
                  Year = Mdat$Year,
                  State = Mdat$ADM1_ABB,
                  MunDetailed = Mdat$MunDetailed,
                  GAUL_CODE = Mdat$GAUL_CODE)

# !!! DHF -> DF conversion using annual aggregated ratios from:
# Dantes et al. 2014 (Table 1) https://journals.plos.org/plosntds/article?id=10.1371/journal.pntd.0003158
Mdat$Ann_cases[Mdat$Year == 2000] = Mdat$Ann_cases[Mdat$Year == 2000] * (25.6 + 1)
Mdat$Ann_cases[Mdat$Year == 2001] = Mdat$Ann_cases[Mdat$Year == 2001] * (14.9 + 1)
Mdat$Ann_cases[Mdat$Year == 2002] = Mdat$Ann_cases[Mdat$Year == 2002] * (6.13 + 1)
Mdat$Ann_cases[Mdat$Year == 2003] = Mdat$Ann_cases[Mdat$Year == 2003] * (2.94 + 1)
Mdat$Ann_cases[Mdat$Year == 2004] = Mdat$Ann_cases[Mdat$Year == 2004] * (3.18 + 1)
Mdat$Ann_cases[Mdat$Year == 2005] = Mdat$Ann_cases[Mdat$Year == 2005] * (3.95 + 1)
Mdat$Ann_cases[Mdat$Year == 2006] = Mdat$Ann_cases[Mdat$Year == 2006] * (4.76 + 1)
Mdat$Ann_cases[Mdat$Year == 2007] = Mdat$Ann_cases[Mdat$Year == 2007] * (4.55 + 1)
Mdat$Ann_cases[Mdat$Year == 2008] = Mdat$Ann_cases[Mdat$Year == 2008] * (3.69 + 1)

# remove one 0 case report
Mdat = Mdat[Mdat$Ann_cases > 0, ]

# now save
write.csv(Mdat, "Data/Mexico_mun_dat_cleaned.csv")



#######################################
# Threshold analysis
#######################################

# add incidence
Mexico = readOGR(dsn = "Data/Mexico_dat/MEX_shapefiles/mex_admbnda_adm2_govmex_20200618.shp", layer = "mex_admbnda_adm2_govmex_20200618")
mun_info2 <- read.csv("Data/Mexico_dat/mex_popp_govmex.csv")
mun_info2$pobtot = as.numeric(gsub(",", "", mun_info2$pobtot))
mun_info2 <- aggregate(pobtot ~ adm2code, data = mun_info2, FUN = sum)

Mexico$GAUL_CODE = as.numeric(gsub("MX", "", Mexico$ADM2_PCODE))
Mexico$Pop = mun_info2$pobtot[match(Mexico$ADM2_PCODE, mun_info2$adm2code)]
Mdat$Pop = Mexico$Pop[match(Mdat$GAUL_CODE, Mexico$GAUL_CODE)]
Mdat$Total_incid = Mdat$Ann_cases / Mdat$Pop


# renaming for ease
bdat = Mdat
bdat$Total = bdat$Ann_cases
bdat$GAUL = bdat$GAUL_CODE

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
bdat_start = bdat[bdat$Year == 1995, ]
u_GAULs = unique(bdat$GAUL_CODE)

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


p1 <- ggplot(Tcase_opts, aes(x = Perc_invaded * 100, y = Avg_persistence * 100, color = Thresh)) + 
  geom_point() +
  labs(color = "Threshold \n (cases per year)") +
  xlab("% of dengue-reporting municipalities invaded 1995-2019") +
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
  xlab("% of dengue-reporting municipalities invaded 1995-2019") +
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
Mex_thesh = Tincid_opts$Thresh[Tincid_opts$best == "red"]


# 0.0002 = 2 cases per 10,000 people
# gives average persistence of 0.650 and a percent invaded of 0.84
# (percent invaded is % of municipalities reporting any dengue at any point)

ggsave(filename = "Plots/SI_Mexico_Invasion_threshold_cases.pdf", p1, width = 8, height = 5)
ggsave(filename = "Plots/SI_Mexico_Invasion_threshold_incidence.pdf", p2, width = 8, height = 5)


# save new version of the spread data with the threshold already applied and subsetted to only first invasion of each municipality

# apply threshold
Mdat = Mdat[Mdat$Total_incid >= Mex_thesh, ]

# universal formatting
Mdat$year = Mdat$Year
Mdat$GAUL_A2 = Mdat$GAUL_CODE

# deleting records after first year of arrival
Mdat = Mdat[order(Mdat$year), ]
Mdat = Mdat[!duplicated(Mdat$GAUL_A2), ]

# save
write.csv(Mdat, "Data/Mexico_mun_dat_cleaned_thresholded.csv")

# identify invasion years for 5 biggest cities that were observed to be invaded for War games maps
Mdat_order <- Mdat[order(Mdat$Pop, decreasing = T), ]
Mdat_order <- Mdat_order[Mdat_order$year > min(Mdat_order$year), ]
# Zapopan (Guadelajara) (2009)
# Leon (2017)
# Monterrey (2005)
# Mexicali (2015)
# Culiacan (2002)

Mexico_big_cities <- data.frame(GAUL = Mdat_order$GAUL_A2[1:6], 
                                Name = Mdat_order$Municipality[1:6], 
                                Year = Mdat_order$year[1:6])
# remove guadelajara(replace with Zapopan)
Mexico_big_cities = Mexico_big_cities[2:6, ]

# save big cities for later loading in
write.csv(Mexico_big_cities, file = "Data/Intermediate_datasets/Mexico_big_cities.csv")







#######################################
# visualisation of invasion over time
#######################################

admin0 <- readOGR(dsn = "Reference/Admin0(2011)/admin0.shp", layer = "admin0")
admin2 = readOGR(dsn = "Data/Mexico_dat/MEX_shapefiles/mex_admbnda_adm2_govmex_20200618.shp", layer = "mex_admbnda_adm2_govmex_20200618")
admin2$GAUL_CODE = as.numeric(gsub("MX", "", admin2$ADM2_PCODE))

# rename
Mdat_thresh <- Mdat


## collapse to first admin2 report
first_intro = aggregate(Mdat_thresh$Year, by = list(Mdat_thresh$GAUL_CODE), FUN = min)

# match first invasion year to shape file
admin2$Den_intro_report = first_intro$x[match(admin2$GAUL_CODE, first_intro$Group.1)]


admin0_NoMEX = admin0[admin0$NAME != "Mexico", ]

g1 <- tm_shape(admin2) +
  tm_polygons("Den_intro_report", border.alpha = 0, title = "First year dengue reported",
              palette = "Reds", n = 10, legend.reverse = TRUE,
              textNA = "Dengue absent as of 2020",
              colorNA = "lightgrey") +
  #tm_layout(legend.format=list(fun=function(x) formatC(x, digits=0, format="d"))) +
  tm_shape(admin0_NoMEX) +
  tm_fill(col = "darkgrey") +
  tm_shape(admin0) +
  tm_borders() +
  tm_legend(bg.color = "white", frame = T)
  
tmap_save(g1, file = "Plots/Den_introDate_Mexico_program.pdf")


# cumulative infected municipalities over time
muns_year <- table(first_intro$x)
muns_year <- data.frame(Year = as.numeric(names(muns_year)),
                        Municipalities = cumsum(muns_year))
#muns_year$Year = as.factor(muns_year$Year)

p1 <- ggplot(muns_year, aes(x = Year, y = Municipalities)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Cumulative municipalities", limits = c(0, nrow(admin2))) +
  scale_x_continuous(n.breaks = 5) +
  theme_bw()

p1
ggsave(filename = "Plots/Cumulative_muns_infected_Mexico.pdf", height = 3, width = 4, p1)

