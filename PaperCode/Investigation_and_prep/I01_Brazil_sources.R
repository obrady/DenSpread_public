rm(list = ls())

require(raster)
require(ggplot2)
require(geobr)

# Occurrence points
occ <- read.csv("Dropbox/C1_Reference/Denguemap/Serotype_spread/OccurenceDB_17012013.csv")
# lat long trim
occ = occ[occ$X < -30, ]
occ = occ[occ$X > -80, ]
occ = occ[occ$Y < 10, ]
occ = occ[occ$Y > -40, ]

occ_1985 = occ[occ$year == 1985, 13:14]
occ_1986 = occ[occ$year == 1986, 13:14]
occ_1987 = occ[occ$year == 1987, 13:14]
occ_1988 = occ[occ$year == 1988, 13:14]
occ_1989 = occ[occ$year == 1989, 13:14]
occ_1990 = occ[occ$year == 1990, 13:14]
occ_1991 = occ[occ$year == 1991, 13:14]
occ_1992 = occ[occ$year == 1992, 13:14]
occ_1993 = occ[occ$year == 1993, 13:14]
occ_1994 = occ[occ$year == 1994, 13:14]
occ_1995 = occ[occ$year == 1995, 13:14]
occ_1996 = occ[occ$year == 1996, 13:14]
occ_1997 = occ[occ$year == 1997, 13:14]
occ_1998 = occ[occ$year == 1998, 13:14]
occ_1999 = occ[occ$year == 1999, 13:14]
occ_2000 = occ[occ$year == 2000, 13:14]
occ_2001 = occ[occ$year == 2001, 13:14]

# brazil map
states <- read_state(year=2019)

# annual maps in Brazil

# 1985
g1<- ggplot(data = states) +
  geom_sf() +
  geom_point(data=occ_1985, aes(X, Y), col = "red")
ggsave(filename = "Dropbox/08_Serotype_spread/Data/Brazil_occurrence_maps/Brazil_1985.png", g1)
# Cases in French Guiana
# and around Fortaleza

# 1986
g2<- ggplot(data = states) +
  geom_sf() +
  geom_point(data=occ_1986, aes(X, Y), col = "red")
ggsave(filename = "Dropbox/08_Serotype_spread/Data/Brazil_occurrence_maps/Brazil_1986.png", g2)
# cases in Roraima (Boa vista?)
# around fortalez and around Rio

# 1987
g3<- ggplot(data = states) +
  geom_sf() +
  geom_point(data=occ_1987, aes(X, Y), col = "red")
ggsave(filename = "Dropbox/08_Serotype_spread/Data/Brazil_occurrence_maps/Brazil_1987.png", g3)
# cases over the border in Bolivia near the state of Matto Grosso
# numerous place in NE and aroiund Rio

# 1988
g4<- ggplot(data = states) +
  geom_sf() +
  geom_point(data=occ_1988, aes(X, Y), col = "red")
ggsave(filename = "Dropbox/08_Serotype_spread/Data/Brazil_occurrence_maps/Brazil_1988.png", g4)
# More cases in Bolivia near Matto Grosso
# Cases over the border in Paraguay near Foz de Iguazu
# some in French Guiana
# around Fortaleza and Rio

# 1989
g5<- ggplot(data = states) +
  geom_sf() +
  geom_point(data=occ_1989, aes(X, Y), col = "red")
ggsave(filename = "Dropbox/08_Serotype_spread/Data/Brazil_occurrence_maps/Brazil_1989.png", g5)
# Cases over the border in Paraguay near Foz de Iguazu
# some in French Guiana
# around Fortaleza and Rio


# 1990
g6<- ggplot(data = states) +
  geom_sf() +
  geom_point(data=occ_1990, aes(X, Y), col = "red")
ggsave(filename = "Dropbox/08_Serotype_spread/Data/Brazil_occurrence_maps/Brazil_1990.png", g6)
# Cases over the border in Paraguay near Foz de Iguazu
# some in French Guiana
# cases in Peru around Iquitos
# around Fortaleza and Rio and N Sao Paulo state


# 1991
g7<- ggplot(data = states) +
  geom_sf() +
  geom_point(data=occ_1991, aes(X, Y), col = "red")
ggsave(filename = "Dropbox/08_Serotype_spread/Data/Brazil_occurrence_maps/Brazil_1991.png", g7)
# cases multiple places French Guiana
# Cases over the border in Paraguay near Foz de Iguazu
# some cases Iquitos
# lots Sao Paulo, rio and fortaleza

# 1992
g8<- ggplot(data = states) +
  geom_sf() +
  geom_point(data=occ_1992, aes(X, Y), col = "red")
ggsave(filename = "Dropbox/08_Serotype_spread/Data/Brazil_occurrence_maps/Brazil_1992.png", g8)
# cases multiple places French Guiana
# Cases over the border in Paraguay near Foz de Iguazu
# some cases Iquitos
# lots Sao Paulo, rio and fortaleza

# 1993
g9<- ggplot(data = states) +
  geom_sf() +
  geom_point(data=occ_1993, aes(X, Y), col = "red")
ggsave(filename = "Dropbox/08_Serotype_spread/Data/Brazil_occurrence_maps/Brazil_1993.png", g9)
# cases multiple places French Guiana
# Cases over the border in Paraguay near Foz de Iguazu
# some cases Iquitos
# some Sao Paulo, rio and fortaleza

# 1994
g10<- ggplot(data = states) +
  geom_sf() +
  geom_point(data=occ_1994, aes(X, Y), col = "red")
ggsave(filename = "Dropbox/08_Serotype_spread/Data/Brazil_occurrence_maps/Brazil_1994.png", g10)
# cases multiple places French Guiana
# Cases over the border in Paraguay near Foz de Iguazu
# some cases Iquitos
# some Sao Paulo, rio and fortaleza and lots central coast. Some spread towards Brasilia

# 1995
g11<- ggplot(data = states) +
  geom_sf() +
  geom_point(data=occ_1995, aes(X, Y), col = "red")
ggsave(filename = "Dropbox/08_Serotype_spread/Data/Brazil_occurrence_maps/Brazil_1995.png", g11)
# lots of spread out from N. Sao paulom connects with Paraguay route
# still lots in Venezuela and French Guiana and Peru

# 1996
g12<- ggplot(data = states) +
  geom_sf() +
  geom_point(data=occ_1996, aes(X, Y), col = "red")
ggsave(filename = "Dropbox/08_Serotype_spread/Data/Brazil_occurrence_maps/Brazil_1996.png", g12)

# 1997
g13<- ggplot(data = states) +
  geom_sf() +
  geom_point(data=occ_1997, aes(X, Y), col = "red")
ggsave(filename = "Dropbox/08_Serotype_spread/Data/Brazil_occurrence_maps/Brazil_1997.png", g13)

# 1998
g14<- ggplot(data = states) +
  geom_sf() +
  geom_point(data=occ_1998, aes(X, Y), col = "red")
ggsave(filename = "Dropbox/08_Serotype_spread/Data/Brazil_occurrence_maps/Brazil_1998.png", g14)

# 1999
g15<- ggplot(data = states) +
  geom_sf() +
  geom_point(data=occ_1999, aes(X, Y), col = "red")
ggsave(filename = "Dropbox/08_Serotype_spread/Data/Brazil_occurrence_maps/Brazil_1999.png", g15)

# 2000
g16<- ggplot(data = states) +
  geom_sf() +
  geom_point(data=occ_2000, aes(X, Y), col = "red")
ggsave(filename = "Dropbox/08_Serotype_spread/Data/Brazil_occurrence_maps/Brazil_2000.png", g16)

# 2001
g17<- ggplot(data = states) +
  geom_sf() +
  geom_point(data=occ_2001, aes(X, Y), col = "red")
ggsave(filename = "Dropbox/08_Serotype_spread/Data/Brazil_occurrence_maps/Brazil_2001.png", g17)



# primary sources
# Fortaleza (GAUL = 7005) AND Rio (GAUL = 9961) in 1986

# secondary sources
# Northern Sao Paulo state in 1990 (Ribeirão Preto, GAUL = 11468)
# multiple major incursions in 1996
# Manaus (GAUL = 6509)

