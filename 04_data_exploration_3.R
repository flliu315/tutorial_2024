# --------------------------------------------
# Script Name: Exploratory data analysis
# Purpose: This section is to show how to do EDA for 
#          finding eco-patterns. The data analysis is 
#          to take the dataset of doubs as an example,
#          and do EDA on the data of a community.

# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2024-03-10
#
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

# 01-download and visualize data

# A) load and visualize river data
# using qgis to find and load river data

# Accessing OpenStreetMap data with R
# chatGPT prompt

"based on openstreetmap data, write R code to find Le Doubs
in France-Switzerland, and use mapview to visulize it on a map"

# final response
# Install and load necessary packages

library(osmdata)
library(mapview)

# Define the bounding box for France-Switzerland
bbox <- c(left = 5.5, bottom = 46.5, right = 7.5, top = 48)

# Query OpenStreetMap for waterways named "Le Doubs" within the bounding box
doubs_query <- opq(bbox = bbox) %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  add_osm_feature(key = "name", value = "Le Doubs") %>%
  osmdata_sf() 

# Visualize Le Doubs on a map
mapview(doubs_query$osm_lines)

## https://ourcodingclub.github.io/tutorials/spatial-vector-sf/
# Look at query output
library(dplyr)
doubs_query
glimpse(doubs_query$osm_lines)
glimpse(doubs_query$osm_multilines)

# bind into a sf object
library(sf)
river_sf <- bind_rows(st_cast(doubs_query$osm_lines, "MULTILINESTRING"),
                      doubs_query$osm_multilines) %>%
  select(name, osm_id, role)

head(river_sf)

# plot doubs_sf
plot(river_sf)

unique(river_sf$role)

# Filter out unneeded shapes to make sure shapes are valid
river_sf_clean <-
  river_sf %>%
  filter(is.na(role) == FALSE) %>%  # remove role NAs
  rename(doubs_type = role) %>% # rename role to doubs_type
  st_make_valid()

st_write(river_sf_clean, "data/SPdata/doubs_river.shp")

st_write(river_sf_clean,
         dsn = "data/SPdata/doubs_river.gpkg", # file path
         layer="doubsspaces", # layer name
         layer_options = c(paste0("DESCRIPTION=Contains spatial multilines for doubss, ",
                                  "Copyright OpenStreetMap constibutors. ODbL ",
                                  "https://www.openstreetmap.org/copyright")),
         # add layer description
         delete_dsn = TRUE
)

# doubs_river <- st_read(dsn = "data/SPdata/doubs_river.gpkg", 
#                       layer="doubsspaces")

doubs_river <- st_read("data/SPdata/doubs_river.shp")

library(ggplot2)
doubs_river <- ggplot(doubs_river) +
  geom_sf(color="blue")
doubs_river

ggsave("data/SPdata/doubs_river.png", doubs_river, 
       width = 8, height = 6.5)

##---------------------------------------------------
# B) load the dataset from database

# Because the dataset was uploaded into the databases
# of PostgreSQL and sqlite, so here load it from the 
# databases

# doubs <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
#                           dbname = 'ecodata',
#                           host = 'localhost',
#                           port = 5432,
#                           user = 'ecosci',
#                           password = 'ecosci')
library(DBI)
library(RSQLite)
library(dbplyr)
library(dplyr)

con <- DBI::dbConnect(RSQLite::SQLite(), 
                        "results/doubs.sqlite")
dbListTables(con) # view the database
dbListFields(con, "spe") # Querying table

spe <- dplyr::tbl(con, "spe") # a lazy object
species <- collect(spe) # pull data into R
species

env <- dplyr::tbl(con, "env")
environment <- collect(env)

site <- dplyr::tbl(con, "site")
coordinates_utm <- collect(site)
coordinates_utm

library(tidyverse)
write_csv(coordinates_utm, "qgis/coorinates_utm.csv")
dbDisconnect(con) 

#############################################
# 02-EDA on the data of fish community

library(vegan) 
source("scripts/panelutils.R") # download from https://figshare.com/s/65782d019a0e2af49cf6?file=4975841

dim(species) # Dimensions of the data frame (rows, columns)
colnames(species) # Column labels (descriptors = species)
rownames(species) # Row labels (objects = sites)
summary(species) # Descriptive statistics for columns

##----------------------------------------
## A) frequency distribution of fish abundance

range(species) 
(ab <- table(unlist(species))) # df→vector→table→output
barplot(ab,
        las = 1, # control target direction
        xlab = "Abundance class",
        ylab = "Frequency",
        col = gray(5 : 0 / 5)
)

sum(species == 0) / (nrow(species) * ncol(species))

##-----------------------------------------------------
# B) Visualizing fish distribution

head(coordinates_utm)
plot(coordinates_utm, # using plot(…) function with lines(…), 
     # points(…), text(…), polygon(…) etc. 
     # to build sophisticated graphs.
     asp = -1,
     type = "p", # a plot point data
     main = "Site Locations",
     xlab = "x coordinate (km)",
     ylab = "y coordinate (km)"
)

lines(coordinates_utm, col = "light blue") # Add a line/labels/text 

text(coordinates_utm, row.names(coordinates_utm), cex = 0.8, col = "red")
text(40, 20, "Upstream", cex = 1.2, col = "red")
text(15, 120, "Downstream", cex = 1.2, col = "red")

par(mfrow=c(2,2))
# Plot four species
xl <- "x coordinate (km)"
yl <- "y coordinate (km)"
plot(coordinates_utm, asp=1, col="brown", cex=species$Neba, 
     main="Brown trout(褐鳟)", xlab=xl, ylab=yl)
lines(coordinates_utm, col="light blue", lwd=2)
plot(coordinates_utm, asp=1, col="brown", cex=species$Cogo, 
     main="Grayling(茴鱼)", xlab=xl, ylab=yl)
lines(coordinates_utm, col="light blue", lwd=2)
plot(coordinates_utm, asp=1, col="brown", cex=species$Baba, 
     main="Barbel(鲃鱼)", xlab=xl, ylab=yl)
lines(coordinates_utm, col="light blue", lwd=2)
plot(coordinates_utm, asp=1, col="brown", cex=species$Abbr, 
     main="Common bream(欧鳊)", xlab=xl, ylab=yl)
lines(coordinates_utm, col="light blue", lwd=2)
par(mfrow=c(1,1))

# Visualizing on a google map

# convert coordinates from no crs to epsg=4326 using qgis
# // converting xy.csv to xy.png with qgis
# // using geocoding and quickOSM to find doubs river
# // A) quickservice→metasearch→add default for basic map
# // B) quickOSM→waterway and river→doubs→runs
# // using freehand georeference Georeferencing an Image
# // A) digitizing river and smapling points (https://www.youtube.com/watch?v=ZgsXKw2ZjlA)
# // B) extracting long and lat of points (https://www.youtube.com/watch?v=aEnKHx14LnU)
# // extracting latlong with shp (https://www.youtube.com/watch?v=bVi7dCBu1hU)

library(tidyverse)
coordinates_geo <- read_csv("qgis/coordinates_geo.csv")
coordinates_geo

# # Comparing geolocation data with georeferring by ourselves
# load("data/Doubs.RData",  Doubs <- new.env())
# ls.str(Doubs)
# latlong <- Doubs$latlong

# map with leaflet

library(leaflet)
doubs_map <- leaflet(data = coordinates_geo) %>% 
  addTiles() %>% # Add default OpenStreetMap map tiles
  addCircleMarkers(~X, ~Y, radius = ~species$Abbr)
doubs_map

category <- c(species$Abbr, species$Cogo, species$Baba, species$Neba)
factpal <- colorFactor(topo.colors(4), category)
doubs_map <- leaflet(data = coordinates_geo) %>% 
  addTiles() %>% # Add default OpenStreetMap map tiles
  addCircleMarkers(~X, ~Y, 
                   radius = ~c(species$Abbr, species$Cogo, species$Baba, species$Neba),
                   color = ~factpal(category))
doubs_map

# map with ggmap
# setting google API key
# devtools::install_github("dkahle/ggmap", ref = "tidyup")
library(ggmap)
api <- " " # get api
register_google(key = api) # set api

# creating a basci map
# https://stackoverflow.com/questions/54624278/getting-connection-timed-out-error-while-geocoding-in-r
# https://stackoverflow.com/questions/51481913/error-when-mapping-in-ggmap-with-api-key-403-forbidden/53686443
library(httr)
set_config(use_proxy(xxx))
map <- get_map(location = 'France', zoom = 6)
ggmap(map)

# adding points layer

ggmap(map) +
  geom_point(data = coordinates_geo,
             aes(x =X, y=Y),
             size=2) +
  labs(title = "sampling points along doubs")

# C) common species and hotspot
spe.pres <- apply(species > 0, 2, sum) # sum for column 
sort(spe.pres)
spe.relf <- 100*spe.pres/nrow(species)
round(sort(spe.relf), 1)
par(mfrow=c(1,2))
hist(spe.pres, main="Species Occurrences", right=FALSE, las=1,
     xlab="Number of occurrences", ylab="Number of species",
     breaks=seq(0,30,by=5), col="bisque")
hist(spe.relf, main="Species Relative Frequencies", right=FALSE,
     las=1, xlab="Frequency of occurrences (%)", ylab="Number of species",
     breaks=seq(0, 100, by=10), col="bisque")

sit.pres <- apply(species > 0, 1, sum)
sort(sit.pres)
par(mfrow=c(1,2))
plot(sit.pres,type="s", las=1, col="gray",
     main="Species Richness vs. \n Upstream-Downstream Gradient",
     xlab="Positions of sites along the river", ylab="Species richness")
text(sit.pres, row.names(species), cex=.8, col="red")
plot(coordinates_utm, asp=1, main="Map of Species Richness", pch=21, col="white",
     bg="brown", cex=5*sit.pres/max(sit.pres), xlab="x coordinate (km)",
     ylab="y coordinate (km)")
lines(coordinates_utm, col="light blue")

## D) fish diversity 
library(vegan)
?diversity
N0 <- rowSums(species > 0) # Species richness
H <- diversity(species) # Shannon entropy
N1 <- exp(H) # Shannon diversity (number of abundant species)
N2 <- diversity(species, "inv") # Simpson diversity (number of dominant species)
J <- H/log(N0) # Pielou evenness
E10 <- N1/N0 # Shannon evenness (Hill's ratio)
E20 <- N2/N0 # Simpson evenness (Hill's ratio)
(div <- data.frame(N0, H, N1, N2, E10, E20, J))

##-----------------------------------------------
# E) transformation and standardization of fish data

?decostand
# Transform abundances to presence-absence
species[1:5, 2:4]
spe.pa <- decostand(species, method="pa")
spe.pa[1:5, 2:4]

# Scale abundances by dividing them by the maximum value for each species
spe.scale <- decostand(species, "max")
spe.scale[1:5,2:4]
apply(spe.scale, 2, max)
# Scale abundances by dividing them by the species totals
spe.relsp <- decostand(species, "total", MARGIN=2)
spe.relsp[1:5,2:4]
apply(spe.relsp, 2, sum)
# Scale abundances by dividing them by the site totals
spe.rel <- decostand(species, "total") # : MARGIN=1 (default value) 
spe.rel[1:5,2:4]
apply(spe.rel, 1, sum)

# The chord transformation: the Euclidean distance
# Useful before PCA and K-means
spe.norm <- decostand(species, "normalize") #  (Euclidean norm)
spe.norm[1:5,2:4]

# The Hellinger transformation can be also be obtained
# by applying the chord transformation to square-root
# transformed species data. Apply Hellinger transformation 
# to correct for the double zero problem

spe.hel <- decostand(species, "hellinger")
spe.hel[1:5,2:4]

# Chi-square transformation
spe.chi <- decostand(species, "chi.square")
spe.chi[1:5,2:4]

# Wisconsin standardization
spe.wis <- wisconsin(species)
spe.wis[1:5,2:4]

# Boxplots of Transformed Abundances of a Common Species
par(mfrow=c(1,4))
boxplot(species$Neba, sqrt(species$Neba), log1p(species$Neba), las=1, main="Simple transformation",
          names=c("raw data", "sqrt", "log"), col="bisque")
boxplot(spe.scale$Neba, spe.relsp$Neba, las=1, main="Standardization by species",
          names=c("max", "total"), col="lightgreen")
boxplot(spe.hel$Neba, spe.rel$Neba, spe.norm$Neba, las=1, main="Standardization by sites",
          names=c("Hellinger", "total", "norm"), col="lightblue")
boxplot(spe.chi$Neba, spe.wis$Neba, las=1, main="Double standardization",
          names=c("Chi-square", "Wisconsin"), col="orange")

#######################################################
# 03-EDA on the environment data

# Bubble Maps of Some Environmental Variables
par(mfrow=c(1,4))
plot(coordinates_utm, asp=1, main="Altitude", pch=21, col="white",
       bg="red", cex=5*environment$alt/max(environment$alt), xlab="x", ylab="y")
lines(coordinates_utm, col="light blue", lwd=2)
plot(coordinates_utm, asp=1, main="Discharge", pch=21, col="white",
       bg="blue", cex=5*environment$flo/max(environment$flo), xlab="x", ylab="y")
lines(coordinates_utm, col="light blue", lwd=2)
plot(coordinates_utm, asp=1, main="Oxygen", pch=21, col="white",
       bg="green3", cex=5*environment$oxy/max(environment$oxy), xlab="x", ylab="y")
lines(coordinates_utm, col="light blue", lwd=2)
plot(coordinates_utm, asp=1, main="Nitrate", pch=21, col="white",
       bg="brown", cex=5*environment$nit/max(environment$nit), xlab="x", ylab="y")
lines(coordinates_utm, col="light blue", lwd=2)

##-----------------------------------------------------------
# Examine the Variation of Some Descriptors Along the Stream
par(mfrow=c(1,4))
plot(environment$dfs, environment$alt, type="l", xlab="Distance from the source (km)",
       ylab="Altitude (m)", col="red", main="Altitude")
plot(environment$dfs, environment$flow, type="l", xlab="Distance from the source (km)",
       ylab="Discharge (m3/s)", col="blue", main="Discharge")
plot(environment$dfs, environment$oxy, type="l", xlab="Distance from the source (km)",
       ylab="Oxygen (mg/L)", col="green3", main="Oxygen")
plot(environment$dfs, environment$nit, type="l", xlab="Distance from the source (km)",
       ylab="Nitrate (mg/L)", col="brown", main="Nitrate")

# Scatter Plots for All Pairs of Environmental Variables
# to check collinearity, to which  constrained ordination 
# methods are highly sensitive

source("scripts/panelutils.R")
op <- par(mfrow=c(1,1), pty="s")
pairs(environment, panel=panel.smooth,
        diag.panel=panel.hist,
        main="Biplots with histograms and smooth surves")
par(op)

##--------------------------------------------------------
# Simple Transformation of An Environmental Variable
# check distribution of slo
par(mfrow=c(1,4))
hist(environment$slo, col="bisque", right=FALSE)
hist(log(environment$slo), col="light green", right=F, main="Histogram of ln(env$slo)")
boxplot(environment$slo, col="bisque", main="Boxplot of env$slo", ylab="env$slo")
boxplot(log(environment$slo), col="light green", main="Boxplot of ln(env$slo)",
          ylab="log(env$slo)")

# Center and scale = standardize variables (z-scores) to reduce the
# effects of different units on analysis of any ordinations

env.z <- decostand(environment, "standardize")
apply(env.z, 2, mean) # means = 0
apply(env.z, 2, sd) # standard deviations = 1

env.z1 <- as.data.frame(scale(environment))# Same standardization with scale() 
env.z1

#######################################################
# 04-Cluster and ordination

# Q- and R-mode analysis

# Q-mode (analysis of distances/dissimilarities among objects or rows)

View(species)
spe.db<-vegdist(species, method="bray") # Sorensen dissimilarity with 0-1 data
spe.dj<-vegdist(species, method="jac") # Jaccard dissimilarity
spe.dg<-vegdist(species, method="gower") # Gower dissimilarity 
spe.db.mat<-as.matrix(spe.db) #Put in matrix form (visualize or save)

View(environment)
?dist # this function also compute dissimilarity matrix
env.z <- decostand(environment, "standardize")
env.de<-dist(env.z, method = "euclidean") # euclidean distance matrix of the standardized environmental variables 
source("scripts/coldiss.R")
coldiss(env.de, diag=TRUE)

# R-mode (analysis of relationships among variables or columns)

(env.pearson<-cor(environment)) # Pearson r linear correlation
round(env.pearson, 2) #Rounds the coefficients to 2 decimal points 
(env.ken<-cor(environment, method="kendall")) # Kendall tau rank correlation
round(env.ken, 2)

##------------------------------------------------------
# Clustering based on distance after transformed data

spe.dhel<-vegdist(spe.hel,method="euclidean") #generates the distance matrix from Hellinger transformed data

head(spe.hel)# Hellinger-transformed species data
head(spe.dhel)# Hellinger distances among sites

spe.dhel.single<-hclust(spe.dhel, method="single")
plot(spe.dhel.single, main="Single linkage clustering", hang =-1)

spe.dhel.complete <- hclust(spe.dhel, method = "complete")
plot(spe.dhel.complete, main="Complete linkage clustering", hang=-1)

###########################################################
# 05-Unconstrained and constrained ordination

# model selection

vegan::decorana(species[-8,])

# unconstrained ordination---PCA

# using the rda() function of vegan
spe.h.pca <- rda(spe.hel) # pca for the variable of species
summary(spe.h.pca) # overall results
# screeplot(spe.h.pca,type="lines") # Identify significant axis with Kaiser-Guttman criterion
ev <- spe.h.pca$CA$eig
ev[ev>mean(ev)]
n <- length(ev)
barplot(ev, main="Eigenvalues", col="grey", las=2)
abline(h=mean(ev), col="red") 
legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")
# plot(spe.h.pca) 
# biplot(spe.h.pca, scaling =1, main="PCA scale=1") # angles between species not meaningful
# biplot(spe.h.pca, scaling =2, main="PCA scale=2") # angles between species reflect their correlations
library(ggplot2)
library(ggrepel)
str(spe.h.pca)
sitepca <- as.data.frame(spe.h.pca$CA$u[, 1:2]) # Site scores
speciespca <- as.data.frame(spe.h.pca$CA$v[, 1:2]) # Species scores

ggplot() +
  geom_point(data = sitepca,aes(x = PC1,y = PC2), size=4, shape = 21, fill = "blue", colour = "black") +
  geom_text_repel(data = sitepca, aes(PC1, PC2, label=row.names(sitepca)), size=4) +
  geom_segment(data = speciespca,aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                             type = "closed"),linetype=1, size=0.6,colour = "black")+
  geom_text_repel(data = speciespca, aes(PC1, PC2, label=row.names(speciespca)), colour = "red")+
  geom_hline(yintercept=0,linetype = 3,size = 1) +
  geom_vline(xintercept=0,linetype = 3,size = 1) +
  theme_bw() +
  theme(panel.grid=element_blank())

env.pca <- rda(env.z) # pca for the variable of environment
summary(env.pca)
ev <- env.pca$CA$eig
ev[ev>mean(ev)]
n <- length(ev)
barplot(ev, main="Eigenvalues", col="grey", las=2)
abline(h=mean(ev), col="red") 
legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")

# unconstrained ordination---NMDS
spe.nmds <- metaMDS(species[-8,], distance = 'bray', k = 2)
spe.nmds$stress # goodness-of-fit


stressplot(spe.nmds, main = "Shepard plot")
plot(spe.nmds, type = "none",
     main = paste("NMDS/Bray - Stress =",
                  round(spe.nmds$stress, 3)),
     xlab = c("NMDS1"), ylab = "NMDS2")
points(scores(spe.nmds, display = "sites",
              choiches = c(1,2),
              pch = 21,
              col = "black",
              g = "steelblue",
              cex = 1.2))
text(scores(spe.nmds, display = "species", choices = c(1)),
     scores(spe.nmds, display = "species", choices = c(2)),
     labels = rownames(scores(spe.nmds, display = "species")),
     col = "red", cex = 0.8)

# constrained ordination---RDA
# hellinger transform species dataset: gives low weights to rare species 
# spe.hel <- decostand(species, "hellinger")
# Calculate distance matrix
spe.bchel<-vegdist(spe.hel, method="bray", binary=FALSE) 

# run RDA
env.z <- subset(env.z, select = -dfs)
spe.rda <- rda(spe.hel ~ ., data = env.z)
summary(spe.rda)
screeplot(spe.rda)
coef(spe.rda) # canonical coefficients
# R^2 retreived from the rda result
R2 <- RsquareAdj(spe.rda)$r.squared # unadjusted R^2 
R2 
R2adj <- RsquareAdj(spe.rda)$adj.r.squared # adjusted R^2
R2adj 

# plot RDA
# Triplot: sites, response variables and explanatory variables
# Scaling 1
plot(spe.rda, scaling=1, main="scaling 1 - wa scores")
spe.sc <- scores(spe.rda, choices=1:2, scaling=1, display="sp")
arrows(0,0,spe.sc[,1], spe.sc[,2], length=0, lty=1, col='red')

# Scaling 2
plot(spe.rda, main="scaling 2 - wa scores")
spe2.sc <- scores(spe.rda, choices=1:2, display="sp")  
arrows(0,0,spe2.sc[,1], spe2.sc[,2], length=0, lty=1, col='red')

# variance inflation factors in the RDA
vif.cca(spe.rda)
# RDA with all explanatory variables  
spe.rda.all <- rda(spe.hel ~., data=environment)
# Forward selection using ordistep (accepts models with lower adjusted R2)
fwd.sel <- ordiR2step(rda(spe.hel ~ 1, data = env.z), # lower model limit (simple)
                      scope = formula(spe.rda), # upper model limit (the "full" model)
                      direction = "forward",
                      R2scope = TRUE, # not surpass the "full" model's R2
                      pstep = 1000,
                      trace = TRUE) # see the selection process
# Test of RDA result
anova.cca(spe.rda, step=1000)
# Test of all canonical axes
anova.cca(spe.rda, by='axis', step=1000)

