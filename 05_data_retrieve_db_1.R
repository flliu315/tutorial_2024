# --------------------------------------------
# Script Name: Data and databases
# Purpose: The class mainly focuses on the dataset of Doubs 
#                River. This script is to show how to get data from 
#                public databases and save a database of SQlite or 
#                postgresql.

# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2024-03-25
#
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

## three ways to access the data from web 
#########################################################
# 01- directly access web non-spatial data
# https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/access-gapminder-data-rcurl-r/

# using download.file() 

library(dplyr)
library(ggplot2)
library(RCurl)

URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"

download.file(URL, destfile = "data/DBdata/download_data.csv", 
              mode="wb")

#############################################################

## 02-R programming for  access data from web API
## https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/API-data-access-r/

library(ggmap)
library(ggplot2)
library(dplyr)
library(rjson) # getJSON() to import data from api
library(jsonlite)
library(RCurl)

# create a API request (url)

base_url = "https://data.colorado.gov/resource/tv8u-hswn.json?"# Base URL path
full_url = paste0(base_url, "county=Boulder", # other arguments
                  "&$where=age between 20 and 40",
                  "&$select=year,age,femalepopulation")

# using fromJSON (full_url) and getting errors

pop_proj_data_df <- rjson::fromJSON(full_url) # Convert JSON to data frame
pop_proj_data_df <- RJSONIO::fromJSON(full_url)
pop_proj_data_df <- jsonlite::fromJSON(full_url)

# encoding the URL with characters

full_url_encoded <- URLencode(full_url)
full_url_encoded
pop_proj_data_df <- jsonlite::fromJSON(full_url_encoded)
head(pop_proj_data_df)

# turn columns to numeric 
pop_proj_data_df <- pop_proj_data_df %>%
  mutate_at(c( "age", "year", "femalepopulation"), as.numeric)
str(pop_proj_data_df)

# plot the data
ggplot(pop_proj_data_df, aes(x = year, y = femalepopulation,
                             group = factor(age), color = age)) + geom_line() +
  labs(x = "Year",
       y = "Female Population - Age 20-40",
       title = "Projected Female Population",
       subtitle = "Boulder, CO: 1990 - 2040")

###------------------------------------------------------------
## Accessing Geodata from web API
# https://kdvdecisions-blog.netlify.app/2020/04/18/obtaining-spatial-data-from-esri-rest-apis-in-r/

library(sf)
library(leaflet)
library(geojsonsf)
library(dplyr)
library(urltools)

# queried an Esri REST API and copy from browser

# navigate to Esri REST APIs → EDW/EDW_ForestSystemBoundaries_01
# → Administrative Forest Boundaries - National Extent (O) → Query
# →  HTML describing →  API

forest <- 
  geojson_sf("https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_ForestSystemBoundaries_01/MapServer/0/query?where=FORESTNAME+LIKE+%27%25Tahoe+National%25%27&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&having=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=geojson")

# take a look at the data

leaflet() %>%
  addProviderTiles("Esri.WorldStreetMap") %>%
  addPolygons(data=forest, weight=2, color="blue")

target_link <- "https://commons.wikimedia.org/wiki/Data:Doubs_River.map"
arget_page <- read_html(target_link)

########################################################

# 03-using rdataretriever for ecological data

# Install rdataretriever in python environment 


# install.packages('reticulate') # interface to Python
library(reticulate)
py_config()
# $pip install retriever # Install retriever package
# install.packages('rdataretriever') # install rdataretriever
library(rdataretriever)
get_updates() # Update the available datasets
# List the datasets available via the Retriever
datasets()

# install_csv('portal') # Install csv portal
download('portal', 'data/DBdata/portal') # .csv data
portal = fetch('portal')
names(portal)
head(portal$species)

download('harvard-forest', 'data/DBdata') # vector data

#####################################################
## two databases for storage of your obtained data
#####################################################
# 01-Working on SQL databases with R

# A) working with RStudio Connections Pane

# https://staff.washington.edu/phurvitz/r_sql/

# // setup sqlite for rstudio's connections
# apt install unixodbc 
# apt install sqliteodbd
# vim /etc/odbcinst.ini
# vim /etc/odbc.ini

# B) working with RStudio codes (dplyr)

# several packages: DBI, RSQLite, tidyverse, dplyr, dbplyr
# https://rdbsql.rsquaredacademy.com/dbplyr

library(DBI)
mammals <- DBI::dbConnect(RSQLite::SQLite(), # create or connect
                          "data/DBdata/portal_mammals.sqlite")

# creating tables and inserting data by dbplyr
library(dbplyr)
dbplyr::src_dbi(mammals) # view the mammals database
surveys <- dplyr::tbl(mammals, "surveys") # Querying table
surveys

library(tidyverse)
data_subset <- surveys %>%
  filter(weight < 5) %>%
  select(species_id, sex, weight) %>%
  collect() # load into the R session

plots <- dplyr::tbl(mammals, "plots")
plots

plots %>% 
  filter(plot_id == 1) %>% # plots and surveys have the common "plot_id"
  inner_join(surveys) %>% # selecting plots based on imported surveys
  collect() # loading the results into R

dbDisconnect(mammals)
mammals

# C) import data to a sqlite database

# load the data from R
library(tidyverse) # for the read_csv()
species1 <- read_csv("data/DBdata/species.csv")
surveys1 <- read_csv("data/DBdata/surveys.csv")
plots1 <- read_csv("data/DBdata/plots.csv")

# create an empty SQLite database
library(dplyr) # create and link to databases by src_sqlite()
my_db_file <- "results/portal_database_output.sqlite"
my_db <- dplyr::src_sqlite(my_db_file, create = TRUE)
my_db

# copy the existing data.frames into the empty database
copy_to(my_db, species1, temporary = FALSE)
copy_to(my_db, surveys1, temporary = FALSE)
copy_to(my_db, plots1, temporary = FALSE)
my_db
dbDisconnect(my_db$con)
my_db

#######################################################

# 02-Working on PostgreSQL with R

## A) Install PostgreSQL on local computer 

# Install and configure postgresql by following

# For ubuntu 22.04, install a default postgresql version by following the site 
# https://www.rosehosting.com/blog/how-to-install-postgresql-on-ubuntu-22-04/
# 
# Verify the installation
# $ dpkg --status postgresql
# $ whereis postgresql
# $ which psql # psql is an interactive PostgreSQL client
# $ ll /usr/bin/psql
# $ psql -V # check postgresql version

# Configure the postgresql
# Including client authentication methods,connecting to 
# PostgreSQL server, authenticating with users, etc. see
# https://ubuntu.com/server/docs/databases-postgresql

# Create a database and enable PostGIS extension
# https://staff.washington.edu/phurvitz/r_sql/

# CREATE EXTENSION postgis;
# CREATE EXTENSION postgis_topology;

# B) create a connect to PostgreSQL

library(DBI)
library(RPostgreSQL)
ecodata <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), # connecting
                       dbname = 'ecodata', 
                       host = 'localhost', 
                       port = 5432, 
                       user = 'xxx', 
                       password = 'xxx')

rdataretriever::install_postgres(dataset = "portal", # .csv file
                                 host = "localhost",
                                 port = "5432", 
                                 database = "ecodata",
                                 database_name = "census",
                                 user = "xxx", 
                                 password = "xxx")

rdataretriever::install_postgres(dataset = 'harvard-forest', # Vector data
                                 host= "localhost",
                                 port = "5432",
                                 database = "ecodata", 
                                 database_name = "postgis",
                                 user = "xxx", 
                                 password = "xxx")

dbDisconnect(ecodata)
dbGetInfo(ecodata)

# C) connect to postgresql and save to specific schemas

# Connect to postgresql
# using connection code

library(DBI)
library(RPostgreSQL)
rsql <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), # connecting
                         dbname = 'rsql', 
                         host = 'localhost', 
                         port = 5432, 
                         user = 'xxx', 
                         password = 'xxx')
dbGetInfo(rsql)
dbExistsTable(rsql, c("rawdata")) # Checking if a table exists

# Upload data to specific schema
ah <- readstata13::read.dta13("data/21600-0001-Data.dta", # read the file
                              convert.factors = FALSE)
colnames(ah) <- tolower(colnames(ah)) # lowercase column names
ah <- ah %>% 
  mutate_at(., -1, as.integer) # convert to integers except 1st column
# make long
ah1 <- ah %>% 
  pivot_longer(-aid, names_to = "varname", values_to = "val")

dbWriteTable(conn = rsql, 
             name = c("addhealth", "data_21600_0001"), 
             value = ah1, 
             row.names = FALSE, 
             overwrite = TRUE)

dbGetQuery(rsql,# List tables in the schema
           "SELECT table_name FROM information_schema.tables
                   WHERE table_schema='addhealth'")

dbListFields(rsql, c("addhealth", 
                   "data_21600_0001")) # List fields of the table

dbDisconnect(rsql)
dbGetInfo(rsql)


