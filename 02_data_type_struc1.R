# --------------------------------------------
# Script Name: Basic R
# Purpose: This section introduces data types and structures
#          used in R, and basic data manipulation. The main
#          websites refered are
#          https://www.javatpoint.com/r-database
#          https://bbolker.github.io/R-ecology-lesson/03-dplyr.html

# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2024-03-15
#
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

###########################################################
# getwd() # current working directory
# setwd() # set working directory

# 01-assignment of variables
  
variable.1 = 124  # Assignment using equal operator           
variable.1

variable.2 <- "Learn R Programming"  # Assignment using leftward   
variable.2 

133L -> variable.3  # Assignment using rightward operator           
variable.3

print(variable.1)  
cat ("variable.1 is ", variable.1, "\n")  
cat ("variable.2 is ", variable.2, "\n")  
cat ("variable.3 is ", variable.3, "\n") 
cat ("variable.3 is ", variable.4, "\n") 

##############################################
# 02-data types and data structures 

x <- pi
class(pi) # why different
typeof(pi)

a<-4:-10 # create a vector 
a 
class(a)
typeof(a)

seq_vec<-seq(1,4,by=0.5)  
seq_vec  
class(seq_vec)  

num_vec<-c(10.1, 10.2, 33.2)  
num_vec  
class(num_vec)  

seq_vec<-seq(1,4,length.out=6)  
seq_vec  
seq_vec[2]  # Accessing elements of vectors

char_vec<-c("shubham"=22,"arpita"=23,"vaishali"=25)  
char_vec  
char_vec["arpita"]  

p<-c(1,2,4,5,7,8)  
q<-c("shubham","arpita","nishka","gunjan","vaishali","sumit")  
r<-c(p,q)  # Combining vectors
r

a<-c(1,3,5,7)  # Arithmetic operations
length(a)
b<-c(2,4,6,8)  
length(b)
a+b  
length(a+b)
a-b  
a/b  
a%%b # help('%%')

vec <- c(3,4,5,6)  # create a list
char_vec<-c("shubham","nishka","gunjan","sumit")  
logic_vec<-c(TRUE,FALSE,FALSE,TRUE)  

out_list<-list(vec,char_vec,logic_vec)  
out_list  

list_data <- list(c("Shubham","Arpita","Nishka"), 
                  matrix(c(40,80,60,70,90,80), nrow = 2),  
                  list("BCA","MCA","B.tech"))  
list_data  

print(list_data[1])  # Accessing the element of the list
print(list_data[2]) 
print(list_data[3])  

list1 <- list(10:20)  
print(list1)  

list2 <-list(5:14)  
print(list2)  

 
v1 <- unlist(list1)  # Converting the lists to vectors 
class(v1)
v2 <- unlist(list2)  

print(v1)  
print(v2)  

vec1 <-c(1,3,5)  # Taking these vectors as input to the array 
vec2 <-c(10,11,12,13,14,15)  
res <- array(c(vec1,vec2),dim=c(3,3,2))  
print(res)  

result <- apply(res,c(1),sum) # calculate sum of all rows
print(result)  

P <- matrix(c(5:16), nrow = 4, byrow = TRUE) # create a matrix 
print(P)  

Q <- matrix(c(3:14), nrow = 4, byrow = FALSE)  
print(Q)  

row_names = c("row1", "row2", "row3", "row4")  
col_names = c("col1", "col2", "col3")  

R <- matrix(c(3:14), nrow = 4, byrow = TRUE, dimnames = list(row_names, col_names))  
print(R)  

print(R[3,2])  

R[R==12]<-0 # note "==" different from "=" in R
print(R)  

R <- matrix(c(5:16), nrow = 4,ncol=3)  # mathematical operations
S <- matrix(c(1:12), nrow = 4,ncol=3)  
sum<-R+S  # Addition 
print(sum) 

# Creating the data frame 
emp.data<- data.frame(  
  employee_id = c (1:5),   
  employee_name = c("Shubham","Arpita","Nishka","Gunjan","Sumit"),  
  sal = c(623.3,915.2,611.0,729.0,843.25),   
  
  starting_date = as.Date(c("2012-01-01", "2013-09-23", "2014-11-15", "2014-05-11",  
                            "2015-03-27")),  
  stringsAsFactors = FALSE  
)  
          
print(emp.data)  
class(emp.data)

final <- data.frame(emp.data$employee_id, emp.data$sal)  
print(final)  

final1 <- emp.data[1,]  
print(final1) 

final2 <- emp.data[4:5, ]  
print(final2)  

emp.data.1<-emp.data[-1,]  
print(emp.data.1)  

emp.data.2 <- emp.data[,2]  
print(emp.data.2) 

emp.data.3 <- emp.data$employee_name
print(emp.data.3) 

emp.data.4 <- emp.data[3,2]  
print(emp.data.4) 

# Creating a factor  
data <- c("Shubham","Nishka","Arpita","Nishka","Shubham","Sumit","Nishka","Shubham","Sumit","Arpita","Sumit")  

print(data)  

print(is.factor(data)) 
class(data)
typeof(data)

factor_data<- factor(data) # factor means category 

print(factor_data)  
print(is.factor(factor_data))  

# print(factor_data[4])  # did not understand
# print(factor_data[-4]) 

new_order_factor<- factor(factor_data, # Changing order of the levels
                          levels = c("Gunjan","Nishka","Arpita","Shubham","Sumit"))  
print(new_order_factor)  

# t(Matrix/data frame)  

a <- matrix(c(4:12),nrow=3,byrow=TRUE)  
a  
b <- t(a)  
b  

###########################################
# 03-conversion among data structures
# convert matrix to data.frame
matrix_data=matrix(c(1,2,3,4,5,6,7,8), nrow=4) # default byrow=FALSE

print(matrix_data) 

class(matrix_data)

dataframe_data=as.data.frame(matrix_data) # convert the matrix into dataframe

print(dataframe_data)

class(dataframe_data)
# convert data frame to matrix

dataframe_data1 <- data.frame(a = 1:3, b = letters[10:12],
                 c = seq(as.Date("2004-01-01"), by = "week", 
                         length.out = 3),
                 stringsAsFactors = TRUE)
dataframe_data1 
class(dataframe_data1)

matrix_data1 <- data.matrix(dataframe_data1[1:2]) # column
class(dataframe_data1[1:2])

matrix_data1
class(matrix_data1)

matrix_data2 <- data.matrix(dataframe_data1)
matrix_data2

# convert dataframe to array

df1 <- data.frame(x = 1:5, y = 5:1)
df1

df2 <- data.frame(x = 11:15,y = 15:11)
df2

Array1 <- array(data = c(unlist(df1),  unlist(df2)),
                dim = c(5, 2, 2),
                dimnames = list(rownames(df1),
                                colnames(df1)))
Array1                                  

library("plotKML") # Vector to raster 

####################################################
# 04-data I/O local directory 

# data file import/output 
emp.data<- data.frame( #Creating data frame    
  name = c("Raman","Rafia","Himanshu","jasmine","Yash"),    
  salary = c(623.3,915.2,611.0,729.0,843.25),     
  start_date = as.Date(c("2012-01-01", "2013-09-23", "2014-11-15", "2014-05-11","2015-03-27")),  
  dept = c("Operations","IT","HR","IT","Finance"),    
  stringsAsFactors = FALSE    
)  

any(grepl("xlsx", install.packages())) #  verify the package

# save the dataframe as a xlsx file

library("xlsx") 
write.xlsx(emp.data, file = "data/employee.xlsx", # save in a file
           col.names=TRUE, 
           row.names=FALSE, # if TRUE, get new X. column
           sheetName="Sheet4",
           append = TRUE)  

# import and save data as a csv file

excel_data<- read.xlsx("data/employee.xlsx", sheetIndex = 4)  
print(excel_data) 
write.csv(excel_data, "data/exployee.csv")

csv_data <- read.csv("data/exployee.csv")  
print(csv_data) 
print(is.data.frame(csv_data)) 
max_sal<- max(csv_data$salary) # Getting the maximum
print(max_sal)

#######################################################
# 05-learn about data from an example

# A) Load data and check their structures
surveys <- read.csv("data/portal_data_joined.csv")
head(surveys)
str(surveys)

##-----------------------------------------------------
# B) column operations
sub_survery1 <- surveys["species_id"] # Result is a data.frame
head(sub_survery1)
class(sub_survery1)

sub_survey2 <- surveys[, "species_id"] # Result is a vector
head(sub_survey2)
str(sub_survey2)
tail(sub_survey2, 3)

sub_survey3 <- surveys[["species_id"]]     # Result is a vector
str(sub_survey3)
tail(sub_survey3, 3)

sub_survey4 <- surveys$species_id          # Result is a vector
str(sub_survey4)

##-----------------------------------------------------
# C) operation with factors
sex <- factor(c("male", "female", "female", "male"))
sex
levels(sex)
nlevels(sex)
sex <- factor(sex, levels = c("male", "female"))
sex # after re-ordering
plot(as.factor(surveys$sex))

# Use stringsAsFactors=TURE
# data are being read as`factor`
surveys <- read.csv("data/portal_data_joined.csv", 
                    stringsAsFactors = TRUE)
str(surveys)
# data are being read as `character`
surveys <- read.csv("data/portal_data_joined.csv", 
                    stringsAsFactors = FALSE)
str(surveys)

# convert "plot_type" into a factor

surveys$plot_type <- factor(surveys$plot_type)
str(surveys)

##-----------------------------------------------------
# D) format date as a “YYYY-MM-DD” vector
library(lubridate)
paste(surveys$year, surveys$month, surveys$day, sep = "-")
# paste() vs paste0() / where there is a separator
# paste0("a", "b") == paste("a", "b", sep="")
# using ymd() to convert Date or POSIXct object
# x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
#        "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
# ymd(x)

ymd(paste(surveys$year, surveys$month, surveys$day, sep = "-"))
surveys$date <- ymd(paste(surveys$year, surveys$month, surveys$day, sep = "-"))
str(surveys)
surveys_dates <- paste(surveys$year, surveys$month, surveys$day, sep = "-")
head(surveys_dates)
surveys_dates[is.na(surveys$date)]

##-----------------------------------------------------
# E) data manipul with apply(), lapply(), sapply(), tapply()
# apply() working on specific row or col
m1 <- matrix(c(1:10),nrow=5, ncol=6)
m1
a_m1 <- apply(m1, 2, sum) # MARGIN=1(row) or 2(col)
a_m1

# lapply() working on each element for list
movies <- c("SPYDERMAN","BATMAN","VERTIGO","CHINATOWN")
movies_lower <-lapply(movies, tolower) # lapply()=list
str(movies_lower)

# further get vector using unlist()
films_lower <- unlist(movies_lower)
str(films_lower)
films_lower # quotes

# sapply() working one each element for vector
dt <- cars
lmn_cars <- lapply(dt, min)
lmn_cars
smn_cars <- sapply(dt, min)
smn_cars

avg <- function(x) {  # self-define function
  ( min(x) + max(x) ) / 2
}

fcars <- sapply(dt, avg)
fcars

# tapply()
data(iris)
tapply(iris$Sepal.Width, iris$Species, median)

###------------------------------------------------------
## visit https://bbolker.github.io/R-ecology-lesson/03-dplyr.html
# for the data of surverys and surverys_wide and complete exercise 

# E) data operation with tidyverse

# Load data from tidyverse package
library("tidyverse")  # load the tidyverse packages, incl. dplyr
surveys <- read_csv("data/portal_data_joined.csv") #  from tidyverse
str(surveys) # check the data

# select(), filter(), mutate(), and pipe
select(surveys, plot_id, species_id, weight) # from dplyr
filter(surveys, year == 1995)

# use pipes for manipulating data
surveys_sml <- surveys %>% # |>
  filter(weight < 5) %>%
  select(species_id, sex, weight)
surveys_sml

# use mutate() to create a new column
surveys %>%
  filter(!is.na(weight)) %>%
  mutate(weight_kg = weight / 1000) %>%
  head()

# Split-apply-combine data analysis
surveys %>%
  group_by(sex) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE))

surveys %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight))

surveys %>%
  group_by(sex) %>%
  tally()

# Reshaping with gather and spread
download.file("tinyurl.com/dcmac2017dec/data/surveys_wide.csv",
              dest="data/surveys_wide.csv")
library(tidyverse)
surveys_wide <- read.csv("data/surveys_wide.csv")

surveys_long <- surveys_wide %>%
  gather(key = species_abbrev, value = count, -(month:plot_id))
str(surveys_long)

# reverse the gather() operation
spread(surveys_long,key=species_abbrev,value=count)

# Exporting data
surveys_complete <- surveys %>%
  filter(species_id != "",         # remove missing species_id
         !is.na(weight),           # remove missing weight
         !is.na(hindfoot_length),  # remove missing hindfoot_length
         sex != "")                # remove missing sex

species_counts <- surveys_complete %>% # Extract the most common species_id
  group_by(species_id) %>%
  tally() %>%
  filter(n >= 50)

surveys_complete <- surveys_complete %>% # Only keep the most common species
  filter(species_id %in% species_counts$species_id)

write_csv(surveys_complete, "results/surveys_complete.csv")

##------------------------------------------------------
# F) Data visualization with ggplot2
surveys_complete <- read_csv("results/surveys_complete.csv")

# Plotting scatter plot
ggplot(data = surveys_complete)
ggplot(data = surveys_complete, 
       aes(x = weight, y = hindfoot_length)) # define aes
ggplot(data = surveys_complete, 
       aes(x = weight, y = hindfoot_length)) +
  geom_point() # dot plots

# Assign plot to a variable and then add plot
surveys_plot <- ggplot(data = surveys_complete, 
                       aes(x = weight, y = hindfoot_length))
surveys_plot + 
  geom_point()

# for large data set using hexagonal binning
library(hexbin)
surveys_plot +
  geom_hex()

ggplot(data = surveys_complete, 
       aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1) # add transparency

ggplot(data = surveys_complete, 
       aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, 
             color = "blue") #  add colors

ggplot(data = surveys_complete, 
       aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, 
             aes(color = species_id)) #  color each species

# Plotting boxplot
ggplot(data = surveys_complete, 
       aes(x = species_id, y = weight)) +
  geom_boxplot()

ggplot(data = surveys_complete, 
       aes(x = species_id, y = weight)) +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.3, color = "tomato") # add points  

# Plotting time series data
yearly_counts <- surveys_complete %>%
  group_by(year, species_id) %>%
  tally()

ggplot(data = yearly_counts, aes(x = year, y = n)) +
  geom_line() # draw a line for all species

ggplot(data = yearly_counts, aes(x = year, y = n, group = species_id)) +
  geom_line() # draw a line for each species 

ggplot(data = yearly_counts, aes(x = year, y = n, color = species_id)) +
  geom_line() # add colors

# split one plot into multiple plots based on a factor
ggplot(data = yearly_counts, aes(x = year, y = n)) +
  geom_line() +
  facet_wrap(~ species_id)

yearly_sex_counts <- surveys_complete %>%
  group_by(year, species_id, sex) %>%
  tally()

ggplot(data = yearly_sex_counts, aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(~ species_id) # split the line in each plot by the sex 

# Usually plots with white background
ggplot(data = yearly_sex_counts, aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(~ species_id) +
  theme_bw() +
  theme(panel.grid = element_blank())

# Customization with aes and title
ggplot(data = yearly_sex_counts, aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(~ species_id) +
  labs(title = "Observed species in time",
       x = "Year of observation",
       y = "Number of species") +
  theme_bw()

ggplot(data = yearly_sex_counts, aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(~ species_id) +
  labs(title = "Observed species in time",
       x = "Year of observation",
       y = "Number of species") +
  theme_bw() +
  theme(text=element_text(size = 16)) # modifying font size

grey_theme <- theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
                    axis.text.y = element_text(colour = "grey20", size = 12),
                    text = element_text(size = 16)) #  created a theme
ggplot(surveys_complete, aes(x = species_id, y = hindfoot_length)) +
  geom_boxplot() +
  grey_theme

# Arranging and exporting plots
library(gridExtra)

spp_weight_boxplot <- ggplot(data = surveys_complete, 
                             aes(x = species_id, y = weight)) +
  geom_boxplot() +
  xlab("Species") + ylab("Weight (g)") +
  scale_y_log10()

spp_count_plot <- ggplot(data = yearly_counts, aes(x = year, y = n, color = species_id)) +
  geom_line() + 
  xlab("Year") + ylab("Abundance")

grid.arrange(spp_weight_boxplot, # combine into a single figure
             spp_count_plot, 
             ncol = 2, widths = c(4, 6))

my_plot <- ggplot(data = yearly_sex_counts, aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(~ species_id) +
  labs(title = "Observed species in time",
       x = "Year of observation",
       y = "Number of species") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        text=element_text(size = 16))
ggsave("results/name_of_file.png", my_plot, width = 15, height = 10)

## This also works for grid.arrange() plots
combo_plot <- grid.arrange(spp_weight_boxplot, spp_count_plot, ncol = 2, widths = c(4, 6))
ggsave("results/combo_plot_abun_weight.png", combo_plot, width = 10, dpi = 300)
