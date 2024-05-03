# --------------------------------------------
# Script Name: Eco-network analysis
# Purpose: This script is shown how to construct a
#          eco-network and analyze the eco-network
#          properties, as well as predict possible
#          links of the econetwork.

# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2024-04-10
#
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

# 01- Econetwork representation and visualization
# A) adjacency matrices for representing food webs
# https://mlurgi.github.io/networks_for_r/lesson-3.html

install.packages("igraph")

library(RCurl) # if refused, revise dns in /etc/resolv.conf
x <- getURL("https://raw.githubusercontent.com/mlurgi/networks_for_r/master/datasets/benguela.edgelist")
benguela.EL <- read.table(text = x) 
benguela.EL <- as.matrix(benguela.EL)

# Create an adjacency matrix called benguela.AM, containing only zeros
benguela.AM <- matrix(0, max(benguela.EL), max(benguela.EL))

# Introduce ones to the matrix to represent interactions between species
benguela.AM[benguela.EL] <- 1

# Species and interactions

# species richness
S <- dim(benguela.AM)[1] # the row

# number of links
L <- sum(benguela.AM)

# B) incidence matrix represent bipartite networks
library(RCurl)
y <- getURL("https://raw.githubusercontent.com/seblun/networks_datacamp/master/datasets/anemonefish.txt")
anemonef <- read.table(text = y)
names(anemonef) <- paste("A", 1:10, sep = "")
row.names(anemonef) <- paste("F", 1:26, sep = "")       
anemonef <- as.matrix(anemonef)

### The number of fish species in the network
n_fish <- dim(anemonef)[1] # the row
### The number of anemone species is the number of columns
n_anemone <- dim(anemonef)[2] # the column

### So, the total number of species is the sum of these two quantities
S <- n_fish + n_anemone

### Whereas the total number of interactions is still the sum of the matrix
L <- sum(anemonef)

# C) Networks as graphs or visualization
library(igraph)

# Weaving networks from scratch

species <- 1:10 # Ten species
network <- graph.empty() + vertices(species) # to an empty graph
network[5,7] <- 1 # Link between species 5 and 7
plot(network) # Plotting your network

# Converting matrix to igraph object

library(RCurl)
x <- getURL("https://raw.githubusercontent.com/seblun/networks_datacamp/master/datasets/benguela.tro")
benguela.EL <- read.table(text = x) 
benguela.EL <- as.matrix(benguela.EL)

# Create an adjacency matrix called benguela.AM
benguela.AM <- matrix(0, max(benguela.EL), max(benguela.EL))

# Introduce ones to the matrix to represent interactions
benguela.AM[benguela.EL] <- 1

# Convert Benguela adjacency matrix to an igraph network
benguela.network <- graph_from_adjacency_matrix(benguela.AM)

# Plotting network
plot(network)

# Plot the Benguela food web
plot(benguela.network, edge.arrow.size = 0.2)

# 02-molecular network construction and exploration

otu_w <- read.table("data/NETdata/warming.txt", head=T, 
                  row.names = 1, sep = "\t")
head(otu_w)
dim(otu_w) # check dataframe
sum(!is.na(otu_w)) 

otu_c <- read.table("data/NETdata/Control.txt", head=T, 
                    row.names = 1, sep = "\t")
sum(!is.na(otu_c))

# remove all rows with 7 or more NA values

cnt_na1 <- apply(otu_w, 1, function(z) sum(is.na(z)))
otu_wd <- otu_w[cnt_na1 < 7,]
head(otu_wd)

cnt_na2 <- apply(otu_c, 1, function(z) sum(is.na(z)))
otu_cd <- otu_c[cnt_na2 < 7,]
dim(otu_cd)

# relative abund of otu in each plot
otu_wd_tran <- otu_wd |>
  replace(is.na(otu_wd), 0) |>
  t()

otu_wd_rel <- prop.table(as.matrix(otu_wd_tran), 
                                   margin = 1)*100
head(otu_wd_rel)

otu_cd_tran <- otu_cd |>
  replace(is.na(otu_cd), 0) |>
  t()

otu_cd_rel <- prop.table(as.matrix(otu_cd_tran), 
                        margin = 1)*100
head(otu_cd_rel)

# calculate correlation coefficient

library(psych) # for correlation coefficient
occor_w <- corr.test(otu_wd_rel, use = "pairwise",
                   method = "spearman", adjust = "fdr",
                   alpha = 0.05)
occor_w_r <- occor_w$r # extracting r values
occor_w_p <- occor_w$p # extracting p valutes
occor_w_r[occor_w_p >0.5 | abs(occor_w_r)<0.79] <-0

# occor_w_rmt <- MetaNet::c_net_calculate(otu_wd_rel)
# rmt(corr_w_rmt) # find the suitable threshold
  
occor_w_r

# constructing men

library(igraph)
igraph_w_r <- graph_from_adjacency_matrix(occor_w_r,
                                          mode = "undirected",
                                          weighted = TRUE,
                                          diag = FALSE)
w_bad_vs <- V(igraph_w_r)[degree(igraph_w_r) == 0]
igraph_w_final <- delete.vertices(igraph_w_r, w_bad_vs)

set.seed(123)
plot(igraph_w_final, main ="co-occurrence network",
     vertex.frame.color = NA,  # Node border color
     vertex.label = NA,
     edge.width =1,
     vertex.size=5,  # Size of the node (default is 15)
     edge.lty =1,
     edge.curved =TRUE)

write_graph(igraph_w_final, "data/NETdata/igraph_w.txt", "edgelist")

# 03-network properties and exploratory analysis

# A) degree and degree distribution

library(igraph)
g <-read_graph("data/NETdata/igraph_w.txt","edgelist") 
g <- as.undirected(g, mode = "collapse")
plot(g,vertex.frame.color=NA,vertex.label=NA,edge.width=1,
     vertex.size=5,edge.lty=1,edge.curved=F)
deg <- degree(g, mode="all") # calculate degree
deg

hist(deg, breaks=1:vcount(g)-1) # degree distribution

# B) closeness and betweenness centrality

deg=degree(g) 
lay <- layout.fruchterman.reingold(g) # fix layout
lay
fine = 500 # increase fine regulation
palette = colorRampPalette(c('blue','red')) # set color
degCol = palette(fine)[as.numeric(cut(deg,breaks = fine))]
plot(g, layout=lay, vertex.color=degCol, 
     vertex.size=deg*1.5, vertex.label=NA)

betw <- betweenness(g) # betweenness
plot(g,layout=lay, vertex.color=degCol,
     vertex.size=betw*0.8, vertex.label=NA)

clos <- closeness(g) # closeness
plot(g,layout=lay, vertex.color=degCol,
     vertex.size=clos*15000,vertex.label=NA)
ev <- evcent(g)
ev <- evcent(g)$vector
ev
plot(g,layout=lay, vertex.color=degCol,
     vertex.size=ev*10, vertex.label=NA)

# C) network connectance

library(igraph)
g<-read_graph("data/NETdata/igraph_w.txt","edgelist")
g <- as.undirected(g, mode = "collapse")
plot(g,vertex.frame.color=NA,vertex.label=NA,edge.width=1,
     vertex.size=5,edge.lty=1,edge.curved=F)
connectance = edge_density(g,loops=FALSE)# connectance
connectance

# D) modularity

library(igraph)
g<-read_graph("data/NETdata/igraph_w.txt","edgelist")
g <- as.undirected(g, mode = "collapse")
ceb <- cluster_edge_betweenness(g)
modularity(ceb)
plot(ceb, g)
