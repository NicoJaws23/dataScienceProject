#Network Mertric function
library(tidyverse)
library(igraph)

#Takes 3 arguments, g, which is an igraph generated graph, 
#cutoff which is the curoff value for the betweenness metric
#and clusterType which is the clustering type for transitivity
metrics <- function(g, Edges, cutoff, clusterType = c("undirected", "global", "globalundirected", "localundirected", "local", 
"average", "localaverage", "localaverageundirected", "barrat", "weighted")){
  inNodes <- Edges$to
  inDegree <- as.vector(table(inNodes))
  
  outNodes <- Edges$from
  outDegree <- as.vector(table(outNodes))
  
  inStrength <- aggregate(weight ~ to, data = Edges, sum)
  outStrength <- aggregate(weight ~ from, data = Edges, sum)
  
  betweenness <- igraph::betweenness(g, cutoff = cutoff)
  
  eigenvectorCentrality <- as.data.frame(eigen_centrality(g))
  
  clusteringCoefficient <- igraph::transitivity(s1G, type = clusterType)
  
  x <- data.frame(ID = V(g)$label, In.Degree = inDegree, Out.Degree = outDegree, In.Strength = inStrength$weight, 
                  Out.Strength = outStrength$weight, Betweenness = betweenness, 
                  Eigenvector.Centrality = eigenvectorCentrality$vector, Clustering.Coefficient = clusteringCoefficient)
  
  return(x)
}

#Testing out on data from Jaworski's data replication project
f <- "https://raw.githubusercontent.com/NicoJaws23/dataReplication/refs/heads/main/IDs_Sex_EloScores.csv"
elo <- read_csv(f)
head(elo)

f1 <- "https://raw.githubusercontent.com/NicoJaws23/dataReplication/refs/heads/main/GroomDyads_S1.csv"
s1 <- read_csv(f1)
head(s1)

s1Edges <- data.frame(from = s1$"Actor", to = s1$"Recip", weight = s1$groom.rate)
s1G <- graph_from_data_frame(d = s1Edges, directed = TRUE)

met <- metrics(s1G, s1Edges, cutoff = -1, clusterType = "local")
V(s1G)$label
