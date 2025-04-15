#Network Mertric function
library(tidyverse)
library(igraph)

#Takes 3 arguments, g, which is an igraph generated graph, 
#cutoff which is the curoff value for the betweenness metric
#and clusterType which is the clustering type for transitivity
metrics <- function(g, Edges, cutoff, clusterType = c("undirected", "global", "globalundirected", "localundirected", "local", 
"average", "localaverage", "localaverageundirected", "barrat", "weighted")){
  node_ids <- V(g)$name
  
  inNodes <- Edges$to
  inDegree <- as.vector(table(inNodes))
  
  outNodes <- Edges$from
  outDegree <- as.vector(table(outNodes))
  
  inStrength <- aggregate(weight ~ to, data = Edges, sum)
  outStrength <- aggregate(weight ~ from, data = Edges, sum)
  
  betweenness <- igraph::betweenness(g, cutoff = cutoff)
  
  eigenvectorCentrality <- as.data.frame(eigen_centrality(g))
  
  clusteringCoefficient <- igraph::transitivity(s1G, type = clusterType)
  
  x <- data.frame(ID = node_ids, In.Degree = inDegree, Out.Degree = outDegree, In.Strength = inStrength$weight, 
                  Out.Strength = outStrength$weight, Betweenness = betweenness, 
                  Eigenvector.Centrality = eigenvectorCentrality$vector, Clustering.Coefficient = clusteringCoefficient)
  
  return(x)
}

#Testing out on data from Jaworski's data replication project
#Load in datasets
f <- "https://raw.githubusercontent.com/NicoJaws23/dataReplication/refs/heads/main/IDs_Sex_EloScores.csv"
elo <- read_csv(f)
head(elo)

f1 <- "https://raw.githubusercontent.com/NicoJaws23/dataReplication/refs/heads/main/GroomDyads_S1.csv"
s1 <- read_csv(f1)
head(s1)

f2 <- "https://raw.githubusercontent.com/NicoJaws23/dataReplication/refs/heads/main/GroomDyads_S2.csv"
s2 <- read_csv(f2)
head(s2)

f3 <- "https://raw.githubusercontent.com/NicoJaws23/dataReplication/refs/heads/main/GroomDyads_S3.csv"
s3 <- read_csv(f3)
head(s3)

#add season value to all data sets along with type of season specified in article, get minutes from seconds columns
s1 <- mutate(s1, Season = 1, sType = "Mating", groomMin = total.groom.secs/60, dyadObsMin = dyad.obs.secs/60)
s2 <- mutate(s2, Season = 2, sType = "Winter", groomMin = total.groom.secs/60, dyadObsMin = dyad.obs.secs/60)
s3 <- mutate(s3, Season = 3, sType = "Birth", groomMin = total.groom.secs/60, dyadObsMin = dyad.obs.secs/60)

#Combine all 3 seasons into one big table
com <- bind_rows(list(s1, s2, s3))

s1Edges <- data.frame(from = s1$"Actor", to = s1$"Recip", weight = s1$groom.rate)
s1G <- graph_from_data_frame(d = s1Edges, directed = TRUE)

s2Edges <- data.frame(from = s2$"Actor", to = s2$"Recip", weight = s2$groom.rate)
s2G <- graph_from_data_frame(d = s2Edges, directed = TRUE)

s3Edges <- data.frame(from = s3$"Actor", to = s3$"Recip", weight = s3$groom.rate)
s3G <- graph_from_data_frame(d = s3Edges, directed = TRUE)


met1 <- metrics(s1G, s1Edges, cutoff = -1, clusterType = "local")
met2 <- metrics(s2G, s2Edges, cutoff = -1, clusterType = "local")
met3 <- metrics(s3G, s3Edges, cutoff = -1, clusterType = "local")
met1 <- full_join(elo, met1, by = c("ID" = "ID"))
met1 <- met1 |> select(-c(Elo.Sep, Elo.Dec))
met2 <- full_join(elo, met2, by = c("ID" = "ID"))
met2 <- met2 |> select(-c(Elo.Dec, Elo.May))
met3 <- full_join(elo, met3, by = c("ID" = "ID"))
met3 <- met3 |> select(-c(Elo.May, Elo.Sep))
df <- bind_rows(met1, met2, met3)
