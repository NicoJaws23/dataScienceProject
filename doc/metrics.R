## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(netMet)

## ----message=FALSE, warning=FALSE---------------------------------------------
#add season value to all data sets along with type of season specified in article
s1 <- dplyr::mutate(s1, Season = 1, sType = "Mating")
s2 <- dplyr::mutate(s2, Season = 2, sType = "Winter")
s3 <- dplyr::mutate(s3, Season = 3, sType = "Birth")

#Use season data to create dataframes containing edge values and pass this into graph_from_data_frame() to create networks 
s1Edges <- data.frame(from = s1$"Actor", to = s1$"Recip", weight = s1$groom.rate)
s1G <- igraph::graph_from_data_frame(d = s1Edges, directed = TRUE)

s2Edges <- data.frame(from = s2$"Actor", to = s2$"Recip", weight = s2$groom.rate)
s2G <- igraph::graph_from_data_frame(d = s2Edges, directed = TRUE)

s3Edges <- data.frame(from = s3$"Actor", to = s3$"Recip", weight = s3$groom.rate)
s3G <- igraph::graph_from_data_frame(d = s3Edges, directed = TRUE)

#Use metrics() to calculate network metris for each season
met1 <- metrics(s1G, s1Edges, cutoff = -1, clusterType = "local")
print(met1)
met2 <- metrics(s2G, s2Edges, cutoff = -1, clusterType = "local")
print(met2)
met3 <- metrics(s3G, s3Edges, cutoff = -1, clusterType = "local")
print(met3)

