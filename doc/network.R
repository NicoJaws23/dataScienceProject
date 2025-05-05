## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
library(netMet)

## ----message=FALSE, warning=FALSE---------------------------------------------
#Edge list with weights and direction
s1Edges <- data.frame(from = s1$"Actor", to = s1$"Recip", weight = s1$groom.rate)

#Demographic for each node in the network
node_info <- data.frame(node = elo$ID, sex = elo$Sex, species = c("A", "A", "A", "B", "B", "A", "A", "B", "B", "B", "A", "A", "A", "A", "A", "B", "A", "B", "B", "A", "A", "A", "A", "B", "A", "A"))

network(edges = s1Edges, nodesInfo = node_info, label = "Y", sexCol = "sex", speciesCol = "species", male = "M", redSpecies = "A", diffSpecies = "Y")

