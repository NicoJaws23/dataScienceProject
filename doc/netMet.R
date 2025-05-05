## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(netMet)

## -----------------------------------------------------------------------------
library(netMet)

#add season value to all data sets along with type of season specified in article
s1 <- dplyr::mutate(s1, Season = 1, sType = "Mating")
s2 <- dplyr::mutate(s2, Season = 2, sType = "Winter")
s3 <- dplyr::mutate(s3, Season = 3, sType = "Birth")

## -----------------------------------------------------------------------------
#Edge list with weights and direction
s1Edges <- data.frame(from = s1$"Actor", to = s1$"Recip", weight = s1$groom.rate)

#Demographic for each node in the network
node_info <- data.frame(node = elo$ID, sex = elo$Sex, species = c("A", "A", "A", "B", "B", "A", "A", "B", "B", "B", "A", "A", "A", "A", "A", "B", "A", "B", "B", "A", "A", "A", "A", "B", "A", "A"))

network(edges = s1Edges, nodesInfo = node_info, label = "Y", sexCol = "sex", speciesCol = "species", male = "M", redSpecies = "A", diffSpecies = "Y")

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
met1 <- dplyr::full_join(elo, met1, by = c("ID" = "ID"))
met1 <- met1 |> 
  dplyr::select(-c(Elo.Sep, Elo.Dec)) |>
  dplyr::mutate(rank = Elo.May, Season = 1) |>
  dplyr::select(-Elo.May)

met2 <- dplyr::full_join(elo, met2, by = c("ID" = "ID"))
met2 <- met2 |> 
  dplyr::select(-c(Elo.Dec, Elo.May)) |>
  dplyr::mutate(rank = Elo.Sep, Season = 2) |>
  dplyr::select(-Elo.Sep)

met3 <- dplyr::full_join(elo, met3, by = c("ID" = "ID"))
met3 <- met3 |> 
  dplyr::select(-c(Elo.May, Elo.Sep)) |>
  dplyr::mutate(rank = Elo.Dec, Season = 3) |>
  dplyr::select(-Elo.Dec)

df <- dplyr::bind_rows(met1, met2, met3)

#Use nodePerm model to run a generalized linear mixed model to determine the variation of In.Degree in relation to ID, Season, and Sex permuted 1000 times
df <- df |>
  dplyr::mutate(Season = as.factor(Season))

c <- nodePerm(df = df, shuffler = "Sex", formula = In.Degree ~ (1|ID) + (1|Season) + Sex, n = 1000, type = "GLMM", returnVal = "SexM", na.action = na.exclude)
print(c$observed)
hist(c$permuted$result)

