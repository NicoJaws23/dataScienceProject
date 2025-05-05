#' @title A network metric calculation function
#' @description This function uses an igraph object and edges to calculate in-degree,
#' out-degreem in-strength, out-strength, eigenvector centrality, betweenness, and
#' clustering coefficients of all the nodes in the graph
#' @param g The network created by igraph
#' @param Edges A dataframe containing edge information between nodes
#' @param cutoff The cutoff value for calculating betweenness
#' @param clusterType The desired method for calcuating the clustering coefficient
#' @keywords metrics
#' @export
#' @examples
#' s1 <- dplyr::mutate(s1, Season = 1, sType = "Mating")
#' s2 <- dplyr::mutate(s2, Season = 2, sType = "Winter")
#' s3 <- dplyr::mutate(s3, Season = 3, sType = "Birth")
#' s1Edges <- data.frame(from = s1$"Actor", to = s1$"Recip", weight = s1$groom.rate)
#' s1G <- igraph::graph_from_data_frame(d = s1Edges, directed = TRUE)
#' s2Edges <- data.frame(from = s2$"Actor", to = s2$"Recip", weight = s2$groom.rate)
#' s2G <- igraph::graph_from_data_frame(d = s2Edges, directed = TRUE)
#' s3Edges <- data.frame(from = s3$"Actor", to = s3$"Recip", weight = s3$groom.rate)
#' s3G <- igraph::graph_from_data_frame(d = s3Edges, directed = TRUE)
#' met1 <- metrics(s1G, s1Edges, cutoff = -1, clusterType = "local")
#' met2 <- metrics(s2G, s2Edges, cutoff = -1, clusterType = "local")
#' met3 <- metrics(s3G, s3Edges, cutoff = -1, clusterType = "local")

metrics <- function(g, Edges, cutoff, clusterType = c("undirected", "global", "globalundirected", "localundirected", "local",
                                                      "average", "localaverage", "localaverageundirected", "barrat", "weighted")){
  node_ids <- igraph::V(g)$name

  inNodes <- Edges$to
  inDegree <- as.vector(table(inNodes))

  outNodes <- Edges$from
  outDegree <- as.vector(table(outNodes))

  inStrength <- aggregate(weight ~ to, data = Edges, sum)
  outStrength <- aggregate(weight ~ from, data = Edges, sum)

  betweenness <- igraph::betweenness(g, cutoff = cutoff)

  eigenvectorCentrality <- as.data.frame(igraph::eigen_centrality(g))

  clusteringCoefficient <- igraph::transitivity(g, type = clusterType)

  x <- data.frame(ID = node_ids, In.Degree = inDegree, Out.Degree = outDegree, In.Strength = inStrength$weight,
                  Out.Strength = outStrength$weight, Betweenness = betweenness,
                  Eigenvector.Centrality = eigenvectorCentrality$vector, Clustering.Coefficient = clusteringCoefficient)

  return(x)
}

#' @title Node based permutation
#' @description This function allow the user to conduct a node based permutation
#' on network metrics. This function can be used for running permutation on repeatability
#' estimate and for generalized linear mixed models.
#' @param df A data frame containing data used in model functions
#' @param shuffler The variable in the dataframe to be permuted
#' @param formula The formula to enter into each model
#' @param n The number of permutations to run
#' @param type The model the user wants to run
#' @param grname The grouping name to be used for repeatability models
#' @param na.action The desired na actions
#' @param returnVal The value from the model the user wants to return
#' @keywords permuation
#' @export
#' @examples
#' s1 <- dplyr::mutate(s1, Season = 1, sType = "Mating")
#' s2 <- dplyr::mutate(s2, Season = 2, sType = "Winter")
#' s3 <- dplyr::mutate(s3, Season = 3, sType = "Birth")
#' s1Edges <- data.frame(from = s1$"Actor", to = s1$"Recip", weight = s1$groom.rate)
#' s1G <- igraph::graph_from_data_frame(d = s1Edges, directed = TRUE)
#' s2Edges <- data.frame(from = s2$"Actor", to = s2$"Recip", weight = s2$groom.rate)
#' s2G <- igraph::graph_from_data_frame(d = s2Edges, directed = TRUE)
#' s3Edges <- data.frame(from = s3$"Actor", to = s3$"Recip", weight = s3$groom.rate)
#' s3G <- igraph::graph_from_data_frame(d = s3Edges, directed = TRUE)
#' met1 <- metrics(s1G, s1Edges, cutoff = -1, clusterType = "local")
#' met2 <- metrics(s2G, s2Edges, cutoff = -1, clusterType = "local")
#' met3 <- metrics(s3G, s3Edges, cutoff = -1, clusterType = "local")
#' met1 <- dplyr::full_join(elo, met1, by = c("ID" = "ID"))
#' met1 <- met1 |>
#'  dplyr::select(-c(Elo.Sep, Elo.Dec)) |>
#'  dplyr::mutate(rank = Elo.May, Season = 1) |>
#'  dplyr::select(-Elo.May)
#'met2 <- dplyr::full_join(elo, met2, by = c("ID" = "ID"))
#'met2 <- met2 |>
#'  dplyr::select(-c(Elo.Dec, Elo.May)) |>
#'  dplyr::mutate(rank = Elo.Sep, Season = 2) |>
#'  dplyr::select(-Elo.Sep)
#'met3 <- dplyr::full_join(elo, met3, by = c("ID" = "ID"))
#'met3 <- met3 |>
#'  dplyr::select(-c(Elo.May, Elo.Sep)) |>
#'  dplyr::mutate(rank = Elo.Dec, Season = 3) |>
#'  dplyr::select(-Elo.Dec)
#'df <- dplyr::bind_rows(met1, met2, met3)
#'df <- df |>
#'  dplyr::mutate(Season = as.factor(Season))
#'c <- nodePerm(df = df, shuffler = "Sex", formula = In.Degree ~ (1|ID) + (1|Season) + Sex, n = 1000, type = "GLMM", returnVal = "SexM", na.action = na.exclude)
nodePerm <- function(df, shuffler, formula, n, type = c("rpt", "rptPoisson", "GLMM", "GLMMpoisson"), grname, na.action, returnVal){
  Obs_est <- NA
  permN <- numeric(n)

  if(type == "rpt"){
    m <- rptR::rpt(data = df, formula = formula, grname = grname, datatype = "Gaussian", na.action = na.action)
    Obs_est <- m$R[[returnVal]]
    permN <- mosaic::do(n) * {
      permd <- df
      permd[[shuffler]] = mosaic::sample(permd[[shuffler]])
      p <- rptR::rpt(data = permd, formula = formula, grname = grname, datatype ="Gaussian", na.action = na.action)
      p$R[[returnVal]]
    }
  }


  else if(type == "rptPoisson"){
    m <- rptR::rptPoisson(data = df, formula = formula, grname, na.action = na.action)
    Obs_est <- m$R[[returnVal]]
    permN <- mosaic::do(n) * {
      permd <- df
      permd[[shuffler]] = mosaic::sample(permd[[shuffler]])
      p <- rptR::rptPoisson(data = permd, formula = formula, grname, na.action = na.action)
      p$R[[returnVal]]
    }
  }

  else if(type == "GLMM"){
    m <- lme4::lmer(data = df, formula = formula, na.action = na.action)
    Obs_est <- lme4::fixef(m)[[returnVal]]
    permN <- mosaic::do(n) * {
      permd <- df
      permd[[shuffler]] = mosaic::sample(permd[[shuffler]])
      p <- lme4::lmer(data = permd, formula = formula, na.action = na.action)
      lme4::fixef(p)[[returnVal]]
    }
  }

  else if(type == "GLMMpoisson"){
    m <- lme4::glmer(data = df, formula = formula, family = poisson, na.action = na.action)
    Obs_est <- lme4::fixef(m)[[returnVal]]
    permN <- mosaic::do(n) * {
      permd <- df
      permd[[shuffler]] = mosaic::sample(permd[[shuffler]])
      p <- lme4::glmer(data = permd, formula = formula, family = poisson, na.action = na.action)
      lme4::fixef(p)[[returnVal]]
    }
  }

  return(list(formula = formula, observed = Obs_est, permuted = permN))
}

#' @title Complex Social Network
#' @description This function builds a social network from a series of edges provided by the user.
#' It is mean to graph the relationship between two or more distinct groups, such as
#' two species. Nodes are shaped according to the sex of the individual nodes, with males
#' being squares and females triangles. If there are 2 different groups or species, one is
#' colored red and the other green
#' @param edges A data frame containing data dyads and the weight of the relationship
#' @param nodesInfo A data frame containing demographic data of each node, namely the node identity, sex, and species
#' @param label The user can choose if nodes will be labeled by their ID by setting label to "Y"
#' @param sexCol The name of the column in the nodesInfo data frame which contains sex designations
#' @param speciesCol The name of the column containing species names
#' @param male How the male sex is identified in the sex column, be it "M", "male", or otherwise
#' @param redSpecies The name of the species the user wants to be colored red
#' @param diffSpecies The user can set this to either "Y" or "N", with "Y" designating that there are different species in the data
#' @keywords network
#' @export
#' @examples
#' s1Edges <- data.frame(from = s1$"Actor", to = s1$"Recip", weight = s1$groom.rate)
#' node_info <- data.frame(node = elo$ID, sex = elo$Sex, species = c("A", "A", "A", "B", "B", "A", "A", "B", "B", "B", "A", "A", "A", "A", "A", "B", "A", "B", "B", "A", "A", "A", "A", "B", "A", "A"))
#' network(edges = s1Edges, nodesInfo = node_info, label = "Y", sexCol = "sex", speciesCol = "species", male = "M", redSpecies = "human", diffSpecies = "Y")
network <- function(edges, nodesInfo, label = c("Y", "N"), sexCol, speciesCol, male, redSpecies, diffSpecies = c("Y", "N")){
  nodes <- unique(c(edges$from, edges$to))

  angles <- seq(0, 2 * pi, length.out = length(nodes) + 1)[-1]
  layout <- data.frame(node = nodes, x = cos(angles), y = sin(angles))

  node_pos <- merge(layout, nodesInfo, by = "node")

  plot(NA, xlim = range(node_pos$x) + c(-0.5, 0.5),
       ylim = range(node_pos$y) + c(-0.5, 0.5), asp = 1,
       xlab = "", ylab = "", axes = FALSE)


  norm_weights <- scales::rescale(edges$weight, to = c(1, 3))

  for(i in 1:nrow(edges)) {
    from <- edges$from[i]
    to <- edges$to[i]
    w <- norm_weights[i]

    from_coords <- node_pos[node_pos$node == from, c("x", "y")]
    to_coords <- node_pos[node_pos$node == to, c("x", "y")]

    segments(x0 = from_coords$x, y0 = from_coords$y,
              x1 = to_coords$x, y1 = to_coords$y,
              lwd = w, col = "gray30")

  }

  shape_map <- ifelse(node_pos[[sexCol]] == male, 21, 24)

  if(diffSpecies == "Y"){
    color_map <- ifelse(node_pos[[speciesCol]] == redSpecies, "red", "green")
  }
  if(diffSpecies == "N"){
    color_map <- "green"
  }

  points(node_pos$x, node_pos$y, pch = shape_map, bg = color_map, cex = 2)

  if(label == "Y"){
    text(node_pos$x, node_pos$y, labels = node_pos$node, pos = 3, font = 2)
  }
  return(node_pos)
}
