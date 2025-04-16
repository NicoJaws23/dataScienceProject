#Complex social network function

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



#Edge list with weights and direction
s1Edges <- data.frame(from = s1$"Actor", to = s1$"Recip", weight = s1$groom.rate)


# 2. Unique nodes and manual layout (non-linear)
node_info <- data.frame(node = elo$ID, sex = elo$Sex, species = c("ape", "ape", "ape", "human", "human", "ape", "ape", "human", "human", "human", "ape", "ape", 
                                                                  "ape", "ape", "ape", "human", "ape", "human", "human", "ape", "ape", "ape", "ape", "human", "ape", "ape"))


network <- function(edges, nodesInfo, label = c("Y", "N"), sexCol, speciesCol, male, redSpecies){
  nodes <- unique(c(edges$from, edges$to))
  
  angles <- seq(0, 2 * pi, length.out = length(nodes) + 1)[-1]
  layout <- data.frame(node = nodes, x = cos(angles), y = sin(angles))
  
  node_pos <- merge(layout, node_info, by = "node")
  
  plot(NA, xlim = range(node_pos$x) + c(-0.5, 0.5),
       ylim = range(node_pos$y) + c(-0.5, 0.5), asp = 1,
       xlab = "", ylab = "", axes = FALSE)
  
  for(i in 1:nrow(edges)) {
    from <- edges$from[i]
    to <- edges$to[i]
    w <- edges$weight[i]
    
    from_coords <- node_pos[node_pos$node == from, c("x", "y")]
    to_coords <- node_pos[node_pos$node == to, c("x", "y")]
    
    arrows(x0 = from_coords$x, y0 = from_coords$y,
           x1 = to_coords$x, y1 = to_coords$y,
           length = 0.1, lwd = w, col = "gray30")
  }
  
  shape_map <- ifelse(node_pos[[sexCol]] == male, 21, 24)
  
  color_map <- ifelse(node_pos[[speciesCol]] == redSpecies, "red", "green")
  
  points(node_pos$x, node_pos$y, pch = shape_map, bg = color_map, cex = 2)
  
  if(label == "Y"){
  text(node_pos$x, node_pos$y, labels = node_pos$node, pos = 3, font = 2)
  }
}

network(edges = s1Edges, nodesInfo = node_info, label = "N", sexCol = "sex", speciesCol = "species", male = "M", redSpecies = "human")


