# .............................................................................. Libraries
library(stringdist)
library(dplyr)
library(ggplot2)
library(igraph)

# .............................................................................. Distance matrix
metadata <- full_metadata

# Cluster selection
# metadata <- metadata[metadata$cluster == "ILC2/ILTi",]

metadata <- metadata[!is.na(metadata$a_cdr3) & !is.na(metadata$b_cdr3), ]

# Keep only unique pairs of (a_cdr3, b_cdr3)
pairs <- unique(metadata[, c("a_cdr3", "b_cdr3")])

dist_a <- stringdistmatrix(pairs$a_cdr3, pairs$a_cdr3, method = "lv")
dist_b <- stringdistmatrix(pairs$b_cdr3, pairs$b_cdr3, method = "lv")

dist_matrix <- dist_a + dist_b
dist_matrix <- as.matrix(dist_matrix)

rownames(dist_matrix) <- paste(pairs$a_cdr3, pairs$b_cdr3, sep = "+")
colnames(dist_matrix) <- paste(pairs$a_cdr3, pairs$b_cdr3, sep = "+")

# 1
saveRDS(dist_matrix, file = "dist_matrix.rds")


# .............................................................................. Distance threshold | 0-25
thresholds <- 0:25
num_nonzero_nodes <- numeric(length(thresholds))


for (i in seq_along(thresholds)) {
  th <- thresholds[i]
  binary_matrix <- ifelse(dist_matrix < th, 1, 0)
  
  diag(binary_matrix) <- 0
  
  nonzero_nodes <- apply(binary_matrix, 1, function(x) any(x != 0))
  
  num_nonzero_nodes[i] <- sum(nonzero_nodes)
}


df <- data.frame(
  Threshold = thresholds,
  Nonzero_Nodes = num_nonzero_nodes
)


png("nonzero_nodes_vs_threshold.png", width = 2000, height = 1500, res = 300)

ggplot(df, aes(x = Threshold, y = Nonzero_Nodes)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.8) +
  theme_minimal() +
  labs(x = "Threshold", y = "Number of Nodes", title = "Nodes vs Distance Threshold") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 9),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7)
  )

dev.off()

# .............................................................................. binary_matrix | threshold <= 9
binary_matrix <- ifelse(dist_matrix < 10, 1, 0)

diag(binary_matrix) <- 0

keep <- rowSums(binary_matrix) > 0
binary_matrix <- binary_matrix[keep, keep]

# 2
saveRDS(binary_matrix, file = "binary_matrix.rds")

num_nodes <- nrow(binary_matrix)
num_edges <- sum(binary_matrix[upper.tri(binary_matrix)])


# .............................................................................. degree_matrix
# Calculate how many connections (degree) each clone has in the network,
# then map each clone to the cluster(s) it belongs to based on metadata.

node_degrees <- rowSums(binary_matrix)
degree_matrix <- cbind(Node = 1:length(node_degrees), Degree = node_degrees)
degree_matrix <- degree_matrix[order(-degree_matrix[,2]), ]


degree_matrix <- as.data.frame(degree_matrix)
degree_matrix$Clone <- rownames(degree_matrix)
cluster_map <- full_metadata %>%
  group_by(cdr_Full_ab) %>%
  summarise(Cluster = paste(unique(cluster), collapse = ","), .groups = "drop")

degree_matrix <- degree_matrix %>% left_join(cluster_map, by = c("Clone" = "cdr_Full_ab"))

# 3
saveRDS(degree_matrix, file = "degree_matrix.rds")
# .............................................................................. Degree Distribution bar plot

degree_counts <- degree_matrix %>% count(Degree, name = "Frequency")

degree_filtered <- subset(degree_counts, Degree < 100)

png("degree_distribution.png", width = 5000, height = 3000, res = 1200)
ggplot(degree_filtered, aes(x = Degree, y = Frequency)) +
  geom_bar(stat = "identity", width = 0.8, fill = "steelblue") +
  theme_minimal() +
  labs(x = "Degree", y = "Frequency", title = "Degree Distribution") +
  scale_x_continuous(breaks = seq(0, 200, by = 5)) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 6),
    axis.title.x = element_text(size = 5), 
    axis.title.y = element_text(size = 5), 
    axis.text.x = element_text(size = 4),  
    axis.text.y = element_text(size = 4)
  )
dev.off()


# .............................................................................. clusters corresponding to the hubs in the network.

degree_matrix <- degree_matrix[order(-degree_matrix$Degree), ]
top <- degree_matrix[1:100, ]
clusters <- unlist(strsplit(top$Cluster, split = ","))
cluster_counts <- table(trimws(clusters))

df <- as.data.frame(cluster_counts)
colnames(df) <- c("Cluster", "Count")


png("Clusters Associated with Network Hubs.png", width = 5000, height = 3000, res = 600)
ggplot(df, aes(x = Cluster, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Cluster Distribution of Top CDRs (hubs) by Degree",
       x = "Cluster",
       y = "Count") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 8),
    axis.title.x = element_text(size = 7), 
    axis.title.y = element_text(size = 7), 
    axis.text.x = element_text(size = 5, angle = 45),  
    axis.text.y = element_text(size = 5)
    )
dev.off()

# .............................................................................. Eigenvector Centrality
g <- graph_from_adjacency_matrix(binary_matrix, mode = "undirected", diag = FALSE)

eig_centrality <- eigen_centrality(g)$vector

Eigenvector <- data.frame(
  Node = V(g)$name,
  Eigenvector_Centrality = eig_centrality
)

Eigenvector <- Eigenvector %>%
  left_join(cluster_map, by = c("Node" = "cdr_Full_ab"))

Eigenvector_sorted <- Eigenvector %>%
  arrange(desc(Eigenvector_Centrality))

top <- ceiling(0.01 * nrow(Eigenvector_sorted))
top_nodes <- Eigenvector_sorted[1:top, ]

clusters <- unlist(strsplit(top_nodes$Cluster, split = ","))
clusters <- trimws(clusters)


cluster_counts <- table(clusters)
df <- as.data.frame(cluster_counts)
colnames(df) <- c("Cluster", "Count")


png("Clusters_of_Top_Eigenvector_Hubs.png", width = 5000, height = 3000, res = 600)
ggplot(df, aes(x = Cluster, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Cluster Distribution of Top 1% Nodes by Eigenvector Centrality",
       x = "Cluster",
       y = "Count") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(size = 10), 
    axis.title.y = element_text(size = 10), 
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  
    axis.text.y = element_text(size = 8)
  )
dev.off()

# .............................................................................. Betweenness Centrality

# betweenness_centrality <- betweenness(g, directed = FALSE, normalized = TRUE)
# 
# Betweenness <- data.frame(
#   Node = V(g)$name,
#   Betweenness_Centrality = betweenness_centrality
# )
# 
# Betweenness <- Betweenness %>%
#   left_join(cluster_map, by = c("Node" = "cdr_Full_ab"))
# 
# Betweenness <- Betweenness %>%
#   arrange(desc(Betweenness_Centrality))
# 
# top <- ceiling(0.01 * nrow(Betweenness))
# top_nodes <- Betweenness[1:top, ]
# 
# clusters <- unlist(strsplit(top_nodes$Cluster, split = ","))
# clusters <- trimws(clusters)
# cluster_counts <- table(clusters)
# df <- as.data.frame(cluster_counts)
# colnames(df) <- c("Cluster", "Count")
# 
# png("Clusters_of_Top_Betweenness_Hubs.png", width = 5000, height = 3000, res = 600)
# ggplot(df, aes(x = Cluster, y = Count)) +
#   geom_bar(stat = "identity", fill = "darkorange") +
#   theme_minimal() +
#   labs(title = "Cluster Distribution of Top 1% Nodes by Betweenness Centrality",
#        x = "Cluster",
#        y = "Count") +
#   theme(
#     plot.title = element_text(hjust = 0.5, size = 12),
#     axis.title.x = element_text(size = 10), 
#     axis.title.y = element_text(size = 10), 
#     axis.text.x = element_text(size = 8, angle = 45, hjust = 1),  
#     axis.text.y = element_text(size = 8)
#   )
# dev.off()