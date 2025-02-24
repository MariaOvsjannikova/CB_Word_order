# Compare the distances based both on words and punctuations marks with 
# the distances based just on words

# upload the data

# default distances based both on words and punctuation
distances_df <- read.table(file.choose(), header = T, sep = "\t")
distances <- as.matrix(distances_df[,-c(1:2)])
rownames(distances) <- colnames(distances)

# the distances without punctuation
wo_punct_distances_df <- read.table(file.choose(), header = T, sep = "\t")
wo_punct_distances <- as.matrix(distances_df[,-c(1:2)])
rownames(wo_punct_distances) <- colnames(wo_punct_distances)

library(vegan)
mantel_test <- mantel(distances, wo_punct_distances, method = "spearman", permutations = 999)
print(mantel_test)
