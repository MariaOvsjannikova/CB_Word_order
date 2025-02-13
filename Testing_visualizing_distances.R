# 1. Uploading and preparing the distance matrix

distances_df <- read.table(file.choose(), header = T, sep = "\t")
distances <- as.matrix(distances_df[,-c(1:2)])
rownames(distances) <- colnames(distances)

# 2. Creating the MDS-visualization
library(smacof)
library(rgl)
library(ggplot2)

mds_CB <- mds(distances)
mds_CB$stress

# create a data frame for plotting
plot_data <- data.frame(D1 = mds_CB$conf[,1], 
                        D2 = mds_CB$conf[,2], 
                        Area = distances_df$X)
plot_data$Area[plot_data$Area == "n"] <- "Other"

# plot the languages and the coordinates
library(ggrepel)

MDS_CB_plot <- ggplot(plot_data, aes(x = D1, y = D2, shape = Area, label = row.names(plot_data))) +
  geom_point(size = 4) + 
  scale_shape_manual(values = c(16, 13)) +
  geom_text_repel(max.overlaps = 25, box.padding = 0.5, size = 5) +
  theme(legend.position = "bottom")

MDS_CB_plot

# save the plot in a file
tiff("Fig_3_MDS_plot_CB.tiff", width = 8, height = 8, units = 'in', res = 300)
MDS_CB_plot
dev.off()


# 3. Test the correlation between distances and area membership
# while controlling for genealogy

install.packages("vegan")
library(vegan)

# Upload matrices for areal and genealogical distances

gen_distances <- read.table(file.choose(), header = T, sep = "\t")
gen_distances <- as.matrix(gen_distances[, 2:17])
rownames(gen_distances) <- colnames(gen_distances)

area_distances <- read.table(file.choose(), header = T, sep = "\t")
area_distances <- as.matrix(area_distances[, 2:17])
rownames(area_distances) <- colnames(area_distances)

partial_mantel <- mantel.partial(distances, area_distances, gen_distances, method = "pearson")
print(partial_mantel)

