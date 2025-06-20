library(ggplot2)
library(ggrepel)

# Upload the table with means distances to CB languages
means <- read.table(file.choose(), sep = '\t', header = T)

# Specify the position of the labels relative the axis
nudge_x = c(-0.05, 0.1, -0.1, 0.1, -0.1, 0.1, -0.05, 0.1, -0.05, 0.05, -0.1, 0.2, -0.1, 0.1, -0.1, 0.2, -0.1, -0.1)

# Plot with means
means_plot <- ggplot(data = means, aes(x = Lg_type, y = Means, group = CB_language, label = Label)) +
  geom_line()+
  geom_point() + xlab("Language group") + ylab("Mean distance") +
  geom_text_repel(nudge_x = nudge_x, size = 5)

means_plot 

tiff("Fig_2_Mean_distances_to_CB.tiff", width = 6, height = 8, units = 'in', res = 300)
means_plot 
dev.off()