library(ggplot2)
library(ggrepel)

# Upload the table with means distances to CB languages
means <- read.table(file.choose(), sep = '\t', header = T)

# Plot with means
means_plot <- ggplot(data = means, aes(x = Lg_type, y = Means, group = CB_language, label = Label)) +
  geom_line()+
  geom_point() + xlab("Language group") + ylab("Mean distance") +
  geom_text_repel(force = 0.7, force_pull = 0.7, size = 3.5)

means_plot 

