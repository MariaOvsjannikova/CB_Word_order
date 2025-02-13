# 1. Retrieve language coordinates from Glottolog, if available

install.packages("lingtypology")  # Provides access to Glottolog data
library(lingtypology)

languages <- read.table(file.choose(), header = T, sep = "\t")
languages

# Load dataset
data("glottolog")  

# Retrieve coordinates
for(i in 1:nrow(languages)) {
  row_number <- match(as.character(languages[i, 1]), glottolog$language)  # Returns single index or NA
  if (!is.na(row_number)) {  # Only assign if a match is found
    languages$Latitude[i] <- glottolog$latitude[row_number]
    languages$Longitude[i] <- glottolog$longitude[row_number]
  }
}

languages

# Other coordinates were filled in manually
# based on the following locations

# for North-Western Russian dialects -- Velikiy Novgorod
languages[2,4] <- 31.1630
languages[2,5] <- 58.3130

# for West Church Slavic -- Ostroh
languages[4,4] <- 26.310
languages[4,5] <- 50.200

# for Borderland Polish -- Brest
languages[6,4] <- 23.3925
languages[6,5] <- 52.0805

# for Latgalian -- Rezekne
languages[9,4] <- 27.342739
languages[9,5] <- 56.512716

# for Yiddish -- Krakow
languages[9,4] <- 27.342739
languages[9,5] <- 56.512716

# for Finnish Romani -- Helsinki
languages[17,4] <- 24.945831
languages[17,5] <- 60.192059

# for Scandinavian Romani -- Malmo
languages[18,4] <- 13.0209
languages[18,5] <- 55.3621

# for Livonian -- Dundaga
languages[19,4] <- 22.2058
languages[19,5] <- 57.3033

# vector indicating whether the language is analysed in the study
included <- c("y", "n", "y", "n", "y", "n", "y", "y", "n", "n","y", "n", "y", "n", "y", "n", "n", "y", "n", "n", "n", "y", "y", "n", "n", "n", "n", "n")

# shortened labels for the map 
labels <- c("Russian", "NW Russian", "Belarusian", "W.Church Slavic", "Polish", "Border.Polish", 
            "Lithuanian", "Latvian", "Latgalian", "L.German", "H.German", "Yiddish", "Swedish",
            "Danish", "Latin", "Balt.Romani", "Fin.Romani", "Scand.Romani", 
            "Livonian", "Estonian", "Finnish", "Veps", "Karelian", "Votic", "Saami", "Karaim")

languages <- cbind(languages, included, labels)

# 2. Create the map and save it in a file

install.packages(c("ggplot2", "sf", "rnaturalearth", "rnaturalearthdata", "ggrepel"))

library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Filter for Europe
europe <- world[world$continent == "Europe", ]

CB_map <- ggplot() +
  geom_sf(data = europe, fill = "white", color = "gray70") +  # Europe base map
  geom_point(data = languages, aes(x = Longitude, y = Latitude, color = included, shape = Branch), size = 3) +  # Points with color and shape based on genetic affiliation
  geom_text_repel(data = languages, aes(x = Longitude, y = Latitude, label = labels), size = 3, box.padding = 0.5, max.overlaps = 10) +  # Repelled labels
  scale_shape_manual(values = c(17, 13, 15, 18, 19, 13, 16, 14)) +  # Set shapes (circle, triangle, etc.)
  scale_color_manual(values = c("gray60", "black")) +  # Set color palette
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = "bottom") +
  xlim(0, 60) + ylim(40, 69)

tiff("Fig_1_CB_map.tiff", width = 8, height = 8, units = 'in', res = 300)
CB_map
dev.off()

