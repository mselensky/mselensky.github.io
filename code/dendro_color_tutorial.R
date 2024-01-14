# This is a tutorial for coloring terminal dendrogram segments by a metadata variable. 
# Check out my blog post for the full explanation! -->
# https://mselensky.github.io/posts/dendro_color/29-jul-2020-r-colored-dendro-ends/

# load required packages
pacman::p_load(tidyverse, ggdendro, RColorBrewer, plotly)

# label rows with unique sample_name
dat <- iris %>%
  mutate(sample_name = paste("iris", seq(1:nrow(iris)), sep = "_"))

# save non-numeric metadata in separate dataframe
metadata <- dat %>%
  select(sample_name, Species) 

# extract numeric vectors for distance matrix
numeric_data <- dat %>%
  select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, sample_name)

# normalize data to values from 0 to 1 
numeric_data_norm <- numeric_data %>%
  select(sample_name, everything()) %>%
  pivot_longer(cols = 2:ncol(.), values_to = "value", names_to = "type") %>%
  group_by(type) %>%
  mutate(value_norm = (value-min(value))/(max(value)-min(value))) %>% # normalize data to values 0-1
  select(sample_name, value_norm) %>%
  pivot_wider(names_from = "type", values_from = "value_norm") %>%
  column_to_rownames("sample_name")

# create dendrogram from distance matrix
dist_matrix <- dist(numeric_data_norm, method = "euclidean")
dendrogram <- as.dendrogram(hclust(dist_matrix, method = "complete"))

# extract dendrogram segment data
dendrogram_data <- dendro_data(dendrogram)
dendrogram_segments <- dendrogram_data$segments # contains all dendrogram segment data

# get terminal dendrogram segments
dendrogram_ends <- dendrogram_segments %>%
  filter(yend == 0) %>% # filter for terminal dendrogram ends
  left_join(dendrogram_data$labels, by = "x") %>%
  rename(sample_name = label) %>%
  left_join(metadata, by = "sample_name") # dataframe now contains only terminal dendrogram segments and merged metadata associated with each name

# Generate custom color palette for dendrogram ends based on metadata attribute
unique_vars <- levels(factor(dendrogram_ends$Species)) %>%
  as.data.frame() %>% rownames_to_column("row_id") 
color_count <- length(unique(unique_vars$.))
get_palette <- colorRampPalette(brewer.pal(n = 8, name = "Set1"))
palette <- get_palette(color_count) %>%
  as.data.frame() %>%
  rename("color" = ".") %>%
  rownames_to_column(var = "row_id")
color_list <- left_join(unique_vars, palette, by = "row_id") %>%
  select(-row_id)
species_color <- as.character(color_list$color)
names(species_color) <- color_list$.

# Alternatively, create a custom named vector for iris species color:
# species_color <- c("setosa" = "#E41A1C", "versicolor" = "#CB6651", "virginica" =  "#F781BF")

p <- ggplot() +
  geom_segment(data = dendrogram_segments, 
               aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_segment(data = dendrogram_ends,
               aes(x=x, y=y.x, xend=xend, yend=yend, color = Species, text = paste('iris: ', sample_name,
                                                                                   '<br>',
                                                                                   'species: ', Species))) +
  scale_color_manual(values = species_color) +
  scale_y_reverse() +
  coord_flip() + theme_bw() + theme(legend.position = "none") + ylab("Distance") +
  ggtitle("Iris dendrogram")

ggplotly(p, tooltip = c("text"))