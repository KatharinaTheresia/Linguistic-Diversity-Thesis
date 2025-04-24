#### Setup Environment and Load Data ####

# Load Required Libraries 
library(readxl)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(viridis)
library(scales)


# Load Data Frames 
# Loading and cleaning individual data frames
df1 <- read_excel("entropy_summary.xlsx") %>%
  rename_with(~ tolower(trimws(.))) 
df2 <- read_excel("RLI_codes.xlsx") %>%
  rename_with(~ tolower(trimws(.))) 


#### Drawing a map with Entropy ####

# Load World Map 

world <- ne_countries(scale = "medium", returnclass = "sf")

# Join entropy data

world_data <- world %>%
  left_join(df1, by = c("iso_a3_eh" = "isocode"))

# Re-scale entropy

world_data <- world_data %>%
  mutate(rescaled_entropy = rescale(entropy, to = c(0, 1)))



ggplot(data = world_data) +
  geom_sf(aes(fill = rescaled_entropy), color = NA) +
  scale_fill_viridis(
    option = "turbo",
    na.value = "white",
    name = "Entropy",
    guide = guide_colorbar(
      direction = "horizontal",
      barwidth = 10,
      barheight = 0.4,
      title.position = "top",  # Move title above bar to avoid overlap
      title.hjust = 0.5        # Center the title
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.margin = margin(5, 5, 5, 5)
  )


#### Drawing a map with RLI ####


# Load World Map

world <- ne_countries(scale = "medium", returnclass = "sf")


# Join RLI data

world_data <- world %>%
  left_join(df2, by = c("iso_a3_eh" = "isocode"))


ggplot(data = world_data) +
  geom_sf(aes(fill = rli), color = NA) +
  scale_fill_viridis(
    option = "turbo",
    na.value = "white",
    name = "RLI",
    guide = guide_colorbar(
      direction = "horizontal",
      barwidth = 10,
      barheight = 0.4,
      title.position = "top",  # Move title above bar to avoid overlap
      title.hjust = 0.5        # Center the title
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.margin = margin(5, 5, 5, 5)
  )

#### Drawing a map with language count ####

# Load World Map 


world <- ne_countries(scale = "medium", returnclass = "sf")

# Join language_count data

world_data <- world %>%
  left_join(df1, by = c("iso_a3_eh" = "isocode"))

# Convert language count to its logarithm
world_data$language_count <- log(world_data$language_count)


world_data <- world_data %>%
  mutate(rescaled_language_count = rescale(language_count, to = c(0, 1)))


ggplot(data = world_data) +
  geom_sf(aes(fill = rescaled_language_count), color = NA) +
  scale_fill_viridis(
    option = "turbo",
    na.value = "white",
    name = "Number of Languages",
    guide = guide_colorbar(
      direction = "horizontal",
      barwidth = 10,
      barheight = 0.4,
      title.position = "top",  # Move title above bar to avoid overlap
      title.hjust = 0.5        # Center the title
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.margin = margin(5, 5, 5, 5)
  )

