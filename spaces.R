library(tidyverse)
library(plotly)
library(arules)
library(tsne)
library(quantable)

setwd('~/Dropbox/Dev/burritoooz')

data <- read_csv('data/burrito.csv') %>%
  filter(!is.na(overall))

clean_location_name <- function(names, from, to) {
  lapply(names, function(name) {
    if (name == from) {
      to
    }
    else {
      name
    }
  }) %>% unlist
}

hot_encode <- function(fields) {
  lapply(fields, function(field) {
    if (is.na(field)) {
      0
    }
    else {
      1
    }
  }) %>% unlist
}

non_zero_var <- function(column) {
  # if (is.integer(column)) {
  #   var(column) != 0
  # }
  # else {
  #   TRUE
  # }
  var(column) != 0
}

get_burrito_names <- function(burrito_names) {
  unique(burrito_names)
}

getListOfNames <- function(burrito) {
  paste(burrito, collapse = "----") 
}

clean_data <- data %>%
  mutate(Location = clean_location_name(Location, "Taco stand", "Taco Stand")) %>%
  mutate(Location = clean_location_name(Location, "Alberto's 623 N Escondido Blvd, Escondido, CA 92025", "Alberto's")) %>%
  mutate(Location = clean_location_name(Location, "Colima's", "Colima's Mexican Food")) %>%
  mutate(Location = clean_location_name(Location, "Lolita's", "Lolita's taco shop")) %>%
  mutate(Location = clean_location_name(Location, "Lolita's Taco shop", "Lolita's taco shop")) %>%
  mutate(Location = clean_location_name(Location, "Lolita's Taco Shop", "Lolita's taco shop")) %>%
  mutate(Location = clean_location_name(Location, "Los Tacos", "Los tacos")) %>%
  mutate(Location = clean_location_name(Location, "Los Tacos", "Los tacos")) %>%
  mutate(Location = clean_location_name(Location, "California burritos", "California Burritos")) %>%
  mutate(Location = clean_location_name(Location, "Donato's Taco Shop", "Donato's taco shop")) %>%
  mutate(Location = clean_location_name(Location, "Kotija Jr.", "Kotija Jr")) %>%
  mutate(Location = clean_location_name(Location, "Taco VIlla", "Taco Villa")) %>%
  mutate(Location = clean_location_name(Location, "Vallarta Express", "Vallarta express")) %>%
  select(Beef,
         Pico,
         Guac,
         Cheese,
         Fries,
         `Sour cream`,
         Pork,
         Chicken,
         Shrimp,
         Fish,
         Tomato,
         `Bell peper`,
         Carrots,
         Cabbage,
         Sauce,
         Salsa_1,
         Cilantro,
         Onion,
         Taquito,
         Pineapple,
         Ham,
         `Chile relleno`,
         Nopales,
         Lobster,
         # Queso, absent in all obs.
         Egg,
         Mushroom,
         Bacon,
         Sushi,
         Avocado,
         Corn,
         Zucchini,
         Reviewer,
         Location,
         Burrito
  ) %>%
  mutate_at(vars(Beef:Zucchini), hot_encode) %>%
  mutate(Avocado = ifelse(Avocado + Guac > 0, 1, 0)) %>%
  select(-Guac) %>%
  rename(Salsa = Salsa_1) %>%
  rename(Nopal = Nopales) %>%
  rename(Chile_relleno = `Chile relleno`) %>%
  rename(Bell_pepper = `Bell peper`) %>%
  rename(Sour_cream = `Sour cream`) %>%
  mutate(target_user = Reviewer == "Scott") %>%
  mutate(num_ingredients = Beef
         + Pico
         + Cheese
         + Fries
         + Sour_cream
         + Pork
         + Chicken
         + Shrimp
         + Fish
         + Tomato
         + Bell_pepper
         + Carrots
         + Cabbage
         + Sauce
         + Salsa
         + Cilantro
         + Onion
         + Taquito
         + Pineapple
         + Ham
         + Chile_relleno
         + Nopal
         + Lobster
         + Egg
         + Mushroom
         + Bacon
         + Sushi
         + Avocado
         + Corn
         + Zucchini) %>%
  filter(num_ingredients >= 3) %>%
  group_by(Beef,
           Pico,
           Cheese,
           Fries,
           Sour_cream,
           Pork,
           Chicken,
           Shrimp,
           Fish,
           Tomato,
           Bell_pepper,
           Carrots,
           Cabbage,
           Sauce,
           Salsa,
           Cilantro,
           Onion,
           Taquito,
           Pineapple,
           Ham,
           Chile_relleno,
           Nopal,
           Lobster,
           Egg,
           Mushroom,
           Bacon,
           Sushi,
           Avocado,
           Corn,
           Zucchini) %>%
  summarise(Count = log(n())) %>%
  ungroup() %>%
  select_if(non_zero_var)

# select_if removes Sushi with zero varience

burrito_elements <- clean_data

# to gather burrito names,
#  1. comment out select_if(non_zero_var)
#  2. include names = getListOfNames(Burrito) in summarise function
# below are all manually found burrito nanes

burrito_names <- c(
  "Veg Out",
  "Lobster",
  "Grilled Fish Salmon",
  "Tilapia One",
  "Battered Fish, Fish", # 5
  "Fish",
  "Mahi",
  "Chicken Nopalito",
  "Chicken Avocado",
  "Adobada", # 10
  "Al Pastor, Adobada",
  "Al Pastor",
  "Adobada",
  "Al Pastor",
  "Hawaiian", # 15
  "Breakfast",
  "Chile Relleno",
  "Custom",
  "Holy Moly",
  "Combo Chicken", # 20
  "Bacon Breakfast",
  "Tegano",
  "Nutty",
  "Shrimp",
  "Chicken Asada",
  "Carnitas, Adobado, Baja Monster, Al Pastor",
  "Carnitas",
  "Chile Verde Pork",
  "Custom",
  "Chile Relleno and Carnitas", # 30
  "California Chicken",
  "California",
  "Local",
  "California Chicken",
  "California Chicken",
  "California - Pork Adobada",
  "Barbacoa, Asada",
  "Bandido",
  "Machaca",
  "Surf & Turf", # 40
  "Surf & Turf",
  "Surf & Turf",
  "619 Burrito Original",
  "Mixed",
  "2 in 1",
  "Surf & Turf, Fajitas",
  "Philly",
  "Especial",
  "Surf & Turf",
  "Fusion", # 50
  "El Hawaiiano",
  "Super",
  "California (only cheese)",
  "California",
  "California",
  "California Surf",
  "Bitchin California",
  "California",
  "California (Guac & Sour Cream)",
  "California Breakfast", # 60
  "Bomb",
  "Dave's California",
  "Carne Asada",
  "Addiction",
  "Carne Adobada",
  "Surf & Turf",
  "Surf & Turf",
  "Custom",
  "California",
  "Arizona", # 70
  "California",
  "Shredded Beef",
  "Carne Adada, Quesaburro",
  "Azteca",
  "Custom",
  "Quesa",
  "Carne Asada Everything",
  "Chimichanga Beef",
  "California, El Rusio",
  "California, Oaxacalifornia, California Everything", # 80
  "California Chipotle",
  "Surfin California",
  "California, California - Steak",
  "Monster California, California, California Everything, Campeon",
  "Cali Diablo"
)

# burrito_elements$Name <- burrito_names

burrito_elements_w_key <- burrito_elements %>%
  rownames_to_column('key')

pca = prcomp(burrito_elements %>% select(Beef:Zucchini), center = TRUE, scale = TRUE)

top_sig_loadings <- function(loadings, top_n = 5) {
  loadings %>%
    mutate(distance = sqrt(PC1 ^ 2 + PC2 ^ 2)) %>%
    top_n(top_n, distance)
}

# i.e., principal components

loadings <- as_tibble(pca$rotation, rownames = NA) %>%
  rownames_to_column('key') %>%
  select(key, PC1, PC2)

top_loadings <- top_sig_loadings(loadings, top_n = 31)

scores <- as_tibble(pca$x) %>%
  rownames_to_column('key') %>%
  left_join(burrito_elements_w_key, by = 'key') %>%
  select(PC1, PC2)

rescaled_scores <- as_tibble(scale(scores, center = TRUE, scale = TRUE))

set.seed(1234)

km = kmeans(rescaled_scores, 11)

clusters <- rescaled_scores %>%
  mutate(cluster = as.factor(km$cluster)) %>%
  rownames_to_column('key')

scores_and_clusters <- burrito_elements_w_key %>%
  left_join(clusters, by = "key")

pca_mapping <- scores_and_clusters %>%
  select(key, PC1, PC2) %>%
  bind_rows(top_loadings) %>%
  select(-distance) %>%
  rownames_to_column("temp_key")

pca_coef <- pca_mapping %>%
  select(PC1, PC2)

pca_mapping_empty <- pca_mapping %>%
  select(key, temp_key)

# scale(pca_coef) for mean/var scaling
# or robustscale(pca_coef)$data for med/quartiles

temp_scaled_pca_coef <- scale(pca_coef, center = TRUE, scale = TRUE)

scaled_pca_coef <- as_tibble(temp_scaled_pca_coef) %>%
  rownames_to_column("temp_key") %>%
  left_join(pca_mapping_empty, by = "temp_key") %>%
  select(-temp_key)

# scaled_pca_coef_one <- robustscale(pca_coef$PC1)
# scaled_pca_coef_two <- robustscale(pca_coef$PC2)
# 
# scaled_pca_coef <- pca_coef %>%
#   mutate(PC1 = scaled_pca_coef_one, PC2 = scaled_pca_coef_two)

# scaled_pca_coef_rec <- scaled_pca_coef %>%
#   filter(type == "R") %>%
#   select(-type)
# 
# scaled_pca_coef_pc <- scaled_pca_coef %>%
#   filter(type == "PC") %>%
#   select(-type)

scaled_scores_and_clusters <- scores_and_clusters %>%
  select(-c(PC1, PC2)) %>%
  left_join(scaled_pca_coef, by = "key")

rescale_loadings <- function(loading_elements) {
  max_pc_coef <- max(loading_elements)
  min_pc_coef <- min(loading_elements)
  med_pc_coef <- median(loading_elements)
  qua_pc_coef <- quantile(loading_elements)

  lapply(loading_elements, function(element) {
    if (abs(element) < 0.0625) {
      element * 16
    }
    else if (abs(element) < 0.125) {
      element * 8
    }
    else if(abs(element) < 0.25) {
      element * 4
    }
    else {
      element * 4
    }
  }) %>% unlist
}

scaled_top_loadings <- top_loadings %>%
  select(-c(PC1, PC2)) %>%
  left_join(scaled_pca_coef, by = "key") %>%
  mutate(PC1 = rescale_loadings(PC1),
         PC2 = rescale_loadings(PC2))

ggplot(scaled_scores_and_clusters,
       aes(x = PC1,
           y = PC2,
           color = cluster,
           size = count)) +
  geom_point(alpha = 1, alpha = 0.5) +
  scale_colour_brewer(palette="Spectral") +
  geom_text(data = scaled_top_loadings,
            aes(x = PC1,
                y = PC2,
                label = key),
            alpha = 1,
            size = 2,
            color = "black") +
  geom_hline(yintercept = 0, alpha = 0.2, color = "black") +
  geom_vline(xintercept = 0, alpha = 0.2, color = "black")

View(scaled_scores_and_clusters %>% filter(cluster == 4))
View(scaled_scores_and_clusters)
