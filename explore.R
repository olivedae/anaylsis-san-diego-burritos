library(tidyverse)
library(plotly)
library(arules)
library(tsne)

setwd('~/Dropbox/Dev/burritoooz')

data <- read_csv('data/burrito.csv') %>%
  filter(!is.na(overall))

View(data)

fours_and_above <- data %>%
  filter(overall >= 4)

View(fours_and_above)

ggplot(fours_and_above, aes(overall)) + 
  geom_histogram(binwidth = 0.1, color = "white")
ggplot(data, aes(overall)) + 
  geom_histogram(binwidth = 0.25, color = "white")
ggplot(data, aes(Reviewer)) + 
  geom_bar()

high_reviewers <- data %>%
  group_by(Reviewer) %>%
  summarise(freq = n()) %>%
  filter(freq > 25)

med_reviewers <- data %>%
  group_by(Reviewer) %>%
  summarise(freq = n()) %>%
  filter(freq > 1) %>%
  filter(freq < 25)

one_reviewers <- data %>%
  group_by(Reviewer) %>%
  summarise(freq = n()) %>%
  filter(freq == 1)

high_reviews <- data %>%
  filter(Reviewer %in% high_reviewers$Reviewer) %>%
  mutate(r_type = "high")

med_reviews <- data %>%
  filter(Reviewer %in% med_reviewers$Reviewer) %>%
  mutate(r_type = "med")

one_reviews <- data %>%
  filter(Reviewer %in% one_reviewers$Reviewer) %>%
  mutate(r_type = "one")

classified_reviews <- union(high_reviews, union(med_reviews, one_reviews))

reviewers <- data %>%
  group_by(Reviewer) %>%
  summarise(freq = n())

ggplot(reviewers, aes(freq)) +
  geom_histogram(binwidth = 1, color = "white")

contrib_weights <- reviewers %>%
  group_by(freq) %>%
  summarise(n = n()) %>%
  mutate(weight = freq * n)

ggplot(contrib_weights,
       aes(x = as.factor(freq),
           y = weight)) +
  geom_bar(stat = "identity")

ggplot(high_reviews, aes(overall, fill = Reviewer)) +
  geom_histogram(color = "white")

ggplot(med_reviews, aes(overall)) +
  geom_histogram(binwidth = 0.25, col = "white")

ggplot(one_reviews, aes(overall)) +
  geom_histogram(binwidth = 0.25, col = "white")

bin_ratings <- function(ratings) {
  lapply(ratings, function(rating) {
    if (rating < 2) {
      "1"
    }
    else if (rating < 3) {
      "2"
    }
    else if (rating < 4) {
      "3"
    }
    else if (rating < 5) {
      "4"
    }
    else {
      "5"
    }
  }) %>% unlist
}

classified_reviews <- classified_reviews %>%
  mutate(bin_rating = bin_ratings(overall))

ggplot(classified_reviews, aes(bin_rating)) +
  geom_bar(aes(fill = r_type), position = "dodge")

ggplot(classified_reviews,
       aes(overall, fill = r_type)) +
  geom_histogram(color = "white",
                 show.legend = FALSE,
                 binwidth = 0.25)

ggplot(data, aes(overall, fill = Reviewer)) +
  geom_histogram(color = "white",
                 show.legend = FALSE)

View(includes_websites)

resturants <- data %>%
  group_by(Location) %>%
  filter(!is.na(overall) && length(overall) > 1) %>%
  summarise(sd = sd(overall)) %>%
  filter(!is.na(sd))

ggplot(resturants, aes(x = Location, y = sd)) +
  geom_bar(stat = "identity")

ggplot(resturants, aes(sd)) +
  geom_histogram(binwidth = .05, color = "white")

ggplot(data %>% filter(!is.na(Neighborhood)), aes(overall, fill = Neighborhood)) +
  geom_histogram(binwidth = .25, 
                 color = "white", 
                 show.legend = FALSE)

ggplot(data %>% filter(!is.na(overall)), aes(overall, fill = Location)) +
  geom_histogram(binwidth = .25, 
                 color = "white", 
                 show.legend = FALSE)

overall_reviewers <- data %>%
  group_by(Reviewer) %>%
  filter(!is.na(overall)) %>%
  summarise(mean = mean(overall),
            sd = sd(overall),
            med = median(overall),
            freq = n())

ggplot(overall_reviewers, aes(mean)) +
  geom_histogram(binwidth = .1,
                 color = "white")

mid <- mean(overall_reviewers$med)

ggplot(overall_reviewers,
       aes(x = mean,
           y = sd,
           size = freq,
           color = med)) +
  geom_point(na.rm = TRUE) +
  scale_color_gradient2(midpoint = mid,
                        low = "red",
                        mid = "yellow",
                        high = "green")

melt_sd <- function(sds) {
  lapply(sds, function(sd) {
    if (is.na(sd)) {
      0
    }
    else {
      sd
    }
  }) %>% unlist
}

overall_reviewers_diffs <- overall_reviewers %>%
  mutate(reg = mean - med) %>%
  mutate(sd = melt_sd(sd))

ggplot(overall_reviewers_diffs,
       aes(x = mean,
           y = reg,
           size = freq,
           color = sd)) +
  geom_point(na.rm = TRUE) +
  scale_color_gradient2(midpoint = 0.5,
                        low = "blue",
                        mid = "white",
                        high = "green")

get_store_info <- function(inputs) {
  inputs <- na.omit(inputs)
  
  if (length(inputs) == 0) {
    ""
  }
  else {
    inputs[1]
  }
}

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

# run next line before this to see differences in spelling
cleaner_data <- data %>%
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
  mutate(Location = clean_location_name(Location, "Vallarta Express", "Vallarta express"))

locations <- cleaner_data %>%
  group_by(Location) %>%
  summarise(neighborhood = get_store_info(Neighborhood),
            address = get_store_info(Address),
            url = get_store_info(URL))

View(locations)

## pca

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

burrito_elements <- cleaner_data %>%
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
  mutate(Avocado = ifelse(Avocado || Guac, 1, 0)) %>%
  select(-Guac) %>%
  rename(Salsa = Salsa_1) %>%
  rename(Nopal = Nopales) %>%
  rename(Chile_relleno = `Chile relleno`) %>%
  rename(Bell_pepper = `Bell peper`) %>%
  rename(Sour_cream = `Sour cream`) %>%
  mutate(target_user = Reviewer == "Scott") %>%
  filter(Beef 
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
         + Zucchini >= 3)

burrito_elements_w_key <- burrito_elements %>%
  rownames_to_column('key')

burrito_tsne <- tsne(burrito_elements %>% select(Beef:Zucchini))
colnames(burrito_tsne) <- c("feat_1", "feat_2")
tsne_tb <- as_tibble(burrito_tsne)

ggplot(tsne_tb, aes(x = feat_1, y = feat_2)) +
  geom_point()

km = kmeans(tsne_tb, 15)

clusters_2 <- tsne_tb %>%
  mutate(cluster = as.factor(km$cluster)) %>%
  rownames_to_column('key')

elements_and_cluster <- burrito_elements_w_key %>%
  left_join(clusters_2, by = "key")

ggplot(elements_and_cluster,
       aes(x = feat_1,
           y = feat_2,
           color = cluster)) +
  geom_point()

pc = prcomp(burrito_elements %>% select(Beef:Zucchini), center = TRUE, scale = FALSE)

top_sig_loadings <- function(loadings, top_n = 5, d = 2) {
  loadings %>%
    mutate(distance = sqrt(PC1 ^ 2 + PC2 ^ 2)) %>%
    top_n(top_n, distance)
}

pc_loadings <- as_tibble(pc$rotation, rownames = NA) %>%
  rownames_to_column('key')

top_loadings <- top_sig_loadings(pc_loadings, top_n = 31)

top_loadings_scaled <- top_loadings %>%
  mutate(PC1 = PC1 * 1, PC2 = PC2 * 1)

biplot(pc, scale = 0)

p_components <- as_tibble(pc$x) %>%
  rownames_to_column('key')

ggplot(p_components,
       aes(x = PC1,
           y = PC2)) +
  geom_point(show.legend = FALSE)

set.seed(1234)

p_components_2 <- p_components %>%
  select(PC1, PC2)

km = kmeans(p_components_2, 11)

clusters_2 <- p_components_2 %>%
  mutate(cluster = as.factor(km$cluster)) %>%
  rownames_to_column('key')

elements_and_cluster <- burrito_elements_w_key %>%
  left_join(clusters_2, by = "key")

ggplot(elements_and_cluster,
       aes(x = PC1,
           y = PC2,
           color = cluster)) +
  geom_point(alpha = 1, size = 1) +
  scale_colour_brewer(palette="Spectral") +
  geom_text(data = top_loadings_scaled,
            aes(x = PC1,
                y = PC2,
                label = key),
            alpha = 1,
            size = 2,
            color = "black") +
  geom_hline(yintercept = 0, alpha = 0.2, color = "black") +
  geom_vline(xintercept = 0, alpha = 0.2, color = "black")

View(elements_and_cluster %>% filter(cluster == 5))
View(elements_and_cluster)

plot_ly(elements_and_cluster, x = ~PC1, y = ~PC2, color = ~cluster) %>%
  add_markers()

ggplot(clusters_2,
       aes(x = PC1,
           y = PC2,
           color = cluster)) +
  geom_point(show.legend = FALSE) +
  scale_colour_brewer(palette="Spectral")

p_components_3 <- p_components %>%
  select(PC1, PC2, PC3)

km = kmeans(p_components_3, 5)

clusters_3 <- p_components_3 %>%
  mutate(cluster = as.factor(km$cluster)) %>%
  rownames_to_column('key')

elements_and_cluster_3 <- burrito_elements_w_key %>%
  left_join(clusters_3, by = "key")

plot_ly(elements_and_cluster_3, x = ~PC1, y = ~PC2, z = ~PC3, color = ~cluster) %>%
  add_markers()

## assoc
## http://r-statistics.co/Association-Mining-With-R.html

single_trans <- burrito_elements %>%
  rownames_to_column('key') %>%
  gather(ingr_type, ingr_value, -key) %>%
  filter(ingr_value == 1) %>%
  select(-ingr_value) %>%
  mutate(key = as.factor(key)) %>%
  rename(item = ingr_type)

write_csv(single_trans, "data/transactions.csv", col_names = TRUE)

transactions <- read.transactions("data/transactions.csv", sep = ",", format = "single", cols = c("key", "item"))

rules <- apriori(transactions, parameter = list(supp = 0.01, conf = 0.7, maxlen = 15))

rules_conf <- sort(rules, by = "confidence", decreasing = TRUE)
inspect(head(rules_conf, 15))

rules_conf <- sort(rules, by = "lift", decreasing = TRUE)
inspect(head(rules_conf, 15))

subset_rules <- which(colSums(is.subset(rules, rules)) > 1)
length(subset_rules)

unique_rules <- rules[-subset_rules]

rules_conf <- sort(rules, by = "confidence", decreasing = FALSE)
inspect(head(rules_conf, 15))

rules_conf <- sort(rules, by = "support", decreasing = FALSE)
inspect(head(rules_conf, 35))
