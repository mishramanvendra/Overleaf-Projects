# Load and process all CSV files
files <- list.files(pattern = "*.csv")

process_file <- function(file) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  colnames(df)[1] <- "country"
  df$country <- standardize_country_names(df$country)
  df[-1] <- lapply(df[-1], as.character)
  df <- df %>% mutate(across(-country, convert_numeric_values))
  
  df_long <- df %>%
    pivot_longer(-country, names_to = "Year", values_to = "Value") %>%
    mutate(Year = as.numeric(str_extract(Year, "\\d+"))) %>%
    filter(!is.na(Value))
  
  selected_year <- df_long %>%
    group_by(Year) %>%
    summarise(non_missing = sum(!is.na(Value)), .groups = "drop") %>%
    arrange(desc(Year)) %>%
    filter(non_missing >= 0.8 * max(non_missing)) %>%
    slice(1) %>% pull(Year)
  
  df_best <- df_long %>% filter(Year == selected_year)
  indicator <- tools::file_path_sans_ext(basename(file))
  colnames(df_best)[colnames(df_best) == "Value"] <- indicator
  
  df_best %>%
    select(-Year) %>%
    group_by(country) %>%
    summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")
}

data_list <- lapply(files, process_file)

# Merge datasets and clean
merged_data <- reduce(data_list, full_join, by = "country") %>%
  filter(!apply(across(where(is.numeric)), 1, function(x) all(x == 0, na.rm = TRUE)))

merged_data$country <- standardize_country_names(merged_data$country)
merged_data <- add_geo_income_groups(merged_data)

# Impute missing numeric values with mean
data_clean <- merged_data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  filter(!is.na(Income))

# PCA for dimensionality reduction
numeric_data <- data_clean %>% select(where(is.numeric))
pca_result <- prcomp(numeric_data, center = TRUE, scale. = TRUE)
pca_reduced <- as.data.frame(pca_result$x[, 1:8])
data_reduced <- bind_cols(data_clean %>% select(country, Income, Geog), pca_reduced)

# K-means clustering
set.seed(123)
scaled_data <- scale(data_reduced[, paste0("PC", 1:8)])
kmeans_result <- kmeans(scaled_data, centers = 4, nstart = 25)
data_reduced$Cluster <- as.factor(kmeans_result$cluster)

# Elbow plot
wss <- sapply(1:10, function(k) kmeans(scaled_data, centers = k, nstart = 25)$tot.withinss)
ggplot(data.frame(Clusters = 1:10, WSS = wss), aes(x = Clusters, y = WSS)) +
  geom_line() + geom_point() +
  labs(title = "Elbow Method for Optimal Clusters")

# PCA cluster plot
ggplot(data_reduced, aes(PC1, PC2, color = Cluster)) +
  geom_point(size = 2) + theme_minimal() +
  labs(title = "K-means Clusters on PC1 vs PC2")

# Cluster profiling
cluster_profile <- data_reduced %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), list(mean = mean), .names = "{.col}_mean"))

# Parallel coordinates plot
scaled_df <- data_reduced %>%
  select(where(is.numeric)) %>%
  scale() %>%
  as.data.frame()
scaled_df$Cluster <- data_reduced$Cluster

ggparcoord(scaled_df, columns = 1:(ncol(scaled_df) - 1),
           groupColumn = "Cluster", scale = "globalminmax") +
  labs(title = "Parallel Coordinates Plot of Clusters")