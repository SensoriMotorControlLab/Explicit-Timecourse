

#####----------------------------------
###on non normal data


model_df <- xgRun()
extract <- getFeaturesFromModel() 
strategy_data <- read.csv("data/strategy_only_participants.csv")

#or with non normalized data:
features_df <- strategy_data %>%
  group_by(participant_id) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

kmeans_input <- features_df %>% select(-participant_id)

k_input   <- extract$kmeans_input 




# -------------------------------
# PCA 
pca <- prcomp(k_input, center = TRUE, scale. = FALSE) ##this normalized data!
pca$rotation <- pca$rotation*-1
pca$x<- pca$x*-1

pca_df <- as.data.frame(pca$x[,1:3])
colnames(pca_df) <- c("PC1","PC2","PC3")
pca_df$participant_id <-features_df$participant_id
distance_matrix <- dist(pca_df[, c("PC1", "PC2","PC3")], method = "euclidean")

hc <- hclust(distance_matrix, method = "ward.D2")
plot(hc, labels = pca_df$participant_id, main = "Hierarchical Clustering Dendrogram", xlab = "Participants", sub = "")

#partition a hierarchical clustering tree (dendrogram) into a specific number of groups or at a certain height

##run sillhoette to see what k should be 
library(cluster)
for (k in 2:10) {
  cl <- cutree(hc, k = k)
  sil <- silhouette(cl, dist(pca_df[,1:3]))
  cat("k =", k, " mean silhouette =", mean(sil[,3]), "\n")
}

clusters <- cutree(hc, k=6)   
table(clusters)
plot(hc, labels = pca_df$participant_id, main = "Hierarchical Clustering Dendrogram", xlab = "Participants", sub = "")



pca_df$cluster <- clusters

# Factor version for plotting
pca_df$cluster_label <- as.factor(clusters)
#Ward’s method is specifically designed to merge clusters in a way that minimizes increases in within-cluster variance.

library(ggplot2)

# Recode cluster labels
pca_df$cluster_label <- factor(
  pca_df$cluster,
  levels = c(1, 2, 3, 4, 5, 6),
  labels = c(
    "Light Exploration",
    "Medium Exploration",
    "Stepwise",
    "Gradual",
    "Grey Cluster",
    "High Exploration"
  )
)


# Custom colors
cluster_colors <- c(
  "Light Exploration"  = "#fca5a5",  # light red
  "Medium Exploration" = "#ef4444",  # medium red
  "High Exploration"   = "#991b1b",  # dark red
  "Gradual"            = "#8b5cf6",  # orange
  "Grey Cluster"       = "grey60",   # grey
  "Stepwise"           = "#14b8a6"   
)

ggplot(
  pca_df,
  aes(PC1, PC2, color = cluster_label)
) +
  geom_point(size = 3) +
  scale_color_manual(values = cluster_colors) +
  theme_classic() +
  labs(color = "Cluster")

####---compare with rot
model_df_with_clusters <- model_df %>%
  left_join(pca_df[,c("participant_id", "cluster_label")],
            by = "participant_id")


pca_df$cluster_label <- factor(
  pca_df$cluster,
  levels = c(1,2,3,4,5,6),
  labels = c(
    "Light Exploration",
    "Medium Exploration",
    "Stepwise",
    "Gradual",
    "Grey Cluster",
    "High Exploration"
  )
)


# Join safely
unique_participants <- strategy_data %>%
  distinct(participant_id)

strategy_data_clustered <- strategy_data %>%
  inner_join(participant_clusters, by = "participant_id")
proportions_df <- strategy_data_clustered %>% group_by(rotation, cluster_label) %>% 
  summarise(n_participants = n_distinct(participant_id), .groups = "drop") %>% 
  group_by(rotation) %>% mutate(prop = n_participants / sum(n_participants)) %>% 
  ungroup() 


strategy_df <- getStrategies()

total_per_rotation <- strategy_df %>%
  group_by(rotation) %>%
  summarise(total_n = n(), .groups = "drop")

non_strategy <- strategy_df %>%
  filter(strategy == "No") %>%
  group_by(rotation) %>%
  summarise(n_non_strategy = n(), .groups = "drop") %>%
  left_join(total_per_rotation, by = "rotation") %>%
  mutate(
    cluster_label = "Non-strategy",
    prop = n_non_strategy / total_n
  )

non_strategy <- non_strategy %>%
  rename(n_participants = n_non_strategy)

proportions_df <- proportions_df %>%
  mutate(cluster_label = trimws(as.character(cluster_label)))
valid_levels <- c(
  "Light Exploration",
  "Medium Exploration",
  "Stepwise",
  "Gradual",
  "Grey Cluster",
  "High Exploration",
  "Non-strategy"
)

proportions_df$cluster_label <- factor(proportions_df$cluster_label,
                                       levels = valid_levels)


non_strategy <- non_strategy %>%
  mutate(cluster_label = as.character(cluster_label))

proportions_combined <- bind_rows(proportions_df, non_strategy)

proportions_combined <- proportions_combined %>%
  filter(!is.na(cluster_label))

proportions_combined$cluster_label <- factor(
  proportions_combined$cluster_label,
  levels = c(
    "Non-strategy",
    "High Exploration",
    "Grey Cluster",
    "Gradual",
    "Stepwise",
    "Medium Exploration",
    "Light Exploration"
  )
)

##find percentages
rotation_totals <- proportions_combined %>%
  group_by(rotation) %>%
  summarise(total_n = sum(n_participants), .groups = "drop")

proportions_plot <- proportions_combined %>%
  group_by(rotation) %>%
  mutate(
    total_n = sum(n_participants),
    percent = n_participants / total_n * 100
  ) %>%
  ungroup() %>%
  filter(cluster_label != "Non-Strategy")


p <- ggplot(
  proportions_plot,
  aes(x = factor(rotation), y = percent, fill = cluster_label)
) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  
  scale_y_continuous(limits = c(0, 100)
  ) +
  labs(
    x = "Rotation Group",
    y = "Percentage of Participants (%)",
    title = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(),
    # axis.text.x  = element_text(size = 17),
    # axis.text.y  = element_text(size = 17),
    # axis.title.x = element_text(size = 17),
    # axis.title.y = element_text(size = 17),
    # legend.title = element_text(size = 17),
    # legend.text  = element_text(size = 16)
  ) 
print(p)





##visualize cluster 4
df <- strategy_data[strategy_data$participant_id == "8bc9fd", ]  # 9c8da7 (exploratory & gradual?), 396716 (2 strtagies?), 1cc592 & 8bc9fd (stepwise)
plot(df$aimdeviation_deg, type = "l", main = "", ylim = c(-58, 80),
     xlim = c(0, 232),
     col= "#c495c9", lwd = 2)
abline(v = 113, col = "grey", lty = 2, lwd = 1)
abline(h = 60, col = "grey", lty = 2, lwd = 1)

#cluster 4 is closest to cluster 2 - exploratory
df <- strategy_data[strategy_data$participant_id == "0c7728", ]  #2ff26d, 0c7728 6a544a
plot(df$aimdeviation_deg, type = "l", main = "", ylim = c(-58, 80),
     xlim = c(0, 232),
     col= "#c495c9", lwd = 2)
abline(v = 113, col = "grey", lty = 2, lwd = 1)
abline(h = 60, col = "grey", lty = 2, lwd = 1)


#cluster 3 middle - stepwise
df <- strategy_data[strategy_data$participant_id == "d6141d", ]  #70e8cb, abf95a, d1436b(lowkey gradual), 1c10b9
plot(df$aimdeviation_deg, type = "l", main = "", ylim = c(-58, 80),
     xlim = c(0, 232),
     col= "#c495c9", lwd = 2)
abline(v = 113, col = "grey", lty = 2, lwd = 1)
abline(h = 60, col = "grey", lty = 2, lwd = 1)


#cluster 1 - one or two negative aims (light explrotory or noisy stepwise)
df <- strategy_data[strategy_data$participant_id == "0c7728", ]  #205194,1cce80, 9eabc1, 4b9fad
plot(df$aimdeviation_deg, type = "l", main = "", ylim = c(-58, 80),
     xlim = c(0, 232),
     col= "#c495c9", lwd = 2)
abline(v = 113, col = "grey", lty = 2, lwd = 1)
abline(h = 60, col = "grey", lty = 2, lwd = 1)



##in 3 clusters, 2 and 1 combine which make sense
proportions_combined2 <- proportions_combined %>%
  mutate(cluster_label = as.character(cluster_label)) %>%
  filter(cluster_label != "Grey Cluster") %>%
  mutate(
    cluster_label = case_when(
      cluster_label %in% c("Light Exploration", "Medium Exploration", "High Exploration") ~ "1_2_6_group",
      TRUE ~ cluster_label
    )
  )



cont_table <- proportions_combined2 %>%
  group_by(rotation, cluster_label) %>%
  summarise(n = sum(n_participants), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = cluster_label,
                     values_from = n,
                     values_fill = 0) %>%
  as.data.frame()

rownames(cont_table) <- cont_table$rotation
cont_table$rotation <- NULL

cont_table <- as.matrix(cont_table)

#                       -----------CHI SQUARE-------------
# set.seed(123)
# 
# test <- chisq.test(
#   cont_table,
#   simulate.p.value = TRUE,
#   B = 10000
# )
# 
# #As a form of post hoc analysis the standarized residuals can be analysed. A rule of thumb is that standarized residuals of above two show significance.
# test$stdres

#                       -----------LME & GLM-------------

install.packages("lme4")
library(lme4)
#Fit a linear mixed-effects model to assess likelihood of phenotype in rotation group
#LME extends standard linear regression by splitting variance into fixed effects (the systematic impact of your experimental variables) and random effects (the random variation across subjects or groups).

#Fixed Effects: These are the factors you manipulate or want to generalize across the population. Rotation Size is a fixed effect because you want to know how different sizes explicitly alter the strategy phenotype.
#Random Effects: These represent the random variation among your experimental units. Subject ID is a random effect because you sampled a subset of individuals from a larger population
library(lme4)

#Question 1: Does rotation size affect the probability of developing any strategy?
dat <- proportions_combined2 %>%
  mutate(strategy_success = ifelse(cluster_label == "Non-strategy",
                                   0,
                                   n_participants),
         strategy_failure = ifelse(cluster_label == "Non-strategy",
                                   n_participants,
                                   0))

dat2 <- proportions_combined2 %>%
  group_by(rotation) %>%
  summarise(
    non_strategy = sum(n_participants[cluster_label == "Non-strategy"]),
    strategy = sum(n_participants[cluster_label != "Non-strategy"]),
    total = sum(n_participants)
  )

m0 <- glm(cbind(strategy, non_strategy) ~ 1,
          family = binomial,
          data = dat2)

m1 <- glm(cbind(strategy, non_strategy) ~ rotation,
          family = binomial,
          data = dat2)

AIC(m0, m1)

deltaAIC <- AIC(m0) - AIC(m1)
deltaAIC


#question 2: does rfotation size affect cluster NOTE: grey cluster is excluded here (n=3)
#Does Stepwise increase/decrease with rotation overall? Does Gradual appear/disappear with rotation? Does 1_2_6 change in prevalence?

library(nnet)
make_model <- function(cluster_name){ ##assessing cluster specific change binomial one vs all approach
  
  dat <- proportions_combined2 %>%
    group_by(rotation) %>%
    summarise(
      success = sum(n_participants[cluster_label == cluster_name]),
      failure = sum(n_participants[cluster_label != cluster_name])
    )
  
  m0 <- glm(cbind(success, failure) ~ 1,
            family = binomial,
            data = dat)
  
  m1 <- glm(cbind(success, failure) ~ rotation,
            family = binomial,
            data = dat)
  
  return(c(
    cluster = cluster_name,
    deltaAIC = AIC(m0) - AIC(m1)
  ))
}


clusters <- unique(proportions_combined2$cluster_label)

results <- t(sapply(clusters, make_model))



clean_dat <- proportions_combined2 %>%
  group_by(rotation, cluster_label) %>%
  summarise(n_participants = sum(n_participants),
            .groups = "drop")

m1 <- multinom(cluster_label ~ rotation,
               weights = n_participants,
               data = clean_dat)

newdat <- data.frame(
  rotation = c(20, 30, 40, 50, 60)
)

pred <- predict(m1, newdata = newdat, type = "probs")