###############################################################
#   CLUSTER ANALYSIS                                          #
###############################################################

# Load required packages ----------------------------------------------------

library(haven)       # to read Stata files
library(tidyverse)   # data manipulation
library(psych)       # factor analysis
library(factoextra)  # clustering visuals
library(cluster)     # clustering utilities


# ---------------------------------------------------------------------------
# 1. LOAD THE DATA
# ---------------------------------------------------------------------------

# Load the original dataset in Stata format
ps <- read_dta("/PATH_TO/DATA_PS.dta")

# Convert labelled variables into factors so they behave correctly in R
ps <- ps %>% mutate(across(where(is.labelled), ~as_factor(.)))


# ---------------------------------------------------------------------------
# 2. CONVERT ALL *_orig VARIABLES TO NUMERIC
# ---------------------------------------------------------------------------
# *_orig variables contain the true numeric codes.
# factors must be converted to numeric manually to avoid losing information.

ps_num <- ps %>% 
  mutate(across(matches("^d[0-9]+_orig$"),
                ~as.numeric(as.character(.))))

summary(ps_num$d009_orig)


# ---------------------------------------------------------------------------
# 3. FACTOR ANALYSIS (PCF) FOR EACH BLOCK OF VARIABLES
# ---------------------------------------------------------------------------
# We define a helper function to compute:
# 1) pairwise correlation matrix
# 2) principal axis factor analysis (fm="pa")
# 3) factor scores for each subject

compute_factor <- function(data, vars) {
  
  # Compute pairwise correlation matrix (required for missing data)
  R <- cor(data[, vars], use = "pairwise.complete.obs")
  
  # Run factor analysis with 1 factor, principal axis
  fa_res <- fa(r = R, nfactors = 1, fm = "pa")
  # --- Print variance explained ---
  cat("\nVariance explained by the factor:\n")
  print(fa_res$Vaccounted)
  
  # --- Print loadings ---
  cat("\nFactor loadings:\n")
  print(fa_res$loadings)
  
  # Compute factor scores from raw data
  # Score_i=w1Xi1 + w2Xi2 + w3Xi3
  scores <- factor.scores(data[, vars], fa_res)$scores[,1]
  
  return(scores)
}


# --- TRIAGE & WAITING TIME --------------------------------------------------
# factor: d009_orig, d010_orig, d016_orig
ps_num$attesa <- compute_factor(ps_num,
                                c("d009_orig", "d010_orig", "d016_orig"))


# --- COMFORT & CLEANLINESS --------------------------------------------------
# factor: d017_orig, d018_orig
ps_num$comfort <- compute_factor(ps_num,
                                 c("d017_orig", "d018_orig"))


# --- ASSISTANCE & COMMUNICATION --------------------------------------------
# factor: d021_orig, d022_orig, d023_orig, d024_orig
ps_num$assistenza <- compute_factor(ps_num,
                                    c("d021_orig", "d022_orig", "d023_orig", "d024_orig"))


# --- DOCTORS ----------------------------------------------------------------
# factor: d026_orig, d027_orig, d028_orig, d029_orig
ps_num$medici <- compute_factor(ps_num,
                                c("d026_orig", "d027_orig", "d028_orig", "d029_orig"))


# --- NURSES -----------------------------------------------------------------
# factor: d031_orig, d032_orig, d033_orig, d034_orig
ps_num$infermieri <- compute_factor(ps_num,
                                    c("d031_orig", "d032_orig", "d033_orig", "d034_orig"))


# ---------------------------------------------------------------------------
# 4. CREATE THE DATASET FOR CLUSTER ANALYSIS
# ---------------------------------------------------------------------------

cluster_data <- ps_num %>% 
  select(attesa, comfort, assistenza, medici, infermieri) %>%
  na.omit()    # Remove rows with missing scores

# Standardize the variables (important for Ward's method)
cluster_scaled <- scale(cluster_data)


# ---------------------------------------------------------------------------
# 5. HIERARCHICAL CLUSTERING
# ---------------------------------------------------------------------------

# Compute Euclidean distance matrix
dist_mat <- dist(cluster_scaled, method = "euclidean")

# Perform Ward's hierarchical clustering 
hc <- hclust(dist_mat, method = "ward.D2") # 'single - complete - average'

# Model output
hc$merge
hc$height
hc$order

# Plot dendrogram
plot(hc,
     labels = FALSE,
     main = "Ward Hierarchical Clustering Dendrogram",
     xlab = "Observations",
     sub = "")

# Optional: show cluster boundaries
rect.hclust(hc, k = 3, border = "red")


# ---------------------------------------------------------------------------
# 6. CUT TREE INTO 3 CLUSTERS
# ---------------------------------------------------------------------------

clusters3 <- cutree(hc, k = 3)

# Attach cluster assignments back to cluster_data
cluster_data$cluster3 <- clusters3


# ---------------------------------------------------------------------------
# 7. CLUSTER SUMMARY
# ---------------------------------------------------------------------------

# Mean factor scores by cluster
cluster_summary <- cluster_data %>%
  group_by(cluster3) %>%
  summarise(across(c(attesa, comfort, assistenza, medici, infermieri),
                   mean, na.rm = TRUE))

print(cluster_summary)



# ---------------------------------------------------------------------------
# 8. K-MEANS CLUSTERING
# ---------------------------------------------------------------------------

set.seed(123)
kmeans_res <- kmeans(cluster_scaled, centers = 3, nstart = 25)

# Model output
kmeans_res$cluster
kmeans_res$centers
kmeans_res$withinss
kmeans_res$size

################################################################################
# Using centers from previous hierarchical (dendogram) analysis
init_cent <- tapply(cluster_scaled, list(rep(cutree(hc,3), 
                                             ncol(cluster_scaled)), 
                                         col(cluster_scaled)), mean)
kmeans_dend <- kmeans(cluster_scaled, init_cent, 25)
kmeans_dend$size

################################################################################

fviz_cluster(kmeans_res,
             data = cluster_scaled,
             main = "K-means Clustering on Factor Scores")

################################################################################

# ---------------------------------------------------------------------------
# 9. PCA
# ---------------------------------------------------------------------------

library(readr)
data_wdbc <- read.csv("/PATH_TO/WDBC.csv")
head(data_wdbc)

# Removing "id" and useless cols
num_cols <- ncol(data_wdbc)
data_wdbc <- data_wdbc[,-c(1, num_cols)]
str(data_wdbc)

# Factorize "diagnosis"
data_wdbc$diagnosis <- as.factor(data_wdbc$diagnosis)
head(data_wdbc)

# Scaling
data_wdbc_scaled <- data_wdbc

for (col_name in colnames(data_wdbc)) {
  if (is.numeric(data_wdbc[[col_name]])) {
    data_wdbc_scaled[[col_name]] <- scale(data_wdbc[[col_name]])
  }
}

data_wdbc <- data_wdbc_scaled

# EDA
diagnosis.table <- table(data_wdbc$diagnosis)
colors <- c("#0072B2", "#56B4E9")

diagnosis.prop.table <- prop.table(diagnosis.table) * 100
diagnosis.prop.df <- as.data.frame(diagnosis.prop.table)

pielabels <- sprintf("%s - %3.1f%s", diagnosis.prop.df[,1], diagnosis.prop.table, "%")

pie(diagnosis.prop.table,
    labels = pielabels,  
    clockwise = TRUE,
    col = colors,
    border = "gainsboro",
    radius = 0.8,
    cex = 1, 
    main = "")
custom_labels <- c("M = Malignant", "B = Bening")

# Correlation matrix
library(corrplot)
library(caret)
corr_matrix <- cor(data_wdbc[,2:31])
corrplot(corr_matrix, order = "hclust", tl.cex = 0.7)

highlyCor <- colnames(data_wdbc)[findCorrelation(corr_matrix, cutoff = 0.9, verbose = TRUE)]

# Removing high correlated variables
data_wdbc_corr <- data_wdbc[, which(!colnames(data_wdbc) %in% highlyCor)]
ncol(data_wdbc_corr)

################################################################################

# PCA
data_pca <- prcomp(data_wdbc[, 2:31], center = TRUE, scale = TRUE)
plot(data_pca, type = "l", main = '')
grid(nx = 10, ny = 14)
title(main = "Principal components weight", sub = NULL, xlab = "Components")
box()

summary(data_pca)

pca_var <- data_pca$sdev^2
pve_df <- pca_var / sum(pca_var)
cum_pve <- cumsum(pve_df)
pve_table <- tibble(comp = 1:(ncol(data_wdbc) - 1), pve = pve_df, cum_pve = cum_pve)

ggplot(pve_table, aes(x = comp, y = cum_pve)) + 
  geom_point() + 
  geom_abline(intercept = 0.7263637, color = "red", slope = 0)

# To visualize which features are the most influent (Using: data_pca)
pca_df <- as.data.frame(data_pca$x)
ggplot(pca_df, aes(x=PC1, y=PC2, col=data_wdbc$diagnosis)) + geom_point(alpha=0.5)

# PCA Plot
plot_PCA <-fviz_pca_var(data_pca,
                        col.var = "contrib", # Color by contributions to the PC
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                        repel = TRUE ) # Avoid text overlapping

explained_variance <- data_pca$sdev^2 / sum(data_pca$sdev^2) * 100
pc1_var <- round(explained_variance[1], 1)
pc2_var <- round(explained_variance[2], 1)
plot_PCA + labs(x = paste0("PC1 (", pc1_var, "%)"), y = paste0("PC2 (", pc2_var, "%)"))
varpca <- get_pca_var(data_pca)
varpca
corrplot(varpca$cos2, is.corr=FALSE)

################################################################################
# END
################################################################################