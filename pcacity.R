
## load packages
library(dplyr)
library(psych)
library(pracma)
library(ggplot2)
dat1 <- read.csv('exhibition2022.csv')
dat <- dat1[,sapply(dat1,is.numeric)]
data.pca <- dat %>%
  select(!c(Aggregated.Score,X,Index))
data.scaled <- scale(data.pca)

pca.result <- prcomp(data.scaled, center = TRUE, scale. = TRUE)

# Summary of PCA results
summary(pca.result)
# Calculate the eigenvalues from the standard deviations


# Plotting the PCA
biplot(pca.result)
pca_mod <- pca(data.scaled, # our unscaled dataset
               nfactors = ncol(data.scaled), # we want all the components
               covar = FALSE, # setting covar to false means pca on the correlation matrix
               rotate = "none") # we don't want our components rotating

# Check the loadings
loadings=pca_mod$loadings
 # Note: the sign in PCA is arbitrary.
scores=pca_mod$scores
# Check the scores
head(pca_mod$scores)


## Interpretation of components
# Now that we've gone through all the procedures, let's get to the important part.
# Look again at the loadings: can you give an interpretation of the first and 
# second components?
loadings <- as.data.frame(pca.result$rotation)
loadings

# Can you visualise the relationship of the variables in the first two components?
loadings$name <- row.names(loadings)
ggplot(loadings, aes(PC1, PC2)) +
  geom_text(aes(label = name),size = 3, check_overlap = TRUE)

## How about two and three?
ggplot(loadings, aes(PC1, PC3)) +
  geom_text(aes(label = name))
pca_data <- data.frame(pca_result$x[, 1:2])

plot(scores[, 1], scores[, 3], xlab = "PC1", ylab = "PC3", main = "PCA of Cities")
text(scores[, 1], scores[, 3], labels = dat1$City, pos = 3, cex= 0.6)

top_loadings <- apply(loadings, 2, function(x) abs(x) > quantile(abs(x), 0.5))  # Adjust threshold as needed
loadings_filtered <- loadings[top_loadings,]
scores <- pca.result$x
top_scores <- apply(scores, 2, function(x) abs(x) > quantile(abs(x), 0.8))  # Adjust threshold as needed
scores_filtered <- scores[top_scores,]
# Selecting top scores


# Filter the scores using the logical vector
scores_filtered <- scores[top_scores, ]
# Plot
scores_df <- data.frame(PC1 = scores_filtered[,1], PC2 = scores_filtered[,2])
loadings_df <- data.frame(PC1 = loadings_filtered[,1]*max(abs(scores_filtered[,1])), 
                          PC2 = loadings_filtered[,2]*max(abs(scores_filtered[,2])), 
                          Variable = rownames(loadings_filtered))

ggplot() +
  geom_point(data = scores_df, aes(x = PC1, y = PC2), color = "blue") +
  geom_segment(data = loadings_df, aes(x = 0, y = 0, xend = PC1, yend = PC2), arrow = arrow(type = "closed"), color = "red") +
  geom_text(data = loadings_df, aes(x = PC1, y = PC2, label = Variable), vjust = -0.5, color = "red") +
  labs(title = "PCA Biplot - Top Features", x = "PC1", y = "PC2") +
  theme_minimal()
