library(dplyr)
library(factoextra)
library(ggplot2)
library(gridExtra)
library(tidyr)

# Read the data
url <- "https://github.com/SharkTuTu/738-project-final/raw/main/data.csv"
data <- read.csv(url)
# View the data
head(data)
# Clean the data
clear_data <- na.omit(data)

# Select columns related to outer nest size
outer_pca_data <- clear_data %>%
  select(`Outer_volume..cm3`, `Mean_temperature`, `Mean_rainfall`, `Range_midpoint_latitude`, 
         `Insularity`, `Loc_rock`, `Loc_cavity_obligatory`, `HBW_names`)

# Convert to numeric type (except species column)
outer_pca_data <- outer_pca_data %>%
  mutate(across(-`HBW_names`, ~ as.numeric(gsub("[^0-9.-]", "", as.character(.)))))
# Remove NA values
outer_pca_data <- na.omit(outer_pca_data)

# Check the variance of each column (remove zero variance columns)
variances <- apply(select(outer_pca_data, -`HBW_names`), 2, var)
zero_variance_cols <- names(variances[variances == 0])
print(zero_variance_cols)
# Remove zero variance columns
outer_pca_data <- outer_pca_data %>%
  select(-one_of(zero_variance_cols))
# Check again for constant columns
variances <- apply(select(outer_pca_data, -`HBW_names`), 2, var)
print(variances)

# PCA analysis of outer nest size
outer_pca_result <- prcomp(select(outer_pca_data, -`HBW_names`), center = TRUE, scale. = TRUE)
summary(outer_pca_result)
# Draw Scree Plot
fviz_eig(outer_pca_result, addlabels = TRUE, ylim = c(0, 70))

# Define plot_loading function
loadings <- outer_pca_result$rotation
loadings_df <- as.data.frame(loadings)
loadings_df$Variable <- rownames(loadings_df) 
loadings_long <- loadings_df %>%
  pivot_longer(cols = starts_with("PC"), names_to = "PC", values_to = "Loading")

plot_loading <- function(pc, loadings_long) {
  ggplot(loadings_long %>% filter(PC == pc), aes(x = Variable, y = Loading, fill = Loading > 0)) +
    geom_bar(stat = "identity", color = "black") +
    scale_fill_manual(values = c("TRUE" = "#00AFBB", "FALSE" = "#FC4E07")) +
    labs(title = paste("Loadings on", pc), x = "Variables", y = "Loadings") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Draw loading plots for PC1 to PC4
plot_list <- lapply(c("PC1", "PC2", "PC3", "PC4"), plot_loading, loadings_long = loadings_long)
do.call(grid.arrange, c(plot_list, ncol = 2))

# Extract PCA scores and ensure data consistency
outer_pca_scores <- as.data.frame(outer_pca_result$x)
matched_data <- outer_pca_data[complete.cases(outer_pca_data), ]  
# Add species information and outer nest size information
outer_pca_scores$Species <- matched_data$`HBW_names`
outer_pca_scores$Outer_volume <- matched_data$`Outer_volume..cm3`
# Log transform the response variable
outer_pca_scores$log_Outer_volume <- log(outer_pca_scores$Outer_volume + 1)  

# Linear regression model (log transformed)
lm_model_log <- lm(log_Outer_volume ~ PC1 + PC2 + PC3 + PC4, data = outer_pca_scores)
summary(lm_model_log)
# Analyze residuals
par(mfrow = c(2, 2))
plot(lm_model_log)
# Draw regression coefficients plot
coef_df_log <- data.frame(
  Term = names(coef(lm_model_log)),
  Estimate = coef(lm_model_log),
  StdError = summary(lm_model_log)$coefficients[, "Std. Error"],
  tValue = summary(lm_model_log)$coefficients[, "t value"],
  pValue = summary(lm_model_log)$coefficients[, "Pr(>|t|)"]
)

ggplot(coef_df_log[-1,], aes(x = Term, y = Estimate, fill = pValue < 0.05)) +
  geom_bar(stat = "identity", color = "black") +
  geom_errorbar(aes(ymin = Estimate - StdError, ymax = Estimate + StdError), width = 0.2) +
  scale_fill_manual(values = c("TRUE" = "#00AFBB", "FALSE" = "#FC4E07")) +
  labs(title = "Regression Coefficients (Log Transformed)", y = "Estimate", x = "Term") +
  theme_minimal()

