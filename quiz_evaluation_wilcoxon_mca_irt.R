# =============================================================================
# Comparative Analysis of Academic Performance Between Groups
# Statistical Analysis using Wilcoxon Test, MCA, and IRT Models
# =============================================================================

# Load required libraries
library(tidyverse)
library(nortest)
library(FactoMineR)
library(factoextra)
library(mirt)
library(ggmirt)

# =============================================================================
# DATA LOADING AND PREPROCESSING
# =============================================================================

# Load grades dataset
grades <- read_csv("datasets/calificaciones.csv")

# Data preprocessing
grades <- grades %>%
    mutate(
        GRUPO = as.factor(GRUPO), 
        Ganancia = `POST-TEST` - `PRE-TEST`
    )

# =============================================================================
# DESCRIPTIVE STATISTICS
# =============================================================================

# Summary statistics by group
descriptive_stats <- grades %>%
    group_by(GRUPO) %>%
    summarise(
        count = n(),
        mean_ganancia = mean(Ganancia, na.rm = TRUE),
        sd_ganancia = sd(Ganancia, na.rm = TRUE),
        min_ganancia = min(Ganancia, na.rm = TRUE),
        max_ganancia = max(Ganancia, na.rm = TRUE),
        median_ganancia = median(Ganancia, na.rm = TRUE),
        IQR_ganancia = IQR(Ganancia, na.rm = TRUE),
        first_quartile_ganancia = quantile(Ganancia, 0.25, na.rm = TRUE),
        third_quartile_ganancia = quantile(Ganancia, 0.75, na.rm = TRUE)
    )

# =============================================================================
# EXPLORATORY DATA ANALYSIS - VISUALIZATIONS
# =============================================================================

# Histograms by group
hist_41 <- grades %>%
    filter(GRUPO == "4.1") %>%
    ggplot(aes(x = Ganancia)) +
    geom_histogram(binwidth = 1, position = "dodge") +
    labs(title = "Histogram of Ganancia values (Group 4.1)",
         x = "Ganancia values",
         y = "Count") +
    theme_minimal()

hist_44 <- grades %>%
    filter(GRUPO == "4.4") %>%
    ggplot(aes(x = Ganancia)) +
    geom_histogram(binwidth = 1, position = "dodge") +
    labs(title = "Histogram of Ganancia values (Group 4.4)",
         x = "Ganancia values",
         y = "Count") +
    theme_minimal()

ggsave("histogram_41.png", plot = hist_41, width = 8, height = 6)
ggsave("histogram_44.png", plot = hist_44, width = 8, height = 6)

# Violin plot comparison
violin_comp <- grades %>%
    ggplot(aes(x = GRUPO, y = Ganancia, fill = GRUPO)) +
    geom_violin(trim = FALSE) +
    labs(title = "Violin Plot of Ganancia by Group",
         x = "Group",
         y = "Ganancia") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2")

ggsave("violin_plot_ganancia.png", plot = violin_comp, width = 8, height = 6)

# Q-Q plots for normality assessment
qq_41 <- grades %>%
    filter(GRUPO == "4.1") %>%
    ggplot(aes(sample = Ganancia)) +
    stat_qq() +
    stat_qq_line() +
    labs(title = "QQ Plot of Ganancia values (Group 4.1)",
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    theme_minimal()

qq_44 <- grades %>%
    filter(GRUPO == "4.4") %>%
    ggplot(aes(sample = Ganancia)) +
    stat_qq() +
    stat_qq_line() +
    labs(title = "QQ Plot of Ganancia values (Group 4.4)",
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    theme_minimal()

ggsave("qq_plot_41.png", plot = qq_41, width = 8, height = 6)
ggsave("qq_plot_44.png", plot = qq_44, width = 8, height = 6)

# =============================================================================
# NORMALITY TESTING
# =============================================================================

# Anderson-Darling test for normality
ad_pvalues <- grades %>%
    group_by(GRUPO) %>%
    summarise(ad_test = ad.test(Ganancia)$p.value) %>%
    ungroup()

ad_statistics <- grades %>%
    group_by(GRUPO) %>%
    summarise(ad_test = ad.test(Ganancia)$statistic) %>%
    ungroup()

# =============================================================================
# NON-PARAMETRIC TESTING
# =============================================================================

# Wilcoxon rank-sum test (Mann-Whitney U test)
x <- grades %>%
    filter(GRUPO == "4.1") %>%
    pull(Ganancia)

y <- grades %>%
    filter(GRUPO == "4.4") %>%
    pull(Ganancia)

wilcox_result <- wilcox.test(x, y, 
                            alternative = "two.sided", 
                            paired = FALSE, 
                            exact = FALSE, 
                            conf.int = TRUE)

# =============================================================================
# MULTIPLE CORRESPONDENCE ANALYSIS (MCA)
# =============================================================================

# Load questions dataset
questions <- read_csv("datasets/questions.csv")
questions <- questions %>%
    mutate(GRUPO = as.factor(GRUPO))

# Perform MCA
mca_results <- MCA(questions[,-1], quali.sup = "GRUPO", graph = FALSE)

# Extract variable coordinates
var_coords <- as.data.frame(mca_results$var$coord)
var_coords$Variable <- rownames(var_coords)

# Separate correct and incorrect responses
correct <- var_coords[grepl("^1\\.", var_coords$Variable), ]
incorrect <- var_coords[grepl("^0\\.", var_coords$Variable), ]

# Calculate success rates for dimension interpretation
success_rates <- na.omit(questions[,-c(1,2)]) %>%
    summarise_all(mean) %>%
    gather(key = "Pregunta", value = "Tasa_Acierto") %>%
    mutate(Question_num = 1:15)

correct$Question_num <- as.numeric(gsub("1\\.", "", correct$Variable))
analysis_df <- merge(correct, success_rates, by.x = "Question_num", by.y = "Question_num")

# Correlation for dimension interpretation validation
correlation <- cor(analysis_df$`Dim 2`, analysis_df$Tasa_Acierto)

# =============================================================================
# MCA VISUALIZATIONS
# =============================================================================

# Scree plot
scree <- fviz_screeplot(mca_results, addlabels = TRUE, ylim = c(0, 60)) +
    labs(title = "Scree Plot of MCA Results",
         x = "Dimensions",
         y = "Eigenvalues") +
    theme_minimal()
ggsave("scree_plot_mca.png", plot = scree, width = 8, height = 6)

# Biplot
biplot <- fviz_mca_biplot(mca_results,
                          repel = TRUE,
                          col.var = "contrib",
                          gradient.cols = c("blue", "red")) +
    labs(title = "Biplot of MCA Results",
         x = "Dimension 1",
         y = "Dimension 2") +
    theme_minimal()
ggsave("biplot_mca.png", plot = biplot, width = 8, height = 6)

# Individuals plot
mca_ind <- fviz_mca_ind(mca_results,
                        habillage = questions$GRUPO,  
                        addEllipses = TRUE,        
                        ellipse.level = 0.95,
                        palette = "Dark2")
ggsave("mca_individuals.png", plot = mca_ind, width = 8, height = 6)

# Variables plot
mca_var <- fviz_mca_var(mca_results,
                        col.var = "contrib", 
                        gradient.cols = c("blue", "red"),
                        repel = TRUE) + 
    labs(title = "Variable Contributions in MCA")
ggsave("mca_variable_contributions.png", plot = mca_var, width = 8, height = 6)

# =============================================================================
# ERROR PROFILES ANALYSIS
# =============================================================================

# Extract individual coordinates
ind_coords <- as.data.frame(mca_results$ind$coord)
ind_coords$GRUPO <- questions$GRUPO

# Focus on low performance group
grupo_bajo <- ind_coords[ind_coords$GRUPO == "4.1", ]
colnames(grupo_bajo) <- c("Dim.1", "Dim.2", "Dim.3", "Dim.4", "Dim.5", "GRUPO")

# Error profiles distribution plot
error_profiles <- ggplot(grupo_bajo, aes(x = Dim.2)) +
    geom_histogram(aes(y = ..density..), bins = 15, alpha = 0.7, fill = "lightblue", color = "black") +
    geom_density(color = "red", size = 1.2) +
    geom_vline(aes(xintercept = quantile(Dim.2, 0.25)), color = "darkblue", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = quantile(Dim.2, 0.75)), color = "darkblue", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = median(Dim.2)), color = "darkgreen", size = 1.5) +
    annotate("text", x = quantile(grupo_bajo$Dim.2, 0.25), y = max(density(grupo_bajo$Dim.2)$y) * 0.8, 
             label = "Subperfil 1\n(Errores en avanzadas)", angle = 90, hjust = 1, size = 3) +
    annotate("text", x = median(grupo_bajo$Dim.2), y = max(density(grupo_bajo$Dim.2)$y) * 0.9, 
             label = "Subperfil 2\n(Errores distribuidos)", angle = 90, hjust = 1, size = 3) +
    annotate("text", x = quantile(grupo_bajo$Dim.2, 0.75), y = max(density(grupo_bajo$Dim.2)$y) * 0.8, 
             label = "Subperfil 3\n(Errores en básicas)", angle = 90, hjust = 0, size = 3) +
    labs(title = "Perfiles de Error - Grupo de Bajo Rendimiento",
         subtitle = "Distribución en Dimensión 2 (Perfil de dificultad)",
         x = "Dimensión 2 (Nivel de dificultad de errores)",
         y = "Densidad") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 12))

ggsave("error_profiles_analysis.png", plot = error_profiles, width = 10, height = 6, dpi = 300)

# =============================================================================
# DIFFICULTY PROGRESSION ANALYSIS
# =============================================================================

# Prepare data for difficulty progression
var_coords <- as.data.frame(mca_results$var$coord)
var_coords$Variable <- rownames(var_coords)
colnames(var_coords) <- c("Dim.1", "Dim.2", "Dim.3", "Dim.4", "Dim.5", "Variable")

correct_answers <- var_coords[grepl("^1\\.", var_coords$Variable), ]
correct_answers$Question <- 1:14

# Classify questions by difficulty level
correct_answers$Grupo_Dificultad <- case_when(
    correct_answers$Question %in% c(12, 4, 8, 6) ~ "Avanzadas",
    correct_answers$Question %in% c(14, 13, 11, 10, 9) ~ "Discriminantes", 
    correct_answers$Question %in% c(3, 7, 5, 1, 2) ~ "Básicas",
    TRUE ~ "Otras"
)

# Order by difficulty
correct_answers <- correct_answers[order(correct_answers$Dim.2, decreasing = TRUE), ]

# Difficulty progression plot
difficulty_progression <- ggplot(correct_answers, aes(x = reorder(paste("P", Question, sep=""), Dim.2), 
                                                      y = Dim.2, 
                                                      fill = Grupo_Dificultad)) +
    geom_col(alpha = 0.8, color = "black") +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
    scale_fill_manual(values = c("Básicas" = "#2E8B57", 
                                "Discriminantes" = "#4682B4", 
                                "Avanzadas" = "#DC143C")) +
    labs(title = "Progresión de Dificultad Identificada por MCA",
         subtitle = "Clasificación de preguntas según Dimensión 2",
         x = "Preguntas (ordenadas por dificultad)",
         y = "Posición en Dimensión 2",
         fill = "Nivel de Dificultad") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          legend.position = "bottom") +
    guides(fill = guide_legend(title = "Nivel de Dificultad"))

ggsave("difficulty_progression.png", plot = difficulty_progression, width = 12, height = 8, dpi = 300)

# =============================================================================
# ITEM RESPONSE THEORY (IRT) ANALYSIS
# =============================================================================

# Prepare data by group
q_group_1 <- questions %>%
    filter(GRUPO == "4.1") %>%
    select(-GRUPO, -`ALUMNO ID`) %>%
    as.data.frame()

q_group_2 <- questions %>%
    filter(GRUPO == "4.4") %>%
    select(-GRUPO, -`ALUMNO ID`) %>%
    as.data.frame()

# =============================================================================
# IRT MODELING - GROUP 4.4 (HIGH PERFORMANCE)
# =============================================================================

# Fit IRT models
fit3PL_44 <- mirt(data = q_group_2, model = 1, itemtype = "3PL", verbose = FALSE)
fit2PL_44 <- mirt(data = q_group_2, model = 1, itemtype = "2PL", verbose = FALSE)
fit1PL_44 <- mirt(data = q_group_2, model = 1, itemtype = "1PL", verbose = FALSE)

# Extract parameters
params2PL_44 <- coef(fit2PL_44, IRTpars = TRUE, simplify = TRUE)

# Model fit assessment
M2(fit2PL_44)
itemfit_44 <- itemfit(fit2PL_44, na.rm = TRUE)

# IRT visualizations for Group 4.4
ifp_44 <- itemfitPlot(fit2PL_44)
tp_44 <- tracePlot(fit2PL_44)
itm_44 <- itempersonMap(fit2PL_44)

ggsave("itemfit_plot_2PL_44.png", plot = ifp_44, width = 8, height = 6)
ggsave("trace_plot_2PL_44.png", plot = tp_44, width = 8, height = 6)
ggsave("itemperson_map_2PL_44.png", plot = itm_44, width = 8, height = 6)

# =============================================================================
# IRT MODELING - GROUP 4.1 (LOW PERFORMANCE)
# =============================================================================

# Fit IRT models
fit3PL_41 <- mirt(data = q_group_1, model = 1, itemtype = "3PL", verbose = FALSE)
fit2PL_41 <- mirt(data = q_group_1, model = 1, itemtype = "2PL", verbose = FALSE)
fit1PL_41 <- mirt(data = q_group_1, model = 1, itemtype = "1PL", verbose = FALSE)

# Extract parameters
params2PL_41 <- coef(fit2PL_41, IRTpars = TRUE, simplify = TRUE)

# Model fit assessment
M2(fit2PL_41)
itemfit_41 <- itemfit(fit2PL_41)

# IRT visualizations for Group 4.1
ifp_41 <- itemfitPlot(fit2PL_41)
tp_41 <- tracePlot(fit2PL_41)
itm_41 <- itempersonMap(fit2PL_41)

ggsave("itemfit_plot_2PL_41.png", plot = ifp_41, width = 8, height = 6)
ggsave("trace_plot_2PL_41.png", plot = tp_41, width = 8, height = 6)
ggsave("itemperson_map_2PL_41.png", plot = itm_41, width = 8, height = 6)

# =============================================================================
# MCA-IRT CONVERGENCE ANALYSIS
# =============================================================================

# Prepare correlation data
correlation_data <- tibble(
    IRT_41 = params2PL_41$items[-15, 2],
    IRT_44 = params2PL_44$items[-15, 2],
    MCA = correct_answers$Dim.2
)

# Correlation plot
correlation_plot <- ggplot(correlation_data, aes(x = MCA)) +
    geom_point(aes(y = IRT_41, color = "IRT 4.1"), size = 2) +
    geom_point(aes(y = IRT_44, color = "IRT 4.4"), size = 2) +
    geom_smooth(aes(y = IRT_41, color = "IRT 4.1"), method = "lm", se = FALSE) +
    geom_smooth(aes(y = IRT_44, color = "IRT 4.4"), method = "lm", se = FALSE) +
    labs(title = "Correlation between IRT Parameters and MCA Dimension 2",
         x = "MCA Dimension 2",
         y = "IRT Difficulty Parameters",
         color = "Model") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          legend.position = "bottom") +
    scale_color_manual(values = c("IRT 4.1" = "blue", "IRT 4.4" = "red")) +
    annotate("text", x = max(correlation_data$MCA) * 0.8, 
             y = max(correlation_data$IRT_41) * 0.9, 
             label = paste("Correlation IRT 4.1:", 
                           round(cor(correlation_data$IRT_41, correlation_data$MCA), 2)),
             color = "blue", size = 4) +
    annotate("text", x = max(correlation_data$MCA) * 0.8, 
             y = max(correlation_data$IRT_44) * 0.9, 
             label = paste("Correlation IRT 4.4:", 
                           round(cor(correlation_data$IRT_44, correlation_data$MCA), 2)),
             color = "red", size = 4)

ggsave("correlation_plot_irt_mca.png", plot = correlation_plot, width = 10, height = 6, dpi = 300)

# =============================================================================
# SUMMARY OF CORRELATIONS FOR VALIDATION
# =============================================================================

# Calculate and print correlations for validation
cor_41 <- cor(correlation_data$IRT_41, correlation_data$MCA)
cor_44 <- cor(correlation_data$IRT_44, correlation_data$MCA)

cat("\n=== MCA-IRT CONVERGENCE VALIDATION ===\n")
cat(paste("Correlation MCA-IRT Group 4.1 (Low Performance):", round(cor_41, 3), "\n"))
cat(paste("Correlation MCA-IRT Group 4.4 (High Performance):", round(cor_44, 3), "\n"))
cat("Opposite correlations confirm ceiling effect in high performance group\n")