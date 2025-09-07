

#### CODIGO FINAL, ya limpio usando ambas, numéricas+ categóricas convertidas en dummies
library(dbscan)
library(caret)
library(uwot)
library(ggplot2)
library(dplyr)
library(ggforce)  # para geom_mark_ellipse

# 1. Cargar datos
data1 <- read.csv("C:/Users/reyna.pinto/Documents/Suzuky/Diplomado Ccia DATOS UNAM/Modulo 6_Ciencia de DATOS_Diplomado UNAM. No supervisada/Sleep_health_and_lifestyle.csv")

# pre-procesamiento de Blood Preassure:
# Separar la columna BloodPressure en antes y después del "/"
Blood_split <- strsplit(data1$Blood.Pressure, "/")

# Crear nuevas columnas
data1$BP_Systolic <- sapply(Blood_split, function(x) as.numeric(x[1]))
data1$BP_Diastolic <- sapply(Blood_split, function(x) as.numeric(x[2]))
data1 <- data1[, !names(data1) %in% "Blood.Pressure"] #eliminamos la col anterior

head(data1)

# 2. Separar variables categóricas y numéricas
cat_data <- data1[, c("Gender", "Occupation", "BMI.Category", "Sleep.Disorder")]
num_data <- data1[, sapply(data1, is.numeric)]

# 3. Convertir categóricas a dummies
dummies <- dummyVars(~ ., data = cat_data)
cat_data_dummies <- predict(dummies, newdata = cat_data)

# 4. Combinar numéricas y dummies
final_data <- cbind(num_data, cat_data_dummies)

# 5. Escalar
final_data_scaled <- scale(final_data)

# 6. OPTICS
set.seed(123)
optics_model <- optics(final_data_scaled, eps = 50, minPts = 5)

# 7. Asignar clusters usando corte
clusters <- extractDBSCAN(optics_model, eps_cl = 50)
cluster_factor <- factor(clusters$cluster)
data1$Cluster <- cluster_factor

# 8. UMAP en 2D
set.seed(123)
umap_coords <- umap(as.matrix(final_data_scaled),
                    n_neighbors = 30,
                    min_dist = 0.1,
                    n_components = 2)

umap_df <- as.data.frame(umap_coords)
colnames(umap_df) <- c("UMAP1", "UMAP2")
umap_df$Cluster <- cluster_factor
umap_df$Sleep.Disorder <- data1$Sleep.Disorder  # agregar variable categórica para color

# 9. Visualización UMAP
ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = Sleep.Disorder)) +
  geom_point(size = 2, alpha = 0.8) +
  # Dibujar elipses por cluster OPTICS
  geom_mark_ellipse(aes(fill = Cluster), alpha = 0.1, show.legend = FALSE) +
  theme_minimal() +
  labs(title = "Clusters OPTICS visualizados con UMAP y Sleep.Disorder",
       color = "Sleep.Disorder") +
  theme(plot.title = element_text(hjust = 0.5))