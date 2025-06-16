library(tidyverse)

# Charger les données
data <- read_csv("data/ObesityDataSet_raw_and_data_sinthetic.csv")

# Ajouter la colonne IMC
data <- data %>%
  mutate(BMI = Weight / (Height^2))

# Supprimer les valeurs aberrantes (règle IQR pour BMI, Age)
clean_data <- data %>%
  filter(
    between(BMI, quantile(BMI, 0.01), quantile(BMI, 0.99)),
    between(Age, 14, 60),  # selon le résumé statistique
    between(Height, 1.45, 2),
    between(Weight, 40, 170)
  )

# Convertir les colonnes catégorielles
clean_data <- clean_data %>%
  mutate(across(
    c(Gender, family_history_with_overweight, FAVC, CAEC, SMOKE, SCC, CALC, MTRANS, NObeyesdad),
    as.factor
  ))

# Ajouter une colonne tranche d'âge
clean_data <- clean_data %>%
  mutate(AgeGroup = case_when(
    Age < 18 ~ "Adolescent",
    Age < 30 ~ "Jeune adulte",
    Age < 45 ~ "Adulte",
    TRUE     ~ "Senior"
  ))

# Vérifier les doublons
duplicates <- clean_data %>% filter(duplicated(.))
n_duplicates <- nrow(duplicates)
print(paste("Nombre de doublons :", n_duplicates))
