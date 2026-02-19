install.packages("readxl")
install.packages("naniar")
install.packages("tidyverse")
install.packages("DataExplorer")
install.packages("corrplot")
install.packages("moments")
install.packages("e1071") 

library(dplyr)
library(e1071)
library(readxl)
library(tidyverse)
library(naniar)
library(DataExplorer)
library(corrplot)
library(moments)
library(reshape2)
library(ggplot2)
library(car)



# Étape 1 : Analyse Exploratoire des Données


#1. Importation des données 
data <- read_excel("C:/Users/dell/Downloads/Finance.xlsx")
View(data)
str(data)
dim(data)
head(data)

#2. Statistiques descriptives générales
summary(data)


#3. Traitement des valeurs manquantes

is.na(data)
sum(is.na(data))
naniar::vis_miss(data)

#4. Détection des valeurs aberrantes

#Sélection des colonnes numériques
numeric_vars <- sapply(data, is.numeric)
data_num <- data[, numeric_vars]

#Méthode IQR
detect_outliers_IQR <- function(x) {
  Q1 <- quantile(x, 0.25 )
  Q3 <- quantile(x, 0.75 )
  IQR <- Q3 - Q1
  
  x[x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR)]
}

outliers_IQR <- lapply(data_num, detect_outliers_IQR)

outliers_IQR


#Méthode BOXPLOT
boxplot(data_num,
        las = 2,
        col = "lightblue",
        main = "Boxplot des variables numériques")

outliers_boxplot <- lapply(data_num, function(x) {
  boxplot.stats(x)$out
})

outliers_boxplot

#Compter le nombre de valeurs abberantes 
sapply(outliers_IQR, length)

#5. Visualisations exploratoires

# Matrice de corrélation (variables quantitatives)

cor_mat <- cor(data_num, use = "complete.obs")
corrplot(
  cor_mat,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.cex = 0.8,
  number.cex = 0.7
)
#Histogrammes

plot_histogram(data)

#Densités

plot_density(data)

#Distribution générale des variables
data_bar <- data %>% 
  select(-Date)


plot_bar(data_bar)

#Création d'un rapport detaill1é sur les données (optionnel)
create_report(data)


#Étape 2 : Analyse Unidimensionnelle
#Pour les variables quantitatives

#Variables quantitatives

# 1. Mesures de tendance centrale

# Moyenne, médiane, mode

mode_stat <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


#Unités vendues
mean(data$`Unités vendues`)
median(data$`Unités vendues`)
mode_stat(data$`Unités vendues`)

#Ventes brutes
mean(data$`Ventes brutes`)
median(data$`Ventes brutes`)
mode_stat(data$`Ventes brutes`)

#Remise
mean(data$Remise)
median(data$Remise)
mode_stat(data$Remise)

#Ventes nettes
mean(data$`Ventes nettes`)
median(data$`Ventes nettes`)
mode_stat(data$`Ventes nettes`)

#Coûts des ventes 
mean(data$`Coûts des ventes`)    #moyenne
median(data$`Coûts des ventes`)   #medianne
mode_stat(data$`Coûts des ventes`)              #mode

#Profit
mean(data$Profit)
median(data$Profit)
mode_stat(data$Profit)

#2. Mesures de dispersion

#Écart-type, variance, étendue

#Unités vendues 
sd(data$`Unités vendues`)        # écart-type
var(data$`Unités vendues`)       # variance
diff(range(data$`Unités vendues`))     # étendue

#Ventes nettes 
sd(data$`Ventes nettes` )        
var(data$`Ventes nettes` )       
diff(range(data$`Ventes nettes` ))

#Profit
sd(data$Profit)      
var(data$Profit)       
diff(range(data$Profit) )    

#Coûts des ventes 
sd(data$`Coûts des ventes` )        
var(data$`Coûts des ventes` )       
diff(range(data$`Coûts des ventes` ))

#Ventes brutes 
sd(data$`Ventes brutes`)        
var(data$`Unités vendues`)       
diff(range(data$`Ventes brutes`))

#Remise 
sd(data$Remise )        
var(data$Remise )       
diff(range(data$Remise ))



# Coefficient de variation

sd(data$`Unités vendues` ) / mean(data$`Unités vendues`)               #Unités vendues
sd(data$`Ventes nettes` ) / mean(data$`Ventes nettes`)                 #Ventes nettes
sd(data$`Coûts des ventes` ) / mean(data$`Coûts des ventes`)           #Coûts des ventes
sd(data$Profit ) / mean(data$Profit )                                  #Profit
sd(data$`Ventes brutes` ) / mean(data$`Ventes brutes`)                 #Ventes brutes
sd(data$Remise ) / mean(data$Remise )                                  #Remise


# Quantiles (Q1, Q2, Q3)

quantile(data$Profit, probs = c(0.25, 0.5, 0.75))
quantile(data$`Unités vendues`, probs = c(0.25, 0.5, 0.75) )
quantile(data$`Ventes brutes`, probs = c(0.25, 0.5, 0.75) )
quantile(data$Remise, probs = c(0.25, 0.5, 0.75) )
quantile(data$`Ventes nettes`, probs = c(0.25, 0.5, 0.75) )
quantile(data$`Coûts des ventes`, probs = c(0.25, 0.5, 0.75))


#3. Forme de la distribution
#Asymétrie (skewness)
skewness(data$`Unités vendues`)
skewness(data$`Ventes nettes`)
skewness(data$`Coûts des ventes` )
skewness(data$Profit)
skewness(data$`Ventes brutes` )
skewness(data$Remise )


#Aplatissement (kurtosis)
kurtosis(data$`Unités vendues`)
kurtosis(data$`Ventes brutes`)
kurtosis(data$Remise)
kurtosis(data$`Ventes nettes`)
kurtosis(data$`Coûts des ventes`)
kurtosis(data$Profit)


#Test de normalité (Shapiro-Wilk)
shapiro.test(sample(data$`Unités vendues`, 300))
shapiro.test(sample(data$`Ventes brutes`, 500))
shapiro.test(sample(data$Remise, 500))
shapiro.test(sample(data$`Ventes nettes`, 300))
shapiro.test(sample(data$`Coûts des ventes`, 300))
shapiro.test(sample(data$Profit, 300))


#4. Visualisations
#Histogramme avec courbe de densité

#Unités vendues
ggplot(data, aes(x = `Unités vendues`)) +
  geom_histogram(aes(y = ..density..),
                 bins = 30,
                 fill = "lightblue",
                 color = "black") +
  geom_density() +
  labs(
    title = "Histogramme et densité des Unités vendues",
    x = "Unités vendues",
    y = "Densité"
  ) +
  theme_minimal()
#Boxplot


ggplot(data, aes(y = `Unités vendues`)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Boxplot des Unités vendues ",
    y = "Unités vendues"
  ) +
  theme_minimal()

#QQ-plot

ggplot(data, aes(sample = `Unités vendues`)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "QQ-plot des Unités vendues"
  ) +
  theme_minimal()



#Ventes brutes
#Histogramme avec courbe de densité
ggplot(data, aes(x = `Ventes brutes`)) +
  geom_histogram(aes(y = ..density..),
                 bins = 30,
                 fill = "lightblue",
                 color = "black") +
  geom_density() +
  labs(
    title = "Histogramme et densité des Ventes brutes",
    x = "Ventes brutes",
    y = "Densité"
  ) +
  theme_minimal()

#Boxplot

ggplot(data, aes(y = `Ventes brutes`)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Boxplot des Ventes brutes",
    y = "Ventes brutes"
  ) +
  theme_minimal()


#QQ-plot

ggplot(data, aes(sample = `Ventes brutes`)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "QQ-plot des Ventes brutes"
  ) +
  theme_minimal()




#Remise
#Histogramme avec courbe de densité
ggplot(data, aes(x = Remise)) +
  geom_histogram(aes(y = ..density..),
                 bins = 30,
                 fill = "lightblue",
                 color = "black") +
  geom_density() +
  labs(
    title = "Histogramme et densité de la Remise",
    x = "Remise",
    y = "Densité"
  ) +
  theme_minimal()
#Boxplot


ggplot(data, aes(y = Remise)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Boxplot de la Remise",
    y = "Remise"
  ) +
  theme_minimal()

#QQ-plot
ggplot(data, aes(sample = Remise)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "QQ-plot de la Remise"
  ) +
  theme_minimal()

#Ventes nettes 
#Histogramme avec courbe de densité
ggplot(data, aes(x = `Ventes nettes`)) +
  geom_histogram(aes(y = ..density..),
                 bins = 30,
                 fill = "lightblue",
                 color = "black") +
  geom_density() +
  labs(
    title = "Histogramme et densité des Ventes nettes ",
    x = "Ventes nettes",
    y = "Densité"
  ) +
  theme_minimal()
#Boxplot


ggplot(data, aes(y = `Ventes nettes`)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Boxplot des Ventes nettes",
    y = "Ventes nettes"
  ) +
  theme_minimal()


#QQ-plot

ggplot(data, aes(sample = `Ventes nettes`)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "QQ-plot des Ventes nettes"
  ) +
  theme_minimal()


#Coûts des ventes

#Histogramme avec courbe de densité
ggplot(data, aes(x = `Coûts des ventes`)) +
geom_histogram(aes(y = ..density..),
               bins = 30,
               fill = "lightblue",
               color = "black") +
geom_density() +
labs(
  title = "Histogramme et densité des Coûts des ventes",
  x = "Coûts des ventes",
  y = "Densité"
) +
theme_minimal()
#Boxplot


ggplot(data, aes(y = `Coûts des ventes`)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Boxplot des Coûts des ventes",
    y = "Coûts des ventes"
  ) +
  theme_minimal()

#QQ-plot

ggplot(data, aes(sample = `Coûts des ventes`)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "QQ-plot des Coûts des ventes"
  ) +
  theme_minimal()


#Profit
#Histogramme avec courbe de densité
ggplot(data, aes(x = Profit)) +
  geom_histogram(aes(y = ..density..),
                 bins = 30,
                 fill = "lightblue",
                 color = "black") +
  geom_density() +
  labs(
    title = "Histogramme et densité du profit",
    x = "Profit",
    y = "Densité"
  ) +
  theme_minimal()
#Boxplot


ggplot(data, aes(y = Profit)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "Boxplot du profit",
    y = "Profit"
  ) +
  theme_minimal()

#QQ-plot

ggplot(data, aes(sample = Profit)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "QQ-plot du profit"
  ) +
  theme_minimal()


#Pour les variables quanlitatives
#1. Tableaux de fréquences
#Fréquences absolues

table(data$`Segment de vente`)
table(data$Pays)
table(data$Produit)
table(data$`Fourchette de remise`)

#Fréquences relatives (pourcentages
prop.table(table(data$`Segment de vente`)) * 100
prop.table(table(data$Pays)) * 100
prop.table(table(data$Produit)) * 100
prop.table(table(data$`Fourchette de remise`)) * 100

#2. Visualisations
#Diagramme en barres
#Segment de vente 

ggplot(data, aes(x = `Segment de vente`)) +
  geom_bar() +
  labs(
    title = "Répartition des observations par segment",
    x = "Segment",
    y = "Effectif"
  ) +
  theme_minimal()

#Pays

ggplot(data, aes(x = Pays)) +
  geom_bar() +
  labs(
    title = "Répartition des observations par pays",
    x = "Pays",
    y = "Effectif"
  ) +
  theme_minimal()

#Produit
ggplot(data, aes(x = Produit)) +
  geom_bar() +
  labs(
    title = "Répartition des observations par produit",
    x = "Produit",
    y = "Effectif"
  ) +
  theme_minimal()

#Fourchette de remise 

ggplot(data, aes(x = `Fourchette de remise`)) +
  geom_bar() +
  labs(
    title = "Répartition des observations par Fourchette de remise",
    x = "Fourchette de remise",
    y = "Effectif"
  ) +
  theme_minimal()

#Diagramme circulaire

#Segment de vente
data %>%
  count(`Segment de vente`) %>%
  ggplot(aes(x = "", y = n, fill = `Segment de vente`)) +
  geom_col() +
  coord_polar(theta = "y") +
  labs(title = "Répartition par segment") +
  theme_void()

#Pays
data %>%
  count(Pays) %>%
  ggplot(aes(x = "", y = n, fill = Pays)) +
  geom_col() +
  coord_polar(theta = "y") +
  labs(title = "Répartition par pays") +
  theme_void()

#Produit
data %>%
  count(Produit) %>%
  ggplot(aes(x = "", y = n, fill = Produit)) +
  geom_col() +
  coord_polar(theta = "y") +
  labs(title = "Répartition par produit") +
  theme_void()

#Fourchette de remise
data %>%
  count(`Fourchette de remise`) %>%
  ggplot(aes(x = "", y = n, fill = `Fourchette de remise`)) +
  geom_col() +
  coord_polar(theta = "y") +
  labs(title = "Répartition par fourchette de remise") +
  theme_void()

# Étape 3 : Analyse Bidimensionnelle
#Quantitative vs Quantitative

#1. Coefficient de corrélation

#Spearman (car données non-normales)
cor.test(
  data$Profit,
  data$`Coûts des ventes`,
  method = "spearman",
  exact = FALSE
)

#2. Visualisation
#Nuage de points avec droite de régression

ggplot(data, aes(x = `Coûts des ventes`, y = Profit)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Relation entre Coûts des ventes et Profit",
    x = "Coûts des ventes",
    y = "Profit"
  ) +
  theme_minimal()

#Quantitative vs Qualitative
#1. Statistiques par groupe
#Moyennes, médianes, écarts-types par modalité

data %>%
  group_by(`Segment de vente`) %>%
  summarise(
    moyenne = mean(`Ventes nettes`, na.rm = TRUE),
    mediane = median(`Ventes nettes`, na.rm = TRUE),
    ecart_type = sd(`Ventes nettes`, na.rm = TRUE),
    n = n()
  )



# Test de Bartlett (variance homogène)
bartlett.test(`Ventes nettes` ~ `Segment de vente`, data = data)

#Test de Kruskal-Wallis
kruskal.test(`Ventes nettes` ~ `Segment de vente`, data = data)


ggplot(data, aes(x = `Segment de vente`, y = `Ventes nettes`, fill = `Segment de vente`)) +
  geom_boxplot() +
  labs(
    title = "Comparaison des ventes nettes par segment",
    x = "Segment de vente",
    y = "Ventes nettes"
  ) +
  theme_minimal()


                            #Qualitative vs Qualitative
                            #1. Tableau de contingence

#Effectifs croisés
table(data$`Fourchette de remise`, data$Produit)

#Proportions (lignes, colonnes, totales)

# Proportion par ligne
prop.table(table(data$`Fourchette de remise`, data$Produit), 1) * 100

# Proportion par colonne
prop.table(table(data$`Fourchette de remise`, data$Produit), 2) * 100

# Proportion totale
prop.table(table(data$`Fourchette de remise`, data$Produit)) * 100

#2. Test d’indépendance
#Test du Chi-2
chi_result <- chisq.test(table(data$`Fourchette de remise`, data$Produit))
chi_result

#Résidus de Pearson
chi_result$residuals


#3. Visualisations
#Diagramme en barres groupées

ggplot(data, aes(x = Produit, fill = `Fourchette de remise`)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Répartition des fourchettes de remise par produit",
    x = "Produit",
    y = "Effectif"
  ) +
  theme_minimal()


#Heatmap du tableau de contingence
tab <- table(data$`Fourchette de remise`, data$Produit)
tab_melt <- melt(tab)

ggplot(tab_melt, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), color = "white") +
  labs(
    title = "Heatmap des effectifs : remise vs produit",
    x = "Produit",
    y = "Fourchette de remise"
  ) +
  theme_minimal()

                            #Étape 4 : Modèle de Régression Linéaire Multiple
                            #Construction du modèle

#1. Choix des variables
#

#Vérifier que les variables qualitatives sont bien en facteur
str(data[, c("Unités vendues", "Produit", "Segment de vente", "Année")])
data$Produit <- as.factor(data$Produit)
data$`Segment de vente` <- as.factor(data$`Segment de vente`)

#2. Estimation du modèle
#Fonction lm() en R
modele <- lm(`Unités vendues` ~ Produit + `Segment de vente` + Année, data = data)

# 3. Résumé du modèle : coefficients, t-tests, p-values
summary(modele)


newdata <- expand.grid(
  Produit = levels(data$Produit),
  `Segment de vente` = levels(data$`Segment de vente`),
  Année = 2025
)
head(newdata)
nrow(newdata)

pred_2025 <- predict(
  modele,
  newdata = newdata,
  interval = "prediction",
  level = 0.95
)
predictions_2025 <- cbind(newdata, pred_2025)
print(predictions_2025)

print(newdata)


summary(modele)



#Intervalles de confiance
confint(modele)

#R² et R² ajusté
summary(modele)$r.squared
summary(modele)$adj.r.squared

#RMSE et MAE
pred <- predict(modele)
RMSE <- sqrt(mean((data$`Unités vendues` - pred)^2))
MAE <- mean(abs(data$`Unités vendues` - pred))

print (pred)
print(RMSE)
print(MAE)
#Validation des hypothèses
#1. Linéarité
plot(modele, which = 1)


#2. Normalité des résidus
qqnorm(residuals(modele))
qqline(residuals(modele))
shapiro.test(residuals(modele))


#3. Homoscédasticité
plot(modele, which = 3)
lmtest::bptest(modele)



#4. Multicolinéarité
car::vif(modele)



#5. Points influents
plot(modele, which = 4)






