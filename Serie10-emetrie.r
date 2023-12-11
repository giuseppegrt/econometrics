# ------------------------------------------------------------------------------
#     Giuseppe Gruttad'Auria
#     Institut du management / Institut du management de l'information
#     Université de Neuchâtel
# ------------------------------------------------------------------------------

ddpath <- "C:/Users/giuse/Desktop/Econometrics/Material/Données-20230621/"

setwd(ddpath)

# Librairies
# ----------

library(car)    # vif, crPlots, avPlots

# ==============================================================================
#                               SERIE EMETRIE NO 10
# ------------------------------------------------------------------------------
# Exercice 10.1
# ------------------------------------------------------------------------------


# a) Données de base
# ------------------

# Lecture des données de base

wdf <- read.csv2(file(paste(ddpath, "Duncan.csv", sep=""), 
                      encoding="latin1"), 
                 header=T, sep=";", dec=".", na.strings="NA")

# b) Estimations par MCO
# ----------------------

# Forme structurelle

f1 <- formula(prestige ~ income + education)
f2 <- update(f1, .~. - income)
f3 <- formula(income ~ education)

# Estimations

lm.fit1 <- lm(f1, data = wdf, na.action = na.exclude)
summary(lm.fit1)

lm.fit2 <- lm(f2, data = wdf, na.action = na.exclude)
summary(lm.fit2)

lm.fit3 <- lm(f3, data = wdf, na.action = na.exclude)
summary(lm.fit3)

lm.fit4 <- lm(residuals(lm.fit2) ~ residuals(lm.fit3) -1.)
summary(lm.fit4)

# c) Graphe de régression partielle pour "income"
# -----------------------------------------------

# Un graphique de régression partielle montre la relation entre les résidus de
# deux variables après avoir éliminé les effets d'autres variables.

plot(residuals(lm.fit3), residuals(lm.fit2), 
# lm.fit2 : Ce modèle prédit le "prestige" en utilisant uniquement "education".
# lm.fit3 : Ce modèle prédit le "income" en utilisant uniquement "education".
     main = "Régression partielle pour income",
     xlab = "Income", ylab = "Prestige")
     # Axe des x (Income) : résidus du modèle lm.fit3
     # Axe des y (Prestige) : résidus du modèle lm.fit4
abline(lm.fit4)
# Ajoute une droite de régression au graphique, représentant la relation entre
# les deux ensembles de résidus. Il s'agit essentiellement de la pente estimée
# en régressant les résidus du "prestige" sur les résidus du "income"

# Un schéma ou une tendance dans le graphique, cela suggère qu'il pourrait y 
# avoir une relation entre les résidus du revenu et du prestige après avoir
# éliminé les effets de l'éducation.
# Les schémas dans le graphique peuvent suggérer une association, mais ils 
# n'établissent pas de causalité.

# ou...

avPlots(lm.fit1)
# Génère des graphiques de valeurs résiduelles partielles pour chaque prédicteur.
# Les valeurs résiduelles partielles représentent la variation non expliquée 
# d'une variable après avoir tenu compte des autres variables explicatives.

# Si le modèle est correct, les points sur le graphique devraient suivre une
# tendance aléatoire autour de zéro.
# Les observations 6, 9, 16 et 27 pourraient avoir une influence excessive sur
# l'estimation de βincome.
# Les observations 6, 9 et 27 pourraient avoir une influence excessive sur
# l'estimation de βeducation.

# Résidus

data.frame(residuals(lm.fit4), residuals(lm.fit1))
# Lorsque les résidus du Modèle 1 sont identiques à ceux du Modèle 4, cela 
# indique une relation spécifique entre les variables en jeu.
# Le Modèle 1 prédit le prestige en utilisant à la fois le revenu et l'éducation
# comme prédicteurs, tandis que le Modèle 4 analyse la relation entre les résidus
# de la prédiction du prestige sans le revenu par rapport aux résidus de la 
# prédiction du revenu en utilisant seulement l'éducation.

# L'information ou la variation dans le prestige que tente d'expliquer le revenu
# est capturée par l'éducation dans le Modèle 4.
# En somme, l'éducation seule explique la même variation dans le prestige que ce
# que l'éducation et le revenu ensemble expliquent dans le Modèle 1.

# La multicollinéarité parfaite survient lorsqu'il existe une relation linéaire 
# exacte entre les variables explicatives dans un modèle de régression.
# Cela signifie que l'une des variables indépendantes peut être parfaitement 
# prédite à partir d'une combinaison linéaire des autres variables.
# Cela peut rendre les estimations des coefficients du modèle peu fiables.
# En effet, lorsque la multicollinéarité parfaite est présente, il n'y a pas
# de solution unique pour les coefficients, et de petites variations dans les 
# données peuvent entraîner de grands changements dans les coefficients estimés.

# ------------------------------------------------------------------------------
# Exercice 10.2
# ------------------------------------------------------------------------------

# a) Données de base
# ------------------

# Lecture des données de base

wdf <- read.csv2(file(paste(ddpath, "prestige.csv", sep=""), 
                      encoding="latin1"), 
                 header=T, sep=";", dec=".", na.strings="NA")

# b) Estimations par MCO 
# ----------------------

# Formes structurelles

f1 <- formula(PrestigeScore ~ AvEducYear + AvIncome + pctWomen)

# Estimations 

lm.fit1 <- lm(f1, data = wdf, na.action = na.exclude, x = T)
summary(lm.fit1)

# c) Résidus partiels et graphe des résidus partiels
# --------------------------------------------------

# Résidus partiels

E2 <- residuals(lm.fit1) + coef(lm.fit1)[2] * lm.fit1$x[,2]
E3 <- residuals(lm.fit1) + coef(lm.fit1)[3] * lm.fit1$x[,3]
E4 <- residuals(lm.fit1) + coef(lm.fit1)[4] * lm.fit1$x[,4]

# coef(lm.fit1)[x] récupère le coefficient associé à la x-ième variable 
# indépendante (AvEducYear, AvIncome, pctWomen)
# lm.fit1$x[,x] sélectionne toutes les lignes de la x-ième colonne de la matrice
# de données, qui représente les valeurs de la x-ième variable indépendante

# Ces étapes calculent les résidus partiels en ajustant les résidus du modèle 
# de régression linéaire multiple en fonction de chaque variable indépendante 
# spécifique, capturant ainsi la partie linéaire de la relation entre les 
# résidus et chaque variable indépendante

# Graphe des résidus partiels

plot(lm.fit1$x[,2], E2)
scatter.smooth(lm.fit1$x[,2], E2,
               main = "Graphe des résidus partiels",
               xlab = "Education", ylab = "Résidus partiels")

# Graphique des résidus partiels pour la variable "Education", permettant de 
# visualiser la relation entre les années d'éducation moyenne par année et les
# résidus partiels du modèle de régression linéaire multiple

# Les résultats indiquent une relation linéaire entre les résidus partiels et
# la variable. Cela suggère que le modèle linéaire capture de manière appropriée
# la tendance générale des données par rapport à cette variable

plot(lm.fit1$x[,3], E3)
scatter.smooth(lm.fit1$x[,3], E3,
               main = "Graphe des résidus partiels",
               xlab = "Revenu", ylab = "Résidus partiels")

# Ce graphique des résidus partiels pour la variable "Revenu" permettra de
# visualiser comment les résidus partiels évoluent par rapport au revenu moyen.

# La relation entre les résidus partiels et la variable de revenu semble non
# linéaire, ce qui suggère des problèmes potentiels avec l'hypothèse de 
# linéarité du modèle pour cette variable. Ainsi, l'utilisation d'une 
# transformation logarithmique pourrait être envisagée pour améliorer 
# l'adéquation du modèle linéaire.

plot(lm.fit1$x[,4], E4)
scatter.smooth(lm.fit1$x[,4], E4,
               main = "Graphe des résidus partiels",
               xlab = "Pourcent Women", ylab = "Résidus partiels")

# Ce graphique des résidus partiels pour la variable "Pourcent Women" permettra
# de visualiser comment les résidus partiels évoluent en fonction du pourcentage
# de femmes dans les professions.

# Le graphique montre une dispersion des résidus autour d'une ligne horizontale.
# Cela pourrait indiquer que la variable "Pourcent Women" n'influence pas de 
# manière significative les résidus du modèle. En pratique, cela suggère que le
# pourcentage de femmes dans les professions étudiées n'a pas un impact important
# sur les erreurs de prédiction du modèle de régression. Cela peut signifier que
# cette variable n'apporte pas de valeur ajoutée à la capacité du modèle à 
# expliquer ou prédire le score de prestige des professions étudiées.

# ou...

crPlots(lm.fit1)
            
# ------------------------------------------------------------------------------
# Exercice 10.3
# ------------------------------------------------------------------------------

# a) Données de base
# ------------------

# Lecture des données de base

wdf <- read.csv2(file(paste(ddpath, "Ericksen.csv", sep=""), 
                      encoding="latin1"), 
                 header=T, sep=";", dec=".", na.strings="NA")

# b) Estimations par MCO 
# ----------------------

# Formes structurelles

f1 <- formula(Undercount ~ Minority + Crime + Poverty + Language + 
                           Highschool + Housing + City + Conventional)

f2 <- formula(Minority ~  Crime + Poverty + Language + 
                           Highschool + Housing + City + Conventional)

# Estimations 

lm.fit1 <- lm(f1, data = wdf, na.action = na.exclude, x = T)
summary(lm.fit1)

# c) Diagnostiques de multicollinéarité
# -------------------------------------

# Matrice design

X <- lm.fit1$x

# Permet d'extraire les données des variables indépendantes utilisées pour 
# ajuster le modèle, sous forme d'une matrice sans la colonne correspondant 
# à la variable dépendante

# Calcul du VIF pour "Minority"

lm.fit2 <- lm(f2, data = wdf, na.action = na.exclude, x = T)
summary(lm.fit2)

# lm.fit2 a une valeur plus élevée de R² ajusté (0.7763) par rapport au premier 
# modèle (0.6667), indiquant une meilleure adaptation des variables

R2j <- summary(lm.fit2)$r.squared
# Extrait le coefficient de détermination (R²) du modèle
VIF.minority <- 1/(1-R2j)
sqrt(VIF.minority)
# Le VIF mesure à quel point la variance du coefficient estimé pour 'Minority' 
# est augmentée en raison de la corrélation avec les autres variables 
# indépendantes du modèle.
# Un VIF de 2.238 pour la variable 'Minority' indique que la variance du 
# coefficient estimé pour 'Minority' est environ 2.24 fois plus grande en 
# raison de la multicollinéarité avec les autres variables du modèle.

# Cela peut compliquer l'interprétation précise de l'effet de 'Minority' sur 
# la variable dépendante ('Undercount') car une plus grande variance du 
# coefficient peut rendre les estimations moins stables et précises.

# Matrice de corrélations

(corX <- cor(X[,-1]))
# Cette matrice de corrélation montre les corrélations entre les variables.
# Les valeurs diagonales de la matrice sont toujours 1, car elles représentent
# la corrélation d'une variable avec elle-même, ce qui est logique 
# (la corrélation d'une variable avec elle-même est parfaite et vaut toujours 1).
# Les valeurs en dehors de la diagonale sont les corrélations entre paires de 
# variables. Une valeur proche de 1 (positif ou négatif) indique une forte 
# corrélation, tandis qu'une valeur proche de 0 indique une faible corrélation.

# 'Minority' a une corrélation de 0.65 avec 'Crime', 0.74 avec 'Poverty', 
# et 0.76 avec 'City'

# Inverse de la matrice de corrélations

solve(corX)
# La matrice inverse est utilisée pour calculer les facteurs d'inflation de la 
# variance (VIF). Les VIF sont obtenus en prenant le rapport entre la plus
# grande et la plus petite valeur propre de la matrice inverse.

# Valeurs et vecteurs propres de la matrice de corrélations

(ei <- eigen(corX)$values)
# Les valeurs propres (eigenvalues) sont des valeurs qui décrivent la variance
# expliquée par chaque composante principale ou chaque axe de variation dans
# un ensemble de données multidimensionnelles.

# Dans ce contexte spécifique, les valeurs propres fournissent des informations 
# sur l'étendue de la corrélation entre les variables.
# Si une ou plusieurs valeurs propres sont très proches de zéro, cela suggère
# une forte multicollinéarité

(nc <- ei[1] / ei[8])
# Cette expression calcule le rapport entre la première valeur propre et la 
# huitième valeur propre dans votre analyse des valeurs propres.
# Ce rapport (35.53364) suggère un niveau potentiellement élevé de 
# multicollinéarité entre les variables. Un rapport supérieur à 10 est souvent
# considéré comme indiquant des problèmes sérieux de multicollinéarité.

# Les valeurs propres sont ordonnées de manière décroissante en raison de 
# l'orthogonalité des composantes principales. Lorsque nous calculons les 
# valeurs propres à partir de la matrice de corrélation, elles représentent 
# la quantité de variance expliquée par chaque axe de variation des données.
# La première valeur propre est celle qui capture le maximum de variance,
# suivie par les suivantes qui expliquent progressivement moins de variance.

# ou...

(nc <- kappa(corX, exact=T))

# VIF (avec le package car)

vif(lm.fit1)
sqrt(vif(lm.fit1))
# Les valeurs de la racine carrée des VIF autour de 2 ou plus peuvent indiquer 
# une certaine multicollinéarité entre les variables correspondantes, ce qui 
# doit être pris en compte lors de l'interprétation des coefficients du modèle.
# Une variable avec un VIF élevé peut avoir son coefficient affecté par la 
# présence de variables corrélées. Par conséquent, l'effet précis d'une variable
# sur la variable dépendante peut être moins fiable.