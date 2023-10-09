# ------------------------------------------------------------------------------
#     Giuseppe Gruttad'Auria
#     Institut du management / Institut du management de l'information
#     Université de Neuchâtel
# ------------------------------------------------------------------------------

ddpath <- "C:/Users/giuse/Desktop/Econometrics/Material/Données-20230621/"

setwd(ddpath)

# ===========================================================================
#                               SERIE EMETRIE NO 2
# ---------------------------------------------------------------------------
# Exercice 2.1
# --------------------------------------------------------------------------

# librairie

library(MASS)
# contient diverses fonctions et ensembles de données pour l'analyse statistique
# et les méthodes d'apprentissage automatique
library(ppcor)
# utilisé pour les calculs de corrélation partielle

# Données

data <- data.frame(Grade = c(80, 50, 80, 90, 92, 85, 76, 81, 90, 95),
                 hours = c(5, 4, 6, 6, 3, 5, 9, 9, 5, 6),
                 Score = c(90, 80, 70, 80, 95, 91, 99, 88, 92, 91))

# Un cadre de données est une structure de données fondamentale en R, similaire 
# à un tableau dans une base de données ou un tableur.
# Le cadre de données (data) comporte trois colonnes : "Grade", "hours" et "Score"
# chacune avec dix valeurs.
# Les valeurs de chaque colonne sont fournies dans la fonction c(),
# qui est utilisée pour créer des vecteurs pour chaque variable.


# Régressions

summary(lm.mod1 <- lm(Grade ~ hours, data=data))
summary(lm.mod2 <- lm(Grade ~ Score, data=data))
summary(lm.mod3 <- lm(Score ~ Grade, data=data))

summary(lm.mod4 <- lm(Grade ~ hours + Score, data=data))
summary(lm.mod5 <- lm(Score ~ hours + Grade, data=data))

summary(lm.mod6 <- lm(Score ~ hours, data=data))
summary(lm.mod7 <- lm(hours ~ Score, data=data))
summary(lm.mod8 <- lm(hours ~ Grade, data=data))

# Ces lignes de code effectuent plusieurs analyses de régression linéaire en 
# utilisant la fonction lm() qui permet de créer des modèles de régression 
# linéaire.
# Chaque ligne de commande effectue une régression différente en utilisant 
# différentes combinaisons de variables dépendantes et indépendantes.

# Corrélations partielles

(r.Grade.Score <-cor(residuals(lm.mod1), residuals(lm.mod6)))
(r.Grade.hours <-cor(residuals(lm.mod2), residuals(lm.mod7)))
(r.Score.hours <-cor(residuals(lm.mod3), residuals(lm.mod8)))

# Pour chaque paire de modèles de régression (par exemple, lm.mod1 et lm.mod6),
# les résidus sont calculés à l'aide de la fonction residuals().
# Après avoir obtenu les résidus des modèles spécifiques, la fonction cor()
# est utilisée pour calculer la corrélation entre ces résidus.
# Cela revient à calculer la corrélation entre les erreurs de prédiction des
# deux modèles tout en tenant compte des autres variables.

# Exemple: La corrélation partielle entre les résidus du modèle lm.mod1
# (Grade ~ hours) et les résidus du modèle lm.mod6 (Score ~ hours).
# Cette corrélation mesure la relation résiduelle entre "Grade" et "Score"
# après avoir tenu compte des effets de la variable "hours"

# Relations avec le coefficient de détermination

(1 - summary(lm.mod4)$r.squared)

# summary(_)$r.squared extrait le coefficient de détermination R-squared ajusté

# (1 - summary... calcule le complément à 1 du R-squared ajusté,  
# ce qui représente la proportion de variabilité résiduelle dans la 
# variable dépendante "Grade" après ajustement.

(1 - summary(lm.mod1)$r.squared) * (1 - r.Grade.Score^2)

# r.Grade.Score est la corrélation partielle entre les résidus du modèle 
# lm.mod1 (Grade ~ hours) et les résidus du modèle lm.mod6 (Score ~ hours)

# (1- r.Grade.Score^2) calcule le carré du complément à 1 de la corrélation
# partielle. Cela donne une mesure de la variabilité résiduelle non
# expliquée par les modèles lm.mod1 et lm.mod6

# Répéter le processus pour d'autres paires de modèles

(1 - summary(lm.mod4)$r.squared)
(1 - summary(lm.mod2)$r.squared) * (1 - r.Grade.hours^2)

# et

(1 - summary(lm.mod5)$r.squared)
(1 - summary(lm.mod3)$r.squared) * (1 - r.Score.hours^2)

# Vérification avec la librairie ppcor

pcor(data)

# La fonction pcor() est utilisée pour calculer les corrélations partielles
# entre les variables d'un ensemble de données spécifique

# Les résultats affichés sont une matrice de corrélations partielles entre 
# toutes les paires de variables de l'ensemble de données data.
# Mesurent la relation entre deux variables en contrôlant les effets de
# toutes les autres variables de l'ensemble de données.
# ce qui peut être utile pour déterminer les associations spécifiques entre deux
# variables après avoir tenu compte des influences des autres.

# ---------------------------------------------------------------------------
# Exercice 2.2
# ---------------------------------------------------------------------------

# a) Données de base
# ------------------

# Lecture des données de base

wdf <- read.csv2(file(paste(ddpath, "WeeklyInterest.csv", sep=""), 
                      encoding="latin1"), 
                 header=T, sep=";", dec=".", na.strings="NA")

head(wdf)
nrow(wdf)
ncol(wdf)

table(wdf$year)
# table() est utilisée pour créer une table de fréquence à partir des valeurs 
# d'une variable spécifique

# b) Estimation par MCO (1er modèle)
# ----------------------------------

# Forme structurelle

f1 <- formula(diff(aaa)~diff(cm10)+diff(cm30))

# diff(aaa) ~ diff(cm10) + diff(cm30) est la formule de régression elle-même.
# diff(aaa) représente la variable dépendante
# diff(cm10) + diff(cm30) représente les variables indépendantes
# différences dans la variable aaa en fonction des différences dans les variables
# cm10 et cm30

# Estimation

lm.fit01 <- lm(f1, data = wdf, y = T, na.action = na.exclude)
summary(lm.fit01)

# Valeurs observées, prédites (estimées) et résidus

data.frame(lm.fit01$y, fitted(lm.fit01), residuals(lm.fit01))
# Le data frame contiendra trois colonnes correspondant aux trois éléments

# Estimation de la qualité d'ajustement

(R2 <- var(fitted(lm.fit01)) / var(lm.fit01$y))
# fitted() est une fonction qui renvoie les valeurs ajustées (prédites) 
# par le modèle de régression

# Cela permet d'examiner comment le modèle se comporte en termes de prédictions 
# et d'erreurs pour chaque observation dans l'ensemble de données.

# ou...

(R2 <- 1 - var(residuals(lm.fit01)) / var(lm.fit01$y))

# cette ligne de code calcule le coefficient de détermination R-squared 
# pour évaluer la qualité du modèle de régression dans l'ajustement des données

# c) Coefficient de corrélation partiel
# -------------------------------------

# Forme structurelle

f2 <- update(f1, .~. - diff(cm30))

# Estimation

lm.fit02 <- lm(f2, data = wdf, y = T, na.action = na.exclude)
summary(lm.fit02)

# Corrélation partielle

1 - (1 - summary(lm.fit01)$r.squared) / (1 - summary(lm.fit02)$r.squared)
# Ces calculs permettent d'évaluer comment différentes combinaisons de modèles de 
# régression expliquent ou laissent de la variabilité résiduelle dans les variables
# dépendantes après ajustement.

# ou on peut également utiliser cette méthode:
f3 <- update(f2, diff(cm30) ~.)

lm.fit.x <- lm(f3, data = wdf, na.action = na.exclude)
summary(lm.fit.x)

cor(residuals(lm.fit02), residuals(lm.fit.x))
cor(residuals(lm.fit02), residuals(lm.fit.x))^2 
# ces calculs permettent d'évaluer la similitude ou la dissimilarité entre 
# les erreurs de prédiction de deux modèles de régression et de quantifier 
# la proportion de variance commune entre ces erreurs.


# d) Changements d'origine et d'échelle
# -------------------------------------

# Constantes

c1 <- 10.
c2 <- 100.

# Formes structurelles

f4 <- formula(I(c1*diff(aaa))~I(c2*diff(cm10)))
f5 <- formula(I(c1+diff(aaa))~I(c2+diff(cm10)))
# Ces formules indiquent quelles variables sont utilisées comme variables
# dépendantes et indépendantes dans chaque modèle, avec l'ajout de constantes

# Estimations

lm.fit03 <- lm(f4, data = wdf, na.action = na.exclude)
summary(lm.fit03)

lm.fit04 <- lm(f5, data = wdf, na.action = na.exclude)
summary(lm.fit04)

# Variables centrées, réduites

f6 <- formula(scale(diff(aaa)) ~ scale(diff(cm10)))
# scale(diff()) représente la variable dépendante dans la formule. 
# scale() est utilisé pour centrer et réduire cette variable.
# Cela signifie que la variable dépendante diff() est transformée pour avoir
# une moyenne de 0 et un écart-type de 1.
lm.fit05 <- lm(f6, data = wdf, na.action = na.exclude)
summary(lm.fit05)