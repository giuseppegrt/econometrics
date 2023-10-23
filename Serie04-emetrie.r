# ------------------------------------------------------------------------------
#     Giuseppe Gruttad'Auria
#     Institut du management / Institut du management de l'information
#     Université de Neuchâtel
# ------------------------------------------------------------------------------

ddpath <- "C:/Users/giuse/Desktop/Econometrics/Material/Données-20230621/"

setwd(ddpath)

# Librairies
# ----------

library(lmtest)       # waldtest
library(car)          # linearHypothesis

# ==============================================================================
#                               SERIE EMETRIE NO 4
# ------------------------------------------------------------------------------
# Exercice 4.1
# ------------------------------------------------------------------------------

# a) Données de base
# ------------------

# Lecture des données de base

wdf <- read.csv2(file(paste(ddpath, "london.csv", sep=""), 
                      encoding="latin1"), 
                 header=T, sep=";", dec=".", na.strings="NA")

head(wdf)
nrow(wdf)
ncol(wdf)

# b) Estimation par MCO (1er modèle)
# ----------------------------------

# Forme structurelle

f1 <- formula(wtrans ~ log(totexp) + age + nk)

# La variable dépendante wtrans représente la quantité d'argent que les ménages
# consacrent à leurs dépenses de transport

# log(totexp) correspond au logarithme naturel des dépenses totales des ménages
# age représente l'âge du chef de ménage
# nk correspond au nombre d'enfants dans le ménage

# Estimation

lm.fit1 <- lm(f1, data = wdf, na.action = na.exclude)
summary(lm.fit1)

# ------------------------------------------------------------------------------
# Exercice 4.2
# ------------------------------------------------------------------------------

# b) Estimations des modèles (2)-(4)
# ----------------------------------

# Formes structurelles

f4 <- update(f1, .~. - nk)
# - nk signifie que vous excluez la variable nk du modèle f1
f3 <- update(f4, .~. - age)
# - age signifie que vous excluez la variable age du modèle f4
f2 <- formula(wtrans ~ 1.) # Modèle nul
# Dans ce modèle, la variable dépendante est wtrans, et à droite de ~, 
# nous avons 1, ce qui signifie une constante. En d'autres termes,
# ce modèle n'a pas de variables explicatives,
# Cela permet de comparer comment un modèle sans variables explicatives se 
# comporte par rapport aux autres modèles plus complexes

# Estimations

lm.fit2 <- lm(f2, data = wdf, na.action = na.exclude)
lm.fit3 <- lm(f3, data = wdf, na.action = na.exclude)
lm.fit4 <- lm(f4, data = wdf, na.action = na.exclude)

summary(lm.fit2)
summary(lm.fit3)
summary(lm.fit4)

# c) Tests avec la librairie lmtest
# ---------------------------------

waldtest(lm.fit2, lm.fit1, test = "F") # ou .. waldtest(lm.fit1)
# Le test Wald est utilisé pour évaluer si les variables explicatives du modèle 2
# (modèle complet) ont une influence significative sur la variable dépendante 
# wtrans par rapport au modèle 1 (modèle nul).

# Le résultat indique une statistique F de 12.801 et une valeur p (Pr(>F)) 
# très faible de 2.913e-08 (notée '***'). Une valeur p très faible signifie
# que le modèle 2 est significativement meilleur que le modèle 1 en termes
# d'ajustement aux données.

waldtest(lm.fit3, lm.fit4, test = "F")
# Le test Wald est utilisé pour évaluer si l'ajout de la variable explicative
# age dans le modèle 2 améliore significativement l'ajustement du modèle par
# rapport au modèle 1

# Le résultat indique une statistique F de 0.0169 et une valeur p (Pr(>F))
# élevée de 0.8965. La valeur p élevée signifie que le modèle 2 n'est pas
# significativement meilleur que le modèle 1 en termes d'ajustement aux données.

waldtest(lm.fit3, lm.fit1, test = "F")
# Le test Wald est utilisé pour évaluer si l'ajout des variables explicatives
# age et nk dans le modèle 2 améliore significativement l'ajustement du modèle
# par rapport au modèle 1.

# Le résultat indique une statistique F de 2.7795 et une valeur p (Pr(>F))
# de 0.06239. La valeur p est légèrement inférieure à 0.05 (notée '.').
# Cela signifie que le modèle 2 est légèrement meilleur que le modèle 1 en termes
# d'ajustement aux données, mais la différence n'est pas statistiquement
# significative au seuil de 0.05.

# F-statistique élevée : Si la F-statistique est élevée et la valeur p associée
# (Pr(>F)) est faible (généralement inférieure à un niveau de signification
# choisi, comme 0,05), vous pouvez conclure qu'au moins l'un des prédicteurs
# a un effet statistiquement significatif sur la variable dépendante.
# En termes pratiques, cela signifie que l'ensemble des prédicteurs explique
# collectivement une part significative de la variance de la variable dépendante.

# F-statistique faible : Si la F-statistique est faible et que la valeur p est
# élevée, vous ne rejetez pas l'hypothèse nulle. En termes pratiques, 
# cela suggère que l'ensemble des prédicteurs n'a pas d'effet statistiquement
# significatif sur la variable dépendante, et le modèle peut ne pas convenir
# aux données.

# d) Test avec la librairie car
# -----------------------------

R <- matrix(c(0, 0, 1, 0,
              0, 0, 0, 1),
              2, 4, byrow = T)
              
r <- c(0, 0)
# utilisé pour spécifier les valeurs contre lesquelles vous souhaitez tester 
# les coefficients dans le modèle

linearHypothesis(lm.fit1, hypothesis.matrix = R, rhs = r, test = "F")
# Res.Df : Les degrés de liberté associés aux modèles 1 et 2.
# RSS : La somme des carrés des résidus pour les deux modèles, ce qui quantifie
# la variation non expliquée dans les données.
# Df : Le changement dans les degrés de liberté entre les modèles, indiquant 
# le nombre de restrictions imposées.
# Somme des carrés : Le changement dans la somme des carrés des résidus, 
# reflétant la réduction de la variation non expliquée en passant du modèle
# restreint (Modèle 1) au modèle complet (Modèle 2)

# La statistique F est de 2,7795, et la valeur p associée est de 0,06239
# Selon ces résultats, vous n'avez pas de preuve solide pour rejeter l'hypothèse
# nulle (H0) selon laquelle 'age' et 'nk' n'ont pas d'effet significatif sur
# 'wtrans'. Cependant, la valeur p est proche du seuil de signification de 0,05,
# suggérant une possible influence.