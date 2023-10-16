# ------------------------------------------------------------------------------
#     Giuseppe Gruttad'Auria
#     Institut du management / Institut du management de l'information
#     Université de Neuchâtel
# ------------------------------------------------------------------------------

ddpath <- "C:/Users/giuse/Desktop/Econometrics/Material/Données-20230621/"

setwd(ddpath)

# ===========================================================================
#                               SERIE EMETRIE NO 3
# ---------------------------------------------------------------------------
# Exercice 3A.1
# ---------------------------------------------------------------------------

# a) Données de base
# ------------------

# Lecture des données de base

wdf <- read.csv2(file(paste(ddpath, "andy.csv", sep=""), 
                      encoding="latin1"), 
                 header=T, sep=";", dec=".", na.strings="NA")

head(wdf)
# les données de sales et de advert doivent être multipliées par 1000
nrow(wdf)
# nombre de villes
ncol(wdf)

# b) Graphique de dispersion de type pairs
# ----------------------------------------

pairs(wdf)
# Graphique de paires, nous pouvons intuitivement observer quel type de relation
# existe entre les variables, s'il s'agit d'une relation linéaire ou non.
# Observons la relation entre les ventes (sales) et le prix (price) ;
# dans ce cas, nous pouvons voir que les points évoluent en fonction du prix.

# Ce graphique permet également de déterminer s'il n'y a pas de colinéarité 
# (relation linéaire) entre les variables que nous avons dans le modèle.
# Cela signifie qu'une variable ne doit pas être exprimée en fonction d'une autre.

# De cette manière, nous pouvons avoir des intuitions sur les variables les plus
# importantes dans le modèle à travers la relation linéaire

# c) Estimation par MCO (1er modèle)
# ----------------------------------

# Forme structurelle

f1 <- formula(sales ~ advert + price)

# Sales en fonction de la publicité (advert) et du prix (price)

# Estimation

lm.fit1 <- lm(f1, data = wdf, na.action = na.exclude)
summary(lm.fit1)

# Allons voir le signe des coefficients. La magnitude de l'effet est relative
# aux autres variables. Regardons le coefficient de détermination R², 
# il n'est pas très élevé, donc le modèle n'explique pas bien la variabilité.
# Le modèle linéaire n'est probablement pas le meilleur, donc explorons d'autres
# hypothèses. Nous pouvons envisager de changer le type de modèle ou les variables.

# Le coefficient de régression estimé de la variable advert est 1.86, il signifie
# que pour chaque augmentation de mille dollars de dépenses en publicité 
# les ventes moyennes des burgers augmentent de 1'800 dollars, ceteris paribus

# Le coefficient de régression estimé de la variable price est -7.9, il nous dit
# que si on augmente l'indice de prix d'une unité alors les ventes moyennes des
# burgers diminuent de presque 8'000 dollars, ceteris paribus.

# d) Estimation par MCO (2e modèle: forme quadratique dans les variables)
# -----------------------------------------------------------------------

# Forme structurelle

f2 <- formula(sales ~ advert + I(advert^2) + price + I(price^2))

# Essayons donc d'utiliser les mêmes variables sous une forme quadratique. 
# 'I' sert à indiquer que nous prenons le carré de cette variable.

# Estimation

lm.fit2 <- lm(f2, data = wdf, na.action = na.exclude)
summary(lm.fit2)

# R² est un peu plus élevé, mais c'est évident car plus nous ajoutons de variables,
# plus il augmente. Donc, commençons à examiner également le R² ajusté,
# qui tient compte du nombre de variables

# L'effet d'inversion des coefficients peut être compris en considérant la 
# dérivée des variables par rapport au modèle. 

# e) Valeurs prédites conditionnellement à la moyenne d'un des régresseurs
# ------------------------------------------------------------------------

# Représentation graphique de deux diagrammes de dispersion

op <- par()
# copie des paramètres graphiques actuels et les stocke dans la variable op.
# Cela permet de sauvegarder les paramètres graphiques existants pour pouvoir
# les restaurer plus tard.

par(mfrow = c(1,2))
# configure les paramètres graphiques pour organiser les futurs graphiques en
# une rangée (1) avec deux colonnes (2)

plot(wdf$advert, wdf$sales, xlab = "Advert", ylab = "Sales", cex.main = 0.9,
     main = "Valeurs estimées\n conditionnellement à mean(price)") 
# graphique de dispersion en utilisant les données de la variable "advert"
# sur l'axe des x et les données de la variable "sales" sur l'axe des y
# xlab/ylab définit l'étiquette de l'axe
# cex.main = 0.9 : définit la taille du titre du graphique.
# "0.9" indique que la taille est réduite à 90 % de la taille par défaut.
# main indique le titre principal du graphique

# sales sur advert, valeurs estimées conditionnellement à mean(price)

new.wdf <- transform(wdf, price = mean(price))
# nouveau dataframe, cela remplace les valeur de "price" par la moyenne de "price"
sales.fit <- predict(lm.fit2, newdata = new.wdf)
# utilise le modèle de régression linéaire lm.fit2
# il prédit les valeurs de "sales" en fonction de "advert" et "price"
# conditionnellement à la moyenne de "price"
mat <- cbind(wdf$advert, sales.fit)
# La première colonne de cette matrice contient les valeurs de "advert" 
# telles qu'elles se trouvent dans le dataframe wdf, et la deuxième colonne
# contient les valeurs prédites sales.fit
mat <-  mat[order(mat[,1]),]
# trie la matrice mat en fonction de la première colonne (les valeurs de "advert")
# en ordre croissant. # Cela permet d'organiser les valeurs prédites en 
# fonction des valeurs d'origine de "advert"
lines(mat, col="red")
# Cela permet de visualiser comment les valeurs prédites de "sales" 
# varient en fonction de "advert" conditionnellement à la moyenne de "price".

# la concavité montre qu'au-delà d'un certain point il n'est pas opportun 
# d'investir dans la publicité

# sales sur price, valeurs estimées conditionnellement à mean(advert)

plot(wdf$price, wdf$sales, xlab = "Price", ylab = "Sales", cex.main = 0.9,
     main =  "Valeurs estimées\n conditionnellement à mean(advert)")
     
new.wdf <- transform(wdf, advert = mean(advert))
sales.fit <- predict(lm.fit2, newdata = new.wdf)
mat <- cbind(wdf$price, sales.fit)
mat <-  mat[order(mat[,1]),]
lines(mat, col="red")

# dans ce cas, nous voyons comment lorsque les prix augmentent, les ventes
# diminuent, mais au-delà d'un certain point, elles ne diminuent pas beaucoup

# Restauration des paramètres par défaut

par(op)

# f) Estimation par MCO (3e modèle: forme polynomiale dans les variables)
# -----------------------------------------------------------------------

# Forme structurelle

f3 <- formula(sales ~ poly(advert, 3, raw = T) + poly(price, 3, raw = T))

# modèle de régression polynomial de degré 3 pour les variables "advert" et 
# "price" par rapport à la variable "sales"
# raw = T signifie que les polynômes sont exprimés en termes des variables
# originales plutôt que des polynômes orthogonaux
# sales = β0 + β1p + β2p^2 + β3p^3 + β4a + β5a^2 + β6a^3

# Estimation

lm.fit3 <- lm(f3, data = wdf, na.action = na.exclude, x = TRUE)
summary(lm.fit3)

# g) Estimation par MCO (4e modèle: régression polynomiale orthogonale)
# -----------------------------------------------------------------------

# Forme structurelle

f4 <- formula(sales ~ poly(advert, 3) + poly(price, 3))

# Estimation

lm.fit4 <- lm(f4, data = wdf, na.action = na.exclude, x = TRUE)
summary(lm.fit4)

# x = TRUE: La matrice modèle contient les valeurs des variables explicatives
# telles qu'elles sont utilisées dans le modèle

# valeur propres
eigen(crossprod(lm.fit3$x))
eigen(crossprod(lm.fit4$x))

# avec ces deux variables (price, advert) nous ne pouvons pas exprimer la
#  variabilité des ventes à travers les modèles

# --------------------------------------------------------------------------
# Exercice 3A.2
# --------------------------------------------------------------------------

# a) Données de base
# ------------------

# Lecture des données de base

wdf <- read.csv2(file(paste(ddpath, "andy.csv", sep=""), 
                      encoding="latin1"), 
                 header=T, sep=";", dec=".", na.strings="NA")
                 
# b) Estimations des modèles (3)-(4)
# ----------------------------------

# Formes structurelles

f5 <- formula(log(sales) ~ log(advert) + log(price))
f6 <- formula(log(sales) ~ advert + price)

# Estimations

lm.fit5 <- lm(f5, data = wdf, na.action = na.exclude)
lm.fit6 <- lm(f6, data = wdf, na.action = na.exclude)

summary(lm.fit5)
summary(lm.fit6)