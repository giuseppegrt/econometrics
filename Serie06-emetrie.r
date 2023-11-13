# ------------------------------------------------------------------------------
#     Giuseppe Gruttad'Auria
#     Institut du management / Institut du management de l'information
#     Université de Neuchâtel
# ------------------------------------------------------------------------------

ddpath <- "C:/Users/giuse/Desktop/Econometrics/Material/Données-20230621/"

setwd(ddpath)

# Librairies
# ----------

library(car)          # linearHypothesis

# ==============================================================================
#                               SERIE EMETRIE NO 6
# ------------------------------------------------------------------------------
# Exercice 6.2
# ------------------------------------------------------------------------------

# a) Données de base
# ------------------

# Lecture des données de base

wdf <- read.csv2(file(paste(ddpath, "andy.csv", sep=""), 
                      encoding="latin1"), 
                 header=T, sep=";", dec=".", na.strings="NA")

# b) Estimations par MCO et IC
# ----------------------------

# Formes structurelles

f1 <- formula(sales ~ price + advert + I(advert^2))

# Estimations

lm.fit1 <- lm(f1, data = wdf, na.action = na.exclude)
summary(lm.fit1)

# Intervalle de confiance à 95% sur l'espérance 

(conf.lm.fit1 <- predict(lm.fit1, interval = "confidence", level = 0.95))
# interval = "confidence": Il quantifie l'incertitude associée aux prédictions 
# du modèle tout en supposant que le modèle lui-même est correct

# Intervalles de confiance et de prévision à 99 % 
# conditionnellement à un nouveau vecteur de covariées
# Nous créons un vecteur de prix allant de 4.5 à 7.0 avec un pas de 0.01 et un
# vecteur de dépenses publicitaires constant à 1.9.
price <- seq(4.5, 7.0, 0.01)
new.df <- data.frame(price, advert = 1.9)

# Calculer les intervalles de confiance à 99 %
conf.lm.fit1 <- predict(lm.fit1, newdata = new.df, 
                        interval = "confidence", level = 0.99)

# Calculer les intervalles de prévision à 99 % pour les ventes en fonction 
# des nouvelles covariées.
pred.lm.fit1 <- predict(lm.fit1, newdata = new.df, 
                        interval = "prediction", level = 0.99)
# interval = "prediction": Il quantifie l'incertitude des valeurs prédites 
# individuelles. En d'autres termes, il estime la plage dans laquelle une 
# nouvelle observation a de fortes chances de se situer.

# Tracer les intervalles de confiance et de prévision.
matplot(new.df$price, cbind(conf.lm.fit1, pred.lm.fit1[,-1]),
        lty = c(1,2,2,3,3), col = c("black", "blue", "blue", "red", "red"),
        type = "l", xlab = "price", ylab = "Predicted sales",
        main = "Confidence and prediction intervals at 99%")

# matplot(... crée le graphique. Les valeurs de prix de new.df sont utilisées
# comme valeurs sur l'axe des x, tandis que les Predicted sales avec les
# intervalles de confiance (conf.lm.fit1) et les intervalles de prédiction
# (pred.lm.fit1[,-1]) sont utilisés comme valeurs sur l'axe des y.
# La fonction cbind() combine les intervalles de confiance et de prédiction
# en une seule structure de données pour faciliter le tracé.

# lty = c(... spécifie les types de ligne pour chaque série de données 
# sur le graphique. Les valeurs 1, 2 et 3 correspondent à différents types de
# lignes (1 pour solide, 2 pour tirets, 3 pour points).

# col = c(... spécifie les couleurs des lignes sur le graphique.

# type = "l" : Cela indique que le graphique sera un graphique de lignes
        
legend(5.75, 100.0, legend = c("Predicted values", "Confidence interval",
                               "Prediction interval"), 
       col = c("black", "blue", "red"), lty = c(1, 2, 3), bty = "n")

# legend(5.75, 100.0, ... spécifie l'emplacement où la légende 
# sera positionnée sur le graphique. Les deux premiers arguments,
# 5.75 et 100.0, définissent les coordonnées (x, y) 
# où la légende commencera à être affichée sur le graphique.

# legend = c(... est un vecteur de texte qui définit les étiquettes
# col = c(... spécifie les couleurs associées aux étiquettes
# lty = c(... spécifie les types de ligne associés aux étiquettes
# bty = "n" indique que vous ne souhaitez pas de boîte autour de la légende.
# La valeur "n" signifie "none"

# les valeurs prédites sont des estimations ponctuelles du modèle pour une 
# valeur particulière des covariables, tandis que l'intervalle de prédiction
# fournit une plage de valeurs dans laquelle une nouvelle observation 
# individuelle est susceptible de se situer. L'intervalle de prédiction 
# est plus large et plus conservateur car il prend en compte l'incertitude 
# du modèle et la variabilité potentielle des futures observations,
# ce qui en fait un outil plus approprié pour la prévision de valeurs 
# individuelles. Les valeurs prédites, en revanche, sont des estimations 
# du modèle sans tenir compte de cette variabilité future.
# ------------------------------------------------------------------------------
# Exercice 6.3
# ------------------------------------------------------------------------------

# a) Données de base
# ------------------

# Lecture des données de base

wdf <- read.csv2(file(paste(ddpath, "beer.csv", sep=""), 
                      encoding="latin1"), 
                 header=T, sep=";", dec=".", na.strings="NA")
                 
# Variable "groupe"

wdf <- transform(wdf, groupe = factor(ifelse(as.numeric(rownames(wdf)) <= 
                                      15, 0, 1),
                                      labels=c("A", "B")))

# as.numeric(rownames(wdf)) extrait les noms des lignes du dataframe wdf et
# les convertit en valeurs numériques.           

# ifelse() teste si les valeurs numériques des noms de lignes sont inférieures
# ou égales à 15. Si c'est le cas, elle attribue la valeur 0, sinon elle attribue
# la valeur 1.

# factor(..., labels=c("A", "B")) : Vous convertissez les valeurs 0 et 1 en une
# variable de type facteur avec deux niveaux, "A" et "B".

# b) Estimations par MCO
# ----------------------

# Formes structurelles

f1 <- formula(log(q) ~ log(pb) + log(pl) + log(pr) + log(i))
                             
# Estimations

lm.fit1 <- lm(f1, data = wdf, na.action = na.exclude)
summary(lm.fit1)

# Le modèle global estime la relation entre la consommation de bière annuelle
# par ménage (q) et les variables explicatives suivantes :
# le prix de la bière (pb), le prix du lait (pl), le prix de la viande (pr),
# et le revenu par ménage (i).

# Les résidus du modèle sont généralement petits et se situent entre -0,180 et 
# 0,088, ce qui suggère que le modèle ajuste bien les données.
# Les coefficients du modèle indiquent que le prix de la bière (pb) a un effet 
# significatif et négatif sur la consommation de bière, tandis que prix de la
# viande (pr) et le revenu par ménage (i) ont des effets significatifs.
# Le modèle global a un R² ajusté de 0,7975, ce qui signifie que 79,75 % de la
# variabilité de la consommation de bière est expliquée par les variables 
# explicatives.
# Le test F global est significatif avec une valeur p très faible (3,799e-09),
# ce qui indique que le modèle est globalement significatif.

lm.fitA <- lm(f1, data = wdf, subset = c(groupe == "A"), na.action = na.exclude,
              y = T, x = T)
summary(lm.fitA)

# Ce modèle estime la relation entre la consommation de bière annuelle par ménage
# (q) et les mêmes variables explicatives, mais uniquement pour le groupe A.

# Les résidus du modèle sont également relativement petits, ce qui suggère un
# bon ajustement aux données du groupe A.
# Les coefficients du modèle pour le groupe A indiquent que le prix de la bière
# (pb) a un effet significatif et négatif sur la consommation de bière, tandis
# que le prix de la viande (pr) et le revenu par ménage (i) ont des effets significatifs.
# Le modèle pour le groupe A a un R² ajusté élevé de 0,87, ce qui signifie que 87%
# de la variabilité de la consommation de bière dans ce groupe est expliqué par
# les variables explicatives.
# Le test F pour le groupe A est significatif avec une valeur p très faible 
# (3,815e-05).

lm.fitB <- lm(f1, data = wdf, subset = c(groupe == "B"), na.action = na.exclude,
              y = T, x = T)
summary(lm.fitB)

# Ce modèle estime la relation entre la consommation de bière annuelle par ménage
# (q) et les mêmes variables explicatives, mais uniquement pour le groupe B.

# Les résidus du modèle pour le groupe B sont également relativement petits, 
# suggérant un bon ajustement aux données de ce groupe.
# Cependant, les coefficients du modèle pour le groupe B ne sont pas 
# significatifs, ce qui signifie que les variables explicatives n'ont pas un 
# impact significatif sur la consommation de bière dans ce groupe.
# Le modèle pour le groupe B a un R² ajusté très faible de -0,1756, indiquant que
# les variables explicatives n'expliquent pas bien la variabilité de la 
# consommation de bière dans ce groupe.
# Le test F pour le groupe B n'est pas significatif avec une valeur p élevée 
# (0,7522).

# En résumé, le modèle global semble expliquer de manière significative la 
# consommation de bière avec des variables explicatives significatives.
# Cependant, lorsque vous divisez les données en groupes A et B, le modèle pour
# le groupe A reste significatif, tandis que le modèle pour le groupe B ne
# parvient pas à expliquer de manière significative la consommation de bière.
# Il semble y avoir des différences entre les groupes A et B en termes d'impact
# des variables explicatives sur la consommation de bière.

# c) Test sur l'égalité des bêtas, i.e. beta_A = beta_B
# -----------------------------------------------------

# Résidus sous l'hypothèse beta_A <> beta_B

res <- c(residuals(lm.fitA), residuals(lm.fitB))
# combine les résidus des modèles de régression lm.fitA et lm.fitB en un vecteur

# Statistique F

df1 <- length(coef(lm.fit1))
# calcule le nombre de coefficients dans le modèle global. Cela représente le
# nombre de degrés de liberté du numérateur de la statistique F.

df2 <- nrow(wdf) - 2*df1
# calcule le nombre de degrés de liberté du dénominateur de la statistique F.
# nrow(wdf) représente le nombre total d'observations dans le dataframe wdf.
# Vous soustrayez deux fois le nombre de coefficients du modèle global (df1)
# pour obtenir le nombre de degrés de liberté du dénominateur.

# Statistique F
(F <- ( (crossprod(residuals(lm.fit1)) - crossprod(res)) / df1 ) / 
      ( crossprod(res) / df2 ))

# (crossprod(residuals(lm.fit1)) - crossprod(res)) calcule la somme des carrés
# des résidus du modèle global lm.fit1 moins la somme des carrés des résidus
# combinés (résultant de la combinaison des résidus des modèles A et B).
# Cela mesure la variation dans les résidus qui est spécifiquement due à la 
# différence entre les groupes A et B.

# (crossprod(res) / df2) calcule la somme des carrés des résidus combinés
# divisée par le nombre de degrés de liberté du dénominateur df2.

(p.value <- pf(F, df1, df2, lower.tail = F))      

# la statistique F est relativement faible (0,4578555), et la valeur p est 
# relativement élevée (0,8027142), ce qui suggère que les coefficients des
# modèles ne sont pas significativement différents entre les groupes A et B.

# Mais il n'y a pas suffisamment de preuves pour conclure que les coefficients des
# modèles diffèrent de manière significative entre les deux groupes
      
# ou...

foo <- solve(solve(crossprod(lm.fitA$x)) +  solve(crossprod(lm.fitB$x)))
dif.beta <- matrix(coef(lm.fitA) - coef(lm.fitB), df1, 1)

(F <- (t(dif.beta) %*% foo %*% dif.beta) / (df1 * crossprod(res) / df2))

# crossprod(lm.fitA$x) et crossprod(lm.fitA$x) calcule le produit matriciel
# de la transposée de la matrice

# solve() est utilisé pour inverser cette matrice résultante

# solve(...) + solve(...) les matrices inversées des produits matriciels des
# designs des modèles A et B sont sommées

# matrix(coef(lm.fitA) - coef(lm.fitB), df1, 1) calcule la différence entre 
# les vecteurs de coefficients des modèles

# t(dif.beta) %*% foo %*% dif.beta donne une statistique de test qui mesure
# la différence entre les coefficients des modèles A et B

# (F <- (...) / (df1 * crossprod(res) / df2)) donne la statistique F pour le
# test d'égalité des coefficients


# Avec la statistique des intervalles de prévision

sigma2_A <- summary(lm.fitA)$sigma^2
# mesure la dispersion des résidus du modèle A

d <- lm.fitB$y - lm.fitB$x %*% coef(lm.fitA)
# mesure les résidus du modèle B sous l'hypothèse que les coefficients du modèle 
# A sont utilisés.

n2 <- length(d)
# calcule la longueur du vecteur d, c'est-à-dire le nombre d'observations dans
# le modèle B.

V.d <- diag(n2) + lm.fitB$x %*% solve(crossprod(lm.fitA$x)) %*%  t(lm.fitB$x)
# calcule la matrice V.d, qui est utilisée pour mesurer la variabilité des
# résidus dans le modèle B

V.d <- sigma2_A * V.d
# La matrice V.d est multipliée par la variance des résidus du modèle A
# sigma2_A pour obtenir une estimation de la matrice de covariance.

(F <- crossprod(d, solve(V.d) %*% d) / n2)
# calcule la statistique F en utilisant la variance des résidus du modèle A,
# la matrice de covariance V.d, et le vecteur d
       
(F0.025 <- qf(0.025, n2, lm.fitA$df.residual))
# calcule le quantile de la distribution F à 0,025 pour les degrés de liberté
# du modèle A. Cela permet de déterminer le seuil inférieur pour un test à 0,05
# de niveau de signification

(F0.975 <- qf(0.975, n2, lm.fitA$df.residual))
# calcule le quantile de la distribution F à 0,975 pour les degrés de liberté
# du modèle A. Cela permet de déterminer le seuil supérieur pour un test à 0,05
# de niveau de signification