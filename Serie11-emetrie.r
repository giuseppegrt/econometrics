# ------------------------------------------------------------------------------
#     Giuseppe Gruttad'Auria
#     Institut du management / Institut du management de l'information
#     Université de Neuchâtel
# ------------------------------------------------------------------------------

ddpath <- "C:/Users/giuse/Desktop/Econometrics/Material/Données-20230621/"

setwd(ddpath)

# Librairies
# ----------

library(lmtest)    # dwtest, bgtest
library(sandwich)  # NeweyWest
library(orcutt)    # cochrane.orcutt

# ==============================================================================
#
#                               SERIE EMETRIE NO 11 
#
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Exercice 11.1
# ------------------------------------------------------------------------------

# a) Données de base
# ------------------

# Lecture des données de base

wdf <- read.csv2(file(paste(ddpath, "bangla.csv", sep=""), 
                      encoding="latin1"), 
                 header=T, sep=";", dec=".", na.strings="NA")
                 
# Création d'un objet time series

ts.wdf <- ts(wdf)
# ts() prend le dataframe () et le convertit en un objet de type série temporelle.
# Cela signifie qu'elle attribue à ces données une structure spécifique qui 
# comprend des informations temporelles

# Diagramme de dispersion des séries

plot(ts.wdf)
# La série temporelle commence au temps 1 pour 34 intervalles temporels. 
# Le graphique en fonction du prix et celui en fonction de la superficie se
# ressemblent, et il semble que plus les prix augmentent, plus la production 
# dans la zone augmente. Ainsi, nous pourrions être confrontés à un problème 
# de corrélation entre les variables ou d'autcorrélation.

# b) Estimations par MCO
# ----------------------

# Forme structurelle

f1 <- formula(log(a) ~ log(p))

# Estimations

lm.fit1 <- lm(f1, data = ts.wdf, na.action = na.exclude)
summary(lm.fit1)
# Le coefficient pour log(p) est significatif avec une p-valeur de 0.00865. 
# Cela indique qu'il y a une relation statistiquement significative entre le 
# logarithme des prix (log(p)) et la variable dépendante (log(a)).

# c) Tests d'autcorrélation
# -------------------------

# Test de Durbin-Warson

dwtest(lm.fit1)
# Le test de Durbin-Watson indique une autocorrélation possible dans les 
# résidus du modèle. Avec une statistique DW de 1.169 et une p-valeur de 0.004443,
# cela suggère une forte évidence d'autocorrélation positive.

# Test de Breusch-Godfrey

bgtest(lm.fit1)
# Le test de Breusch-Godfrey (BG) pour la corrélation sérielle jusqu'à l'ordre 1
# révèle une statistique de test (LM test) de 5.4743 avec 1 degré de liberté et 
# une p-valeur associée de 0.0193. Cette p-valeur inférieure à 0.05 suggère une
# preuve raisonnable de corrélation sérielle dans les résidus du modèle jusqu'à
# l'ordre 1.
bgtest(lm.fit1, order = 2)
# Bien que cette p-valeur soit supérieure à 0.05, elle reste proche et indique
# une preuve légèrement moins significative de corrélation sérielle dans les 
# résidus du modèle jusqu'à l'ordre 2.
(bg3 <- bgtest(lm.fit1, order = 3))
# Cette p-valeur, dépassant le seuil conventionnel de 0.05, indique une preuve 
# moins significative de corrélation sérielle dans les résidus du modèle jusqu'à
# l'ordre 3.
coeftest(bg3)
# Ce tableau présente les résultats du test des coefficients après le test de 
# Breusch-Godfrey pour la corrélation sérielle jusqu'à l'ordre 3.
# Cela suggère que seule la première lag de la corrélation sérielle (lag 1) 
# est significative, tandis que les autres ne le sont pas, en ce qui concerne 
# l'influence sur la variable dépendante.

# Le test de Breusch-Godfrey réalisé à différents ordres (1, 2 et 3) vise à
# évaluer la présence de corrélation dans les résidus du modèle jusqu'à un 
# certain délai temporel. Cela signifie examiner s'il existe des corrélations
# entre les résidus à différents intervalles de temps (1, 2 ou 3 périodes
# précédentes). Lorsque l'on observe la significativité d'un "lag" particulier
# tel que "lag(resid)_1" dans le test des coefficients, cela indique la 
# pertinence de la corrélation des résidus au premier délai temporel,
# c'est-à-dire entre les résidus d'une période précédente.

# d) Variance-covariance HAC
# --------------------------

NeweyWest(lm.fit1, lag=1)
# Matrice de variance-covariance HAC (Heteroskedasticity and Autocorrelation
# Consistent) estimée pour les coefficients du modèle. Cette matrice est obtenue
# en utilisant la méthode de Newey-West avec un délai (lag) égal à 1.
# Sur la diagonale principale, vous trouvez les variances estimées pour les 
# coefficients du modèle.
# En dehors de la diagonale principale, les valeurs représentent les covariances
# estimées entre les différents coefficients du modèle.
# Ces valeurs sont utilisées pour calculer les erreurs standards robustes qui 
# tiennent compte à la fois de l'autocorrélation et de l'hétéroscédasticité
# potentielles des résidus du modèle. Cette correction vise à obtenir des 
# estimations plus précises des coefficients en tenant compte de la structure
# de corrélation temporelle des résidus.
coeftest(lm.fit1, vcov = NeweyWest)
# Ces résultats indiquent que la variable log(p) reste significative avec un 
# effet statistiquement différent de zéro, même après avoir tenu compte de 
# l'autocorrélation et de l'hétéroscédasticité potentielles des résidus grâce 
# à la correction de la matrice de variance-covariance.

vcovHAC(lm.fit1)
# Matrice de variance-covariance HAC (Heteroskedasticity and Autocorrelation 
# Consistent) pour les coefficients du modèle estimée à l'aide de la méthode 
# vcovHAC.
# Les valeurs diagonales principalement représentent les variances estimées pour
# les coefficients du modèle.
# Les valeurs hors diagonale indiquent les covariances estimées entre les 
# différents coefficients du modèle.
# Ces erreurs standards robustes visent à fournir des estimations plus précises
# des coefficients en tenant compte de la structure de corrélation temporelle et
# de l'hétéroscédasticité des résidus.
coeftest(lm.fit1, vcov = vcovHAC)
# Ces résultats suggèrent que la variable log(p) conserve une signification 
# statistique avec un effet significativement différent de zéro, même après 
# avoir pris en compte l'autocorrélation et l'hétéroscédasticité potentielles
# des résidus via la matrice de variance-covariance corrigée.

# e) Estimation par Cochrane-Orcutt
# ---------------------------------

cochrane.orcutt(lm.fit1)
# Nombre d'itérations : la méthode Cochrane-Orcutt a été répété huit fois pour
# tenter de réduire l'autocorrélation des résidus jusqu'à un niveau acceptable.
# Coefficient de corrélation (rho) : Un coefficient de corrélation de 0.422139
# a été estimé, indiquant une corrélation positive entre les résidus consécutifs,
# après avoir corrigé l'autocorrélation.
# Valeur originale de Durbin-Watson : Elle est de 1.16899. Une valeur inférieure
# à 2 indique une légère autocorrélation positive des résidus dans le modèle 
# initial. (des valeurs plus faibles ou plus élevées que 2 suggèrent une 
# possible autocorrélation positive ou négative)
# Valeur transformée de Durbin-Watson : Après la transformation effectuée par 
# la méthode Cochrane-Orcutt, la statistique de Durbin-Watson est de 1.82056.
# Cette transformation est conçue pour réduire l'autocorrélation des résidus.
# Les estimations des coefficients sont obtenues après avoir appliqué la méthode
# de Cochrane-Orcutt pour réduire l'autocorrélation des résidus, ce qui a 
# potentiellement affecté les estimations des coefficients du modèle par 
# rapport à la version originale non corrigée. (log(p) 0.77612 vs 0.888371).