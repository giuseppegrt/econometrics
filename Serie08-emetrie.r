# ------------------------------------------------------------------------------
#     Giuseppe Gruttad'Auria
#     Institut du management / Institut du management de l'information
#     Université de Neuchâtel
# ------------------------------------------------------------------------------

ddpath <- "C:/Users/giuse/Desktop/Econometrics/Material/Données-20230621/"

setwd(ddpath)

# Librairie
# ---------

library(tseries)       # jarque-bera.test

# ==============================================================================
#                               SERIE EMETRIE NO 8
# ------------------------------------------------------------------------------
# Exercice 8.2
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

f1 <- formula(log(prestige) ~ income + education)

# Estimations

lm.fit1 <- lm(f1, data = wdf, na.action = na.exclude)
summary(lm.fit1)
# Les résidus sont centrés autour de zéro, ce qui est une bonne indication 
# pour un modèle linéaire (Median 0.04839)
# mais il y a quelques valeurs extrêmes (max à 0.98244 et min à -1.33638)
# suggérant des observations potentiellement influentes ou atypiques
# Cela signifie que ces résidus devraient idéalement être répartis symétriquement
# autour de zéro, avec la plupart des résidus proches de zéro et moins de résidus
# à mesure que l'on s'éloigne de zéro dans les deux sens (Normalité des résidus)

# c) Valeurs aberrantes
# ---------------------

# Plots des résidus standardisés

plot(lm.fit1, which=3)
# which=3 produit un graphique qui met en évidence les résidus standardisés.
# Cela permet de visualiser la répartition de ces résidus et d'identifier
# visuellement les valeurs atypiques ou les schémas inattendus dans les données

# La ligne rouge représente généralement la ligne zéro sur l'axe des ordonnées (y)
# Pour les résidus standardisés, cette ligne indique où les résidus équivalent à
# zéro. Les résidus au-dessus de cette ligne sont positifs, tandis que ceux en
# dessous sont négatifs.

# Dans ce graphique, la variance des résidus change en fonction des différentes
# valeurs prédites.


# Test d'une observation aberrante

nu <- lm.fit1$df.residual - 1

# lm.fit1$df.residual récupère les degrés de liberté des résidus à partir du
# modèle "lm.fit1" et - 1 soustrait 1 à ce nombre de degrés de liberté
# car l'intercept est incluse dans le modèle et réduit donc d'un degré de liberté

data.frame(rstudentized = rstudent(lm.fit1), 
           pvalue = 2*pt(abs(rstudent(lm.fit1)), nu, lower.tail = F))

# rstudent() calcule les résidus studentisés
# abs() calcule les valeurs absolues
# pt(abs(rstudent(lm.fit1)), nu, lower.tail = F) calcule les p-valeurs associées
# à chaque résidu studentisé. pt() calcule la fonction de répartition cumulative
# de la distribution t de Student pour chaque valeur absolue des résidus
# studentisés, en utilisant les degrés de liberté nu précédemment calculés.
# La multiplication par 2 et la spécification de lower.tail = FALSE correspondent
# à un test bilatéral pour évaluer l'extremum des valeurs absolues des résidus

# Compréhension des résultats :
# Des valeurs absolues plus importantes des résidus studentisés indiquent des
# observations qui s'écartent significativement des prédictions du modèle.
# Les p-valeurs servent à évaluer la signification de ces écarts.
# L'observation 6 a un résidu studentisé absolu élevé de 2,442, suggérant qu'elle
# est notablement différente de ce que prédit le modèle.
# Sa p-valeur correspondante est de 0,019, indiquant que cette observation est
# statistiquement significative à un niveau de 0,05.


# qq-plot des résidus studentisés

p <- seq(0, 1, 0.01)
# seq(0, 1, 0.01) crée une séquence de nombres allant de 0 à 1 (inclus),
# avec un pas de 0,01 entre chaque nombre
# Essentiellement, cela génère une plage de probabilités de 0 % à 100 % avec un
# intervalle de 1 %

qrstudent <- quantile(rstudent(lm.fit1), prob = p)
# quantile() calcule les quantiles de la distribution de ces résidus studentisés
# en se basant sur les probabilités spécifiées dans la séquence p
qtheo <- qt(p, nu)
# qt(p, nu) calcule les quantiles de la distribution de Student-t pour les mêmes
# probabilités (p) et les degrés de liberté (nu) calculés précédemment.
# Il calcule les quantiles théoriques attendus sous l'hypothèse d'une
# distribution de Student-t

plot(qtheo, qrstudent)

# Interprétation :
# Le graphique compare les quantiles théoriques d'une distribution de Student-t
# (basée sur les probabilités spécifiées et les degrés de liberté) avec les
# quantiles observés dérivés des résidus studentisés.
# Une correspondance parfaite entre les quantiles théoriques et observés 
# indiquerait que la distribution des résidus studentisés correspond bien à 
# une distribution théorique de Student-t.
# Des différences ou des écarts entre les points du graphique pourraient 
# suggérer des déviations par rapport à la distribution supposée ou des motifs
# dans les résidus qui s'écartent d'une distribution de Student-t théorique.
# Idéalement, les points tracés suivraient une ligne diagonale de 45 degrés
# depuis l'origine. Cela signifierait que les résidus observés correspondent
# exactement à ceux attendus d'une distribution de Student-t

# d) Normalité
# ------------

# Statistique descriptive des résidus et résidus standardisés

summary(residuals(lm.fit1))
# La médiane des résidus est proche de zéro (0.04839), ce qui indique une
# certaine symétrie dans la distribution des résidus autour de la moyenne.
# La moyenne des résidus est exactement zéro (0.00000), ce qui suggère que 
# la somme des résidus est égale à zéro, conforme à la nature des modèles
# de régression centrés.
summary(rstandard(lm.fit1))
# La médiane des résidus standardisés est positive et égale à 0.106192,
# ce qui peut suggérer une légère asymétrie positive dans la distribution.
# La moyenne des résidus standardisés est très proche de zéro (0.000717),
# ce qui signifie que la somme des résidus standardisés est pratiquement
# égale à zéro

# Box plot des résidus standardisés

boxplot(rstandard(lm.fit1), horizontal = T)
# En examinant ce graphique, vous pouvez :
# Identifier visuellement la répartition des résidus standardisés autour de 
# leur médiane.
# Repérer les valeurs aberrantes potentielles qui sont représentées par des
# points en dehors des moustaches.
# Évaluer la dispersion et la symétrie des résidus standardisés autour 
# de leur médiane.

# Plot des probabilités normales

plot(lm.fit1, which=2)
# En examinant ce graphique, vous pouvez évaluer visuellement la normalité des
# résidus standardisés. Une adéquation proche de la ligne diagonale suggère une
# distribution normale des résidus, tandis que des motifs ou des courbes dans 
# les points pourraient indiquer des problèmes de non-normalité dans les résidus


# évaluer si la distribution des résidus standardisés du modèle de régression 
# linéaire correspond à une distribution normale:

# Test de Kolmogorov-Smirnov

ks.test(rstandard(lm.fit1), "pnorm")
# Avec une p-valeur élevée de 0.9854 il n'y a pas suffisamment de preuves pour 
# rejeter l'hypothèse nulle. Cela suggère que les données ne fournissent pas de
# preuves significatives pour affirmer que la distribution des résidus 
# standardisés diffère de manière significative d'une distribution normale.

# Test de Shapiro-Wilk

shapiro.test(rstandard(lm.fit1))
# Avec une p-valeur de 0.8088 on ne dispose pas de suffisamment de preuves pour
# rejeter l'hypothèse nulle. Cela suggère que les résidus standardisés du modèle
# de régression ne présentent pas de différences significatives par rapport à
# une distribution normale

# Test de Jarque-Bera

jarque.bera.test(rstandard(lm.fit1))
# X-squared est égale à 1.0752. Cette statistique est utilisée pour évaluer si 
# les résidus standardisés suivent une distribution normale. Plus la valeur 
# de cette statistique est proche de zéro, plus les résidus sont proches d'une
# distribution normale.
# Avec une p-valeur de 0.5841 il n'y a pas suffisamment de preuves pour rejeter
# l'hypothèse nulle. Cela suggère que les résidus standardisés du modèle de 
# régression ne présentent pas de différences significatives par rapport à une
# distribution normale