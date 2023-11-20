# ------------------------------------------------------------------------------
#     Giuseppe Gruttad'Auria
#     Institut du management / Institut du management de l'information
#     Université de Neuchâtel
# ------------------------------------------------------------------------------

ddpath <- "C:/Users/giuse/Desktop/Econometrics/Material/Données-20230621/"

setwd(ddpath)

# ==============================================================================
#
#                               SERIE EMETRIE NO 7
#
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Exercice 7.2
# ------------------------------------------------------------------------------

# a) Données de base
# ------------------

# Lecture des données de base

wdf <- read.csv2(file(paste(ddpath, "stockton3.csv", sep=""), 
                      encoding="latin1"), 
                 header=T, sep=";", dec=".", na.strings="NA")

# b) Estimations par MCO
# ----------------------

# Formes structurelles

f1 <- formula(log(sprice) ~ livarea + age + beds + baths)

# Estimations

lm.fit1 <- lm(f1, data = wdf, na.action = na.exclude)
summary(lm.fit1)

# La valeur minimale de -1.27265 et la valeur maximale de 1.98562 indiquent 
# l'étendue des résidus.
# La médiane (0.00312) ainsi que le premier quartile (1Q) à -0.13606 et le 
# troisième quartile (3Q) à 0.14844 suggèrent que la majorité des résidus sont 
# centrés autour de zéro.
# L'asymétrie vers le côté droit de la distribution est inférée du fait que le 
# résidu maximal (1.98562) est plus grand que le résidu minimal (-1.27265),
# indiquant une distribution légèrement asymétrique vers des valeurs positives 
# plus élevées.

# c) Analyse des résidus
# ----------------------

# Résidus

data.frame(residuals(lm.fit1), rstandard(lm.fit1), rstudent(lm.fit1))
# L'utilisation de data.frame() combine ces trois vecteurs de résidus en un
# tableau de données, ce qui facilite leur visualisation et leur analyse, 
# en fournissant une vue côte à côte des différentes mesures de résidus
# pour chaque observation du modèle de régression.

# Résidus  -> residuals(lm.fit1) :
# Différences entre les valeurs observées et les valeurs prédites

# Résidus Standardisés -> rstandard(lm.fit1) :
# Ces résidus mesurent à quel point chaque résidu est éloigné de la moyenne
# en termes d'écart-type. Ils aident à identifier les observations atypiques
# qui s'écartent considérablement de la tendance générale des résidus

# Résidus Studentisés -> rstudent(lm.fit1)
# Ces résidus prennent en compte à la fois l'écart-type estimé des résidus et
# la contribution de chaque observation à l'ajustement du modèle. Ils sont
# utiles pour détecter les observations influentes qui ont un impact 
# disproportionné sur les résultats du modèle.


# Plots des résidus

op <- par()
# permet de conserver les réglages graphiques pour les restaurer plus tard
par(mfrow=c(2,2))
# mfrow=c(2,2) signifie "multi-frame row" avec 2 lignes et 2 colonnes. 
# Ainsi, cela créera une grille de 2 lignes par 2 colonnes pour afficher
# les graphiques suivants.
plot(lm.fit1)
par(op)
# restaure les paramètres graphiques sauvegardés précédemment

# Résidus vs Valeurs ajustées :
# Ce graphique montre la relation entre les valeurs ajustées (valeurs prédites)
# et les résidus. Idéalement, les résidus devraient être dispersés aléatoirement
# autour de la ligne horizontale à zéro sans présenter de motif spécifique.
# S'il y a un motif distinct, cela indique des problèmes potentiels avec le
# modèle, tels que la non-linéarité ou l'hétéroscédasticité.

# Il semble qu'il y ait quelques problèmes de linéarité. Cela indique que la
# variance n'est pas constante et qu'il y a une violation de l'hypothèse
# d'homoscédasticité.

# Graphique de Quantiles-Quantiles (Q-Q normal) :
# Ce graphique compare la distribution des résidus à une distribution normale
# théorique. Si les points suivent une ligne droite, cela suggère que les
# résidus suivent une distribution normale. Toute déviation par rapport à une
# ligne droite indique des écarts à la normalité des résidus.

# Graphique d'Étendue-Lieu :
# Ce graphique évalue la dispersion (variabilité) des résidus par rapport aux 
# valeurs ajustées. Idéalement, les points devraient former une bande 
# horizontale sans motif discernable. Si la dispersion change de manière
# systématique selon les valeurs ajustées, cela suggère une hétéroscédasticité 
# ou des problèmes de variance constante.

# le "Résidus vs Valeurs ajustées" est plus axé sur la détection des schémas
# non linéaires ou spécifiques dans la relation entre les valeurs ajustées et
# les résidus, tandis que le "Graphique d'Étendue-Lieu" se concentre
# spécifiquement sur l'évaluation de la constance de la variance des résidus
# en fonction des valeurs ajustées.

# Résidus vs Levier :
# Ce graphique aide à identifier les observations influentes (valeurs aberrantes)
# en examinant le levier de chaque observation par rapport à ses résidus 
# standardisés. Les observations situées dans les coins supérieur-droit ou
# inférieur-droit du graphique (levier élevé avec résidus élevés) sont des 
# valeurs aberrantes potentielles ou des points influents qui pourraient avoir
# un impact significatif sur le modèle de régression.


# Mesures d'influence et diagnostics

names(influence(lm.fit1))
# permet d'obtenir une liste des noms des différentes mesures d'influence
# pouvant être calculées pour évaluer l'impact des observations individuelles
# sur le modèle

influence.measures(lm.fit1)
# retourne un ensemble de mesures d'influence telles que les valeurs de levier,
# les résidus studentisés, les distances de Cook, etc., pour chaque observation
# du modèle. Ces mesures aident à identifier les observations ayant un impact
# important sur les résultats du modèle.

summary(influence.measures(lm.fit1))

# dfb.1_, dfb.livr, dfb.age, dfb.beds, dfb.bths :
# Ces colonnes contiennent les mesures de DFFITS pour chaque coefficient 
# du modèle. DFFITS représente le nombre d'écarts-types par lesquels la valeur
# ajustée d'une observation spécifique diffère si cette observation est incluse
# ou exclue du modèle.
# Les valeurs élevées dans ces colonnes indiquent une forte influence sur les
# estimations des coefficients associés à chaque variable.

# dffit :
# représente la distance de Cook pour chaque observation.
# Une distance de Cook élevée suggère une forte influence sur les estimations
# des coefficients du modèle ainsi que sur la prédiction globale.

# cov.r :
# Indique le ratio de la variance des résidus avec l'observation incluse et
# sans elle. Les valeurs éloignées de 1 peuvent indiquer une observation
# ayant un impact significatif sur la variance des résidus.

# cook.d :
# Représente la statistique de Cook pour chaque observation.
# Des valeurs élevées de Cook.D indiquent des observations influentes sur 
# la régression.

# hat :
# Désigne les valeurs du levier. Les valeurs élevées suggèrent une observation
# ayant un effet disproportionné sur la régression.