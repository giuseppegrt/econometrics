# ------------------------------------------------------------------------------
#     Giuseppe Gruttad'Auria
#     Institut du management / Institut du management de l'information
#     Université de Neuchâtel
# ------------------------------------------------------------------------------

ddpath <- "C:/Users/giuse/Desktop/Econometrics/Material/Données-20230621/"

setwd(ddpath)

# Librairies
# ----------

library(lmtest)       # bptest, waldtest, coeftest
library(sandwich)     # vcovHC

# ==============================================================================
#                               SERIE EMETRIE NO 9
# ------------------------------------------------------------------------------
# Exercice 9.2
# ------------------------------------------------------------------------------


# Lecture des données de base

wdf <- read.csv2(file(paste(ddpath, "london.csv", sep=""), 
                      encoding="latin1"),
                 header=T, sep=";", dec=".", na.strings="NA")

# b) Estimations par MCO 
# ----------------------

# Formes structurelles

f4 <- formula(log(wfood) ~ log(totexp) + income + age + nk)
f2 <- update(f4, .~. - age - nk)

# Estimations 

lm.fit2 <- lm(f2, data = wdf, na.action = na.exclude)
lm.fit4 <- lm(f4, data = wdf, na.action = na.exclude)

summary(lm.fit2)
summary(lm.fit4)

# c) Tests d'hétéroscédasticité
# -----------------------------

# H0: homoscédasticité contre H1: hétéroscédasticité

# Modéle 2

bptest(lm.fit2)
# Cette fonction est utilisée pour effectuer le test de Breusch-Pagan, 
# qui évalue l'hypothèse d'homoscédasticité (constance de la variance des erreurs)
# par rapport à l'hypothèse alternative d'hétéroscédasticité (variance des erreurs
# dépendant des valeurs des variables indépendantes).

# En règle générale, une valeur plus élevée de la statistique de test indique une
# plus grande probabilité que l'hétéroscédasticité soit présente dans les résidus
# du modèle. La statistique de test (BP) est de 8.728.

# Modèle 4

bptest(lm.fit4)

# Comparativement au modèle précédent (lm.fit2), cette statistique de test (BP)
# plus élevée suggère un écart encore plus important par rapport à 
# l'homoscédasticité, ce qui renforce l'évidence d'une possible 
# hétéroscédasticité dans les résidus du modèle lm.fit4

# L'ajout de variables supplémentaires pourrait potentiellement contribuer à une
# plus grande complexité dans la relation entre les prédicteurs et la variable
# dépendante, ce qui peut conduire à une hétéroscédasticité différente ou plus
# prononcée dans les résidus


# Plot des résidus du modèle 4 par régresseur et pour la variable estimée

plot(log(wdf$totexp), residuals(lm.fit4))
# Si les résidus montrent une répartition aléatoire autour de zéro sur l'axe des
# y pour différentes valeurs de log(totexp) sur l'axe des x, cela suggère une 
# absence d'hétéroscédasticité. En revanche, si une structure ou un schéma est 
# observé (comme une forme de cône ou une tendance claire), cela peut indiquer
# la présence d'hétéroscédasticité, ce qui signifie que la variance des résidus
# change avec les valeurs de log(totexp).

plot(wdf$income, residuals(lm.fit4))
plot(wdf$age, residuals(lm.fit4))

plot(lm.fit4, which=1)
# Ce graphique "Residuals vs Fitted" représente les résidus (les erreurs entre
# les valeurs observées et prédites) en fonction des valeurs ajustées ou prédites
# par le modèle. Cela permet d'évaluer visuellement comment les résidus se
# comportent en fonction des valeurs prédites par le modèle.

# Si les résidus sont aléatoirement répartis autour de zéro, cela suggère que 
# l'hypothèse d'homoscédasticité (constance de la variance des résidus) est 
# respectée, ce qui est souhaitable pour un bon modèle de régression.
# Si une tendance ou un schéma est observé dans la dispersion des résidus par 
# rapport aux valeurs ajustées, cela pourrait indiquer une violation de 
# l'homoscédasticité


# Après avoir examiné les graphiques, nous pouvons supposer une violation de 
# l'hypothèse d'homoscédasticité


# Tests d'hétéroscédasticité avec forme fonctionnelle ad hoc

bptest(lm.fit4, varformula = ~ totexp,
       data = wdf)
# Ces résultats indiquent qu'il y a une forte probabilité (la valeur p est 
# inférieure à 0.05) de rejeter l'hypothèse nulle d'homoscédasticité pour le
# modèle lm.fit4
# La relation entre la variable 'totexp' et la variance des résidus semble 
# donc être significative pour expliquer l'hétéroscédasticité observée.

bptest(lm.fit4, varformula = ~ age + totexp,
       data = wdf)
# On ne peut pas affirmer avec assurance la présence d'hétéroscédasticité basée
# sur cette valeur p, bien que la statistique de test indique une certaine
# divergence par rapport à l'homoscédasticité.

bptest(lm.fit4, varformula = ~ log(totexp) + I(log(totexp))^2 + income,
       data = wdf)
# Suggère fortement que l'hypothèse nulle d'homoscédasticité doit être rejetée
# pour le modèle lm.fit4. Ces résultats impliquent qu'il existe des preuves
# statistiquement significatives pour supporter l'idée d'une hétéroscédasticité
# dans les résidus du modèle, spécifiquement liée aux variables 'log(totexp)',
# '(log(totexp))^2' et 'income'.

# d) Inférences hétéroscédastiques-robustes
# -----------------------------------------
       
# Coefficients individuels

coeftest(lm.fit4, df=Inf, vcov = vcovHC(lm.fit4, type="HC0"))
# Le test présenté est un test z des coefficients dans un modèle de régression
# linéaire, mais il est réalisé avec des erreurs standards robustes (type HC0),
# ce qui offre une correction pour l'hétéroscédasticité et les éventuelles 
# spécifications incorrectes du modèle.
# Un modèle linéaire standard suppose certaines conditions sur les erreurs
# ou résidus, notamment l'homoscédasticité (variance constante des erreurs) 
# et l'absence de corrélation entre les erreurs. Lorsque ces hypothèses sont
# violées, les erreurs types standards des coefficients peuvent ne pas être 
# fiables, ce qui affecte la précision des tests de signification.

# HC0 : Cette méthode calcule des erreurs types robustes sans correction pour
# la corrélation ou l'hétéroscédasticité. Elle fournit des erreurs types robustes
# en ajustant pour l'hétéroscédasticité seule, mais suppose l'indépendance des
# observations. Il s'agit de la forme la plus simple d'erreurs types robustes
# pour l'hétéroscédasticité.

coeftest(lm.fit4, df=Inf, vcov = vcovHC(lm.fit4, type="HC3"))
# HC3 : Contrairement à HC0, HC3 est une méthode plus conservatrice.
# Elle intègre une correction pour l'autocorrélation potentielle et 
# l'hétéroscédasticité dans les données. Cette correction rend HC3 plus 
# efficace pour traiter les violations d'homoscédasticité et de corrélation
# entre les observations.

# Tests des modèles

waldtest(lm.fit2, lm.fit4)
# Ces résultats indiquent que l'ajout des variables supplémentaires age et nk 
# dans le Modèle 2 apporte une amélioration significative par rapport au Modèle 1
# qui ne les inclut pas. Cela suggère que le Modèle 2 offre une meilleure 
# explication ou ajustement des données par rapport au Modèle 1.

waldtest(lm.fit2, lm.fit4, vcov = vcovHC(lm.fit4, type = "HC3"), test = "F")
# Cela confirme que l'ajout des variables age et nk dans le Modèle 2 apporte 
# une amélioration significative par rapport au Modèle 1, même en tenant compte
# de l'hétéroscédasticité potentielle dans les données à l'aide d'erreurs 
# standards robustes de type HC3.