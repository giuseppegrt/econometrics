# ------------------------------------------------------------------------------
#     Giuseppe Gruttad'Auria
#     Institut du management / Institut du management de l'information
#     Université de Neuchâtel
# ------------------------------------------------------------------------------

ddpath <- "C:/Users/giuse/Desktop/Econometrics/Material/Données-20230621/"

setwd(ddpath)

# ==============================================================================
#                               SERIE EMETRIE NO 5
# ------------------------------------------------------------------------------
# Exercice 5.2
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

# b) Estimations par MCO 
# ----------------------

# Formes structurelles

f4 <- formula(wtrans ~ log(totexp) + age + nk)
f3 <- update(f4, .~. -nk)
f2 <- update(f3, .~. - age)
f1 <- formula(wtrans ~ 1.) # Modèle nul

# Estimations

lm.fit1 <- lm(f1, data = wdf, na.action = na.exclude)
lm.fit2 <- lm(f2, data = wdf, na.action = na.exclude)
lm.fit3 <- lm(f3, data = wdf, na.action = na.exclude)
lm.fit4 <- lm(f4, data = wdf, na.action = na.exclude)

summary(lm.fit1)
summary(lm.fit2)
summary(lm.fit3)
summary(lm.fit4)

# c) Analyse de variance
# ----------------------

# Chaque modèle séparément

anova(lm.fit1)

anova(lm.fit2)
# La valeur F est de 32.768, avec une très faible valeur p (1,249e-08),
# indiquant que l'ajout de "log(totexp)" est statistiquement significatif.

anova(lm.fit3)
# La somme des carrés de "age" est très faible (0.0002) avec une moyenne des 
# carrés très basse (0.00018), cela signifie que les valeurs de "age" ne sont pas
# fortement associées aux valeurs de "wtrans" dans ce modèle
# La valeur F est faible (0.0169) et la valeur p élevée (0.8965)
# suggèrent que "age" n'est pas un prédicteur important de "wtrans"

anova(lm.fit4)
# la variable "nk" semble avoir un effet significatif sur la variable dépendante
# "wtrans." La valeur F significative (5.542) et la valeur p relativement basse 
# (0.01869) indiquent que l'ajout de "nk" améliore de manière significative la 
# capacité du modèle à expliquer la variabilité de "wtrans." 

# Tous les modèles pris simultanément

anova(lm.fit1, lm.fit2, lm.fit3, lm.fit4)
# Les résultats indiquent que "log(totexp)" est une variable significative,
# "age" est non significative, "nk" est significatif.

anova(lm.fit2, lm.fit4)
# La valeur F n'est pas significative (p = 0.06239) pour la comparaison de ces 
# deux modèles, ce qui signifie que l'ajout de "age" et "nk" dans le modèle 
# "lm.fit4" n'améliore pas significativement l'ajustement du modèle par rapport
# au modèle "lm.fit2."