# ------------------------------------------------------------------------------
#     Giuseppe Gruttad'Auria
#     Institut du management / Institut du management de l'information
#     Université de Neuchâtel
# ------------------------------------------------------------------------------

ddpath <- "C:/Users/giuse/Desktop/Econometrics/Material/Données-20230621/"

# Cette ligne de code crée une variable appelée ddpath et lui attribue une valeur.
# Dans ce cas, la valeur est une chaîne de caractères qui représente le chemin 
# d'accès complet vers un répertoire sur votre système de fichiers.

setwd(ddpath)

# Cette ligne de code utilise la fonction setwd() (pour "Set Working Directory"),
# qui est utilisée pour définir le répertoire de travail dans R.
# Le répertoire de travail est le dossier où R cherchera les fichiers par défaut
# lorsqu'il effectue des opérations de lecture ou d'écriture de fichiers,
# à moins que vous ne spécifiiez un chemin complet.

# ==============================================================================
#                               SERIE EMETRIE NO 1
# ------------------------------------------------------------------------------
# Exercice 1.2
# ------------------------------------------------------------------------------

#1) Travail Préparatoire :

#  Importation de la Base de Données :
#  La première étape consiste à importer le jeu de données dans l'environnement R.


wdf <- read.csv2(file(paste(ddpath, "menage.csv", sep=""), 
                      encoding="latin1"), 
                 header=T, sep=";", dec=".", na.strings="NA")

# wdf <- ... assigne les données lues depuis le fichier CSV à une variable
# appelée wdf. Les données sont stockées dans un objet appelé "data frame" en R,
# qui est une structure de données tabulaire similaire à une feuille de calcul.

# read.csv2(...) utilise la fonction read.csv2() pour lire un fichier CSV
# (Comma-Separated Values) dans R

# file(paste(ddpath...)) en tant qu'argument de read.csv2
# Cela signifie que read.csv2() lira le contenu du fichier spécifié
# par le chemin d'accès généré par paste().
# L'argument sep="" spécifie qu'il ne doit pas y avoir de séparateur entre le
# chemin d'accès et le nom du fichier.
# L'argument encoding="latin1" spécifie l'encodage des caractères du fichier,
# dans ce cas, "latin1".

#Les arguments de read.csv2(...) :
#  header=T : Cela indique à R que la première ligne du fichier CSV contient des
# noms de colonnes (c'est-à-dire une ligne d'en-tête).
# sep=";" : Cela spécifie que le point-virgule (;) 
# est utilisé comme séparateur de champs dans le fichier CSV.
# dec="." : Cela indique que le point (.) est utilisé comme séparateur décimal
# pour les valeurs numériques.
# na.strings="NA" : Cela spécifie que les occurrences de "NA" dans le fichier CSV
# doivent être considérées comme des valeurs manquantes (NA, pour "Not Available").

head(wdf)

# head() est utilisée pour afficher les premières lignes d'un objet de données
# Dans cet exemple, wdf est un data frame qui contient des données
# Cela permet de vérifier rapidement les premières observations de vos données pour
# comprendre leur structure et leurs valeurs

nrow(wdf)

# nrow() est utilisée pour compter le nombre de lignes dans un data frame

ncol(wdf)

# ncol() est utilisée pour compter le nombre de colonnes dans un data frame

# ------------------------------------------------------------------------------

# Conversion d'une Variable en Facteur :
# Transformer la variable "type.menage" en facteur en la nommant "menage".

wdf <- transform(wdf, menage = factor(type.menage, 
                                      labels = c("1 personne",
                                                 "couple",
                                                 "famille")))

# transform() est utilisée pour créer une version modifiée du data frame

# (wdf, menage ... ici Nous créons une nouvelle variable appelée "menage" dans le
# data frame wdf. Cette variable va contenir les valeurs transformées.

# La fonction factor() est utilisée pour convertir une variable en facteur,
# ce qui est utile pour les variables catégorielles.

# type.menage est la variable que nous convertissons en facteur

# labels = c("1 personne", "couple", "famille") spécifie les libellés que nous
# attribuons aux niveaux du facteur. Dans cet exemple, nous assignons les libellés
# "1 personne", "couple" et "famille" aux niveaux de la variable "type.menage"

# ------------------------------------------------------------------------------

# Création d'une Deuxième Base de Données :
# Créer une deuxième base de données qui contient le revenu moyen (rev.moy) et
# les dépenses moyennes (dep.moy) par ménage. Cette nouvelle base de données
# contiendra également les variables "menage", "chef", et "npers" en tant
# qu'informations supplémentaires.

attach(wdf)

# attach() est utilisée pour attacher un data frame.
# Lorsqu'un data frame est attaché, cela signifie que vous pouvez accéder aux 
# variables de ce data frame sans avoir à spécifier le nom du data frame à 
# chaque fois que vous les utilisez.

(tab1 <- tapply(revenu, id.menage, mean))

# tapply() est une fonction utilisée pour appliquer une fonction (dans ce cas, 
# mean(), qui calcule la moyenne) à des valeurs groupées en fonction d'une 
# variable de groupe (ici, id.menage).

# revenu est la variable de revenu à partir de laquelle nous calculons la moyenne

# id.menage est la variable qui sert de groupe. La fonction tapply() calcule la
# moyenne du revenu pour chaque groupe distinct de id.menage

# Le résultat est stocké dans la variable tab1

(tab2 <- tapply(depense, id.menage, mean))

# Cette ligne de code est similaire à la précédente, mais cette fois-ci, nous
# calculons la moyenne des dépenses pour chaque groupe de id.menage

wdf.m <- data.frame(cbind(tab1, tab2), id.menage = names(tab1))

# data.frame(..., id.menage = names(tab1)) crée un data frame à partir de la
# matrice précédemment créée, où id.menage est une nouvelle colonne qui contient
# les noms des groupes id.menage

# cbind(tab1, tab2) combine les vecteurs tab1 et tab2 en colonnes
# pour former une matrice.

foo <- subset(wdf, chef == 1, select = c("id.menage", "npers", "menage"))

# subset() est utilisé pour extraire un sous-ensemble de lignes et de colonnes
# d'un data frame
# Dans cet exemple, nous extrayons les lignes où la variable chef est égale 
# à 1 (ce qui peut indiquer le chef de ménage) à partir du data frame wdf.
# Nous sélectionnons également trois colonnes : "id.menage", "npers", et "menage".

wdf.m <- merge(wdf.m, foo, by = "id.menage", all.x = T)

# merge() est utilisé pour fusionner deux data frames en fonction d'une ou plusieurs
# colonnes communes
# Dans cet exemple, nous fusionnons le data frame wdf.m (qui contient les moyennes
# du revenu et des dépenses) avec le data frame foo (qui contient les informations
# sur le chef de ménage).

# La fusion est effectuée en utilisant la colonne "id.menage" comme clé de fusion.

# L'argument all.x = T indique que toutes les observations du data frame de gauche
# (wdf.m) doivent être incluses dans le résultat final, même si elles ne 
# correspondent pas à des observations dans le data frame de droite (foo).

colnames(wdf.m) <- c("id.menage", "rev.moy", "dep.moy", "npers", "menage")

# Cette ligne de code définit les noms des colonnes du data frame wdf.m.
# Les noms des colonnes sont définis dans l'ordre des variables dans le ()

detach(wdf)

# La commande detach() est utilisée pour détacher un objet, cela signifie qu'il
# n'est plus attaché à l'environnement de travail, et ses variables ne sont plus
# accessibles directement sans spécifier le nom de l'objet.

# ------------------------------------------------------------------------------

# 2) Régression des Moindres Carrés Ordinaires (MCO) :
# Nous allons estimer plusieurs équations de régression en utilisant
# la méthode des moindres carrés ordinaires (MCO) et commenter les résultats.

# Modèle (1) :
# Calculez toutes les valeurs estimées et les résidus pour
# l'équation "depense = β1 + β2revenu".

f1 <- formula(depense ~ revenu)

# La formule depense ~ revenu indique que vous souhaitez modéliser la variable 
# depense en fonction de la variable revenu. Vous essayez de trouver une relation
# linéaire entre ces deux variables.

lm.mod1 <- lm(f1, data = wdf, na.action = na.exclude, x = T)

# lm() est utilisée pour effectuer la régression linéaire.
# Vous spécifiez le modèle f1 que vous avez défini précédemment.

# data = wdf indique que les données pour l'estimation se trouvent dans
# le data frame wdf

# na.action = na.exclude indique que les valeurs manquantes dans les données
# doivent être exclues de l'estimation

# x = T est utilisé pour demander l'inclusion de la matrice modèle X dans l'objet
# de régression linéaire lm.mod1. Cette matrice modèle contient les variables
# indépendantes (dans ce cas, uniquement "revenu") utilisées dans le modèle

summary(lm.mod1)
  
# Résumé statistique du modèle de régression lm.mod1.
# Vous obtiendrez des informations telles que les coefficients estimés,
# les statistiques de régression, les résidus, les tests d'hypothèses etc..

fitted(lm.mod1)

# fitted() est utilisée pour obtenir les valeurs ajustées par le modèle.
# Ces valeurs représentent les valeurs prévues pour la variable dépendante (depense)

residuals(lm.mod1)

# residuals() est utilisée pour obtenir les résidus, qui sont les différences entre
# les valeurs observées de la variable dépendante (depense) et les valeurs
# ajustées par le modèle

x0 <- data.frame(t(c(1, mean(wdf$revenu))))

# data.frame() crée le data frame x0

# t() transpose ce vecteur pour qu'il soit affiché en tant que 
# ligne dans le data frame x0.

# c(1, mean(wdf$revenu)) crée un vecteur contenant deux valeurs :
# 1 (pour l'intercept) et la moyenne des valeurs de la variable revenu
# du data frame wdf

colnames(x0) <- c("(Intercept)", "revenu")

# Les noms de colonnes sont définis comme "(Intercept)" (pour l'intercept)
# et "revenu"

predict(lm.mod1, newdata = x0)

# predict() est utilisée pour obtenir les prédictions à partir du modèle

# newdata = x0 spécifie les nouvelles données sur lesquelles vous souhaitez
# faire des prédictions

# ------------------------------------------------------------------------------

# Calculez les dépenses moyennes. Quel modèle vous donne ce résultat ?

# predict() pour obtenir des prédictions pour la moyenne des dépenses

moyenne_depense <- mean(wdf$depense)
moyenne_depense

# ------------------------------------------------------------------------------

# Modèle (2) :
# Effectuez une régression à partir de l'origine pour l'équation "depense = β1."

lm.mod0 <- lm(depense ~ 1., data = wdf, na.action = na.exclude)
summary(lm.mod0)

# La formule depense ~ 1. spécifie le modèle. Ici, le modèle est extrêmement simple,
# car il ne comprend qu'une constante (1). En d'autres termes, vous effectuez une
# régression linéaire où la variable dépendante depense est expliquée uniquement
# par une constante

# ------------------------------------------------------------------------------

# Modèles (3), (4) et (5) :
# Mettez à jour le modèle initial avec des variables supplémentaires
# et interprétez les résultats.


lm.mod2 <- update(lm.mod1, .~. + chef)
summary(lm.mod2)

lm.mod3 <- update(lm.mod1, .~. + menage)
summary(lm.mod3)

lm.mod4 <- update(lm.mod1, .~. + chef*menage)
summary(lm.mod4)

# La fonction update() est utilisée pour mettre à jour le modèle existant
# La notation .~. + X signifie que vous ajoutez la variable X au modèle existant.

# ------------------------------------------------------------------------------

# 3) Régression des Moindres Carrés Pondérés :
  
# En utilisant la deuxième base de données contenant des informations sur les ménages,
# nous allons estimer un modèle de régression des moindres carrés pondérés.

# Estimez le modèle "dep.moy = β1 + β2 * rev.moy" en utilisant les moindres carrés
# pondérés avec des poids w = 1/npers

f1 <- formula(dep.moy ~ rev.moy)

# le modèle de régression est défini comme dep.moy ~ rev.moy, ce qui signifie que vous
# effectuez une régression linéaire où la variable dépendante est dep.moy et 
# la variable indépendante est rev.moy

lm.mod1 <- lm(f1, data = wdf.m, weights = 1 / npers, na.action = na.exclude,
              x = T, y = T)

# weights = 1 / npers spécifie que les poids sont définis comme 1 / npers
# x = T, y = T indiquent que vous souhaitez conserver les matrices de conception X
# et la variable dépendante y

summary(lm.mod1)

# Par calcul matriciel

X <- as.matrix(lm.mod1$x)

# crée une matrice X à partir de la matrice de conception x du modèle de régression

y <- as.matrix(lm.mod1$y)

W <- diag(1/wdf.m$npers)

# crée une matrice de poids W en utilisant les valeurs inverses de la variable npers
# du data frame wdf.m

solve(crossprod(X, W %*% X)) %*% crossprod(X, W %*% y)

# régression linéaire pondérée en utilisant le calcul matriciel. La régression est
# basée sur les matrices X, W, et y que nous avons créées précédemment.

# ------------------------------------------------------------------------------
# Exercice 1.3
# ------------------------------------------------------------------------------

# Lecture des données de base

wdf <- read.csv2(file(paste(ddpath, "prestige.csv", sep=""), 
                      encoding="latin1"), 
                 header=T, sep=";", dec=".", na.strings="NA")

# Diagramme de dispersion et courbe non paramétrique
# entre le revenu et le prestige associé à un type d’emploi.

attach(wdf)

# Utilisez la fonction scatter.smooth du logiciel R pour dessiner le diagramme de
# dispersion de la variable PrestigeScore sur la variable AvIncome et ajuster
# une courbe non paramétrique au nuage de points.

scatter.smooth(AvIncome, PrestigeScore)

detach(wdf)