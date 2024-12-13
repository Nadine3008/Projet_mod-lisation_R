
#Installation du package

install.packages("CaTools")
library(caTools)

#IMPORTATION DE LA BASE DE DONNEES

Data = read.csv("C:\\Users\\nadin\\Downloads\\flydronepro.csv" )
Data

set.seed(123)

#PARTIE I : Élaboration d’un Modèle d’Analyse Prédictive

#1.1 Quel est le nombre d’observation ? Nous avons 1470 observations

#1.2 Quel est le nombre de variables ? Nous avons 35 variables

#1.3 Quelle est la rémunération mensuelle (MonthlyIncome) la plus élevée ?
  
valeur_max <- max(Data$MonthlyIncome)
valeur_max      #la rémunération mensuelle la plus élevée est de 19999 euros

#1.4 Quelle est la durée d'ancienneté la plus élevée dans l'entreprise (YearsAtCompany) ?

Max_ancienneté <- max(Data$YearsAtCompany)
Max_ancienneté    #la durée d'ancienneté la plus élevée dans l'entreprise est de 40 ans

#1.5 Quelle est la durée d’ancienneté la plus élevée au post (YearsInCurrentRole) ?

Max_post<-max(Data$YearsInCurrentRole)
Max_post   #la durée d'ancienneté la plus élevé au poste est de 18 ans

#1.6 Quel est le nombre de collaborateurs qui sont encore présents au sein de  l’entreprise ? 

Nb_collaborateur<- sum (Data$Attrition == "No" )
Nb_collaborateur  #le nombre de collaborateur encore présent est de 1233

#1.7 Parmi les collaborateurs présents, combien ont-ils 5 ans d’ancienneté au sein de  l’entreprise ?

collaborateurs_5ans <- sum(Data$Attrition == "No" & Data$YearsAtCompany == 5)
collaborateurs_5ans    #Nous avons 175 collaborateurs encore présents qui on une ancienneté de 5 ans

#1.8 Parmi les collaborateurs ayant quitté l’entreprise, combien ont-ils 5 ans  d’ancienneté au poste ?

collaborateurs_quitte_5ans <- sum(Data$Attrition == "Yes" & Data$YearsAtCompany == 5)
collaborateurs_quitte_5ans  #Nous avons 21 collaboteurs qui ont quitté et qui ont une anciennéte au poste de 5ans

#1.9 Quelle est la rémunération mensuelle moyenne des collaborateurs ayant quitté  l’entreprise ?

collaborateurs_nopresent <- Data[Data$Attrition == "Yes", ]
salaire_moyen_nopresent <- mean(collaborateurs_nopresent$MonthlyIncome)
salaire_moyen_nopresent  #le salaire moyen est de 4787 euros

#1.10 Quel est le nombre moyen d’années d’ancienneté au sein de l’entreprise des  collaborateurs l’ayant quittée ?

collab_partis <- Data[Data$Attrition == "Yes", ]
moyenne_anci_quitte <- mean(collab_partis$YearsAtCompany)
moyenne_anci_quitte  #le nombre moyen d'ancienneté des collaborateurs ayant quittés est de 5 ans

#2. Répartition du fichier de données en deux dataframes : Train et Test

Division= sample.split(Data$Attrition, SplitRatio = 0.8) 
print(Division)

train= subset(Data, Division == TRUE) 
test= subset(Data, Division == FALSE)

#2.1 Quel est le nombre de lignes de la dataframe « Train » ?

# notre dataframe train contient 1176 lignes

#2.2 Quel est le nombre de lignes de la dataframe « Test » ?

#Notre dataframe test contient 294 lignes


#3. Synthèse d’un modèle d’analyse prédictive : 

#recodage de notre variable Attrition (transformation en une variable binaire)

train$Attrition <- factor(train$Attrition, levels = c("No", "Yes"), labels = c(0, 1))
str(train)

#CONSTRUCTION DU MODELE

Modele <- glm(Attrition ~ DistanceFromHome + Education + JobInvolvement + MonthlyIncome, data = train, family = "binomial")
summary(Modele) 

#3.1 Justifier le choix d’un modèle de régression logistique pour analyser le turnover.

#Nous avons choisir un modèle de regression logistique parce que notre variable
#dépendante (Attrition) est une variable binaire.

#3.2 Quel est la valeur de probabilité (p-value) de la variable « Education » ?

#le p-value de notre variable éducation est de 0.55513

#3.3 Que peut-on dire de l’effet de la variable « Education » sur la variable « Attrition » ? 

#etant donne que le p-value de notre variable est >0, nous pouvons en déduire que la variable
#education n'est pas statistiquement significative.  

#3.4 Dans ce modèle, quel est le coefficient de la variable « DistanceFromHome » ?

#le coefficient de la variable DistanceFromHome est de 0.02495. Ce qui veut dire qu'une augmentation de cette variable
#entrainera une augmentation de 0.02495 de la variable Attrition.


#PARTIE II : Validation du Modèle d’Analyse Prédictive

#1. Calculs Comparatifs

#1.1 Quel est le nombre de salariés dont la rémunération est supérieure à la moyenne ?

Lambda1 = as.numeric(Data$MonthlyIncome > mean(Data$MonthlyIncome))
table(Lambda1) #Nous avons 493 salariés qui ont une rémunération supérieure à la moyenne

#1.2 Quelle est le nombre de salariés dont la distance travail-domicile est supérieure à  la moyenne ?

Lambda2 = as.numeric(Data$DistanceFromHome > mean(Data$DistanceFromHome))

table(Lambda2) #Nous avons 530 salariés dont la distance travail-domicile est supérieure à  la moyenne ?


#1.3 Parmi les salariés dont la distance travail-domicile est supérieure à la moyenne,  combien ont-ils quitté l’entreprise ?

Sal_quitte_DTD <- sum(Data$Attrition == "Yes" & Lambda2 == 1)
print(Sal_quitte_DTD) # Nous avons 104 salariés dont la distance travail-domicile est supérieure à la moyenne

#2. Validation du modèle

#2.1 Matrice de confusion

PredictTrain = predict(Modele, type = "response")
table(train$Attrition, PredictTrain>=0.5)


#2.2 Calculer manuellement l’exactitude du modèle sur la dataframe « Train »

#l'exactitude est de 0,840

#2.3 Matrice de confusion (test)

PredictTest = predict(Modele, newdata = test, type = "response")
table(test$Attrition, PredictTest>=0.5)


#2.4 Calculer manuellement l’exactitude du modèle sur la dataframe « Test »

#0,843

#2.5 Comparer les exactitudes sur les dataframes Train et Test. Que peut-on en déduire ?

#L'exactitude de nos deux bases de données sont tres tres proche ce qui nous permet d'en deduire que
#que notre modèle est capable de prédire correctement la classe correcte (attrition ou non) dans environ 84% des cas.










