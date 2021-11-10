library(nnet)
library(broom)
library(ggplot2)
library(GGally)
library(forestmodel)
library(ggeffects)
library(effects)
library(MASS)
library(pROC)
library(cowplot) # graphe combiner
library(forcats) # facteur


############################################# Exercice 1 : Prédiction du diabète

#Importer la base de données
diab <- read.csv("/home/samnouni/Bureau/M2 ISN/methode daprentissage/regression logisatique/diabetes.csv",sep=",", header=TRUE) 


#Neufs variables ont été mesurées ou relevées chez 768 patients

# faire une analyse descriptive
str(diab)
head(diab)
summary(diab)

# Graphe des patients ayont le diabète par age
diab$Outcome=as.factor(diab$Outcome)
ggplot(diab)+aes(x = Outcome, y = Age)+geom_boxplot()+xlab("Outcome")+ylab("Âge")+ggtitle("Répartition par âge selon l'absence ou la présence du diabète")


# Régression logistique
log.diab=glm(Outcome~.,data= diab, family=binomial)  # Premier modele 
summary(log.diab)  #Afficher les résultats du modèle
coef.reg = coef(log.diab)  # pour savoir les beta chapeaux estimés 
ci.reg=confint(log.diab) # intrvalle de confiance pour les coefficients 

exp(coef.reg) # pour récuperer les odds ratio 
exp(ci.reg) # l'intervalle de confiance des odds ratio
cbind(exp(coef.reg),exp(ci.reg))


#Effet d'une augmentation de l'age de 10 
coef.age=coef.reg["Age"] # beta chapeau de la variable age
coef.age
coef.age.plu10=10*coef.age
coef.age.plu10
or.age.plus10=exp(coef.age.plu10) # pour rcuperer odds ratio 
or.age.plus10
ci.age.plus10=10*ci.reg["Age",]
ci.age.plus10
ci.OR.age.plus10=exp(ci.age.plus10)
ci.OR.age.plus10

print(paste0("OR pour l'Age qui augmente de 10 ans :",or.age.plus10,
             " IC à 95% : [",ci.OR.age.plus10[1],";",ci.OR.age.plus10[2]))

# pour choisir le modéle le plus pertinent
stepAIC(log.diab, trace = TRUE, data = diab) 

log.diab.2=glm(Outcome~Pregnancies+Glucose+BMI+DiabetesPedigreeFunction,data=diab,family = binomial) # modéle pertinent

#Visualiser l’effet de chaque variable sur la variable à prédire  
g=plot(ggeffect(log.diab.2)) 
g
plot_grid(g$Pregnancies,g$Glucose,g$BMI,g$DiabetesPedigreeFunction, ncol = 2, nrow = 2)

# Tracer la courbe ROC associée au modèle
pred.diab=predict(log.diab,type = "response") # probabilite d'etre dans la classe 1.
plot(roc(diab$Outcome,pred.diab)) # Courbe ROC 
pred <- prediction(pred.diab, diab$Outcome)
perf <- performance(pred, "auc")
perf@y.values[[1]] # le seuil optimal pour predire Outcome = 1 



###################################### Exercice 2 : Prédiction du mode de contraception



#Importer la base de données
data<- read.table("/home/samnouni/Bureau/M2 ISN/TP R/cmc.data",sep=",", header=TRUE)

# faire une analyse descriptive
str(data)
head(data)
summary(data)


## Variable en factor
data$education=as.factor(data$education)
data$husband_education=as.factor(data$husband_education)
data$religion=as.factor(data$religion)
data$working=as.factor(data$working)
data$husband_occupation=as.factor(data$husband_occupation)
data$standard_of_living=as.factor(data$standard_of_living)
data$media=as.factor(data$media)
data$contraceptive=as.factor(data$contraceptive)

# Recodage de la variable contraceptive
levels(data$contraceptive)=c( "aucun","court_terme","long_terme")

# refaire une analyse descriptive
str(data)
summary(data)

########## Boxplot Age et nb des enfants
box1= ggplot(data, aes(x=contraceptive, y=age, color=contraceptive)) + geom_boxplot() + theme(legend.position = "none")
box2= ggplot(data, aes(x=contraceptive, y=nbchildren, color=contraceptive)) + geom_boxplot() + theme(legend.position = "none")
plot_grid(box1,box2, ncol = 1, nrow = 2)

### Graphe working ##
ggplot(data, aes(x = working , fill =contraceptive)) +geom_bar()  

### Graphe standart_ofçliving ##
ggplot(data, aes(x = standard_of_living , fill =contraceptive)) +geom_bar()  

# Ajuster un modèle de régression logistique multinomial,
log.data= multinom(contraceptive~.,data=data) 
summary(log.data) # Pour afficher les resultats du modele
coef(log.data)  # pour savoir les beta chapeaux estimés 

# l'effet des variables sur la variables à predire 
g=plot(ggeffect(log.data))
plot_grid(g$age,g$nbchildren, ncol = 1, nrow = 2) 
plot_grid(g$education,g$standard_of_living, ncol = 1, nrow = 2)
plot_grid(g$husband_education,g$husband_occupation, ncol = 1, nrow = 2)
plot_grid(g$religion,g$media ,ncol = 1, nrow = 2)
