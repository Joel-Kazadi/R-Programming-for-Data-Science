###########################################
###### FORMATION DATA SCIENCE #############
###########################################

#Lien de telechargement de R : https://cran.r-project.org/bin/windows/base/
#Lien de telechargement de RStudio : https://www.rstudio.com/products/rstudio/download/

### 0. PREAMBULE

### 0.1. Liste des packages necessaires pour la data science
install.packages("ggplot2", dependencies = T)                                   #Installation des packages
install.packages("ggrepel", dependencies = T)
install.packages("dplyr", dependencies = T)
install.packages("plotly", dependencies = T)
install.packages("caret", dependencies = T)
install.packages("olsrr", dependencies = T)
install.packages("FactoMineR", dependencies = T)
install.packages("gridExtra", dependencies = T)
install.packages("randomForest", dependencies = T)
install.packages("Stat2Data", dependencies = T)
install.packages("readxl")
install.packages("readr")
library(ggplot2)                                                                #Chargement des packages
library(ggrepel)
library(dplyr)
library(plotly)
library(caret)
library(olsrr)
library(FactoMineR)
library(gridExtra)
library(randomForest)
library(Stat2Data)
library(readxl)
library(readr)
search()                                                                        #Permet d'afficher la liste des packages charges dans l'environnement
setwd("C://Users//USER//Documents//Data_Science//RStudio")                      #Permet de definir le repertoire de travail
save.image()                                                                    #Permet de sauvegarder l'espace de travail sur disque dur a la fin d'une session,
                                                                                #mais il est preferable de conserver les codes R dans un fichier script
ls()                                                                            #Affiche les objets charges dans l'environnement
rm(list = ls())                                                                 #Nettoie completement l'environnement de travail
quit()                                                                          #Permet de quitter la session de travail

### 0.2. Recherche des fonctions
help("matrix")                                                                  #Affiche l'aide sur la fonction MATRIX
help.start()                                                                    #Lance une page HTML contenant la documentation de R
help.search("matrix")                                                           #Affiche l'aide sur le mot-cle MATRIX

### 1. STRUCTURE DES DONNEES (TYPES D'OBJET)

### 1.1. Creation d'un vecteur

vector1 <- c(1,2,3)                                                             #donnees numeriques
vector2 <- c("Joe","&","Kaz")                                                   #chaine de caracteres
length(vector1)                                                                 #Donne la taille du vecteur

### 1.2. Creation d'une matrice

matrix1 <- matrix(1:9, nrow=3, ncol=3, byrow = TRUE)                            #le symbole d'assignation est obtenu par la combinaison des touches "Alt" + "-"
print(matrix1)
identite <- diag(x = 1, nrow = 3)                                               #cree une matrice identite de dimension 3
print(identite)
matrix1%*%identite                                                              #produit matriciel
rowSums(matrix1)                                                                #Fait la somme par ligne
colSums(matrix1)                                                                #Fait la somme par colonne
rowMeans(matrix1)                                                               #Fait la moyenne par ligne
colMeans(matrix1)                                                               #Fait la moyenne par colonne
t(matrix1)                                                                      #transposition
det(matrix1)                                                                    #calcul du determinant
solve(matrix1)                                                                  #calcul de l'inverse
eigen(matrix1)                                                                  #valeurs et vecteurs propres
chol(matrix1)                                                                   #factorisation de Cholesky

matrix2 <- matrix(c("A","B","C","d","e","f"), nrow=3, ncol=2, byrow = FALSE)
dim(matrix2)                                                                    #Donne la dimension de la matrice
rownames(matrix2)=c("L1", "L2", "L3")                                           #Permet de renommer les lignes (en dehors de la fonction MATRIX)
colnames(matrix2)=c("C1", "C2")                                                 #Permet de renommer les colonnes (en dehors de la fonction MATRIX)
matrix2
matrix2[c(2,3),1]                                                               #Extrait les elements des lignes 2 & 3 et de la colonne 1 (sur base des index)
matrix2[c("L2", "L3"),"C1"]                                                     #Extrait les elements des lignes 2 & 3 et de la colonne 1 (sur base des etiquettes)
matrix2[c(1:3),2]=c("D","E","F")                                                #Modifie les entrees de la ligne 1 ? la ligne 3 sur la colonne 2 dans la matrice
matrix2

library(Stat2Data)
data("HorsePrices")                                                             #Importe le jeu de donnees "HorsePrices" loge dans le package "Stat2Data"
cheval <- as.matrix(HorsePrices[,-c(1,5)])                                      #Retire du precedent jeu de donnees (JD) les colonnes 1 et 5
rownames(cheval)=HorsePrices[,1]                                                #Attribue les noms contenus dans la colonne 1 du JD HorsePrices aux lignes du JD cheval
colnames(cheval)=c("Prix", "Age", "Taille")                                     #Renomme les colonnes du JD cheval
cheval[,3]=cheval[,3]*0.1016                                                    #Convertit les donnees de la colonne 3 de hand en metres (1 h = 0.1016 m)
cheval <- na.omit(cheval)                                                       #Supprime dans le JD les lignes qui ont des valeurs manquantes
cheval
resume1 <- summary(cheval)                                                      #Presente le resume statistique du JD
resume1
cheval[cheval[,1] >= 26840,]                                                    #Liste des chevaux a prix superieur ou egal a la moyenne
dim(cheval[cheval[,1] >= 26840,])                                               #Nombre de chevaux a prix superieur ou egal a la moyenne

### 1.3. Creation d'un array

#Le vecteur est un objet de dimension=1, la matrice de dim=2, et l'array de dim=3

V <- vector(mode = "integer", length = 24)                                      #cree un vecteur nul de taille 24
A <- array(data = V, dim = c(3,4,2))                                            #cree un array compose de 2 matrices de dimension 3x4
A

### 1.4. Creation d'une dataframe (jeu des donnees)

#Dans les vecteurs ou les matrices, les elements doivent etre de meme nature (homogeneite).
#Les dataframe permettent d'avoir des donnees de nature variee (numerique et caractere a la fois).

data <- data.frame(Econometrie=c(18,16,17,18), Statistique=c(14,13,15,19), Niveau=c("Bon","Moyen",NA,"Excellent"),
                   row.names=c("Kadima","Kazadi","Nsamba","Malu"))
data
is.na(data)                                                                     #permet de visualiser les valeurs manquantes (NA)

### 2. IMPORTATION ET EXPORTATION DES DONNEES

### 2.1. Importation

install.packages("readxl")
install.packages("gdata")
library(readxl)
library(gdata)
Data_frame <- read_excel("C://Users//USER//Documents//Data_Science//RStudio//Data_frame.xlsx",
                         sheet = "time_series")                                 #Importe un fichier .xlsx
View(Data_frame)
attach(Data_frame)

install.packages("readr")
library(readr)
data_iris <- read_csv("C://Users//USER//Documents//Data_Science//RStudio//iris.csv",
                      col_names = T)                                            #Importe un fichier .csv
data_iris <- as.data.frame(data_iris[,-1])
View(data_iris)

link <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00611/accelerometer.csv"
data_online <- read.csv(file = link, header = TRUE, sep = ",", dec = ".")       #Importe un JD en ligne

  #Les detais sur ce JD sont fournis dans le lien ci-dessous :
  #https://archive.ics.uci.edu/ml/datasets/Accelerometer

### 2.2. Exportation

data
library(readxl)
write_excel_csv(data, "C://Users//USER//Documents//Data_Science//RStudio//data_grades.xlsx", col_names = T)              #Exporte un fichier .xlsx
library(readr)
write_csv(data, "C://Users//USER//Documents//Data_Science//RStudio//data_grades.csv", col_names = T)                     #Exporte un fichier .csv
save(data, file = "C://Users//USER//Documents//Data_Science//RStudio//data_grades.RData")                                #Enregistre sous le format R (recommande pour les grands jeu de donnees)
load("C:/Users/USER/Documents/Data_Science/RStudio/data_grades.RData")                                                   #Charge dans l'environnement l'objet R precedemment enregistre

### 3. MANIPULATION AVANCEE DES DATAFRAME

head(Data_frame)                                                                #Permet de visualiser les 6 premieres lignes du jeu de donnees (JD)
head(Data_frame, n = 10L)                                                       #Permet de visualiser les 10 premieres lignes du JD
head(Data_frame, c(10L, 4L))                                                    #Permet de visualiser les 10 premieres lignes et les 4 premieres colonnes du JD
tail(Data_frame)                                                                #Permet de visualiser les 6 dernieres lignes du JD
tail(Data_frame, n = 10L)                                                       #Permet de visualiser les 10 dernieres lignes du JD
tail(Data_frame, c(10L, 2L))                                                    #Permet de visualiser les 10 dernieres lignes et les 2 dernieres colonnes du JD

### 3.1. Creation d'une nouvelle colonne

Data_frame$gain <- Data_frame$ca - Data_frame$charges                           #Permet de creer la variable Benefice (gain)
Data_frame                                                                      #Visualiser la nouvelle variable dans le dataframe

### 3.2. Subdivision du jeu de donnees

data1 <- subset(Data_frame, date >= 2000)                                       #Extrait les donnees pour lesquelles l'annee est superieure ou egale a 2000
data1                                                                           #Visualiser la sous-dataframe
data2 <- subset(Data_frame, date >= 2000 & effectif == 1, select = c(date, ca, charges))        #Extrait les donnees pour lesquelles l'annee est superieure ou egale a 2000 et l'effectif est egal a 1, en ne preservant que les colonnes date, ca et charges
data2

data3 <- Data_frame[Data_frame$statut == 3,]                                    #Extrait les donnees pour lesquelles le statut est egal a la modalita 3
data3
data4 <- Data_frame[which(Data_frame$statut == 3 & Data_frame$utility == 6),]   #Extrait les donnees pour lesquelles le statut est egal 3 et l'utillite est egale a 6
data4

### 3.3. Fonctions apply et tapply

moyenne <- apply(X = Data_frame, MARGIN = 2, FUN = "mean")                      #Permet de calculer la moyenne de toutes les variables du jeu de donnees
moyenne                                                                         #le chiffre "2" dans l'argument MARGIN indique que la fonction s'applique par colonne (1, pour les lignes)

mediane <- tapply(X=Data_frame$gain, INDEX=Data_frame$statut, FUN="median")     #Permet de calculer le benefice median selon le type de statut
mediane

### 3.4. Fonctions by et aggregate

View(data_iris)
correlation <- by(data_iris[,-5], data_iris$Species, cor)                       #Calcule le coefficient de correlation des variables dans le JD deux ? deux par esp?ces
correlation

resume2 <- aggregate(x=data_iris, by=as.data.frame(data_iris$Species), FUN = "summary")             #Effectue le resume statistique dans le JD par especes
resume2

### 3.5. Package Dplyr

library(dplyr)
data_iris
iris_data <- as_tibble(data_iris)                                               #le tibble est une classe de donnees qui constitue une amelioration des dataframe
class(iris_data)                                                                #la fonction CLASS permet de connaitre la classe d'un objet (ici, d'un JD)
iris_data$Species <- as.factor(iris_data$Species)                               #permet de convertir la variable SPECIES en variable categorielle
str(iris_data)                                                                  #la fonction STR permet de connaitre la nature des variables dans un JD

select(iris_data, Sepal.Length, Petal.Width, Species)                           #Selectionne quelques colonnes dans le tibble
select(iris_data, Sepal.Length : Petal.Length)                                  #Selectionne des colonnes qui se succedent
select(iris_data, -Species)                                                     #Retire la colonne precedee du signe negatif
select(iris_data, starts_with("Petal"))                                         #Selectionne les colonnes qui commencent par le mot-cle
select(iris_data, -ends_with("Width"))                                          #Retire les colonnes qui se terminent par le mot-cle
select(iris_data, contains("S"))                                                #Selectionne les colonnes qui contiennent le mot-cle

filter(iris_data, Sepal.Length >= 6, between(Petal.Width, 1, 2), Species != "virginica")          #Filtre le tibble en fonction des criteres indiques
filter(iris_data, Species %in% c("setosa", "versicolor"))                       #Ne retient que les especes setosa et versicolor
filter(iris_data, Species == "setosa" | Species == "versicolor")                #Autre maniere d'ecrire la commande precedente
filter_all(select(iris_data, -Species), any_vars(. > 5))                        #Filtre toutes les colonnes dans le tibble (extrait de la variable Species) pour lesquelles au moins une valeur est sup?rieure ? 5
filter_all(select(iris_data, -Species), all_vars(. > 2))                        #Filtre toutes les colonnes dans le tibble (extrait de la variable Species) pour lesquelles toutes les valeurs sont sup?rieures ? 2

arrange(iris_data, Sepal.Length)                                                #Ordonne les lignes du tibble par ordre croissant selon la variable indiquee
arrange(iris_data, desc(Sepal.Length))                                          #Ordonne les lignes du tibble par ordre d?croissant selon la variable indiquee

mutate(iris_data, somme_petal=Petal.Length+Petal.Width)                         #Ajoute une nouvelle variable dans le tibble
mutate(iris_data, Species = NULL)                                               #Supprime du tibble la variable indiquee

iris_data %>%
  select(-Species) %>%
  filter_all(all_vars(. > 2)) %>%
  arrange(Sepal.Length)                                                         #L'operateur pipe (%>%) permet l'utilisation enchainee de plusieurs verbes DPLYR

iris_data %>%
  group_by(Species) %>%
  mutate(petal_ratio=Petal.Length/Petal.Width,
         sepal_ratio=Sepal.Length/Sepal.Width) %>%
  summarise_each(funs(mean, sd), petal_ratio, sepal_ratio)                      #group_by permet de ranger le tibble en sous-categories

### 3.6. Utilisation des facteurs

Data_frame2 <- read_excel("C://Users//USER//Documents//Data_Science//RStudio//Data_frame.xlsx", sheet = "cross_section")
View(Data_frame2)                                                               #Dans ce JD, les variables sexe et statut peuvent etre considerees comme des facteurs, car elles contiennent un nombre fini de modalites
str(Data_frame2)                                                                #Permet de voir la nature(format) des variables dans le JD (sexe et statut sont des chaines de caracteres, et non pas des facteurs)
Data_frame2$Sexe <- as.factor(Data_frame2$Sexe)                                 #Permet de convertir la variable sexe en variable categorielle (facteur)
Data_frame2$Statut <- as.factor(Data_frame2$Statut)                             #Permet de convertir la variable statut en facteur
str(Data_frame2)                                                                #La conversion a reussi
summary(Data_frame2)                                                            #Le resume statistique differe selon qu'il s'agit d'une variable numerique (quantitative) ou d'un facteur (qualitative/categorielle)
table(Data_frame2[,c("Statut", "Sexe")])                                        #construit un tableau de contingence des effectifs croises par modalites de deux variables categorielles d'un dataframe

### 3.7 Concatenation des dataframe et des tibbles

library(readr)
data_iris <- read_csv("C://Users//USER//Documents//Data_Science//RStudio//iris.csv", col_names = T)
data_iris <- as.data.frame(data_iris[,-1])
dim(data_iris)
rownames(data_iris)=paste("iris_", rownames(data_iris), sep = "")               #Colle du texte dans les noms des lignes du JD
price <- data.frame(Prices=sample(1000:5000, size = 150))                       #Simule une suite aleatoire de 150 prix compris entre 1000 et 5000
rownames(price)=paste("iris_", rownames(price), sep = "")                       #Renomme identiquement les lignes que le JD precedent
data_iris_full <- merge.data.frame(data_iris, price, by = "row.names")          #Combine les deux JDs en prenant les noms de lignes (element commun) comme reference
data_iris_full <- data_iris_full[,-1]                                           #Retire la colonne 1 du JD combine
View(data_iris_full)

v1 <- sample(x = 1:5, size = 4, replace = T)
v2 <- sample(x = 1:5, size = 4, replace = T)
tbl1 <- tibble(v1, v2)                                                          #creation du premier tibble
v3 <- sample(x = c(TRUE, FALSE), size = 4, replace = T)
v4 <- sample(x = c("Blue", "Red"), size = 4, replace = T)
tbl2 <- tibble(v3, v4)                                                          #creation du deuxieme tibble
Tbl_1 <- bind_cols(tbl1, tbl2)                                                  #concatenation par colonne des deux tibbles (les tibbles concernent les memes individus)

v1 <- sample(x = 5:9, size = 4, replace = T)
v2 <- sample(x = 5:9, size = 4, replace = T)
v3 <- sample(x = c(TRUE, FALSE), size = 4, replace = T)
v4 <- sample(x = c("Green", "Yellow"), size = 4, replace = T)
tbl3 <- tibble(v1, v2, v3, v4)
Tbl_2 <- bind_rows(Tbl_1, tbl3)                                                 #concatenation par ligne des deux tibbles (les tibbles concernent les memes variables)

tbl4 <- tibble(v1, v2, v3)
tbl5 <- tibble(v1, v2, v4)
Tbl_3 <- left_join(tbl4, tbl5)                                                  #jointure de 2 tibbles en evitant la redondance des colonnes communes aux 2 tibbles

### 4. GRAPHIQUES

### 4.1. Graphiques simples

par(mfrow = c(1,2))                                                             #Partage l'ecran en une ligne et deux colonnes
chart1 <- plot(Data_frame$utility, Data_frame$gain, main = "Nuage de points", xlab = "Degre de satisfaction", ylab = "Benefice annuel",
               ylim = c(min(Data_frame$gain),max(Data_frame$gain)), col="orange")
abline(a = 3750, b = -900, col = "lightblue")                                   #ajoute une droite de coordonnees "a" et "b" dans le CHART 1
chart1
statut_freq <- table(Data_frame$statut)                                         #Cree une table de frequence en fonction du statut
statut_freq
chart2 <- pie(statut_freq, labels = c("Type A", "Type B", "Type C", "Type D"), main = "Camember", col = c("red", "yellow", "blue", "green"))                        #Genere un diagramme circulaire
chart2
chart3 <- hist(data1$statut, main = "Histogramme", freq = FALSE, cex.main = 1.5, cex.axis=0.8, cex.lab=1.2, xlab = "Statut", ylab = "Frequences", breaks = 4, ylim = c(0,1))
chart3

par(mfrow = c(1,1))                                                             #Retablir la mise en page par defaut
layout(matrix(c(1,2,3,3), nrow = 2, ncol = 2, byrow = TRUE))                    #Permet d'appreter la mise en page de sorte qu'un graphique occupe deux blocs
chart1 <- plot(Data_frame$utility, Data_frame$gain, main = "Nuage de points", xlab = "Degre de satisfaction", ylab = "Benefice annuel",
               ylim = c(min(Data_frame$gain),max(Data_frame$gain)), pch = 5)    #L'option pch permet de changer le type de points dans le scatterplot
chart1
statut_freq <- table(Data_frame$statut)                                         #Cree une table de frequence en fonction du statut
statut_freq
chart2 <- pie(statut_freq, labels = c("Type A", "Type B", "Type C", "Type D"), main = "Camember", col = c("red", "yellow", "blue", "green"))                        #Genere un diagramme circulaire
chart2
chart3 <- hist(data1$statut, main = "Histogramme", freq = FALSE, cex.main = 1.5, cex.axis=0.8, cex.lab=1.2, xlab = "Statut", ylab = "Fr?quences", breaks = 4, ylim = c(0,1))
chart3

library(readxl)
Data_frame2 <- read_excel("C://Users//USER//Documents//Data_Science//RStudio//Data_frame.xlsx", sheet = "cross_section")
View(Data_frame2)
attach(Data_frame2)
install.packages("ggplot2")                                                     #La commande ggplot permet de tracer des graphiques visuellement plus beaux.
library(ggplot2)
scatter1 <- ggplot(Data_frame2, aes(x=Taille, y=Poids))+
  geom_point(data = Data_frame2, aes(Taille, Poids), colour="red", size=3)      #Genere un nuage de points
scatter1

scatter2 <- ggplot(Data_frame2, aes(x=Taille, y=Poids))+
  geom_point(data = Data_frame2, aes(Taille, Poids, color=Sexe, shape=Sexe), size=2)+   #shape permet de modifier le type de points selon le Sexe
  geom_smooth(formule = Poids ~ Taille, method = lm, se = FALSE)+                       #ajoute la droite de regression lineaire
  theme(axis.line = element_line(size = 0.75))+xlim(1,2)                                #permet de tracer les lignes des axes
scatter2

hist1 <- ggplot(Data_frame2, aes(Statut, fill = Sexe))+
  geom_bar(data = Data_frame2, aes(Statut), size=3)+                            #Genere un histogramme des statuts par sexe (en ligne)
  facet_grid(Sexe~.)+
  scale_fill_manual(values = c("red", "blue"))
hist1

hist2 <- ggplot(Data_frame2, aes(Statut, fill = Sexe))+
  geom_bar(data = Data_frame2, aes(Statut), size=3)+                            #Genere un histogramme des statuts par sexe (en colonne)
  facet_grid(.~Sexe)+
  scale_fill_manual(values = c("red", "blue"))
hist2

scatter3 <- ggplot(Data_frame2, aes(Taille, Poids))+
  geom_point(data = Data_frame2, aes(Taille, Poids), colour="red", size=3)+     #Genere un nuage de points par statut (en ligne) et par sexe (en colonne)
  facet_grid(Statut~Sexe)
scatter3

### 4.2. Graphiques avancees

scatter4 <- ggplot(Data_frame2, aes(Taille, Poids, colour = Sexe))+
  geom_point(data = Data_frame2, aes(Taille, Poids), size=3)+                   #Permet de differencier les points du nuage selon le sexe
  facet_grid(.~Statut)+
  scale_fill_manual(values = c("red", "blue"))+
  theme(strip.text = element_text(face = "italic"))                             #pour mettre les titres des sous-blocs en italiques
scatter4

scatter5 <- ggplot(Data_frame2, aes(Taille, Poids, colour = Sexe))+
  geom_point(data = Data_frame2, aes(Taille, Poids), size=3)+                   #Permet de differencier les points du nuage selon le sexe
  facet_grid(.~Sexe)+theme_minimal()+
  scale_fill_manual(values = c("red", "blue"))+
  theme(strip.background = element_rect(linetype = "solid"))+                   #mettre en forme les rectangles des sous-blocs en trait plein (continu)
  theme(axis.line = element_line(size = 1))
scatter5

hist3 <- ggplot(Data_frame2, aes(Statut, fill = Sexe))+
  geom_bar(data = Data_frame2, aes(Statut), size=1, colour = "black")+                            #Les arguments supplementaires utilises pour l'histogramme sont aussi compatibles avec le nuage de points
  facet_grid(.~Sexe)+
  scale_fill_manual(values = c("red", "blue"))+
  labs(title = "Statut des individus selon le Sexe", x="STATUT", y="NOMBRE")+
  theme_bw()+                                                                   #theme permet de modifier la couleur d'arriere-plan du graphique (en tapant theme_, plusieurs propositions apparaissent)
  theme(plot.title = element_text(hjust = 0.5))+                                #permet de centrer le titre du graphique
  theme(legend.position = "bottom")
hist3

tsline1 <- ggplot(Data_frame2, aes(x=individu, y=Poids, color=Poids))+
  geom_line()+theme_minimal()+
  scale_color_gradient(low="gray80", high="gray10")+                                                       #permet de generer une echelle croissante de couleurs
  theme(legend.title = element_text(face = "bold"), legend.text = element_text(face = "italic"))+          #permet de mettre le titre de la legende en gras et le texte de la legende en italique
  theme(legend.background = element_rect(fill = "azure", linetype = "dotted", size = 1.2))                 #permet de modifier l'arriere-plan de la l?gende
tsline1

tsline2 <- ggplot(Data_frame2, aes(x=individu, y=Poids, color=Poids))+
  geom_line()+theme_minimal()+
  scale_color_gradient(low="gray80", high="gray10")+
  theme(legend.title = element_text(face = "bold"), legend.text = element_text(face = "italic"))+
  theme(legend.background = element_rect(fill = "azure", linetype = "solid", size = 1))+
  theme(axis.line = element_line(size = 0.5))+
  xlim(0, 15)+
  annotate("text", x = c(2.5, 6), y = 78, colour = "steelblue", size = 2.2, fontface = "italic",
           label = c("Poids minimum = 60 Kg", "Poids maximum = 80 Kg"))+                                   #annotate permet d'ajouter des annotations sur le graphique
  annotate("rect", xmin = 5.5, xmax = 6.5, ymin = 59, ymax = 61, alpha=0.2, colour = "steelblue2", size = 1)+
  annotate("segment", x=6, xend = 5.75, y=60, yend = 69)+
  annotate("text", x=6, y=70, colour = "steelblue4", size = 3, fontface = "bold",
           label = "Le plus petit poids")
tsline2

boxplot1 <- ggplot(Data_frame2, aes(x = Sexe, y = Taille, fill = Sexe))+
  geom_boxplot()+labs(title = "Boîte à moustache")+theme_bw()+ylim(1, 2)+
  theme(plot.title = element_text(face = "bold", hjust = 0.5))+
  theme(legend.title = element_text(face = "italic"))                           #genere une boite a moustache
boxplot1

jpeg("C://Users//USER//Documents//Data_Science//RStudio//boxplot.jpg")          #enregistre le graphique en format photo (le format pdf aussi est possible)
boxplot1
dev.off()

### 4.3. Cartes choroplethes

library(dplyr)
library(ggplot2)
library(plotly)
library(readxl)
library(readr)

data.africa <- read_excel("C://Users//USER//Documents//Data_Science//RStudio//maps.xlsx", sheet = "AfricaIncome")
View(data.africa)
mapdata <- map_data("world")
View(mapdata)
africa.income <- inner_join(mapdata, data.africa, by = "region")                #concatenation des deux JD en eliminant les lignes contenant des NAs
View(africa.income)

map <- ggplot(africa.income, aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = Income), color = "black")+theme_bw()+
  labs(x = "longitude", y = "latitude")+
  scale_fill_gradient(name="Income", low="red", high="green", na.value="gray")+
  theme(legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "italic"))
ggplotly(map)

write_csv(mapdata, "C://Users//USER//Documents//Data_Science//RStudio//choropleth.csv", col_names = T)

### 5. CREATION DES FONCTIONS ET CONDITIONS

### 5.1. Operateurs logiques

objet_1 <- 1==1 & 2==1                                                          #Conjonction
objet_2 <- 1==1 | 2==1                                                          #Disjonction inclusive
cbind(objet_1, objet_2)                                                         #Empilement des objets cote a cote

objet_3 <- objet_1 != objet_2                                                   #Inegalite
objet_4 <- xor(6<=6, 6>5)                                                       #Disjonction exclusive
rbind(objet_3, objet_4)                                                         #Empilement des objets l'un en-dessous de l'autre

### 5.2. Conditions et Boucles

data
new_data <- data[,-3]
dim(new_data)
new_data$Moyenne <- round(rowMeans(new_data), digits = 2)                       #Ajoute une colonne des moyennes pour les trois cours par etudiant
                                                                                #La fonction rowmeans calcule la moyenne par ligne (arrondissement a 2 chiffres apres la virgule)
new_data$Decision <- ifelse(new_data$Moyenne > 15, "Reussite", "Echec")         #Ajoute la colonne de decision dans le dataframe
new_data
print(data[3,])                                                                 #Affiche uniquement la troisieme ligne du jeu de donnees
print(data[c(3,4),])                                                            #Affiche les resultats des deux dernieres lignes
print(data[,c(1:3)])                                                            #Isole uniquement les cotes des etudiants pour tous les cours
summary(new_data$Moyenne)                                                       #Donne le resume statistique du vecteur des cotes moyennes

vector3 <- seq(from = 10, to = 50, by = 5)                                      #Cree un vecteur (VECTOR3) de 10 a 50, par pas de 5
conclusion <- rep(NA, length(vector3))                                          #Cree un vecteur (CONCLUSION) contenant de valeurs manquantes de meme dimension que le vecteur VECTOR3
for (i in 1:length(vector3)) {
  if (vector3[i] <= quantile(vector3, probs = 0.25)){
    conclusion[i] <- "First part"
  }
  else{
    conclusion[i] <- "Second part"
  }
}                                                                               #Affiche "First part" dans le vecteur CONCLUSION si le i-eme element de VECTOR3 est inf?rieur ou ?gal au 1er quartile de ce vecteur,
                                                                                #et "Second Part" sinon
cbind(vector3, conclusion)                                                      #La commande cbind permet de superposer deux vecteurs de meme dimension

### 5.3. Creation des fonctions

#Exemple 1
data
attach(data)
sd(data$Econometrie)                                                            #Permet de calculer l'ecart-type des points dans un seul cours, a savoir Econometrie
sd1 <- function(cours1){sd(cours1)}                                             #Creation de la fonction "sd1" a appliquer sur le premier cours
sd2 <- function(cours2){sd(cours2)}                                             #Creation de la fonction "sd2" a appliquer sur le deuxieme cours

ecart_type <- matrix(c("Econometrie", "Statistique", sd1(data$Econometrie), sd2(data$Statistique)),
                     nrow=2, ncol=2, byrow = TRUE)                              #Obtention des ecarts-types pour les 2 cours simultanement
ecart_type

#Exemple 2
comparaison <- function(a,b) {
  if (is.numeric(c(a,b))) {
    if (a<b)
      return(b)
    if (a>b)
      return(a)
    else
      warning("Valeurs identiques")
  }
  else
    stop("Au moins un des deux arguments n'est pas du type NUMERIQUE")
}                                                                               #cette fonction permet de comparer deux nombres
comparaison(a=5, b=1)                                                           #test 1
comparaison(a=0, b=2)                                                           #test 2
comparaison(a=3, b=3)                                                           #test 3
comparaison(a="x", b=4)                                                         #test 4

#Exemple 3
second <- function(a,b,c){
  d <- (b)^2 - 4*a*c
  if (d >= 0)  { 
    roots <- c((-b+sqrt(d))/(2*a), (-b-sqrt(d))/(2*a))
  } else {
    roots <- c("Racine Imaginaire", "Racine Imaginaire")      
  }
  return(c(roots,d))
}                                                                               #cette fonction calcule les racines d'un polynome de second degre
second(1,1,1)                                                                   #test 1
second(-1,1,30)                                                                 #test 2
second(1,-5,0)                                                                  #test 3
second(-2,16,-32)                                                               #test 4

### 5.4. Fonctions, Conditions et Boucles ensemble

#Exemple 1
matrix3 <- matrix(rep(0,25), nrow = 5)                                          #Permet de creer une matrice carree d'ordre 5 composee des zeros
ma_fonction <- function(matrice){
  for (i in 1:5) {
    for (j in i:5) {
      if (i<j & j == i+1) {
        matrice[i,j] <- -1
      } else if (i==j) {
        matrice[i,j] <- 1
      } else
        matrice[i,j] <- 0
    }
  }
  return(matrice)                                                               #La fonction creee permet de retourner le resultat final a la fin de la boucle
}
ma_fonction(matrix3)

#Exemple 2
library(readr)
data_iris <- read_csv("C://Users//USER//Documents//Data_Science//RStudio//iris.csv", col_names = T)
data_iris <- as.data.frame(data_iris[,-1])
View(data_iris)
mean_data_frame <- function(dataframe){
  iris_species_setosa = subset(data_iris, data_iris$Species == "setosa")
  iris_species_versicolor = subset(data_iris, data_iris$Species == "versicolor")
  iris_species_virgina = subset(data_iris, data_iris$Species == "virginica")
  mean_setosa <- colMeans(iris_species_setosa[,-5])
  mean_versicolor <- colMeans(iris_species_versicolor[,-5])
  mean_virgina <- colMeans(iris_species_virgina[,-5])
  result <- data.frame(Setosa = mean_setosa, Versicolor = mean_versicolor, Virginica = mean_virgina)
  return(result)
}
mean_data_frame(data_iris)                                                      #Cette fonction permet de calculer la moyenne par espece pour chaque variable

### 5.5. Boucles, Conditions et Compteurs

#Exemple 1
head(data_iris)
counter_setosa=0                                                                #Initialisation du compteur pour l'espece SETOSA
counter_versicolor=0                                                            #Initialisation du compteur pour l'espece VERSICOLOR
counter_virginica=0                                                             #Initialisation du compteur pour l'espece VIRGINICA
for (species in data_iris$Species) {
  if (species == "setosa"){
    counter_setosa=counter_setosa+1
  } else if (species == "versicolor"){
    counter_versicolor=counter_versicolor+1
    }
  else {
    counter_virginica=counter_virginica+1
  }
}                                                                               #Cette boucle permet de compter le nombre de fleurs par espece
counting <- rbind(counter_setosa, counter_versicolor, counter_virginica)        
counting                                                                        #Affiche les resultats de la boucle

#Exemple 2
head(data_iris)
counter=0
for (rangee in 1:dim(data_iris)[1]) {
  individu = data_iris[rangee,]
  if (individu$Species == "setosa" & individu$Sepal.Length >= 5){
    counter=counter+1
  }
}                                                                               #Cette boucle permet de parcourir le JD par ligne (Par colonne, il faut remplacer 1 par 2 entre les crochets)
print(counter)                                                                  #Comptage le nombre de fleurs d'espece setosa dont la taille de sepales est d'au moins 5 cm

### 6. ANALYSE STATISTIQUE

### 6.1. Test de Khi-carre

Data_frame2                                                                     #Reprenons notre jeu de donnees CROSS_SECTION
attach(Data_frame2)                                                             #Question : La taille d'une personne, est-elle liee a son poids?
pivot1 <- table(Taille, Poids)                                                  #Cree un tableau de contigence reprenant les deux variables
pivot1
khi_square1 <- chisq.test(pivot1, correct = TRUE)                               #Test d'independance entre les deux variables (Taille et Poids)
khi_square1
khi_square2 <- chisq.test(pivot1, simulate.p.value = TRUE, B = 1000)            #Test avec p-values calculees sur base des simulations de Monte Carlo, avec 1000 replications
khi_square2

### 6.2. Test ANOVA

#Exemple 1 (ANOVA avec un seul facteur)
library(dplyr)
library(ggplot2)
economie <- sample(50:99, size = 20, replace = TRUE)                            #Genere un echantillon aleatoire de 20 elements entre 50 et 99
litterature <- sample(50:99, size = 20, replace = TRUE)
physique <- sample(50:99, size = 20, replace = TRUE)
pivot2 <- data.frame(cbind(economie, litterature, physique))
attach(pivot2)                                                                  #Question : Est-ce que le pourcentage obtenu par un apprenant differe selon la faculte (une seule variable qualitative) ?
pivot2
stacked_pivot <- stack(pivot2)
stacked_pivot <- stacked_pivot %>%
  rename(pourcentage = values, facultes = ind)
ggplot(data = stacked_pivot, aes(facultes, pourcentage))+
  geom_boxplot(data = stacked_pivot, aes(facultes, pourcentage),
               colour = "greenyellow", size  = 1.5)+
  theme_minimal()+theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  scale_y_continuous(limits = c(50,100))+
  labs(title = "Boites a moustache", x = "GROUPES", y = "POURCENTAGE")
anova <- aov(data=stacked_pivot, pourcentage~facultes, projections=T, qr=T)     #Effectue l'analyse de la variance de POURCENTAGE en fonction de FACULTE
TukeyHSD(anova, conf.level = 0.99)                                              #Presente les differences des moyennes entre les groupes (facultes) deux a deux
summary(anova)                                                                  #Renvoie les resultats du test ANOVA : p-value > 0.05,
                                                                                #donc AH0, i.e. le pourcentage ne varie pas inegalement en fonction de l'orientation ou faculte
                                                                                #il y a donc homogeneite des trois sous-populations, car la statistique F est non-significative
write.csv(pivot2, "C://Users//USER//Documents//Data_Science//RStudio//data_anova.csv", row.names = T)       #Exportation du jeu de donnees simule

#Exemple 2 (ANOVA avec plusieurs facteurs)
library(dplyr)
library(ggplot2)
library(faraway)
rats <- force(rats)
rats <- rats %>%
  group_by(poison, treat)
View(rats)                                                                      #ce JD se rapporte au temps de survie de 48 rats en fonction du type de poison et du type de traitement (deux variables qualitatives)
ggplot(data = rats, aes(poison, time))+theme_bw()+
  geom_boxplot(colour = "deepskyblue", outlier.colour = "deepskyblue4")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust=0.5, face="italic", size=12.5))+
  facet_wrap(.~treat, nrow = 2, ncol = 2, dir = "v", scales = "free")+
  labs(title = "Temps de survie par categories de poison",
       subtitle = "selon le type de traitement", x = "", y = "")                #on constate que la distribution du temps de survie varie fortement en fonction du type de poison, et ce, quelque soit le type de traitement
mean.diff_poison <- with(rats, tapply(X = time, INDEX = poison, FUN = mean))    #difference des moyennes de temps de survie en fonction du type de poison
mean.diff_treat <- with(rats, tapply(X = time, INDEX = treat, FUN = mean))      #difference des moyennes de temps de survie en fonction du type de traitement
rats.reg <- lm(formula = time ~ poison * treat, data = rats)                    #les parametres de cette regression peuvent etre interpretes comme des differences croisees de moyenne de la variable d'interet selon les deux facteurs (car il y a presence des termes d'interaction)
summary(rats.reg)                                                               #dans la regression, les premieres modalites de chacun des 2 facteurs ont ete utilisees comme "CELLULE DE REFERENCE" pour eviter la colinearite avec l'intercepte (contrainte d'identifiabilite)
anova(rats.reg)                                                                 #on constate que les 2 facteurs ont separement des effets significatifs sur la moyenne du temps de survie, mais pas conjointement car F devient non-significative (les termes d'interaction sont statistiquement nuls)

### 6.3. Regression lineaire

View(Data_frame2)
row.names(Data_frame2) <- Data_frame2$Nom
attach(Data_frame2)
library(ggplot2)
library(ggrepel)                                                                #ce package permet d'ajuster la position des labels dans le scatterplot
scatter1 <- ggplot(Data_frame2, aes(Taille, Poids, label = Nom))+xlim(1,2)+
  geom_point(data = Data_frame2, aes(Taille, Poids), colour="red", size=3)+
  geom_text_repel()+theme_minimal()+
  geom_smooth(formule = Poids ~ Taille, method = lm, se = TRUE)+
  theme(axis.title=element_text(size=15), axis.text=element_text(size=10))
scatter1
Data_frame2$dummy <- rep(NA, length(Data_frame2$Sexe))                          #Ajoute une colonne dummy dans le JD
for (i in 1:length(Data_frame2$Sexe)) {
  if (Sexe[i] == "F") {
    Data_frame2$dummy[i] <- 0
    }
  else {
    Data_frame2$dummy[i] <- 1
  }
}                                                                               #La boucle convertit les valeurs de la nouvelle colonne ajoutee
Data_frame2$Taille.dummy <- (Data_frame2$Taille)*(Data_frame2$dummy)            #ajoute une variable croisee dans le JD
Data_frame2$ln_Poids <- log(Data_frame2$Poids)                                  #transformation logarithmique de la variable d'interet
cor(select(Data_frame2, ln_Poids, Taille, dummy, Taille.dummy))                 #matrice de correlation des variables
reg1 <- lm(formula=ln_Poids~Taille+Taille.dummy+dummy , data=Data_frame2)       #Regression lineaire multiple (model 1)
reg2 <- lm(formula = ln_Poids ~ Taille, data = Data_frame2)                     #Regression lineaire multiple (model 2)
summary(reg1)                                                                   #Resultats detailles de l'estimation 1
                                                                                #Interpretation : Les coefficients des deux derniers regresseurs sont statistiquement nuls
summary(reg2)                                                                   #Resultats detailles de l'estimation 2
                                                                                #Interpretation : Apres avoir retirer les regresseurs non significatifs, le R2 s'est accru
anova(reg1,reg2)                                                                #test de nullite conjointe des parametres des deux regresseurs qui ont ete retires
                                                                                #Interpretation : La statistique F est non significative (p-value > 0.05). Donc, les deux parametres sont conjointement nuls
aic <- as.data.frame(AIC(reg1, reg2))                                           #comparaison des deux deux modeles sur base du critere de parcimonie d'Akaike
bic <- as.data.frame(BIC(reg1, reg2))                                           #comparaison des deux deux modeles sur base du critere de parcimonie de Schwarz
parsimony_criterion <- merge.data.frame(aic, bic, by = "row.names")             #superposition des resultats des deux tests de parcimonie (on retient le modele qui a la plus faible valeur du critere)

library(olsrr)
all.models <- ols_step_all_possible(reg1)                                       #comparaison de toutes les combinaisons possibles des regresseurs
ols_step_forward_p(model = reg1, penter = 0.05, progress = T, details = T)      #selection du modele optimal sur base de l'algorithme STEPWISE (procedure forward, basee sur la p-value)
                                                                                #Si la p-value d'un regresseur est inferieure au seuil renseigne, ce regresseur est introduit dans la specification
ols_step_backward_p(model = reg1, prem = 0.1, progress = T, details = T)        #selection du modele optimal sur base de l'algorithme STEPWISE (procedure backward, basee sur la p-value)
                                                                                #Si la p-value d'un regresseur est superieure au seuil renseigne, ce regresseur est retire de la specification
ols_step_both_p(model=reg1, penter=0.05, prem=0.1, progress=T, details=T)       #combinaison des deux procedures de l'algorithme (forward et backward)
library(dplyr)
residual_fitted <- bind_cols(tibble(reg2$residuals),tibble(reg2$fitted.values)) #recuperation des series des residus et des valeurs ajustees
plot(reg2, which = c(1,2,3,4,5))                                                #produit 5 graphiques de diagnostic des residus et de detection des valeurs aberrantes (points-leviers)
                                                                                #l'examen de ces graphiques revele que JOEL et VIVIANE sont des individus extremes dans l'echantillon
S <- vcov(reg1)                                                                 #recuperation de la matrice de variance-covariance des parametres estimes
print(diag(S))                                                                  #affiche la diagonale principale de la matrice S, i.e. les variances des parametres
library(RcmdrMisc)                                                              #package renfermant des fonctions pour le diagnostic d'une regression
ols_test_breusch_pagan(reg1)                                                    #test d'heteroscedasticite
ols_test_normality(reg1)                                                        #test de normalite
vif(reg1, type=c("terms", "marginal", "high-order"))                            #variance-inflation factors (pour tester la multicolinearite)
                                                                                #Interpretation : Les VIFs des deux dernieres variables sont tres eleves (>10).
                                                                                #Ils ne sont donc pas orthogonaux au reste des regresseurs (il y a multicolinearite).
                                                                                #La presence de cette multicolinearite implique que ces deux regresseurs soient retires dans la specification.

### 6.4. Regression lineaire generalisee

#Exemple 1 : Donnees binaires (Logit/Probit)
library(readxl)
library(dplyr)
library(ggplot2)
library(caret)
library(margins)
data_bank <- read_excel("C://Users//USER//Documents//Data_Science//RStudio//bank.xlsx")
data_bank <- as_tibble(data_bank) %>%
  rename(outcome = y)                                                           #ce JD se rapporte a 4500 clients d'une banque qui ont participe a une enquete marketing pour la promotion d'un nouveau service bancaire
dummy <- dummyVars(~., data = data_bank)
dummy_data <- predict(dummy, newdata = data_bank)                               #permet de transformer toutes les variables categorielles (factor) en variables binaires (dummy)
dummy_data <- as_tibble(dummy_data)
dummy_data$y <- dummy_data$outcomeyes
data_bank$y <- dummy_data$y
View(data_bank)

plot(select(data_bank, y, duration, balance, age))                              #visualisation graphique des variables
logit <- glm(formula = y ~ duration + balance + age, data = data_bank,
             family = binomial(link = "logit"))
summary(logit)                                                                  #resultats de l'ajustement du modele logit (les coefficients s'interpretent en termes de sens de variation, et non d'ampleur de variation)
odd <- margins(logit)                                                           #les coefficients marginaux moyens qui s'affichent donnent les ampleurs de variation (odd ratio) pour chaque regresseur.
data_bank$predictions <- logit$fitted.values                                    #ajout de la colonne des probabilites predites dans le JD
  #Interpretation : les 3 regresseurs retenus ont des effets positifs et significatifs sur la variable d'interet
  #la duree de l'appel durant la campagne marketing, le solde bancaire et l'age du client sont des facteurs qui ameliorent la probabilite d'acceptation du nouveau service bancaire

ggplot(data = data_bank, aes(x = duration, y = predictions))+
  geom_line(color="darkslategray1")+geom_smooth(se=F, color="darkslategray4")+
  geom_abline(intercept = 0.5, slope = 0, color = "brown1", linetype = "dashed")+
  theme_classic()+labs(title="Logit regression", x="Duration", y="Predictions")+
  annotate("text", x = 500, y = 0.75, label = "inflection point",
           fontface = "italic", family = windowsFont("Times New Roman"))+
  annotate("segment", x = 910, xend = 500, y = 0.5, yend = 0.7)+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20,
                                  family = windowsFont("mono")))                #graphique de la fonction logistique

#Exemple 2 : Donnees de comptage (Poisson/Binomiale negative)
library(dplyr)
library(car)
force(Ornstein)
Ornstein <- as_tibble(Ornstein)
View(Ornstein)                                                                  #ce JD se rapporte a 248 firmes canadiennes ou l'on a observe le nombre de postes d'administrateurs, les actifs financiers et le secteur industriel

summary(Ornstein)
plot(Ornstein)
poisson <- glm(formula = interlocks ~ assets + nation, data = Ornstein,
               family = poisson(link = "log"))
summary(poisson)
  #Interpretation : La valeur des actifs financiers d'une firme a un effet positif et significatif sur le nombre de ses postes d'administrateurs
  #le fait pour une firme d'etre d'un pays de controle different du CANADA reduit le nombre moyen des postes, mais cette baisse est non significative pour la categorie OTH (autres pays)

### 6.5. Regression regularisee 

#Ces modeles de regressions sont utilises lorsque la matrice X'X n'est plus inversible
#Ceci est possible dans 2 cas : (i) il y a plus de regresseurs que d'observations ; (ii) il y a multicolinearite entre les regresseurs

#Le jeu de donnees (JD) utilise dans cette application se rapporte a une enquete sur le logement pour 506 secteurs a Boston (recensement de 1970).
#Il est question de predire le prix median d'une maison (en milliers de USD) en fonction d'un certain nombre de determinants sociologiques et ecologiques.

library(dplyr)
library(readxl)
library(corrplot)
library(mlbench)                                                                #package contenant le JD
data("BostonHousing")                                                           #importation du JD
help("BostonHousing")                                                           #description des variables du JD
View(BostonHousing)                                                             #visualisation du JD
str(BostonHousing)                                                              #nature des variables contenues dans le JD
summary(BostonHousing)                                                          #resume statistique des variables du JD
set.seed(1234)
data <- BostonHousing[,-4] %>%
  sample_n(size = 10, replace = F)                                              #Ce nouveau JD contient 10 observations obtenues par echantillonnage aleatoire
attach(data)
View(data)
summary(data)

corr.matrix <- cor(data[,-13])
corr_matrix <- as.data.frame(corr.matrix)                                       #matrice de correlation des regresseurs
corrplot(corr.matrix, method = "circle", type = "upper", order = "hclust",
         tl.cex = 0.8, tl.col = "blue", tl.srt = 45)                            #diagramme de correlation des regresseurs
library(psych)                                                                  #package utile pour les statistiques descriptives
pairs.panels(data[,-13], cex.cor = 0.8)                                         #cette commande presente des nuages de points bivaries sous la diagonale, des histogrammes sur la diagonale et la correlation de Pearson au-dessus de la diagonale

#Exemple 1 : Regression Ridge

library(MASS)
library(car)
v.lambda <- seq(from = 0, to = 10, by = 0.1)                                    #determination de la plage de variation du shrinkage parameter (parametre de retrecissement)
ols <- lm(formula = medv ~., data = data)                                       #la regression OLS a reduit le nombre de regresseurs en fonction du nombre d'observations
ridge <- lm.ridge(formula = medv ~., data = data, lambda = v.lambda)            #par contre, la regression ridge fait apparaitre les parametres de tous les regresseurs
print(ridge)                                                                    #estimation pour toutes les valeurs du shrinkage parameter

library(ggplot2)
library(broom)
res.ridge <- tidy(x = ridge)
View(res.ridge)                                                                 #resultats des estimations
ggplot(res.ridge, aes(x = lambda, y = estimate, color = term))+
  geom_line()+theme_classic()+
  labs(x = "Lambda", y = "Estimation", title = "Chemin de regularisation")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.background = element_blank(), legend.title = element_blank(),
        legend.text = element_text(face = "italic"))                            #visualisation de l'evolution des parametres estimes en fonction du shrinkage parameter

lambda.crit_ridge <- as.data.frame(glance(x = ridge))
colnames(lambda.crit_ridge) <- c("Hoerl-Kennard-Baldwin", "Lawless-Wang", "Cross-Validation")
rownames(lambda.crit_ridge) <- "Lambda"
View(lambda.crit_ridge)                                                         #valeurs optimales du shrinkage parameter en fonction de 3 criteres de selection
ggplot(res.ridge, aes(x = lambda, y = GCV))+geom_line(color = "dodgerblue")+
  geom_vline(xintercept = lambda.crit_ridge$`Cross-Validation`, col = "red",
             linetype = "dotted", size = 1)+theme_minimal()+
  labs(title = "Determination de la valeur optimale du shrinkage parameter",
       subtitle = "sur base d'une approche de CROSS-VALIDATION", y = "PRESS")+
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "bold.italic", hjust = 0.5))        #visulation graphique de l'un des criteres de selection (validation croisee)

ridge.HKB <- lm.ridge(formula = medv ~., data = data,
                      lambda = lambda.crit_ridge$`Hoerl-Kennard-Baldwin`, model = T)  #estimation definitive sur base du critere HKB
ridge.LW <- lm.ridge(formula = medv ~., data = data,
                      lambda = lambda.crit_ridge$`Lawless-Wang`, model = T)           #estimation definitive sur base du critere LW
ridge.GCV <- lm.ridge(formula = medv ~., data = data,
                      lambda = lambda.crit_ridge$`Cross-Validation`, model = T)       #estimation definitive sur base du critere PRESS
comparison <- as.data.frame(cbind(ridge.HKB$coef, ridge.LW$coef, ridge.GCV$coef, ols$coefficients[-1]))
colnames(comparison) <- c("HKB", "LW", "GCV", "OLS")
View(comparison)                                                                #comparaison des parametres estimes suivante les 3 criteres

#Exemple 2 : Regression Lasso

library(glmnet)                                                                 #la fonction glmnet n'est pas compatible avec la structure des donnees en dataframe, uniquement avec des matrices
explanatory_variables <- data.matrix(data[,-13])
dependent_variable <- data.matrix(data[,13])
lasso <- glmnet(x = explanatory_variables, y = dependent_variable,
                family = "gaussian", alpha = 1)                                 #si alpha = 0, on retrouve la regression ridge
print(lasso)

library(ggplot2)
library(broom)
res.lasso <- tidy(x = lasso)
View(res.lasso)                                                                 #resultats organises des estimations (coefficients du modele selon la valeur du shrinkage parameter)
ggplot(data = res.lasso[-seq(from = 1, to = 100),],
       aes(x = log(lambda), y = estimate, color = term))+
  geom_line()+theme_classic()+
  labs(x = "log(Lambda)", y = "Estimation", title = "Chemin de regularisation")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.background = element_blank(), legend.title = element_blank(),
        legend.text = element_text(face = "italic"))                            #visualisation de l'evolution des parametres estimes (sauf l'intercept) en fonction du shrinkage parameter

lambda.crit_lasso <- cv.glmnet(x = explanatory_variables, y = dependent_variable,
                      type.measure = "mse", family = "gaussian", alpha = 1)     #selection de la veleur optimale du shrinkage parameter par cross-validation (sur base de l'erreur quadratique moyenne comme critere de selection)
plot(lambda.crit_lasso)                                                         #visualition graphique de l'EQM selon les valeurs du shrinkage parameter
MSE <- print(coef(lambda.crit_lasso, s = "lambda.min"))                         #modele definitif pour lequel la valeur optimale du shrinkage parameter minimise l'EQM
MSE <- as.data.frame(as.matrix(MSE))
comparison$MSE <- MSE[-1,]
View(comparison)                                                                #Ridge vs Lasso

#Exemple 3 : Regression Elastic net

library(glmnet)
library(caret)
explanatory_variables <- data.matrix(data[,-13])
dependent_variable <- data.matrix(data[,13])
elnet1 <- glmnet(x = explanatory_variables, y = dependent_variable,
                family = "gaussian", alpha = 0.5)                               #la valeur de alpha est fixee arbitrairement
elnet2 <- train(x = data[,-13], y = data[,13], data = data,
                preProcess = c("center", "scale"), method = "glmnet",
                trControl = trainControl(method = "cv"), tuneLength = 10)       #les valeurs de alpha (mixing proportion) et de lambda (shrinkage parameter) sont determinees en optimisant le critere de selection RMSE (racine carree de l'erreur quadratique moyenne)
print(elnet2)                                                                   #resultats de la regression avec le package caret
plot(elnet2$finalModel, xvar = "lambda", label = T)                             #visualisation de l'evolution des parametres estimes en fonction de lambda
optimal.values <- as.data.frame(elnet2$bestTune)                                #recuperation des valeurs optimales de alpha et lambda

lambda.crit_elnet <- as.data.frame(cbind(elnet2$results$alpha,
                                         elnet2$results$lambda,
                                         elnet2$results$RMSE))
colnames(lambda.crit_elnet) <- c("alpha", "lambda", "RMSE")
View(lambda.crit_elnet)
library(ggplot2)
ggplot(data = lambda.crit_elnet, aes(x = lambda, y = RMSE), fill = alpha)+
  geom_line()+theme_classic()+geom_smooth()+
  labs(title = "Determination de la valeur optimale du shrinkage parameter",
       y = "RMSE", x = "Lambda")+
  geom_vline(xintercept = optimal.values$lambda, col = "red",
             linetype = "dotted", size = 1)+
  theme(plot.title = element_text(hjust = 0.5, face = "bold.italic"))           #visualisation graphique de la RMSE selon les valeurs du shrinkage parameter
plot(elnet2)                                                                    #visualisation graphique de la RMSE selon lambda (regularization parameter) et alpha (mixing percentage)

plot(varImp(elnet2, scale = T), main = "Importance relative des variables")     #Poids relatif de chaque regresseur dans l'ajustement du modele
RMSE <- print(coef(elnet2$finalModel, elnet2$bestTune$lambda))                  #modele definitif pour lequel la valeur optimale du shrinkage parameter minimise la RMSE
RMSE <- as.data.frame(as.matrix(RMSE))
comparison$RMSE <- RMSE[-1,]
View(comparison)                                                                #Lasso vs Elastic net

### 6.6. Series temporelles

library(dplyr)
library(ggplot2)
library(plotly)
library(readr)

time.serie <- read_csv("C://Users//USER//Documents//Data_Science//RStudio//multiTimeline.csv",
                       col_names = T, col_types = cols(Month = col_date(format = "%Y-%m")))
ts.line1 <- ggplot(data = time.serie, aes(x = Month))+
  xlab("Years")+ylab("Interest in research")+
  geom_line(aes(y = `data scientist`, colour = "Data Scientist"), size = 1)+
  geom_line(aes(y = statistician, colour = "Statistician"), size = 1)+
  theme_minimal()+
  theme(legend.title = element_blank(),
        legend.text = element_text(face = "italic", size = 10),
        axis.line = element_line(size = 0.5),
        axis.title = element_text(size = 15))
ggplotly(ts.line1)                                                              #graphique avec 2 series temporelles superposees

    #Les etapes de la modelisation ARIMA, selon l'approche de Box-Jenkins, sont les suivantes :
        #Analyse exploratoire : visualisation graphique, test de racine unitaire, stabilisation de la variance (par transformation logarithmique), etc.
        #Identification du modele : selection des lags optimaux a l'aide des correlogrammes (simple et partiel)
        #Estimation des parametres : comparaison des modeles potentiels a l'aide des criteres de parcimonie (AIC, BIC, ...)
        #Validation du modele : test de significativite des parametres et diagnostic des residus (heteroscedasticite / autocorrelation)
        #Prevision : Utilisation du modele retenu a des fins de previsions

# Analyse exploratoire

library(forecast)
library(dplyr)
passenger <- as.vector(AirPassengers)                                           #AirPassangers est un JD stocke dans le package de base en R
date <- seq(from = as.Date("01/01/1949", "%d/%m/%Y"),
            to = as.Date("01/12/1960", "%d/%m/%Y"),
            by = "month")                                                       #creation isolee de la sequence des dates
data.passenger <- tibble(date, passenger)                                       #concatenation du JD et de la sequence des dates
View(data.passenger)
str(data.passenger)
ts.line2 <- ggplot(data.passenger, aes(x = date, y = passenger))+
  geom_area(color = "red", fill = "steelblue")+theme_classic()
ggplotly(ts.line2)                                                              #graphique de la serie temporelle obtenue a la suite de l'operation de concatenation

ts.passenger <- ts(data = passenger, start = c(1949, 1), frequency = 12)        #autre maniere de creer la sequence des dates, mais en serie temporelle directement (sans concatenation)
is.ts(ts.passenger)                                                             #verifie que le nouvel objet cree est bel et bien une serie temporelle
ts.passenger
ts.plot(ts.passenger, main = "Evolution du nombre de passagers",
        xlab = "Annees", ylab = "", ylim = c(100,600))                          #graphique de la serie temporelle (package de base)
ggseasonplot(ts.passenger, year.labels = T,
             main = "Season plot", ylab = "Passengers")                         #graphique saisonnier

#Du point de vue de l'approche de Box-Jenkins, l'analyse de la stationnarite d'une serie temporelle se realise, informellement, a l'aide des correlogrammes.
#Cependant, l'approche formelle exige qu'un test de racine unitaire soit realisee afin de confirmer cette intuition.
#Selon l'approche informelle, la decroissance lente (lineaire) des coefficients d'auto-correlation (simple ou partielle) est un signe de non-stationnarite de la serie temporelle.
#Par contre, si les coefficients d'auto-correlation decroissent tres rapidement (exponentiellement), alors le processus etudie est sans memoire (il y a stationnarite).

library(tseries)
acf_level <- ggAcf(ts.passenger, lag.max = 60, type = "correlation", plot = T)  #correlogramme simple de la serie en niveau
pacf_level <- ggAcf(ts.passenger, lag.max = 60, type = "partial", plot = T)     #correlogramme partiel de la serie en niveau
ADF_level <- adf.test(ts.passenger)                                             #test de racine unitaire de ADF sur la serie en niveau
PP_level <- pp.test(ts.passenger)                                               #test de racine unitaire de PP sur la serie en niveau
KPSS_level <- kpss.test(ts.passenger)                                           #test de racine unitaire de KPSS sur la serie en niveau
ggtsdisplay(ts.passenger, points = F, smooth = T, lag.max = 60,
            main = "Serie en niveau", ylab = "Passengers")

ndiffs(ts.passenger)                                                            #donne l'ordre adequat de l'operateur de difference standard susceptible de rendre la serie stationnaire
nsdiffs(ts.passenger)                                                           #donne l'ordre adequat de l'operateur de difference saisonniere susceptible de rendre la serie stationnaire
d_passenger <- diff(ts.passenger, differences = 1)                              #difference d'ordre 1 de la serie en niveau
data.passenger$d.passenger <- NA
data.passenger$d.passenger[-1] <- d_passenger
ggtsdisplay(d_passenger, points = F, smooth = T, lag.max = 60,
            main = "Serie en difference", ylab = "Passengers")

# Identification du modele

ADF_diff <- adf.test(d_passenger)                                               #test de racine unitaire de ADF sur la serie en difference
PP_diff <- pp.test(d_passenger)                                                 #test de racine unitaire de PP sur la serie en difference
KPSS_diff <- kpss.test(d_passenger)                                             #test de racine unitaire de KPSS sur la serie en difference
acf_diff <- ggAcf(d_passenger, lag.max = 60, type = "correlation", plot = T)    #correlogramme simple de la serie en difference
pacf_diff <- ggAcf(d_passenger, lag.max = 60, type = "partial", plot = T)       #correlogramme partiel de la serie en difference
    #l'ordre de la partie autoregressive s'identifie a travers le correlogramme partiel
    #l'ordre de la partie moyenne mobile s'identifie a travers le correlogramme simple
    #les resultats identiques que l'on peut pronostiquer pour un modele ARIMA(1,1,1)
    #de plus, il y a presence d'une saisonnalite de periodicite s = 12 (cf. ACF)

# Estimation des parametres

model1 <- arima(ts.passenger, order = c(1,1,1), method = "ML",
                seasonal = list(order = c(0,0,1), period = 12))                 #le modele pronostique est un SARIMA(1,1,1)(0,0,1)[12] (les ordres sont selectionnes par trial-error process, en reference aux correlogrammes)
model2 <- auto.arima(ts.passenger, method = "ML", stepwise = F, trace = T)      #le modele automatique est une SARIMA(2,1,1)(0,1,0)[12] (les ordres sont selectionnes par validation croisee, en minimisant la RMSE)
summary(model1)
summary(model2)

# Validation du modele

autoplot(model1$residuals, ylab = "Residuals", main = "Model 1")                #graphique des residus du modele 1
Box.test(model1$residuals, lag = 12, type = "Ljung-Box")                        #test portemanteau sur le modele 1
jarque.bera.test(model1$residuals)                                              #test de normalite sur le modele 1
acf_res1 <- ggAcf(model1$residuals, lag.max = 60, type = "correlation", plot = T)
pacf_res1 <- ggAcf(model1$residuals, lag.max = 60, type = "partial", plot = T)
checkresiduals(model1, plot = TRUE)
    #les residus du modele 1 semblent ne pas etre totalement aleatoires (cf. statistique de Ljung-Box)

autoplot(model2$residuals, ylab = "Residuals", main = "Model 2")                #graphique des residus du modele 2
Box.test(model2$residuals, lag = 12, type = "Ljung-Box")                        #test portemanteau sur le modele 2
jarque.bera.test(model2$residuals)                                              #test de normalite sur le modele 2
acf_res2 <- ggAcf(model2$residuals, lag.max = 60, type = "correlation", plot = T)
pacf_res2 <- ggAcf(model2$residuals, lag.max = 60, type = "partial", plot = T)
checkresiduals(model2, plot = TRUE)
    #les residus du modele 2 semblent etre generes par un processus bruit blanc

# Previsions

library(forecast)
library(ggplot2)
prev1 <- forecast(model1, h = 12)
summary(prev1)                                                                  #previsions pour les 12 mois de 1961 (modele 1)
accuracy(prev1)
autoplot(prev1, ylab = "Passengers", level = c(90, 99))+
  autolayer(fitted(model1), linetype = "dashed")                                #graphique des previsions sur base du modele 1

prev2 <- forecast(model2, h = 12)
summary(prev2)                                                                  #previsions pour les 12 mois de 1961 (modele 2)
accuracy(prev2)
autoplot(prev2, ylab = "Passengers", level = c(90, 99))+
  autolayer(fitted(model2), linetype = "dashed")                                #graphique des previsions sur base du modele 2

time <- seq(from = as.Date("01/01/1961", "%d/%m/%Y"),
            to = as.Date("01/12/1961", "%d/%m/%Y"),
            by = "month")                                                       #creation d'une variable temporelle correspondant a l'horizon de prevision
forecasted_values <- data.frame(time = time,
                                forecast_model1 = prev1$mean,
                                forecast_model2 = prev2$mean)                   #tableau des previsions mensuelles pour les 2 modeles
View(forecasted_values)

    #Conclusion : On va donc retenir les previsions basees sur le modele 2, car il a satisfait aux tests de diagnostic
    #TP : Refaire le meme exercice avec la serie temporelle 'a10' loge dans le package fpp2 (ou la serie 'elec' du package fma)

### 6.7. ACP et AFC

#L'ACP et l'AFC sont des methodes statistiques qui permettent d'etudier les liens entre toutes les variables d'un JD,
#sans attribuer un role specifique a l'une d'elles (contrairement aux methodes explicatives).
#L'ACP concerne le cas des variables quantitatives (numeric), et l'AFC celui des variables qualitatives (factor).

#Analyse en Composantes Principales (ACP)

#L'objectif de l'ACP est d'obtenir le resume le plus pertinent possible des donnees initiales (par individus et par variables).
#Le JD utilise pour realiser l'ACP se rapporte a une etude sur le pays d'origine du vin en fonction de plusieurs determinants psycho-chimiques.
#Nous avons aleatoirement attribue des etiquettes a chaque ligne (individu) dans le JD.

library(dplyr)
library(readxl)
library(readr)
wine <- read_csv(file = "C://Users//USER//Documents//Data_Science//RStudio//Wine.csv")
wine$Country <- as.factor(wine$Country)                                         #la variable QUALITY est consideree comme factor pour enrichir la visualisation graphique
wine <- as.data.frame(wine[,-1])
str(wine)
summary(wine)
wine.list <- read_excel("C://Users//USER//Documents//Data_Science//RStudio//wine_list.xlsx")
row.names(wine) <- wine.list$Vin
View(wine)
cov.matrix <- cov(wine[,-7], use = "na.or.complete")
cor.matrix <- cor(wine[,-7], use = "na.or.complete")

library(FactoMineR)
library(factoextra)
acp <- PCA(wine, scale.unit = T, graph = F,
               axes = c(1,2), quali.sup = 7)                                    #realisation de l'ACP (l'indice de la variable categorielle est 7)
fviz_eig(acp, choice = "variance", ggtheme = theme_minimal(), ncp = 5,
         addlabels = T, barfill = "darkslategray1",
         main="Pourcentage d'inertie", xlab="Axes factoriels", ylab="Inertie")  #graphique des valeurs propres (inertie) : la regle du coude nous conduit a retenir les 2 premiers axes factoriels
var.map <- plot.PCA(acp, axes = c(1,2), choix = "var", graph.type = "ggplot",
                    title = "Variables factor map", habillage = "contrib")
var.map                                                                         #resultats de l'ACP sur les variables (ces dernieres sont representees en fonction de leurs contributions)
summary.PCA(acp)                                                                #le resume de l'ACP indique les elements suivants :
                                                                                #Dist : la distance entre l'individu/la variable et le centre de gravite du nuage de points
                                                                                #Coord : les coordonnees de l'individu/la variable dans l'axe factoriel considere
                                                                                #Contr : la contribution de l'individu/la variable a la generation du facteur considere
                                                                                #Cos2 : la qualite de la representation de l'individu/la variable dans le plan factoriel
data_factor.acp <- as.data.frame(acp$ind$coord[,1:3])                           #recuperation des coordonnees des 3 premiers facteurs generes par l'ACP
print(acp$var$cor[,1:3], digits = 3)                                            #correlation entre toutes les variables initiales et les 3 premiers facteurs
fviz_pca_contrib(acp, choice = "var", axes = 3, ggtheme = theme_classic())      #contributions des variables initiales a la generation du facteur 3
fviz_pca_contrib(acp, choice = "ind", axes = 3, top = 10)                       #contributions des 10 premiers individus a la generation du facteur 3
dimdesc(res = acp)                                                              #fournit la description des axes factoriels (les 2 premiers facteurs representent pres de 55% de la dispersion totale)
ind.map <- plot.PCA(acp, axes = c(1,2), choix = "ind", habillage = "Country",
                    title = "Individuals factor map", select = "cos2 0.75")
ind.map                                                                         #resultats de l'ACP sur les individus (uniquement les individus dont le Cos2 est superieur a 0.75)
fviz_pca_biplot(acp, habillage = "Country", addEllipses = F,
                repel = T, ggtheme = theme_bw())                                #VarMap (graphique des variables) et IndMap (graphique des individus) simultanement

library(explor)                                                                 #ce package fournit des resultats visuellement plus beaux
explor(acp)
  #Interpretation :
  #Les 3 premiers axes factoriels representent plus de 70% de la dispersion (inertie) totale
  #La plupart des variables sont eloignees du centre du cercle dans le VarMap, ce qui temoigne d'une correlation importante entre les variables initiales et les facteurs virtuels
  #Le IndMap indique les possibilites de classification qui s'offrent pour les individus, ici 3 classes (voir rapport detaille)
  #Les vins d'origine canadienne ont une forte teneur en sulphates, alors que ceux d'origine francaise ont le prix le plus eleve

library(Factoshiny)                                                             #ce package permet de realiser aisement l'ACP sans passer par les codes R
Factoshiny(wine)
library(FactoInvestigate)                                                       #ce package permet de produire automatiquement un rapport detaille de l'analyse
Investigate(res = acp, document = "word_document")

#Analyse Factorielle des Correspondances (AFC)

#L'objectif de l'AFC est d'analyser la liaison existant entre deux variables qualitatives.
#Pour plus de deux variables qualitatives, on recourt à l'Analyse des Correspondances Multiples (ACM).
#Le JD utilise pour realiser l'AFC se rapporte aux informations relatives aux agents dans une industrie.
#Les variables prises en compte sont le niveau d'education, le genre, l'efficacite professionnelle et l'age.

library(dplyr)
library(ggplot2)
library(readr)
data.mca <- read_csv("C://Users//USER//Documents//Data_Science//RStudio//mca.csv", col_names = T)
data.mca <- as_tibble(select(data.mca, -1))
View(data.mca)

data.mca$gender <- as.factor(data.mca$gender)
data.mca$education <- as.factor(data.mca$education)
data.mca$effectiveness <- as.factor(data.mca$effectiveness)
summary(data.mca)
subset <- table(data.mca$education, data.mca$effectiveness)                     #tableau de contingence Education-Efficacite
khi.square.test <- chisq.test(subset, simulate.p.value = T, B = 5000)           #le test de Khi-carre constitue la premiere etape d'une AFC

library(FactoMineR)
library(factoextra)
library(explor)
library(Factoshiny)
library(FactoInvestigate)
afc <- MCA(data.mca, quali.sup = 1, quanti.sup = 4, graph = F, axes = c(1,2))   #resultats de l'AFC
summary.MCA(afc)                                                                #le resume de l'AFC indique que l'on peut garder les 2 premiers facteurs
                                                                                #les modalites HIGHEST, BACHELOR et PRIMARY contribuent le plus a la generation de l'axe 1
                                                                                #les modalites SECONDARY et MEDIUM contribuent le plus a la generation de l'axe 2
fviz_screeplot(afc, barfill = "darkslategray1", addlabels = T)                  #choix du nombre d'axes factoriels suivant la regle du coude (critere de Kaiser)
data_factor.afc <- as.data.frame(afc$ind$coord[,1:2])                           #recuperation des coordonnees des 2 premiers facteurs generes par l'AFC
plot.MCA(afc, axes = c(1,2), choix = "var", graph.type = "ggplot",
         title = "Variables map")                                               #graphique des variables (les coefficients de correlation sont eleves au carre, d'ou pas de cercle)
                                                                                #ce graphique ressort bien la liaison identifiee par le test de khi-carre
                                                                                #la variable qualitative supplementaire (gender) et la variable quantitative supplementaire (age) sont tres faiblement correlee avec les 2 facteurs
print(rbind(afc$var$eta2[,1:4], afc$quali.sup$eta2[,1:4],
            afc$quanti.sup$coord[,1:4]), digits = 3)                            #correlation entre, d'une part, les variables actives et supplementaires, et d'autre part, les 4 premiers facteurs
plot.MCA(afc, axes = c(1,2), choix = "ind", habillage = "gender", label = "var",
         col.var = "blue", graph.type = "ggplot", title = "Individuals map")    #graphique des individus (les moins instruits semblent etre peu performants,
                                                                                #mais les plus performants ne sont pas necessairement les plus instruits).
                                                                                #l'axe 1 oppose bien les modalites extremes de l'efficacite professionnelle.
fviz_mca_biplot(afc, axes = c(1,2), label = "var", addEllipses = F, ggtheme = theme_bw(),
                habillage = "gender", col.var = "darkmagenta", repel = T)       #graphique simultane des variables et individus
explor(afc)
Factoshiny(data.mca)
Investigate(res = afc, document = "word_document")

acm <- MCA(data.mca, graph = F, quanti.sup = 4, method = "Burt")                #pour l'ACM, on precise la methode de Burt (generalisation de l'AFC)
summary.MCA(acm)                                                                #la v.test permet d'apprecier si une modalite a une position significative sur un axe factoriel,
                                                                                #i.e. elle apprecie si la coordonnee cette modalite sur l'axe est statistiquement non nulle.
                                                                                #la v.test est distribuée suivant la loi normale centree reduite (distribution de Gauss).
fviz_screeplot(acm, barfill = "darkslategray1", addlabels = T)
plot.MCA(acm, axes = c(1,2), choix = "var", graph.type = "ggplot")              #le genre semble etre la variable active la moins correlee avec les deux premiers facteurs
                                                                                #la variable quantitative supplementaire (age) est aussi moins correlee avec les 2 premiers facteurs
plot.MCA(acm, axes = c(1,2), choix = "ind", label = "var",
         col.var=c(1,1,2,2,2,2,2,2,3,3,3,3,3), col.ind = "blue",
         selectMod = "contrib 13", shadowtext = T, graph.type = "ggplot")       #graphique des individus, avec les 13 modalites qui contribuent le plus dans le plan factoriel
plot.MCA(acm, axes = c(1,2), choix = "quanti.sup", autoLab = "yes")             #graphique de la variable quantitative supplementaire (le coefficient de correlation n'est pas au carre, d'ou le cercle)
                                                                                #l'age est correlee negativement avec les 2 axes factoriels
print(rbind(acm$var$eta2[,1:2], acm$quanti.sup$coord[,1:2]), digits = 3)        #correlation entre les variables et les facteurs
fviz_mca_biplot(acm, axes = c(1,2), label = "var", addEllipses = F, ggtheme = theme_bw(),
                habillage = "age", col.var = "darkmagenta", repel = T)
explor(acm)

### 7. CAS PRATIQUES

### 7.1. Exercice I

library(dplyr)
library(ggplot2)
library(readr)
fast_food <- read_csv("C://Users//USER//Documents//Data_Science//RStudio//FastFoodRestaurants.csv", col_names = T)
fast_food <- as_tibble(fast_food)
View(fast_food)

#Quelles sont les 10 villes avec le plus de fast food ?

list_city <- fast_food %>%
  group_by(city) %>%
  summarise(number = length(city)) %>%
  arrange(desc(number)) %>%
  head(n=10)
list_city

#Quelles sont les parts de marche des 10 meilleurs fast food ?

list_market_share <- fast_food %>%
  group_by(name) %>%
  summarise(market_share = length(name)/dim(fast_food)[1]*100) %>%
  arrange(desc(market_share)) %>%
  head(n=10)
list_market_share

# Combien de fast food compte la ville de New York ?

new_york1 <- fast_food %>%
  group_by(city) %>%
  filter(city %in% "New York") %>%
  summarise(length(city))
new_york1

# Quels sont les fast food les plus presents a New York et leurs parts de marche ?

new_york2 <- fast_food %>%
  filter(city %in% "New York") %>%
  group_by(name) %>%
  summarise(number = length(name), market_share = length(name)/dim(fast_food)[1]*100) %>%
  arrange(desc(number)) %>%
  head(n=5)
new_york2

# Representation graphique des 5 villes avec le plus de fastfood
# en fonction des 5 fastfood ayant les meilleurs parts de marche

city_list <- fast_food %>%
  group_by(city) %>%
  summarise(number = length(city)) %>%
  arrange(desc(number)) %>%
  head(n=5) %>%
  pull(city)
city_list                                                                       #cree la liste des 5 villes avec le plus de fastfood grace au verbe PULL

tibble_1 <- fast_food %>%
  filter(city %in% city_list)
tibble_1                                                                        #extrait du JD principal un sous-JD (tibble_1) uniquement pour ces 5 villes

market_share_list <- tibble_1 %>%
  group_by(name) %>%
  summarise(market_share = length(name)/dim(fast_food)[1]*100) %>%
  arrange(desc(market_share)) %>%
  head(n=5) %>%
  pull(name)
market_share_list                                                               #cree la liste de fastfood ayant les meilleurs part de marche

tibble_2 <- tibble_1 %>%
  filter(name %in% market_share_list)                                           #extrait du tibble_1 un autre tibble divisionnaire prenant en compte uniquement les 5 meilleurs fastfood
tibble_2

plot_1 <- ggplot(tibble_2, aes(city, fill = name))+
  geom_bar()+ylim(0,80)+theme_classic()+
  labs(title = "Distribution des 5 meilleurs fast-foods \n dans les 5 principales villes", x = "Villes", y = "")+
  theme(plot.title = element_text(hjust = 0.5, face ="bold"),
        legend.title = element_blank(),
        legend.text = element_text(face = "italic"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, face = "bold"))
plot_1

pdf("C://Users//USER//Documents//Data_Science//RStudio//plot_1.pdf")
plot_1
dev.off()

library(plotly)
ggplotly(plot_1)                                                                #le package plotly permet de rendre les graphiques interactifs

### 7.2. Exercice II

#Dans cet exercice, il est question de construire un algorithme de Machine Learning (ML)
#permettant de predire la souscription d'un client a un produit bancaire

#Lien de telechargement du JD : https://archive.ics.uci.edu/ml/datasets/bank+marketing

library(readxl)
library(dplyr)
data_bank <- read_excel("C://Users//USER//Documents//Data_Science//RStudio//bank.xlsx")
data_bank <- as_tibble(data_bank) %>%
  rename(outcome = y)
View(data_bank)

# Etape 1 : Resume statistique

data_bank$job <- as.factor(data_bank$job)
data_bank$marital <- as.factor(data_bank$marital)
data_bank$education <- as.factor(data_bank$education)
data_bank$default <- as.factor(data_bank$default)
data_bank$housing <- as.factor(data_bank$housing)
data_bank$loan <- as.factor(data_bank$loan)
data_bank$contact <- as.factor(data_bank$contact)
data_bank$month <- as.factor(data_bank$month)
data_bank$poutcome <- as.factor(data_bank$poutcome)
data_bank$outcome <- as.factor(data_bank$outcome)
summary(data_bank)

data_bank_resume <- data_bank %>%
  select(age, balance, duration)
library(RcmdrMisc)
numSummary(data_bank_resume, statistics = c("mean", "sd", "se(mean)", "cv", "IQR",
                                            "skewness", "kurtosis", "quantiles"),
           quantiles = c(0, 0.25, 0.50, 0.75, 1), type = c("2", "1", "3"),
           groups = data_bank$marital)                                          #personnalisation du resume statistique
corr_matrix <- cor(data_bank_resume, use = "na.or.complete",
                   method = c("pearson", "kendall", "spearman"))                #matrice de correlation
corr_matrix

# Etape 2 : Visualisation des donnees

library(ggplot2)
library(plotly)

graphique1 <- ggplot(data_bank, aes(x = balance, y = duration, shape = housing))+
  geom_point()+labs(title = "Durée de l'appel selon le solde bancaire")+
  theme_bw()+facet_grid(.~marital)+xlim(0,30000)+
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        strip.text = element_text(face = "italic"),
        strip.background = element_rect(linetype = "solid"),
        axis.text.x = element_text(angle = 45, vjust = 0.1))
ggplotly(graphique1)

graphique2 <- ggplot(data_bank, aes(x = education, y = age))+
  geom_boxplot(color = "blue", outlier.color = "lightblue")+
  labs(title = "Âge selon le niveau d'étude")+theme_classic()+
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(face = "italic"))
ggplotly(graphique2)

graphique3 <- ggplot(data_bank, aes(outcome, fill = contact))+
  geom_bar(size=2)+theme_minimal()+ylim(0,4000)+
  labs(title = "Résultats de la campagne Marketing \n selon le type de communication",
       x="Issues", y="")+
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.title = element_blank(),
        legend.text = element_text(face = "italic"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, face = "bold"))
ggplotly(graphique3)

pie_data <- data_bank %>% group_by(loan) %>% count(marital) %>%
  mutate(marital.freq = n/sum(n)*100,
         position = round(cumsum(marital.freq) - 0.5*marital.freq, digits = 2)) #creation d'un sous JD en frequences a representer dans un diagramme circulaire
graphique4 <- ggplot(pie_data)+theme_minimal()+
  geom_col(mapping = aes(x = "", y = marital.freq, fill = marital), color = "white")+
  geom_text(aes(x = "", y = position, label = paste0(round(marital.freq, 1), "%")), size = 3)+
  coord_polar(theta = "y", start = 0)+facet_grid(.~loan)+
  theme(axis.text = element_blank(),
        legend.text = element_text(face = "italic"),
        legend.title = element_text(face = "bold"))+
  scale_fill_brewer(palette = "Blues", name = "Statuts")+
  guides(fill = guide_legend(reverse = T))+
  labs(title = "Proportion des statuts matrimoniaux en fonction de l'accès au crédit", x = "", y = "")
graphique4

# Etape 3 : Creation du JD d'apprentissage et du JD de test

library(caret)
dummy <- dummyVars(~., data = data_bank)
dummy_data <- predict(dummy, newdata = data_bank)                               #permet de transformer toutes les variables categorielles (factor) en variables binaires (dummy)
dummy_data <- as_tibble(dummy_data)
dummy_data$outcome <- ifelse(dummy_data$outcome.no == 1, "no", "yes")
dummy_data$outcome.no = NULL                                                    #supprime la variable du JD
dummy_data$outcome.yes = NULL                                                   #supprime la variable du JD
View(dummy_data)                                                                #visualisation du JD a modeliser

set.seed(3033)                                                                  #permet d'avoir les memes resultats statistiques avec la simulation de donnes generees aleatoirement
training_size <- floor(0.7*nrow(dummy_data))                                    #recuperation de 70% du JD a modeliser pour des fins d'apprentissage
rbind(head(training_size), dim(dummy_data)[1])                                  #la taille du JD d'apprentissage constitue bien 70% du JD a modeliser
index <- sample(seq_len(nrow(dummy_data)), size = training_size)                #tirage aleatoire de 3164 indices des individus qui composeront le JD d'apprentissage
data_bank_train <- dummy_data[index,]                                           #creation du JD d'apprentissage
data_bank_test <- dummy_data[-index,]                                           #creation du JD de test

set.seed(3033)                                                                  #autre maniere de partionner les donnees
training.sample <- dummy_data$outcome %>%
  createDataPartition(p = 0.7, list = F)
data.bank.train <- dummy_data[training.sample,]                                 #JD d'apprentissage
data.bank.test <- dummy_data[-training.sample,]                                 #JD test

train <- rbind(dim(data_bank_train), dim(data.bank.train))
colnames(train) <- c("Obs", "Var")
rownames(train) <- c("Approach 1", "Approach 2")
train                                                                           #comparaison des deux approches (JD train)
test <- rbind(dim(data_bank_test), dim(data.bank.test))
colnames(test) <- c("Obs", "Var")
rownames(test) <- c("Approach 1", "Approach 2")
test                                                                            #comparaison des deux approches (JD test)

# Etape 4 : Normalisation des donnees

data_pretreatment <- preProcess(data_bank_train, method = c("center", "scale"))
data_bank_train_scaled <- predict(data_pretreatment, data_bank_train)
data_bank_test_scaled <- predict(data_pretreatment, data_bank_test)

    # A present, les donnees sont normalisées. Mais elles ne sont pas equilibrees.
    # En effet, comme indique dans le graphique3 (cf. Etape 2), la variable a predire (i.e. OUTCOME) contient considerablement plus de NO que de YES.
    # Certains algorithmes de ML son tres sensibles aux donnees desequilibrees.
    # Dans ce cas, il est important d'equilibrer les donnees avant la modelisation.
    # Il existe des fonctions dans le package CARET qui permettent de le faire.

  table(data_bank_train_scaled[,"outcome"])                                     #visualisation du desequilibre indique dans le graphique3

  # Methode downsample
    # Elle fait le sous-echantillonnage, i.e. equilibre les donnees en fonction de la modalite la moins frequente
  
  library(dplyr)
  set.seed(3033)
  data_bank_train_scaled.downsample <- downSample(x = select(data_bank_train_scaled, -outcome),
                                                  y = as.factor(data_bank_train_scaled$outcome))
  data_bank_train_scaled.downsample <- as_tibble(data_bank_train_scaled.downsample) %>%
    rename(outcome = Class)
  comparaison_down <- cbind(table(data_bank_train_scaled[,"outcome"]),
                            table(data_bank_train_scaled.downsample[,"outcome"]))
  colnames(comparaison_down) = c("Avant équilibrage", "Après équilibrage")
  comparaison_down
  
  # Methode upsample
    # Elle fait le sur-echantillonnage, i.e. equilibre les donnees en fonction de la modalite la plus frequente
  
  library(dplyr)
  set.seed(3033)
  data_bank_train_scaled.upsample <- upSample(x = select(data_bank_train_scaled, -outcome),
                                              y = as.factor(data_bank_train_scaled$outcome))
  data_bank_train_scaled.upsample <- as_tibble(data_bank_train_scaled.upsample) %>%
    rename(outcome = Class)
  comparaison_up <- cbind(table(data_bank_train_scaled[,"outcome"]),
                          table(data_bank_train_scaled.upsample[,"outcome"]))
  colnames(comparaison_up) = c("Avant équilibrage", "Après équilibrage")
  comparaison_up

# Etape 5 : Modelisation

# La modelisation se rapporte a une methode de classification, car la variable a predire est qualitative
# Il existe plusieurs algorithmes de ML :
  # (a) K-Nearest Neighbors (KNN) ;
  # (b) Naive Bayes (NB) ;
  # (c) Support Vector Machine (SVM) ;
  # (d) Random Forest (RF) ; etc.

# Algorithme KNN

set.seed(3033)
train_control_data <- trainControl(method="repeatedcv", number=10, repeats=5)   #permet de controler les nuances de calcul lors de la modelisation
explanatory_variables <- select(data_bank_train_scaled, -outcome)
outcome_variable <- as.factor(data_bank_train_scaled$outcome)
knn_model <- train(explanatory_variables, outcome_variable,
                           method = "knn", preProcess = NULL)                   #Ajustement du modele et Calcul d'une mesure de precision (Accuracy pour la classification / RMSE pour la regression)
print(knn_model)
prediction_knn_model <- predict(knn_model, newdata = select(data_bank_test_scaled, -outcome))             #prediction avec le modele ajuste sur le JD test
prediction.knn_vs_real <- confusionMatrix(prediction_knn_model, as.factor(data_bank_test_scaled$outcome)) #compare les predictions du modele aux realisations (Accuracy mesure le degre de precision du modele)
prediction.knn_vs_real                                                                                    #visualisation de la matrice de confusion
    # Interpretation : Le modele ajuste sur le JD d'apprentissage vient d'etre soumis au JD test
    # Sur les 1205 clients ayant repondu NON au produit bancaire, le modele a bien predit 1180 cas.
    # Sur les 152 clients qui ont souscrits, le modele n'a bien predit que 25 cas.
    # On constate que globalement, la modelisation est bonne, ce qui est confirme par la mesure de precision qui s'eleve à 88,8%.
    # Toutefois, on peut comparer les predictions de la methode KNN a d'autres methodes afin d'apprecier la qualite de l'ajustement et de comparer la precision dans la prediction

full_data.knn_model <- cbind(data_bank_test_scaled, prediction_knn_model)
View(full_data.knn_model)
    #Les deux dernieres colonnes de ce JD complet contiennent les predictions du modele qui a ete ajuste ainsi que les realisations.
    #A partir de la, il est possible de retrouver les clients potentiellement interesses par ce nouveau service bancaire.

comparison_table.knn_model <- cbind(table(data_bank_test_scaled[,"outcome"]),
                                    table(prediction_knn_model))
colnames(comparison_table.knn_model) = c("Real", "Prediction")
comparison_table.knn_model                                                      #Ce tableau compare les predictions du modele aux donnees reelles observees,
                                                                                #Il donne egalement les totaux de la matrice de confusion

# Algorithme RF

set.seed(3033)
train_control_data <- trainControl(method="repeatedcv", number=10, repeats=5)
explanatory_variables <- select(data_bank_train_scaled, -outcome)
outcome_variable <- as.factor(data_bank_train_scaled$outcome)
rf_model <- train(explanatory_variables, outcome_variable,
                  method = "rf", preProcess = NULL)
print(rf_model)
prediction_rf_model <- predict(rf_model, newdata = select(data_bank_test_scaled, -outcome))
prediction.rf_vs_real <- confusionMatrix(prediction_rf_model, as.factor(data_bank_test_scaled$outcome))
prediction.rf_vs_real
    # Interpretation : Compare a la methode KNN, on constate que la methode RF ajuste le modele avec une meilleure precision, soit 89,9%.
    # Nous pouvons donc retenir l'algorithme de ML base sur la methode RF pour predire la souscription d'un client a un produit bancaire.

full_data.rf_model <- cbind(data_bank_test_scaled, prediction_rf_model)
View(full_data.rf_model)
#Les deux dernieres colonnes de ce JD complet contiennent les predictions du modele qui a ete ajuste ainsi que les realisations.
#A partir de la, il est possible de retrouver les clients potentiellement interesses par ce nouveau service bancaire.

comparison_table.rf_model <- cbind(table(data_bank_test_scaled[,"outcome"]),
                                    table(prediction_rf_model))
colnames(comparison_table.rf_model) = c("Real", "Prediction")
comparison_table.rf_model                                                       #Ce tableau compare les predictions du modele aux donnees reelles observees,
                                                                                #Il donne egalement les totaux de la matrice de confusion

# Etape 6 : Recherche des variables predictives pertinentes

library(randomForest)
varImp(rf_model)                                                                #Cette fonction n'est compatible qu'avec les methodes NB et RF
plot(varImp(rf_model, scale = T))                                                 #Importance relative des regresseurs dans l'ajustement du modele
    # Interpretation : Les resultats indiquent que les 3 variables les plus pertinentes sont :
    # (i) la duree de l'appel ; (ii) le solde du compte ; et (iii) l'age du client.

### 7.3. Exercice III

#Probleme de classification ou la variable a predire comporte plus de deux modalites
#Il s'agit de predire la qualite du vin en fonction de plusieurs determinants psycho-chimiques.

library(dplyr)
library(readxl)
link <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
wine <- read.csv(file = link, header = T, sep = ";", dec = ".")

wine$target <- rep(NA, length(wine$quality))
for (i in 1:length(wine$quality)) {
  if (wine$quality[i] == 3 | wine$quality[i] == 4 | wine$quality[i] == 5) {
    wine$target[i] <- "low"
  } else if (wine$quality[i] == 6) {
    wine$target[i] <- "medium"
  } else {
    wine$target[i] <- "high"
  }
}

wine$target <- as.factor(wine$target)                                           #la variable TARGET est consideree comme factor, car c'est la variable cible
wine <- as.data.frame(wine[,-12])
View(wine)
str(wine)
summary(wine)

library(ggplot2)
library(grid)
library(gridExtra)

g1 <- ggplot(data=wine, aes(x=target, y=density, fill=target))+
  geom_boxplot()+theme_bw()+
  theme(axis.title = element_text(face = "bold", family = windowsFont("mono")),
        axis.text.x = element_blank(),
        legend.title=element_blank(), legend.text=element_text(face="italic"))
g2 <- ggplot(data=wine, aes(x=target, y=pH, fill=target))+
  geom_boxplot()+theme_bw()+
  theme(axis.title = element_text(face = "bold", family = windowsFont("mono")),
        axis.text.x = element_blank(),
        legend.title=element_blank(), legend.text=element_text(face="italic"))
g3 <- ggplot(data=wine, aes(x=target, y=sulphates, fill=target))+
  geom_boxplot()+theme_bw()+
  theme(axis.title = element_text(face = "bold", family = windowsFont("mono")),
        axis.text.x = element_blank(),
        legend.title=element_blank(), legend.text=element_text(face="italic"))
g4 <- ggplot(data=wine, aes(x=target, y=alcohol, fill=target))+
  geom_boxplot()+theme_bw()+
  theme(axis.title = element_text(face = "bold", family = windowsFont("mono")),
        axis.text.x = element_blank(),
        legend.title=element_blank(), legend.text=element_text(face="italic"))

graphs <- list(g1, g2, g3, g4)                                                  #creation d'un objet de classe LIST contenant tous les graphiques
grids <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE)                   #mise en page des dimensions de l'ecran multi-graphique a partitionner
grid.arrange(grobs = graphs, layout_matrix = grids,
             vp = viewport(width = 0.9, height = 0.9))                          #partitionnement de l'ecran et presentation simultannee des 4 graphiques

    #Pour la classification, nous allons recourir a 4 methodes supervisees :
      # (i) Naive Bayes ; (ii) arbre de decision ; (iii) random forest ; (iv) SVM ; (v) KNN ; (vi) reseau neuronal

# Naive Bayes

library(caret)
library(e1071)
set.seed(12345)
wine_NB1 <- naiveBayes(target~., data = wine.train, laplace = 0)                #ajustement du modele sur le JD d'apprentissage (sans parametre de lissage)
wine_NB2 <- naiveBayes(target~., data = wine.train, laplace = 1)                #ajustement du modele sur le JD d'apprentissage (avec parametre de lissage)
print(wine_NB1)                                                                 #affiche les probabilites conditionnelles (modele 1)
print(wine_NB2)                                                                 #affiche les probabilites conditionnelles (modele 2)
wine.NB_predict1 <- predict(wine_NB1, newdata = wine.test)                      #predictions sur le JD test (modele 1)
confusion.matrix_NB1 <- confusionMatrix(wine.NB_predict1, wine.test$target)     #matrice de confusion et accuracy (modele 1))
confusion.matrix_NB1
wine..NB_predict2 <- predict(wine_NB2, newdata = wine.test)                      #predictions sur le JD test (modele 2)
confusion.matrix_NB2 <- confusionMatrix(wine..NB_predict2, wine.test$target)     #matrice de confusion et accuracy (modele 2)
confusion.matrix_NB2

# Arbre de decision

library(caret)
library(rpart)
library(rpart.plot)                                                             #ces deux packages permettent d'obtenir l'arbre de decision

set.seed(12345)                                                                  #partionnement du JD
training.sample <- wine$target %>%
  createDataPartition(p = 0.7, list = F)
wine.train <- wine[training.sample,]                                            #JD d'apprentissage
wine.test <- wine[-training.sample,]                                            #JD test

wine.tree <- rpart(formula = target ~., data = wine.train, method = "class",
                   control = rpart.control(minsplit = 5, cp = 0.001))           #ajustement du modele sur le JD train
plot(wine.tree, uniform=T, branch=0.5, margin=0.1, main="Decision tree")
text(wine.tree, all = F, use.n = T)                                             #visualisation de l'arbre de decision (pas tres lisible)
VarImp_tree <- rename(as.data.frame(wine.tree$variable.importance),
                      Pertinence = `wine.tree$variable.importance`)             #pertinence des variables predictives dans l'ajustement du modele
VarImp_tree$Contribution <- VarImp_tree$Pertinence/sum(VarImp_tree$Pertinence)
summary(wine.tree)                                                              #resultats detailles de l'ajustement du modele
plotcp(wine.tree, lty = "dashed", col = "red")                                  #representation de l'erreur relative en fonction de la taille des sous-arbres (echelle en abscisse de haut) et de la complexite des sous-arbres (echelle en abscisse de bas)
                                                                                #la valeur optimale du parametre de complexite CP est d'environ 0.003, correspondant a 71 sous-arbres (splits)

  # Interpretation :
  #Si la longueur du petale est inferieure a 2.5, la fleur est de la categorie SETOSA
  #Si la longueur du petale est comprise entre 2.5 et 4.8, la fleur est de la categorie VERSICOLOR
  #Si la longueur du petale est superieure a 4.8 et que la largeur du petale est superieure a 1.7, alors la fleur est du type VIRGINICA
  #Pour les fleurs dont la largeur du petale est inferieure a 1.7, elles sont du type VERSICOLOR si la longueur du petale est comprise entre 2.5 et 4.8, et du type VIRGINICA si cette longueur est superieure a 4.8

#Il peut arriver que l'arbre maximal obtenu par l'algorithme soit complexe a interpreter, au regard du nombre eleve de noeuds (faible lisibilite).
#Il est possible de le simplifier afin de le rendre plus lisible. C'est la phase de l'ELAGAGE (prunning, en anglais).

wine.tree_opt <- prune(tree = wine.tree,
                       cp=wine.tree$cptable[which.min(wine.tree$cptable[,4]),1])    #elagage de l'arbre maximal
prp(wine.tree_opt, extra = 2, type = 4, yesno = 2, varlen = 0, cex = 0.7,
    box.col = 5, border.col = 4, shadow.col = "darkgray", main = "Decision tree")   #representation de l'arbre optimal (methode 1)
rpart.plot(wine.tree_opt, type = 1, extra = 104, main = "Decision tree")            #representation de l'arbre optimal (methode 2)

wine.test_predict <- predict(wine.tree_opt, newdata = wine.test, type = "class")    #predictions sur le JD test
confusion.matrix_wine <- table(wine.test$target, wine.test_predict)
confusion.matrix_wine                                                               #matrice de confusion
TotConfMat_tree <- cbind(rowSums(confusion.matrix_wine), colSums(confusion.matrix_wine))
colnames(TotConfMat_tree) = c("Real", "Predict")
TotConfMat_tree                                                                     #totaux de la matrice de confusion
real_vs_predictions.tree <- cbind(wine.test, wine.test_predict)                     #comparaison des valeurs predites aux valeurs reelles
accuracy_tree <- sum(diag(confusion.matrix_wine))/sum(confusion.matrix_wine)        #mesure du degre de precision de l'ajustement du modele
accuracy_tree                                                                       #l'Accuracy etant tres eleve (> a 50%), il s'ensuit que l'ajustement est de bonne qualite
confusionMatrix(wine.test_predict, wine.test$target)                               #resultats d'ensemble en une seule commande

# Random Forest

library(randomForest)
library(e1071)
set.seed(12345)
wine_RF <- randomForest(target~., data=wine.train, ntree=500, importance=T)     #ajustement provisoire du modele sur le JD train
plot(wine_RF)                                                                   #selection du nombre optimal d'arbres (100)
set.seed(12345)
RF <- tune.randomForest(target~., data = wine.train, nodesize = 1:10, mtry = 1:10, ntree = 100,
                        tunecontrol = tune.control(sampling = "cross"))         #calibration des parametres optimaux par validation croisee
print(RF)                                                                       #affichage des parametres optimaux (nodesize et mtry)
best_model.RF <- as.data.frame(RF$performances)                                 #identification des parametres optimaux qui minimisent l'erreur et la dispersion
wine_RF <- randomForest(target~., data=wine.train, importance = T, ntree = 80,
                        nodesize = RF$best.parameters$nodesize,
                        mtry = RF$best.model$mtry)                              #ajustement definitif avec le nombre optimal d'arbres
print(wine_RF)                                                                  #resultats detailles de l'ajustement du modele
VarImp_RF <- as.data.frame(wine_RF$importance)                                  #importance des variables predictives selon l'Accuracy ou le coefficient de GINI
varImpPlot(wine_RF, type = 1, scale = T, main = "Variables importance")         #visualisation de la pertinence des variables predictives (type = 1 pour Accuracy, type = 2 pour Gini)
wine.RF_predict <- predict(wine_RF, newdata = wine.test)                        #predictions sur le JD test
confusion.matrix_RF <- table(wine.test$target, wine.RF_predict)
confusion.matrix_RF                                                             #matrice de confusion
TotConfMat_RF <- cbind(rowSums(confusion.matrix_RF), colSums(confusion.matrix_RF))
colnames(TotConfMat_RF) = c("Real", "Predict")
TotConfMat_RF                                                                   #totaux de la matrice de confusion
real_vs_predictions.RF <- cbind(wine.test, wine.RF_predict)                     #comparaison des valeurs predites aux valeurs reelles
accuracy_RF <- sum(diag(confusion.matrix_RF))/sum(confusion.matrix_RF)          #mesure du degre de precision de l'ajustement du modele
accuracy_RF
confusionMatrix(wine.RF_predict, wine.test$target)                              #resultats d'ensemble en une seule commande

# Support Vector Machine (SVM)

library(caret)
library(e1071)
set.seed(12345)
SVM <- tune.svm(target~., data = wine.train, gamma = 2^(-5:5), cost = 2^(2:9),
                tunecontrol = tune.control(sampling = "cross"))                 #calibration des parametres par validation croisee
summary(SVM)                                                                    #resultats detailles de la determination des parametres optimaux
best_model.SVM <- as.data.frame(SVM$performances)                               #identification des parametres optimaux (gamma et cost) qui minimisent l'erreur et la dispersion
plot(SVM, main = "Performance de l'algorithme SVM")                             #visualisation du processus d'optimisation de la performance du modele
wine_SVM <- svm(target~., data = wine.train, scale = T, kernel = "radial",
                gamma = SVM$best.model$gamma, cost = SVM$best.model$cost,
                cross = 10, probability = T)                                    #estimation du modele avec les parametres optimaux (voir CROSS dans SAMPLING, i.e. SVM$sampling)
summary(wine_SVM)                                                               #resultats detailles de l'ajustement du modele
plot(wine_SVM, data = wine.train, alcohol ~ sulphates,
     slice = list(fixed.acidity = mean(wine.train$fixed.acidity),
                  volatile.acidity = mean(wine.train$volatile.acidity),
                  citric.acid = mean(wine.train$citric.acid),
                  residual.sugar = mean(wine.train$residual.sugar),
                  chlorides = mean(wine.train$chlorides),
                  free.sulfur.dioxide = mean(wine.train$free.sulfur.dioxide),
                  total.sulfur.dioxide = mean(wine.train$total.sulfur.dioxide),
                  density = mean(wine.train$density),
                  pH = mean(wine.train$pH)))                                    #visualisation du plan de classification sur base des 2 predicteurs les plus pertinents (les autres etant fixes a leur niveau moyen)
wine.SVM_predict <- predict(wine_SVM, newdata = wine.test)                      #predictions sur le JD test
confusion.matrix_SVM <- confusionMatrix(wine.SVM_predict, wine.test$target)     #l'accuracy du modele SVM est faible par rapport au modele RF
confusion.matrix_SVM

# K-Nearest Neighbors (KNN)

library(caret)
library(e1071)
set.seed(12345)
KNN <- tune.knn(x = wine.train[,-12], y = wine.train$target, k = 1:10, l = 1:10,
                tunecontrol = tune.control(sampling = "cross"))                 #determination des parametres optimaux par validation croisee
print(KNN)                                                                      #resultats de la calibration des parametres
best_model.KNN <- as.data.frame(KNN$performances)                               #identification des parametres optimaux (K et L) qui minimisent l'erreur et la dispersion
plot(KNN, main = "Performance de l'algorithme KNN")                             #evaluation de la performance du modele
wine_KNN <- gknn(target~., data = wine.train, scale = T, method = "Euclidean",
                 k = KNN$best.model$k, l = KNN$best.model$l)                    #ajustement du modele avec les parametres optimaux
summary(wine_KNN)
wine.KNN_predict <- predict(wine_KNN, newdata = wine.test)
confusion.matrix_KNN <- confusionMatrix(wine.KNN_predict, wine.test$target)     #l'accuracy du modele KNN est plus grand que celui du modele RF
confusion.matrix_KNN

# Reseau neuronal

library(nnet)                                                                   #ce package necessite que le tableau des predicteurs soit transforme en matrice et la variable d'interet en dummy
library(NeuralNetTools)                                                         #ce package permet de representer graphiquement le reseau neuronal ajuste
library(e1071)                                                                  #ce package permet de déterminer le nombre optimal de neurones dans le reseau

X.train <- as.matrix(wine.train[,-12])
X.test <- as.matrix(wine.test[,-12])
y.train <- cbind(as.numeric(wine.train$target == "low"),
                 as.numeric(wine.train$target == "medium"),
                 as.numeric(wine.train$target == "high"))
colnames(y.train) <- c("low", "medium", "high")
y.test <- cbind(as.numeric(wine.test$target == "low"),
                as.numeric(wine.test$target == "medium"),
                as.numeric(wine.test$target == "high"))
colnames(y.test) <- c("low", "medium", "high")

set.seed(12345)
NN <- nnet(x = X.train, y = y.train, size = c(1,2), entropy = T, maxit = 1000)  #ajustement d'un reseau de 2 couches dont la premiere contient 1 noeud et la derniere 2 noeuds
plotnet(NN, max_sp = TRUE, circle_cex = 4, cex_val = 0.6)                       #visualisation graphique du reseau neuronal provisoirement ajuste
opt_CV <- tune.nnet(target~., data = wine.train, size = 1:10,
                    decay = seq(from = 0, by = 0.1, to = 1), maxit = 1000,
                    trace = T, tunecontrol = tune.control(cross = 10))          #determination de la taille optimale du reseau neuronal (nombre optimal de couches et de noeuds)
plot(opt_CV, main = "Performance de l'algorithme NN")                           #evaluation de la performance du modele
best_model.NN <- as.data.frame(opt_CV$performances)                             #identification (parmi les 10 modeles) de la taille optimale qui minimise l'erreur et la dispersion
best_size <- unlist(opt_CV$best.parameters$size)                                #recuperation de la taille optimale du reseau
best_decay <- unlist(opt_CV$best.parameters$decay)                              #recuperation de l'hyper-parametre DECAY optimal

library(ggplot2)
library(grid)
library(gridExtra)
size <- ggplot(data = best_model.NN, aes(x = size))+
  xlab("Taille du reseau")+ylab("Indicateurs de precision")+
  labs(title = "Precision de l'ajustement du reseau",
       subtitle = "en fonction de la taille du reseau")+
  geom_line(aes(y = error, colour = "error"))+
  geom_line(aes(y = dispersion, colour = "dispersion"))+
  theme_classic()+ylim(0,0.5)+
  geom_vline(xintercept=best_size, color="black", linetype="dotted", size=1)+
  theme(legend.title = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "bold.italic", hjust = 0.5),
        legend.text = element_text(face = "italic"))
decay <- ggplot(data = best_model.NN, aes(x = decay))+
  xlab("Hyper-parametre")+ylab("Indicateurs de precision")+
  labs(title = "Precision de l'ajustement du reseau",
       subtitle = "en fonction de l'hyper-parametre")+
  geom_line(aes(y = error, colour = "error"))+
  geom_line(aes(y = dispersion, colour = "dispersion"))+
  theme_classic()+ylim(0,0.5)+
  geom_vline(xintercept=best_decay, color="black", linetype="dotted", size=1)+
  theme(legend.title = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "bold.italic", hjust = 0.5),
        legend.text = element_text(face = "italic"))
graphs <- list(size, decay)
grids <- matrix(c(1,2), nrow = 1, ncol = 2, byrow = TRUE)
grid.arrange(grobs = graphs, layout_matrix = grids,
             vp = viewport(width = 0.9, height = 0.9))                          #visualisation des parametres optimaux du reseau

library(caret)
wine_NN <- nnet(x = X.train, y = y.train, size = best_size, decay = best_decay,
                entropy = T, maxit = 1000, Hess = T)                            #ajustement sur le JD train (approche 1)
plotnet(wine_NN, max_sp = TRUE, circle_cex = 4, cex_val = 0.6)                  #representation graphique du reseau neuronal optimal
wine_NN2 <- train(x = wine.train[,-12], y = wine.train$target, method = "nnet",
                  preProcess = c("center", "scale"), maxit = 1000,
                  tuneGrid = data.frame(size = best_size, decay = best_decay),
                  trControl = trainControl(method = "repeatedcv",
                                           number = 10,
                                           repeats = 5))                        #ajustement sur le JD train (approche 2)
print(wine_NN2)                                                                 #resultats de l'ajustement du modele
varImp(wine_NN2)                                                                #pertinence des predicteurs
wine.NN_predict <- predict(wine_NN2, newdata = wine.test)                       #predictions sur le JD test
confusion.matrix_NN <- confusionMatrix(wine.NN_predict, wine.test$target)       #matrice de confusion et accuracy
confusion.matrix_NN

### 7.4. Exercice IV

#Cet exercice aborde un cas pratique sur les methodes de regression, i.e. la variable a predire est quantitative
#Le JD utilise se rapporte a des donnees sur le logement pour 506 secteurs a Boston (recensement de 1970)
#Il est question de predire la valeur d'une maison (mediane, en milliers de USD) sur le marche de logement en fonction d'un certain nombre de determinants sociologiques et ecologiques

library(mlbench)                                                                #package contenant le JD
data("BostonHousing")                                                           #importation du JD
help("BostonHousing")                                                           #description des variables du JD
View(BostonHousing)                                                             #visualisation du JD
str(BostonHousing)                                                              #nature des variables contenues dans le JD
summary(BostonHousing)                                                          #resume statistique du JD

library(GGally)
ggpairs(BostonHousing)

### 7.5. Exercice V

#Cet exercice est aussi un cas de regression, mais ou les donnees sont des series temporelles
#le JD utilise renseigne sur l'evolution journaliere de la consommation electrique dans 3 zones de la ville de Tetouan en 2017  

library(dplyr)
library(lubridate)                                                              #ce package permet la conversion en format Date jusqu'a des frequences evaluees en minutes et en secondes
link <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00616/Tetuan%20City%20power%20consumption.csv"
power_consumption <- read.csv(file = link, header = TRUE, sep = ",", dec = ".")
power_consumption$DateTime <- mdy_hm(power_consumption$DateTime)
power_consumption <- as_tibble(power_consumption)
View(power_consumption)
attach(power_consumption)
str(power_consumption)
summary(power_consumption)

library(ggplot2)
library(grid)
library(gridExtra)

g1 <- ggplot(power_consumption, aes(x = Temperature, y = Zone.1.Power.Consumption))+
  geom_point(size=0.1, colour = "cyan")+theme_bw()+
  labs(title = "Power consumption - Temperature \n (Zone 1)")+
  geom_smooth(formule = Zone.1.Power.Consumption ~ Temperature, method = lm, se = F)+
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 10),
        axis.title = element_text(size = 7), axis.text = element_text(size = 6))
g2 <- ggplot(power_consumption, aes(x = Temperature, y = Zone.2..Power.Consumption))+
  geom_point(size=0.1, colour = "cyan")+theme_bw()+
  labs(title = "Power consumption - Temperature \n (Zone 2)")+
  geom_smooth(formule = Zone.2..Power.Consumption ~ Temperature, method = lm, se = F)+
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 10),
        axis.title = element_text(size = 7), axis.text = element_text(size = 6))
g3 <- ggplot(power_consumption, aes(x = Temperature, y = Zone.3..Power.Consumption))+
  geom_point(size=0.1, colour = "cyan")+theme_bw()+
  labs(title = "Power consumption - Temperature \n (Zone 3)")+
  geom_smooth(formule = Zone.3..Power.Consumption ~ Temperature, method = lm, se = F)+
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 10),
        axis.title = element_text(size = 7), axis.text = element_text(size = 6))
g4 <- ggplot(power_consumption, aes(x = Humidity, y = Zone.1.Power.Consumption))+
  geom_point(size=0.1, colour = "cyan")+theme_bw()+
  labs(title = "Power consumption - Humidity \n (Zone 1)")+
  geom_smooth(formule = Zone.1.Power.Consumption ~ Humidity, method = lm, se = F)+
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 10),
        axis.title = element_text(size = 7), axis.text = element_text(size = 6))
g5 <- ggplot(power_consumption, aes(x = Humidity, y = Zone.2..Power.Consumption))+
  geom_point(size=0.1, colour = "cyan")+theme_bw()+
  labs(title = "Power consumption - Humidity \n (Zone 2)")+
  geom_smooth(formule = Zone.2..Power.Consumption ~ Humidity, method = lm, se = F)+
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 10),
        axis.title = element_text(size = 7), axis.text = element_text(size = 6))
g6 <- ggplot(power_consumption, aes(x = Humidity, y = Zone.3..Power.Consumption))+
  geom_point(size=0.1, colour = "cyan")+theme_bw()+
  labs(title = "Power consumption - Humidity \n (Zone 3)")+
  geom_smooth(formule = Zone.3..Power.Consumption ~ Humidity, method = lm, se = F)+
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 10),
        axis.title = element_text(size = 7), axis.text = element_text(size = 6))

graphs <- list(g1, g2, g3, g4, g5, g6)                                          #creation d'un objet de classe LIST contenant tous les graphiques
grids <- matrix(c(1,2,3,4,5,6), nrow = 2, ncol = 3, byrow = TRUE)               #mise en page des dimensions de l'ecran multi-graphique a partitionner
grid.arrange(grobs = graphs, layout_matrix = grids,
             vp = viewport(width = 0.9, height = 0.8))                          #partitionnement de l'ecran et presentation simultannee des 6 graphiques

### 7.6. Exercice VI

#Cet exercice aborde les methodes d'apprentissage non-supervise (clustering)
#Le clustering permet de regrouper les individus en sous-populations homogenes.
#L'application consideree est celle utilisee dans la partie relative a l'ACP
#Il s'agit du JD sur l'etude des caracteristiques psycho-chimiques du vin.
#Nous avons aleatoirement attribue des etiquettes a chaque ligne dans le JD.

library(dplyr)
library(readxl)
library(readr)
wine <- read_csv(file = "C://Users//USER//Documents//Data_Science//RStudio//Wine.csv")
wine$Country <- as.factor(wine$Country)                                         #la variable COUNTRY est consideree comme factor pour enrichir la visualisation graphique
wine <- as.data.frame(wine[,-1])
str(wine)
summary(wine)
wine.list <- read_excel("C://Users//USER//Documents//Data_Science//RStudio//wine_list.xlsx")
row.names(wine) <- wine.list$Vin
View(wine)

  #Pour le clustering, nous allons recourir a 2 methodes non-supervisees :
    # (i) kmeans , et (ii) methode hierarchique ascendante.

# K-means

library(NbClust)
clust <- NbClust(data=wine[,-7], distance="euclidean", method="kmeans",
                 index = "all", alphaBeale = 0.1)                               #determination du nombre de clusters (classes)
library(factoextra)
fviz_nbclust(clust)                                                             #visualisation de la regle a la majorite simple : on retient 3 clusters (mais k=2 aussi est un choix raisonnable)

km <- kmeans(x = wine[,-7], centers = 3,
             algorithm = c("Hartigan-Wong",
                            "Lloyd",
                            "Forgy",
                            "MacQueen"),
             trace=TRUE)                                                        #ajustement de l'algorithme kmeans
print(km)                                                                       #resultats detailles de l'ajustement
km$centers                                                                      #centres des classes
km$size                                                                         #tailles des classes
wine <- wine %>%
  mutate(cluster1 = km$cluster)                                                  #ajout de la colonne CLUSTER dans le dataset
wine$cluster1 <- as.factor(wine$cluster1)
criteria <- data.frame(withinss = km$tot.withinss, betweenss = km$betweenss)    #variance intra-classe (homogeneite a l'interne) et variance inter-classe (heterogeneite a l'externe)

for (i in 1:length(km$size)) {
  idx <- which(km$cluster == i)
  nb <- length(idx)
  cat("Cluster ", i, ": ", sep = "")
  for (j in 1:nb) {
    cat(names(idx)[j], " - ", sep = "")
  }
  cat("\n")
}                                                                               #repartition des vins par clusters

par(mfrow = c(1,1))

library(cluster)
clusplot(x = wine[,-c(7,8)], clus = km$cluster, color = T, shade = T,
         labels = 2, lines = 0, plotchar = T, span = T, cex.txt = 0.8,
         main = "Cluster plot")                                                 #cluster plot

library(factoextra)
fviz_cluster(object = km, data = wine[,-c(7,8)], stand = T, axes = c(1,2),
             geom = c("point", "text"), show.clust.cent = T, ellipse = T,
             ellipse.type = "convex", ellipse.alpha = 0.1, pointsize = 1,
             main = "Cluster plot", palette = "jco", repel = T,
             ggtheme = theme_classic())                                         #cluster plot ameliore

fviz_nbclust(x = wine[,-c(7,8)], FUNcluster = kmeans, method = "wss")+
  geom_vline(xintercept = 3, linetype = "dotted", color = "red", size = 1)+
  labs(subtitle = "Elbow method")+
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(face = "italic", hjust = 0.5))             #regle du coude (sur base de la methode de minimisation de la variance intra-classe : WSS)

library(ggplot2)
library(ggrepel)
library(plotly)
ggplotly(
  ggplot(data = wine, aes(x = Price, y = Rating, color = cluster1,
                          label = row.names(wine)))+
    geom_point()+theme_bw()+geom_text_repel()+
    facet_wrap(Country~., nrow = 2, ncol = 2)+
    labs(title = "Visualisation des clusters sur les donnees reelles")+
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.title = element_text(size = 15),
          legend.title = element_text(size = 12.5, face = "italic"),
          legend.text = element_text(size = 12.5))
)                                                                               #visualisation des clusters sur les donnees originales (les deux variables choisies sont celles qui contribuent le plus a la formation du premier axe factoriel)

# Methode hierarchique ascendante

library(factoextra)
dis_matrix <- get_dist(x = wine[,-c(7,8)], method = "euclidean", stand = T)     #calcul des dissimilarites entre les individus sur base de la distance euclidienne
dis.matrix <- as.matrix(dis_matrix)                                             #visualisation de la matrice des distances (les entrees sur la diagonale sont nuls)

library(fastcluster)                                                            #ce package permet d'optimiser le temps de calcul de l'algorithme, surtout en face de grands jeux de donnees
dendrogram <- hclust(d = dis_matrix, method = "ward.D2")                        #construction du dendrogramme sur base du critere d'aggregation de Ward

library(ggdendro)
ggdendrogram(dendrogram)+
  labs(title = "Cluster Dendrogram", y = "Height")+
  theme(plot.title = element_text(face = "bold", hjust = 0.5))                  #visualisation du dendrogramme : par parcimonie, on peut couper l'arbre a la hauteur de 10 pour obtenir 4 clusters

library(ggplot2)
library(dendextend)
ggplot(color_branches(dendrogram, h = 10), labels = FALSE)+
  labs(title = "Cluster Dendrogram", y = "Height")+
  theme(plot.title = element_text(face = "bold", hjust = 0.5))                  #visualisation des 4 clusters obtenus (approche 1)

library(FactoMineR)
fviz_dend(dendrogram, k = 4, show_labels = T, rect = T, rect_fill = F,
          labels_track_height = 10, repel = T, ggtheme = theme_classic())       #visualisation des 4 clusters obtenus (approche 2)

ACP <- PCA(wine, scale.unit = T, graph = F, axes = c(1,2), quali.sup = -c(1:6)) #analyse en composantes principales
fviz_eig(ACP, choice = "variance", ggtheme = theme_minimal(), ncp = 5,
         addlabels = T, barfill = "darkslategray1",
         main="Pourcentage d'inertie", xlab="Axes factoriels", ylab="Inertie")  #Inertie
plot.PCA(ACP, axes = c(1,2), choix = "var", graph.type = "ggplot",
         title = "Variables factor map", habillage = "contrib")                 #VarMap
plot.PCA(ACP, axes = c(1,2), choix = "ind", habillage = "Country",
         title = "Individuals factor map", select = "cos2 0.75")                #IndMap
ACP$var$contrib                                                                 #RATING contribue plus a la formation de l'axe 1, et ALCOHOL a la formation de l'axe 2
CAH <- HCPC(res = ACP, nb.clust = 4, graph.scale="inertia", graph = T)          #clustering sur les resultats de l'ACP
plot.HCPC(x = CAH, axes = c(1,2), choice = "3D.map")                            #visualisation du dendrogramme en 3D

library(questionr)
classes <- cutree(tree = dendrogram, h = 10)                                    #recuperation des classes d'appartenance pour chaque individu
freq(classes, total = T)                                                        #frequences de repartition des individus dans les clusters
wine <- wine %>%
  mutate(cluster2 = classes)                                                    #generation d'une nouvelle colonne dans le JD qui repartit les individus dans les classes precedemment constitues
wine$cluster2 <- as.factor(wine$cluster2)
table <- rbind(tapply(wine$Rating, classes, mean),
               tapply(wine$Price, classes, mean),
               tapply(wine$Alcohol, classes, mean),
               tapply(wine$Residual_Sugar, classes, mean),
               tapply(wine$Sulphates, classes, mean),
               tapply(wine$pH, classes, mean))
row.names(table) <- c("Rating", "Price", "Alcohol", "Residual_Sugar", "Sulphates", "pH")
colnames(table) <- c("Cluster I", "Cluster II", "Cluster III", "Cluster IV")
View(table)                                                                     #moyennes des variables quantitatives par clusters

library(grid)
library(gridExtra)
library(ggrepel)
g1 <- ggplot(data = wine, aes(x = Price, y = Rating, color = cluster1,
                              label = row.names(wine)))+
  geom_point()+theme_bw()+geom_text_repel()+
  labs(title = "Clustering non hierarchique", subtitle = "k = 3")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold.italic"),
        axis.title = element_text(size = 15),
        legend.title = element_text(size = 12.5, face = "italic"),
        legend.text = element_text(size = 12.5))
g2 <- ggplot(data = wine, aes(x = Alcohol, y = Rating, color = cluster2,
                              label = row.names(wine)))+
  geom_point()+theme_bw()+geom_text_repel()+
  labs(title = "Clustering hierarchique", subtitle = "k = 4")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold.italic"),
        axis.title = element_text(size = 15),
        legend.title = element_text(size = 12.5, face = "italic"),
        legend.text = element_text(size = 12.5))

graphs <- list(g1, g2)                                                          #creation d'un objet de classe LIST contenant tous les graphiques
grids <- matrix(c(1,2), nrow = 1, ncol = 2, byrow = TRUE)                       #mise en page des dimensions de l'ecran multi-graphique a partitionner
grid.arrange(grobs = graphs, layout_matrix = grids,
             vp = viewport(width = 0.9, height = 0.9))                          #comparaison des deux types de clustering

### 7.7. Bonus de dataset

# Heart Disease

library(readr)
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
heart.disease <- read.csv(file = url, header = FALSE)
colnames(heart.disease) <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg",
                             "thalach", "exang", "oldpeak", "slope", "ca", "thal", "hd")
heart.disease$sex <- ifelse(heart.disease$sex == 0, "F", "M")                   #recodage de la variable SEX
heart.disease$hd <- ifelse(heart.disease$hd == 0, "Healthy", "Unhealthy")       #recodage de la variable a predire
heart.disease[heart.disease == "?"] <- NA                                       #remplacement de ? par NA
heart.disease$sex <- as.factor(heart.disease$sex)
heart.disease$hd <- as.factor(heart.disease$hd)
heart.disease$cp <- as.factor(heart.disease$cp)
heart.disease$fbs <- as.factor(heart.disease$fbs)
heart.disease$restecg <- as.factor(heart.disease$restecg)
heart.disease$exang <- as.factor(heart.disease$exang)
heart.disease$slope <- as.factor(heart.disease$slope)
heart.disease$ca <- as.integer(heart.disease$ca)
heart.disease$thal <- as.integer(heart.disease$thal)
heart.disease$ca <- as.factor(heart.disease$ca)
heart.disease$thal <- as.factor(heart.disease$thal)
str(heart.disease)
sum(is.na(heart.disease))                                                       #nombre de valeurs manquantes dans le JD
heart.disease <- na.omit(heart.disease)                                         #suppression des lignes contenant des NAs
View(heart.disease)

# Accelerometer

library(dplyr)
link <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00611/accelerometer.csv"
accelerometer <- read.csv(file = link, header = TRUE, sep = ",", dec = ".")

set.seed(12345)
accelerometer_subdataset <- sample_n(accelerometer, size = 1000, replace = F)
accelerometer_subdataset <- as_tibble(accelerometer_subdataset)
accelerometer_subdataset$configuration <- rep(NA, length(accelerometer_subdataset$wconfid))
for (i in 1:length(accelerometer_subdataset$wconfid)) {
  if (accelerometer_subdataset$wconfid[i] == 1) {
    accelerometer_subdataset$configuration[i] <- "red"
  }
  else if (accelerometer_subdataset$wconfid[i] == 2){
    accelerometer_subdataset$configuration[i] <- "blue"
  }
  else {
    accelerometer_subdataset$configuration[i] <- "green"
  }
}
View(accelerometer_subdataset)
accelerometer_subdataset$configuration <- as.factor(accelerometer_subdataset$configuration)
str(accelerometer_subdataset)
summary(accelerometer_subdataset)