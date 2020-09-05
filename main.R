library(magrittr) #pour utiliser les pipes
library(plotly)
library(reshape2) #pour utiliser melted
library(ggplot2) #viz
library(caret) #confusion matrix, etc.
#On utilise la librairie e1071 pour tester différentes combiles hyper paramètres...
library(e1071)
library(gbm)
library(webshot)
webshot::install_phantomjs()
data = read.csv2("Spotify.csv")

#data %>% summary()

set_to_numeric_var    = c('acousticness', 'danceability', 'energy', 'instrumentalness', 
                          'liveness', 'loudness', 'speechiness', 'tempo', 'time_signature',
                          'valence')
set_to_categorial_var = c('key', 'mode', 'like')

data[set_to_numeric_var]    <- data[set_to_numeric_var]    %>% lapply(as.numeric)
data[set_to_categorial_var] <- data[set_to_categorial_var] %>% lapply(as.factor)

data %>% summary()
data %>% str()
# les données sont au bon format maintenant et il n'y a pas besoin de
# faire des traitements de valeurs manquantes..
# Maintenant toutes les variables sont au bon format
# 
# Rq: la variable à expliquer (like/dislike) semble bien proportionné dans
# l'échantillon, il n'y a donc pas besoin d'avoir recours à des méthodes de
# traitement spécifiques au données mal balancées.
# 
# vrai travail de statisticien d'abord (regarder toutes les combinaisons bivariées avec la variable cible)
# Nous allons faire des  visualisations pour connaitre l'impact des covariables X sur la variable à prédire Y


freq.table <- table(data$like, data$key)

df <- apply(freq.table, 1, function(x) x/freq.table%>% colSums()) %>% t() %>% round(3) %>% t() %>% data.frame() 

df['key'] = 1:12


(p1 =plot_ly(df, x=~key, y=~X0, type = 'bar', name = 'dislike') %>%  
                        add_trace(y = ~X1, name = 'like')  %>% 
layout(yaxis = list(title = 'Pourcentage %'), barmode = 'stack'))


 plot_ly(data, y = ~valence, color = ~like, type = 'box')

# tapply(data$like, data$key, length)

p2 <- plot_ly(data, x = ~acousticness, y = ~energy, type = 'scatter',
             mode = "markers", marker = list(color = "blue")) %>%
  layout(
    title = "Gender earnings disparity",
    xaxis = list(title = "Annual Salary (in thousands)"),
    margin = list(l = 100)
  )
p2

plots = list(p1, p2)

### Mettre plusieurs plots dans un seul graphe plotly
subplot(p1, p2,nrows = 2)

subplot(plots)

plot_ly(data, x = ~data[set_to_numeric_var], y = ~energy, type = 'scatter',
        mode = "markers", marker = list(color = "blue")) %>%
  layout(
    title = "Gender earnings disparity",
    xaxis = list(title = "Annual Salary (in thousands)")
  )


plots = list()
N_numeric = length(set_to_numeric_var)
N_categ   = length(set_to_categorial_var) - 1
for(i in 1:N_numeric){
  var = set_to_numeric_var[i]
  plots[[i]] = plot_ly(y = data[,var], color = data$like, type = 'box') %>% 
    layout(title = set_to_numeric_var[i])
  
  #test de student de difference  de moyenne par groupe de (like ou non like)
  test_stat = t.test(data[,var] ~ like, data = data)
  print(paste(var, " : p-value = ", test_stat$p.value))
}

##rq, seule la variable time_signature semble ne pas vraiment changfer selon 
# la variable catégorique et ca sevoit dans le boxplot.


for(j in 1:N_categ){
  var = set_to_categorial_var[j]
  freq.table <- table(data$like, data[,var])
  df <- apply(freq.table, 1, function(x) x/freq.table%>% colSums()) %>% t() %>% round(3) %>% t() %>% data.frame() 
  df[var] = data[,var] %>% levels()
  plots[[j+N_numeric]] = plot_ly(df, x=df[,var], y=~X0, type = 'bar', name = 'dislike') %>%  
    add_trace(y = ~X1, name = 'like')  %>% 
    layout(yaxis = list(title = 'Count'), barmode = 'stack')
  
  #test
  #test de proportion
  print(freq.table)
  chi.test = chisq.test(freq.table)
  print(paste(var, " : p-value = ", chi.test$p.value))
}

##la répartition des effectifs est significativement différente
# selon les likes/dislikes... On ne peut donc pas dire qu'il n'existe 
# pas de lien d entre ces varia bles catégoriques et la variable Y


subplot(plots, nrows = 4) %>% 
  layout(title='Statistiques bivariées entre la variable Y et les variables explicatives', 
         showlegend = FALSE)




##### On peut également faire des tests statistiques ###
corr_matrix = round(100*cor(data[set_to_numeric_var]),2) %>% as.data.frame()

corr_matrix %>% rowMeans()

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)


# Fondre la matrice de corrélation
melted_cormat <- melt(upper_tri,   na.rm = TRUE)
#plot_ly(x = corr_matrix %>% rownames(), y = corr_matrix %>% colnames(),type = 'heatmap')

melted_cormat %>% plot_ly(x = ~Var1, y=~Var2, z=~value, type = 'heatmap') 



cormat <- round(cor(mydata),2)

reorder_cormat <- function(cormat){
  # Utiliser la corrélation entre les variables
  # comme mésure de distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}


# Reordonner la matrice de corrélation
cormat <- reorder_cormat(cormat)
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)


# Fondre la matrice de corrélation
melted_cormat <- melt(upper_tri,   na.rm = TRUE)

melted_cormat = data[, set_to_numeric_var] %>% 
  cor() %>% round(2) %>% get_upper_tri() %>% melt(na.rm = T)

# Créer un ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed() +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

# Afficher heatmap
print(ggheatmap)

# lES CORR2LATIONS OSNT TOUTES FAIBLES... on peut supposer ne pas avoir de problème
# de multicolinéarité plus tard lors de la modélisation...


##################### partie 2 (modelisation) ################
#ON PREND uniquement les variables dont on a besoin
data = data[,c(set_to_numeric_var, set_to_categorial_var)]

# il faut decouper des ici train tesT 80%, 20%
# voici la fonction qu'on va fréquemment utiliser pour le découpage
train.test.split = function(data, train.size, seed){
  set.seed(seed) 
  sample <- sample.int(n = nrow(data), size = floor(train.size*nrow(data)), replace = F)
  train  <- data[sample, ]
  test   <- data[-sample, ]
  return(list('train'=train, 'test'=test))
}

splited.data = train.test.split(data, train.size = 0.8, seed = 7)
train = splited.data$train
test  = splited.data$test
train$like %>% summary() 
test$like %>% summary()

# On voit bien que les "Y sont bien réparties entre entrainement et test

#Principe : 1. pour chaque méthode, on choisit d'abord les paramètres simples par défaut
# dont on va comparer les performances via une méthode de découpage train/val sur
# laquelle on fixe le mm chiffre generateur pour tout le monde.
# 2. en second lieu, on fait de la validation croisée sur tout le train pour
# choisir les meilleurs paramètres que l'on va garder pour la partie test

## Pour ce faire, on va d'abord découper la partie entrainement encore en
# 2 parties (train et val)

splited.data = train.test.split(train, train.size = 0.8, seed = 2)
inner.train  = splited.data$train
validation   = splited.data$test
inner.train$like %>% summary() 
validation$like %>% summary()

############ une méthode de bagging, le randomforest ###############

# 1ere methode par decoupage train/validation pour donner une interprétation 
# générale du modèle

set.seed(123)
library(randomForest)
fit <- randomForest(like ~ ., data = inner.train)
fit

#par defaut il y a 500 arbres construits et p=3 pour chaque division...
# sur l'entrainement on voit une erreur globale

varImpPlot(fit)

#Ce plot nous montre que l'instrument a une tres grande importance


prediction <-predict(fit, validation)
confusionMatrix(prediction, validation$like)
#on a une performance de 78%

### place à la validation croisée maintenant....
#On sait que les 12 premieres variables sont nos regresseurs X
# Define training control

set.seed(123) 
control  <- trainControl(method = "cv", number = 5)
tunegrid <- expand.grid(mtry = 1:12)
set.seed(123)
custom <- train(like~., data=train, method="rf", ntree=500,
                tuneGrid=tunegrid, trControl=control)
summary(custom)
plot(custom)


##### Boosting()



##

set.seed(123)
boosting = gbm(as.character(like)~., data=inner.train, distribution = "bernoulli")
summary(boosting)
plot(boosting)

prediction2 <-predict.gbm(boosting, validation, n.trees=100, type="response")
prediction2 = prediction2 %>% sapply(function(x) ifelse(x>.5, 1, 0))
confusionMatrix(as.factor(prediction2), validation$like)


#########

which.max(2)
boost.cv = gbm(as.character(like)~., data=inner.train, distribution = "bernoulli", 
    n.trees = 80)

gbm.perf(boost.cv)

?predict.gbm
#Validation croisee
train.control = expand.grid(method = "cv", number = 5)
boost.cv <- train(as.character(like) ~ ., data = train, 
               method = "gbm",
               trControl = train.control, verbose=F)

gbmCrossVal(cv.folds = 5, data = train, n)

gbm.perf(boosting)


summary(boost)
plot(boost)


##### Fin de la sélection des hyperparamètres ###

# selection finale de modèles
final.rf = randomForest(like ~ ., data = train, mtry=7)
rf.pred  = final.rf %>% predict(test)
confusionMatrix(rf.pred, test$like)

boosting = gbm(like ~ ., data = train)

rf.pred  = final.rf %>% predict(test)
confusionMatrix(rf.pred, test$like)

