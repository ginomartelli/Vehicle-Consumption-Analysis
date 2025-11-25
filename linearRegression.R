
#Création de la base de donnée selon l'énoncé

A = mtcars
F1 = as.factor(A[, 8])
A[, 8] = F1
set.seed(3)
set.seed(3 * floor(100 * runif(1, 0, 3)))
set1 = sample(1:32, 1)
B = A[-set1, ]
Y = B[, 1]  #ordonnée mpg
u = 1:11
v = u[-c(1, 8, 9)]
set2 = c(8, sample(v, 6, replace = FALSE))
X = B[, set2]





#Création des deux fonction permettant de trouver les coéfficient de la régression linéaire (simple et multiple)
#Implémentation des formules du cours
LR_simple = function(X, Y) {
  n = length(X[, 1])
  Xn = sum(X) / n
  Yn = sum(Y) / n
  a = (sum(X * Y) - n * Xn * Yn) / sum((X - Xn)^2)
  b = Yn - a * Xn
  return (c(b, a))
}

LR_mat = function(X, Y) {
  #X * C = Y on cherche C =  (X'*X)-1 * X' * Y
  ordonnéeOrigine = rep(1, length(X[, 1]))
  X = cbind(ordonnéeOrigine, X)
  if(is.matrix(X)==FALSE || is.numeric(X)==FALSE){
    X = as.matrix(sapply(X, as.numeric))
  }
  print(X)
  C = solve(t(X) %*% X) %*% t(X) %*% Y
  return (C)
}


V = LR_simple(X[3], Y) #Récuration des coefficients a et b de la régression linéire par rapport à la modalité n°3
# Transformation de la data frame X en matrice Xmat sans la première colone qui est vs qui représente la 
# forme du moteur et qui est une donnée qualitative binaire
Xmat = as.matrix(sapply(X, as.numeric))
Xmat = Xmat[,-1] 
x = Xmat[, 2]

xmoy = mean(x)
ymoy = mean(Y)
n = length(x)
a = V[2]
b = V[1]
Ye = V[2] * x + V[1] # Calcul de l'estimation de Y à l'aide des deux coefficients obtenus avec la fonction LR_simple
R2 = 1 - (sum((Ye - Y) * (Ye - Y))) / (sum((Y - ymoy) * (Y - ymoy))) #Implémentation de la formule du coefficient de détermination
R2
summary(lm(Y ~ x)) #vérification que le coefficient est bien le même que celui calculé par la fonction de R




# Ici nous allons affiché les points de notre tableau ainsi que la droite y = ax+b

plot(x,
     Y,
     xlab = "X",
     ylab = "mpg",
     main = "Droite y = ax + b")
x_vals = seq(min(x), max(x), length.out = 100)
y_vals = a * x_vals + b
lines(x_vals, y_vals, col = "blue", lwd = 2)


#Introduction de Xnew afin d'afficher l'intervale de confiance, qui sera ici de 95% donc alpha = 0.05

xnew = seq(min(x), max(x), length.out = length(x_vals))
vect = a * xnew + b
top = (xnew - xmoy)^2
buttom = sum((x - xmoy)^2)
t = 2.045 #loi de student pour t_0.975,30
sigma2 = sum((Y - Ye)^2) / n - 2
errpredict = sqrt(sigma2) * sqrt(1 + 1 / n + top / buttom)
bornesup = vect + t * errpredict
borneinf = vect - t * errpredict
lines(x_vals, borneinf, col = "green", lty = "dashed")
lines(x_vals, bornesup, col = "green", lty = "dashed")





#Ici on va afficher tous les modéles de régression linéaire 

Xplot = X[,-1]
Xplot
par(mfrow=c(3,2))
titres = c("mpg en fonction du qsec","mpg en fonction de la puissance","","","mpg en fonction du drat","mpg en fonction du poids")
abstitres = c("Puissance (chevaux)","","","Ratio du pont arrière","Poids")
for(i in 1:6){
  if (i==3){
    ind1 = which(Xplot[,3]==4)
    ind2 = which(Xplot[,3]==6)
    ind3 = which(Xplot[,3]==8)
    Y1 = Y[ind1]
    Y2 = Y[ind2]
    Y3 = Y[ind3]
    boxplot(Y1,Y2,Y3,names = c("4","6","8"),main = " mpg en fonction du nombre de cylindre", ylab = "mpg",xlab = "nombre de cylindre")
  }
  else if (i==4){
    ind1 = which(Xplot[,4]==3)
    ind2 = which(Xplot[,4]==4)
    ind3 = which(Xplot[,4]==5)
    Y1 = Y[ind1]
    Y2 = Y[ind2]
    Y3 = Y[ind3]
    boxplot(Y1,Y2,Y3,names = c("3","4","5"),main = " mpg en fonction du nombre de vitesse", ylab = "mpg",xlab = "nombre de vitesse")
    
  }

  
  
  else{
    coeff = LR_simple(Xplot[i],Y)
    coeff
    plot(Xplot[,i],Y,col="red",pch=16,ylab = "mpg",main = titres[i],xlab=abstitres[i])
    x=c(min(Xplot[,i]),max(Xplot[,i]),0.1)
    y=coeff[1] + coeff[2]*x
    lines(x,y,col="blue")
  }
}

par(mfrow=c(1,1))

ind1 = which(X[,1]==0)
ind2 = which(X[,1]==1)
Y1 = Y[ind1]
Y2 = Y[ind2]
boxplot(Y1,Y2,names = c("0","1"),main = " mpg en fonction du nombre de vs", ylab = "mpg",xlab = "forme du moteur")
  


# Création de la matrice X du cours permettant de valider l'hypothèse du bruit suivant les formules du diapo

par(mfrow=c(1,1))
ordonnéeOrigine = rep(1, length(X[, 1]))
Hmat = cbind(ordonnéeOrigine, x)
Hmat = Hmat %*% solve((t(Hmat) %*% Hmat)) %*% t(Hmat)
h = diag(Hmat)
h
sigma2 = sum((Y - Ye)^2) / n - 2
errsd = (Y - Ye) / (sqrt(sigma2) * sqrt((1 - h)))

errsd
qqnorm(errsd) # Tracé de erreurs en norme QQ, on doit avoir une courbe ressemblant à la fonction de répartition d'une Loi normale de paramètre (0,1)


# Ici on va ajouter des nouvelle donnée pour voir si notre modèle linéaire marche (prévision d'une nouvelle valeur)

Xmat2 = X
Xmat2
abscisse=2
n = length(Xmat2[,1])
indices = sample(1:n  , size = 0.8 * n) #permet de séparer la base de donnée en 80% train et 20% test
X_train = Xmat2[indices,]
X_test = Xmat2[-indices,]
Y_train = Y[indices]
Y_test = Y[-indices]
coeff = LR_simple(X_train[abscisse],Y_train)
Ye = coeff[1] + coeff[2]*Xmat
plot(X_train[,abscisse],Y_train)
x=c(min(X_train[,abscisse]),max(X_train[,abscisse]),0.1)
y=coeff[1] + coeff[2]*x
lines(x,y,col="blue")
points(X_test[,abscisse],Y_test,col="red",pch=16)

#Calul de la moyenne de l'erreur 
mean_absolute_error = sum(abs(Y_test - coeff[2]*X_test[,abscisse] - coeff[1]))/(n*0.2)
mean_squared_error = sum((Y_test - coeff[2]*X_test[,abscisse] - coeff[1])^2)/(n*0.2)  #risque empirique Rn
mean_absolute_error
mean_squared_error



# Validation de l'hypothèse du bruit par le teste de Kolmogorov-Smirnov

summary(lm(Y ~ X[,7]))
valuer = lm(Y ~ X[,7])
residu = rstudent((valuer))
plot(ecdf(residu), main = "Test de Kolmogorov-Smirnov",
     ylab = "Probabilités cumulées", xlab = "Valeurs",
     col = "red", lwd = 2,verticals = TRUE,do.points = FALSE)
curve(pnorm(x), add = TRUE, col = "blue", lwd = 2)
legend("topleft", legend = c("ECDF des résidus", "N(0,1) théorique"),
       col = c("red", "blue"), lwd = 2)






#########################################################################################################"

# Régression linéaire multiple
par(mfrow=c(1,1))

C = LR_mat(X, Y) # Coefficients pour les variables explicatives
n = length(X[, 6])
Xmat = as.matrix(sapply(X, as.numeric))
ymoy = mean(Y)
Ye = Xmat %*% C[-1] + C[1] #Estimation de Y en enlevant la variable vs 

# Calcul de R2 et R2 ajusté et vérification des résultats obtenus
R2 = 1 - (sum((Ye - Y) * (Ye - Y))) / (sum((Y - ymoy) * (Y - ymoy)))
R2a = 1 - (n - 1) * (1 - R2) / (n - qr(Xmat)$rank)
summary(lm(Y ~ Xmat))

# Création de la matrice X du cours permettant de valider l'hypothèse du bruit suivant les formules du diapo
ordonnéeOrigine = rep(1, length(Xmat[, 1]))
Hmat = cbind(ordonnéeOrigine, Xmat)
Hmat = Hmat %*% solve((t(Hmat) %*% Hmat)) %*% t(Hmat)
h = diag(Hmat)
sigma2 = sum((Y - Ye)^2) / n - 2
errsd = (Y - Ye) / (sqrt(sigma2) * sqrt((1 - h)))
qqnorm(errsd)


#Ci-dessous le code permettant d'obtenir les courbes de R2 et R2 ajusté en fonction du nombre de variable explicative
par(mfrow=c(1,1))
XmatR = as.matrix(sapply(X[-1], as.numeric))
adjR=leaps(XmatR,Y,method=c("adjr2"))# On récupère le tableau de r2 ajusté et r2 avec la fonction leaps 
R=leaps(XmatR,Y,method=c("r2"))
sizes = R$size
adjr2 = adjR$adjr2
r2 = R$r2

best_r2_size = tapply(r2 , sizes , max)
best_adjr2_size = tapply(adjr2 , sizes , max)

plot(seq(2,7,1),best_r2_size,
     xlim=c(1,7),
     ylim=c(min(best_r2_size),max(best_r2_size)),
     col="blue",type="l",
     xlab="Nombres de variable explicative",
     ylab="R squared",
     main ="Graphique représentant R2 et R2 ajusté en fonction du nombre de variables explicatives"
)
lines(seq(2,7,1),best_adjr2_size,col="red")
legend("topleft",legend=c("R2","adjR2"),col = c("blue", "red"),lty = 1 )
#Ce qui va nous permettre de faire une sélection de variables
# A l'aide du afjR on va donc pouvoir faire la sélection et le tableau que nous obtenons nous dit que
# ce sont les variables correspondant à qsec, gear et wt qui nous permettent d'obtenir le meilleur R2 ajusté 
# et donc le meilleur modéle linéaire


# Calcul de la moyenne des erreurs pour la régression multiple
coeff = LR_mat(X_train,Y_train)

X_test = as.matrix(sapply(X_test, as.numeric))
mean_absolute_error = sum(abs(Y_test - X_test %*% coeff[-1] - coeff[1]))/(n*0.2)
mean_squared_error = sum((Y_test - X_test %*% coeff[-1] - coeff[1])^2)/(n*0.2) #risque empirique Rn
mean_absolute_error
mean_squared_error

