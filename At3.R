library(readxl)
library(factoextra)
library(FactoMineR)
library(writexl)
library(readxl)
library(devtools)
source('https://raw.githubusercontent.com/vqv/ggbiplot/master/R/ggbiplot.r')
source('https://raw.githubusercontent.com/vqv/ggbiplot/master/R/ggscreeplot.r')

#######################################


d3 = as.data.frame(read_xlsx("d3.xlsx"))
d3$ETARIA = as.factor(d3$ETARIA)

## Matriz de covari√¢ncias
mcov = cov(d3[,c(4,5,7,8)])



## autovalores e autovetores
eig = eigen(mcov);eig
write_xlsx(as.data.frame(aval), "autovalores.xlsx")
aval = eig$values;aval


# autovalores
st = sum(aval);st

v=0
for (i in 1:4) {
  v[i] = aval[i]/st
};v
write_xlsx(as.data.frame(v), "v.xlsx")

# autovetores
avet = eig$vectors;avet
tavet = t(avet)
write_xlsx(as.data.frame(tavet), "autovetores_tranposto.xlsx")
r=matrix(NA, nrow = 4, ncol = 4)
for (i in 1:4) {
  for (j in 1:4) {
    r[i,j] = (tavet[i,j]*sqrt(aval[i]))/sqrt(mcov[j,j])
  }
};t(r)
write_xlsx(as.data.frame(t(r)), "t(r).xlsx")


#### Graficos
d3.pca <- prcomp(d3[,c(4,5,7,8)], center = TRUE,scale. = TRUE)
summary(d3.pca)
d3.sexo <- c(rep("Feminino", 40), rep("Masculino",40))

g1 = ggbiplot(d3.pca)+
  geom_point(fill = "powderblue", colour = "dodgerblue3", shape = 21, size = 2)+
  theme_minimal(); g1

g2 = ggbiplot(d3.pca, ellipse = TRUE, labels=substring(d3[,1],1,1), groups = d3.sexo)+
  theme_minimal(); g2 + labs(colour = "Sexo")

g3 = ggbiplot(d3.pca,ellipse=TRUE,  labels = substring(d3[,1],1,1), groups = d3$ETARIA)+
  theme_minimal(); g3 + labs(colour = "Faixa et·ria")

g4 = ggbiplot(d3.pca,ellipse=TRUE, choices = c(3,4), labels = substring(d3[,1],1,1), groups = d3$ETARIA)+
  theme_minimal(); g4 + labs(colour = "Faixa et·ria")

pca=PCA(d3[,c(4,5,7,8)], graph=T)


pcacor=PCA(d3[,c(4,5,7,8)], scale.unit = T,graph=TRUE)
fviz_pca_var(d3.pca, col.var="contrib")+
  theme_minimal()
