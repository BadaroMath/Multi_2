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

## Matriz de covariÃ¢ncias
mcov = cor(d3[,c(4,5,7,8)])



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


p = fviz_eig(d3.pca, addlabels = TRUE, ylim = c(0, 100));p

p1 <- fviz_pca_ind(d3.pca, label="none", habillage=d3.sexo,
                  addEllipses=F, ellipse.level=0.95);p1


p2 <- fviz_pca_biplot(d3.pca, label="var", habillage=d3.sexo,
                     addEllipses=T, ellipse.level=0.95);p2


p3 <- fviz_pca_biplot(d3.pca, label="var", habillage=d3$ETARIA,
                      addEllipses=T, ellipse.level=0.95);p3

p4 <- fviz_pca_biplot(d3.pca,axes=c(3,4), label="var", habillage=d3$ETARIA,
                      addEllipses=T, ellipse.level=0.95);p4

library("corrplot")
var <- get_pca_var(d3.pca)
corrplot(var$cos2, is.corr=F)

library(gridExtra)
d3.pca$rotation
a12 = fviz_cos2(d3.pca, choice = "var", axes = 1:2)+
  ylim(c(0,1))
a34 = fviz_cos2(d3.pca, choice = "var", axes = 3:4)+
  ylim(c(0,1))

grid.arrange(a12, a34, ncol=2)
