data("USArrests")
row.names(USArrests)
names(USArrests)
apply(USArrests,2,mean)
apply(USArrests,2,var)
pr.out <- prcomp(USArrests,scale=TRUE)
names(pr.out)
pr.out$center
pr.out$rotation 
pr.out$x
biplot(pr.out,scale = 0)
pr.out$sdev

pr.var = pr.out$sdev^2
pr.var

pve = pr.var/sum(pr.var)
pve

data("iris")
head("iris")
irisdata1 <- iris[,1:4]
irisdata1
princomp(irisdata1,cor=TURE,score=TRUE)