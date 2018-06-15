#Array
?array

array(data = NA, dim = length(data), dimnames = NULL)

#Comp- 2, prod-3 ,locations-4

(salesfig= floor(runif(24,70,100)))

a1 = array(data = salesfig, dim = c(4,3,2), dimnames = NULL)
Arrays
(salesfig = floor(runif(60, 70, 100)))
(a1 = array(data = salesfig, dim = c(4,3,5), dimnames = list(paste('Loc',1:4),
                                                             paste('Prod',1:3),
                                                             paste('Coy',1:5))))
apply(a1,1, sum)# sum locationwise
apply(a1,2, sum)
apply(a1,3, sum)
(ma1 = apply(a1,c(1,3), sum))
colSums(ma1)
rowSums(ma1)

apply(a1,c(2,3), sum)
