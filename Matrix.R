#Matrix
#row x columns

set.seed(1234)
(x = trunc(runif(24,100,500)))
x= sample(100:500,24)
(m1 = matrix(data=x, nrow=4,dimnames = list(c('delhi','mumbai','noida','chennai')
                                            ,paste('Prod',1:6,sep="-"))))
colMeans(m1);rowMeans(m1)
colSums(m1); rowSums(m1)
pie(x=rowMeans(m1))
pie(x=rowMeans(m1),labels = paste(rownames(m1),
                                  trunc((rowMeans(m1)/sum(rowMeans(m1)))*100),
                                  "%"))
barplot(rowMeans(m1))  # barplot for locations
barplot(colMeans(m1))  #barlplot for products
barplot(colMeans(m1), horiz = T)
barplot(colMeans(m1), horiz = T, col=1:6)
m1
#Subset a Matrix
m1[ , 1:2]
m1[ ,c(1,4)]
m1[c(1,3) ,c(1,4)]
m1[c('delhi','mumbai'),c('Prod-3')]
m1[m1 > 300]
m1
m1[c('delhi'),]
sd(m1[c('delhi'),])
sum(m1[c('delhi','mumbai'),c('Prod-3','Prod-4')])

#m end

(m2 = matrix(data=x, nrow=4, byrow = T))
(m3= matrix(x, ncol=4 ))
(m4 = matrix(c(1,2,3,4), nrow=2, ncol=4,byrow=T))
m1

# apply function

?apply

apply(X, MARGIN, FUN, ...)

apply(m1,2,mean) # 2 for columnwise

apply(m1,1,sum)  # 1 for rowise


