#factor

#category type- ordered or unordered
#Factors
df1
# category type - ordered or unordered
#gender,  course, color - unordered
#grades, division, position,likertscale, ratings
summary(df1)
(grades = sample(c('A', 'B', 'C'),size=10, replace=T, prob=c(.4,.3,.3)))

df1$grades = grades
df1
summary(df1)
df1$gender = factor(df1$gender)
summary(df1)
df1$grades =factor(df1$grades, ordered=T)
df1$grades
aggregate(df1$age, by=list(df1$grades), mean)
aggregate(df1$age, by=list(df1$gender), mean)
aggregate(df1$age, by=list(df1$course), mean)
(df1$grades =factor(df1$grades, ordered=T, levels=c('C','B','A')))

(division = sample(c('Excellent', 'Very Good', 'Sat'),size=10, replace=T, prob=c(.4,.3,.3)))
division
summary(division)
Fdivision = factor(division)
summary(Fdivision)
Fdivision2 = factor(division, ordered=T, levels=c('Sat', 'Very Good', 'Excellent'))
summary(Fdivision2)
Fdivision2
Fdivision3 = factor(division, ordered=T)
summary(Fdivision3)
Fdivision3
#factors end
