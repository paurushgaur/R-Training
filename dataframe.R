class(sname)
#dataframe

rollno = 1:10

sname= paste('student',1:10,sep= '-')
sample(c('M','F'),10,replace= T)
age= floor(runif(10,20,30))
  gender= c(rep('M',5),rep('F',5))
  course = sample(c('Eng','Med'),10,replace=T,
                  prob= c(.8,.2))
  table(course)
  married= sample(c(TRUE,FALSE),10,replace= T)

  #Data Frame
  (rollno = 1:10)
  (sname = paste('Student',1:10,sep='-'))
  (age = floor(runif(10, 20, 30)))
  (gender = c(rep('M',5),rep('F',5)))
  (course = sample(c('Engg','Medical','MBA'), 10, replace=T, prob=c(.3, .4, .3)))
  table(course)
  (married = sample(c(TRUE, FALSE), 10, replace=T))
  table(married)
  rollno; sname; age ; gender; course; married
  
  (df1 = data.frame(rollno, sname, age , gender, course, married))

  #Subset a dataframe  
  
  df1[1:2,3:4]
  df1$sname
  df1[df1$married==T,  ]
  df1[df1$course=='Engg' & df1$age > 25,  ]
  df1[df1$married==T & df1$course=='Engg' & df1$age > 25, c('sname') ]
  ?aggregate
  aggregate(df1$age, by=list(df1$gender), FUN=mean)
  aggregate(df1$age, by=list(df1$course), FUN=mean)
  aggregate(df1$age, by=list(df1$course, df1$gender), FUN=mean)
  
  
  lapply(X = df1,MARGIN = 2,FUN = sum)
        