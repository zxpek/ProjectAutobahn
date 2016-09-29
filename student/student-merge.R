d1=read.csv("student-mat.csv",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)

d3=rbind(d1,d2)
print(nrow(d3)) # 1044 students
