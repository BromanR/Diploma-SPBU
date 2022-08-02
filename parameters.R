#Моделируем данные
#и оцениваем параметры 

v<-4
b<-6
r<-3
k<-2
lambda<-1
T<-4
I<-4
J<-6

v_i<-c(-3,2,3,-2) #истинные значения
b_j<-c(-9,-6, -4, 3,6,10)
mu<-100

data<-rep(0,I*J*T)
x<-array(data,c(I,J,T))

for (i in 1:I){
  for (j in 1:J){
    for (t in 1:T){
      x[i,j,t]=mu+v_i[i]+b_j[j]+rnorm(1,mean = 0, sd=5)
    }
  }
}

x[3,1,]=NA; x[4,1,]=NA; x[2,2,]=NA;x[4,2,]=NA; x[2,3,]=NA; x[3,3,]=NA
x[1,4,]=NA; x[4,4,]=NA;x[1,5,]=NA; x[3,5,]=NA; x[1,6,]=NA; x[2,6,]=NA
x[is.na(x)]<-0
est.mu<-sum(x)/(b*k*T)
est.mu #оценка параметра mu
V_i<-rep(0,I)
for (i in 1:I){
  V_i[i]=sum(x[i,,])/T
}
B_j<-rep(0,J)
for (j in 1:J){
  B_j[j]=sum(x[,j,])/T
}
T_i=rep(0,I)                                                                
for (i in (1:I)){                                                           
  sum=0
  for (j in (1:J)){
    if(x[i,j,1]!=0){  
      sum=sum+B_j[j]
    }
  }
  T_i[i]=sum
}
est.v<-(k*V_i-T_i)/(lambda*v)
est.v  #оценка параметра v_i

sum_v_j<-rep(0,J)
for (j in (1:J)){
  for (i in (1:I)){
    if (x[i,j,1]!=0){
      sum_v_j[j]<-sum_v_j[j]+est.v[i] 
    }
  }
}
est.b<-(B_j-sum_v_j)/k-est.mu
est.b  #оценка параметра b_j

#Далее проведем дисперсионный анализ по этим данным
#Можно менять t, заново моделировать данные и смотреть как меняются результаты

colnames(x)<-c(1:b)                              
rownames(x)<-c(1:v)                             
df=as.data.frame.table(x)
df<-df[ , !(names(df) %in% "Var3")]
df<-df[order(df$Var2,df$Var1),]
df<-na.omit(df)

pv<-anova_multiple_bibd(Freq~Var1+Var2, c(4,6,3,2,1),df,T)
boxplot(pv, names=c("v","b"))
boxplot(pv[,2],names="b", show.names=TRUE)
