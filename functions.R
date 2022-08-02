library (matrixcalc)
library(readxl)
library(plyr)
library(dplyr)



# Функция дисперсионного анализа данных df по формуле formula на дизайне design.
# T -- кол-во наблюдений в каждой ячейке

anova_bibd<-function(formula,design, df,T=1){                                      
  x<-all.vars(formula)[1]                                                       #зависимая переменная
  variable<-all.vars(formula)[2]                                                #независимая переменная (строки)
  block<-all.vars(formula)[3]                                                   #независимая перменная (блоки)
  v=design[1]; b=design[2]; r=design[3]; k=design[4]; lambda=design[5]
  df.table<-xtabs(df[[x]]~df[[variable]]+df[[block]])                           #табличный вид, элементы x_ij берутся отсюда
  
  est.mu=sum(df[[x]])/(b*k*T)                                                   #оценка общего среднего
  
  V_i<-rep(0,v)
  for (i in 1:v){
    V_i[i]=sum(filter(df, df[[variable]]==i)[[x]])/T                            #сумма по строкам
  }
  
  B_j=rep(0,b)
  for (j in (1:b)){
    B_j[j]=sum(filter(df, df[[block]]==j)[[x]])/T                               #сумма по блокам
  }
  
  T_i=rep(NA,v)                                                                 #сумма сумм по блокам, в которых присутсвует
  for (i in (1:v)){                                                             #элемент из данной строки 
    sum=0
    for (j in (1:b)){
      if(df.table[i,j]!=0){
        sum=sum+B_j[j]
      }
    }
    T_i[i]=sum
  }
  
  est.v<-(k*V_i-T_i)/(lambda*v)                                                 #оценка v_i
  
  
  SSt=sum((df[,x]-est.mu)^2)                                                    #общая сумма квадратов 
  SSb=k*T*sum((B_j/k-est.mu)^2)                                                 #cумма квадартов по блокам
  SSv=lambda*v*T/k*sum(est.v^2)                                                 #сумма квадратов по строкам
  
  v_e<-rep(0,b)
  for (j in 1:b){
    for (i in 1:v){
      if(df.table[i,j]!=0){
        v_e[j]=v_e[j]+est.v[i]
      }  
    }
  }
  SSe=0
  
  #z=1                                                                          #здесь непосредственно считается SSe
  #for (j in (1:b)){                                                            #можно проверить, что результат не меняется
  #  for (i in (1:v)){
  #    for (t in (1:T)){
  #      if(sum(filter(df, df[[block]]==j & df[[variable]]==i)[[x]])!=0){  #Быстрее: if(df.table[i,j]!=0)
  #        SSe=SSe+(df[[x]][z]-(est.v[i]-v_e[j]/k)-B_j[j]/k)^2
  #        z=z+1
  #      }
  #    }
  #  }
  #}
  SSe=SSt-SSv-SSb                                                               #сумма квадратов ошибки
  
  MSv=SSv/(v-1)
  MSb=SSb/(b-1)
  MSr=SSe/(b*k*T-v-b+1) 
  Fv=MSv/MSr
  Fb=MSb/MSr
  
  p_vaule_v=1-pf(Fv, v-1,b*k*T-v-b+1) 
  p_value_b=1-pf(Fb, b-1, b*k*T-v-b+1) 
  
  tab<-matrix(c(v-1,b-1,b*k*T-v-b+1,SSv,SSb,SSe,MSv,MSb,MSr,Fv,Fb,NA,p_vaule_v,p_value_b,NA),ncol=5)
  colnames(tab)<-c("df","Sum Sq", "Mean Sq", "F value", "Pr(>F)")
  rownames(tab)<-c(variable,block, "error")
  tab
}

#Функция выдает матрицу всевозможных перестановок n элементов
permutations <- function(n){          
  if(n==1){                                                                 
    return(matrix(1))
  } else {
    sp <- permutations(n-1)
    p <- nrow(sp)
    A <- matrix(nrow=n*p,ncol=n)
    for(i in 1:n){
      A[(i-1)*p+1:p,] <- cbind(i,sp+(sp>=i))
    }
    return(A)
  }
}

#Функция считающая дисперисонный анализ по формуле formula на блок-схеме design
#по всем возможнным перестановкам
#Возвращает список p-value
#T -- кол-во наблюдений в каждрй ячейке
#nofull -- указать TRUE, если в данных есть пропуски
#inc_matrix -- матрица инцидентности дизайна, если не указана, то строит дизайн комбинаторно
anova_multiple_bibd<-function(formula, design,data,T=1, ... ,nofull=FALSE, inc_matrix){
  x<-all.vars(formula)[1]                                                       #зависимая переменная
  treatment<-all.vars(formula)[2]                                                #независимая переменная (строки)
  block<-all.vars(formula)[3]                                                   #независимая перменная (блоки)
  v<-design[1]
  b<-design[2]
  r<-design[3]
  k<-design[4]
  lambda<-design[5]
  
  perm_matrix<-permutations(b)
  incidence_matrix<-matrix(0,nrow=v, ncol=b)
  
  if (missing(inc_matrix)){
    library(gtools)
    pos_bibd <- combinations(v,k)                                                  
    for (i in 1:b){
      incidence_matrix[pos_bibd[i,],i]=1;
    }
  }
  else incidence_matrix<-inc_matrix
  
  list_of_pvalue<-list()
  p_values_treatment<-vector()
  p_values_block<-vector()        
  
  
  data$T<-rep(c(1:T),nrow(data)/T) 
  data.table=xtabs(data[[x]]~data[[treatment]]+data[[block]]+data[["T"]])
  data.freq=xtabs(~data[[treatment]]+data[[block]])
  data.table[data.freq==0]=NA
  
  if (nofull){
    data_table_incidence<-matrix(0,nrow=v,ncol=b)
    data_table_incidence[!is.na(data.table[,,1])]<-1 
    data_table_incidence[is.na(data.table[,,1])]<-0
  }
  
  
  for (i in 1:factorial(b)){  #идем по каждой перестановке
    if (nofull){
      if (sum(hadamard.prod(data_table_incidence,incidence_matrix[ ,perm_matrix[i,] ]))!=b*k) next
    }
    bibd_table<-matrix(NA,nrow=v,ncol=b) 
    incidence_matrix[incidence_matrix==0]=NA                                    
    bibd_table<-hadamard.prod(data.table[,,1],incidence_matrix[,perm_matrix[i,]])
    if (T>1){
      for (t in 2:T){
        tmp<-hadamard.prod(data.table[,,t],incidence_matrix[,perm_matrix[i,]])
        bibd_table<-array(c(bibd_table, tmp), dim=c(v,b,t))
      }
    }                                       #формируем данные для конкретной подстановки дизайн
    colnames(bibd_table)<-c(1:b)                                   #подготовительные действия 
    rownames(bibd_table)<-c(1:v)                                    #для применения функции
    df=as.data.frame.table(bibd_table)
    df<-df[ , !(names(df) %in% "Var3")]
    df=na.omit(df)
    colnames(df)[1]<-treatment
    colnames(df)[2]<-block 
    colnames(df)[3]<-x
    df<-df[order(df[[block]],df[[treatment]]),] 
    
    result<-anova_bibd(formula,design, df,T) #считает дисп. анализ конкретного дизайна
    p_values_treatment<-append(p_values_treatment, result[treatment,"Pr(>F)"])
    p_values_block<-append(p_values_block, result[block,"Pr(>F)"])
    p_values<-cbind(p_values_treatment,p_values_block)
    incidence_matrix[is.na(incidence_matrix)]=0                   
  } 
  colnames(p_values)<-c(treatment, block)
  return(p_values)
}


#Если данные df с разным количеством наблюдений в каждой ячейке
#Функция находящая максимальное t=количество элементов в ячейке, которое 
#может получиться при разных подстановках данных в дизайн design
find_biggest_t<-function(formula, design, df){
  x<-all.vars(formula)[1]                                                       #зависимая переменная
  variable<-all.vars(formula)[2]                                                #независимая переменная (строки)
  block<-all.vars(formula)[3]                                                   #независимая перменная (блоки)
  v<-design[1]
  b<-design[2]
  r<-design[3]
  k<-design[4]
  lambda<-design[5]
  
  
  perm_matrix<-permutations(b)
  incidence_matrix<-matrix(0,nrow=v, ncol=b)
  data.table=xtabs(~df[[variable]]+df[[block]])
  library(gtools)
  pos_bibd <- combinations(v,k)                                                  
  for (i in 1:b){
    incidence_matrix[pos_bibd[i,],i]=1;
  }
  maxT<-0
  for (i in 1:factorial(b)){ 
    bibd_table<-matrix(NA,nrow=v,ncol=b) 
    incidence_matrix[incidence_matrix==0]=NA                                      #часть костыля
    bibd_table<-hadamard.prod(data.table,incidence_matrix[,perm_matrix[i,]])
    minT<-99
    for (j in 1:b){
      for (i in 1:v){
        if (!is.na(bibd_table[i,j])){
          if (bibd_table[i,j]<minT){
            minT<-bibd_table[i,j]
          }
        }
      }
    }
    if (minT>maxT){
      maxT<-minT
    }
  }
  maxT  
}

#Если данные df с разным количеством наблюдений в каждой ячейке
#Функция возвращающая dataframe с одинаковым количеством t наблюдений в ячейке 
#Выкидывает наблюдения случайный образом, если в ячейке больше t наблюдений
make_one_freq<-function(formula,design,df, t){
  x<-all.vars(formula)[1]                                                       #зависимая переменная
  treatment<-all.vars(formula)[2]                                                #независимая переменная (строки)
  block<-all.vars(formula)[3]   
  
  freq<-xtabs(~df[[treatment]]+df[[block]])
  freq_vect<-c(freq)
  freq_vect<-freq_vect[freq_vect>0]
  v=design[1]; b=design[2]; k=design[4];
  df<-df[order(df[[block]],df[[treatment]]),]
  rem_rows<-vector()
  pos<-0
  for (i in 1:length(freq_vect)){
    rem_rows<-append(rem_rows, pos+sample.int(freq_vect[i],t))
    pos<-pos+freq_vect[i]
  }
  
  survey_one_freq<-df[rem_rows,]
}