data=read_excel("dataTat.xls")

#1 D(4,6,3,2,1)  по средним значения
survey1=data[c("age", "satur","proc")]
survey1<-na.omit(survey1)
quant_age<-quantile(survey1$age, probs = seq(0, 1, 1/6))
survey1$age=cut(survey1$age, breaks = quant_age, labels = c(1:6), include.lowest=TRUE)
quant_satur=quantile(survey1$satur, probs = seq(0,1,1/4))
survey1$satur=cut(survey1$satur, breaks=quant_satur, labels=c(1:4), include.lowest=TRUE)
survey1<-survey1[order(survey1$age, survey1$satur),]

survey1_mean<-aggregate(survey1$proc, by=list(survey1$satur,survey1$age), FUN=mean) #усредняем
colnames(survey1_mean)[1]<-"satur"
colnames(survey1_mean)[2]<-"age"
colnames(survey1_mean)[3]<-"proc"
mean_table<-xtabs(proc~satur+age, data=survey1_mean);mean_table

#1.1 допустим дан конкретный дизайн (пропуски все на своих местах)
survey1_bibd=survey1_mean[-c(3,4,6,8,10,11,13,16,17,19,21,22),]
survey1_bibd_table<-xtabs(proc~satur+age, data=survey1_bibd);survey1_bibd_table

result<-anova_bibd(proc~satur+age,c(4,6,3,2,1), survey1_bibd);result

#1.2 перебираем все возможные подстановки #720 вычислений работает ~минуту на моем пк (i5-8265U)
p_values<-anova_multiple_bibd(proc~satur+age,c(4,6,3,2,1),data=survey1_mean)
boxplot(p_values, names=c("satur","age")) 


#2 D(4,4,3,3,2) по средним значениям, сразу по всевозможным перестановкам
survey_4x4=data[c("age", "satur","proc")]
survey_4x4<-na.omit(survey_4x4)
quant_age<-quantile(survey_4x4$age, probs = seq(0, 1, 1/4))
survey_4x4$age=cut(survey_4x4$age, breaks = quant_age, labels = c(1:4), include.lowest=TRUE)
quant_satur=quantile(survey_4x4$satur, probs = seq(0,1,1/4))
survey_4x4$satur=cut(survey_4x4$satur, breaks=quant_satur, labels=c(1:4), include.lowest=TRUE)
survey_4x4<-survey_4x4[order(survey_4x4$age, survey_4x4$satur),]

survey_4x4_mean<-aggregate(survey_4x4$proc, by=list(survey_4x4$satur,survey_4x4$age), FUN=mean)
colnames(survey_4x4_mean)[1]<-"satur"
colnames(survey_4x4_mean)[2]<-"age"
colnames(survey_4x4_mean)[3]<-"proc"

mean_table<-xtabs(proc~satur+age, data=survey_4x4_mean); mean_table

p_values<-anova_multiple_bibd(proc~satur+age,c(4,4,3,3,2),survey_4x4_mean)
boxplot(p_values, names=c("satur","age"),main="p-value distribution")
boxplot(p_values[,1],names=c("satur"),show.names=TRUE, main="p-value distribution")


#3 D(5,5,4,4,3) по средним значениям, есть пропуски

survey_5x5=data[c("age", "satur","proc")]
survey_5x5<-na.omit(survey_5x5)
quant_age<-quantile(survey_5x5$age, probs = seq(0, 1, 1/5))
survey_5x5$age=cut(survey_5x5$age, breaks = quant_age, labels = c(1:5), include.lowest=TRUE)
quant_satur=quantile(survey_5x5$satur, probs = seq(0,1,1/5))
survey_5x5$satur=cut(survey_5x5$satur, breaks=quant_satur, labels=c(1:5), include.lowest=TRUE)
survey_5x5<-survey_5x5[order(survey_5x5$age, survey_5x5$satur),]
survey_5x5_mean<-aggregate(survey_5x5$proc, by=list(survey_5x5$satur,survey_5x5$age), FUN=mean)
colnames(survey_5x5_mean)[1]<-"satur"
colnames(survey_5x5_mean)[2]<-"age"
colnames(survey_5x5_mean)[3]<-"proc"
mean_table<-xtabs(proc~satur+age, data=survey_5x5_mean)
mean_table_nofull<-mean_table

mean_table_nofull[2,1]<-NA; mean_table_nofull[4,5]<-NA; mean_table_nofull
survey_5x5_mean_nofull<-survey_5x5_mean[-c(2,24),] #сами добавляем пропуски

v<-5
b<-5
r<-4
k<-4
lambda<-3
design_5x5<-c(v,b,r,k,lambda)

anova_multiple_bibd(proc~satur+age,design_5x5, data=survey_5x5_mean_nofull, nofull=TRUE) #используем аргумент nofull=TRUE

#4 
#Рассмотрим дизайн $D(7,7,3,3,1)$

survey_7x7=data[c("age", "satur","proc")]
survey_7x7<-na.omit(survey_7x7)
quant_age<-quantile(survey_7x7$age, probs = seq(0, 1, 1/7))
survey_7x7$age=cut(survey_7x7$age, breaks = quant_age, labels = c(1:7), include.lowest=TRUE)
quant_satur=quantile(survey_7x7$satur, probs = seq(0,1,1/7))
survey_7x7$satur=cut(survey_7x7$satur, breaks=quant_satur, labels=c(1:7), include.lowest=TRUE)
survey_7x7<-survey_7x7[order(survey_7x7$age, survey_7x7$satur),]

survey1_mean<-aggregate(survey_7x7$proc, by=list(survey_7x7$satur,survey_7x7$age), FUN=mean, drop=FALSE)
colnames(survey1_mean)[1]<-"satur"
colnames(survey1_mean)[2]<-"age"
colnames(survey1_mean)[3]<-"proc"
mean_table<-xtabs(proc~satur+age, data=survey1_mean, addNA = TRUE);mean_table

#пример где передаем матрицу инцидентности дизайна (он не очевиден, построен)
incedence_matrix<-cbind(c(1,1,0,1,0,0,0),c(1,0,1,0,1,0,0),c(1,0,0,0,0,1,1), c(0,1,1,0,0,1,0), c(0,1,0,0,1,0,1),c(0,0,1,1,0,0,1),c(0,0,0,1,1,1,0)) #это схема дизайна, которую передаем в функцию
p_values<-anova_multiple_bibd(proc~satur+age, c(7,7,3,3,1), nofull=TRUE,inc_matrix = incedence_matrix, data=survey_7x7)
boxplot(p_values, names=c("satur","age"),main="p-value distribution")


#5 Пример с разным количеством наблюдений в одной ячейке

survey=data[c("age", "satur","proc")]
survey<-na.omit(survey)
survey$age=cut(survey$age, breaks=c(0,54,61,68,75,82, Inf), labels = c(1:6), include.lowest=TRUE) #44->54
survey$satur=cut(survey$satur, breaks=c(0,88,92,95,100), labels=c(1:4), include.lowest=TRUE)
survey<-survey[order(survey$age, survey$satur),]
xtabs(~satur+age, data=survey)

t<-find_biggest_t(proc~satur+age,c(4,6,3,2,1), survey) #находим максимальное t
counts <- ddply(survey, .(survey$age, survey$satur), nrow)
survey_delete_low<-survey[(rep(counts$V1,counts$V1)>=t),] #удаляем все меньшие t ячейки
xtabs(~satur+age, data=survey_delete_low)

#делаем все ячейки одной частоты случайно выкидывая наблюдения, если их больше t в ячейке
survey_one_freq<-make_one_freq(proc~satur+age,c(4,6,3,2,1), survey_delete_low, t)
xtabs(~satur+age, data=survey_one_freq)
survey_one_freq<-survey_one_freq[order(survey_one_freq$age, survey_one_freq$satur),]
#и наконец, по всем возможным перестановкам выполняем дисперсионный анализ

pv<-anova_multiple_bibd(proc~satur+age,c(4,6,3,2,1),survey_one_freq,T=t,nofull=TRUE)
boxplot(pv)


#проведем испытания монте-карло
satur<-vector()
age<-vector()
for (i in 1:100){
  survey_one_freq<-make_one_freq(proc~satur+age,c(4,6,3,2,1), survey_delete_low, t)
  survey_one_freq<-survey_one_freq[order(survey_one_freq$age, survey_one_freq$satur),]
  pv<-anova_multiple_bibd(proc~satur+age,c(4,6,3,2,1),survey_one_freq,T=t,nofull=TRUE)
  satur<-append(satur, mean(pv[,1]))
  age<-append(age, mean(pv[,2]))
}
boxplot(satur,names=c("satur"),show.names=TRUE)
boxplot(age,names=c("age"),show.names=TRUE)
