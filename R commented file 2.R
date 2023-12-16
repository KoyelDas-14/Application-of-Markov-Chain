setwd("C:\\Users\\Koyel Das\\Desktop\\Data Mining")
rmlist=ls()
s=c(1,2,3,4) # sample space
Xn=c() # Markov Chain
Xn0=Xn[1]=4 # as on the initial day stock will be always 4
P=matrix(c(0.5,0,0,0.5,0.25,0.5,0,0.25,0.25,0.25,0.5,0,0,0.25,0.25,0.5),byrow=TRUE,4,4)
P # TPM 
n=100 # Markov chain of length n
for(i in 2:n){
  if (Xn[i-1]==1) Xn[i]=sample(s,1,prob=P[1,])
  if (Xn[i-1]==2) Xn[i]=sample(s,1,prob=P[2,])
  if (Xn[i-1]==3) Xn[i]=sample(s,1,prob=P[3,])
  if (Xn[i-1]==4) Xn[i]=sample(s,1,prob=P[4,])
}
Xn
c=length(which(X==1))
pi_1=m/length(X) # probability that there is 1 vaccine on the given day
est_p=pi_1*0.25;est_p # point estimate of the probability that we will lose an order on a given day
m=1000 # number of samples
p_est=c()
p_1=c()
for(j in 1:m){
  X=c()
  X0=X[1]=4
  for(i in 2:n){
    if (X[i-1]==1) X[i]=sample(s,1,prob=P[1,])
    if (X[i-1]==2) X[i]=sample(s,1,prob=P[2,])
    if (X[i-1]==3) X[i]=sample(s,1,prob=P[3,])
    if (X[i-1]==4) X[i]=sample(s,1,prob=P[4,])
  }
  count=length(which(X==1))
  p_1[j]=count/length(X) # probability that there is 1 vaccine on the given day
  p_est[j]=p_1[j]*0.25  
}
p_est;p_1
table(p_est);table(p_1)
hist(p_est,prob=TRUE,main="Histogram of estd prob") # since p_est belongs to [0,1], it will not follow normal distribution but we can standardized it
pest_stand=(p_est-mean(p_est))/sd(p_est)
hist(pest_stand,prob=TRUE,main="Histogram of standardized estd prob")
curve(dnorm(x),col="red",add=TRUE)
lb=mean(pest_stand)-1.96*sd(pest_stand)
ub=mean(pest_stand)+1.96*sd(pest_stand)
abline(v=c(lb,ub),col="blue",lty=2)
lb1=lb*sd(p_est)+mean(p_est)
ub1=ub*sd(p_est)+mean(p_est)
lb1;ub1 # interval estimation of the estimated prob
abline(v=c(lb1,ub1),lty=c(2,2),col="blue")
(lb1+ub1)/2 # point estiamte
install.packages("markovchain")
library(markovchain)
dtmc=new("markovchain",transitionMatrix=P,states=c("1","2","3","4"),name="DTMC")
dtmc
plot(dtmc,main ="Diagrammatic Representation of TPM")
par(mfrow=c(1,2))
