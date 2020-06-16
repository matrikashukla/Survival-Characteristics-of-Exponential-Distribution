n<-53
theta1<-1.31
p_cal<-0.05
x<-rexp(n,theta1);x
theta_ml<-1/mean(x);theta_ml
###testing for exponential population
###H0:the generated sample is from exponential population
ks.t<-ks.test(x,"pexp",theta_ml)$p.value;ks.t
if(ks.t<p_cal){
print("rejectH0")
}else{
print("cannot reject H0")
}
hist(x,prob=T,main="pdf of Exponential Distribution",xlab="time")
summary(x)
samp<-seq(min(x),max(x),length=n);samp
lines(samp,dexp(samp,theta1),col="red")
lines(samp,dexp(samp,theta_ml),col="green")

#SIMULATION
reject<-c()
theta_sim<-c()
m<-100
for(i in 1:m){
y<-rexp(m,theta1);y
th_ml<-1/mean(y)
theta_sim[i]<-th_ml
p_sim<-ks.test(y,"pexp",th_ml)$p.value;p_sim
if(p_sim<p_cal){
reject[i]<-0
}else{
reject[i]<-1
}
}
prob<-length(reject[reject=1])/m;prob
theta_ml_sim<-mean(theta_sim);theta_ml_sim
lines(samp,dexp(samp,theta_ml_sim),col="blue")
legend("topright",c(expression("sample"),eval(substitute(expression(theta==theta1),list(theta1=theta1)))
,eval(substitute(expression(theta[ml]==theta_ml),list(theta_ml=theta_ml)))
,eval(substitute(expression(theta[ml_sim]==theta_ml_sim),list(theta_ml_sim=theta_ml_sim)))),
col=c("black","red","green","blue"),lty=1)


###SURVIVAL CHARACTERISTICS
###PDF
pdf_1<-dexp(samp,theta1);pdf_1
pdf_2<-dexp(samp,theta_ml);pdf_2
pdf_3<-dexp(samp,theta_ml_sim);pdf_3

###CDF
cdf_1<-pexp(samp,theta1);cdf_1
cdf_2<-pexp(samp,theta_ml);cdf_2
cdf_3<-pexp(samp,theta_ml_sim);cdf_3

plot(samp,cdf_1,type="l",col="red",main="CDF",xlab="Time",ylab="cdf")
lines(samp,cdf_2,col="green")
lines(samp,cdf_3,col="blue")
legend("bottomright",c(eval(substitute(expression(theta==theta1),list(theta1=theta1)))
,eval(substitute(expression(theta[ml]==theta_ml),list(theta_ml=theta_ml)))
,eval(substitute(expression(theta[ml_sim]==theta_ml_sim),list(theta_ml_sim=theta_ml_sim)))),
col=c("red","green","blue"),lty=1)


#RELIABILITY
r1<-(1-cdf_1);r1
r2<-(1-cdf_2);r2
r3<-(1-cdf_3);r3

plot(samp,r1,type="l",col="red",main="RELIABILITY",xlab="Time",ylab="reliability")
lines(samp,r2,col="green")
lines(samp,r3,col="blue")
legend("topright",c(eval(substitute(expression(theta==theta1),list(theta1=theta1)))
,eval(substitute(expression(theta[ml]==theta_ml),list(theta_ml=theta_ml)))
,eval(substitute(expression(theta[ml_sim]==theta_ml_sim),list(theta_ml_sim=theta_ml_sim)))),
col=c("red","green","blue"),lty=1)

#HAZARD
h1<-pdf_1/r1;h1
h2<-pdf_2/r2;h2
h3<-pdf_3/r3;h3

plot(samp,h1,type="l",col="red",main="HAZARD",xlab="Time",ylab="hazard",ylim=c(1,2))
lines(samp,h2,col="green")
lines(samp,h3,col="blue")
legend("topright",c(eval(substitute(expression(theta==theta1),list(theta1=theta1)))
,eval(substitute(expression(theta[ml]==theta_ml),list(theta_ml=theta_ml)))
,eval(substitute(expression(theta[ml_sim]==theta_ml_sim),list(theta_ml_sim=theta_ml_sim)))),
col=c("red","green","blue"),lty=1)


#  CUMMULATIVE HAZARD
H1<--log(r1);H1
H2<--log(r2);H2
H3<--log(r3);H3

plot(samp,H1,type="l",col="red",main="CUMULATIVE HAZARD",xlab="time",ylab="H(t)",ylim=c(0,5))
lines(samp,H2,col="green")
lines(samp,H3,col="blue")
legend("bottomright",c(eval(substitute(expression(theta==theta1),list(theta1=theta1)))
,eval(substitute(expression(theta[ml]==theta_ml),list(theta_ml=theta_ml)))
,eval(substitute(expression(theta[ml_sim]==theta_ml_sim),list(theta_ml_sim=theta_ml_sim)))),
col=c("red","green","blue"),lty=1)