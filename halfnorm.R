#a<-(glm(am~qsec,data=mtcars,family="binomial"))

halfnorm<-function(glmObject,resType="deviance",nsim=100){
n<-dim(glmObject$data)[1]
library(fdrtool)
simRes<-list()
datTemp<-glmObject$data
resp<-strsplit(as.character(glmObject$formula),"~")[[2]]
for (i in 1:nsim){
  #Simulate response
  datTemp[[resp]]<-rbinom(n,1,fitted(glmObject))
  #Fit the model 
  b<-(glm(glmObject$formula,data=datTemp,family=glmObject$family$family))
  #Store the output
  simRes[[i]]<-sort(abs(residuals(b,resType)))
}
#Concatenate the results
results<-do.call(rbind,simRes)
#Get the quantiles that you want 
simEnvelope<-apply(results,2,function(x){quantile(x,c(0.025,0.5,0.975))})

#Plot it
plot(qhalfnorm((c(1:n)-0.5)/n),sort(abs(residuals(glmObject,"deviance"))),ylim=c(0,max(abs(residuals(glmObject,"deviance")))),xlab="Expected",ylab="Absolute value of deviance residuals",main="Half-normal probabilty plot")
points(qhalfnorm((c(1:n)-0.5)/n),simEnvelope[1,],type="l")
points(qhalfnorm((c(1:n)-0.5)/n),simEnvelope[3,],type="l")
points(qhalfnorm((c(1:n)-0.5)/n),simEnvelope[2,],type="l",lwd=3)
}


