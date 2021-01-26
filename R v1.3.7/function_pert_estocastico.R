#' @title Stochastic projects
#' @description This function calculates the average duration time for a stochastic project and the activities criticality index. It also plots the estimate density of the project duration, as well as the estimate density of the early and last times.
#' @param prec1and2 A matrix indicating the order of precedence type 1 and 2 between the activities (Default=matrix(0)). If value \eqn{(i,j)=1} then activity \eqn{i} precedes type \eqn{1} to \eqn{j}, and if \eqn{(i,j)=2} then activity \eqn{i} precedes type \eqn{2} to \eqn{j}. Cycles cannot exist in a project,  i.e. if an activity \eqn{i} precedes \eqn{j} then \eqn{j} cannot precede \eqn{i}.
#' @param prec3and4 A matrix indicating the order of precedence type 3 and 4 between the activities (Default=matrix(0)). If value \eqn{(i,j)=3} then activity \eqn{i} precedes type \eqn{3} to \eqn{j}, and if \eqn{(i,j)=4} then activity \eqn{i} precedes type \eqn{4} to \eqn{j}. Cycles cannot exist in a project,  i.e. if an activity \eqn{i} precedes \eqn{j} then \eqn{j} cannot precede \eqn{i}.
#' @param distribution Type of distribution that each activities' duration has. It can be NORMAL, TRIANGLE, EXPONENTIAL, UNIFORM, T-STUDENT, FDISTRIBUTION, CHI-SQUARED, GAMMA, WEIBULL, BINOMIAL, POISSON, GEOMETRIC, HYPERGEOMETRIC and EMPIRICAL.
#' @param values Matrix with the parameters corresponding to the distribution associated with the duration for each activity. Considering i as an activity we have the following cases. If the distribution is TRIANGLE, then (i, 1) it is the minimum value, (i, 2) the maximum value and (i, 3) the mode. If the distribution is NORMAL, (i, 1) is the mean and (i, 2) the variance. If the distribution is EXPONENTIAL, then (i, 1) is the \eqn{\lambda} parameter. If the distribution is UNIFORM, (i, 1) it is the minimum value and (i, 2) the maximum value. If the distribution is T-STUDENT, (i, 1) degrees of freedom and (i, 2) non-centrality parameter delta. In FDISTRIBUTION, (i, 1) and (i, 2) degrees of freedom and (i, 3) non-centrality parameter. In CHI-SQUARED, (i, 1) degrees of freedom and (i, 2) non-centrality parameter (non-negative). In GAMMA, (i, 1) and (i, 3) shape and scale parameters and (i, 2) an alternative way to specify the scale. In WEIBULL, (i, 1) and (i, 2) shape and scale parameters. In BINOMIAL, (i, 1) number of trials (zero or more) and (i, 2) probability of success on each trial. In POISSON, (i, 1) non-negative mean. In GEOMETRIC, (i, 1) probability of success in each trial, between 0 and 1. In HYPERGEOMETRIC, (i, 1) number of white balls in the urn, (i, 2) number of black balls in the urn and (i, 3) numer of balls drawn from the urn. Finally, if the distribution is EMPIRICAL, then (i,j), for all \eqn{j\in \{1,...,M\}} such that \eqn{M>0}, is the sample.
#' @param percentile Percentile used to calculate the maximum time allowed for the duration of the project (Default=0.95).
#' @param plot.activities.times Vector of selected activities to show the distribution of their early and last times (Default=NULL).
#' @param compilations Number of compilations that the function will use for average calculations (Default=1000).
#' @export
#' @return Two values, average duration time and the maximum time allowed, a critically index vector and a durations histogram.
#' @examples
#' prec1and2<-matrix(c(0,1,0,1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0),nrow=5,ncol=5,byrow=TRUE)
#' distribution<-c("TRIANGLE","TRIANGLE","TRIANGLE","TRIANGLE","EXPONENTIAL")
#' values<-matrix(c(1,3,2,1/2,3/2,1,1/4,9/4,1/2,3,5,4,1/2,0,0),nrow=5,byrow=TRUE)
#' percentile<-0.95
#' plot.activities.times<-c(1,4)
#' stochastic.pert(prec1and2=prec1and2,distribution=distribution,values=values,
#' percentile=percentile,plot.activities.times=plot.activities.times)
#'
stochastic.pert<-function(prec1and2=matrix(0),prec3and4=matrix(0),distribution,values,percentile=0.95,plot.activities.times=NULL,compilations=1000){

  or1<-order(organize(prec1and2,prec3and4)$Order[,2])
  or2<-organize(prec1and2,prec3and4)$Order[,2]
  precedence<-organize(prec1and2,prec3and4)$Precedence
  activities<-c(1:dim(precedence)[1])
  n<-length(activities)
  distribution<-distribution[or2]
  values<-values[or2,]
  if(is.null(plot.activities.times)==FALSE){
    T.E<-matrix(0,nrow=compilations,ncol=length(plot.activities.times))
    T.L<-matrix(0,nrow=compilations,ncol=length(plot.activities.times))
  }
  tiempo.computacion<-numeric(compilations)
  caminos.criticos<-matrix(0,nrow=compilations,ncol=length(activities))
  C<-numeric(length(activities))
  n<-length(activities)
  tiempo.early<-rep(0,n)
  tiempo.last<-rep(0,n)
  duration<-matrix(0,nrow=compilations,ncol=length(activities))

  ii<-as.logical(colSums(precedence))
  iii<-activities[ii]
  nn<-length(iii)
  if(nn>0){
  prec<-matrix(0,nrow=nn,ncol=n-1)
  for(j in 1:length(iii)){
    prec[j,1:length(which(precedence[,iii[j]]==1))]<-which(precedence[,iii[j]]==1)
  }
  prec<-prec[,as.logical(colSums(prec)),drop=FALSE]
  }
  ii1<-as.logical(rowSums(precedence))
  iii1<-activities[ii1]
  nn1<-length(iii1)
  if(nn1>0){
  prec1<-matrix(0,nrow=length(iii1),ncol=n-1)

  for(j in 1:length(iii1)){
    prec1[j,1:length(which(precedence[iii1[j],]==1))]<-which(precedence[iii1[j],]==1)
  }
  prec1<-prec1[,as.logical(colSums(prec1)),drop=FALSE]
  }

  for(j in 1:length(activities)){
    {
    if(distribution[j]=="NORMAL"){
      duration[,j]<-c(rnorm(compilations,values[j,1],values[j,2]))
    }
    else if(distribution[j]=="TRIANGLE"){
      duration[,j]<-c(rtriangle(compilations,values[j,1],values[j,2],values[j,3]))
    }
    else if(distribution[j]=="EXPONENTIAL"){
      duration[,j]<-c(rexp(compilations,values[j,1]))
    }
    else if(distribution[j]=="UNIFORM"){
      duration[,j]<-c(runif(compilations,values[j,1],values[j,2]))
    }
    else if(distribution[j]=="BETA"){
      duration[,j]<-c(rbeta(compilations,values[j,1],values[j,2]))
    }
    else if(distribution[j]=="EMPIRICAL"){
      duration[,j]<-rnorm(compilations,sample(values[j,],size=compilations,replace=TRUE),density(values[j,])$bw)
    }
    else if(distribution[j]=="T-STUDENT"){
        duration[,j]<-rt(compilations,df=values[j,1],ncp=values[j,2])
    }
      else if(distribution[j]=="FDISTRIBUTION"){
        duration[,j]<-rf(compilations,df1 =values[j,1],df2 =values[j,2],ncp=values[j,3])
      }
      else if(distribution[j]=="CHI-SQUARED"){
        duration[,j]<-rchisq(compilations,df =values[j,1],ncp=values[j,2])
      }
      else if(distribution[j]=="GAMMA"){
        duration[,j]<-rgamma(compilations,shape =values[j,1],rate =values[j,2],scale=values[j,3])
      }
      else if(distribution[j]=="WEIBULL"){
        duration[,j]<-rweibull(compilations,shape =values[j,1],scale=values[j,2])
      }
      else if(distribution[j]=="BINOMIAL"){
        duration[,j]<-rbinom(compilations,size =values[j,1],prob=values[j,2])
      }
      else if(distribution[j]=="POISSON"){
        duration[,j]<-rpois(compilations,lambda =values[j,1])
      }
      else if(distribution[j]=="GEOMETRIC"){
        duration[,j]<-rgeom(compilations,prob=values[j,1])
      }
      else if(distribution[j]=="HYPERGEOMETRIC"){
        duration[,j]<-rhyper(compilations,m=values[j,1],n=values[j,2],k=values[j,3])
      }
  }
}
  for(i in 1:compilations){

  if(nn>0){
    for(j in 1:length(iii)) {
      tiempo.early[iii[j]]=max(tiempo.early[prec[j,]]+duration[i,prec[j,]]);
    }
}

    tiempo.last[activities[ii1==FALSE]]<-max(tiempo.early+duration[i,])
    if(nn1>0){
    for(j in length(iii1):1) {
      tiempo.last[iii1[j]]=min(tiempo.last[prec1[j,]]-duration[i,prec1[j,]]);
    }
    }
    if(is.null(plot.activities.times)==FALSE){
    T.E[i,]<-tiempo.early[plot.activities.times]
    T.L[i,]<-tiempo.last[plot.activities.times]
    }
    tiempo.computacion[i]<-max(tiempo.early+duration[i,])
    caminos.criticos[i,]<-(tiempo.last-tiempo.early-duration[i,])

  }
  delta<-quantile(ecdf(tiempo.computacion),percentile,names=FALSE)

  caminos.criticos<-round(caminos.criticos,4)

  for(i in 1:length(activities)){
    C[i]<-length(which(caminos.criticos[,i]==0))
  }

  cat("Average time of the project = ", mean(tiempo.computacion), "\n")
  cat(" ","\n")
  cat("Percentile duration of the project = ", delta, "\n")
  A<-matrix(c((C[or1]/compilations)*100),ncol=length(activities),byrow=TRUE)
  colnames(A)=c(activities)
  rownames(A)=c("Criticality index by activity  ")
  cat(" ","\n")
  #hist(tiempo.computacion,freq=F,xlab="Project duration times",main="histogram",ylim=c(0,max(density(tiempo.computacion)$y)+0.2))
  {
  if(is.null(plot.activities.times)==FALSE){
  layout(matrix(c(1,1,2:(2*length(plot.activities.times)+1)), (length(plot.activities.times)+1), 2, byrow = TRUE))
  plot(density(tiempo.computacion),lwd=2,xlab="Project duration times",main="Density of project duration time")
  for(i in 1:length(plot.activities.times)){
    plot(density(T.E[,i]),lwd=2,xlab="Earliest start times",main=paste("Density of Earliest start time activity ",plot.activities.times[i]))
    plot(density(T.L[,i]),lwd=2,xlab="Latest completion times",main=paste("Density of Latest completion time activity ",plot.activities.times[i]))
  }
  }
    else{
      plot(density(tiempo.computacion),lwd=2,xlab="Project duration times",main="Density of project duration time")
    }
    }
  return(round(A,5))
}


