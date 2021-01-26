#' @title Problems of distribution of delay in stochastic projects
#' @description This function calculates the delay of a stochastic project, once it has been carried out. In addition, it also calculates the distribution of the delay on the different activities with the Stochastic Shapley rule.
#' @param prec1and2 A matrix indicating the order of precedence type 1 and 2 between the activities (Default=matrix(0)). If value \eqn{(i,j)=1} then activity \eqn{i} precedes type \eqn{1} to \eqn{j}, and if \eqn{(i,j)=2} then activity \eqn{i} precedes type \eqn{2} to \eqn{j}. Cycles cannot exist in a project,  i.e. if an activity \eqn{i} precedes \eqn{j} then \eqn{j} cannot precede \eqn{i}.
#' @param prec3and4 A matrix indicating the order of precedence type 3 and 4 between the activities (Default=matrix(0)). If value \eqn{(i,j)=3} then activity \eqn{i} precedes type \eqn{3} to \eqn{j}, and if \eqn{(i,j)=4} then activity \eqn{i} precedes type \eqn{4} to \eqn{j}. Cycles cannot exist in a project,  i.e. if an activity \eqn{i} precedes \eqn{j} then \eqn{j} cannot precede \eqn{i}.
#' @param distribution Type of distribution that each activities' duration has. It can be NORMAL, TRIANGLE, EXPONENTIAL, UNIFORM, T-STUDENT, FDISTRIBUTION, CHI-SQUARED, GAMMA, WEIBULL, BINOMIAL, POISSON, GEOMETRIC, HYPERGEOMETRIC and EMPIRICAL.
#' @param values Matrix with the parameters corresponding to the distribution associated with the duration for each activity. Considering i as an activity we have the following cases. If the distribution is TRIANGLE, then (i, 1) it is the minimum value, (i, 2) the maximum value and (i, 3) the mode. If the distribution is NORMAL, (i, 1) is the mean and (i, 2) the variance. If the distribution is EXPONENTIAL, then (i, 1) is the \eqn{\lambda} parameter. If the distribution is UNIFORM, (i, 1) it is the minimum value and (i, 2) the maximum value. If the distribution is T-STUDENT, (i, 1) degrees of freedom and (i, 2) non-centrality parameter delta. In FDISTRIBUTION, (i, 1) and (i, 2) degrees of freedom and (i, 3) non-centrality parameter. In CHI-SQUARED, (i, 1) degrees of freedom and (i, 2) non-centrality parameter (non-negative). In GAMMA, (i, 1) and (i, 3) shape and scale parameters and (i, 2) an alternative way to specify the scale. In WEIBULL, (i, 1) and (i, 2) shape and scale parameters. In BINOMIAL, (i, 1) number of trials (zero or more) and (i, 2) probability of success on each trial. In POISSON, (i, 1) non-negative mean. In GEOMETRIC, (i, 1) probability of success in each trial, between 0 and 1. In HYPERGEOMETRIC, (i, 1) number of white balls in the urn, (i, 2) number of black balls in the urn and (i, 3) numer of balls drawn from the urn. Finally, if the distribution is EMPIRICAL, then (i,j), for all \eqn{j\in \{1,...,M\}} such that \eqn{M>0}, is the sample.
#' @param observed.duration  Vector with the observed duration for each activity.
#' @param percentile Percentile used to calculate the maximum time allowed for the duration of the project (Default=NULL). Only percentile or delta is necessary. This value is only used if the function uses the default cost function.
#' @param delta   Maximum time allowed for the duration of the project (Default=NULL). Only delta or pencetile is necessary. This value is only used if the function uses the default cost function.
#' @param cost.function Delay costs function. If this value is not added, a default cost function will be used.
#' @param compilations Number of compilations that the function will use for average calculations (Default=1000).
#' @details Given a problem of sharing delays in a stochastic project \eqn{(N,\prec,\{X_i\}_{i\in N},\{x_i\}_{i\in N})},  such that \eqn{\{X_i\}_{i\in N}} is the random variable of activities' durations and \eqn{\{x_i\}_{i\in N}} the observed value. It is defined as \eqn{E(D(N,\prec,\{X_i\}_{i\in N}))} the expected project time, where \eqn{E} is the mathematical expectation, and \eqn{D(N,\prec,\{x_i\}_{i\in N})} the observed project time, then \eqn{d=D(N,\prec,\{X_i\}_{i\in N})-\delta}, with \eqn{\delta>0}, normally \eqn{\delta>E(D(N,\prec,\{X_i\}_{i\in N}))}, is the delay. The proportional and truncated proportional rule, see delay.pert function, can be adapted to this context by using the mean of the random variables.
#'
#'The Stochastic Shapley rule is based on the Shapley value for the TU game \eqn{(N,v)} where \eqn{v(S)=E(C(D(N,\prec,(\{X_i\}_{i\in N\backslash S},\{x_i\}_{i\in S})))}, for all \eqn{S\subseteq N}, where \eqn{C} is the costs function (by default \eqn{C(y)=D(N,\prec,y)-\delta}).  If the number of activities is greater than ten, the Shapley value, of the game \eqn{(N,v)}, is estimated using a unique sampling process for all players, see \cite{Castro et al. (2009)}.
#'
#'The Stochastic Shapley rule 2 is based on the sum of the Shapley values for the TU games \eqn{(N,v)} and \eqn{(N,w)} where \eqn{v(S)=E(C(D(N,\prec,(\{X_i\}_{i\in N\backslash S},\{x_i\}_{i\in S}))))-E(C(D(N,\prec,(\{X_i\}_{i\in N}))))} and \eqn{w(S)=E(C(D(N,\prec,(\{0_i\}_{i\in N\backslash S},\{X_i\}_{i\in S}))))}, for all \eqn{S\subseteq N}, \eqn{0_N} denotes the vector in \eqn{R^N} whose components are equal to zero and where \eqn{C} is the costs function (by default \eqn{C(y)=D(N,\prec,y)-\delta}).
#'
#' @export
#' @references
#' \describe{
#'   \item{}{Castro, J., Gómez, D., & Tejada, J. (2009). Polynomial calculation of the Shapley value based on sampling. Computers & Operations Research, 36(5), 1726-1730.}
#'   \item{}{Gonçalves-Dosantos, J.C., García-Jurado, I., Costa, J. (2018) Sharing delay costs in Stochastic projects.}
#' }
#' @return  A delay value and solution vector.
#' @examples
#'
#' prec1and2<-matrix(c(0,1,0,1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0),nrow=5,ncol=5,byrow=TRUE)
#' distribution<-c("TRIANGLE","TRIANGLE","TRIANGLE","TRIANGLE","EXPONENTIAL")
#' values<-matrix(c(1,3,2,1/2,3/2,1,1/4,9/4,1/2,3,5,4,1/2,0,0),nrow=5,byrow=TRUE)
#' observed.duration<-c(2.5,1.25,2,4.5,3)
#' percentile<-NULL
#' delta<-6.5
#' delay.stochastic.pert(prec1and2=prec1and2,distribution=distribution,values=values,
#' observed.duration=observed.duration,percentile=percentile,delta=delta,
#' cost.function=NULL,compilations=1000)
#'

delay.stochastic.pert<-function(prec1and2=matrix(0),prec3and4=matrix(0),distribution,values,observed.duration,percentile=NULL,delta=NULL,cost.function=NULL,compilations=1000){

  or1<-order(organize(prec1and2,prec3and4)$Order[,2])
  or2<-organize(prec1and2,prec3and4)$Order[,2]
  precedence<-organize(prec1and2,prec3and4)$Precedence
  distribution<-distribution[or2]
  values<-values[or2,]
  observed.duration<-observed.duration[or2]
  activities<-1:length(distribution)
  tiempo.computacion<-numeric(compilations)
  tiempo.computacion2<-numeric(compilations)
  n<-length(activities)
  tiempo.early<-rep(0,n)
  tiempo.early2<-rep(0,n)
  tiempo2<-numeric(n)
  tiempo3<-numeric(n)
  tiempo4<-numeric(n)
  duration<-matrix(0,nrow=compilations,ncol=length(activities))
  v<-numeric(2^n-1)
  w<-numeric(2^n-1)
  v1<-numeric(2^n-1)
  w1<-numeric(2^n-1)
  durations.means<-numeric()
  Prop<-numeric()
  TProp<-numeric()
  ii<-as.logical(colSums(precedence))
  iii<-activities[ii]
  nn<-length(iii)

  if(nn>0){
  prec<-matrix(0,nrow=nn,ncol=n-1)
  for(j in 1:nn){
    prec[j,1:length(which(precedence[,iii[j]]==1))]<-which(precedence[,iii[j]]==1)
  }
  prec<-prec[,as.logical(colSums(prec)),drop=FALSE]


  for(i in 1:nn) {
    tiempo.early[iii[i]]=max(tiempo.early[prec[i,]]+observed.duration[prec[i,]]);
  }
  }
  for(j in 1:n){
    {
    if(distribution[j]=="NORMAL"){
      duration[,j]<-c(rnorm(compilations,values[j,1],values[j,2]))
      durations.means[j]<-values[j,1]
    }
    else if(distribution[j]=="TRIANGLE"){
      duration[,j]<-c(rtriangle(compilations,values[j,1],values[j,2],values[j,3]))
      durations.means[j]<-(values[j,1]+values[j,2]+values[j,3])/3
    }
    else if(distribution[j]=="EXPONENTIAL"){
      duration[,j]<-c(rexp(compilations,values[j,1]))
      durations.means[j]<-1/values[j,1]
    }
    else if(distribution[j]=="UNIFORM"){
      duration[,j]<-c(runif(compilations,values[j,1],values[j,2]))
      durations.means[j]<-(values[j,1]+values[j,2])/2
    }
    else if(distribution[j]=="BETA"){
      duration[,j]<-c(rbeta(compilations,values[j,1],values[j,2]))
      durations.means[j]<-values[j,1]/(values[j,1]+values[j,2])
    }
    else if(distribution[j]=="EMPIRICAL"){
      duration[,j]<-  rnorm(compilations,sample(values[j,],size=compilations,replace=TRUE),density(values[j,])$bw)
      duration.means[j]<-mean(values[j,])
    }
      else if(distribution[j]=="T-STUDENT"){
        duration[,j]<-rt(compilations,df=values[j,1],ncp=values[j,2])
        duration.means[j]<-mean(duration[,j])
      }
      else if(distribution[j]=="FDISTRIBUTION"){
        duration[,j]<-rf(compilations,df1 =values[j,1],df2 =values[j,2],ncp=values[j,3])
        {
        if(values[j,2]>2){duration.means[j]<-values[j,1]/(values[j,2]-2)}
        else{duration.means[j]<-mean(duration[,j])}
          }
      }
      else if(distribution[j]=="CHI-SQUARED"){
        duration[,j]<-rchisq(compilations,df =values[j,1],ncp=values[j,2])
        duration.means[j]<-values[j,1]
      }
      else if(distribution[j]=="GAMMA"){
        duration[,j]<-rgamma(compilations,shape =values[j,1],rate =values[j,2],scale=values[j,3])
        duration.means[j]<-values[j,1]*values[j,3]
      }
      else if(distribution[j]=="WEIBULL"){
        duration[,j]<-rweibull(compilations,shape =values[j,1],scale=values[j,2])
        duration.means[j]<-mean(duration[,j])
      }
      else if(distribution[j]=="BINOMIAL"){
        duration[,j]<-rbinom(compilations,size =values[j,1],prob=values[j,2])
        duration.means[j]<-values[j,1]*values[j,2]
      }
      else if(distribution[j]=="POISSON"){
        duration[,j]<-rpois(compilations,lambda =values[j,1])
        duration.means[j]<-values[j,1]
      }
      else if(distribution[j]=="GEOMETRIC"){
        duration[,j]<-rgeom(compilations,prob=values[j,1])
        duration.means[j]<-1/values[j,1]
      }
      else if(distribution[j]=="HYPERGEOMETRIC"){
        duration[,j]<-rhyper(compilations,m=values[j,1],n=values[j,2],k=values[j,3])
        duration.means[j]<-values[j,3]*(values[j,1]/(values[j,1]+values[j,2]))
      }
    }
  }

  duracion.total.T<-max(tiempo.early+observed.duration)

  if(is.null(cost.function)==TRUE){
  if(is.null(percentile)==TRUE && is.null(delta)==TRUE){
    cat("necessary delta or percentile to continue", "\n")
    cat("Continue with percentile 0.95? (Y = Yes, N = No) \n")
    continue <- scan(what = "character", n = 1)
    {
      if(continue=="N"){
        stop("necessary delta or percentile")
      }
      else{
        percentile<-0.95
      }
    }

  }



  if(is.null(percentile)==FALSE && is.null(delta)==FALSE){
    cat("only one is necessary to continue", "\n")
    cat("Continue with percentile or delta? (P = PERCENTILE, D = DELTA) \n")
    continue <- scan(what = "character", n = 1)

    if(continue=="P"){
      delta<-NULL
    }
    else{
      percentile<-NULL
    }
  }

  if(is.null(percentile)==FALSE){

    for(i in 1:compilations){

      if(nn>0){
      for(j in 1:nn) {
        tiempo.early[iii[j]]=max(tiempo.early[prec[j,]]+duration[i,prec[j,]]);
      }
      }
      tiempo.computacion[i]<-max(tiempo.early+duration[i,])
    }
    delta<-quantile(ecdf(tiempo.computacion),percentile,names=FALSE)
  }
}

  tiempo.computacion<-numeric(compilations)


  for(i in 1:compilations){

    if(nn>0){
      for(j in 1:nn) {
        tiempo.early[iii[j]]=max(tiempo.early[prec[j,]]+duration[i,prec[j,]]);
      }
    }
    tiempo.computacion[i]<-max(tiempo.early+duration[i,])
    if(is.null(cost.function)==FALSE){
      tiempo.computacion[i]<-cost.function(tiempo.computacion[i])
    }
  }


  {
  if(is.null(cost.function)==FALSE){
    Duration.project<-mean(tiempo.computacion)
  }
    else{
      Duration.project<-mean(tiempo.computacion-delta)
    }
}

  for(z in 1:n){
    for(i in 1:compilations){
      duration1<-duration[i,]
      duration1[z]<-observed.duration[z]

      duration2<-duration[i,]
      duration2[-z]<-0
      if(nn>0){
      for(j in 1:nn) {
        tiempo.early[iii[j]]=max(tiempo.early[prec[j,]]+duration1[prec[j,]])
        tiempo.early2[iii[j]]=max(tiempo.early2[prec[j,]]+duration2[prec[j,]])
      }
      }

      tiempo.computacion[i]<-max(tiempo.early+duration1)
      tiempo.computacion2[i]<-max(tiempo.early2+duration2)


      if(is.null(cost.function)==FALSE){
        tiempo.computacion[i]<-cost.function(tiempo.computacion[i])
        tiempo.computacion2[i]<-cost.function(tiempo.computacion2[i])
      }

      }


    {
    if(is.null(cost.function)==FALSE){
      v1[z]<-mean(tiempo.computacion)
      w1[z]<-mean(tiempo.computacion2)
    }
      else{
        v[z]<-mean(tiempo.computacion-delta)
        w[z]<-mean(tiempo.computacion2-delta)
      }
  }
}
  {
    if(length(activities)>10){
      cat("shapley need some time to compute a", n, "player game \n")
      cat("Continue calculation of the Shapley value based on sampling? (Y = Yes, N = No) \n")
      continue <- scan(what = "character", n = 1)

      if(continue=="Y"){
        cont<-1
        while(cont<=1000){
        x<-sample(n)
        for(i in 1:(n-1)){
          for(j in 1:1000){
            duracion1<-duration[j,]
            duracion1[x[1:i]]<-observed.duration[x[1:i]]

            duracion2<-duration[j,]
            duracion2[-x[1:i]]<-0

            if(nn>0){
            for(t in 1:nn) {
              tiempo.early[iii[t]]=max(tiempo.early[prec[t,]]+duration1[prec[t,]])
              tiempo.early2[iii[t]]=max(tiempo.early2[prec[t,]]+duracion2[prec[t,]])
            }
            }
            tiempo.computacion[j]<-max(tiempo.early+duracion1)
            tiempo.computacion2[j]<-max(tiempo.early2+duracion2)
            if(is.null(cost.function)==FALSE){
              tiempo.computacion[i]<-cost.function(tiempo.computacion[i])
              tiempo.computacion2[j]<-cost.function(tiempo.computacion2[i])
            }
          }

          {
            if(is.null(cost.function)==FALSE){
              tiempo2[i]<-mean(tiempo.computacion)
              tiempo4[i]<-mean(tiempo.computacion2)
            }
            else{
              tiempo2[i]<-mean(tiempo.computacion-delta)
              tiempo4[i]<-mean(tiempo.computacion2-delta)
            }
          }
        }

        if(nn>0){
          for(t in 1:nn){
            tiempo.early[iii[t]]=max(tiempo.early[prec[t,]]+observed.duration[prec[t,]])
          }
          }

        {
          if(is.null(cost.function)==FALSE){
            tiempo2[n]<-mean(cost.function(max(tiempo.early+duracion1)))
          }
          else{
            tiempo2[n]<-mean(max(tiempo.early+duracion1)-delta)
          }
        }
        tiempo4[n]<-Duration.project
        tiempo3<-tiempo2-Duration.project

        sh[x[1]]<-tiempo2[1]
        sh[x[2:n]]<-tiempo2[2:n]-tiempo2[1:(n-1)]

        sh1[x[1]]<-tiempo3[1]
        sh1[x[2:n]]<-tiempo3[2:n]-tiempo3[1:(n-1)]

        sh2[x[1]]<-tiempo4[1]
        sh2[x[2:n]]<-tiempo4[2:n]-tiempo4[1:(n-1)]

        cont<-cont+1
        }
        SOLUTION<-(sh1+sh2)
      }
    }

    else{

    continue<-"Y"
  tiempo.computacion<-numeric(compilations)
  p<-n+1

  for(j in 2:n){
    con<-as.matrix(combn(c(1:n),j))

    for(z in 1:dim(con)[2]){

      for(i in 1:compilations){
        duracion1<-duration[i,]
        duracion1[con[,z]]<-observed.duration[con[,z]]
        duracion2<-duration[i,]
        duracion2[-con[,z]]<-0
        if(nn>0){
        for(t in 1:nn) {
          tiempo.early[iii[t]]=max(tiempo.early[prec[t,]]+duracion1[prec[t,]])
          tiempo.early2[iii[t]]=max(tiempo.early2[prec[t,]]+duracion2[prec[t,]])
        }
        }
        tiempo.computacion[i]<-max(tiempo.early+duracion1)
        tiempo.computacion2[i]<-max(tiempo.early2+duracion2)
        if(is.null(cost.function)==FALSE){
          tiempo.computacion[i]<-cost.function(tiempo.computacion[i])
          tiempo.computacion2[i]<-cost.function(tiempo.computacion2[i])
        }
      }

      {
      if(is.null(cost.function)==FALSE){
        v1[p]<-mean(tiempo.computacion)
        w1[p]<-mean(tiempo.computacion2)
      }
        else{
          v[p]<-mean(tiempo.computacion-delta)
          w[p]<-mean(tiempo.computacion2-delta)
        }
      }
      p<-p+1
    }
  }




  {
    if(is.null(cost.function)==FALSE){
      z<-as.matrix(DefineGame(n,v1)$Lex)
      z<-as.vector(z)
      coalitions <- set.func(c(0, z))
      sh <- Shapley.value(coalitions)


      V1<-v1-Duration.project
      z<-as.matrix(DefineGame(n,V1)$Lex)
      z<-as.vector(z)
      coalitions<- set.func(c(0, z))
      sh1<-Shapley.value(coalitions)


      z<-as.matrix(DefineGame(n,w1)$Lex)
      z<-as.vector(z)
      coalitions<- set.func(c(0, z))
      sh2<-Shapley.value(coalitions)
      SOLUTION<-(sh1+sh2)
    }
    else{

      z<-as.matrix(DefineGame(n,v)$Lex)
      z<-as.vector(z)
      coalitions <- set.func(c(0, z))
      sh <- Shapley.value(coalitions)

      V1<-v-Duration.project
      z<-as.matrix(DefineGame(n,V1)$Lex)
      z<-as.vector(z)
      coalitions<- set.func(c(0, z))
      sh1<-Shapley.value(coalitions)

      z<-as.matrix(DefineGame(n,w)$Lex)
      z<-as.vector(z)
      coalitions<- set.func(c(0, z))
      sh2<-Shapley.value(coalitions)
      SOLUTION<-(sh1+sh2)
    }
  }

}



  {
    if(is.null(cost.function)==FALSE){
      cat("Total delay of the stochastic project = ",cost.function(duracion.total.T), "\n")
      cat(" ","\n")
      Prop<-((observed.duration-durations.means)/sum(pmax(observed.duration-durations.means,0)))*(cost.function(duracion.total.T))
      TProp<-(pmin(observed.duration-durations.means,cost.function(duracion.total.T))/sum(pmin(observed.duration-durations.means,cost.function(duracion.total.T))))*(cost.function(duracion.total.T))
    }
    else{
      cat("Total delay of the stochastic project = ",duracion.total.T-delta, "\n")
      cat(" ","\n")
      Prop<-((observed.duration-durations.means)/sum(observed.duration-durations.means))*(duracion.total.T-delta)
      TProp<-(pmin(observed.duration-durations.means,duracion.total.T-delta)/sum(pmin(observed.duration-durations.means,duracion.total.T-delta)))*(duracion.total.T-delta)
    }
  }

  {
    if(continue=="Y"){
    A<-matrix(c(sh[or1],SOLUTION[or1],Prop[or1],TProp[or1]),ncol=n,byrow=TRUE)
    colnames(A)=c(activities)
    rownames(A)=c("Stochastic Shapley rule :  ","Stochastic Shapley rule 2:  ","Proportional rule :  ","Truncated Proportional rule :  ")
    return(round(A,5))
    }
    else{
      A<-matrix(c(Prop[or1],TProp[or1]),ncol=n,byrow=TRUE)
      colnames(A)=c(activities)
      rownames(A)=c("Proportional rule :  ","Truncated Proportional rule :  ")
      return(round(A,5))
    }
  }
}
}

