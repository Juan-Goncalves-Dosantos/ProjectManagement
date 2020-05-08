#' @title Project resource allocation
#' @description This function calculates the project schedule so that resource consumption does not exceed the maximum available per time period..
#' @param duration Vector with the duration for each activity.
#' @param prec1and2 A matrix indicating the order of precedence type 1 and 2 between the activities (Default=matrix(0)). If value \eqn{(i,j)=1} then activity \eqn{i} precedes type \eqn{1} to \eqn{j}, and if \eqn{(i,j)=2} then activity \eqn{i} precedes type \eqn{2} to \eqn{j}. Cycles cannot exist in a project,  i.e. if an activity \eqn{i} precedes \eqn{j} then \eqn{j} cannot precede \eqn{i}.
#' @param prec3and4 A matrix indicating the order of precedence type 3 and 4 between the activities (Default=matrix(0)). If value \eqn{(i,j)=3} then activity \eqn{i} precedes type \eqn{3} to \eqn{j}, and if \eqn{(i,j)=4} then activity \eqn{i} precedes type \eqn{4} to \eqn{j}. Cycles cannot exist in a project,  i.e. if an activity \eqn{i} precedes \eqn{j} then \eqn{j} cannot precede \eqn{i}.
#' @param resources Vector indicating the necessary resources for each activity per period of time.
#' @param max.resources Numerical value indicating the maximum number of resources that can be used in each period.
#' @param int Numerical value indicating the duration of each period of time (Default=1).
#' @export
#' @details The problem of resource allocation takes into account that in order for activities to be carried out in the estimated time, a certain level of resources must be used. The problem is that the level of resources available in each period is limited. The aim is to obtain the minimum time and a schedule for the execution of the project taking into account this new restriction.
#' @references
#'  \describe{
#'   \item{}{Hegazy, T. (1999). Optimization of resource allocation and leveling using genetic algorithms. Journal of construction engineering and management, 125(3), 167-175.}
#' }
#' @return  A solution matrices.
#' @examples
#'
#'duration<-c(3,4,2,1)
#'prec1and2<-matrix(c(0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0),nrow=4,ncol=4,byrow=TRUE)
#'resources<-c(4,1,3,3)
#'max.resources<-4
#'
#'resource.allocation(duration,prec1and2,prec3and4=matrix(0),resources,max.resources,int=1)



resource.allocation<-function(duration,prec1and2,prec3and4=matrix(0),resources,max.resources,int=1){

  or1<-order(organize(prec1and2,prec3and4)$Order[,2])
  or2<-organize(prec1and2,prec3and4)$Order[,2]
  precedence<-organize(prec1and2,prec3and4)$Precedence
  durations<-duration[or2]
  costes<-resources[or2]
  n<-length(durations)
  activities<-1:n
  early.times<-rep(0,n)
  ii<-as.logical(colSums(precedence))
  iii<-activities[ii]
  nn<-length(iii)
  if(nn>0){
    prec<-matrix(0,nrow=nn,ncol=n-1)
    for(j in 1:nn){
      prec[j,1:length(which(precedence[,iii[j]]==1))]<-which(precedence[,iii[j]]==1)
    }
    prec<-prec[,as.logical(colSums(prec)),drop=FALSE]
    prec1<-prec
    III<-iii
    for(i in 1:nn) {
      early.times[iii[i]]=max(early.times[prec[i,]]+durations[prec[i,]]);
    }
  }


  tiempo.last<-rep(0,n)
  ii<-as.logical(rowSums(precedence))
  iii<-activities[ii]
  tiempo.last[activities[ii==FALSE]]<-max(early.times+durations)
  nn<-length(iii)
  if(nn>0){
    prec<-matrix(0,nrow=nn,ncol=n-1)
    for(j in 1:nn){
      prec[j,1:length(which(precedence[iii[j],]==1))]<-which(precedence[iii[j],]==1)
    }
    prec<-prec[,as.logical(colSums(prec)),drop=FALSE]


    for(i in length(iii):1) {
      tiempo.last[iii[i]]=min(tiempo.last[prec[i,]]-durations[prec[i,]]);
    }
  }

  holguras<-round(tiempo.last-early.times-durations,5)

  n<-length(durations)
  P1<-precedence
  colnames(P1)<-1:nrow(P1)
  rownames(P1)<-1:nrow(P1)
  n.early.times<-numeric(n)
  cost.period<-numeric(sum(durations)) #Hai que eliminar los iguales a cero al final
  periods<-seq(0,sum(durations),int)

  {
    if(length(which(resources>max.resources))>=1){
      cat("Error. There is no feasible solution.",  "\n")
    }
    else{
      while(ncol(P1)>0){
        ii<-as.logical(colSums(P1))
        AE<-as.numeric(colnames(P1)[!ii])
        OAE<-AE[order(holguras[AE])][1]
        {
          if(sum(which(III==OAE))!=0){
            TT<-max(n.early.times[prec1[which(III==OAE),]]+durations[prec1[which(III==OAE),]])
          }
          else{
            TT<-(which(cost.period<=max.resources)[1]-1)*int
          }
        }
        x<-0
        while(x<1){
          X<-costes[OAE]+sum(cost.period[TT*(1/int)+1])
          if(X<=max.resources){
            x<-1
            break
          }
          TT<-TT+int
        }


        n.early.times[OAE]<-periods[TT*(1/int)+1]

        ii[-which(ii==FALSE)[order(holguras[AE])[1]]]<-TRUE


        for(j in OAE){
          cost.period[(n.early.times[j]*(1/int)+1):((n.early.times[j]+durations[j])*(1/int))]<-cost.period[(n.early.times[j]*(1/int)+1):((n.early.times[j]+durations[j])*(1/int))]+costes[j]
        }
        P1<-P1[ii,ii,drop=FALSE]

      }


      cost.period<-cost.period[as.logical(cost.period)]
      cat("Project duration = ",  "\n")
      print(length(cost.period)*int)
      cat("Early times = ",  "\n")
      print(n.early.times[or1])
      cat("Resources  by period = ",  "\n")
      print(cost.period)
    }
  }
}

