#' @title Project resource levelling
#' @description This function calculates the schedule of the project so that the consumption of resources is as uniform as possible.
#' @param duration Vector with the duration for each activity.
#' @param prec1and2 A matrix indicating the order of precedence type 1 and 2 between the activities (Default=matrix(0)). If value \eqn{(i,j)=1} then activity \eqn{i} precedes type \eqn{1} to \eqn{j}, and if \eqn{(i,j)=2} then activity \eqn{i} precedes type \eqn{2} to \eqn{j}. Cycles cannot exist in a project,  i.e. if an activity \eqn{i} precedes \eqn{j} then \eqn{j} cannot precede \eqn{i}.
#' @param prec3and4 A matrix indicating the order of precedence type 3 and 4 between the activities (Default=matrix(0)). If value \eqn{(i,j)=3} then activity \eqn{i} precedes type \eqn{3} to \eqn{j}, and if \eqn{(i,j)=4} then activity \eqn{i} precedes type \eqn{4} to \eqn{j}. Cycles cannot exist in a project,  i.e. if an activity \eqn{i} precedes \eqn{j} then \eqn{j} cannot precede \eqn{i}.
#' @param resources Vector indicating the necessary resources for each activity per period of time.
#' @param int Numerical value indicating the duration of each period of time (Default=1).
#' @export
#' @details The problem of leveling resources takes into account that in order for activities to be carried out in the estimated time, a certain level of resources must be used. The problem is to find a schedule that allows to execute the project in the estimated time so that the temporary consumption of resources is as level as possible.
#' @references
#'  \describe{
#'   \item{}{Hegazy, T. (1999). Optimization of resource allocation and leveling using genetic algorithms. Journal of construction engineering and management, 125(3), 167-175.}
#' }
#' @return  A solution matrices.
#' @examples
#'
#'duration<-c(3,4,2,1)
#'resources<-c(4,1,3,3)
#'prec1and2<-matrix(c(0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0),nrow=4,ncol=4,byrow=TRUE)
#'
#'levelling.resources(duration,prec1and2,prec3and4=matrix(0),resources,int=1)



levelling.resources<-function(duration,prec1and2=matrix(0),prec3and4=matrix(0),resources,int=1){


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

prec<-matrix(0,nrow=n,ncol=n-1)
for(j in 1:n){
  if(sum(which(precedence[j,]==1))!=0){prec[j,1:length(which(precedence[j,]==1))]<-which(precedence[j,]==1)}
}

D.T<-max(early.times+durations)
holguras<-round(tiempo.last-early.times-durations,5)

periods<-seq(int,D.T,by=int)
cost.period<-numeric(length(periods))

X<-function(y){
  sum((y-mean(y))^2)
  #sum(sqrt(y))
}
for(i in 1:n){
  cost.period[(early.times[i]*(1/int)+1):((early.times[i]+durations[i])*(1/int))]<-cost.period[(early.times[i]*(1/int)+1):((early.times[i]+durations[i])*(1/int))]+costes[i]
}
cost.period0<-cost.period
for(i in n:1){

  if(holguras[i]!=0){
    {
      if(sum(prec[i,])==0){times<-seq(early.times[i],early.times[i]+holguras[i],by=int)}
      else{times<-seq(early.times[i],min(early.times[prec[i,]])-durations[i],by=int)}
    }
    new.periods<-numeric(length(times))
    cost.periods<-matrix(0,nrow=length(times),ncol=length(periods))

    for(j in 1:length(times)){
      early.times1<-early.times
      early.times1[i]<-times[j]

      for(z in 1:n){
        cost.periods[j,(early.times1[z]*(1/int)+1):((early.times1[z]+durations[z])*(1/int))]<-cost.periods[j,(early.times1[z]*(1/int)+1):((early.times1[z]+durations[z])*(1/int))]+costes[z]
      }
    }
    minimos<-apply(cost.periods,1,X)

    early.times[i]<-times[max(which(minimos==min(minimos)))]
  }
}


cost.period<-numeric(length(periods))
for(i in 1:n){
  cost.period[(early.times[i]*(1/int)+1):((early.times[i]+durations[i])*(1/int))]<-cost.period[(early.times[i]*(1/int)+1):((early.times[i]+durations[i])*(1/int))]+costes[i]
}

cat("Early times = ",  "\n")
print(early.times[or1])
cat("Resources  by period= ",  "\n")
print(cost.period)

z<-seq(int,D.T,int)-int/2
zz<-seq(0,D.T,int)
{
plot(z,cost.period0,type="b",pch=18,lwd=2,axes=F,xlab="Periods",ylab="Resources")
axis(side=1,at=zz)
axis(side=2,at=1:20)
lines(z,cost.period,type="b",pch=1,lwd=2,col="red")
legend(D.T-4, max(cost.period0), legend=c("First solution", "Readjusted solution"),
       col=c("black", "red"), lwd=c(2,2),pch=c(18,1), cex=1.1)
}
}
