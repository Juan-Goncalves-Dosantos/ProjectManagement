#' @title Last time for a deterministic projects
#' @description This function calculates the last time for one project.
#' @param prec1and2 A matrix indicating the order of precedence type 1 and 2 between the activities (Default=matrix(0)). If value \eqn{(i,j)=1} then activity \eqn{i} precedes type \eqn{1} to \eqn{j}, and if \eqn{(i,j)=2} then activity \eqn{i} precedes type \eqn{2} to \eqn{j}. Cycles cannot exist in a project,  i.e. if an activity \eqn{i} precedes \eqn{j} then \eqn{j} cannot precede \eqn{i}.
#' @param prec3and4 A matrix indicating the order of precedence type 3 and 4 between the activities (Default=matrix(0)). If value \eqn{(i,j)=3} then activity \eqn{i} precedes type \eqn{3} to \eqn{j}, and if \eqn{(i,j)=4} then activity \eqn{i} precedes type \eqn{4} to \eqn{j}. Cycles cannot exist in a project,  i.e. if an activity \eqn{i} precedes \eqn{j} then \eqn{j} cannot precede \eqn{i}.
#' @param duration Vector with the duración for each activity.
#' @param early.times Vector with the early times for each activities.
#' @export
#' @references
#' \describe{
#'   \item{}{Burke, R. (2013). Project management: planning and control techniques. New Jersey, USA.}
#' }
#' @return Last time vector.
#' @examples
#' prec1and2<-matrix(c(0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0),nrow=5,ncol=5,byrow=TRUE)
#' duration<-c(3,2,1,1.5,4.2)
#' early.times<-c(0,0,3.5,2,0)
#' last.time(prec1and2,duration=duration,early.times=early.times)

last.time<-function(prec1and2=matrix(0),prec3and4=matrix(0),duration,early.times){

  or1<-order(organize(prec1and2,prec3and4)$Order[,2])
  or2<-organize(prec1and2,prec3and4)$Order[,2]
  precedence<-organize(prec1and2,prec3and4)$Precedence
  activities<-c(1:dim(precedence)[1])
  n<-length(activities)
  duration<-duration[or2]
  early.times<-early.times[or2]

  tiempo.last<-rep(0,n)

  ii<-as.logical(rowSums(precedence))
  iii<-activities[ii]
  tiempo.last[activities[ii==FALSE]]<-max(early.times+duration)
  nn<-length(iii)
  if(nn>0){
  prec<-matrix(0,nrow=nn,ncol=n-1)
  for(j in 1:nn){
    prec[j,1:length(which(precedence[iii[j],]==1))]<-which(precedence[iii[j],]==1)
  }
  prec<-prec[,as.logical(colSums(prec)),drop=FALSE]


  for(i in length(iii):1) {
    tiempo.last[iii[i]]=min(tiempo.last[prec[i,]]-duration[prec[i,]]);
  }
  }
  return(tiempo.last[or1])
}
