#' @title Early time for a deterministic projects
#' @description This function calculates the early time for one project.
#' @param prec1and2 A matrix indicating the order of precedence type 1 and 2 between the activities (Default=matrix(0)). If value \eqn{(i,j)=1} then activity \eqn{i} precedes type \eqn{1} to \eqn{j}, and if \eqn{(i,j)=2} then activity \eqn{i} precedes type \eqn{2} to \eqn{j}. Cycles cannot exist in a project,  i.e. if an activity \eqn{i} precedes \eqn{j} then \eqn{j} cannot precede \eqn{i}.
#' @param prec3and4 A matrix indicating the order of precedence type 3 and 4 between the activities (Default=matrix(0)). If value \eqn{(i,j)=3} then activity \eqn{i} precedes type \eqn{3} to \eqn{j}, and if \eqn{(i,j)=4} then activity \eqn{i} precedes type \eqn{4} to \eqn{j}. Cycles cannot exist in a project,  i.e. if an activity \eqn{i} precedes \eqn{j} then \eqn{j} cannot precede \eqn{i}.
#' @param duration vector with the duración for each activities.
#' @export
#' @references
#' \describe{
#'   \item{}{Burke, R. (2013). Project management: planning and control techniques. New Jersey, USA.}
#' }
#' @return Early time vector.
#' @examples
#' prec1and2<-matrix(c(0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0),nrow=5,ncol=5,byrow=TRUE)
#' duration<-c(3,2,1,1.5,4.2)
#' early.time(prec1and2,duration=duration)

early.time<-function(prec1and2=matrix(0),prec3and4=matrix(0),duration){
  or1<-order(organize(prec1and2,prec3and4)$Order[,2])
  or2<-organize(prec1and2,prec3and4)$Order[,2]
  precedence<-organize(prec1and2,prec3and4)$Precedence
  activities<-c(1:dim(precedence)[1])
  n<-length(activities)
  duration<-duration[or2]
  tiempo.early<-rep(0,n)

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
    tiempo.early[iii[i]]=max(tiempo.early[prec[i,]]+duration[prec[i,]]);
  }
  }

  return(tiempo.early[or1])
}


