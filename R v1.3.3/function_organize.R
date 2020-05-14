#' @title Organize project activities
#' @description This function organizes the activities of a project, in such a way that if i precedes j then i is less strict than j.
#' @param prec1and2 A matrix indicating the order of precedence type 1 and 2 between the activities (Default=matrix(0)). If value \eqn{(i,j)=1} then activity \eqn{i} precedes type \eqn{1} to \eqn{j}, and if \eqn{(i,j)=2} then activity \eqn{i} precedes type \eqn{2} to \eqn{j}. Cycles cannot exist in a project,  i.e. if an activity \eqn{i} precedes \eqn{j} then \eqn{j} cannot precede \eqn{i}.
#' @param prec3and4 A matrix indicating the order of precedence type 3 and 4 between the activities (Default=matrix(0)). If value \eqn{(i,j)=3} then activity \eqn{i} precedes type \eqn{3} to \eqn{j}, and if \eqn{(i,j)=4} then activity \eqn{i} precedes type \eqn{4} to \eqn{j}. Cycles cannot exist in a project,  i.e. if an activity \eqn{i} precedes \eqn{j} then \eqn{j} cannot precede \eqn{i}.
#' @export
#' @return A list containing:
#' \itemize{
#' \item{Precedence: }{ ordered precedence matrix.}
#' \item{Order: }{ new activities values.}
#' }
#' @examples
#' prec1and2<-matrix(c(0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0),nrow=5,ncol=5,byrow=TRUE)
#' organize(prec1and2)

organize<-function(prec1and2=matrix(0),prec3and4=matrix(0)){
  P1<-rebuild(prec1and2,prec3and4)$Precedence
  colnames(P1)<-1:nrow(P1)
  rownames(P1)<-1:nrow(P1)
  indices<-c()
    while(ncol(P1)>0){
     ii<-as.logical(colSums(P1))
     indices<-c(indices,colnames(P1)[!ii])
     P1<-P1[ii,ii,drop=FALSE]
   }
    indices<-as.numeric(indices)
    precedence<-rebuild(prec1and2,prec3and4)$Precedence[indices,indices]
    N<-matrix(0,nrow=dim(precedence)[1],ncol=2)
    N[,1]<-1:dim(precedence)[1]
    N[,2]<-indices
    lista<-list(precedence,N)
    names(lista)<-c("Precedence","Order")
  return(lista)
}
