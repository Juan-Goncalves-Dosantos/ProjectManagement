#' @title Build a precedence matrix
#' @description This function builds a unique type 1 precedence matrix given any kind of precedence.
#' @param prec1and2 A matrix indicating the order of precedence type 1 and 2 between the activities (Default=matrix(0)). If value \eqn{(i,j)=1} then activity \eqn{i} precedes type \eqn{1} to \eqn{j}, and if \eqn{(i,j)=2} then activity \eqn{i} precedes type \eqn{2} to \eqn{j}. Cycles cannot exist in a project,  i.e. if an activity \eqn{i} precedes \eqn{j} then \eqn{j} cannot precede \eqn{i}.
#' @param prec3and4 A matrix indicating the order of precedence type 3 and 4 between the activities (Default=matrix(0)). If value \eqn{(i,j)=3} then activity \eqn{i} precedes type \eqn{3} to \eqn{j}, and if \eqn{(i,j)=4} then activity \eqn{i} precedes type \eqn{4} to \eqn{j}. Cycles cannot exist in a project,  i.e. if an activity \eqn{i} precedes \eqn{j} then \eqn{j} cannot precede \eqn{i}.
#' @export
#' @details There are four types of precedence between two activities \eqn{i,j}:
#' Type 1: the activity \eqn{j} cannot start until activity \eqn{i} has finished.
#' Type 2: the activity \eqn{j} cannot start until activity \eqn{i} has started.
#' Type 3: the activity \eqn{j} cannot end until activity \eqn{i} has ended.
#' Type 4: the activity \eqn{j} cannot end until activity \eqn{i} has started.
#'
#' All these precedences can be written only as type 1. It should be noted that precedence type 1 implies type 2, and type 2 implies type 4. On the other hand, precedence type 1 implies type 3, and type 3 implies type 4.
#' @return A list containing:
#' \itemize{
#' \item{Precedence: }{ precedence matrix.}
#' \item{Type 2: }{ activities related to type 2 precedence.}
#' \item{Type 3: }{ activities related to type 3 precedence.}
#' \item{Type 4: }{ activities related to type 4 precedence.}
#' }
#' @examples
#'
#' prec1and2<-matrix(c(0,1,0,2,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,2,0),nrow=5,ncol=5,byrow=TRUE)
#' prec3and4<-matrix(0,nrow=5,ncol=5)
#' prec3and4[3,1]<-3
#' rebuild(prec1and2,prec3and4)

rebuild<-function(prec1and2=matrix(0),prec3and4=matrix(0)){
  P1<-prec1and2
  P2<-prec3and4
  PX<-P1				#Matriz original

  prec2<-which(P1==2,arr.ind=TRUE) #Buscamos posiciones igual a 2
  PX[prec2]<-0			#Eliminamos los 2

  PX[,prec2[,2]][which(PX[,prec2[,1]]==1,arr.ind=TRUE)]<-1 #en bprec2a si c precede a b entonces precede a a

  prec3<-which(P2==3,arr.ind=TRUE) #Buscamos posiciones igual a 3

  PX[prec3[,1],][which(PX[prec3[,2],]==1,arr.ind=TRUE)]<-1#en bprec3a si a precede a c entonces b precede a c

  prec4<-which(P2==4,arr.ind=TRUE) #Buscamos posiciones igual a 4

  PX[,prec4[,1]][which(PX[prec4[,2],]==1,arr.ind=TRUE)]<-1 #en bprec4a si aprec1d y cprec1b entonces cprec1d
  precedence<-PX
  lista<-list(precedence,prec2,prec3,prec4)
  names(lista)<-c("Precedence","Type 2","Type 3","Type 4")
  return(lista)
}
