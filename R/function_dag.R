#' @title DAG plot
#' @description This function plots a directed acyclic graph (DAG).
#' @param prec1and2 A matrix indicating the order of precedence type 1 and 2 between the activities (Default=NULL). If value \eqn{(i,j)=1} then activity \eqn{i} precedes type \eqn{1} to \eqn{j}, and if \eqn{(i,j)=2} then activity \eqn{i} precedes type \eqn{2} to \eqn{j}. Cycles cannot exist in a project,  i.e. if an activity \eqn{i} precedes \eqn{j} then \eqn{j} cannot precede \eqn{i}.
#' @param prec3and4 A matrix indicating the order of precedence type 3 and 4 between the activities (Default=NULL). If value \eqn{(i,j)=3} then activity \eqn{i} precedes type \eqn{3} to \eqn{j}, and if \eqn{(i,j)=4} then activity \eqn{i} precedes type \eqn{4} to \eqn{j}. Cycles cannot exist in a project,  i.e. if an activity \eqn{i} precedes \eqn{j} then \eqn{j} cannot precede \eqn{i}.
#' @param critical.activities A vector indicating the critical activities to represent them in a different color (Default=NULL) .
#' @export
#' @return A plot.
#'
#' @examples
#'
#' prec1and2<-matrix(c(0,1,0,2,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,2,0),nrow=5,ncol=5,byrow=TRUE)
#' prec3and4<-matrix(0,nrow=5,ncol=5)
#' prec3and4[3,1]<-3
#' dag.plot(prec1and2,prec3and4)



dag.plot<-function(prec1and2=NULL,prec3and4=NULL,critical.activities=NULL){


A<-prec1and2
A0<-prec1and2
A0[A0%in%2]<-0

if(is.null(prec3and4)){prec3and4=matrix(0,nrow=dim(A)[1],ncol=dim(A)[2])}

B<-prec3and4

ii <- as.logical(colSums(A0))
iii <- as.logical(rowSums(A0))

A[as.logical(A)]<-1					#Esto se tiene que hacer con prec1and2 y prec2and3
B[as.logical(B)]<-1					#Esto se tiene que hacer con prec1and2 y prec2and3
A<-A+B

 colnames(A) <- 1:nrow(A)
 rownames(A) <- 1:nrow(A)

B<-cbind(A,rep(0,dim(A)[1]))
B<-rbind(B,rep(0,dim(B)[2]))				#A?adimos el nodo inicio
B<-cbind(B,rep(0,dim(B)[1]))
B<-rbind(B,rep(0,dim(B)[2]))				#A?adimos el nodo final
 colnames(B) <- 1:nrow(B)
    rownames(B) <- 1:nrow(B)


 #ii <- as.logical(colSums(A))


 #iii <- as.logical(rowSums(A))

B[dim(A)[1]+1,colnames(A)[!ii]]<-1						#Nodo inicio precede a las actividades iniciales
B[rownames(A)[!iii],dim(A)[1]+2]<-1						#Nodo fin es precedido por  las actividades iniciales

colnames(B)[dim(A)[1]+1]<-"S"
rownames(B)[dim(A)[1]+1]<-"S"
colnames(B)[dim(A)[1]+2]<-"E"
rownames(B)[dim(A)[1]+2]<-"E"

i0<-which(t(B) !=0)


A0<-B
A0[1:(dim(A0)[1]-2),1:(dim(A0)[2]-2)]<-prec1and2
B0<-B
B0[1:(dim(B0)[1]-2),1:(dim(B0)[2]-2)]<-prec3and4

i2<-which(t(A0) ==2)
i3<-which(t(B0) ==3)
i4<-which(t(B0) ==4)

label<-rep("",length(i0))
label[match(i2,i0)]<-"SS"
label[match(i3,i0)]<-"FF"
label[match(i4,i0)]<-"SF"
network <- graph_from_adjacency_matrix(B)

# plot it
if(is.null(critical.activities)){
  plot(network,edge.label=label,vertex.color = "green",vertex.shape="square",vertex.label.color=c(rep("black",dim(A)[1]),"blue","blue")
,layout=layout.kamada.kawai)
}
else{
  color<-c(rep("green",dim(prec1and2)[1]),"red","red")
  color[critical.activities]<-"red"
  plot(network,edge.label=label,vertex.color = color,vertex.shape="square",vertex.label.color=c(rep("black",dim(A)[1]),"blue","blue")
       ,layout=layout.kamada.kawai)
}
#layout.reingold.tilford
#layout.kamada.kawai

}


