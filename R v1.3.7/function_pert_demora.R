#' @title Problems of distribution of delay in deterministic projects
#' @description This function calculates the delay of a project once it has been completed. In addition, it also calculates the distribution of the delay between the different activities with the proportional, truncated proportional and Shapley rule.
#' @param duration Vector with the expected duration for each activity.
#' @param prec1and2 A matrix indicating the order of precedence type 1 and 2 between the activities (Default=matrix(0)). If value \eqn{(i,j)=1} then activity \eqn{i} precedes type \eqn{1} to \eqn{j}, and if \eqn{(i,j)=2} then activity \eqn{i} precedes type \eqn{2} to \eqn{j}. Cycles cannot exist in a project,  i.e. if an activity \eqn{i} precedes \eqn{j} then \eqn{j} cannot precede \eqn{i}.
#' @param prec3and4 A matrix indicating the order of precedence type 3 and 4 between the activities (Default=matrix(0)). If value \eqn{(i,j)=3} then activity \eqn{i} precedes type \eqn{3} to \eqn{j}, and if \eqn{(i,j)=4} then activity \eqn{i} precedes type \eqn{4} to \eqn{j}. Cycles cannot exist in a project,  i.e. if an activity \eqn{i} precedes \eqn{j} then \eqn{j} cannot precede \eqn{i}.
#' @param observed.duration Vector with the observed duration for each activity.
#' @param delta Value to indicate the maximun time that the project can take without delay. If this is not added, the function will use as delta the expected project time. This value is only used if the function uses the default cost function.
#' @param cost.function Delay costs function. If this value is not added, a default cost function will be used.
#' @export
#' @details Given a problem of sharing delays in a project \eqn{(N,\prec,\{\bar{X}_i\}_{i\in N},\{x_i\}_{i\in N})},  such that \eqn{\{\bar{X}_i\}_{i\in N}} is the expected value of activities' duration and \eqn{\{x_i\}_{i\in N}} the observed value. If \eqn{D(N,\prec,\{\bar{X}_i\}_{i\in N})} is the expected project time and \eqn{D(N,\prec,\{x_i\}_{i\in N})} is the observed project time, it has to \eqn{d=D(N,\prec,\{\bar{X}_i\}_{i\in N})-\delta} is the delay, where \eqn{\delta} can be any arbitrary value greater than zero. The following rules distribute the delay costs among the different activities.
#'
#' The proportional rule, from \cite{Brânzei et al. (2002)}, distributes the delay, \eqn{d}, proportionally. So that each activity receives a payment of:
#' \deqn{\phi_i=\frac{\displaystyle x_{i}-\bar{X}_{i}}{\displaystyle \sum_{j\in N}\max\{x_{j}-\bar{X}_{j},0\}}\cdot C(D(N,\prec,\{\bar{X}_i\}_{i\in N})).}
#'
#' The truncated proportional rule, from \cite{Brânzei et al. (2002)}, distributes the delay, \eqn{d}, proportionally, where the individual delay of each player is reduced to \eqn{d} if if is larger. So that each activity receives a payment of:
#' \deqn{\bar{\phi}_i=\frac{\displaystyle \min\{x_{i}-\bar{X}_{i},C(D(N,\prec,\{\bar{X}_i\}_{i\in N}))\}}{\displaystyle \sum_{j\in N} \max\{\min\{x_{j}-\bar{X}_{j},C(D(N,\prec,\{\bar{X}_i\}_{i\in N}))\},0\}}\cdot C(D(N,\prec,\{\bar{X}_i\}_{i\in N})).}
#'
#'These values are only well defined when the sum of the individual delays is different from zero.
#'
#' Shapley rule distributes the delay, \eqn{d}, based on the Shapley value for TU games, see \cite{Bergantiños et al. (2018)}. Given a project problem with delays \eqn{(N,\prec,\{\bar{X}_i\}_{i\in N},\{x_i\}_{i\in N})}, its associated TU game, \eqn{(N,v)}, is \eqn{v(S)=C(D(N,\prec,(\{\bar{X}_i\}_{i\in N\backslash S},\{x_i\}_{i\in S})))} for all \eqn{S\subseteq N}, where \eqn{C} is the costs function (by default \eqn{C(D(N,\prec,y))=D(N,\prec,y)-\delta}. If the number of activities is greater than ten, the Shapley value, of the game \eqn{(N,v)}, is estimated using a unique sampling process for all players, see \cite{Castro et al. (2009)}.
#' @references
#' \describe{
#'   \item{}{Bergantiños, G., Valencia-Toledo, A., & Vidal-Puga, J. (2018). Hart and Mas-Colell consistency in PERT problems. Discrete Applied Mathematics, 243, 11-20.}
#'   \item{}{Brânzei, R., Ferrari, G., Fragnelli, V., & Tijs, S. (2002). Two approaches to the problem of sharing delay costs in joint projects. Annals of Operations Research, 109(1-4), 359-374.}
#'   \item{}{Castro, J., Gómez, D., & Tejada, J. (2009). Polynomial calculation of the Shapley value based on sampling. Computers & Operations Research, 36(5), 1726-1730.}
#' }
#' @return  The delay value and a solution matrix.
#' @examples

#' prec1and2<-matrix(c(0,1,0,1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0),nrow=5,ncol=5,byrow=TRUE)
#' duration<-c(2,1,1,4,2)
#' observed.duration<-c(2.5,1.25,2,4.5,3)
#' delta<-6
#' delay.pert(duration,prec1and2=prec1and2,observed.duration=observed.duration,
#' delta=delta,cost.function=NULL)


delay.pert<-function(duration,prec1and2=matrix(0),prec3and4=matrix(0),observed.duration,delta=NULL,cost.function=NULL){

  or1<-order(organize(prec1and2,prec3and4)$Order[,2])
  or2<-organize(prec1and2,prec3and4)$Order[,2]
  precedence<-organize(prec1and2,prec3and4)$Precedence
  duration<-duration[or2]
  observed.duration<-observed.duration[or2]
  activities<-1:length(duration)
  n<-length(activities)
  SD<-numeric(length(activities))
  w<-numeric(2^n-1)
  tiempo.early<-rep(0,n)

  ii<-as.logical(colSums(precedence))
  iii<-activities[ii]
  nn<-length(iii)
  if(nn>0){
  prec<-matrix(0,nrow=nn,ncol=n-1)
  for(j in 1:length(iii)){
    prec[j,1:length(which(precedence[,iii[j]]==1))]<-which(precedence[,iii[j]]==1)
  }
  prec<-prec[,as.logical(colSums(prec)),drop=FALSE]

  for(i in 1:length(iii)) {
    tiempo.early[iii[i]]=max(tiempo.early[prec[i,]]+duration[prec[i,]]);
  }
}

  tiempo<-max(tiempo.early+duration)
  if(is.null(delta)==FALSE){tiempo<-delta}
  if(nn>0){
  for(i in 1:nn) {
    tiempo.early[iii[i]]=max(tiempo.early[prec[i,]]+observed.duration[prec[i,]]);
  }
}

  tiempo.observado<-max(tiempo.early+observed.duration)



    {
    if(is.null(cost.function)==FALSE){
      cat("There has been a delay of = ", cost.function(tiempo.observado), "\n")
      Prop<-((observed.duration-duration)/sum(observed.duration-duration))*(cost.function(tiempo.observado))
      TProp<-(pmin(observed.duration-duration,cost.function(tiempo.observado))/sum(pmin(observed.duration-duration,cost.function(tiempo.observado))))*(cost.function(tiempo.observado))
    }
    else{
      cat("There has been a delay of = ", tiempo.observado-tiempo, "\n")
    Prop<-((observed.duration-duration)/sum(observed.duration-duration))*(tiempo.observado-tiempo)
    TProp<-(pmin(observed.duration-duration,tiempo.observado-tiempo)/sum(pmax(observed.duration-duration,tiempo.observado-tiempo)))*(tiempo.observado-tiempo)
    }
    }

    for(j in 1:length(activities)){
      duracion1<-duration
      duracion1[j]<-observed.duration[j]
      if(nn>0){
      for(i in 1:nn) {
        tiempo.early[iii[i]]=max(tiempo.early[prec[i,]]+duracion1[prec[i,]]);
      }
      }

      if(is.null(cost.function)==FALSE){w[j]<-cost.function(max(tiempo.early+duracion1))}
      SD[j]<-max(tiempo.early+duracion1)
    }

    {
      if(length(activities)>10){
        cat("shapley need some time to compute a", n, "player game \n")
        cat("Continue calculation of the Shapley value based on sampling? (Y = Yes, N = No) \n")
        continue <- scan(what = "character", n = 1)

        if(continue=="Y"){
          contador<-0
          sh<-numeric(length(activities))
          while(contador<=1000){
            x<-sample(length(activities))
            for(i in 1:n){
                duracion1<-duration
                duracion1[x[1:i]]<-observed.duration[x[1:i]]
                if(nn>0){
                for(j in 1:nn) {
                  tiempo.early[iii[j]]=max(tiempo.early[prec[j,]]+duracion1[prec[j,]])
                }
                }
                {
                  if(is.null(cost.function)==FALSE){
                    tiempo2[i]<-cost.function(max(tiempo.early+duracion1))
                  }
                  else{
                    tiempo2[i]<-max(tiempo.early+duracion1)-tiempo
                  }
                }

            }
            sh[x[1]]<-sh[x[1]]+tiempo2[1]
            sh[x[2:n]]<-sh[x[2:n]]+tiempo2[2:n]-tiempo2[1:(n-1)]
            contador<-contador+1
          }
          sh<-sh/contador
        }
      }
      else{
        v<-numeric(2^n-1)
        v[1:n]<-SD[1:n]
        continue<-"Y"
        p<-n+1

        for(j in 2:n){
          con<-as.matrix(combn(c(1:n),j))
          for(z in 1:dim(con)[2]){

            duracion1<-duration
            duracion1[con[,z]]<-observed.duration[con[,z]]
            if(nn>0){
            for(i in 1:length(iii)) {
              tiempo.early[iii[i]]=max(tiempo.early[prec[i,]]+duracion1[prec[i,]])
            }
            }
            if(is.null(cost.function)==FALSE){w[p]<-cost.function(max(tiempo.early+duracion1))}
            v[p]<-max(tiempo.early+duracion1)
            p<-p+1
          }
        }

        v<-v-tiempo

        #sh<-c(shapleyValue(n,v=v)$value)


        {
        if(is.null(cost.function)==FALSE){
          z<-as.matrix(DefineGame(n,w)$Lex)
          z<-as.vector(z)
          coalitions <- set.func(c(0, z))
          sh <- Shapley.value(coalitions)
        }
          else{
            z<-as.matrix(DefineGame(n,v)$Lex)
            z<-as.vector(z)
            coalitions <- set.func(c(0, z))
            sh <- Shapley.value(coalitions)
          }
        }
        }
    }


    {
      if(continue=="Y"){
        A<-matrix(c(Prop[or1],TProp[or1],sh[or1]),ncol=length(activities),byrow=TRUE)
        colnames(A)=c(activities)
        rownames(A)=c("The proportional payment ","The truncated proportional payment ", "Shapley rule")
        cat(" ","\n")
        return(round(A,5))
      }
      else{
        A<-matrix(c(Prop[or1],TProp[or1]),ncol=length(activities),byrow=TRUE)
        colnames(A)=c(activities)
        rownames(A)=c("The proportional payment ","The truncated proportional payment ")
        cat(" ","\n")
        return(round(A,5))
      }
    }

}

