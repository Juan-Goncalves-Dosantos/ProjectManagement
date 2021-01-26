#' @title Build a precedence matrix
#' @description This function calculates the costs per activity to accelerate the project.
#' @param duration Vector with the duration for each activity.
#' @param minimum.durations Vector with the Minimum duration allowed for each activity.
#' @param prec1and2 A matrix indicating the order of precedence type 1 and 2 between the activities (Default=matrix(0)). If value \eqn{(i,j)=1} then activity \eqn{i} precedes type \eqn{1} to \eqn{j}, and if \eqn{(i,j)=2} then activity \eqn{i} precedes type \eqn{2} to \eqn{j}. Cycles cannot exist in a project,  i.e. if an activity \eqn{i} precedes \eqn{j} then \eqn{j} cannot precede \eqn{i}.
#' @param prec3and4 A matrix indicating the order of precedence type 3 and 4 between the activities (Default=matrix(0)). If value \eqn{(i,j)=3} then activity \eqn{i} precedes type \eqn{3} to \eqn{j}, and if \eqn{(i,j)=4} then activity \eqn{i} precedes type \eqn{4} to \eqn{j}. Cycles cannot exist in a project,  i.e. if an activity \eqn{i} precedes \eqn{j} then \eqn{j} cannot precede \eqn{i}.
#' @param duration.project numerical value indicating the minimum time sought in the project (Default=NULL).
#' @param activities.costs Vector indicating the cost of accelerating a unit of time the duration of each activity.
#' @export
#' @details The MCE method (Minimal Cost Expediting) tries to speed up the project at minimum cost. It considers that the duration of some project activities could be reduced by increasing the resources allocated to them (and thus increasing their implementation costs).
#' @references
#' \describe{
#'   \item{}{Kelley Jr, J. E. (1961). Critical-path planning and scheduling: Mathematical basis. Operations research, 9(3), 296-320.}
#' }
#' @return  A solution matrices.
#' @examples
#'
#'duration<-c(5,4,5,2,2)
#'minimum.durations<-c(3,2,3,1,1)
#'activities.costs<-c(1,1,1,1,1)
#'prec1and2<-matrix(c(0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0),nrow=5,ncol=5,byrow=TRUE)
#'duration.project<-6
#'
#'mce(duration,minimum.durations,prec1and2,prec3and4=matrix(0),activities.costs,duration.project)

mce<-function(duration,minimum.durations,prec1and2=matrix(0),prec3and4=matrix(0),activities.costs,duration.project=NULL){
  estimated.durations<-duration
  n<-length(estimated.durations)
  activities<-1:n
  or1<-order(organize(prec1and2,prec3and4)$Order[,2])
  or2<-organize(prec1and2,prec3and4)$Order[,2]
  precedence<-organize(prec1and2,prec3and4)$Precedence
  tiempo.early<-early.time(prec1and2,prec3and4,estimated.durations)[or2]


  ii<-as.logical(rowSums(precedence))
  iii<-activities[ii]
  nn<-length(iii)
  if(nn>0){
    prec<-matrix(0,nrow=nn,ncol=n-1)
    for(j in 1:nn){
      prec[j,1:length(which(precedence[iii[j],]==1))]<-which(precedence[iii[j],]==1)
    }
    prec<-prec[,as.logical(colSums(prec)),drop=FALSE]
  }

  m<-length(which(prec!=0))
  mm<-rep(1,n)

  des<-rep("<=",2*n+m+(n-length(iii)))

  cont<-1
  EJEM1<-make.lp(2*n+m+(n-length(iii)),2*n)
  A<-diag(1,n,2*n)
  {
    if(is.null(duration.project)==FALSE){
      for(i in 1:n){
        set.row(EJEM1,i,A[i,])
        set.row(EJEM1,n+i,-A[i,])
        #set.row(EJEM1,2*n+i,A[i,])

        {
          if(sum(precedence[i,])!=0){

            AA<-A[i,]
            AA[n+i]<-1

            pr<-prec[iii==i,]
            pr<-pr[pr!=0]

            for(j in 1:length(pr)){
              AAA<-AA
              AAA[n+pr[j]]<--1
              set.row(EJEM1,2*n+cont,AAA)
              cont<-cont+1
            }

          }
          else{

            AA<-A[i,]
            AA[n+i]<-1
            set.row(EJEM1,2*n+cont,AA)
            set.rhs(EJEM1,duration.project,2*n+cont)
            cont<-cont+1
          }
        }
      }

      set.rhs(EJEM1,estimated.durations[or2],1:n)
      set.rhs(EJEM1,-minimum.durations[or2],(n+1):(2*n))

      ob<-c(activities.costs[or2],rep(0,n))
      set.constr.type(EJEM1,des)
      set.objfn(EJEM1,ob)
      lp.control(EJEM1,sense="max")

      solve(EJEM1)


      {
        if(solve(EJEM1)<=1){
          tiempos<-get.variables(EJEM1)[1:n]
          costes<-(estimated.durations[or2]-tiempos)*activities.costs[or2]
          A<-matrix(0,nrow=n,ncol=2)
          A[,1]<-tiempos[or1]
          A[,2]<-costes[or1]
          colnames(A)<-c("estimated activities durations","costs")
          cat("The project duration is",duration.project ,"\n")
          return(round(A,5))
        }
        else{
          cat("The problem has no solution", "\n")
        }
      }
    }
    else{
      cat("necessary negative increase", "\n")
      inc <- scan(what = "character", n = 1)

      duracion.total<-max(tiempo.early+estimated.durations[or2])
      inc<-seq(duracion.total-as.numeric(inc),as.numeric(inc),by=-as.numeric(inc))
      tiempos<-matrix(0,n,1)
      for(j in inc){
        cont<-1
        for(i in 1:n){
          set.row(EJEM1,i,A[i,])
          set.row(EJEM1,n+i,-A[i,])
          #set.row(EJEM1,2*n+i,A[i,])

          {
            if(sum(precedence[i,])!=0){

              AA<-A[i,]
              AA[n+i]<-1

              pr<-prec[iii==i,]
              pr<-pr[pr!=0]

              for(z in 1:length(pr)){
                AAA<-AA
                AAA[n+pr[z]]<--1
                set.row(EJEM1,2*n+cont,AAA)
                cont<-cont+1

              }
            }
            else{

              AA<-A[i,]
              AA[n+i]<-1
              set.row(EJEM1,2*n+cont,AA)
              set.rhs(EJEM1,j,2*n+cont)
              cont<-cont+1

            }
          }
        }

        set.rhs(EJEM1,estimated.durations[or2],1:n)
        set.rhs(EJEM1,-minimum.durations[or2],(n+1):(2*n))

        ob<-c(activities.costs[or2],rep(0,n))
        set.constr.type(EJEM1,des)
        set.objfn(EJEM1,ob)
        lp.control(EJEM1,sense="max")

        if(solve(EJEM1)>=2){break}

        {
          if(j==inc[1]){tiempos[,1]<-get.variables(EJEM1)[1:n]
          durations<-j
          }
          else{tiempos<-cbind(tiempos,get.variables(EJEM1)[1:n])
          durations<-c(durations,j)
          }
        }
      }
      {
        if(sum(tiempos[,1])!=0){
          costes<-apply(tiempos,2,function(x) ((cbind(estimated.durations[or2])-x)*activities.costs[or2]))
          cat("Project duration = ",  "\n")
          print(durations)
          cat("Estimated durations = ",  "\n")
          print(round(tiempos[or1,],5))
          cat("Costs per solution  = ", "\n")
          print(round(costes[or1,],5))
        }
        else{
          cat("The problem has no solution", "\n")
        }
      }
    }
  }

}


