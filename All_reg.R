
All_reg <- function(model.in, data.in, nbest.in, nvmax.in, ...){

    #library(leaps)
    regs <- regsubsets(model.in, nbest=nbest.in, nvmax=nvmax.in, data=data.in)
    rs <- summary(regs)
    n = dim(data.in)[1]
    n_vars = dim(data.in)[2] - 1
    if (nvmax.in > n_vars) {
        stop("Nvmax must be < or = the number of variables")
        }

    if (nbest.in > n_vars) {
        stop("Nbest must be < or = the number of variables")
        } 

    if (nvmax.in == n_vars) {
        p <- vector(length=nbest.in*(nvmax.in-1)+1)
        } 
    else {
        p <- vector(length=nbest.in*nvmax.in)
        }
           
 
    pars = 2
    nbest_counter = 1    
    for (i in 1:length(p)){ 
          if (nbest_counter <= nbest.in) {
              p[i] <- pars
              nbest_counter = nbest_counter + 1             
              }
          else {
              pars = pars + 1
              p[i] <- pars
              nbest_counter = 2   
              }           
         }
        
    df = n-p
    MSE_p = rs$rss/df

    r_stats <- cbind(p, rs$rsq, rs$adjr2, rs$rss, MSE_p, rs$cp, rs$bic)  
    colnames(r_stats) <- c("P", "RSQ", "RSQ_A", "SSE", "MSE", "Cp", "BIC")

    vars1 <- as.matrix(rs$which)
    vars1 <- cbind(p, vars1)
    v_names <- variable.names(vars1)

    n_models = dim(vars1)[1]
    n_cols = dim(vars1)[2]
    namelist <- matrix(NA, nrow=n_models, ncol=n_cols) 
    for (i in 1:n_models){
         for (j in 1:n_cols){
              if(vars1[i,j] == 1)
                 {namelist[i,j]<-v_names[j]
                  }
              }
         }  

    namelist2 <- matrix("", nrow=n_models, ncol=1)
    for (i in 1:n_models){
         for (j in 3:n_cols){
              if(!is.na(namelist[i,j]==1)){
                  namelist2[i]<-paste(namelist2[i], namelist[i,j])
                  }
              }
          }

    all_out <- data.frame(round(r_stats,4), namelist2)
    colnames(all_out) <- c(colnames(r_stats), "Variables In") 

    all_export <- format(all_out, justify = "left")
    print(all_export)
    capture.output(print(all_export, print.gap=3, quote=FALSE), file="reg_out.txt")
    return(all_out)
    } 