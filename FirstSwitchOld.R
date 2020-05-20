FirstSwitchOld <- function(verb=TRUE, Kappa, swlamda, up_start, n_ou, n_bm)  #seed=NULL
{
   # Handle randomization
   
  
   cap <- 100 # say
   pregen <- matrix(NA,2,cap)
   P_ac_sw = ( n_ou*swlamda[1] + n_bm*swlamda[2] )/Kappa
   for (i in 1:cap)
   {
      pregen[1,i] <- rexp(1,Kappa)
      pregen[2,i] <- sample( c(0,1), 1, FALSE, prob=c(1-P_ac_sw,P_ac_sw) )
   }
   # From main code
   npsw = 0
   repeat{
      npsw=npsw+1
      ac_sw = pregen[2,npsw] #sample( c(0,1), 1, FALSE, prob=c(1-P_ac_sw,P_ac_sw) )
      if(ac_sw==1) break
   }
   
   pswt = 0
   po_t = pregen[1,1:npsw] #rexp(npsw,Kappa)
   pswt = sum(po_t)
   # Results
   if (verb) cat("old po_t vector", po_t, "\n")
   return(list('pswt'= pswt,  'po_t'=po_t  ))  #invisible(po_t)
}
