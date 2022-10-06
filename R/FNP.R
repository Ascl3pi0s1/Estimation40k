FNP <-
function(fnp,miss_save,dmg,Pv,save){
  x=0
  a=Pv
  for (i in rep(dmg,miss_save)){
    b=rbinom(1,dmg,(fnp-1)/6)
    if((a-b)<=0){
      x=x+1
      a=Pv}
    else{a=a-b}
  }
  #cat("Nombre de modèle tués=",x,"\n")
  x
}
