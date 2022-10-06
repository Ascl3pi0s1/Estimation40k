jet_de_bless <-
function(JetTouch,For,End,rrBless=0,mod_Bless=0,Transu=0){
  touche=JetTouch[[1]]
  x=sample(1:6,size=touche,replace = TRUE)
  B=max(min(6,max(Bless_rate(For,End)-mod_Bless,2)),Transu)
  as=sum(x==1)
  Bless=sum(x>B-1)
  miss=sum(x<B)
  if(rrBless==1){
    x=sample(1:6,size=as,replace = TRUE)
    Bless=Bless+sum(x>B-1)
  }
  if(rrBless==2){
    x=sample(1:6,size=miss,replace = TRUE)
    Bless=Bless+sum(x>B-1)
  }
  as=sum(x==1)
  miss=sum(x<B)
  #cat("Nombre de blessure=",Bless,"Nombre d'as=",as,"Nombre de blessure raté=",miss,"\n")
  Bless+JetTouch[[2]]
}
