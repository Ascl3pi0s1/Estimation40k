jet_de_touche <-
function(Nb_attack,CT,rrTouch=0,mod_Touch=0,Bless_auto=7){
  x=sample(1:6,size=Nb_attack,replace = TRUE)
  as=sum(x==1)
  CT=min(6,max(CT-mod_Touch,2))
  touch=sum(x>CT-1)
  miss=sum(x<CT)
  six=sum(x>=Bless_auto)
  if(rrTouch==1){
    x=sample(1:6,size=as,replace = TRUE)
    touch=touch+sum(x>CT-1)
    six=six+sum(x>=Bless_auto)
  }
  if(rrTouch==2){
    x=sample(1:6,size=miss,replace = TRUE)
    touch=touch+sum(x>CT-1)
    six=six+sum(x>=Bless_auto)
  }
  as=sum(x==1)
  miss=sum(x<CT)
  #cat("Nombre de touche=",touch,"Nombre d'as=",as,"Nombre de blessure raté=",miss,"\n")
  list(touch=touch,bless_auto=six)
}
