Bless_rate <-
function(x,y){
  if(x>=2*y){a=2}
  if(2*y>x & x>y){a=3}
  if(x==y){a=4}
  if(x<y & y<2){a=5}
  if(y>=2*x){a=6}
  a
}
