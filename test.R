yesterday <- function(){
  x <- Sys.Date() - 1
  return(x)
}




pooled_variance <- function(n1,n2,s1,s2){
  ( (n1 - 1)*s1^2   +   (n2 - 1)*s2^2 ) / 
    (n1 + n2 - 2)
}

# pooled_variance(80,22,15.3,18.2)



