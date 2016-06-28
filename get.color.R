get.color <- function(pv, alpha=0.10){
  
  if (is.na(pv)) mycol = 'black' else {
    mycol = ifelse(pv < alpha , 'red', 'black')
  } # end else
  
  col.msg = paste("\\textcolor{", mycol, "}{", formatC(pv, dig=2, format="g"), "}", sep="")
  
  return(col.msg)
} # end get.color function