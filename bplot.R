
## Spine plots/Spinograms contributed by Achim Zeileis
## https://svn.r-project.org/R/trunk/src/library/graphics/R/spineplot.R

bplot = function(x,y, nmx, nmy, cx=1, cx.lab=1, cx.main=1, cx.axis=1) {
  
  if (is.factor(x) & is.factor(y)) b=spineplot(x,y, xlab=nmx, ylab=nmy, cex=cx,
                                               col=gray.colors(nlevels(y), start=.3, end=.8)) 
  
  if (!is.factor(x) & is.factor(y)) b=boxplot(x~y, horizontal=T, xlab=nmx, ylab=nmy,
                                              cex=cx) 
  
  if (is.factor(x) & !is.factor(y)) b=boxplot(y~x, horizontal=F, xlab=nmx, ylab=nmy,
                                              cex=cx) 
  
  if (!is.factor(x) & !is.factor(y)) {
    b=plot(x, y, xlab=nmx, ylab=nmy, cex=cx, cex.main=cx.main, cex.lab=cx.lab, cex.axis=cx.axis)
  }
  
 invisible(return(b))
} # end bplot function