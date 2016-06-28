
  
pairs2 = function(dat, save.plot=F, plot.name='pairs2.pdf', alpha=0.10, 
                  cx=1, cx.main=1, cx.lab=0.5, cx.axis=1, cx.text=2, cx.pval=0.9, nonparametric=T ) {
  
  ### Function to examine pairwise relationships among variables in a dataframe
  
  ### Given a matrix/dataframe with n columns, creates an n x n matrix of pairwise  
  ### Arguments:
  ### - dat:matrix or dataframe
  ### - save.plot: should the plot be saved to a pdf file? (logical)
  ### - plot.name: name of the saved pdf (character, with or without .pdf extension)
  ### - alpha: significance level; p-values below this value will be printed on the 
  ###     plot in blue; all others will be printed in black (numeric)
  ### - cx: character expansion factor (numeric)
  ### - cx.main: character expansion factor for the plot title (numeric)
  ### - cx.lab: character expansion factor for the axis labels (numeric)
  ### - cx.axis: character expansion factor for the axis annotation (numeric)
  ### - cx.text: character expansion factor for the text along the diagnal of the pairs2 array (numeric)
  ### - cx.pval: character expansion factor for the p-values printed on the plots (numeric)
  ### - nonparametric: Should pairwise tests be done using nonparametric methods? (logical)
  
  ## Load required packages
  library(Hmisc, quietly=T)
  library(plotrix, quietly=T)

  ## Add .pdf extension to file name if missing
  if (length(grep('\\.pdf', plot.name)) == 0) plot.name = paste(plot.name, '.pdf', sep='')
  
  ## Convert matrix to dataframe if applicable
  if (is.matrix(dat)) dat = data.frame(dat)
  
  ## Convert any character or logical variables to factors
  for (q in 1:ncol(dat)) {
    if (is.character(dat[,q])) dat[,q] = factor(dat[,q])
    if (is.logical(dat[,q])) dat[,q] = factor(as.numeric(dat[,q]))
  } # end q loop
  
  ## Expand the plotting pdf grid as the number of columns grows 
 (shr.plot.dim = 1 + ncol(dat)/5)
  
  if (save.plot == T) pdf(plot.name, height=7*shr.plot.dim, width=7*shr.plot.dim)

  ## Set plot margins, character expansion factors
  par(mfrow=c(ncol(dat), ncol(dat)), cex.main=cx.main, cex.lab=cx.lab, cex.axis=cx.axis, 
      mar=c(3,3,2,2), las=2)
  
  ## Matrices to hold p-values and correlation coefficients (if computed)
  pvals = matrix(NA, ncol(dat), ncol(dat))
  colnames(pvals) = rownames(pvals) = colnames(dat)
  rvals = pvals 
  
  for (j in 1:ncol(dat)) {
    for (i in 1:ncol(dat)) {

      if (i==j) {
        plot(c(0,1), c(0,1), xaxt='n', yaxt='n', xlab='', ylab='', bty='n', pch=19, col='white')
        text(0.5, 0.5, colnames(dat)[i], cex=cx.text)
      } else {
        (nmx=colnames(dat)[j])
        (nmy=colnames(dat)[i])
        b = bplot(dat[,j], dat[,i], nmx=nmx, nmy=nmy, cx=cx, cx.lab=cx.lab, cx.axis=cx.axis, 
                  cx.main=cx.main)
        panel.obj = panel.pairs2(dat[,j], dat[,i], newplot=F, cx=cx, b=b, cx.pval=cx.pval, 
                                 nonparametric=nonparametric)
        pvals[i,j] = panel.obj$pval
        if (!is.na(panel.obj$rval)) rvals[i,j] = panel.obj$rval
      } # end else
      
    } # end j loop
  } # end i loop
  
  if (save.plot == T)  invisible(dev.off())
  
  ## Colors for significant p-values in LaTex table
  pvals.color = pvals
  for (q in 1:nrow(pvals)) pvals.color[,q] = sapply(pvals[,q], get.color)
  rownames(pvals.color) = colnames(pvals)
  
  return(list(pvals=pvals, pvals.color=pvals.color, rvals=rvals))
} # end pairs2 function



