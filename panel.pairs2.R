
panel.pairs2 = function(x, y, quant=0.75, wh=0.005, alpha=.10, xlab='', ylab='', newplot=F, 
                    cx=2, cx.lab=2, cx.axis=2, cx.pval=1, cadj = 1, 
                    signif.color = 'red', b, nonparametric = T) {
  
  ## Initialize parameters, use only complete cases, drop unused factor levels
  d = data.frame(x=x, y=y)          
  d = droplevels(d[complete.cases(d),])		
  x = d$x                           
  y = d$y                  
  pval=1.5; 											
  text.col='black'									
  get.corr = F											
  p.msg=''
  r.msg=''								
  rval = NA
  
  
  ## Choose type of statistical tests
  if (nonparametric) {
    corr.method='spearman'
    compare.groups = list(wilcox.test, kruskal.test)
    contingency.test = fisher.test
  } else {
     corr.method='pearson'
     compare.groups = list(t.test, aov)
     contingency.test = chisq.test
  }
  
  
  
  ## Create new scatterplot, boxplot, or mosaic plot (if newplot == T)
  if (newplot==T) {
    par(cex=cx, cex.main=cx, cex.lab=cx.lab, cex.axis=cx.axis)
    bplot(x,y, nmx=xlab, nmy=ylab, cx=cx, cx.lab=cx.lab, rpch=1)
    } # end if
  
  
  ####################################
  ## Compute p-values
  ## Plot the points/frequencies
  ## Graph linear regression fits for pairs of continuous variables
  #####################################
  
  ## Case 1: x and y are continuous variables
  ## Test for nonzero correlation
  ## Add fitted regression line to plot
  if ( !is.factor(x) & !is.factor(y) ) {
    pval = cor.test(x,y, exact=F, method=corr.method)$p.value		
    rval = round(cor(x,y, method = corr.method),2) 
    r.msg = paste('r =', rval)	
    get.corr = T										
    fit = lm(y ~ x)         
    abline(fit)             
    d = data.frame(apply(d[,1:2], 2, as.numeric))                
    es = maxEmptyRect(range(x),range(y),x,y)
    es = list(x=es$rect[1], y=es$rect[4])
  }
  
  
  
  ## Case 2: x is a factor, y is continuous
  ## Wilcoxon rank sum test or Kruskall-Wallis test
  if ( is.factor(x) & !is.factor(y) ) {
    if (length(levels(x))==2 ) pval = compare.groups[[1]](y~x, exact=F)$p.value	
    if (length(levels(y))> 2 ) {
      a = compare.groups[[2]](x~y)
      if (nonparametric==T) {
        pval = a$p.value	
      } else {
        pval = summary(a)[[1]][1,'Pr(>F)']
      }
      
    }			
    es = maxEmptyRect(c(0.5,0.5+length(levels(x))),range(y),as.numeric(x),y)
    es = list(x=es$rect[1], y=es$rect[4])
  }
  
  
  
  ## Case 3: x is continuous, y is a factor
  ## Wilcoxon rank sum test or Kruskall-Wallis test
  if ( !is.factor(x) & is.factor(y) ) {
    if (length(levels(y))==2 ) pval = compare.groups[[1]](x~y, exact=F)$p.value	
    if (length(levels(y))> 2 ) {
      a = compare.groups[[2]](x~y)
      if (nonparametric==T) {
        pval = a$p.value	
      } else {
        pval = summary(a)[[1]][1,'Pr(>F)']
      }
      
    }
    ylm = range(b$stats)
    es = maxEmptyRect(range(x), c(0.5,0.5+length(levels(y))), x, as.numeric(y))
    es = list(x=es$rect[1], y=es$rect[4])
  }
  
  
  
  ## Case 4: x and y are factors
  ## Fisher's exact test
  if ( is.factor(x) & is.factor(y)) { 
    (tab = xtabs(~x + y))
    (col.tot = apply(tab, 2, sum))
    (row.tot = apply(tab, 1, sum))
    if (any(c(col.tot, row.tot) == 0) | ncol(tab)==1 | nrow(tab)==1)  {
      pval = NA 
    } else {
      pval = contingency.test(x, y, simulate.p.value=T)$p.value				 			
    }
    text.col='white' 
    nx=nlevels(x)
    ny=nlevels(y)
    es = list(x=(.5)/nx,  y=(.5)/ny)
  }
  
  # print(pval)
  
  ## P-value, for printing on the graph
  ## Print significant p-value in different color (red or color specified)
  if (!is.na(pval)) {
    
    if (pval < .001) {
        (p.msg = paste('p =', round(pval,2)))
      } else {
        (p.msg = 'p < .001')
      }
    if (pval <= alpha) text.col = signif.color
    
    } else {
      p.msg = 'p = NA'
  }
  

  

  ## Print correlation coefficient if it was computed
  if (get.corr==T) 	{	
    text(es[[1]], es[[2]] - 0.10*diff(range(as.numeric(y), na.rm=T)), r.msg, col=text.col, 
         cex=cx.pval*cadj, adj=0)	## Print p-value
  } 
  
  
  text(es[[1]], es[[2]], p.msg, col=text.col, cex=cx.pval*cadj, adj=0)	

  
  invisible(return(list(pval=pval, rval=rval)))
} # end panel.lm function