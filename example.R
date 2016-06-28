
## Clear workspace
rm(list=ls())



## Generate sample dataset
set.seed(100)
n = 40
x1 = rnorm(n, 1, 1)
x2 = rnorm(n, 25, 4.7)
factor.names = c('A', 'B', 'C', 'D')
factor1 = numeric(n)
for (i in 1:n) 	factor1[i] = 
  if (x1[i] + rnorm(1) < median(x1)) sample(factor.names[1:2], 1) else sample(factor.names[3:4], 1)
factor1 = factor(factor1)
character1 = sample(c('apple', 'banana', 'strawberry', 'grape', 'pineapple', 'cherry'), n, replace=T)
character2 = sample(c('tv', 'radio', 'internet', 'billboard', 'magazine'), n, replace=T)

dat = data.frame(
  continuous1 = x1,												
  logical1 = as.logical(sample(c(0,1), n, replace=T)),
  continuous2 = -2 * x1 + rnorm(n, 0, 2),								
  factor1 = factor1,											## factor variable no. 1
  factor2 = factor(ifelse( x1 + rnorm(n) < median(x1), 0, 1)),					
  character1 = character1,
  continuous3 = x2,												
  continuous4 = 1.4 * x2 + rnorm(n, 0, 10),								
  factor3 = factor(ifelse(x1 + x2 > median(x2), 'Healthy', 'Unhealthy')),
  character2 = character2
  )
dat$character1 = as.character(dat$character1)
dat$character2 = as.character(dat$character2)

summary(dat)

(rfiles = list.files())
(rfiles = rfiles[grep('.R', rfiles)])
(rfiles = rfiles[rfiles != 'example.R'])

for (k in 1:length(rfiles)) source(rfiles[k])


## Testing for various numbers of variables
for (q in 2:ncol(dat)) {
#pairs2(dat[,1:q],cx=1, save.plot=F, cx.main=1, cx.lab=.5, cx.axis=1, cx.text=2, cx.pval=0.9)
pairs2(dat[,1:q],cx=1, save.plot=T, cx.main=1, cx.lab=.5, cx.axis=1, nonparametric=F,
       cx.text=2, cx.pval=0.9, plot.name=paste('pairs2-',n,'.pdf', sep=''))
} # end n loop

