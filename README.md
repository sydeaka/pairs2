# pairs2
R function used to examine pairwise relationships among variables in a dataframe. Given a matrix/dataframe with n columns, it returns an n x n matrix plot consisting of scatterplots, mosaic plots, and/or boxplots as appropriate. P-values assoicated with tests of each pairwise relationship are printed on each plot, colored red if significant at the pre-specified significance level.

  Arguments:
  - dat:matrix or dataframe
  - save.plot: should the plot be saved to a pdf file? (logical)
  - plot.name: name of the saved pdf (character, with or without .pdf extension)
  - alpha: significance level; p-values below this value will be printed on the 
      plot in blue; all others will be printed in black (numeric)
  - cx: character expansion factor (numeric)
  - cx.main: character expansion factor for the plot title (numeric)
  - cx.lab: character expansion factor for the axis labels (numeric)
  - cx.axis: character expansion factor for the axis annotation (numeric)
  - cx.text: character expansion factor for the text along the diagnal of the pairs2 array (numeric)
  - cx.pval: character expansion factor for the p-values printed on the plots (numeric)
  - nonparametric: Should pairwise tests be done using nonparametric methods? (logical)
