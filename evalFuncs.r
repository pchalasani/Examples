# Extract a specific column from a set of named matrices.
# matCols( A = matrixA, B = matrixB, C = matrixC, col = 20)
# tall = bool indicating we want a "tall" (i.e. melted) data-frame
matCols <- function(..., cols = c(), labels = c(), tall = c() ) {
  if(is.null(col))
    cols <- 1
  dots <- unlist( match.call( expand.dots = FALSE )$... )
  nams <- names(dots)
  if(is.null(nams))
    nams <- 1:length(dots)
  if(length(cols) < length(nams)) # one column-name, many matrices
    cols <- rep(cols, length(nams))[ 1: length(nams) ]
  if(length(cols) > length(nams)) {# one matrix, many column-names
    dots <- rep(dots, length(cols))[ 1: length(cols) ]
    nams <- cols
  }
    
  df <- as.data.frame( mapply( function(mat,col) eval(mat)[, col], dots, cols ))
  if(!is.null(labels))
    colnames(df) <- labels else
  colnames(df) <- nams
  
  if(!is.null(tall)) {
    require(reshape)
    df <- cbind(index = rownames(df), df)
    colnames(df)[1] <- tall[1]
    if(!is.na(str2date( df[1,1] ))) df[,1] <- str2date( df[,1])
    df <- melt(df, id = tall[1], variable_name = tall[2])
  }  
  invisible( df )
}




# Take a data-frame = (y1, y2, ... ) and plot multiple y series
# If an id column is supplied, idCol should be that column, 
# else it will be added in as the rownames of the df,
# (which we will try to interpret as dates) or just 1:nrow(df)
# Same = whether to plot all series on same plot.
# expr = expression indicating which parts of series should be highlighted
# The highlighted data will be shaded in light red
mplot <- function(df, idCol = c(), expr = c(), same = T) {
  mf <- match.call()
  if(is.null(idCol)) {
    rnames <- rownames(df)
    if(is.null(rnames))
      rnames <- 1:nrow(df)
    df <- cbind(index = rnames, df)
    idCol <- 1
  }
  if(is.numeric(idCol))
    xname <- colnames(df)[idCol] else
  xname <- idCol
  if(!is.na(str2date( df[1,1] ))) df[,1] <- str2date( df[,1])
  doHighlight <- 'expr' %in% names(mf)
  if(doHighlight) {
    exprVal <- eval(substitute(expr), envir = df )
    exprVal <- na2zero(exprVal)*1
    runs <- runPts( exprVal, contig=TRUE)
    dts <- df[[ xname ]]
    ranges <- data.frame( highlight = as.factor(runs$values), start = dts[ runs$starts ], fin = dts[ runs$ends ])
  }
  df.m <- melt(df, id = xname, variable_name = 'type')
  gg <- ggplot( df.m, aes_string(x= xname)) +  geom_line( aes(y = value, colour = type, group = type), size = 1)
# +    scale_x_discrete(breaks = xname, labels=xname)
  if(doHighlight)
    gg <- gg +  geom_rect( aes(NULL, NULL, xmin = start, xmax = fin, fill = highlight),
                                 ymin = 0, ymax = 2e9, data = ranges) +
                      scale_fill_manual( values = alpha( c('blue','red'), c(0,0.2))) 
  
  if( same )
    final <-  gg + geom_point(aes(y=value, shape = type))    else
  final <- gg + facet_grid(type ~ ., scales = 'free')


  final <- final + opts(axis.text.x = theme_text( angle = -90, hjust = 0))
  print(final)
  invisible(final)
}                   

## "Matrix plot", i.e. take a named arg-list of matrices, and extract the
## column indicated by 'tkr' from each, plot them as time-series,
## highlighting the periods during which the given expression 'ex' is TRUE.
## same = indicates whether to plot them on same plot
matPlot <- function(..., tkr = c(), ex = c(), same = FALSE) {
  df <- matCols(..., cols = tkr )
  p <- eval( substitute( mplot(df, expr = ex, same = same)))
  invisible(p)
}
