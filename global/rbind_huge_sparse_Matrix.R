rBind_huge<-function(x,y,fill=NULL,out.class=class(x))
{
  out.class<-force(out.class)
  fillMissing<-is.null(fill)
  if(fillMissing)
    fill<-if(is(x,'Matrix')) 0 else NA
  if (is.null(nrow(x)))
    x<-matrix(x,nrow=1,dimnames=list(NULL,names(x)))
  if (is.null(nrow(y)))
    y<-matrix(y,nrow=1,dimnames=list(NULL,names(y)))
  
  nullNames<-FALSE
  #Cannot currently handle duplicate column names
  if(is.null(colnames(x)))
    colnames(x)<-make.names(colnames(y)[1:ncol(x)],unique = TRUE)
  if(is.null(colnames(y)))
    colnames(y)<-make.names(colnames(x)[1:ncol(y)],unique = TRUE)
  if(is.null(colnames(x)))
  {
    nullNames<-TRUE
    colnames(x)<-1:ncol(x)
    colnames(y)<-1:ncol(y)
  }
  ymiss<-setdiff(colnames(x),colnames(y))
  ybind<-rsparsematrix_huge(nrow=nrow(y),ncol=length(ymiss),0)
  colnames(ybind)<-ymiss
  if(!fillMissing)
    ybind[seq_along(ybind)]<-fill
  xmiss<-setdiff(colnames(y),colnames(x))
  xbind<-rsparsematrix_huge(nrow=nrow(x),ncol=length(xmiss),0)
  colnames(xbind)<-xmiss
  if(!fillMissing)
    xbind[seq_along(xbind)]<-fill
  if (ncol(xbind>0))
    x<-cbind2(x,xbind)
  if(ncol(ybind)>0)
    y<-cbind2(y,ybind)
  y<-as(y,out.class)
  x<-as(x,out.class)
  result<-rbind2(x,y[,order(match(colnames(y),colnames(x)))])
  if(nullNames)
    colnames(result)<-NULL
  return(result)
}


rsparsematrix_huge<-function (nrow, ncol, density, nnz = round(density * maxE), symmetric = FALSE, 
          rand.x = function(n) signif(rnorm(n), 2), ...) 
{
  maxE <- if (symmetric) 
    as.numeric(nrow) * as.numeric(nrow + 1)/2
  else as.numeric(nrow) * as.numeric(ncol)
  stopifnot((nnz <- as.numeric(nnz)) >= 0, nrow >= 0, ncol >= 
              0, nnz <= maxE)
  ijI <- -1L + if (symmetric) 
    sample(indTri(nrow, diag = TRUE), nnz)
  else sample.int(maxE, nnz)
  if (is.null(rand.x)) 
    sparseMatrix(i = ijI%%nrow, j = ijI%/%nrow, index1 = FALSE, 
                 symmetric = symmetric, dims = c(nrow, ncol), ...)
  else sparseMatrix(i = ijI%%nrow, j = ijI%/%nrow, index1 = FALSE, 
                    symmetric = symmetric, x = rand.x(nnz), dims = c(nrow, 
                                                                     ncol), ...)
}