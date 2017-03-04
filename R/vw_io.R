to_vw = function(DT, file, ...) {
  fwrite(vw_dt(DT, ...), file, header = FALSE, sep = " ",
         col.names = FALSE, quote = FALSE)
}

vw_dt = function(DT, y, x, tag, precision = '%.5f',
                 transformation = identity) {
  ysub = substitute(y)
  if (missing(tag)) tag = character(nrow(DT))
  tsub = substitute(tag)
  if (missing(x)) x = setdiff(names(DT), as.character(ysub))
  col_format = paste0('%s:', precision)
  DT[ , {
    coln_to_vw = function(vn) { 
      sprintf(col_format, vn, transformation(get(vn)))
    }
    c(list(y = eval(ysub), 
           l = sprintf('%s|', eval(tsub))), 
      lapply(x, coln_to_vw))
  }]
}
