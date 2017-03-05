to_vw = function(DT, file, ...) {
  fwrite(vw_dt(DT, ...), file, header = FALSE, sep = " ",
         col.names = FALSE, quote = FALSE)
}

vw_dt = function(DT, y, x, tag, precision = '%.5f',
                 transformation = identity) {
  if (!is.data.table(DT)) {
    warning('Input not a data.table; forcing using ', 
            'as.data.table, which requires making a copy. ',
            'Use setDT on your input to avoid this.')
    DT = as.data.table(DT)
  }
  ysub = substitute(y)
  if (missing(tag)) tag = character(nrow(DT))
  tsub = substitute(tag)
  if (missing(x)) x = setdiff(names(DT), as.character(ysub))
  col_format = paste0('%s:', precision)
  DT[ , {
    coln_to_vw = function(vn) { 
      if (is.numeric(get(vn))) {
        sprintf(col_format, vn, transformation(get(vn))) 
      } else {
        sprintf('%s:%s', vn, get(vn)) 
      }
    }
    c(list(y = eval(ysub), 
           l = sprintf('%s|', eval(tsub))), 
      lapply(x, coln_to_vw))
  }]
}
