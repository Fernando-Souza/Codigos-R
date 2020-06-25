panel = function(x, y,...) {
    panel.grid(h = -1, v = -1, col.line = grey(0.9))
    panel.xyplot(x, y, ...)
    panel.text(x, y, labels = row.names(dados2),
      col = grey(0.5), cex = 0.7, pos = 3, offset = 1)
  }
