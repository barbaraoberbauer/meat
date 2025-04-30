# combine ggplot's position_nudge and position_dodge
# found here: https://stackoverflow.com/questions/56816072/position-dodge-and-nudge-y-together

position_nudgedodge <- function(x = 0, y = 0, width = 0.75) {
  ggproto(NULL, PositionNudgedodge,
          x = x,
          y = y,
          width = width
  )
}

PositionNudgedodge <- ggproto("PositionNudgedodge", PositionDodge,
                              x = 0,
                              y = 0,
                              width = 0.3,
                              
                              setup_params = function(self, data) {
                                l <- ggproto_parent(PositionDodge,self)$setup_params(data)
                                append(l, list(x = self$x, y = self$y))
                              },
                              
                              compute_layer = function(self, data, params, layout) {
                                d <- ggproto_parent(PositionNudge,self)$compute_layer(data,params,layout)
                                d <- ggproto_parent(PositionDodge,self)$compute_layer(d,params,layout)
                                d
                              }
)