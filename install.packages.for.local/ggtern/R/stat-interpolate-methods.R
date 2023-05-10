#' Prediction data frame
#' 
#' Get predictions with standard errors into data frame
#' 
#' @param model the model to predict
#' @param xseq,yseq the x and y values
predictdf2d <- function(model, xseq, yseq) UseMethod("predictdf2d")

predictdf2d.default <- function(model, xseq, yseq ) {
  newdata = expand.grid(x=xseq,y=yseq)
  pred    = stats::predict(model, newdata = newdata, se.fit = FALSE, interval = "none")
  data.frame(newdata, z = as.vector(pred))
}

predictdf2d.glm <- function(model, xseq, yseq) {
  newdata = expand.grid(x=xseq,y=yseq)
  pred    = stats::predict(model, newdata = newdata, se.fit = FALSE, type = "link")
  data.frame(newdata, z = model$family$linkinv(as.vector(pred)))
}

predictdf2d.loess <- function(model, xseq, yseq ) {
  newdata = expand.grid(x=xseq,y=yseq)
  pred    = stats::predict(model,newdata, se = FALSE)
  data.frame(newdata, z = as.vector(pred))
}

predictdf2d.locfit <- function(model, xseq, yseq ) {
  newdata = expand.grid(x=xseq,y=yseq)
  pred    = stats::predict(model, newdata = newdata, se.fit = FALSE)
  data.frame(newdata, z = as.vector(pred))
}


