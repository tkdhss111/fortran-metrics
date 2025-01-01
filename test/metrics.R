y <- 1:5
yhat <- c(1.1, 0.8, 3.3, 3.7, 5.0) 

get.accuracy <- function(yhat, y)
{
  d <- data.frame(MBE  = round(mean(yhat - y), 2),
                  MAE  = round(mean(abs(yhat - y)), 2),
                  MAPE = round(mean(abs((yhat - y) / y))*100, 2),
                  RMSE = round(sqrt(mean((yhat - y)^2)), 2),
                  R    = round(cor(yhat, y), 2),
                  p.value = cor.test(yhat, y)$p.value)
  return(d)
}

get.accuracy(yhat, y)
