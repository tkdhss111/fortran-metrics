program unit_test1
  use metrics_mo 
  implicit none
  real             :: y(5), y1
  real             :: yhat(5), yhat1
  type(metrics_ty) :: metrics

  y    = [1.0, 2.0, 3.0, 4.0, 5.0]
  yhat = [1.1, 0.8, 3.3, 3.7, 5.0]

  call metrics.calc ( yhat, y )
  call metrics.print

  y1 = 2
  yhat1 = 3
  call metrics.calc ( yhat1, y1 )

  print *, '  E:', metrics.E
  print *, ' AE:', metrics.AE
  print *, ' PE:', metrics.PE
  print *, 'APE:', metrics.APE

end program
