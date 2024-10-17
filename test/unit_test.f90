program unit_test

  use metrics_mo 

  implicit none

  real             :: y(5), y1
  real             :: yhat(5), yhat1
  type(metrics_ty) :: metrics

  !======================================
  ! Vector data test
  !
  print *, 'Test: Vector data'
  yhat = [1.1, 0.8, 3.3, 3.7, 5.0]
  y    = [1.0, 2.0, 3.0, 4.0, 5.0]

  call metrics%calc ( yhat, y )
  call metrics%print

  !======================================
  ! Scalar data test
  !
  print *, 'Test: Scalar data'
  yhat1 = 3
  y1 = 2

  call metrics%calc ( yhat1, y1 )
  call metrics%print

  !======================================
  ! NA data test
  !

  ! Scalar NA
  print *, 'Test: yhat1 = NA'
  yhat1 = NA
  y1 = 2

  call metrics%calc ( yhat1, y1 )
  call metrics%print

  ! All NA's
  print *, 'Test: yhat = [NA, NA, NA, NA, NA]'
  yhat = [NA, NA, NA, NA, NA]

  call metrics%calc ( yhat, y )
  call metrics%print

  ! Partial NA's
  print *, 'Test: yhat = [1.0, NA, NA, 2.0, NA]'
  yhat = [1.0, NA, NA, 2.0, NA]

  call metrics%calc ( yhat, y )
  call metrics%print

end program
