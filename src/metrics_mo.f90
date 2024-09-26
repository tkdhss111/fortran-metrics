module metrics_mo
  implicit none
  private
  public :: metrics_ty
  type metrics_ty
    integer :: N    ! Sample size
    real    :: E    ! (Scalar) Error
    real    :: AE   ! (Scalar) Absolute Error
    real    :: PE   ! (Scalar) Percentage Error
    real    :: APE  ! (Scalar) Absolute Percentage Error
    real    :: MBE  ! Mean Bias Error
    real    :: MAE  ! Mean Absolute Error
    real    :: MAPE ! Mean Percentage Absolute Error
    real    :: RMSE ! Root Mean Squared Error
    character(500) :: msg ! Summary message
  contains
    generic   :: calc => calc_metrics_scalar, calc_metrics_vector
    procedure :: calc_metrics_scalar, calc_metrics_vector
    procedure :: print => print_metrics
  end type

  integer :: iNA = -999
  real    ::  NA = -999.0

contains

  elemental pure logical function is_eq ( x, ref )
    real, intent(in) :: x
    real, intent(in) :: ref
    is_eq = abs(x - ref) < epsilon(ref)
  end function

  subroutine calc_metrics_scalar ( this, yhat, y )
    class(metrics_ty), intent(inout) :: this
    real,  intent(in) :: yhat
    real,  intent(in) :: y
    real, allocatable :: e(:)
    if ( is_eq( yhat, NA ) ) then
      print '(a)', '*** Warning: yhat is NA.'
      this.N   = iNA
      this.E   = NA 
      this.PE  = NA 
      this.AE  = NA 
      this.APE = NA 
      return
    end if
    e = yhat - y
    this.N    = 1
    this.E    = yhat - y
    this.PE   = this.E / y * 100.0
    this.AE   = abs( this.E )
    this.APE  = abs( this.PE )
  end subroutine

  subroutine calc_metrics_vector ( this, yhat, y )
    class(metrics_ty), intent(inout) :: this
    real, intent(in)  :: yhat(:)
    real, intent(in)  :: y(:)
    real, allocatable :: yhat_(:)
    real, allocatable :: y_(:)
    real, allocatable :: e(:)
    logical           :: is_na(size(yhat))
    is_na = is_eq( yhat, NA )
    if ( all( is_na ) ) then
      !print '(a)', '*** Warning: All yhats are NAs'
      this.N    = iNA
      this.MBE  = NA 
      this.MAE  = NA 
      this.MAPE = NA 
      this.RMSE = NA 
      return
    end if
    yhat_ = pack( yhat, .not. is_na )
    y_    = pack( y,    .not. is_na )
    e = yhat_ - y_
    this.N    = size(e)
    this.MBE  = sum( e ) / this.N
    this.MAE  = sum( abs( e ) ) / this.N
    this.MAPE = sum( abs( e ) / y_ * 100.0 ) / this.N
    this.RMSE = sqrt( sum( e ** 2 ) / this.N )
  end subroutine

  subroutine print_metrics ( this )
    class(metrics_ty), intent(inout) :: this
    print '( a, i3, 4(a, f5.2)$ )', &
    'N:',      this.N   , &
    ', RMSE:', this.RMSE, &
    ', MAE:',  this.MAE , &
    ', MBE:',  this.MBE , &
    ', MAPE:', this.MAPE
  end subroutine

end module
