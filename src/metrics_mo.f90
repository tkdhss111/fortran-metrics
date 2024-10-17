module metrics_mo
  use, intrinsic :: iso_fortran_env, only : stdin  => input_unit,  &
                                            stdout => output_unit, &
                                            stderr => error_unit
  implicit none

  private
  public :: metrics_ty, iNA, NA

  integer, parameter :: iNA = -999
  real,    parameter ::  NA = -999.0

  type metrics_ty
    logical :: is_scalar = .false.
    integer :: N    = iNA ! Sample size
    real    :: E    = NA  ! (Scalar) Error
    real    :: AE   = NA  ! (Scalar) Absolute Error
    real    :: PE   = NA  ! (Scalar) Percentage Error
    real    :: APE  = NA  ! (Scalar) Absolute Percentage Error
    real    :: MBE  = NA  ! Mean Bias Error
    real    :: MAE  = NA  ! Mean Absolute Error
    real    :: MAPE = NA  ! Mean Absolute Percentage Error
    real    :: RMSE = NA  ! Root Mean Squared Error
    character(500) :: msg = 'NA' ! Summary message
  contains
    generic   :: calc => calc_metrics_scalar, calc_metrics_vector
    procedure :: calc_metrics_scalar, calc_metrics_vector
    procedure :: print => print_metrics
  end type

contains

  subroutine calc_metrics_scalar ( this, yhat, y )
    class(metrics_ty), intent(inout) :: this
    real,  intent(in) :: yhat
    real,  intent(in) :: y
    real              :: e
    this%is_scalar = .true.
    if ( is_eq( yhat, NA ) ) then
      write ( stderr, '(a$)' ) '*** Warning: yhat is NA. '
      this%N   = iNA
      this%E   = NA 
      this%PE  = NA 
      this%AE  = NA 
      this%APE = NA 
      return
    end if
    e = yhat - y
    this%N    = 1
    this%E    = yhat - y
    this%PE   = this%E / y * 100.0
    this%AE   = abs( this%E )
    this%APE  = abs( this%PE )
  end subroutine

  subroutine calc_metrics_vector ( this, yhat, y )
    class(metrics_ty), intent(inout) :: this
    real, intent(in)  :: yhat(:)
    real, intent(in)  :: y(:)
    real, allocatable :: yhat_(:)
    real, allocatable :: y_(:)
    real, allocatable :: e(:)
    logical           :: is_na(size(yhat))
    this%is_scalar = .false.
    is_na = is_eq( yhat, NA )
    if ( all( is_na ) ) then
      write ( stderr, '(a$)' ) '*** Warning: All yhats are NAs. '
      this%N    = iNA
      this%MBE  = NA 
      this%MAE  = NA 
      this%MAPE = NA 
      this%RMSE = NA 
      return
    end if
    allocate ( y_(count(is_na) ) )
    yhat_ = pack( yhat, .not. is_na )
    y_    = pack( y,    .not. is_na )
    e = yhat_ - y_
    this%N    = size(e)
    this%MBE  = sum( e ) / this%N
    this%MAE  = sum( abs( e ) ) / this%N
    this%MAPE = sum( abs( e ) / y_ * 100.0 ) / this%N
    this%RMSE = sqrt( sum( e ** 2 ) / this%N )
  end subroutine

  subroutine print_metrics ( this )
    class(metrics_ty), intent(in) :: this
    if ( this%is_scalar ) then
      print '( a, i3, 4(a, f5.2) )', &
      'N:',     this%N,  &
      ', E:',   this%E,  &
      ', AE:',  this%AE, &
      ', PE:',  this%PE, &
      ', APE:', this%APE
    else
      print '( a, i3, 4(a, f5.2) )', &
      'N:',      this%N   , &
      ', RMSE:', this%RMSE, &
      ', MAE:',  this%MAE , &
      ', MBE:',  this%MBE , &
      ', MAPE:', this%MAPE
    end if
  end subroutine

  elemental pure logical function is_eq ( x, ref )
    real, intent(in) :: x
    real, intent(in) :: ref
    is_eq = abs(x - ref) < epsilon(ref)
  end function

end module
