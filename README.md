# fortran-metrics

A Simple Forecasting Metrics Calculator

- Auto skip N/A data
- Accepts both scalar or vector data

[List of Metrics]

    integer :: N    ! Sample size
    real    :: E    ! (Scalar) Error
    real    :: AE   ! (Scalar) Absolute Error
    real    :: PE   ! (Scalar) Percentage Error
    real    :: APE  ! (Scalar) Absolute Percentage Error
    real    :: MBE  ! Mean Bias Error
    real    :: MAE  ! Mean Absolute Error
    real    :: MAPE ! Mean Absolute Percentage Error
    real    :: RMSE ! Root Mean Squared Error
