**********************************************************************
* Template soil data file for PADDY soil water balance model.        *
**********************************************************************
* Soil        : SDTO - texture classes:c("Lo", "SaLo", "SaLo")
* File name        : SDTO.sol
* Sampling date      : 
* Additional info  : Create with https://github.com/jrodriguez88
*--------------------------------------------------------------------*

SCODE = 'PADDY'

*---------------------------------------------------------------*
* 1. Various soil and management parameters
*---------------------------------------------------------------*
WL0MX = 100.   ! Bund height (mm)
NL = 3        ! Number of soil layers (maximum is 10) (-)
TKL = 0.20, 0.20, 0.20   ! Thickness of each soil layer (m)
ZRTMS = 0.5   ! Maximum rooting depth in the soil (m)

*---------------------------------------------------------------*
* 2. Puddling switch: 1=PUDDLED or 0=NON PUDDLED
*---------------------------------------------------------------*
SWITPD = 0  !Non puddled
NLPUD = 1
WCSTRP = 0.48, 0.44, 0.46
PFCR = 6.0
DPLOWPAN = 0.6

*---------------------------------------------------------------*
* 3. Groundwater switch: 0=DEEP (i.e., not in profile), 1=DATA
* (supplied), 2=CALCULATE
*---------------------------------------------------------------*
SWITGW = 0
ZWTB =   1.,200.,
       366.,200.

ZWTBI = 100. ! Initial groundwater table depth (cm)
MINGW = 100. ! Minimum groundwater table depth (cm)
MAXGW = 100. ! Maximum groundwater table depth (cm)
ZWA   = 1.0  ! Receding rate of groundwater with no recharge (cm d-1)
ZWB   = 0.5  ! Sensitivity factor of groundwater recharge (-)

*---------------------------------------------------------------*
* 4. Percolation switch
* Value for SWITVP cannot be 1 (CALCULATE) for non-puddled soil
*---------------------------------------------------------------*
SWITVP = -1 ! Fixed percolation rate
FIXPERC = 6.466074484

PTABLE =
  1., 1.0,   
 50., 1.0,
100., 20.0,
366., 20.0

*---------------------------------------------------------------*
* 5. Conductivity switch: 0=NO DATA, 1=VAN GENUCHTEN or 2=POWER
*  OR 3= SPAW  function used
*---------------------------------------------------------------*
SWITKH = 0 ! No data

*---------------------------------------------------------------*
* 6. Water retention switch: 0=DATA; 1=VAN GENUCHTEN. When DATA, data
* have to be supplied for saturation, field capacity,
* wilting point and at air dryness
*---------------------------------------------------------------*
SWITPF = 0  ! Data

*---------------------------------------------------------------*
* 7.Soil physical properties, these parameters will be used when model
* runs under actual water or nitrogen condition, or even both. Otherwise
* these parameters will not be used.
*---------------------------------------------------------------*
CLAYX = 0.14, 0.14, 0.15
SANDX = 0.45, 0.57, 0.67
BD = 1.55, 1.66, 1.57

SOC = 34638.50, 23568.94, 13635.28
SON = 2485.96, 2151.38, 1074.27
SNH4X = 25.78, 18.00, 14.55
SNO3X = 43.09, 16.01, 10.71

*-----------------------------------------------------------------*
* 8. Soil hydrological properties. Required type of data input    *
* according to setting of conductivity and water retention switch *
*-----------------------------------------------------------------*
KST = 42.82, 50.97, 64.66
WCST = 0.48, 0.44, 0.46
WCFC = 0.30, 0.25, 0.21
WCWP = 0.20, 0.16, 0.12
WCAD = 0.06, 0.07, 0.08

*---------------------------------------------------------------*
* 9. Initialization conditions, and re-initialization
*---------------------------------------------------------------*
WL0I = 0.
WCLI = 0.30, 0.25, 0.21
RIWCLI = 'NO'

*---------------------------------------------------------------*
* 10. Initialization of soil thermal conditions
*---------------------------------------------------------------*
SATAV = 25.
SOILT = 25., 23., 22.

*---------------------------------------------------------------*
* 11. Observations/measurements
*    Switches to force observed water content in water balance
*---------------------------------------------------------------*
WCLINT = 1,1,1,
         2,2,2,
         3,3,3
