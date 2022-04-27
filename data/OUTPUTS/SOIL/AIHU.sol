**********************************************************************
* Template soil data file for PADDY soil water balance model.        *
**********************************************************************
* Soil        : AIHU - texture classes:c("Cl", "Cl", "Cl")
* File name        : AIHU.sol
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
WCSTRP = 0.51, 0.52, 0.52
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
FIXPERC = 0.4952461275

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
CLAYX = 0.62, 0.65, 0.69
SANDX = 0.11, 0.14, 0.12
BD = 1.41, 1.48, 1.47

SOC = 36465.48, 22443.11, 19680.85
SON = 3339.48, 2018.29, 1784.81
SNH4X = 21.27, 12.98, 13.05
SNO3X = 4.64, 4.41, 3.97

*-----------------------------------------------------------------*
* 8. Soil hydrological properties. Required type of data input    *
* according to setting of conductivity and water retention switch *
*-----------------------------------------------------------------*
KST = 7.59, 5.14, 4.95
WCST = 0.51, 0.52, 0.52
WCFC = 0.43, 0.41, 0.41
WCWP = 0.34, 0.33, 0.33
WCAD = 0.18, 0.18, 0.19

*---------------------------------------------------------------*
* 9. Initialization conditions, and re-initialization
*---------------------------------------------------------------*
WL0I = 0.
WCLI = 0.43, 0.41, 0.41
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
