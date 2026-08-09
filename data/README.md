# Data
## Welfare Model
The calibration of the welfare model is based on various literature review sources, empirical studies and surveys. Namely,

  Secretaría Distrital de Movilidad. (2023). Encuesta de movilidad (Informe técnico). Secretaría
  Distrital de Movilidad. Bogotá, Colombia.
  
## Open Data Source Repository (Trunk Line F)
Name: 	Validaciones SITP
Source: Sistema FCS
	Base de datos de Recaudo de Bogotá S. A.S
	https://datosabiertos-transmilenio.hub.arcgis.com/

Link: https://datosabiertos-transmilenio.hub.arcgis.com/documents/2085b5a41a0243c7958ebeb36911bb1a/explore 

Google Drive with datasets: https://console.cloud.google.com/storage/browser/validaciones_tmsa;tab=objects?prefix=&forceOnObjectsSortingFiltering=false 
	
"It contains the information related to user records at the moment they enter a station of the trunk component or when boarding a bus from the zonal, special, complementary, or dual components within the SITP."
The analysis uses passenger validation and exit records from Bogotá's TransMilenio system.

# Comment
The raw datasets are not included in this repository. They should be
obtained from the original data provider and placed in this directory
under the following filenames:

- `validaciones.csv`
- `salidas.csv`

The R scripts expect these files to be located in the `data/` directory.
