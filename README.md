# CASA0010-Dissertation

The files are to be run in numerical order:
1. school_dataset.ipynb is used to create the ranking of schools (Python)
2. r5r_brighton.ipynb is used to generate geographical variables (city centre and rail stations)  (R)
3. schools_geo.ipynb is used to generate geographical variables for schools (R) 
Between 3. and 4., excel was used to generate the variable of proximity to good school in year of transaction, using horizontal lookup function
4. catchment-elect.ipynb was used to add catchment area and electoral wards categorical variables
5. merge.ipynb was used to merge it all into the full dataset. 
6. analysis was used to run the analysis on the dataset. 

Data sources: 
- House Price Dataset: https://data.london.gov.uk/dataset/house-price-per-square-metre-in-england-and-wales  
- Indices of Multiple Deprivation: https://www.gov.uk/government/collections/english-indices-of-deprivation 
- 2011 Ouput Area Classification: https://data.cdrc.ac.uk/dataset/output-area-classification-2011  
- Rail Station Coordinates: https://www.google.com/maps/place/Brighton/@50.8288553,-0.14359,17z/data=!3m1!4b1!4m6!3m5!1s0x487585743a2f976d:0x59436d7ac9c32976!8m2!3d50.8288519!4d-0.1410151!16zL20vMDNwMzgz?entry=ttu
- CDRC Dataset on Regional and Town Centres: https://data.cdrc.ac.uk/dataset/retail-centre-boundaries-and-open-indicators
- Secondary School Catchment Areas: In data file
- Electoral Wards: https://geoportal.statistics.gov.uk/ 
- R5R files: https://access-ucl.readthedocs.io/en/latest/notebooks/r5r_intro.html# or 
https://a3s.fi/swift/v1/AUTH_0914d8aff9684df589041a759b549fc2/R5edu/Brighton.zip 
- Education files: In data file (schools)
- Full dataset used in analysis: In data file (full_dataset.csv)
