# Changelog

## [0.2.1]

### Changed
- updated getting_started.Rmd for the debugging section from przell (#10 on develop in version 0.3)

### Fixed
- NA, NaN and NULL are mapped to null in JSON instead of NA returned as text "NA".

## [0.2.0]

### Added
- readded the `/examples/data` folder after it was temporarily removed
- added a `getting_started` markdown describing the design choices and the UDF scripting
- added support for UDF data translation from stars to xts and back
- annotation to require a certain called variable with a specific type
- getting_started.Rmd
- endpoint for versions of the service (api version and implementation version)

### Changed
- folder structure in /examples
- moved `send_udf` function into the `openeo` R package
- endpoint `/libs` renamed into `/packages`
- dimension name from "t" into "time" in /examples/data/hypercube.json

### Fixed
- coordinate shift for spatial coordinates

### Removed
- introduction.Rmd, which was integrated into the getting_started.Rmd


## [0.1]

### Changed
- modified the repository
- changed experimental data models to the UDF API (RasterCollectionTile and HyperCube)
- data translation from lists to stars into arrays to stars
- dissolved package structure to plumber 'plumbs' with data_translation scripts

### Removed
- command line tool
- large examples
