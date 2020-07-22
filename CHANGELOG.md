# Changelog

## [0.3.2]

### Fixed
- bad naming of variables_collections
- set required fields in dimension model
- several other alignments for required fields

## [0.3.1]

### Fixed
- NA, NULL and NaN values are now mapped to null in the JSON response. Before NA was stated as "NA" in the JSON which resulted in an array that contained a mixed data types of text and number.

## [0.3] UDF API version 0.1.0

### Added
- overview diagram for the new data model
- new examples for the DataCube model
- added context options for users and server
  * `export_digits` as server context now allows to controll the number of digits in the response json from the R-UDF service

### Changed
- old /udf endpoint is now /udf_legacy
- old coerce functions for structured data are renamed with suffix legacy
- old StructuredData is now StructuredDataLegacy

## [0.2]

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
