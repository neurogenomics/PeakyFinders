# PeakyFinders 0.99.1

## New features

* Add generic `find_links` function. 

## Bug fixes

* Removed duplicated files from Google Drive ("(1).R"). 
* Accidentally hard-coded a GSM ID into `process_ids`. 
* Extract supp files from both the project-level GSE ID and sample-level GSM ids.
* Figure out why package is unable to find `"peaks_metadata_roadmap"` unless package is loaded.
* Force all rownames to be unique. 

# PeakyFinders 0.99.0

## Bug fixes  

* Select `SnowParam` vs. `SerialParam` depending on OS to avoid issues on Windows. 
* Fix vignette formatting. 
* Add error handlers for `import.bw` on Windows 
(until someone fixes `rtracklayer`). 
* Add error handlers for `MACSr` on Windows (until it becomes compatible). 
* Remove `import_bigwig_filtered` function (not used). 

## New features 

* Added a `NEWS.md` file to track changes to the package.
* Set GHA to push to *DockerHub*.  
* Give `call_peaks_macsr` it's own subfunction. 
* Add conda env. 
