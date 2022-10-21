create_trackhub_trackdb <- function(trackdbFile,
                                    hub,
                                    path,
                                    domain,
                                    folders,
                                    filetypes, 
                                    visibility,
                                    genome,
                                    as_list=TRUE,
                                    sep="\t",
                                    verbose=TRUE){ 
    
    messager("Creating file:",basename(trackdbFile),v=verbose) 
    #### Find files ####
    files <- lapply(filetypes,
                    function(x){
                        list.files(path = file.path(path,folders),
                                   pattern= paste0(".",x,"$",collapse = "|"),
                                   ignore.case = TRUE,
                                   full.names = TRUE, 
                                   recursive = TRUE) 
                    })
    if(length(unlist(files))==0){
        stopper("0 files identified from search.")
    } else {
        messager(formatC(length(unlist(files)), big.mark = ","),
                 "file(s) found total.",v=verbose)
    }
    #### Convert bed files to bigBed files ####
    ## UCSC tracks require bigBed format (not bed)
    if("bed" %in% names(filetypes) &&
       length(files$bed)>0){       
        files[["bigBed"]] <- bed_to(files = files$bed, 
                                    save_dir = file.path(path,"bigbed"), 
                                    build = genome, 
                                    formats = "bigbed", 
                                    verbose = verbose)
        files[["bed"]] <- NULL
    }
    #### Create track data ####
    trackdb <- lapply(stats::setNames(names(files),
                                      names(files)), function(ft){
              message("Processing: ",ft)
              x <- files[[ft]]  
              lapply(stats::setNames(x,basename(x)), function(y){ 
                  #### Description page ####
                  descriptionUrl <- create_trackhub_description(
                      file=y,
                      trackdbFile=trackdbFile,
                      path=path,
                      domain=domain,
                      verbose=verbose)
                  #### Lines in trackDb.txt ####
                  list(track=basename(y),
                       bigDataUrl=gsub(path,domain,y), 
                       descriptionUrl=descriptionUrl,
                       shortLabel=basename(y),
                       longLabel=paste(hub,ft,"file:",basename(y)),
                       type=ft,
                       visibility=visibility)  
              })    
          }) 
    #### Convert to text ####
    trackdb_char <- lapply(trackdb,function(db){
        lapply(db, function(lst){
            paste(names(lst),lst,
                  sep = sep,
                  collapse = "\n")
        }) |> paste(collapse = "\n\n")
    }) |> paste(collapse = "\n\n") 
    
    #### Write ####
    messager('Writing trackdb file ==>',trackdbFile,v=verbose)
    dir.create(dirname(trackdbFile),showWarnings = FALSE, recursive = TRUE)
    writeLines(text = trackdb_char, 
               con = trackdbFile)  
    #### Return ####
    if(isTRUE(as_list)){
        return(trackdb)
    } else {
        return(trackdb_char)
    }
}
