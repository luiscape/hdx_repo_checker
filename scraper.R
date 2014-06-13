## Script with series of functions to monitor the
## state of datasets in the HDX repository.

library(rjson)
library(RCurl)

# Function to get a list of the current datasets in the system
# under the HDX organization.
getDatasets <- function() { 
    # Load credentials
    source('auth.R')
    
    message('Setting up a list of the current datasets owned by HDX.\n
             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
    
    # Getting a list of all the datasets uploaded by.
    api_base_url <- 'http://data.hdx.rwlabs.org/api/action/'
    list_url <- paste0(api_base_url, 'organization_show?id=hdx')
    json_out <- fromJSON(getURL(list_url, userpwd = base_key))
    json_out <- json_out[3]
    
    # Get the 'package count' from the resulting JSON.
    a <- json_out$result$package_count
    
    # Get the actual result from the JSON.
    b <- length(json_out$result$packages)
    
    # In case investigation is needed in couting correctly
    # the datasets above.
    if (a != b) {
        # Miscounted packages in the Repo?
        c <- b / a
        verb_message <- paste('It seems that', 
                              round(c-1, 2)*-1, 
                              "% of the datasets are not being counted in the API result. It is worth investigating.")
        message(verb_message)
    }
    
    #############################################################
    ### Getting all the lists necessary to perform the tests. ###
    #############################################################
    
    # Setting the base progress bar.
    pb <- txtProgressBar(min = 0, max = b)
    
    # Getting dataset_url
    message('Getting dataset_url ...')
    getTxtProgressBar(pb)
    for (i in 1:b) {
        # Updating progress bar.
        setTxtProgressBar(pb, i)
        
        x <- as.character(json_out$result$packages[[i]]$name)
        y <- paste0('http://data.hdx.rwlabs.org/dataset/', x)
        if (i == 1) dataset_url <- y
        else dataset_url <- rbind(dataset_url, y)
        
        # Transform to a list in final iteration.
        if (i == b) { 
            dataset_url <- as.list(dataset_url)
            message('Dataset_url: done.')
        }
    }
    message('Overall progress: 20%')
    
    # Getting download_url
    # This one takes a little longer: creating progress bar
    message('Getting download_url ...')
    getTxtProgressBar(pb)
    for (i in 1:b) {
        # Updating progress bar.
        setTxtProgressBar(pb, i)
        
        timA <- system.time(name <- json_out$result$packages[[i]]$name)
        timB <- system.time(x <- paste0(api_base_url, 'package_show?id=', name))
        timC <- system.time(y <- fromJSON(getURL(x, userpwd = base_key)))
        
        z <- y$result$resources[[2]]$url
        
        # estimation not very good ... improve?
        if (i == 1) { 
            download_url <- z
            tim <- as.numeric(timA[1]) + 
                as.numeric(timB[1]) +
                as.numeric(timC[1])
            est <- ceiling((tim * b) / 60)
            mess <- paste('This should take way more than', est, 'minute(s).')
            message(mess)
        }
        else download_url <- rbind(download_url, z)
        
        # Transform to a list in final iteration.
        if (i == b) { 
            download_url <- as.list(download_url)
            message('Download_url: done.')
            # close connection ?
            # close(con, ...)
        }
    }
    message('Overall progress: 40%')
    
    # Getting the file_name
    message('Getting file_name ...')
    getTxtProgressBar(pb)
    for (i in 1:b) {
        # Updating progress bar.
        setTxtProgressBar(pb, i)
        
        name <- json_out$result$packages[[i]]$name
        x <- paste0(api_base_url, 'package_show?id=', name)
        y <- fromJSON(getURL(x, userpwd = base_key))
        z <- y$result$resources[[2]]$name
        
        if (i == 1) { file_name <- z }
        else { file_name <- rbind(file_name, z) }
        if (i == b) { 
            file_name <- as.list(file_name)
            message('IndID: done.')
        }
    }
    message('Overall progress: 60%')
    
    # Getting the indID 
    # Also may take a bit to complete.
    message('Getting indID ...')
    getTxtProgressBar(pb)
    for (i in 1:b) {
        # Updating progress bar.
        setTxtProgressBar(pb, i)
        
        name <- json_out$result$packages[[i]]$name
        x <- paste0(api_base_url, 'package_show?id=', name)
        y <- fromJSON(getURL(x, userpwd = base_key))
        f <- y$result$resources[[2]]$name
        z <- sub(".csv", "", f)
        
        if (i == 1) { indID <- z }
        else { indID <- rbind(indID, z) }
        if (i == b) { 
            indID <- as.list(indID)
            message('IndID: done.')
        }
    }
    message('Overall progress: 80%')
    
    # Creating a list of dataset_title.
    message('Getting dataset_title ...')
    for (i in 1:b) { 
        x <- as.character(json_out$result$packages[[i]]$title)
        if (i == 1) dataset_title <- x
        else dataset_title <- rbind(dataset_title, x)
        
        # Transform to a list in final iteration.
        if (i == b) { 
            dataset_title <- as.list(dataset_title)
            message('dataset_title: done.')
        }
    }
    message('Overall progress: 100%')

    datasets_list <- data.frame(indID, dataset_title, file_name, 
                                dataset_url, download_url)


    #######################
    #### Running Tests ####
    #######################
    
    message('\nRunning performance tests.\n\n')
    # Getting download_error
    for (i in 1:b) {
        
        tryCatch(
            download.file(download_url[i])
        )
        
    }
    
    # Data error
    for (i in 1:b) {
        
        # If timeout error, try again.
        if () {
            i = i - 1
        }
        
        # If any other error, go to next iteration.
        if () {}
        err_mess <- paste('There was an', x, 'error. Trying again')
        message(err_mess)
        finally 
    }
    
    # Other error
    for (i in 1:b) {
        
    }
    
datasets_list
}


# Function to download the .csv dataset from that dataset.
getData <- function(dataset = NULL) {
    
    # Simple function to get data.
    package_url <- paste('http://data.hdx.rwlabs.org/api/action/package_show?id=', 
                             dataset, sep = "")
    dataset_url <- fromJSON(getURL(package_url, userpwd = base_key))
    data <- getDataset('children_under_five_mortality_rate_per_1000_live_births')
    
    # Check if if the downloads are.
    if (dt[2]$success == TRUE) {
        down_url <- dt$result$resources[[2]]$url
        file_name <- paste('../http/data/', dt$result$name, ".csv", sep = "")
        download.file(down_url, destfile = file_name)
    }
    else print('The dataset failed to fetch.')
}

# Inverting rows and columns.




###### Experimenting with tryCatch ###### 
tryCatch(
    message(test), 
    error = function(e) test,
    finally = message('No error.')
    )


### snippet from http://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r

urls <- c(
    "http://stat.ethz.ch/R-manual/R-devel/library/base/html/connections.html",
    "http://en.wikipedia.org/wiki/Xz",
    "xxxxx"
)
readUrl <- function(url) {
    out <- tryCatch(
{
    # Just to highlight: if you want to use more than one 
    # R expression in the "try" part then you'll have to 
    # use curly brackets.
    # 'tryCatch()' will return the last evaluated expression 
    # in case the "try" part was completed successfully
    
    message("This is the 'try' part")
    
    readLines(con=url, warn=FALSE) 
    # The return value of `readLines()` is the actual value 
    # that will be returned in case there is no condition 
    # (e.g. warning or error). 
    # You don't need to state the return value via `return()` as code 
    # in the "try" part is not wrapped insided a function (unlike that
    # for the condition handlers for warnings and error below)
},
error=function(cond) {
    message(paste("URL does not seem to exist:", url))
    message("Here's the original error message:")
    message(cond)
    # Choose a return value in case of error
    return(NA)
},
warning=function(cond) {
    message(paste("URL caused a warning:", url))
    message("Here's the original warning message:")
    message(cond)
    # Choose a return value in case of warning
    return(NULL)
},
finally={
    # NOTE:
    # Here goes everything that should be executed at the end,
    # regardless of success or error.
    # If you want more than one expression to be executed, then you 
    # need to wrap them in curly brackets ({...}); otherwise you could
    # just have written 'finally=<expression>' 
    message(paste("Processed URL:", url))
    message("Some other message at the end")
}
    )    
return(out)
}




