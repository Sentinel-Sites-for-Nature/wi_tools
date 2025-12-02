## Load necessary packages
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)

## Function to create image assignments for Wildlife Insights projects
WIdist <- function(imageTable, ## Exported data table from Wildlife Insights, be sure to include ALL data (especially images in the "identify" tab)
                   studentTable=NULL, ## This expects a gradebook CSV export from Canvas, though it will also accept a character vector of names.
                   progressCounts=FALSE, ## If TRUE, this returns a summary of progress so far by each student who has registered and identified at least one photo in the WI image table you've included.
                   progressAccounting=FALSE, ## If students are already working but you want to assign additional photos, setting this to TRUE will re-allocate fewer photos to students who have completed more (i.e. be fair and equal for a lab exercise).  Note that this will ignore the studentList names and reallocate based on every user who has tagged an image in the Wildlife Insights project. You can pre-filter out any users you don't want to include here.
                   nStudents=NA, ## If you don't provide table with student names, you can specify the number of students / buckets to divide images across or how many times to assign imagesPerStudent.
                   imagesPerStudent=NA, ## If set, this will limit the number of images assigned per student to your provided number, rather than dividing all unverified images between students.
                   byDeployment=FALSE, ## Should assignments be created per-deployment rather than purely date/time within a project?  This can be helpful as a student will not swap as often (or at all) between cameras/backgrounds, and won't need to worry about timestamps, but they must be trained to filter by deployment in addition to dates.
                   #bridgeDeployments=TRUE, ## Should assignments bridge deployments?  I.e. can a student be assigned images in multiple deployments and have 2 lines in the assignment table?
                   blankVal=1.0 ## This can be set between 1.0, which treats blanks as any other photo, and 0, which means that blanks, while still in each assignment, don't count towards the total photos assigned. Any other number between 0 and 1 (i.e a percentage) will count each blank as a "fraction" of a photo when counting towards the total assigned.
) {
  
  ##### Section 1: progress counts and the first steps of progress accounting #####
  ## It remains unfinished, though the initial logical flow is here and the
  ## progress counter works.  In the end, if progressAccounting is true, there
  ## may need to be a further modification step here, or just an addition to the
  ## logical flow in the next section to change the way assignments are made taking
  ## into account individual progress.
  if(progressCounts==TRUE || progressAccounting==TRUE) { ## If either progress flags are true, generate an IDsummary table
    IDsummary <- imageTable |>
      filter(identified_by != "Computer vision") |>
      group_by(identified_by, is_blank) |>
      summarize(nImages=n()) |>
      pivot_wider(names_from=is_blank,values_from=nImages) |>
      rename(nImages="0",blanks="1") |>
      mutate(nImages=replace_na(nImages,0)) |>
      mutate(blanks=replace_na(blanks,0)) |>
      relocate(blanks,.before=nImages) |>
      mutate(nIDcredits=nImages+(blankVal*blanks)) |>
      ungroup()
  } else{}
  
  if(progressCounts==TRUE && progressAccounting==FALSE) { ## If only progress counts are requested, return those and end the script. Eventually if both are true, I'll have to figure out how to deal with this... perhaps add a "number already identified" column into the output of progressAccounting?
    return(IDsummary)
  } else {}
  
  if(progressAccounting==TRUE) { ## This logic will need to be down in section 4 eventually, but for now as it's not implemented, just throw a warning and return the ID summary if true.
    warning("Progress accounting not implemented yet, as I'm still working out how to deal with names not being exactly the same in WI and Canvas/Student Lists.")##
    return(IDsummary)
  } else {}
  
  ##### Section 2: Student list preparation #####
  ## Output from this section is the studentList table, with two colunns:
  ## 1) student -- student names or character versions of assignment number
  ## 2) assignmentNumber -- a number to match names with image assignments later
  
  ## This set of logic takes either a character vector of student names or a
  ## Canvas gradebook imported as a data frame / tibble, and turns it into 
  ## the required studentList data frame
  if(is.vector(studentTable,mode="character")){ # if studentTable is a character vector of names
    studentList <- as.vector(studentTable)
    studentList <- data.frame(student=studentList,assignmentNumber=1:(length(studentList)))
  }
  else if(is.data.frame(studentTable)) { # if studentTable is a data frame of some kind (this expects a canvas gradebook export by default)
    studentList <- studentTable |> 
      filter(!is.na(`SIS User ID`)) |> # get rid of test student and points possible
      pull(Student) # pull out just the student names as a vector
    studentList <- data.frame(student=studentList,assignmentNumber=1:(length(studentList))) # assign each student an "assignment number"
  }
  else { ## Only gets to here if there isn't a useable student name vector or table 
    if(is.null(studentTable)) {
      if(is.na(nStudents)) {stop("No table or character vector of student names present, please provide a number of students (nStudents) to apply image distribution function across") } # Fatal error if neither student names nor a number of students / buckets to divide into is present
      else{studentList <- data.frame(student=paste0("Assignment ",as.character(1:nStudents)),assignmentNumber=1:nStudents)}
    }
    else {stop("Unrecognized format supplied for student names, please supply a canvas gradebook (as a tibble or data frame) or a character vector of students.") } # Fatal error if unrecognized format for student names
  }
  
  ##### Section 3: Prepare the image data to be sorted #####
  ## Depending on settings, can divide up the images in various ways,
  ## or just prepare the raw table a bit for later use.
  ## These are then fed into the sorting section to be assigned to students,
  ## and the way images are sorted here can change the way they're assigned later.
  
  ## Isolate the images that have only been identified by the classifier
  UnidentifiedImages <- imageTable |>
    filter(identified_by=="Computer vision") |>
    arrange(timestamp) 
  
  ## This set of logic creates the image list table, grouping by deployment or 
  ## prepping each record for date/time assignment
  if(byDeployment==TRUE) {
    imageList <- UnidentifiedImages |>
      mutate(captureDate=date(timestamp)) |> # Extract the date from the timestamp
      group_by(deployment_id, captureDate, is_blank) |>
      summarize(nImages=n()) |> # Get the number of blank and non-blank images per deployment/date
      pivot_wider(names_from=is_blank,values_from=nImages) |>
      rename(nImages="0",blanks="1") |>
      mutate(nImages=replace_na(nImages,0)) |>
      mutate(blanks=replace_na(blanks,0)) |>
      relocate(blanks,.before=nImages) |>
      mutate(nIDcredits=nImages+(blankVal*blanks)) |>
      ungroup()
  } 
  else {
    imageList <- UnidentifiedImages |>
      mutate(nIDcredits=if_else(is_blank==1,blankVal,1)) |> # Add nIDcredits.  This assigns the blank value to blanks, and 1 to non-blanks.
      ungroup() 
  }
  
  ##### Section 4: Assign images to students / buckets
  imageBreakdown <- UnidentifiedImages |> ## Get the number of unidentified images, blanks, etc. for the entire sheet, for reference
    group_by(is_blank) |>
    summarize(nImages=n()) |>
    pivot_wider(names_from=is_blank,values_from=nImages) |>
    rename(nImages="0",blanks="1") |>
    mutate(nIDcredits=nImages+(blankVal*blanks))
  
  ## This divides up all unidentified images in a project between students
  ## if no number per student is specified.
  if(is.na(imagesPerStudent)){ ## If the number of images per student is not defined, divide all remaining images up between students, taking into account blank weighting
    if(byDeployment==TRUE) {
      ## Don't do anything here, as we want imagesPerStudent to remain NA if 
      ## dividing up images by deployment/date rather than date/time
      message(paste0("Number of images per student not specified, images will be distributed as evenly as possible."))
    } else {
      imagesPerStudent <- imageBreakdown |>
        mutate(nIDsPerStudent=floor(nIDcredits/nrow(studentList))) |>
        pull(nIDsPerStudent) 
      
      message(paste0("Number of images per student not specified; ",imagesPerStudent," images are being assigned to each student."))
    }
  } 
  else if(byDeployment==FALSE) { ## check to make sure there are enough images to split between students given the assignment
    imageBreakdown <- imageBreakdown |>
      mutate(remainder=nIDcredits-(imagesPerStudent*nrow(studentList))) 
    leftovers <- imageBreakdown |> pull(remainder)
    if(leftovers<0) {
      warning(paste0("There are not enough images to provide each student with ",imagesPerStudent," images each.  Some students may not have been assigned images."))
      print(imageBreakdown)
    } else if(leftovers>0) {
      warning(paste0("There are ",imageBreakdown$nImages," images with likely animals and ",imageBreakdown$blanks," likely blanks that were not assigned to any student given that ",imagesPerStudent," were to be assigned to each student."))
      print(imageBreakdown)
    } else {}
  }
  else {
    ## If images are assigned by deployment/date, the above warnings don't make sense.
  }
  
  ## This assigns images by deployment and date, not overall for a project by 
  ## date/time.  Because it uses full days, the number assigned is AT LEAST the 
  ## number specified in imagesPerStudent.
  if(byDeployment==TRUE) { 
    
    ## If imagesPerStudent is not specified, weight deployment/dates and assign 
    ## based on weights.  This will leave some images un-assigned, most likely.
    if(is.na(imagesPerStudent)) {
      AssignmentList <- imageList |> 
        ungroup() |>
        mutate(wt=nIDcredits/max(nIDcredits)) |> ## Weight each deployment/date based on how many "credits" it has
        mutate(wtPerStud=sum(wt)/nrow(studentList)) |> ## How much "weight" should each student be assigned if we could get it perfectly even?
        mutate(imgGroup = cumsum_with_reset_min_group(wt, first(wtPerStud))) ## Build assignments based on accumulation of weights
      
      if(length(unique(AssignmentList$imgGroup))<length(studentList$student)) {
        i=0.9
        ## This loop essentially makes sure all students get assigned some images 
        ## in the cases where the "default" split doesn't work. 
        while(length(unique(AssignmentList$imgGroup))<length(studentList$student) && i>0.25){
        AssignmentList <- imageList |> 
          ungroup() |>
          mutate(wt=nIDcredits/max(nIDcredits)) |> ## Weight each deployment/date based on how many "credits" it has
          mutate(wtPerStud=sum(wt)/nrow(studentList)) |> ## How much "weight" should each student be assigned if we could get it perfectly even?
          mutate(imgGroup = cumsum_with_reset_min_group(wt, first(wtPerStud*i))) ## Build assignments based on accumulation of weights
        i=i-0.01
        }
      }
    } 
    ## If imagesPerStudent is specified, divide up the deployment/date groups so
    ## each student gets assigned at least the number of images specified.
    else {
      AssignmentList <- imageList |> 
        ungroup() |>
        mutate(imgGroup = cumsum_with_reset_min_group(nIDcredits, imagesPerStudent))
    }
    
    ## Format the assignment list to be easily usable by students when filtering
    ## on wildlife insights
    AssignmentList <- AssignmentList |> 
      rename(assignmentNumber=imgGroup) |>
      left_join(studentList) |>
      group_by(student,deployment_id) |>
      mutate(startDate=first(captureDate),endDate=last(captureDate)) |>
      group_by(student,deployment_id,startDate,endDate) |>
      summarize(nImages=sum(nImages), nBlanks=sum(blanks))
    
  }
  ## This assigns images by date/time, overall for a project. Though this will
  ## generally assign exactly the number of images specified to each student, 
  ## WI doesn't allow true sorting by a start and end date/time, only by dates,
  ## so it's up to students to find the first and last image they need to 
  ## identify by the timestamp on the image, and this often leads to confusion.
  else {
    AssignmentList <- imageList |>
      ungroup() |>
      mutate(imgGroup = cumsum_with_reset_min_group(nIDcredits, imagesPerStudent)) |>
      rename(assignmentNumber=imgGroup) |>
      left_join(studentList) |>
      group_by(student,is_blank) |>
      mutate(nImages=n()) |>
      group_by(student) |>
      mutate(startDateTime=first(timestamp),endDateTime=last(timestamp)) |>
      group_by(student,startDateTime,endDateTime,is_blank,nImages) |>
      summarize() |>
      pivot_wider(names_from=is_blank,values_from=nImages) |>
      rename(nImages="0",blanks="1") |>
      group_by(student,startDateTime,endDateTime,nImages,blanks) |>
      summarize() |>
      mutate(nImages=replace_na(nImages,0)) |>
      mutate(blanks=replace_na(blanks,0)) |>
      mutate(startDate=date(startDateTime),endDate=date(endDateTime)) |>
      ungroup()
  }
  
  ## Generate warnings about by-deployment assignments that may have excess or too few images
  if(byDeployment==TRUE){ 
    if((AssignmentList |> filter(is.na(student)) |> nrow()) > 0) { ## At least some images were leftover, so all students were assigned some images
      unassignedSummary <- AssignmentList |> 
        ungroup() |>
        filter(is.na(student)) |> 
        group_by(deployment_id) |>
        summarize(totImages=sum(nImages), totBlanks=sum(nBlanks))
      ## Warn given there were leftover images if this statement runs
      warning(paste0("There were ",sum(unassignedSummary$totImages)," images with likely animals and ",sum(unassignedSummary$totBlanks)," likely blanks across ",nrow(unassignedSummary)," deployment(s) that were not assigned to any student given that ",imagesPerStudent," were to be assigned to each student, at a minimum."))
      
    } else { ## No images were left over, so it's possible that some students were not assigned images, check on that here and warn user if some students have no assignment.
      unassignedSummary <- NA
      assignedStudents <- AssignmentList |> 
        ungroup() |>
        filter(!is.na(student))
      unassignedStudents <- anti_join(studentList,assignedStudents) |>
        pull(student) 
      ## Could be fine (i.e. all students got an assignment), but if not, warn the user.
      if(length(unassignedStudents>0)) {
        unassignedStudentsPrint <- paste(unassignedStudents, collapse = "; ")
      warning(paste0("There were not enough images to provide each student with at least ",imagesPerStudent," images each. The ",length(unassignedStudents)," students not assigned any images were: ",unassignedStudentsPrint,"."))
      } else {}
    }
    
  } else {} ## Warnings for byDeployment==FALSE covered earlier in function
  ## End the by-deployment warnings section
  
  return(AssignmentList)
  
}

## Function that accumulates images until the threshold is met (or exceeded, 
## if images are assigned only by deployment and date and not specific times)
cumsum_with_reset_min_group <- function(x, threshold) {
  cumsum <- 0
  group <- 1
  result <- numeric()
  
  for (i in 1:length(x)) {
    cumsum <- cumsum + x[i]
    currgroup <- group
    
    if (cumsum >= threshold) {
      currgroup <- group 
      group <- group + 1
      cumsum <- 0
    }
    result = c(result, currgroup)
  }
  return (result)
}

cumsum_with_reset_group <- function(x, threshold) {
  cumsum <- 0
  group <- 1
  result <- numeric()
  
  for (i in 1:length(x)) {
    cumsum <- cumsum + x[i]
    
    if (cumsum > threshold) {
      group <- group + 1
      cumsum <- x[i]
    }
    result = c(result, group)
  }
  return (result)
}
