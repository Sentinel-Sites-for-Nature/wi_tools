# Wildlife Insights Image Distribution Function

#### Distribute WI computer-vision classified images to individuals (students) for verification.

This function takes an image data spreadsheet exported from Wildlife Insights and divides un-verified images between students (either a number of students or a list of student names, format descriptions below). You must provide the image data table and either the names of students or number of students; all other arguments are optional.

Tip: The most straightforward way for students to filter for a group of photos is by deployment and date range, which is not the default here as it results in varying numbers of photos being assigned to each student.  However, thanks to limitations of the wildlife insights filters, the "precise" method of assigning exactly some number of photos within a project purely based on date/time stamps leads to students identifying the wrong photos.

Instead, *I suggest setting* `byDeployment` *to* `TRUE` *in order to make student assignments easier*; just be aware that `imagesPerStudent` then becomes a minimum number assigned rather than the same number per student.  This is because varying numbers of photos are taken by each camera each day, of course, and it is not possible to assign a deployment/date range with a perfectly even number of photos per student without overly complicating the assignments with a mixture of dates and deployments.

You'll find an example R script that creates assignment tables using an example canvas gradebook (with made-up student names and ID numbers, don't worry!) and two image table examples: one a simple single-deployment image set, and the other a moderately more complex image set with several deployments and some images already verified.


#### Details

*Function:* `WIdist()`

*Arguments:*

-   `imageTable` Exported data table from Wildlife Insights, be sure to include ALL data (especially images in the "identify" tab)
-   `studentTable` This expects a gradebook CSV export from Canvas or a character vector of student names.
-   `progressCounts` this returns a summary of progress so far by each student who has registered and identified at least one photo in the WI image table you've included.
-   `progressAccounting` If students are already working but you want to assign additional photos, setting this to TRUE will re-allocate fewer photos to students who have completed more (i.e. be fair and equal for a lab exercise). Note that this will ignore the studentList names and reallocate based on every user who has tagged an image in the Wildlife Insights project. You can pre-filter out any users you don't want to include here.
-   `nStudents` If you don't provide table with student names, you can specify the number of students / buckets to divide images across or how many times to assign imagesPerStudent.
-   `imagesPerStudent` If set, this will limit the number of images assigned per student to your provided number, rather than dividing all unverified images between students.
-   `byDeployment` Should assignments be created per-deployment rather than purely date/time within a project? This can be helpful as a student will not swap as often (or at all) between cameras/backgrounds, and won't need to worry about timestamps, but they must be trained to filter by deployment in addition to dates.
-   `blankVal` This can be set between the default 1.0, which treats blanks as any other photo, and 0, which means that blanks, while still in each assignment, don't count towards the total photos assigned. Any other number between 0 and 1 (i.e a percentage) will count each blank as a "fraction" of a photo when counting towards the total assigned.
