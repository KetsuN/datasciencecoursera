##Codebook for run_analysis.R

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals 
tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant 
rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter 
with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated 
into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass 
Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals 
(tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were 
calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, 
fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to 
indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.  All signals are included in

- tBodyAcc-XYZ (number)
- tGravityAcc-XYZ (number)
- tBodyAccJerk-XYZ (number)
- tBodyGyro-XYZ (number)
- tBodyGyroJerk-XYZ (number)
- tBodyAccMag (number)
- tGravityAccMag (number)
- tBodyAccJerkMag (number)
- tBodyGyroMag (number)
- tBodyGyroJerkMag (number)
- fBodyAcc-XYZ (number)
- fBodyAccJerk-XYZ (number)
- fBodyGyro-XYZ (number)
- fBodyAccMag (number)
- fBodyAccJerkMag (number)
- fBodyGyroMag (number)
- fBodyGyroJerkMag (number)

The set of variables that were estimated and supplied with this data set are: 

- mean(): Mean value (number)
- std(): Standard deviation (number)

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

- gravityMean (number)
- tBodyAccMean (number)
- tBodyAccJerkMean (number)
- tBodyGyroMean (number)
- tBodyGyroJerkMean (number)

The dataset originally from UCI HAR has been transformed to append 
- Subject (number) 
- Activity performed by subject (string)

Each row in the dataset represents a single Subject/Activity combination along with the average values for each mean and std signal variable.  
