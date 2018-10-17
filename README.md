## Rolling Time Window

There is a simple program which computes time series statistics based on sliding time window of 60 seconds

## Run
###Input parameters:
1. _input file path_: absolute or relative file path which can be resolved by FS


### Execute
Run SBT command below:

```bash
sbt "run data.txt" 
``` 

Once program is terminated see output in the same dir at **output.txt**