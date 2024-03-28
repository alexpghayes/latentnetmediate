# Raw or partially processed data

This folder contains four different datasets. Three of these datasets have
been cleaned and are exported in the `latentnetmediate` R package as data objects
`reddit`, `addhealth` and `smoking`. See the data documentation for details.

The file structure of this folder is as follows:

```
data-raw
├── AddHealth84Communities.Rda  # Li and Le's preprocessed AddHealth data
├── README.md                   # this file
├── addhealth                   # directory containing raw AddHealth data
├── clean-add-health.R
├── clean-reddit.R
├── clean-smoking.R
├── download-add-health.R
├── reddit                      # directory containing raw Reddit data
└── smoking.csv                 # raw smoking data
```

## The Li & Le version of the AddHealth Data

The important additional dataset not exported here is
`AddHealth84Communities.Rda`, a version of the AddHealth data (supposedly pulled
from the same online source as the data files in the `addhealth` directory), but
pre-processed by Tianxi Li and Can Le for their 2022+ paper on network 
regression [1]. I do not know what pre-processing they used to generate the 
networks in `AddHealth84Communities.Rda`, but would eventually like to
reproduce the results from their paper in this package for comparison purposes.

## Data to update and import

Glasgow data from Siena package, pulled from <https://www.stats.ox.ac.uk/~snijders/siena/Glasgow_data.htm> on 2022-06-08


## References

[1] Le, Can M., and Tianxi Li. “Linear Regression and Its Inference on Noisy Network-Linked Data.” ArXiv:2007.00803 [Stat], 2021. <http://arxiv.org/abs/2007.00803>.
