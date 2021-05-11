# abnormal-behavior-chimp
Data and code for "Individual Variation in the Abnormal Behavior Repertoire of Chimpanzees" by Zoë Goldsborough, Elisabeth H.M. Sterck, Frans B.M. de Waal & Christine E. Webb (2021)

This repository contains the R script ("Quality Abnormal Behavior.R") required to run all analyses reported in the manuscript. The files required to run this script are data from individual observations ("Focals_75_S.csv"), supplemental data from group observations ("DataGlobals_AbnS.csv"), and an overview of matrilineal kinship ("Matrilineal kinship matrix.csv")

Several manual operations have occurred that are described here:

1. The file with focals is amended from our raw focal data, as focals for which the individual was absent more than 25% of the time were excluded from analyses and all non-relevant behaviors were stripped
2. The time that individuals were not-visible was calculated manually in Excel using the full dataset and added into the R script. This was done in the following way:
    - For the focals: per individual, we subtracted the timestamp when "not-visible" was scored from the timestamp when "BEHAVIOR_end(not_visible)" was scored. Time absent in the focals was rounded towards the nearest whole or half minute.
    - For the globals: per individual, we subtracted the timestamp when the first behavior was scored with an individual marked as absent from the timestamp when the individual was marked as visible again. 
3. Figure 1 was initially generated with the R script but then altered in Adobe Illustrator to add in the evenness score and total rate of abnormal behavior per individual
