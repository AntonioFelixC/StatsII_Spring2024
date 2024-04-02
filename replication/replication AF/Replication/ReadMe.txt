##############

Replication materials for: Farming Then Fighting: Agricultural Idle Time and Armed Conflict

##############

This replication folder includes several files:

A. STATA DoFiles:
	A.1. DHR_2023_replicationA: The main do file for generating almost all the Tables and Figures in the article and the online appendix.
	A.2. DHR_2023_replicationB_Map: The do file for generating the maps (Figure 2).

B. Shape files and STATA Data Files:
	B.1. AllDataMerged_15May2023_weighted, which is needed for DHR_2023_replicationA and DHR_2023_replicationB_Map dofiles. 
	B.3. Africa_admin1.dbf, Africa_admin1.shx, and Africa_admin1.shp, which are required for DHR_2023_replicationB_Map.

Replication note:

* Download all the files from PSRM's dataverse and place them in one folder.
* Set this folder as your working directory.
* Create a folder within your working directory and name it "FigTbl", so all the figures and tables will be saved there.

### Installing btscs package, if you do not have it installed.

The btscs package is based on Beck et al. (1998) suggested method. btscs-a-binary-time_STATA.pdf explains this package in details. 

Installing btscs package is different than other STATA packages, and you cannot install it directly using ssc install.  Instead, you need to download its compressed folder (btscs.rar), extract it on your computer, and then copy the unzipped file to your ado folder. You can find btscs files online as well. 

If you use Windows, ado folder is probably c:\ado\personal, but it might be someplace else.
If you use Mac, ado folder is probably ~/Documents/Stata/ado, but it might be someplace else. See Accessing personal ado-files to learn how Stata determines which folder to use.
If you use Unix, it is ~/ado/personal (ado/personal in your home directory).

You can also type sysdir. It gives you data on the location of "ado/personal"


Reference:

Beck, Nathaniel, Jonathan N. Katz, and Richard Tucker. "Taking time seriously: Time-series-cross-section analysis with a binary dependent variable." American Journal of Political Science 42, no. 4 (1998): 1260-1288.