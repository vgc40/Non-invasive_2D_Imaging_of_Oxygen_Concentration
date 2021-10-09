# Non-invasive_2D_Imaging_of_Oxygen_Concentration
This GitHub is associated with the publication “Non-invasive 2-D Imaging of Dissolved Oxygen Concentration in Flow-Through Sediment Columns” submitted to PLoS One (Garayburu-Caruso et al., 2021). 

This work presents a novel application of planar optode technology to sediment columns that allows the study of well-constrained flow fields without the need to assume completely homogeneous flow. We introduce a flow-through column with the interior coated in an oxygen sensing film and a series of sampling ports as a unique tool capable of capturing changes in DO at high frequency (10 s) and spatial (< 1 mm) resolution across the length of a column. 

This repository provides custom R scripts that allow image processing steps to be automated and consistently reproduced over a variety of experimental conditions. It is comprised of three folders (1) Column_Input_parameters, (2) Example_Images, and (1) Example_output/Column_1/DI. Column_Input_parameters contains a csv file with an example of column and injection specific input parameters for the image processing script.Example_images contains blue, green and red filter otode images to use as example images when running the code. Example_output/Column_1/DI contains a series of subfolder to examplify the folder structure needed to store the outputs of Basic_processing_raster.R code. 

The scripts needed for the image processing are the following: 
- 01_Image_pre_procesing.py

- 02_Find_Column_parameters.R
 This code is intended to help the user find the values for the necessary Column_Input_paramers parameters needed in the Input_col_parameters_DI.csv

- 03_Basic_processing_raster.R
