# -*- coding: utf-8 -*-
"""
Created on Mon Jun 14 12:33:09 2021

@author: gara009
"""

import os
import cv2



mypath='C:/Users/gara009/OneDrive - PNNL/Documents/Column experiments/Script_images/Images/Column_1/Sampling'
outpath='C:/Users/gara009/OneDrive - PNNL/Documents/Column experiments/Script_images/Images/Column_1/Sampling/output'

if not os.path.exists(outpath):
    os.makedirs(outpath)



filenamelist=[]



for file in os.listdir(mypath):
    if file.endswith(".tif"):
        filenamelist.append(file)


for file in filenamelist:
    temp=cv2.imread(os.path.join(mypath,file),-1)
    cv2.imwrite(os.path.join(outpath,file),temp)