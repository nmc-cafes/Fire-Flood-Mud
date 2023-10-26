# -*- coding: utf-8 -*-
"""
Created on Mon May 10 09:34:18 2021

@author: zcope
"""

import numpy as np
#from shutil import copy
import os
import shutil
from distutils.dir_util import copy_tree
import subprocess
import sys
import time
import pickle
from options import options_dict, OG_PATH
import TTRS_QUICFire_Support as ttrs

#Determine which print functions to use
if options_dict['QFVD'] == 4:
    from QFVD4.print_functions import *
elif options_dict['QFVD'] == 5:
    from QFVD5.print_functions import *
else: 
    print("QFVD version not supported. Check options.py.")
    sys.exit(70)

def main():
    ri = options_dict
    #Domain Settings
    cell_nums = [ri['nx'], ri['ny'], ri['nz']]

    #Build Folder Requirements:
    if options_dict['QFVD'] == 4:
        CopyToRuns_PATH = os.path.join(OG_PATH,"QFVD4","copy_to_runs")
    elif options_dict['QFVD'] == 5:
        CopyToRuns_PATH = os.path.join(OG_PATH,"QFVD5","copy_to_runs")
    else: 
        print("QFVD version not supported. Check options.py.")
        sys.exit(70)
    FF_PATH = os.path.join(OG_PATH, "ff_out")
    ri['RUN_PATH'] = RUN_PATH = os.path.join(OG_PATH,"Runs","Chips")
    ri['RESULTS_PATH'] = RESULTS_PATH = os.path.join(OG_PATH,"Results","Chips")
    if not os.path.exists(RUN_PATH):
        os.makedirs(RUN_PATH)
    if not os.path.exists(RESULTS_PATH):
        os.makedirs(RESULTS_PATH)
    
    ###Build and run simmulations      
    
    #copy topo
    src = os.path.join(FF_PATH,"topo.dat")
    dst = os.path.join(RUN_PATH,"topo.dat")
    shutil.copyfile(src,dst)
    
    #copy qf
    src = CopyToRuns_PATH
    dst = RUN_PATH
    copy_tree(src, dst)
    
    #copy fuels
    src = FF_PATH
    copy_tree(src,dst)
    
    ## Process .dat files for parameter sweep
    filepath = dst+"/"
    # read in bulk density array so we know where there are fuels
    filename = 'treesrhof.dat'
    rhof = ttrs.import_fortran_dat_file(filepath+filename,cell_nums)
    # Below is only necessary if we're not sure that the whole fire grid has fuel (ie if from fastfuels)
    # bulkdensity_sum = 1.
    # cnt = -1
    # while bulkdensity_sum!=0: # count up the vertical fire grid until there are no more fuels
    #     cnt +=1 
    #     bulkdensity_sum = np.sum(rhof[cnt:,:,:])
    # ri['nz'] = cnt #actual height of vertical fire grid
    ttrs.export_fortran_dat_file(rhof[:ri['nz'],:,:], filepath+"treesrhof.dat")
    
    # Modifying depth and moisture arrays
    filename = 'treesfueldepth.dat'
    height = ttrs.import_fortran_dat_file(filepath+filename,cell_nums)
    ttrs.plot_array(height[0,:,:], "Fuel Depth (m)")
    # height[np.where(rhof>0.01)] = ri['depth']
    ttrs.export_fortran_dat_file(height[:ri['nz'],:,:],filepath+"treesfueldepth.dat")    
    
    filename = 'treesmoist.dat'
    moist = ttrs.import_fortran_dat_file(filepath+filename,cell_nums)
    # keep everything else at surface fuel moisture
    # moist[0,:,:][np.where(np.logical_and(rhof[0,:,:]>0.01, moist[0,:,:]<ri['cmc']))] = ri['fmc']
    ttrs.plot_array(moist[0,:,:],'Fuel Moisture (%)')
    ttrs.export_fortran_dat_file(moist[:ri['nz'],:,:],filepath+"treesmoist.dat")  
    
    ##Find max topo
    filename = 'topo.dat'
    topo_arr = ttrs.import_topo_dat_file(filepath+filename, cell_nums)
    max_topo = np.max(topo_arr) - np.min(topo_arr)
    ri['max_topo'] = max_topo
    
    # rename firetec files for qf version 5
    if ri['QFVD'] == 5:
        os.chdir(RUN_PATH)
        # os.rename("depth.dat","treesfueldepth.dat")
        # os.rename("bulk_density.dat","treesrhof.dat")
        # os.rename("moisture.dat","treesmoist.dat")
        os.rename("topo.dat","ftelevation.dat")
    
    # Print QF input files
    print_gridlist(ri)
    print_QFire_Advanced_User_Inputs_inp(ri)
    print_QFire_Bldg_Advanced_User_Inputs_inp(ri) 
    print_QFire_Plume_Advanced_User_Inputs_inp(ri)
    print_QP_buildout_inp(ri)
    print_QUIC_fire_inp(ri)
    print_QU_buildings_inp(ri)
    print_QU_fileoptions_inp(ri)
    print_QU_metparams_inp(ri)
    print_QU_movingcoords_inp(ri)
    print_QU_simparams_inp(ri)
    if ri['QFVD'] == 5:
        print_QU_TopoInputs_inp(ri)
    print_rasterorigin_txt(ri)
    print_Runtime_Advanced_User_Inputs_inp(ri)
    print_sensor1_inp(ri)
    if ri['QFVD'] == 4:
        print_topo_inp(ri) 
        
if __name__=="__main__":
    main()