import numpy as np
from scipy.io import FortranFile
from pathlib import Path
import matplotlib.pyplot as plt

def _read_dat_file(
    dire: Path, filename: str, arr_dim: tuple, order: str = "C"
) -> np.array:
    """
    Read in a .dat file as a numpy array.

    Dimensions of the array must be known, and in the order (z,y,x)
    """

    # Import and reshape .dat file
    path = dire / filename
    with open(path, "rb") as fin:
        arr = FortranFile(fin).read_reals(dtype="float32").reshape(arr_dim, order=order)

    return arr

def plot_array(x,fig,title,clab):
    plt.figure(fig)
    plt.set_cmap('viridis')
    plt.imshow(x)
    plt.colorbar(label = clab)
    plt.title(title,fontsize=18)
    plt.show()

path = Path("/Users/ntutland/Documents/Projects/Fire-Flood-Mud/QF_runs/KNP/KNP_KaweahNorth_500m/")

arr = _read_dat_file(path,"treesmoist.dat",(83,302,302))
plot_array(arr[0,:,:],1,"duet rhof", "")
