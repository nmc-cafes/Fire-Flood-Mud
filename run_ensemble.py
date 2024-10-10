"""
Run ensemble of sample sites
Adapted from code provided by Zach Cope 2/15/24
"""

from pathlib import Path
import subprocess
from multiprocessing import Pool
import time
import shutil
import json
import os


def main():
    # Specify the directory containing the executables
    ensemble_dir = Path(__file__).parent / "QF_runs" / "Severe_Steep"

    # Get a list of all executable files in the directory
    # fires = ["Caldor", "CedarCreek", "CubCreek2", "Dixie", "KNP"]
    fires = ["Caldor"]
    executables = []
    for fire in fires:
        for site in range(1, 21):
            exe = os.path.join(ensemble_dir, fire, f"{fire[:3]}{site}")
            executables.append(exe)

    # Number of concurrent processes
    concurrent_processes = 3

    # Create a pool of workers
    pool = Pool(processes=concurrent_processes)

    # Iterate through the executables and run them
    for executable in executables:
        pool.apply_async(run_executable, (executable,))

    # Close the pool and wait for all processes to finish
    pool.close()
    pool.join()


def run_executable(executable):
    subprocess.run(
        ["chdir", executable, "&&", "wsl", "./quicfire_LIN64.exe"], shell=True
    )


if __name__ == "__main__":
    main()
