"""
Run ensemble of sample sites
Adapted from code provided by Zach Cope 2/15/24
"""

from shutil import rmtree
from pathlib import Path
import subprocess
from multiprocessing import Pool
import os
from quicfire_tools.inputs import QU_Simparams


def main():
    # Specify the directory containing the executables
    # ensemble_dir = Path(__file__).parent / "QF_runs" / "Severe_Steep"
    ensemble_dir = Path("D:/Fire-Flood-Mud/QF_runs/SBS")

    # Get a list of all executable files in the directory
    # fires = ["Caldor", "CedarCreek", "CubCreek2", "Dixie", "KNP"]
    fires = ["CedarCreek", "CubCreek2"]
    executables = []
    for fire in fires:
        for site in range(1, 21):
            if os.path.exists(
                os.path.join(ensemble_dir, fire, f"{fire[:3]}{site}_COR")
            ):
                exe = os.path.join(ensemble_dir, fire, f"{fire[:3]}{site}_COR")
                simparams = QU_Simparams.from_file(
                    os.path.join(ensemble_dir, fire, f"{fire[:3]}{site}_COR")
                )
                simparams.quic_domain_height = 3000
                simparams.to_file(
                    os.path.join(ensemble_dir, fire, f"{fire[:3]}{site}_COR")
                )
                executables.append(exe)
    # Number of concurrent processes
    concurrent_processes = 4

    # Create a pool of workers
    pool = Pool(processes=concurrent_processes)

    # Iterate through the executables and run them
    for executable in executables:
        pool.apply_async(run_executable, (executable,))

    # Close the pool and wait for all processes to finish
    pool.close()
    pool.join()


def run_executable(executable):
    subprocess.run(["wsl", "./quicfire_LIN64.exe"], cwd=executable)


if __name__ == "__main__":
    main()
