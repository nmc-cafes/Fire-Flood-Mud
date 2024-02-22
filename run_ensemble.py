"""
Run ensemble of sample sites
Adapted from code provided by Zach Cope 2/15/24
"""

from pathlib import Path
import subprocess
import time
import shutil
import json
import os


def main():
    PATH_ENSEMBLE = Path(__file__).parent / "QF_runs"
    PATH_FIRES = [dir for dir in PATH_ENSEMBLE.iterdir() if dir.is_dir()]
    PATH_QF = Path(__file__).parent.parent.parent / "Quicfire" / "QF_6.0.0" / "exe"

    # Tracking Vars
    PATH_BUILT_SIMS = []
    PROCS = []
    MAX_PARALLEL_SIMS = 12

    for fire in PATH_FIRES:
        for sim in fire.iterdir():
            if sim.is_dir():
                PATH_BUILT_SIMS.append(sim)

    ## Run simulations
    for path2run in PATH_BUILT_SIMS:
        shutil.copy(
            src=PATH_QF / "quicfire_LIN64.exe", dst=path2run / "quicfire_LIN64.exe"
        )
        MAC_QF_PATH = path2run / "quicfire_MACI.exe"
        MAC_QF_PATH.unlink(missing_ok=True)
        OUTPUT_PATH = path2run / "Output"
        if OUTPUT_PATH.exists():
            shutil.rmtree(OUTPUT_PATH)

        # Create QF working directory for subprocess
        CWD_SUBPROCESS = str(Path.cwd() / path2run)
        print("Running simulation: ", path2run)
        start_time = time.time()
        proc = subprocess.Popen(
            "./quicfire_LIN64.exe",
            cwd=CWD_SUBPROCESS,
            stdout=subprocess.PIPE,
            preexec_fn=os.setpgrp,
        )
        PROCS.append({"proc": proc, "path": path2run, "start_time": start_time})

        if len(PROCS) >= MAX_PARALLEL_SIMS:
            PROCS = remove_completed_processes(PROCS)
    while len(PROCS) > 0:
        PROCS = remove_completed_processes(PROCS)


def remove_completed_processes(PROCS: list):
    time.sleep(60)
    for proc in PROCS:
        p = proc["proc"]

        # check if the process has finished
        try:
            p.communicate(timeout=10)
        except subprocess.TimeoutExpired:
            continue

        if not p.returncode is None:
            PROCS.remove(proc)
    return PROCS


if __name__ == "__main__":
    main()
