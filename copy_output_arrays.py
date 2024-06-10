from pathlib import Path
from shutil import copytree

dst_dir = Path(__file__).parent / "Arrays"
dst_dir.mkdir(exist_ok=True)

fires = ["Caldor", "CedarCreek", "CubCreek2", "Dixie", "KNP"]

for fire in fires:
    fire_src = Path(__file__).parent / "QF_runs" / "Expanded_Sampling" / fire
    fire_dst = dst_dir / fire
    fire_dst.mkdir(exist_ok=True)
    sites = [f"{fire}{i}" for i in range(1, 19)]
    runs = [f"{fire}_{site}_duet" for site in sites]
    for run in runs:
        site_dst = fire_dst / run
        site_dst.mkdir(exist_ok=True)
        src = fire_src / run / "Arrays"
        dst = site_dst / "Arrays"
        copytree(src, dst)
