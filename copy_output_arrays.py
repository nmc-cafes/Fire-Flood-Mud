from pathlib import Path
from shutil import copytree

dst_dir = Path(__file__).parent / "Arrays"
dst_dir.mkdir(exist_ok=True)

fires = ["Caldor","CedarCreek","CubCreek2","Dixie","KNP"]

for fire in fires:
    fire_src = Path(__file__).parent / "QF_runs" / fire
    fire_dst = dst_dir / fire
    fire_dst.mkdir(exist_ok=True)
    sites = [path.name for path in fire_src.iterdir() if path.is_dir()]
    for site in sites:
        site_dst = fire_dst / site
        site_dst.mkdir(exist_ok=True)
        src = fire_src / site / "Arrays"
        dst = site_dst / "Arrays"
        copytree(src,dst)