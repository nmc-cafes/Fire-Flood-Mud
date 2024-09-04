from pathlib import Path
from pfdf import watershed
from pfdf.raster import Raster
from pfdf.segments import Segments


if __name__ == "__main__":
    dem_path = Path(__file__).parent / "KNP" / "yucca_creek_DEM.tif"
    # Watershed analysis
    dem = Raster(dem_path)
    conditioned = watershed.condition(dem)
    flow = watershed.flow(conditioned)
    npixels = watershed.accumulation(flow)

    # Delineate a network
    large_enough = npixels.values > 250
    segments = Segments(flow, mask=large_enough)

    # Filtering
    pass

    # Hazard assessment models
    pass

    # Locate basins in parallel
    segments.locate_basins(parallel=True)

    # Save to file
    save_path = Path(__file__).parent / "KNP" / "yucca_basis.geojson"
    segments.save(save_path, type="basins")
