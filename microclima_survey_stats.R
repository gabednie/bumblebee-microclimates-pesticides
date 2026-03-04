# ------------------------------------------------------------
# Summarise Microclima hourly Tair rasters to CSV
# (mean, min, max, sd per raster)
# ------------------------------------------------------------

library(terra)

root_dir  <- "F:/Kerr Lab Paper/Microclima Outputs"
out_csv   <- "F:/Kerr Lab Paper/microclima_hourly_means_advanced.csv"

# 1. Find valid "Site X" folders (ignore "Site_X")
site_dirs <- list.dirs(root_dir, recursive = FALSE, full.names = TRUE)
site_dirs <- site_dirs[grepl("^Site\\s+\\d+$", basename(site_dirs))]

results <- vector("list", length = 0)  # to collect rows
row_idx <- 1

for (sd in site_dirs) {
  site_name <- basename(sd)                     # e.g. "Site 1"
  site_num  <- sub("^Site\\s+", "", site_name)  # e.g. "1"
  
  # 2. Within each site, find survey folders like "site1_20160609"
  surv_dirs <- list.dirs(sd, recursive = FALSE, full.names = TRUE)
  surv_dirs <- surv_dirs[grepl("^site\\d+_\\d{8}$", basename(surv_dirs), ignore.case = TRUE)]
  
  if (!length(surv_dirs)) next
  
  for (sdir in surv_dirs) {
    surv_basename <- basename(sdir)                 # e.g. "site1_20160609"
    date_str      <- sub(".*_(\\d{8})$", "\\1", surv_basename)  # "20160609"
    date_val      <- as.Date(date_str, format = "%Y%m%d")
    
    # 3. Find the "hourly_YYYYMMDD" folder inside this survey directory
    hourly_dirs <- list.dirs(sdir, recursive = FALSE, full.names = TRUE)
    hourly_dirs <- hourly_dirs[grepl("^hourly_\\d{8}$", basename(hourly_dirs))]
    if (!length(hourly_dirs)) next  # no hourly rasters for this survey
    
    # (Assume one hourly folder per survey date; take the first if multiple)
    hdir <- hourly_dirs[1]
    
    # 4. List Tair hourly rasters: Tair_YYYYMMDDT0000Z.tif etc.
    tif_files <- list.files(
      hdir,
      pattern = "^Tair_\\d{8}T\\d{4}Z\\.tif$",
      full.names = TRUE
    )
    if (!length(tif_files)) next
    
    message("Processing: ", site_name, " | ", date_val, " | ", length(tif_files), " rasters")
    
    for (tf in tif_files) {
      fname <- basename(tf)
      
      # Extract time from filename: Tair_20160609T2200Z.tif -> "2200" -> "22:00"
      time4 <- sub("^Tair_\\d{8}T(\\d{4})Z\\.tif$", "\\1", fname)
      time_str <- paste0(substr(time4, 1, 2), ":", substr(time4, 3, 4))  # "HH:MM"
      
      # 5. Read raster and compute stats (mean, min, max, sd)
      r <- try(rast(tf), silent = TRUE)
      if (inherits(r, "try-error")) {
        warning("Could not read: ", tf)
        next
      }
      
      mean_val <- try(global(r, "mean", na.rm = TRUE)[1, 1], silent = TRUE)
      if (inherits(mean_val, "try-error")) {
        warning("Could not compute mean for: ", tf)
        next
      }
      
      min_val  <- try(global(r, "min",  na.rm = TRUE)[1, 1], silent = TRUE)
      if (inherits(min_val, "try-error")) {
        warning("Could not compute min for: ", tf)
        next
      }
      
      max_val  <- try(global(r, "max",  na.rm = TRUE)[1, 1], silent = TRUE)
      if (inherits(max_val, "try-error")) {
        warning("Could not compute max for: ", tf)
        next
      }
      
      sd_val   <- try(global(r, "sd",   na.rm = TRUE)[1, 1], silent = TRUE)
      if (inherits(sd_val, "try-error")) {
        warning("Could not compute sd for: ", tf)
        next
      }
      
      # 6. Store row
      results[[row_idx]] <- data.frame(
        Site        = site_num,
        Date        = date_val,
        Time        = time_str,
        Mean_Tair   = as.numeric(mean_val),
        Min_Tair    = as.numeric(min_val),
        Max_Tair    = as.numeric(max_val),
        SD_Tair     = as.numeric(sd_val),
        stringsAsFactors = FALSE
      )
      row_idx <- row_idx + 1
    }
  }
}

# 7. Combine and write to CSV
if (length(results)) {
  out_df <- do.call(rbind, results)
  out_df <- out_df[order(out_df$Site, out_df$Date, out_df$Time), ]
  write.csv(out_df, out_csv, row.names = FALSE)   # overwrites existing CSV with added columns
  message("Done. Wrote: ", out_csv)
} else {
  warning("No hourly Tair rasters found. CSV not written.")
}



# ------------------------------------------------------------
# Summarise Microclima hourly Tair rasters by distance rings
#   Zone 1:   0–500 m
#   Zone 2: 500–2000 m
#   Zone 3: 2000–5000 m
#   Stats: mean, min, max, sd per zone
# ------------------------------------------------------------

library(terra)

root_dir      <- "F:/Kerr Lab Paper/Microclima Outputs"
out_csv_rings <- "F:/Kerr Lab Paper/microclima_hourly_rings.csv"

# 1. Find valid "Site X" folders (ignore "Site_X")
site_dirs <- list.dirs(root_dir, recursive = FALSE, full.names = TRUE)
site_dirs <- site_dirs[grepl("^Site\\s+\\d+$", basename(site_dirs))]

results <- vector("list", length = 0)  # to collect rows
row_idx <- 1

for (sd in site_dirs) {
  site_name <- basename(sd)                     # e.g. "Site 1"
  site_num  <- sub("^Site\\s+", "", site_name)  # e.g. "1"
  
  # 2. Within each site, find survey folders like "site1_20160609"
  surv_dirs <- list.dirs(sd, recursive = FALSE, full.names = TRUE)
  surv_dirs <- surv_dirs[grepl("^site\\d+_\\d{8}$", basename(surv_dirs), ignore.case = TRUE)]
  
  if (!length(surv_dirs)) next
  
  for (sdir in surv_dirs) {
    surv_basename <- basename(sdir)                 # e.g. "site1_20160609"
    date_str      <- sub(".*_(\\d{8})$", "\\1", surv_basename)  # "20160609"
    date_val      <- as.Date(date_str, format = "%Y%m%d")
    
    # 3. Find the "hourly_YYYYMMDD" folder inside this survey directory
    hourly_dirs <- list.dirs(sdir, recursive = FALSE, full.names = TRUE)
    hourly_dirs <- hourly_dirs[grepl("^hourly_\\d{8}$", basename(hourly_dirs))]
    if (!length(hourly_dirs)) next  # no hourly rasters for this survey
    
    # (Assume one hourly folder per survey date; take the first if multiple)
    hdir <- hourly_dirs[1]
    
    # 4. List Tair hourly rasters: Tair_YYYYMMDDT0000Z.tif etc.
    tif_files <- list.files(
      hdir,
      pattern = "^Tair_\\d{8}T\\d{4}Z\\.tif$",
      full.names = TRUE
    )
    if (!length(tif_files)) next
    
    message("Processing (rings): ", site_name, " | ", date_val, " | ", length(tif_files), " rasters")
    
    # --------------------------------------------------------
    # Precompute distance raster and zone masks for this day
    #   Assumes CRS units are in metres and rasters are 5 km buffers
    # --------------------------------------------------------
    r0 <- try(rast(tif_files[1]), silent = TRUE)
    if (inherits(r0, "try-error")) {
      warning("Could not read first raster in: ", hdir)
      next
    }
    
    e   <- ext(r0)
    ctr <- c((e$xmin + e$xmax) / 2, (e$ymin + e$ymax) / 2)   # approximate centre
    p   <- vect(matrix(ctr, ncol = 2), crs = crs(r0))
    
    # Distance from centre (m)
    dist_r <- try(distance(r0, p), silent = TRUE)
    if (inherits(dist_r, "try-error")) {
      warning("Could not compute distance raster for: ", hdir)
      next
    }
    
    # Zone masks (logical SpatRasters)
    zone1_mask <- dist_r <= 500
    zone2_mask <- dist_r > 500  & dist_r <= 2000
    zone3_mask <- dist_r > 2000 & dist_r <= 5000  # outer radius ~ existing 5 km buffer
    
    for (tf in tif_files) {
      fname <- basename(tf)
      
      # Extract time from filename: Tair_20160609T2200Z.tif -> "2200" -> "22:00"
      time4    <- sub("^Tair_\\d{8}T(\\d{4})Z\\.tif$", "\\1", fname)
      time_str <- paste0(substr(time4, 1, 2), ":", substr(time4, 3, 4))  # "HH:MM"
      
      # 5. Read raster
      r <- try(rast(tf), silent = TRUE)
      if (inherits(r, "try-error")) {
        warning("Could not read: ", tf)
        next
      }
      
      # Make zone-specific rasters (values only where mask is TRUE)
      r_z1 <- try(ifel(zone1_mask, r, NA), silent = TRUE)
      r_z2 <- try(ifel(zone2_mask, r, NA), silent = TRUE)
      r_z3 <- try(ifel(zone3_mask, r, NA), silent = TRUE)
      if (inherits(r_z1, "try-error") || inherits(r_z2, "try-error") || inherits(r_z3, "try-error")) {
        warning("Could not apply zone masks for: ", tf)
        next
      }
      
      # 6. Compute stats per zone (mean, min, max, sd)
      s1 <- try(global(r_z1, c("mean", "min", "max", "sd"), na.rm = TRUE), silent = TRUE)
      s2 <- try(global(r_z2, c("mean", "min", "max", "sd"), na.rm = TRUE), silent = TRUE)
      s3 <- try(global(r_z3, c("mean", "min", "max", "sd"), na.rm = TRUE), silent = TRUE)
      if (inherits(s1, "try-error") || inherits(s2, "try-error") || inherits(s3, "try-error")) {
        warning("Could not compute zone stats for: ", tf)
        next
      }
      
      # Extract as numeric (one row x 4 columns)
      z1_mean <- as.numeric(s1[1, "mean"]); z1_min <- as.numeric(s1[1, "min"])
      z1_max  <- as.numeric(s1[1, "max"]);  z1_sd  <- as.numeric(s1[1, "sd"])
      
      z2_mean <- as.numeric(s2[1, "mean"]); z2_min <- as.numeric(s2[1, "min"])
      z2_max  <- as.numeric(s2[1, "max"]);  z2_sd  <- as.numeric(s2[1, "sd"])
      
      z3_mean <- as.numeric(s3[1, "mean"]); z3_min <- as.numeric(s3[1, "min"])
      z3_max  <- as.numeric(s3[1, "max"]);  z3_sd  <- as.numeric(s3[1, "sd"])
      
      # 7. Store row
      results[[row_idx]] <- data.frame(
        Site          = site_num,
        Date          = date_val,
        Time          = time_str,
        Z1_Mean_Tair  = z1_mean,
        Z1_Min_Tair   = z1_min,
        Z1_Max_Tair   = z1_max,
        Z1_SD_Tair    = z1_sd,
        Z2_Mean_Tair  = z2_mean,
        Z2_Min_Tair   = z2_min,
        Z2_Max_Tair   = z2_max,
        Z2_SD_Tair    = z2_sd,
        Z3_Mean_Tair  = z3_mean,
        Z3_Min_Tair   = z3_min,
        Z3_Max_Tair   = z3_max,
        Z3_SD_Tair    = z3_sd,
        stringsAsFactors = FALSE
      )
      row_idx <- row_idx + 1
    }
  }
}

# 8. Combine and write to CSV
if (length(results)) {
  out_df <- do.call(rbind, results)
  out_df <- out_df[order(out_df$Site, out_df$Date, out_df$Time), ]
  write.csv(out_df, out_csv_rings, row.names = FALSE)
  message("Done. Wrote ring stats to: ", out_csv_rings)
} else {
  warning("No hourly Tair rasters found. Ring CSV not written.")
}


##### CALCULATE FOLIAGE HEIGHT DIVERSITY (DYNAMIC BINS) ####################

library(terra)
library(vegan)

# ---- Paths ----
chm_folder <- "D:/Gabby/PhD/Kerr Lab Paper/Lidar/CHM"
out_dir    <- "F:/Kerr Lab Paper"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
output_csv <- file.path(out_dir, "CHM_heterogeneity.csv")

# ---- Get list of CHM files: siteX_CHM.tif, excluding *_test ----
all_tifs  <- list.files(chm_folder, pattern = "\\.tif$", full.names = TRUE)
bn        <- basename(all_tifs)

keep <- grepl("^site[0-9]+_CHM\\.tif$", bn, ignore.case = TRUE) &
  !grepl("_test\\.tif$", bn, ignore.case = TRUE)

chm_files <- all_tifs[keep]

if (!length(chm_files)) {
  stop("No CHM files matching 'siteX_CHM.tif' (excluding *_test) found in: ", chm_folder)
}

# ---- Find global max height across all selected CHM rasters ----
cat("Computing global max CHM height across", length(chm_files), "rasters...\n")

max_heights <- sapply(chm_files, function(f) {
  r <- rast(f)
  as.numeric(global(r, "max", na.rm = TRUE))
})

global_max <- max(max_heights, na.rm = TRUE)
cat("Global max CHM height:", global_max, "m\n")

# ---- Build dynamic reclassification matrix in 0.5 m increments ----
# First two classes mirror your original setup:
#   (-Inf, 0.1] and (0.1, 0.5]
rows <- list()
cid  <- 1
rows[[cid]] <- c(-Inf, 0.1, cid); cid <- cid + 1
rows[[cid]] <- c(0.1, 0.5, cid); cid <- cid + 1

# Now 0.5 m bins starting at 0.5 m up to just below the max,
# with a final open-ended bin for anything above the top bin.
top_floor <- floor(global_max * 2) / 2      # round down to nearest 0.5
if (top_floor < 0.5) top_floor <- 0.5      # safety

lower_breaks <- seq(0.5, top_floor - 0.5, by = 0.5)
if (length(lower_breaks) > 0) {
  for (lb in lower_breaks) {
    rows[[cid]] <- c(lb, lb + 0.5, cid)
    cid <- cid + 1
  }
}

# Last closed bin: [top_floor - 0.5, top_floor)
if (top_floor >= 0.5) {
  last_lb <- top_floor - 0.5
  rows[[cid]] <- c(last_lb, top_floor, cid)
  cid <- cid + 1
}

# Final open bin: [top_floor, Inf)
rows[[cid]] <- c(top_floor, Inf, cid)

reclass_m <- do.call(rbind, rows)
colnames(reclass_m) <- c("from", "to", "class")
cat("Reclass matrix built with", nrow(reclass_m), "classes.\n")

# ---- Initialize results data frame ----
results <- data.frame(
  Site    = character(),
  Shannon = numeric(),
  Simpson = numeric(),
  stringsAsFactors = FALSE
)

# ---- Loop through CHM files ----
for (chm_file in chm_files) {
  file_name <- tools::file_path_sans_ext(basename(chm_file))  # e.g., "site1_CHM"
  site      <- sub("_CHM$", "", file_name, ignore.case = TRUE) # e.g., "site1"
  
  cat("Processing:", file_name, "\n")
  
  # Load CHM raster
  chm_raster <- rast(chm_file)
  
  # Reclassify using dynamic matrix
  chm_reclassified <- classify(chm_raster, reclass_m)
  
  # Extract values, drop NA
  vals <- values(chm_reclassified, mat = FALSE)
  vals <- vals[!is.na(vals)]
  
  if (!length(vals)) {
    warning("All values are NA after reclassification for ", file_name, "; skipping.")
    next
  }
  
  # Compute diversity indices on class frequencies
  canopy_classes <- table(vals)
  shannon_index  <- diversity(canopy_classes, index = "shannon")
  simpson_index  <- diversity(canopy_classes, index = "simpson")
  
  # Append to results
  results <- rbind(
    results,
    data.frame(
      Site    = site,
      Shannon = shannon_index,
      Simpson = simpson_index,
      stringsAsFactors = FALSE
    )
  )
}

# ---- Write results ----
write.csv(results, output_csv, row.names = FALSE)
cat("Diversity indices saved to", output_csv, "\n")
