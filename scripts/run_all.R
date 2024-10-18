
# 1. Unification
files_sources <- list.files(path = "scripts/01_unification",
                            full.names = TRUE)
sapply(files_sources, source)
print("Unification done!")

# 2. Quality control
files_sources <- list.files(path = "scripts/02_quality-control",
                            full.names = TRUE)
sapply(files_sources, source)
print("Quality control done!")

# 3. Gap-filling
files_sources <- list.files(path = "scripts/03_gap-filling",
                            full.names = TRUE)
sapply(files_sources, source)
print("Gap-filling control done!")

# 4. Homogenization
files_sources <- list.files(path = "scripts/04_homogenization",
                            full.names = TRUE)
sapply(files_sources, source)
print("Homogenization done!")

# 5. Visualization
files_sources <- list.files(path = "scripts/05_visualization",
                            full.names = TRUE)
sapply(files_sources, source)
print("Visualization done!")

print("Workflow completed successfully!")