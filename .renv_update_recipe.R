# Safe renv update recipe for SPCify
#
# This script ensures ggrepel fork stays locked while updating other packages
#
# Usage: source(".renv_update_recipe.R")

cat("🔒 SPCify Dependency Update Workflow\n")
cat("=====================================\n\n")

# Step 1: Check current status
cat("📊 Step 1: Current dependency status\n")
status <- renv::status()
print(status)
cat("\n")

# Step 2: Update all packages EXCEPT ggrepel
cat("🔄 Step 2: Updating all packages except ggrepel fork...\n")
cat("   (This preserves ggrepel at commit 0bbdee8)\n\n")

readline("Press [Enter] to continue or [Ctrl+C] to abort: ")

renv::update(exclude = "ggrepel", prompt = TRUE)

cat("\n✅ Update complete!\n\n")

# Step 3: Verify ggrepel unchanged
cat("🔍 Step 3: Verifying ggrepel fork is unchanged...\n")
lock <- jsonlite::fromJSON("renv.lock", simplifyVector = FALSE)
ggrepel_sha <- lock$Packages$ggrepel$RemoteSha

if (ggrepel_sha == "0bbdee8174712929f297da6f0e4170059b0a9344") {
  cat("   ✅ ggrepel fork unchanged (SHA: 0bbdee8...)\n\n")
} else {
  cat("   ⚠️  WARNING: ggrepel SHA changed!\n")
  cat("   Expected: 0bbdee8174712929f297da6f0e4170059b0a9344\n")
  cat("   Got:      ", ggrepel_sha, "\n\n")
}

# Step 4: Run tests
cat("🧪 Step 4: Running test suite...\n")
cat("   Please run: testthat::test_dir('tests/testthat')\n\n")

readline("Press [Enter] after tests pass to snapshot: ")

# Step 5: Snapshot new versions
cat("📸 Step 5: Snapshotting updated dependencies...\n")
renv::snapshot(prompt = TRUE)

cat("\n🎉 Dependency update complete!\n")
cat("   Remember to commit renv.lock:\n")
cat("   git add renv.lock\n")
cat("   git commit -m 'chore(deps): update dependencies (exclude ggrepel)'\n")
