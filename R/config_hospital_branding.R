# Hospital Branding Configuration
# Legacy file - main branding functions moved to branding_getters.R
# This file is preserved for backward compatibility and to provide global variables

# HOSPITAL BRANDING ================================

# Load branding using the new getter functions
# This ensures we use the same safe loading logic everywhere

## Legacy global variables for backward compatibility -----
# These are set during package initialization via .onLoad()
# and are kept here for reference but no longer computed directly

# HOSPITAL_NAME - set by initialize_package_globals()
# HOSPITAL_LOGO_PATH - set by initialize_package_globals()
# my_theme - set by initialize_package_globals()
# HOSPITAL_COLORS - set by initialize_package_globals()

# The actual loading logic is now in branding_getters.R

# Legacy functions and variables are now provided by branding_getters.R
# This file is kept for backward compatibility but contains no active code