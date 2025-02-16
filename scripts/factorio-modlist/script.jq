# Exclude these mods from list
def exclude: ["base", "space-age", "quality", "elevated-rails"];

## Input sample:
# {
#   "mods": 
#   [
#     {
#       "name": "base",
#       "enabled": true
#     },
#   ]
# }

# Process and filter mods
.mods[]
  | select(.enabled)
  | .name as $name
  | select(exclude | index($name) | not)
  | "- \($name): https://mods.com/\($name)"
