# Sort files by their length.
# Usage: bash sorted.sh one_or_more filenames
wc -l "$@" | sort -n
