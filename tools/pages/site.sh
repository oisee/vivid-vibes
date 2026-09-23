#!/bin/sh
# Assembles the GitHub Pages site: pages/ as it is committed, plus the media
# the player loads (the W3MI objects of src/, under the names the page asks
# for), plus, when given, a folder of recorded frames as rec/.
#
#   tools/pages/site.sh <out> [<recorded frames dir>]
set -eu
out=${1:?usage: tools/pages/site.sh <out> [<rec dir>]}
root=$(cd "$(dirname "$0")/../.." && pwd)
rm -rf "$out"
mkdir -p "$out/media"
cp -R "$root/pages/." "$out/"
for f in "$root"/src/*.w3mi.data.*; do
  name=$(basename "$f")
  obj=${name%%.w3mi.data.*}                              # zo4d_06_plasma%2epng
  file=$(printf '%s' "$obj" | sed 's/%2e/./g' | tr 'A-Z' 'a-z')  # zo4d_06_plasma.png
  cp "$f" "$out/media/$file"
done
if [ "${2:-}" ]; then
  mkdir -p "$out/rec"
  cp -R "$2/." "$out/rec/"
fi
touch "$out/.nojekyll"
echo "site in $out: $(find "$out" -type f | wc -l) files, $(du -sh "$out" | cut -f1)"
