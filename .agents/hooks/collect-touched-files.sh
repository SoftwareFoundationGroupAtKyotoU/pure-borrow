#!/usr/bin/env bash
# Shared helper for agent hooks: print files touched by common edit tools.
set -u

input="$(cat)"
command -v jq >/dev/null 2>&1 || exit 0

json_files="$(
  printf '%s' "$input" |
    jq -r '
      .tool_input as $input |
      [
        $input.file_path?,
        $input.path?,
        ($input.files? // [] | .[]?),
        ($input.edits? // [] | .[]?.file_path?)
      ]
      | .[]?
      | select(type == "string")
    ' 2>/dev/null
)"

patch="$(
  printf '%s' "$input" |
    jq -r '
      .tool_input.command?
      // .tool_input.patch?
      // empty
    ' 2>/dev/null
)"

patch_files=""
if [ -n "$patch" ]; then
  patch_files="$(
    printf '%s\n' "$patch" |
      awk '
        /^\*\*\* Add File: / { sub(/^\*\*\* Add File: /, ""); print; next }
        /^\*\*\* Update File: / { sub(/^\*\*\* Update File: /, ""); print; next }
        /^\*\*\* Delete File: / { sub(/^\*\*\* Delete File: /, ""); print; next }
        /^\*\*\* Move to: / { sub(/^\*\*\* Move to: /, ""); print; next }
      '
  )"
fi

{
  printf '%s\n' "$json_files"
  printf '%s\n' "$patch_files"
} | sed '/^$/d' | sort -u
