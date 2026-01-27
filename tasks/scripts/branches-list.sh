#!/usr/bin/env bash
# Lists unique branches with upstream tracking info
# Output format: display-name<TAB>checkout-ref

set -eo pipefail

declare -A local_branches
declare -A remote_branches
declare -A upstream_info

# Get local branches with their upstream tracking
while IFS= read -r line; do
  ref=$(echo "$line" | awk '{print $1}')
  upstream=$(echo "$line" | awk '{print $2}')

  local_branches[$ref]=1
  if [[ -n "$upstream" ]]; then
    upstream_info[$ref]=$upstream
  fi
done < <(git for-each-ref --format='%(refname:short) %(upstream:short)' refs/heads)

# Get remote branches
while IFS= read -r ref; do
  [[ "$ref" == "origin" ]] && continue
  remote_branches[$ref]=1
done < <(git for-each-ref --format='%(refname:short)' refs/remotes/origin | grep -v HEAD)

# Exit if no branches found
if [[ ${#local_branches[@]} -eq 0 && ${#remote_branches[@]} -eq 0 ]]; then
  exit 0
fi

# Calculate max width for alignment
max_width=0
for branch in "${!local_branches[@]}"; do
  len=${#branch}
  ((len > max_width)) && max_width=$len
done
for remote_ref in "${!remote_branches[@]}"; do
  len=${#remote_ref}
  ((len > max_width)) && max_width=$len
done
((max_width += 5))

# Output formatted branch list
{
  # Local branches
  for branch in "${!local_branches[@]}"; do
    if [[ -n "${upstream_info[$branch]:-}" ]]; then
      # Local with upstream: feature/stuff -> origin/feature/stuff
      printf "%-${max_width}s -> %s\t%s\n" "$branch" "${upstream_info[$branch]}" "$branch"
    else
      # Local only: feature/stuff
      printf "%-${max_width}s\t%s\n" "$branch" "$branch"
    fi
  done

  # Remote-only branches
  for remote_ref in "${!remote_branches[@]}"; do
    branch_name="${remote_ref#origin/}"
    if [[ ! -v local_branches[$branch_name] ]]; then
      # Remote only: origin/feature/new
      printf "%-${max_width}s\t%s\n" "$remote_ref" "$remote_ref"
    fi
  done
} | sort
