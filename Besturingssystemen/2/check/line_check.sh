#!/usr/bin/env bash

# redirect file args to stdin
cat $* | \

# replace multi-line comments that span a single line
sed 's%/\*.*\*/%%g' | \

# move trailing /* to the beginning of the next line, and replace any newly
# formed single-line comments
sed -r '
s%([^ \t]?)[ \t]*/\*$%\1\n/*%g
Tend
N;s%/\*\n%/*%g;s%/\*.*\*/%%g
:end' | \

# run awk script above
awk -- "$(cat <<EOF
BEGIN {
    # LIMIT is number of lines above which errors are reported (config in env)
    LIMIT = ENVIRON["LIMIT"]
    if (!LIMIT) LIMIT = 30
    indent = 0
    lineno = 1
    failed = 0
}

# count non-empty lines
/./ { block_lines++ }

# don't count single-line comments without preceding code
/^[ \t]*\/\// { block_lines-- }

# don't count multi-line comments (check if first and last line contain code)
/\/\*/ { comment_start = lineno }
/[^ \t][ \t]*\/\*/ { comment_start++ }
/\*\/[ \t][^ \t]*/ { comment_start++ }
/\*\// { block_lines -= lineno - --comment_start }

# count lines within top-level braces
/\{/ && indent++ == 0 {
    block_start = lineno
    block_lines = 0
}
/\}/ && --indent == 0 && --block_lines > LIMIT {
    print block_lines "-line block at line " block_start
    failed = 1
}

# maintain line number for error printing
{ lineno++ }

# exit with status 1 if any blocks where bigger than the limit
END { exit failed }
EOF
)"
