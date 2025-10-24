let lockfile = open flake.lock | from json

let duplicates = (
  $lockfile.nodes
  | transpose name input
  | group-by { $in.name | parse --regex '^(?P<name>.+)_(\d+)$' | $in.name.0? }
  | transpose
  | where column0 != ""
  | transpose -rd
)

# Find tree traversals for each of the duplicates
let root = $lockfile.nodes | get $lockfile.root

if ($duplicates | is-empty) {
  print "No duplicate inputs found; check successful"
  exit
}

let traversals = $duplicates | items {|duplicate_name, _|
  mut traversals = []

  # We assume flake inputs are an n-ary tree, not a graph; i.e., no
  # cycles. I *think* this is true, as nix probably tells you off for
  # creating circular dependency trees.
  mut queue = [{path: "" node: $root}]
  while ($queue != []) {
    let visiting = $queue | first 1
    $queue = $queue | skip 1

    # For some reason, nushell returns a list<nothing> if a table
    # column doesn't exist.
    if ($visiting.node.inputs? == [null]) {
      continue
    }

    for $input in ($visiting.node.inputs | transpose) {
      let flake_name = $input.column0
      let lock_name = $input.column1

      # If the lock name is not a string, it refers to a `.follows`
      # entry, which we don't need to worry about
      if (($lock_name | describe) != "string") {
        continue
      }

      let node = {
        path: ($visiting.path | path join $flake_name)
        node: ($lockfile.nodes | get $lock_name)
      }

      if (($lock_name == $duplicate_name) or ($lock_name | str starts-with ($duplicate_name + '_'))) {
        $traversals = $traversals | append $node.path
      }

      $queue = $queue | append $node
    }
  }

  $traversals
}

# For each set of traversals:
# 1. Take the first traversal.
# 2. For each remaining traversal, set the corresponding `.follows` to
#    the first traversal.

print 'Duplicates found; Please fix the `.follows` definitions of the entries'

for traversal in $traversals {
  # Figure out a way to apply these with ast-grep or such; currently
  # matching the `inputs = {}` attrset seems broken?
  print $traversal
}

exit 1
