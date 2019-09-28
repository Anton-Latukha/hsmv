# hsmv

Program to rename Haskell modules.

    $ hsmv --help
    Haskell module rename

    Usage: hsmv [--version] [--help] --from ARG --to ARG --from-path ARG
                --to-path ARG MODULEPATH [--dry-run]
      This program renames modules.

    Available options:
      --version                Show version
      --help                   Show this help text
      --from ARG               The original module name
      --to ARG                 The target module name
      --from-path ARG          The original module path
      --to-path ARG            The target module path
      MODULEPATH               Filepath for a module
      --dry-run                Don't make changes, just print what would be done.
