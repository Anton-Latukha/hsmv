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

Pipe in all the files in your project. If I were to do it on this
project, it would be:

    $ find src app test -name '*.hs' | \
      xargs hsmv --from Hsmv --to Hsmv2 \
                 --from-path src/Hsmv.hs --to-path src/Hsmv2.hs

You can use `--dry-run` to see what would be changed in all the files
before running it.

## Scope and limitations

* Renames headers `module X ...`
* Renames imports `import .. X`
* **Does not** rename qualified expressions `X.sort`, for
  example. May in the future.
