# 02 - Quick start

## Use rebar3 and burbweb_rebar3

## burbweb_admin

## Dev mode

When developing you probably don't want to quit erlang, recompile and then re-run the whole thing again. If you specify `{devmode, true}` in your configuration the `burbweb_reloader` module will start.
This module checks for file changes and recompiles/reloades the files if needed. You can also add additional directories with `burbweb_reloader:add_directory/1`.

*Note!* We semi-support reloading dtl-templates at this stage, but we'll fix this asap.
