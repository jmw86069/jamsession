# jamsession

## Overall workflow

### Central file paths are stored as options.

* `jamsession_paths()`

   - sessions: `getOption("jam.sessions_path", "~/Projects/R-sessions")`
   - objects: `getOption("jam.objects_path", "~/Projects/R-objects")`
   - functions: `getOption("jam.functions_path", "~/Projects/R-scripts")`

* direct access to each file path:

   - sessions: `sessions_path=jamsession_paths()$sessions`
   - objects: `sessions_path=jamsession_paths()$objects`
   - functions: `sessions_path=jamsession_paths()$functions`
   
### Sessions / Projects

* `list_jamsessions()` - list sessions
   - sessions_path=jamsession_paths()$sessions

* `grep_jamsessions()` - find sessions by name
   - sessions_path=jamsession_paths()$sessions

* `load_jamsession()` - load session by name
   - sessions_path=jamsession_paths()$sessions
   - loads as a virtual package named based upon the script
   - note that it is able to load as an existing package name

* `save_jamsession()` - save session by name
   - default saves `.GlobalEnv`
   - need to test (or remove) option to save another environment

### Objects

* `list_objects()` - list stored objects
* `grep_objects()` - find objects by name

