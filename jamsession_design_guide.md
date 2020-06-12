# jamsession

## Overall workflow

### Central file paths are stored as options.
Need to decide which function/method to use:

* `jamsession_paths()`
   - sessions
   - objects
   - functions
   
### Sessions / Projects

* `list_jamsessions()` - list sessions
   - sessions_path=jamsession_paths()$sessions

* `grep_jamsessions()` - find sessions by name
   - sessions_path=jamsession_paths()$sessions

* `load_jamsession()` - load session by name
   - sessions_path=jamsession_paths()$sessions
   - default loads into `.GlobalEnv`
   - need to test loading into named environment

* `save_jamsession()` - save session by name
   - default saves `.GlobalEnv`
   - need to test (or remove) option to save another environment
