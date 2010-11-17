Listed herein are language incompatibilites between 3.3 and 3.5,
that you should pay attention to when updating.

- Heartbeats were called only when players are logged on. They are now
  called regardless of logged-on players.
  However, the mudlib can activate/deactivate the calls to heartbeats
  globally by using configure_driver()

- Long exections can be detected and traced by setting DC_LONG_EXEC_TIME with
  configure_driver(). If an execution exceeds this time, the current LPC stack
  will be dumped into the debug log (execution will continue).

- Some more efuns cause a privilege violation:
  * shutdown() (bug #178)
  * garbage_collection() (bug #178)

