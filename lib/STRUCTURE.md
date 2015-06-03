#####master.ml
* maintains the worker table and task queues for workers
* responds to requests from workers
  * register a new worker
  * heartbeat the state of a worker, working or idle
  * request the description of a specific task to execute
  * consoult the position of an object
  * publish an object, no matter produced by the worker or retrieved from other workers
* responds to opam task build requests, through a PR number or the pacakge name (+vertion) directly

#####scheduler.ml
* maintains task table, task state table, object table and object hooks table
* when an object is published, the scheduler could use object hooks table to find tasks who just become runnable
  due to the object publishing
* when master finds out an empty task queue, scheduler will come up a runnable task for the given worker ip/port

#####ci_opam.ml
* given a package name (+version), resolves it to an action graph by OpamSolver
* before resolving, modify the host opam configuration as having installed no packages or pinned pakcages
* when given callback functions and an action graph to add to task table
  * for a vertex in the graph with no dependencies, adds it to task graph and marks as runnable
  * for those with dependencies, create a task, gather all the object ids corresponding to its dependencies,
    put them in the `input` field of the just created task, marks as pending

#####worker.ml
* sends heartbeats to master, with state information
* when receives new task id, requests a description from master, finds out all the dependencies,
  gathers them locally or from other workers after consulting the positions from master, executes the task,
  publishs the just produced object, when received objects from other workers, also publishs them to master
* listens for requests from other workers for objects and responds with the content
