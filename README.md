## CISO:

A (distributed) Continuous Integration engine for OPAM

### Install

Ciso is not yet properly released so you need to pin some dev packages to
compile and install the project:

```shell
opam pin add opam-lib --dev
opam pin add fmt https://github.com/dbuenzli/fmt -n
opam pin add ciso https://github.com/samoht/ciso.git
```

### Architecture

There are three major parts in this system: **master** **worker** **scheduler**.

**Master** maintains three tables and listen for http request made by **worker**s and the **shceduler**.
The tables are task table, object table, and a worker table.
Here, a _task_ is a package build, who could possibly be divided into multiple smaller _task_.
_Object_ is the type of the output of a _task_, and also the inputs/dependencies of a _tast_.

**Worker** polls master to see if there is new _task_ dispatched by the **scheduler**.
If so, fetch the _task_ and execute the _task_, the results of the execution is supposed to save as a local file.
And a reference to this _object_ will be published to the object table in the **master**.
Any other **worker** who needs this _object_ to execute their own _task_ will make a request to get this _object_ directly

There are more parts to add, like a github repo listener, who listens PR from ocaml/opam-repository, and add tasks in the task table of the **master**
Rigth now, this project is in phase **pre-alpha**

### License

ISC. See the [LICENSE](./blob/master/LICENSE) file.