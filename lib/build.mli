(** Build and test all the opam packages in a given build context on the given platform.
    [~repo] is the ID of the repository-under-test on GitHub. *)
val v :
  platforms:Platform.t list Current.t ->
  repo:Current_github.Repo_id.t Current.t ->
  spec:Spec.t Current.t ->
  Current_git.Commit.t Current.t ->
  ([> `Built | `Checked ] Current_term.Output.t * Current.job_id option) Current.t

val make_build_spec :
  base:Current_docker.Raw.Image.t ->
  repo:Current_github.Repo_id.t ->
  variant:Variant.t ->
  ty:Spec.ty ->
  opam_version:[`V2_0|`V2_1] ->
  Obuilder_spec.t
