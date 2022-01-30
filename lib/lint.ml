let opam_version = Opam_version.default

let install_ocamlformat =
  let open Obuilder_spec in
  let cache = [ Obuilder_spec.Cache.v Opam_build.download_cache ~target:"/home/opam/.opam/download-cache" ] in
  let network = ["host"] in
  function
  | Analyse_ocamlformat.Vendored { path } ->
    let opam_file = Filename.concat path "ocamlformat.opam" in
    [
      copy [ opam_file ] ~dst:opam_file;
      run ~network "opam pin add -k path -n ocamlformat %S" path;
      (* Pinned to a directory containing only the .opam file *)
      run ~network "opam depext ocamlformat";
      run ~network ~cache "opam install --deps-only -y ocamlformat";
    ]
  | Opam { version } ->
    [ run ~network ~cache "opam depext -i ocamlformat=%s" version ]

let fmt_spec ~base ~ocamlformat_source ~selection =
  let open Obuilder_spec in
  let { Selection.packages = _; commit; variant = _ } = selection in
  let cache = [ Obuilder_spec.Cache.v Opam_build.download_cache ~target:"/home/opam/.opam/download-cache" ] in
  let network = ["host"] in
  stage ~from:base @@ [
    user ~uid:1000 ~gid:1000;
    run ~network ~cache
      "cd ~/opam-repository && \
       (git cat-file -e %s || git fetch origin master) && \
       git reset -q --hard %s && git log --no-decorate -n1 --oneline \
       && opam update -u" commit commit;
    run ~network ~cache "opam depext -i dune";  (* Necessary in case current compiler < 4.08 *)
                                                (* Not necessarily the dune version used by the project *)
    workdir "/src";
  ] @ (match ocamlformat_source with
      | Some src -> install_ocamlformat src
      | None -> []) @ [
      copy ["."] ~dst:"/src/";
      run "opam exec -- dune build @fmt || (echo \"dune build @fmt failed\"; exit 2)";
  ]

let doc_spec ~base ~opam_files ~selection =
  let cache = [ Obuilder_spec.Cache.v Opam_build.download_cache ~target:"/home/opam/.opam/download-cache" ] in
  let network = ["host"] in
  let open Obuilder_spec in
  stage ~from:base @@
    comment "%s" (Fmt.str "%a" Variant.pp selection.Selection.variant) ::
    user ~uid:1000 ~gid:1000 ::
    Opam_build.install_project_deps ~opam_version ~opam_files ~selection @ [
      (* Warnings-as-errors was introduced in Odoc.1.5.0 *)
      (* conf-m4 is a work-around for https://github.com/ocaml-opam/opam-depext/pull/132 *)
      run ~network ~cache "opam depext -i conf-m4 && opam depext -i dune 'odoc>=1.5.0'";
      copy ["."] ~dst:"/src/";
      run "ODOC_WARN_ERROR=false opam exec -- dune build @doc \
           || (echo \"dune build @doc failed\"; exit 2)";
    ]

let install_opam_dune_lint ~cache ~network ~base =
  let open Obuilder_spec in
  stage ~from:base [
    user ~uid:1000 ~gid:1000;
    run ~cache ~network "git -C ~/opam-repository pull origin master && opam update && opam pin add -yn opam-dune-lint.dev https://github.com/ocurrent/opam-dune-lint.git#a431a1128b9564d1b3d08b8bee2977be47bc6b6b";
    run ~cache ~network "opam depext -i opam-dune-lint";
    run "sudo cp $(opam exec -- which opam-dune-lint) /usr/local/bin/";
  ]

let opam_lint_spec ~base ~opam_files ~selection =
  let cache = [ Obuilder_spec.Cache.v Opam_build.download_cache ~target:"/home/opam/.opam/download-cache" ] in
  let network = ["host"] in
  let open Obuilder_spec in
  stage
    ~child_builds:["opam-dune-lint", install_opam_dune_lint ~cache ~network ~base]
    ~from:base @@
    comment "%s" (Fmt.str "%a" Variant.pp selection.Selection.variant) ::
    user ~uid:1000 ~gid:1000 ::
    Opam_build.install_project_deps ~opam_version ~opam_files ~selection @ [
      workdir "/src";
      copy ["."] ~dst:"/src/";
      run "opam lint %s" (String.concat " " opam_files);
      copy ["/usr/local/bin/opam-dune-lint"]
        ~from:(`Build "opam-dune-lint")
        ~dst:"/usr/local/bin/";
      run "opam exec -- opam-dune-lint";
    ]
