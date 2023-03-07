{ withHoogle ? true, withCommitHook ? true }:
with (import ./. { });

let
  python-commit-hooks =
    pkgs.python3Packages.pre-commit-hooks.overridePythonAttrs
    (_: { doCheck = false; });
  pre-commit-hooks = import sources.pre-commit-hooks;
  pre-commit-config = if withCommitHook then {
    src = ./.;
    tools = pkgs;
    settings = { hpack = { silent = true; }; };
    hooks = {
      yamllint.enable = true;
      hpack.enable = true;
      nixfmt = {
        enable = true;
        excludes = [ "nix/sources.nix" ];
      };
      nix-linter = {
        enable = true;
        excludes = [ "nix/sources.nix" ];
      };
      trailing-whitespace = {
        enable = true;
        name = "trailing-whitespace";
        entry = "${python-commit-hooks}/bin/trailing-whitespace-fixer";
        types = [ "text" ];
      };
      end-of-file = {
        enable = true;
        name = "end-of-file";
        entry = "${python-commit-hooks}/bin/end-of-file-fixer";
        types = [ "text" ];
      };
    };
  } else {
    src = ./.;
  };
  pre-commit-check = pre-commit-hooks.run pre-commit-config;

  inherit (pkgs.haskell.lib) doBenchmark doCheck;

in haskellPkgs.shellFor {
  packages = p: [ (doBenchmark (doCheck p.stoplight)) ];

  inherit withHoogle;

  doBenchmark = true;

  nativeBuildInputs = with pkgs; [
    cabal-install
    ghcid
    # haskell-language-server
    hlint
    hpack
    ormolu
  ];
  shellHook = ''
    cd ''${CI_PROJECT_DIR:=$PWD}
    { ${pre-commit-check.shellHook} } 2> /dev/null
  '';
}
