{
  pkgs ? import <nixpkgs> { inherit system; },
  system ? builtins.currentSystem,
  ghc ? pkgs.haskell.compiler.ghc883,
  depSrc,
  src,
  name,
}:
let deps = pkgs.stdenv.mkDerivation {
      name = "${name}-deps";
      version = "v1.0.0";
      src = depSrc;
      buildInputs = [ pkgs.stack pkgs.zlib ghc pkgs.gnugrep ];
      LANG = "en_US.UTF-8";

      configurePhase = ''
        export HOME=$NIX_BUILD_TOP/fake-home
        export STACK_ROOT=$NIX_BUILD_TOP/.stack
        mkdir -p fake-home
        mkdir -p $STACK_ROOT
        stack config set system-ghc true
        stack config set install-ghc false
        echo 'allow-different-user: true' >>"$STACK_ROOT/config.yaml"
      '';

      buildPhase = ''
        stack build --no-nix -j4 --only-dependencies
      '';

      installPhase = ''
        for filepath in $STACK_ROOT/snapshots/*/*/*/pkgdb/*.conf; do
          PKGROOT=$(dirname $(dirname "$filepath"))
          echo "Replacing $filepath"
          substituteInPlace "$filepath" --replace " $PKGROOT/" ' ''${pkgroot}/'
        done

        PKGDB=$(stack exec -- ghc-pkg list | grep "\\.stack/" | grep "/pkgdb")
        echo "Recaching $PKGDB"
        stack exec -- ghc-pkg --user --no-user-package-db --package-db "$PKGDB" recache
        echo "Recached"

        mkdir -p $out
        mv $STACK_ROOT $out/.stack
        mv .stack-work $out/.stack-work
      '';
    };
in
pkgs.stdenv.mkDerivation {
  name = name;
  version = "v1.0.0";
  src = src;
  buildInputs = [ pkgs.stack pkgs.zlib ghc ];
  LANG = "en_US.UTF-8";

  configurePhase = ''
    stack --version
    env
    export HOME=$NIX_BUILD_TOP/fake-home
    export STACK_ROOT=$NIX_BUILD_TOP/.stack

    cp -R ${deps}/.stack $STACK_ROOT
    cp -R ${deps}/.stack-work .stack-work
    mkdir -p fake-home

    chown -R "$(whoami)" $STACK_ROOT/
    chown -R "$(whoami)" .stack-work/
    chmod -R 777 $STACK_ROOT/
    chmod -R 777 .stack-work/

    stack config set system-ghc true
    stack config set install-ghc false
  '';

  buildPhase = ''
    stack build --no-nix -j4
  '';

  installPhase = ''
    mkdir -p $out/bin
    stack --local-bin-path=$out/bin build --copy-bins
  '';
}
