let
  buildDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (build dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  sysDepError = pkg:
    builtins.throw ''
      The Nixpkgs package set does not contain the package: ${pkg} (system dependency).
      
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      '';
  pkgConfDepError = pkg:
    builtins.throw ''
      The pkg-conf packages does not contain the package: ${pkg} (pkg-conf dependency).
      
      You may need to augment the pkg-conf package mapping in haskell.nix so that it can be found.
      '';
  exeDepError = pkg:
    builtins.throw ''
      The local executable components do not include the component: ${pkg} (executable dependency).
      '';
  legacyExeDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (executable dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  buildToolDepError = pkg:
    builtins.throw ''
      Neither the Haskell package set or the Nixpkgs package set contain the package: ${pkg} (build tool dependency).
      
      If this is a system dependency:
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      
      If this is a Haskell dependency:
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
in { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "ferry-web"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "GPLv3";
      maintainer = "i@b123400.net";
      author = "b123400";
      homepage = "https://github.com/b123400/ferry-web#readme";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."servant-server" or (buildDepError "servant-server"))
          (hsPkgs."wai" or (buildDepError "wai"))
          (hsPkgs."warp" or (buildDepError "warp"))
          (hsPkgs."regex-tdfa" or (buildDepError "regex-tdfa"))
          (hsPkgs."xml-conduit" or (buildDepError "xml-conduit"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."html-conduit" or (buildDepError "html-conduit"))
          (hsPkgs."http-conduit" or (buildDepError "http-conduit"))
          (hsPkgs."lucid" or (buildDepError "lucid"))
          (hsPkgs."http-media" or (buildDepError "http-media"))
          (hsPkgs."raw-strings-qq" or (buildDepError "raw-strings-qq"))
          (hsPkgs."parsec" or (buildDepError "parsec"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."exceptions" or (buildDepError "exceptions"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."lens" or (buildDepError "lens"))
          (hsPkgs."cache" or (buildDepError "cache"))
          (hsPkgs."clock" or (buildDepError "clock"))
          (hsPkgs."http-api-data" or (buildDepError "http-api-data"))
          (hsPkgs."utf8-string" or (buildDepError "utf8-string"))
          (hsPkgs."cookie" or (buildDepError "cookie"))
          ];
        buildable = true;
        };
      exes = {
        "ferry-web-exe" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."ferry-web" or (buildDepError "ferry-web"))
            ];
          buildable = true;
          };
        };
      tests = {
        "ferry-web-test" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."ferry-web" or (buildDepError "ferry-web"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ./.; }