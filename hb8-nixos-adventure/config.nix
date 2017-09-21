# This file:
# ~/.config/nixpkgs/config.nix
#
# To install
# nix-env -f "<nixpkgs>" -iA ghcEnv
# nix-env -iA nixos.haskellPackages.cabal-install
#
# To see what's installed:
# nix-env -q
# nix-env -qaP -A nixos.haskellPackages
# nix-env -f "<nixpkgs>" -qaP -A haskell.packages.ghc763
#
#
# Next will list a few packages:
# nix-env -f "<nixpkgs>" -qaP -A haskellPackages
#
# And occasionally to upgrade the installed packages:
# nix-env -u
#
# To remove
# nix-env -e ghcEnv
#
#
# Alternatively:
# nix-build "<nixpkgs>" -A ghcEnv
#
# and then
#
# nix-shell -p ghcEnv
# nix-shell -p ghcEnv821
# Shell started to compile and produce docs for those that use buildEnv 
# (on the 1st time, 2nd faster, unless used nix-build first).
{ pkgs }:
{
  allowUnfree = true;
  allowBroken = true;
  permittedInsecurePackages = [
    "webkitgtk-2.4.11"
  ];
  haskellPackageOverrides = self: super: {
  # nix-env -f "<nixpkgs>" -iA haskellPackages.reflex-dom-htmlea
    ### reflex-dom-htmlea = self.callPackage <absolute path>/reflex-dom-htmlea {};
    reflex-dom-htmlea = self.callPackage ~/git/reflex-dom-htmlea {};
  };
  packageOverrides = super: let self = super.pkgs; in with self; rec {
  # The following didn't work
  /* packageOverrides = super: let self = super.pkgs; in */
  /* { */
  /*   haskellPackageOverrides = super.haskellPackages.override { */
  /*     overrides = self: super: { */
  /*       # reflex-dom-htmlea = self.callPackage <absolute path>/reflex-dom-htmlea {}; */
  /*       reflex-dom-htmlea = self.callPackage ~/git/reflex-dom-htmlea {}; */
  /*     }; */
  /*   }; */
    ghcEnv = haskellPackages.ghcWithHoogle (p : with p; [
      alex
      cabal2nix
      cabal-install
      cabal-bounds
      c2hs
      # hsc2hs
      cpphs
      codex
      ghc
      ghcid
      ghc-mod
      hasktags
      haskdogs
      hdevtools
      hlint
      happy
      # hoogle
      hspec
      # hscope # did not compile 26.8.2017, 20.9.2017
      lambdabot
      djinn mueval
      pointfree
      pointful
      stack
      tinc
      styx
    ]);
    # ghc-821 environment. Utils marked with a date didn't compile/install at
    # that date. Haskell-distribution for 821 was announced 25.8.2017.
    # use env: nix-shell -p ghcEnv821
    # The following are tested on unstable channel
    ghcEnv821 = with haskell.packages.ghc821; buildEnv {
      name = "ghcEnv821";
      paths = [
          alex
          cabal2nix
          cabal-install
          # cabal-bounds # 20.9.2017 (hmm, worked on 26.8.)
          c2hs
          # hsc2hs
          cpphs
          # codex     # 26.8.2017
          ghc
          ghcid
          # ghc-mod   # 26.8.2017, 20.9.2017
          hasktags
          haskdogs
          # hdevtools # 26.8.2017, 20.9.2017
          hs-pkg-config
          # llvm-pkg-config
          hlint
          happy
          # hoogle    # 26.8.2017
          hspec
          # hscope    # 26.8.2017
          # lambdabot # 26.8.2017
          djinn mueval
          # pointfree # 26.8.2017, 20.9.2017
          pointful
          # stack     # 26.8.2017, 20.9.2017
          # tinc
          styx
          # Extra packages having difficulties when compiling
          zlib 
      ];
    };
  };
    /* # default purescript environment */
    /* # install: nix-env -f "<nixpkgs>" -iA purescriptToolsEnv */
    /* # load: load-env-purescript */
    /* # query: nix-env -f "<nixpkgs>" -qaPA nodePackages */
    /* purescriptEnv = buildEnv { */
    /*   name = "purescriptEnv"; */
    /*   paths = [ */
    /*     nodejs */
    /*     haskellPackages.purescript */
    /*     nodePackages.jshint */
    /*     nodePackages.grunt-cli */
    /*     nodePackages.bower */
    /*     nodePackages.gulp */
    /*   ]; */
    /* }; */

/* #     # default xmonad env */
/* #     # install: nix-env -f "<nixpkgs>" -iA xmonadToolsEnv */
/* #     # load: load-env-xmonad */
/* #     xmonadToolsEnv = buildEnv { */
/* #       name = "xmonadTools"; */
/* #       paths = with .haskellPackages; [ */
/* #        xmonad */
/* #        xmonad-contrib */
/* #        xmonad-extras */
/* #        xmobar */
/* #       ]; */
/* #     }; */

    /* # default mynodejs environment */
    /* # install: nix-env -f "<nixpkgs>" -iA nodeJSToolsEnv */
    /* # load: load-env-mynodejs */
    /* # query: nix-env -f "<nixpkgs>" -qaPA nodePackages */
    /* nodeJSEnv = buildEnv { */
    /*   name = "nodeJS"; */
    /*   paths = [ */
    /*     # nodejs */
    /*     # nodePackages.npm */
/* #         nodePackages.jshint */
/* #         nodePackages.bower */
/* #         nodePackages.grunt-cli */
    /*     nodePackages.npm2nix */
    /*     nodePackages.bower2nix */
    /*     nodePackages.phantomjs */
    /*     nodePackages.nodemon */
    /*     chromedriver */
    /*     # rhino */
    /*     # nodejs nodePackages.npm nodePackages.jshint */
    /*     # nodePackages.grunt-cli nodePackages.npm2nix nodePackages.bower2nix */
    /*   ]; */
    /* }; */

    /* # install: nix-env -f "<nixpkgs>" -iA androidToolsEnv */
/* #     androidToolsEnv = buildEnv { */
/* #       name = "androidTools"; */
/* #       paths = [ */
/* #         androidsdk_4_4 */
/* #         idea.android-studio */
/* #         idea.idea-community */
/* #         heimdall */
/* #       ]; */
/* #     }; */

}
