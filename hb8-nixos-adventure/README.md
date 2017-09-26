
# hb8 - Nixos adventure

To learn more about the 
[nix-expressions](https://nixos.org/nix/manual/#chap-writing-nix-expressions) 
and [nix package management](https://nixos.org/nix/manual/), 
my spare machine got [Nixos](https://nixos.org/) installation. Note that nix 
package management can be used with other distros, too.

My main motivation is to use nix with [Haskell](https://www.haskell.org/) 
and [reflex](https://github.com/reflex-frp/reflex-platform). With appropriately
organized nix-files, the work-flow and dev-env can be reasonably fluent 
and practical.



## Machine setup

The [Nixos manual](https://nixos.org/nixos/manual/) describes basic installation 
and configuration.

On various blogs, the approaches mentioned to keeping packages up-to-date include: 
system-wide packages from stable channel with user selected from unstable, or all 
from unstable channel.

System-wide settings will go to the file /etc/nixos/configuration.nix and below 
you may find a couple of examples. Here the system-wide packages don't have any 
Haskell packages as it is possible to make several different environments for 
haskelling, thus enabling the trials with, e.g., different ghc version (see 
below for user configuration).

### Firewall

```
  networking.firewall = {
    enable = true;
    # networking.firewall.allowPing = true;
    # Open ports in the firewall.
    allowedTCPPorts = [ xxx ];
    allowedUDPPorts = [ xxx  yyy ];
    extraCommands = ''
      iptables -I INPUT -p tcp -m tcp -s 192.168.0.zzz --dport yyy -j ACCEPT
      iptables -I INPUT -p udp -m udp -s 192.168.0.zzz --dport yyy -j ACCEPT
    '';
    # iptables-save -I INPUT -p tcp -m tcp -s 192.168.0.zzz --dport yyy -j ACCEPT
  };
```

And use ssh (the port ssh uses is opened above):

```
  # Enable the OpenSSH daemon.
  # services.openssh.enable = false;
  services.openssh.enable = true;
  services.openssh.ports = [ xxx ];
  services.openssh.forwardX11 = true;
```

for which we need the key:

```
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.<username> = {
    isNormalUser = true;
    uid = 1000;
    createHome = true;
    home = "/home/<username>";
    shell = "${pkgs.zsh}/bin/zsh";
    extraGroups = [ "wheel" "networkmanager" "audio" "video"];
    openssh.authorizedKeys.keys = [ "your key goes here" ];
  };
```

### System-wide packages

```
  environment.systemPackages = with pkgs; [
    # General
    wget bash tmux zsh # oh-my-zsh
    binutils coreutils autoconf automake autobuild
    autoreconfHook gitFull unzip unrar lzma libarchive
    # unar
    xpdf zathura zlib zlib.dev htop pciutils hwdata
    file tree lsof psmisc ag

    #     # desktop etc.
    #     gnome3.gtk
    #     gnome3.gconf
    #     gnome3.gnome_desktop
    #     gnome3.gnome_session
    #     gnome3.gnome_shell
    #     gnome3.gnome_terminal
    #     gnome3.gnome-tweak-tool
    #     gnome3.gdm
    #     gnome3.gpaste
    # kDE
    gwenview
    kdeApplications.okular
    kdeApplications.kdf
    # kdeApplications.kdelibs
    kdeApplications.kdenlive
    yakuake

    # Network
    nmap wireshark iftop netcat bind-
    klibcShrunk
    # busybox # lot of collisions

    # Editors
    # vim
    vimHugeX neovim
    # yi - requires something else

    # Languages and dev
    perl python python35Full
    # python27Full python36Full
    gcc
    # gcc7
    gmp glib glibcLocales libtool strace libzip zlib zlib.dev
    pkgconfig
    stdenv # This contains gnu coreutils, c compiler and some other common cmds.
    cmake gnumake ctags
    #     R rWrapper
    sqlite sqliteman sqlitebrowser

    # Other
    firefox chromium conkeror
    elinks lynx w3m pandoc xclip hexchat
```

Intrestingly, kde had/has some screen update problems (it produces some junk 
on the screen when using mouse or writing text with editor), and to find out 
a correct set of packages and services enabled for gnome3 wasn't that easy. 
(Gnome3 didn't clutter my screen.) Anyhow, as using the spare machine mainly 
through ssh the desktop manager really doesn't matter so it is kde now.


Fonts

```
  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      corefonts  # Micrsoft free fonts
      inconsolata  # monospaced
      ubuntu_font_family  # Ubuntu fonts
      terminus_font # for hidpi screens, large fonts
      liberation_ttf
      corefonts
      vistafonts
      proggyfonts
      dejavu_fonts
      font-awesome-ttf
      source-code-pro
      source-sans-pro
      source-serif-pro
    ];
    #fontconfig.dpi = 192;
  };
```


## User config

The provided 
[config](https://github.com/gspia/half-baked/blob/master/hb8-nixos-adventure/config.nix)
takes ideas from the following 
[config](https://github.com/ardumont/dot-files/blob/master/.nixpkgs/config.nix).

It defines environments for ghc and ghc821. The file has also 
other env's in comments (they should be inside packageOverrides).

Be sure to check a bit larger 
[config](https://github.com/jwiegley/nix-config/blob/master/config.nix)
for further ideas.


### How to use environments?

To start shell with some dev tools:
```
nix-shell -p ghcEnv
```

(You may build the env first with 'nix-build "<nixpkgs>" -A ghcEnv'.)

This way it is quite easy and quick to change between ghc802 and ghc821 etc.

### Other notes on config.nix

To use this with reflex, we allow unfree and broken packages, and the use of
webkitgtk-2.4.11 requires permission to use insecure packages at the time of
writing this.

Many blogs, some maybe a bit old, show how to make your own packages available
to other modules you develope - it took several trials before a working setup
was found. By making haskellPackageOverrides, the introduced names (packages) 
can be used in nix-expression of other packages you develop.


### Other things to configure

Nix-packages have many vim plugins and related but I couldn't find 
[haskell-vim-now](https://github.com/begriffs/haskell-vim-now). 
This time just git cloning it, going through the plugins and using own 
installation path with a bit different name was ok, and then using PlugInstall 
and PlugUpdate inside vim.  At the same time this helped to learn
some nice commands like the ones given in easymotion-plugin.
Here you may find some 
[notes](https://github.com/gspia/half-baked/blob/master/hb8-nixos-adventure/vimtips.txt) 
about the plugins and the short-cut keys.

Some additions: [delimitMate](https://github.com/Raimondi/delimitMate), 
[vim-ctrlp-ag](https://github.com/lokikl/vim-ctrlp-ag) 
and [vim-ags](https://github.com/gabesoft/vim-ags).

(And commented away the [vim-haskellConcealPlus](https://github.com/enomsg/vim-haskellConcealPlus)
as I had some difficulties indenting lines.)

Note that some of the shortcut keys don't seem to work (or that their 
useability depends on the environment used). 


Why these changes? The installation script of haskell-vim-now would like to 
install stack and other tools while the configurations described here is used to
do that. (I didn't try the installation script within nixos and was happy with 
the results given by PlugInstall.)

The ghci.conf file is in ~/.ghc-directory and define hoogle, doc, pf (pointfree)
and pF (pointful) commands for ghci.

The .tmux.conf file adds some moving commands: M-h, M-j, M-k, M-l work directly.



## Some useful nix-commands

### Update
```
nix-channel --update nixos
nixos-rebuild test
nixos-rebuild switch
```

### Garbage collection 

Unstable and occasional building & installing eats disk capacity. 
```
nix-collect-garbage --delete-older-than 15d
```

### Package management

To get package definitions:
```
git clone git://github.com/NixOS/nixpkgs.git
```

To show user-installed packages:
```
nix-env -q
```

To remove package:
```
nix-env -e packagename
```

To install package:
```
nix-env -f "<nixpkgs>" -iA haskellPackages.packagename
nix-env -iA nixos.oh-my-zsh
```
and if you are going to use zsh with oh-my-zsh, then maybe
```
cp -v $(nix-env -q --out-path oh-my-zsh | cut -d' ' -f3)/share/oh-my-zsh/templates/zshrc.zsh-template ~/.zshrc
```

To list Haskell-packages:
```
nix-env -f "<nixpkgs>" -qaP -A haskellPackages
```

To upgrade packages:
```
nix-env -u
```

To build and use a dev env (assuming the environment defined as shown above):
```
nix-build "<nixpkgs>" -A ghcEnv
nix-shell -p ghcEnv
```

## Nix, reflex-libs, reflex based dev

### How to organize nix-files for fluent work-flow?

Git clone [reflex-platform](https://github.com/reflex-frp/reflex-platform)
and take into account the installation instruction for Nixos so that 
pre-compiled reflex-packages and ghcjs is used in the installation.

Then, to organize nix-files for nix-build and nix-shell, it would be convenient,
if it is possible to

  1) use the work-on -script provided at reflex-platform
  2) nix-build with ghcjs as default compiler
  3) nix-shell with ghc as default compiler
  4) nix-shell with ghcjs 

If the lib has default.nix, shell.nix and libname.nix, as shown, the the above 
items work with one minor irritation. The following commands work
```
nix-shell
nix-build
path-to-reflex-platform/work-on ghc ./libname.nix
```
while
```
path-to-reflex-platform/work-on ghcjs ./libname.nix
path-to-reflex-platform/work-on ghcjs ./libname.nix --argstr "compiler" "ghcjs" --argstr "ghcjs-base" "ghcjs-base"
```
didn't. Note that
```
nix-shell --argstr "compiler" "ghcjs" 
```
does work, this is the item 4) above. In this case, in nix-shell, it is 
possible to
```
cabal configure --ghcjs
cabal build
```
and build artefacts can be found somewhere from dist-directory.

All this is achieved by using a few default parameters at default.nix, 
shell.nix and libname.nix. Compiler default is ghc in shell.nix and libname.nix
and ghcjs in default.nix. This way 2) and 3) will get going and this is
the reason we have to override nix-shell compiler arg if we want to use ghcjs 
there as in case 4). 

Note that shell.nix calls default.nix, which in turn, contains some programs
so that compiling and other work is possible to do (with both ghc and ghcjs).

When using 1) with ghc, we have to provide the libs that libname uses (depends 
on) and that are also in cabal-file. (This is the work-on cmd above.)
Dependencies are given in the libname.nix file, which is almost a direct 
output of cabal2nix-command. It has been modified so that it works with both 
ghc and ghcjs. 

The former (ghc) uses jsaddle-wai and can be used with ghcid to give quick 
feedback when doing modifications. (Or it can be used to webkitgtk target.) The 
latter (ghcjs) is used to build js-file that is either in result-directory 
(nix-build) or in dist-directory. See the libname.nix file.

Further, note that 1) gives fluent and quick 
dev cycle, 3) helps the editor while 4) is semi-quick and 
no editor goodies (yet). See ghcid-section below.

To see more polished setup, check
[Queensland FP lab intro to functional reactive programming](https://blog.qfpl.io/projects/reflex/). 
The exercises contain good nix-file organization. And if the rfp and reflex is 
new to you, the blog is super great introduction to the topic.



### Ghcid turbo-charging for reflex-dev

Ghcid can be turbo-charged with appropriate 
[.ghci-file](https://github.com/gspia/half-baked/blob/master/hb8-nixos-adventure/.ghci), 
a short 
[shell script](https://github.com/gspia/half-baked/blob/master/hb8-nixos-adventure/dev-server.sh)
and with few lines of jsaddle-wai related 
[code](https://github.com/gspia/half-baked/blob/master/hb8-nixos-adventure/jswai.hs) 
so that it is possible to see the changes directly in the web browser that is 
used to test/see the results. If you put the 
[index2.html](https://github.com/gspia/half-baked/blob/master/hb8-nixos-adventure/index2.html) 
into the static-directory, you can: 

 1) Use work-on ghc, that is, case 1) above, and then start ghcid, that is, 
    dev-server.sh, in one terminal.
 2) Start nix-shell, that is, case 3) above, and then start your editor. If
    using vim with the above setup, you have hlint, hdevtools, ghcmod etc 
    running with short-cut keys. E.g. ,hl applies hlint for the file active
    buffer.
 3) Start you browser at http://localhost:8000/index2.html
 4) Edit code and see the results on the fly on browser.

In the index2.html we can put other css- and js-libs etc. as required.

Why all this hassle with ghcid (and nix-shell --argstr)? Since nix-build, the 
case 2) in the above section, is somewhat slow. In the end nix-build will be 
used, anyway. Further note that if you try to use ghcid in nix-shell as
given in case 3) in the above section, the ghcid is from reflex-platform
while the tools are not. This means that some of the libraries are incompatible 
versions and simply don't work (libs used by rfp-ghcid vs. other tools). 
This also means that ghcmod doesn't work very well here. (When doing
other development, the vim-goodies are ok.)


This is from [a gist](https://gist.github.com/3noch/ee335c94b92ea01b7fee9e6291e833be)
by 3noch.



## Links

 - [Guide to Haskell infra](https://nixos.org/nixpkgs/manual/#users-guide-to-the-haskell-infrastructure)
 - [nixos-haskell](https://github.com/nixos-haskell/getting-started)

Blogs and other useful links

 - [fluffynukeit on installation](http://fluffynukeit.com/series/haskell-nixos/)
 - [dmjio intro on nix](https://github.com/dmjio/nixintro/blob/master/nixtalk.org)
 - [example project](https://www.reddit.com/r/haskell/comments/5y4uot/example_projecttutorial_nixbased_multimodule/)
 - [Gabriels haskell-nix](https://github.com/Gabriel439/haskell-nix)



## TODOs

- postgresql & nixos
- hoogle and haddock with some of the working options shown above should be 
  tried (don't know it they work or how they should be used atm)
- some of the vim-shortcut keys seem to not work, why? Env-depending thing?
- nixops, disnix, hydra?
- and many others...

