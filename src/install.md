\title Install

# Quick installation

Before installing Ciao, please check the
[**requirements**](/ciao/build/doc/ciao.html/Install.html#Installing%20dependencies):

 - A computing device running Linux, macOS,
   Windows (with [WSL](https://docs.microsoft.com/en-us/windows/wsl/install-win10)),
   or Android (with [Termux](https://termux.com)).
 - A modern C compiler (`gcc` or `clang`).
 - Optionally, [Emacs](https://www.gnu.org/software/emacs/) (required for the
   *development environment*).  Graphical Emacs on WSL requires an
   X-server (e.g.,
   [vcxsrv](https://sourceforge.net/projects/vcxsrv/)).

To begin the interactive **installation** type the following
*one-liner* in an `sh`-compatible terminal:
```
curl https://ciao-lang.org/boot -sSfL | sh
```

Please report as [issues](https://github.com/ciao-lang/ciao/issues)
any problems found during the installation process.

You can explore additional components at the **[bundle catalog](/bundles.html)**
(including links to documentation and source code
repositories) and install them with the `ciao get BUNDLENAME` command.

# Manual installation

The detailed [**installation instructions**](/ciao/build/doc/ciao.html/Install.html)
in the Ciao manual contain more information about
non-interactive and manual installation. In a nutshell, it consists in 
cloning the [Ciao repository](https://github.com/ciao-lang/ciao):
```
git clone https://github.com/ciao-lang/ciao
```
and then executing `./ciao-boot.sh local-install` to perform a core
installation, or `./ciao-boot.sh get devenv` to install the full
environment ([devenv](https://github.com/ciao-lang/devenv) bundle).

**Notes for Windows users**: Native builds for Windows are
experimental, supported via [MSYS2](http://www.msys2.org/). Please
contact us if you need this.

# Uninstalling Ciao

If installed using a *one-liner*, the system must be removed from
the default installation directory at `~/.ciaoroot/master`. E.g.,
using a script like:

```sh
( cd ~/.ciaoroot/master; ./ciao-boot.sh uninstall )
rm -rf ~/.ciaoroot/master
rmdir ~/.ciaoroot > /dev/null 2>&1 || true
```

Bundles installed via `ciao get` must be uninstalled and removed
explicitly. Currently, this needs to be done manually per
bundle. E.g., uninstalling the **development environment** (if
installed) requires:

```
ciao uninstall ciao_emacs; ciao rm ciao_emacs; ciao rm devenv
```

Once all bundles have been removed, the core Ciao system can be
uninstalled running `./ciao-boot.sh uninstall` from the source
directory (then, remove the directory).

