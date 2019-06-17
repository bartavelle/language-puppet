![language puppet](https://img.shields.io/hackage/v/language-puppet.svg)
![lts](https://www.stackage.org/package/language-puppet/badge/lts)
![nightly](https://www.stackage.org/package/language-puppet/badge/nightly)
![Build Status](https://travis-ci.org/bartavelle/language-puppet.svg?branch=master)
![cachix cachix orange](https://img.shields.io/badge/cachix-cachix-orange.svg)

A library to work with Puppet manifests, test them and eventually replace everything ruby.

# Install

**Install with stack:**
```
stack install language-puppet
```

**Install with nix:**
```
nix-env -i -f https://github.com/bartavelle/language-puppet/tarball/v1.4.5
```
(replace `1.4.3` with any commit ref or tag).

**Build from sources:**

```
git clone https://github.com/bartavelle/language-puppet.git
cd language-puppet
# Using nix
nix build
# Using stack
ln -s stack-10.yaml stack.yaml
stack build
```

# Puppetresources

**Basic usage**

    puppetresources --puppetdir /where/your/puppet/files/are --node node.name.com

The `puppetresources` command is a command line utility that let you
interactively compute catalogs on your local computer. It is much faster
than its ruby counterpart, and has been designed for giving assistance
to the Puppet catalog writer.

There are 4 different modes:

  - `--node` will display all resources on screen in a nice
    user-friendly colored fashion.

  - `--all` displays statitics and optionally shows dead code.

  - `--parse` only goes as far as parsing. No interpretation.

  - `--showcontent` to display file content.

Catalog is not computed exactly the same way Puppet does. Some good
practices are enforced. A strict and more permissive mode are provided.

**Command line arguments**

  - `-p` or `--puppetdir`
    This argument is mandatory except in `parse` mode. It must point to
    the base of the puppet directory (the directory that contains the
    `modules` and `manifests` directories).

  - `-o` or `--node`
    Enable the `node mode`. This let you specify the name of the node
    you wish to compute the catalog for.

  - `-a` or `--all`
    Enable the `stats mode`. If you specify `allnodes` it will compute
    the catalogs for all nodes that are specified in `site.pp` (this
    will not work for regexp-specified or the default nodes). You can
    also specify a list of nodes separated by a comma.

    Combined with `--deadcode`, it will display the list of puppet files
    that have not been used.

    This is useful as automated tests, to check a change didn’t break
    something. You might want to run this option with `+RTS -N`.

  - `-t` or `--type`
    Filters the resources of the resulting catalog by type. Using PCRE
    regex is supported.

  - `-n` or `--name`
    Filters the resources of the resulting catalog by name. Using PCRE
    regex is supported.

  - `-c` or `--showcontent`
    If `-n` is the exact name of a file type resource defined in the
    catalog, this will display the file content nicely. Useful for
    debugging templates.

    Example: `puppetresources -p . -o mynodename -n '/etc/motd'
    --showcontent`

  - `--loglevel` or `-v`
    Possible values are : DEBUG, INFO, NOTICE, WARNING, ERROR

  - `--pdburl`
    Expects the url of a live PuppetDB.

  - `--pdbfile`
    Expects a path to a **fake** PuppetDB, represented as a YAML file on
    disk. This option is pretty slow but can be invaluable to test
    exported resources tricks.

  - `--hiera`
    Expects the path to the `hiera.yaml` file.

  - `--ignoredmodules`
    Expects a list of comma-separated modules. The interpreter will not
    try to parse and evaluate the defined types and classes from this
    module. This is useful for using modules that use bad practices
    forbidden by `puppetresources`.

  - `--commitdb`
    When this flag is set, exported resources, catalogs and facts are
    saved in the PuppetDB. This is useful in conjunction with
    `--pdbfile`.

  - `--checkExported`
    When this flag is set, exported resources are saved in the PuppetDB.
    This is useful in conjunction with `--pdbfile`.

  - `-j` or `--JSON`
    Displays the catalog as a Puppet-compatible JSON file, that can then
    be used with `puppet apply`.

  - `--strict`
    Enable strict check. Strict is less permissive than vanilla Puppet.
    It is meant to prevent some pitfalls by enforcing good practices.
    For instance it refuses to

      - silently ignore/convert `undef` variables

      - lookup an hash with an unknown key and return `undef`.

  - `--noextratests`
    Disable the extra tests from `Puppet.OptionalTests`.

  - `--parse`
    Enable `parse mode`. Specify the puppet file to be parsed. Variables
    are not resolved. No interpretation.

  - `--version`
    Output version information and exist.

**Settings defaults using a yaml file**

Defaults for some of these options can be set using a
`/yourworkingdirectory/tests/defaults.yaml` file. For instance
`OptionalTests` is checking that all users and groups are known. Because
some of these users and groups might be defined outside puppet, a list
of known ones is used internally. This can be overridden in that file
using the key `knownusers` and `knowngroups`.

Please look at [the template
file](https://github.com/bartavelle/language-puppet/blob/master/tests/defaults.yaml)
for a list of possible defaults.

# pdbQuery

The `pdbquery` command will work with different implementations of
PuppetDB (the official one with its HTTP API, the file-based backend and
dummy ones). It can be used to:

  - export data from production PuppetDB to a file (in order to debug
    some issue with `puppetresources**).
  - query a Puppetdb

**Command line arguments**

  - `-l` or `--location`
    The URL of the PuppetDB when working with a remote PuppetDB, a file
    path when working with the file-based test implementation.

  - `-t` or `--pdbtype`
    The type of PuppetDB to work with:

      - dummy: a dummy PuppetDB.

      - remote: a "real" PuppetDB, accessed by its HTTP API.

      - test: a file-based backend emulating a PuppetDB.


  - `facts`
    Output facts for a specific node (json)

  - `nodes`
    Output all nodes (json)

  - `resources`
    Output all resources for a specific node (json)

  - `dumpfacts`
    Dump all facts to `/tmp/allfacts.yaml`.

  - `snapshot`
    Create a test DB from the current DB

  - `addfacts`
    Adds facts to the test DB for the given node name, if they are not
    already defined.

  - `--version`
    Output version information and exit.

# Supported features

  - Supported version
    puppet 4 is mostly supported. Please look at the list of issues for
    details.

  - Custom ruby functions
    The tool might bark when resolving custom ruby functions. These
    function can easily be mocked or implemented in Haskell if
    necessary.

  - Puppet functions

      - the `require` function is not supported (see [issue
        \#17](https://github.com/bartavelle/language-puppet/issues/17))

      - the deprecated `import` function is not supported

      - the deprecated node inheritance feature is not supported

  - OS
    Linux is the default OS. The tool has also been successfully
    installed and used on `OS X`. Windows is not supported.
