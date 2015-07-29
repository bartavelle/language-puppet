# v1.1.4 ()

## New features
* The `regsubst` function now works with arrays.
* The `file` variable is resolved in templates.

# v1.1.3 (2015/05/31)

## New features
* Support for the `$settings` variables.
* Support for the `to_yaml` function in templates.
* Settings can now be altered in the default YAML file.
* Defaults and overriden facts are now controlled in the YAML file too.

# v1.1.1.2 (2015/04/28)

Various packaging changes.

# v1.1.1 (2015/04/20)

## New features
* Add 'notify' native type
* Ability to provide defaults via a yaml file for some options
* Added the 'ensure_packages' and 'ensure_resources' functions

## Bugs fixed
* Enable 'package' native type (issue #102)
* Builds against GHC 7.10

## Changes
* Even in Permissive mode, don't resolve unknown variable (see #103)
* Add priority to the logger permissive output (see #106)
* New hruby version
* Rename option `--ignoremodules` into `--ignoredmodules`

## Various
* Hiera config interpolation logs decrease from NOTICE to INFO

# v1.1.0 (2015/03/11)

Critical bugs have been fixed, upgrade recommended.

## New features
* New `dumpinfos` debug function.
* The interpreter can now run in a strict or permissive mode.
* The new `-a` option accepts a comma separated list of nodes for gathering stats.
* The new `--noextratests` option disable optional tests from `Puppet.OptionalTests`.
* Implementation of `member()` from stdlib (see issue #100 for details)

## Bugs fixed
* Exported/virtual custom types are not expanded. This is a huge bug.
* Class/define parameters that are explicitely set as undefined are now overriden by
  default values.
* Empty resource groups are now rejected.
* An existing resource can now be realized.

## Various
* Hiera config interpolation logs decrease from WARN to NOTICE
* Remove option `--nousergrouptest`
* Ease the use of the puppetresources command options. See the README file for changes.


# v1.0.1 (2014/11/13)
## New features
* Support for the `join` function.
* Support for filtering json puppetresources output (fix issue #64)
* Support for `cmpversion` in the templates.
* The various chaining modes have been implemented.
* Support for the `is_bool` function (Pierre Radermecker)
* Support for `concat` and `concat::fragment` (Pierre Radermecker)

## Bugs fixed
* Fix array value extrapolation in string (issue #35)
* ${var} without quotes will now be rejected by the parser (issue #78)

## Various
* `README` moved to asciidoc (Pierre Radermecker)

# v1.0.0 (2014/08/31)
## IMPORTANT
Building without hruby is now unsupported.
## New features
* Support for Debian distribution detection in facter.
* Support for the "~>" operator.
* Support for mixed-case resource references.
* Added the `grep` function.
## Bugs fixed
* Better support for --ignoremodules.
* Fixed parsing of standalone `$` characters in strings.

# v0.14.0 (2014/06/12)
## New features
* Overhauled the dependency check system
* Added an option to skip the user and group checks
* Added an option to ignore some modules
## Bugs fixed
* Added `vagrant`, `nagios`, `www-data`, `postgres` and `nginx` to the list of known users.
* Fixed how resource relationships were resolved with notify and before.
* Fixed a problem where inheritance whould be used with `::` prefix.
* The `defined` function now works with classes.
* All numbers are now strings in templates.

# v0.13.0 (2014/05/21)
## New features
* Hacky support for `scope.get_hash`.
* New stuff from the new parser (adding hashes, arrays, etc.).
* Wrote a pure evaluation function, for unit tests and prisms.
* `Num` and `Fractional` instances for `Expression`.
* Numbers are now internally stored as numbers, just like the new parser does.
* Add support for "structured facts".
* New stdlib functions: `is_hash`, `has_key`, `size`, `values`.
## Bugs fixed
* Puppetresources does not fail tests for file sources starting with `file://`.
* Escaped characters were not properly handled in the parser.
* Properly catch division by 0 (!!!!).
* Got rid of the orphan instances ... code is now a lot uglier.
* Fixed settings of "title" and "name" in classes. The original puppet version
  only seems to do this when we declare in define style :(
* Fixed associativity priority between `=~` and `and`.

# v0.12.3 (2014/03/13)
## New featues
* puppetresources now tests that groups and users are defined before being used
 in file, user, cron and exec.

# v0.12.2 (2014/02/18)
## New featues
* Facts are now dumped in `TestDB` format by `pdbquery`.
* The `puppetresources` command now has switch controlling the PuppetDB commit and "catalog update".

# v0.12.1 (2014/02/10)
## New featues
* *Dead code* finder in puppetresources.
* CPU related facts.
* `puppetresources` now exits with the proper error code.
* `puppetresources` can now display some statistics about compilation times.
* Bumped the version of the http-conduit dependency.
## Bugs fixed
* Fixed dependencies so that builds with GHC 7.8-rc1 work.

# v0.12.0 (2014/02/07)
## New featues
* Builds against GHC 7.8-rc1.

# v0.11.1 (2014/01/31)
## Bugs fixed
* Fixed build issues with strict-base-types version.

# v0.11.0 (2014/01/30)
## New features
* Removal of the dedicated parsing threads.
* Better default RTS options (for now, just the default allocation size)
* Upgraded dependencies : aeson 0.7, attoparsec 0.11, lens 4, parsers
	  0.10, text 1.1, filecache 0.3, hruby 0.2

# v0.10.6 (2014/01/25)
## New features
* New all nodes testing for puppetresources.
* Added some uname related facts.
* Added some lenses and prisms.
## Bugs fixed
* Parsing function calls without parens at the expression level is not
	allowed now.
* Allow parsing of boolean facts from YAML files.
* Allow resource references with array variables.
* Fix spurious multiple includes error.
* Fixed the implementation of some puppet functions.

# v0.10.5 (2014/01/06)
## Bugs fixed
* Lambda blocks can now end with a bare function call
* Fix version bounds with hslua and luautils

# v0.10.4 (2013/12/18)
## New features
* Moved to the latest hruby version.
* Updated the text bound

# v0.10.3 (2013/12/03)
## New features
* The scope tracking system has been improved. It is now possible to know
	the original host of an imported resource, which helps a lot in case of
	resource clashes
* is_virtual fact
* new stdlib functions: flatten, str2bool, validate_absolute_path
* Hiera support
* JSON output that is compatible with "puppet apply"
* New addfacts command for the pdbquery utility
* Support for the classes variable in templates
* Support for @instance variables in inline_template
* Support for scope['key'] syntax in templates
* Support for facts overriding with puppetresources
## Bugs fixed
* Deserialization problems with puppetDBs
* Fixed several bugs with imported resources
* Bug with relationships overrides that got stored as parameters
* Importing several exported resources from the same class now works
* Templates with an invalid encoding could crash the process
* Yaml parse errors of the puppetdb file now throw errors

# v0.10.2 (2013/10/27)
## Bugs fixed
* PVP support
## New features
* Support for properly setting instance variables before computing
	templates with native Ruby.

# v0.10.1 (2013/10/27)
## Bugs fixed
* The TestDB file was never created.

# v0.10.0 (2013/10/27)
## New features
* Map/each/filter functions with lambdas (not really tested)
* Rewrite of the PuppetDB API
* The whole scope stack is kept with each Resource, for easier debugging
* Inclusion of three PuppetDB backends : dummy (no effect), TestDB (stored
	in a YAML file) and Remote (standard PuppetDB API)
## Bugs fixed
* This is a hack : variables declared in a parent (inheritance) can now be
	overriden. This is because inheritance is not handled like in Vanilla. As
	I do not really use inheritance, I am not sure if this is much of a
	breakage.

# v0.9.0 (2013/08/15)
  Huge rewrite !
* See http://lpuppet.banquise.net/blog/2013/08/15/version-0-dot-9-0-is-out-for-testing/

# v0.4.2 (2013/06/01)
## New features
* Functions 'values' and 'keys' from stdlib are now implemented.
* hruby integration
## Bugs fixed

# v0.4.0 (2013/05/16)
## New features
* Big refactor of the PuppetDB API.
* New "fake" PuppetDB used for testing
* Support of the caller_module_name variable.
* Support for a dumpvariable() function.
* More details stored in the resource types, and in error messages.
* User native type
* Removal of the MissingH, filepath, monad-loop and directory dependency
* Puppet booleans are now handled at parse stage
* inline_template function
## Bugs fixed
* fqdn_rand now puppet perfect (at least for 32 bit max values)
* Now depends on the built-in bytestring library that comes with
	GHC-7.6.1.
* Aliases should now work as expected ... I wish!
* regexp_subs now works in a PCRE manner
* Destination dependency can now be a variable resolving in an array.

# v0.3.3 (2013/01/21)
## New features
* Tries to find calcerb.rb next to the executable.
* Started cleaning imports ...
* It is now possible to write "top level" functions in lua.
* Function getvar (stdlib)
* TENTATIVE support for aliases.
* Checks that file names don't have trailing slashes.
* Checks that exec commands are fully qualified if path is not defined.
* New native type : package.
## Bugs fixed
* Fixed a ton of problems related to exported resources and relations.
* Minor fix about zonerecord.
* Resolving variable names starting with :: in templates
* Fixed the file function.

# v0.3.2 (2012/12/13)
  The license has been changed to BSD3.
## Bugs fixed
* It is now possible to use expressions in include blocks. This is
	temporary, as include should be handled just like every other function.

# v0.3.1 (2012/11/23)
## New features
* Yes, we can generate JSon catalogs now.
## Bugs fixed
* Several bugs about resource relationships.

# v0.3.0 (2012/11/19)
## New features
* Resource relationships are somehow supported. The API is broken as a
	result.
* Exported resources are now returned.

# v0.2.2 (2012/11/12)
## New features
* A few statistics are exported.

# v0.2.1 (2012/11/12)
## Bugs fixed
* The defaults system was pretty much broken, it should be better now.
## New features
* Basic testing framework started.
* create_resources now supports the defaults system.
* defined() function works for resource references.
* in operator implemented for hashes.
* Multithreading works.
* The ruby <> daemon communication is now over ByteStrings.
* The toRuby function has been optimized, doubling the overall speed for
	rendering complex catalogs.
* Various internal changes.

# v0.2.0 (2012/10/08)
## New features
* Lua integration for custom functions.
* Automatically creates magic types based on the content of the modules.
## Bugs fixed
* Defaults parameters can now end with a comma.

# v0.1.8.0 (2012/09/20)
## New features
* Refactoring of the PuppetDB API for interfacing with the facter library.
* Support of exported resource resolution through PuppetDB ! This results
	in an API breakage.
* Make binary distribution possible (ruby helper path).
## Bugs fixed
* Defines with spurious parameters, or unset mandatory parameters, should
	now be catched.
* Exception handling for the HTTP failures.
* Handles undefined variables in Ruby templates.
* Undefined variables in Erbs now always throw exceptions. This is
	stricter than Puppet (which throws exceptions for "native" variables), but
	is I believe good practice.

# v0.1.7.2 (2012/09/17)
## New features
* Preliminary support for PuppetDB

# v0.1.7.1 (2012/09/14)
## Bugs fixed
* Various details have been modified since the official language
	  documentation has been published.
* Better handling of collector conditions.
* Solves bug with interpolable strings that are not resolved when first
	  found.
## New features
* Amending attributes with a collector.
* Stdlib functions : chomp
* Resource pretty printer now aligns =>.
* Case statements with regexps.

# v0.1.7 (2012/08/24)
## Bugs fixed
* Fix bug with '<' in the Erb parser !
* Assignments can now be any valid Puppet expression.
* Proper list of metaparameters.
## New features
* Quick resolution of boolean conditions.
* Start of the move to a real PCRE library.
* Function is_domain_name.
* New native types : zone_record, cron, exec, group, host, mount, file.

# v0.1.6 (2012/08/01)
## New features
* Errors now print a stack trace (only works with profiling builds).
* Nested classes.
* generate() function.
* defines with spurious top level statements now should work.
* validate_* functions from puppetlabs/stdlib.
## Bugs fixed
* Metaparameters now include stages (not handled).
* Resolving non empty arrays as boolean returns true.
* Duplicate parameters are now detected.

# v0.1.5 (2012/07/06)
## Bugs fixed
* Detection of spurious parameters when declaring parametrized classes now
	works.
* Resource overrides with non trivial names should now work.
* Require statements in required files would not be loaded.

# v0.1.4 (2012/07/02)
## New features
* Basic native template function.
* Added anchor as a native type for now. A better fix will be to just parse
	  for defined types in the lib directory of modules.
* Tentative defined() implementation. Will not work for resource references.
* Functions md5, sha1, lowcase, upcase, split.
## Bugs fixed
* String comparison is not case insensitive.
* Variable scope for inherited classes should now work.
* Support for the $module_name variable (probably a bit buggy).
* Proper location of a "define not found" error.
* Parsing bug for single quoted strings and slashes.
* Bug where a resource name is a variable that is actually an array.
* Array indexing.
* Top level variables are now supported in Erb.
## Various
* Removed the title parameter from the catalog printing functions.
* Used hslint a bit.
