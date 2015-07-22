# projectable.el #

A lightweight project framework I created for the main purpose of integration with my `JS-DEPENDENCY-INJECTOR` package, which requires associative lists of files with project ids and their locations.

# Installation #

Clone this repository and add it to Emac's `load-path`. Then require the file:
```
(add-to-list 'load-path "/path/to/cloned/repo")
(require 'projectable)
```
Then enable it globally to load the key mappings
```
(projectable-global-mode)
```

# Defining a project #

This package *(currently only)* defines projects using JSON files.
It defaults to looking in `projectable-project-directory` first.

If you load a *directory*, instead of a JSON file, it will just cache all of the files recursively within that directory.

An example JSON project file would look like this:
```JSON
{
	"projectId":"My Project",
	"project":[
		{ "dir":"~/path/to/project" },
		{ "dir":"~/path/to/another/project" }
	],
	"testing":{
		"path": "tests/specs",
		"extension": "Spec",
		"sourcePath": "src"          //Optional if files don't have an explicit source
	},
	"tabs":false,
	"indent":2,
}
```
Where the two properties in the `project` array define paths required in that project. This is useful if you have multiple respositories for a single project.

The `testing` block defines how to find your tests, *e.g.* they are found in `tests/specs` and would look like `originalFileSpec.js`

The `tabs` property defines whether this projects uses **tabs** or **spaces**

# Usage #
If you enable `projectable-global-mode` you will have access to the following keybindings:

Key Binding | Command | Effect 
--- | --- | ---
`C-c p c` | `projectable-change` | Change to a new project file/directory 
`C-c p r` | `projectable-refresh` | Refresh the list of files cached 
`C-c p f` | `projectable-ido-find-file` | Open a file from the cache 
`C-c p t` | `projectable-toggle-open-test` | Try and find the related test to the current class

# Planned Features #
I'm trying to get this project in a nicer state for my own purposes, future features will include
- [x] `.gitignore` integration for filtering
- [x] Better integration for test
    - [x] opening
    - [ ] running
- [ ] Hosting on [Melpa](http://melpa.org/#/)
- [ ] Ability to define projects as elisp objects _e.g._ `(:project "My Project" :dirs '("path/one" "path/two") ...)`
- [ ] Etags/Ctags integration


#### Why not Projectile ####
I'm aware that [projectile](https://github.com/bbatsov/projectile) does a lot of this already and I would actively encourage you to try that package out as well!

The _main reason_ I wrote my own version was that I work on projects comprised of multiple directories that use [RequireJS 0.15.0](https://libraries.io/bower/rjs/0.15.0) and I was fed up of _manually_ typing the require paths for modules across multiple projects. So I wrote my own _JS Dependency Injector_ which uses an associative list of project ids _(the ones used in the require js mapping definition)_ to a list of files with their containing directories. This way I could build the require paths for Classes I wanted to include.

I made a plugin for [Sublime Text](http://www.sublimetext.com/) which does a similar thing, see [JSDependencyInjector.py](https://github.com/domtronn/jsdependencyinjector) for a better example of what I mean.
