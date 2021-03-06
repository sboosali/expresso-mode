#------------------------------------------------#
# README ----------------------------------------#
#------------------------------------------------#

# Targets:
#
# • Standard targets — « make {build,check,dist,install} ».
# • Release targets  — « make {upload,publish}-{melpa,github} ».
#

#------------------------------------------------#
# Makefile Settings -----------------------------#
#------------------------------------------------#

SHELL=bash

.EXPORT_ALL_VARIABLES:

#------------------------------------------------#
# Makefile Variables: Metadata ------------------#
#------------------------------------------------#

Version=0.0.0
Feature=expresso-mode

#------------------------------------------------#

CaskDirectory?=./

#------------------------------------------------#
# Makefile Variables: Paths ---------------------#
#------------------------------------------------#

BuildDirectory ?=./.cask
DataDirectory  ?=./data
ShareDirectory ?=./gitignored/share

#------------------------------------------------#

#------------------------------------------------#
# Makefile Variables: Programs ------------------#
#------------------------------------------------#

Cask  ?=cask
Emacs ?=emacs

#------------------------------------------------#

Open ?=xdg-open
Nix  ?=nix

#------------------------------------------------#
# Makefile Variables: Paths ---------------------#
#------------------------------------------------#

BuildDirectory   ?=./dist-newstyle
DataDirectory    ?=./data

#------------------------------------------------#

ShareDirectory      ?=./gitignored/share
HaddockDirectory    ?=$(ShareDirectory)/doc
CompletionDirectory ?=$(ShareDirectory)/bash/completions

#------------------------------------------------#
# Makefile Variables: Options -------------------#
#------------------------------------------------#

EmacsOptions ?=--no-init-file --maximized --no-splash --no-desktop

#------------------------------------------------#

CaskOptions=--path $(CaskDirectory) --builddir $(BuildDirectory)

#------------------------------------------------#

MelpaUser     =sboo
MelpaPassword =pass melpa.org/user/$(MelpaUser)

#------------------------------------------------#

GitHubOwner      =sboosali
GitHubRepository =$(Project)

#------------------------------------------------#
# Makefile Variables: Subcommands ---------------#
#------------------------------------------------#

CaskBuild   ?=$(Cask) build $(CaskOptions)
CaskExec    ?=$(Cask) exec $(CaskOptions)
CaskRun     ?=$(Cask) emacs $(CaskOptions)
# CaskTest    ?=$(Cask) test --enable-tests $(CaskOptions)

#------------------------------------------------#

EmacsBuild ?=emacs  -batch  --funcall=batch-byte-compile
EmacsStart ?=emacs "--name=Emacs - SBoo/$(Timestamp)" --maximized --no-splash --no-desktop

#------------------------------------------------#
# Makefile Variables: Environment Variables -----#
#------------------------------------------------#

LC_ALL=C.UTF-8

#------------------------------------------------#
# Makefile Targets: Standard --------------------#
#------------------------------------------------#

build:
	$(CaskBuild)

.PHONY: build

;; “Byte compile all package files. The .elc files are placed in the same directory as the source file.”

#------------------------------------------------#

check:
	$(CaskTest)

.PHONY: check

#------------------------------------------------#

clean:
	$(Cask) clean-elc

.PHONY: clean

#------------------------------------------------#
# Makefile Targets: Custom ----------------------#
#------------------------------------------------#

emacs27:

	EMACS="emacs27.1"

.PHONY: emacs27


#------------------------------------------------#

emacs26:

	EMACS="emacs26.1"

.PHONY: emacs26

#------------------------------------------------#

emacs25:

	EMACS="emacs25.3"

.PHONY: emacs25

#------------------------------------------------#
# Makefile Targets: Custom ----------------------#
#------------------------------------------------#

print:

	$(Cask) info
	$(Cask) files

#------------------------------------------------#
# EOF -------------------------------------------#
#------------------------------------------------#