## Process this file with automake to produce Makefile.in.
##
##   Copyright (C) 2019 Michael Gran <spk121@yahoo.com>
##
##   This file is part of YVDebug.
##
##   YVDebug is free software: you can redistribute it and/or modify
##   it under the terms of the GNU General Public License as published
##   by the Free Software Foundation, either version 3 of the License,
##   or (at your option) any later version.
##
##   YVDebug is distributed in the hope that it will be useful, but
##   WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
##   General Public License for more details.
##
##   You should have received a copy of the GNU General Public License
##   along with Foobar.  If not, see <https://www.gnu.org/licenses/>.

ACLOCAL_AMFLAGS = -I m4 ${ACLOCAL_FLAGS}
AM_DISTCHECK_CONFIGURE_FLAGS = --with-gnu-filesystem-hierarchy

################################################################
# STUFF

EXTRA_DIST = \
 AUTHORS \
 NEWS \
 ChangeLog \
 LICENSE \
 INSTALL \
 build-aux/config.rpath

################################################################
# DATA

# Desktop file
applications_datadir = $(datadir)/applications
applications_data_DATA = data/com.lonelycactus.yvdebug.desktop

# Icons
apps_48x48_hicolor_icons_datadir = $(datadir)/icons/hicolor/48x48/apps
apps_48x48_hicolor_icons_data_DATA = data/lonelycactus-yvdebug.png
apps_scalable_hicolor_icons_datadir = $(datadir)/icons/hicolor/scalable/apps
apps_scalable_hicolor_icons_data_DATA = data/lonelycactus-yvdebug.svg

install-exec-hook:
	xdg-icon-resource forceupdate

uninstall-local:
	xdg-icon-resource forceupdate

# UI files
RESOURCE_FILES = data/mainwindow.ui data/headerbar.ui

data/yvdebug.gresource: data/yvdebug.gresource.xml $(RESOURCE_FILES)
	glib-compile-resources --sourcedir=data --target=$@ $<

# Package data
dist_pkgdata_DATA = data/yvdebug.gresource

EXTRA_DIST += \
  data/lonelycactus-yvdebug.png \
  data/lonelycactus-yvdebug.svg \
  data/yvdebug.gresource.xml \
  data/com.lonelycactus.yvdebug.desktop

CLEANFILES = \
  yvdebug.gresource

################################################################
# MODULE

pkgguilesitedir = $(guilesitedir)/yvdebug
pkgguileobjectdir = $(guileobjectdir)/yvdebug

dist_pkgguilesite_DATA = \
  module/yvdebug/errorlog.scm \
  module/yvdebug/main.scm \
  module/yvdebug/terminal.scm \
  module/yvdebug/typelib.scm \
  module/yvdebug/interpreter.scm

pkgguileobject_DATA = \
  $(dist_pkgguilesite_DATA:%.scm=%.go)

GUILEC_FLAGS = \
 -O2 \
 -Warity-mismatch \
 -Wformat \
 -Wmacro-use-before-definition \
 -Wunbound-variable \
 --load-path=$(abs_srcdir)/module \
 --load-path=$(abs_builddir)/module

.scm.go:
	GUILE_AUTO_COMPILE=0 \
	$(GUILE_TOOLS) compile $(GUILEC_FLAGS) \
	-o "$@" "$<"

CLEANFILES += \
  $(dist_guilesite_DATA:%.scm=%.go) \
  $(dist_pkgguilesite_DATA:%.scm=%.go)

################################################################
# SRC

dist_bin_SCRIPTS = src/yvdebug

src/yvdebug: src/yvdebug.in
	sed s^GUILE^$(GUILE)^ $< > $@
	chmod 755 $@

EXTRA_DIST += src/yvdebug.in
CLEANFILES += src/yvdebug
