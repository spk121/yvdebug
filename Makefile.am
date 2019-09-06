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
yvdebug.gresource: yvdebug.gresource.xml
	$(GLIB_COMPILE_RESOURCES) --target=$@ $<

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

dist_guilesite_DATA = \
 module/yvdebug.scm

dist_pkgguilesite_DATA = \
  module/yvdebug/terminal.scm \
  module/yvdebug/typelib.scm

guileobject_DATA = \
  $(dist_guilesite_DATA:%.scm=%.go)

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

EXTRA_DIST += src/yvdebug.in
CLEANFILES += src/yvdebug
