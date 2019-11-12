AC_INIT([YVDebug], [0.0.1], [spk121@yahoo.com], [yvdebug])

AC_CONFIG_AUX_DIR([build-aux])
AC_REQUIRE_AUX_FILE([test-driver.scm])
AM_INIT_AUTOMAKE

AC_PREREQ([2.63])
AC_CONFIG_MACRO_DIR([m4])

# from guile.m4
GUILE_PKG([2.2])
GUILE_PROGS
GUILE_SITE_DIR

################
# Guile has three directories that don't follow the GNU File System
# Heirarchy Standards.  If one follows the GNU FHS, files get installed
# in directories in which Guile does not search for them by default.
AC_ARG_WITH([gnu-filesystem-hierarchy],
            [AS_HELP_STRING([--with-gnu-filesystem-hierarchy],
                            [Strictly follow GNU File System Hierarchy when installing, instead of querying Guile to discover the install directories that it expects.])])

AS_CASE(["x$with_gnu_filesystem_hierarchy"], [xyes],
    [
    # Follow the GNU File System Hierarchy Standard correctly
    # Installed .scm scheme libraries go here
    AC_MSG_CHECKING([Guile site directory])
    guilesitedir="\${datadir}/guile/site/$guile_effective_version"
    AC_MSG_RESULT([$guilesitedir])
    AC_SUBST(guilesitedir)

    # Compiled libguile-*.so extension libraries go here
    AC_MSG_CHECKING([Guile extension directory])
    guileextensiondir="$libdir/guile/$guile_effective_version"
    AC_MSG_RESULT([$guileextensiondir])
    AC_SUBST(guileextensiondir)

    # Compiled .go scheme libraries go here
    AC_MSG_CHECKING([Guile object directory])
    guileobjectdir="$libdir/guile/$guile_effective_version/site-ccache"
    AC_MSG_RESULT([$guileobjectdir])
    AC_SUBST(guileobjectdir)],
    [
    # Get the installation directories from Guile
    # Installed .scm scheme libraries go here
    AC_MSG_CHECKING([Guile site directory])
    guilesitedir=$GUILE_SITE
    AC_MSG_RESULT([$guilesitedir])
    AC_SUBST(guilesitedir)

    # Compiled libguile-*.so extension libraries go here
    AC_MSG_CHECKING([Guile extension directory])
    guileextensiondir=$GUILE_EXTENSION
    AC_MSG_RESULT([$guileextensiondir])
    AC_SUBST(guileextensiondir)

    # Compiled .go scheme libraries go here
    AC_MSG_CHECKING([Guile object directory])
    guileobjectdir=$GUILE_SITE_CCACHE
    AC_MSG_RESULT([$guileobjectdir])
    AC_SUBST(guileobjectdir)])


AC_CONFIG_FILES([Makefile])

AC_OUTPUT
