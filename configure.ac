AC_INIT([automatic-fol-prover], [0.1])
AC_CONFIG_SRCDIR([automatic.scm])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

GUILE_PKG([2.2 2.0])
GUILE_PROGS
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your guile-2.x installation.])
   fi

GUILE_MODULE_REQUIRED([cairo])

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

AC_OUTPUT
