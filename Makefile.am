SOURCES =                     \
  automatic.scm               \
  automatic/utils.scm         \
  automatic/proposition.scm   \
  automatic/diagram.scm       \
  automatic/presburger.scm    \
  automatic/dfa.scm           \
  automatic/afa.scm           \
  automatic/graph.scm

TESTS =                       \
  tests/automatic.scm         \
  tests/utils.scm             \
  tests/proposition.scm       \
  tests/diagram.scm           \
  tests/presburger.scm        \
  tests/dfa.scm               \
  tests/afa.scm               \
  tests/graph.scm

TEST_EXTENSIONS = .scm

AM_TESTS_ENVIRONMENT = RESOURCES_DIR='$(srcdir)/resources/'; export RESOURCES_DIR;

SCM_LOG_COMPILER =                 \
  $(top_builddir)/pre-inst-env     \
  $(GUILE) --no-auto-compile

EXTRA_DIST =            \
  README.rst            \
  bootstrap             \
  pre-inst-env.in

moddir=$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

GOBJECTS = $(SOURCES:%.scm=%.go)

nobase_dist_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.

guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_dist_modDATA

CLEANFILES = $(GOBJECTS)
GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILD) compile $(GUILE_WARNINGS) -o "$@" "$<"
