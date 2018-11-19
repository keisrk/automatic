#!/usr/bin/env python
#
# This file runs all the configuration necessary to setup a given build.ninja
# file for an indicated architecture/OS (TODO: This still needs some work). At
# the moment we only build for Linux systems, 64-bit x86 systems and ARM.
#

""" Script that generates the build.ninja for the project.
"""

from __future__ import print_function

import os
import string
import sys
import platform
from optparse import OptionParser
import ninja_syntax
from distutils.spawn import find_executable

BUILD_FILENAME = 'build.ninja'
SOURCEDIR = os.path.dirname(os.path.realpath(__file__))

CWD = os.getcwd()
ROOT = '.' if SOURCEDIR == CWD else CWD
BUILD_DIR = '/'.join([ROOT, '_build'])


DEFAULT = 'all'
OCAMLBUILD_DESC = """OCamlBuild takes input ${in} and outputs ${out}. `_tag` file must be present. """

OCB = 'ocamlbuild'
OCB_FLAGS = '-tag bin_annot'
CLN_FLAGS = '-clean'

# Sanitize the environment

executables = [
    # command names
    'ninja',
    'opam',
    'ocamlbuild',
    'minisat',
]

def sanitize():
    """Check toolchains are available."""
    for exe in executables:
        if find_executable(exe) is None:
            raise Exception(
                ' '.join(['Cannot find:', exe, 'in the current $PATH.']))
        else:
            print('found: ' + exe)

variables = [
    # name, value
    # value is either string or string list
    # string list rendered as space separated string.
    ('root', ROOT),
    ('ocb', [OCB, OCB_FLAGS]),
    ('ocbclean', [OCB, CLN_FLAGS]),
]

rules = [
    # name, command, description
    # The tuple above suffices unless you build C/C++.
    ('clean', '${ocbclean}',    'OCamlBuild clean command'),
    ('ocaml', '${ocb} ${out}',  OCAMLBUILD_DESC),
]

ocaml_sources = [
    # Sources to compile
    'src/main.ml',
    'test/test.ml'
]
ocaml_binaries = [
    src.replace('.ml', '.byte')
    for src in ocaml_sources
]
builds = [
    {'outputs': bin, 'rule': 'ocaml'}
    for bin in ocaml_binaries
] + [
    # outputs, rule, inputs
    # Cullently we ignore the following configurations:
    # implicit(| imp1 imp2), order_only(|| oo1 oo2), variables=None,
    # implicit_outputs(out1 out2 :)
    {'outputs': 'all',   'rule': 'phony', 'inputs': ocaml_binaries},
    {'outputs': 'clean', 'rule': 'clean'},
]

if __name__ == "__main__":
    # Sanitize the environment
    sanitize()

    with open(BUILD_FILENAME, 'w') as bf:
        ninja_writer = ninja_syntax.Writer(bf)

        ninja_writer.comment("Define variables.")
        for name, value in variables:
            ninja_writer.variable(name, value)

        ninja_writer.newline()

        ninja_writer.comment("Define rules.")
        for rule, cmd, desc in rules:
            ninja_writer.rule(rule,
                              command = cmd,
                              description = desc)
            ninja_writer.newline()

        ninja_writer.comment("Define build targets.")
        for bld in builds:
            inputs = bld['inputs'] if 'inputs' in bld else None
            ninja_writer.build(
                bld['outputs'],
                bld['rule'],
                inputs=inputs)
            ninja_writer.newline()

        ninja_writer.comment("Define the default target.")
        ninja_writer.default(DEFAULT)
