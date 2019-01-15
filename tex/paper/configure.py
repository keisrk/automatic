# This file defines the project structure, dependency and build
# procedure.

import logging
import os
import sys

logging.basicConfig(level=logging.INFO)

sourcedir = os.path.dirname(os.path.realpath(__file__))
py3_dir = os.path.join(sourcedir, os.pardir, os.pardir, 'py3')
sys.path.append(py3_dir)
logging.info('Add module path {}.'.format(py3_dir))

from ninja_model import write_models

IMAGES_DIR = 'images'
TEX_PACKAGE_DIR = os.environ['HOME'] + '/tex_projects/llncs2e'

symlink = {
    'comment': """\
Tectonic does not offer a path for local tex package installation.
To use llncs style everytime we need to make a symlink to it.
""",
    'variable': {
        'tex_package_dir': TEX_PACKAGE_DIR,
    },
    'rule': {
        'name': 'symlink',
        'command': 'ln -s $in $out',
        'description': 'Make symbolic link to $in.',
    },
    'builds': [{
        'rule': 'symlink',
        'outputs': 'llncs.cls',
        'inputs': '$tex_package_dir/llncs.cls',
    }]
}

tectonic = {
    'comment': """\
Dependencies are automatically downloaded. We see no intermediate files 
`.log', `.aux' and so on.\
""",
    'rule': {
        'name': 'tectonic',
        'command': 'tectonic $in',
        'description': 'Compile tex into pdf.'
    },
    'builds': [{
        'rule': 'tectonic',
        'outputs': 'paper.pdf',
        'inputs': 'paper.tex',
        'implicit': [
            'llncs.cls',
            'algorithm.tex',
            'alter.tex',
            'encoding.tex',
            'introduction.tex',
            'prelim.tex',
            'reference.bib',
        ]
    }]
}

if __name__ == "__main__":
    write_models(symlink, tectonic)
    logging.info('Wrote build file')
