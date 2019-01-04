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

rsvg = {
    'comment': """\
Convert svg file to pdf. Images are located in `images' dir.\
""",
    'variable': {
        'rsvg_args': ['-f', 'pdf'],
        'images_dir': IMAGES_DIR
    },
    'rule': {
        'name': 'rsvg',
        'command': 'rsvg-convert $rsvg_args -o $out $in',
        'description': 'Convert svg to pdf.',
    },
    'builds': [{
        'rule': 'rsvg',
        'outputs': '$images_dir/{}.pdf'.format(image),
        'inputs': '$images_dir/{}.svg'.format(image),
    } for image in ['nano', 'micro']]
}

r2b = {
    'comment': """\
Convert restructuredtext to latex beamer. Doctutils offers stylesheet 
and theme configuration through arguments.
""",
    'variable': {
        'r2b_args': [
            '--stylesheet', 'my_beamer',
            '--theme', 'metropolis'
        ]
    },
    'rule': {
        'name': 'r2b',
        'command': 'rst2beamer $r2b_args $in $out',
        'description': 'Convert restructuredtext into latex.',
    },
    'builds': [{
        'rule': 'r2b',
        'outputs': 'slide.tex',
        'inputs': 'slide.rst',
        'implicit': 'my_beamer.sty'
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
        'outputs': 'slide.pdf',
        'inputs': 'slide.tex',
        'implicit': '$images_dir/nano.pdf'
    }]
}

if __name__ == "__main__":
    write_models(rsvg, r2b, tectonic)
    logging.info('Wrote build file')
