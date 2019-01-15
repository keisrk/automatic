import ninja_syntax

def write_model(ninja_writer, rule, builds, comment=None, variable={}):
    if comment:
        ninja_writer.comment(comment)

    for item in variable.items():
        ninja_writer.variable(*item)

    ninja_writer.rule(**rule)

    for b in builds:
        ninja_writer.build(**b)

def write_models(*models, file_name='build.ninja'):
    with open(file_name, 'w') as outfile:
        ninja_writer = ninja_syntax.Writer(outfile)

        for model in models:
            write_model(ninja_writer, **model)
