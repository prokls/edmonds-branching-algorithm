#!/usr/bin/env python3

"""
    Visualize graph
    ---------------

    This script takes a digraph file and print dot-syntax to represent
    the graph in a visually appealing way. As far as dot output is generated,
    it implies the installation of graphviz.

    (C) 2016, CC-0, Lukas Prokop
"""

import re
import argparse


def graph_to_dot(front_edges, background_edges, total_weight=None,
                 root=None, name='graph'):
    """Given a graph consisting of front edges and background edges and a name,
    generate a corresponding dot representation for the graph.

    :param front_edges:         a set of focused edges
    :type front_edges:          [(int, int, float), ...]
    :param background_edges:    a set of auxiliary edges
    :type background_edges:     [(int, int, float), ...]
    :param total_weight:        total weight of this graph
    :type total_weight:         float
    :param root:                a root vertex
    :type root:                 int
    :param name:                name of the digraph
    :type name:                 str
    :return:                    dot source code
    :rtype:                     str
    """
    def esc(n):
        return n
    if total_weight is not None:
        wrepr = ' (weight: {})'.format(total_weight)
    else:
        wrepr = ''

    output = 'digraph "' + esc(name) + '" {\n'
    if root is not None:
        output += '    {} [shape="circle" root="yes"];\n'.format(root)
    for (s, d, w) in front_edges:
        output += '    {} -> {} [label={}];\n'.format(s, d, w)
    for (s, d, w) in background_edges:
        if (s, d, w) in front_edges:
            continue
        tmpl = '    {} -> {} [label={} fontcolor="#cccccc" color="#cccccc"]\n'
        output += tmpl.format(s, d, w)
    output += '    label="{}";\n'.format(str(name) + wrepr)
    output += '}\n'
    return output


def read_graph(lines, base=True):
    """Given the lines of a digraph file, return the graph data.

    :param lines:   a sequence of lines
    :type lines:    [str, ...]
    :param base:    Shall I consider base edges?
    :type base:     bool
    :return:        front edges, background edges, total weight and root vertex
    :rtype:         ([(int, int, float), ...], [(int, int, float), ...],
                     float, int)
    """
    fedges = []
    bedges = []
    root = None
    total_weight = None
    first = True

    for lineno, line in enumerate(lines):
        if line.startswith('c ') or line.startswith('#') or not line.strip():
            continue
        elif first:
            first = False
            vals = line.split()
            assert len(vals) in [3, 4], "invalid header line"
            root = int(vals[2])
            if len(vals) == 4:
                total_weight = float(vals[3])
        elif line.startswith('b ') and not base:
            continue
        elif line.startswith('b '):
            vals = line.split()
            assert len(vals) == 4, "invalid base edge line {}".format(lineno)
            bedges.append((int(vals[1]), int(vals[2]), float(vals[3])))
        else:
            vals = line.split()
            assert len(vals) == 3, "invalid edge line {}".format(lineno)
            fedges.append((int(vals[0]), int(vals[1]), float(vals[2])))

    return (fedges, bedges, total_weight, root)


def main(srcfile, name='graph', base=True):
    """Main routine.

    :param srcfile:     File path to a digraph file
    :type srcfile:      str
    :param name:        name of the graph
    :type name:         str
    :param base:        Shall I consider base edges?
    :type base:         bool
    """
    if not name:
        name = srcfile
        if name.endswith('.in.graph'):
            name = name[0:-9]
        if name.endswith('.out.graph'):
            name = name[0:-10]

    with open(srcfile) as fd:
        fedges, bedges, tweight, root = read_graph(fd)
        print(graph_to_dot(fedges, bedges, tweight, root, name))


if __name__ == '__main__':
    desc = 'Visualize a graph file using GraphViz.'
    parser = argparse.ArgumentParser(description=desc)
    parser.add_argument('srcfile', help='source file to read graph from')
    parser.add_argument('-d', '--discard-base',
                        help='discard any base graph data')
    parser.add_argument('-n', '--name',
                        help='name of the generated graph')
    args = parser.parse_args()
    main(args.srcfile, name=args.name, base=not args.discard_base)
