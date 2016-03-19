#!/usr/bin/env python3

"""
    Brute force implementation
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

    Determines all possible arborescences in a given rooted digraph.
    Returns the one of min-weight. Arborescences are found by
    considering all edges extending the tree retrieved so far
    until the tree size hits the total number of vertices.

    Helps to verify testcases. Not meant for any other purposes.

    (C) 2016, CC-0, Lukas Prokop
"""

import os
import sys
import argparse
import itertools


def read_input_graph(filepath: str):
    """Given a filepath, read a digraph file.
    The format is specified in the script documentation.

    :param filepath:    A filepath of a digraph file
    :type filepath:     str
    :return:            set of vertices, edges and a root vertex
    :rtype:             ([int, ...], [(int, int, float), ...], int)
    """
    vertices = []
    edges = []
    root = None

    first = True
    with open(filepath, encoding='utf-8') as fd:
        for lineno, line in enumerate(fd):
            if line.startswith("#"):
                continue
            if first:
                vals = tuple(map(int, line.split()))
                assert len(vals) >= 3, "first line must contain 3 integers"
                assert vals[0] > 0, "number of vertices must be positive"
                assert vals[1] >= 0, "number of edges must be non-negative"
                assert vals[2] > 0, "root must be an existing vertex"
                vertices = list(range(1, vals[0] + 1))
                num_edges = vals[1]
                root = vals[2]
                first = False
            else:
                vals = line.split()
                assert len(vals) == 3, "every edge line must contain 3 values"
                assert int(vals[0]) > 0 and int(vals[1]) > 0, \
                    "vertices must be 1-enumerated (1..n)"
                edges.append((int(vals[0]), int(vals[1]), float(vals[2])))

    assert not first, "file must not be empty"
    assert len(edges) == num_edges, "Actual # of edges differs from specified"
    assert root in vertices, "root id exceeds vertex enumeration"
    assert all(s in vertices and d in vertices for (s, d, w) in edges)

    return (vertices, edges, root)


def find_arborescences(V, E, root, base=None):
    """Retrieve all arborescences.

    :param V:       set of vertices
    :type V:        {int, int, ...}
    :param E:       set of edges
    :type E:        {(int, int, float), ...}
    :param base:    an intermediate base tree retrieved
    :type base:     [(int, int, float), ...]
    :return:        generator for arborescences
    :rtype:         [[(int, int, float), ...]]
    """
    if not base:
        base = []

    nodes = {e[0] for e in base}.union({e[1] for e in base})
    if len(nodes) == len(V):
        yield base
        return
    if not nodes:
        nodes = [root]

    for node in nodes:
        for edge in filter(lambda e: e[0] == node, E):
            if edge[1] in nodes:
                # makes it cyclic
                continue
            yield from find_arborescences(V, E, root, base + [edge])


def main(filepath, print_base=False):
    """Main routine.

    :param filepath:        A filepath to a digraph file
    :type filepath:         str
    :param print_base:      Shall I print the base graph as 'b' lines?
    :type print_base:       bool
    """
    V, E, root = read_input_graph(filepath)
    max_branching = []
    total_weight = float('inf')

    for arb in find_arborescences(V, E, root):
        weight = sum([e[2] for e in arb])
        if weight < total_weight:
            max_branching = arb
            total_weight = weight

    print('{} {} {} {}'.format(max(V), len(max_branching), root, total_weight))
    if print_base:
        for (s, d, w) in E:
            print('b {} {} {}'.format(s, d, int(w) if w % 1 == 0.0 else w))
    for (s, d, w) in max_branching:
        print('{} {} {}'.format(s, d, int(w) if w % 1 == 0.0 else w))


if __name__ == '__main__':
    d = 'Brute-force implementation to find min-weight arborescence.'
    parser = argparse.ArgumentParser(description=d)
    parser.add_argument('digraphfile', help='source file to read graph from')
    parser.add_argument('-b', '--base', action='store_true',
                        help='print base graph as "b " lines')
    args = parser.parse_args()

    main(args.digraphfile, print_base=args.base)
