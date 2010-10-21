#!/usr/bin/env python
# -*- python-mode -*-
from tempfile import mkstemp
import complexity
import sys
import os


def main():
    if sys.stdin.isatty() and len(sys.argv) < 2:
        print "Missing filename"
        return

    temporary_file = None
    if len(sys.argv) > 1:
        file_name = sys.argv[1]
    else:
        content = sys.stdin.read()
        temporary_fd, file_name = mkstemp()
        temporary_file = os.fdopen(temporary_fd, "w")
        temporary_file.write(content)
        temporary_file.close()

    try:
        for score in complexity.compute_scores_for(file_name):
            print score.start_line, score.end_line, score.score, score.type_
    except:
        pass
    finally:
        if temporary_file is not None:
            os.unlink(file_name)

if __name__ == '__main__':
    main()
