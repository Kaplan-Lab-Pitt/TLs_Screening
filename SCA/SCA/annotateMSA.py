#!/ihome/ckaplan/bid14/.conda/envs/sca/bin/python3
"""
The annotateMSA script provides utilities to automatically annotate sequence
headers (for a FASTA file) with taxonomic information. Currently this can be
done in one of two ways:

    1) For Pfam alignments, annotations can be extracted from the file
       pfamseq.txt (please download from:
       ftp://ftp.ebi.ac.uk/pub/databases/Pfam/current_release/database_files/pfamseq.txt.gz)

    2) For Blast alignments, annotations can be added using the NCBI Entrez
       utilities provided by BioPython. They can be based on GI or accession
       numbers that are used to query NCBI for taxonomy information (note that
       this approach requires a network connection).

To extract GI or accession numbers, use the scripts alnParseGI.py or
alnParseAcc.py, respectively.

For both the Pfam and NCBI utilities, the process of sequence annotation *can
be slow* (on the order of hours, particularly for NCBI entrez with larger
alignments). However, the annotation process only needs to be run once per
alignment.

**Keyword Arguments**
    -i, --input         Some input sequence alignment, Default: Input_MSA.fasta
    -o, --output        Specify an output file, Default: Output_MSA.an
    -a, --annot         Annotation method. Options are 'pfam' or 'ncbi'.
                        Default: 'pfam'
    -l, --idList        This argument is necessary for the 'ncbi' method.
                        Specifies a file containing a list of GI numbers
                        corresponding to the sequence order in the alignment; a
                        number of "0" indicates that a GI number wasn't
                        assigned for a particular sequence.
    -g, --giList        Deprecated. Identical to '--idList' and kept to keep
                        the CLI consistent with older versions of pySCA.
    -p, --pfam_seq      Location of the pfamseq.txt file. Defaults to
                        path2pfamseq (specified at the top of scaTools.py)
    -m, --delimiter     Character(s) used for separating fields in the sequence
                        headers of the annotated output. Default: '|'

**Examples**::

  annotateMSA -i PF00186_full.txt -o PF00186_full.an -a 'pfam'
  annotateMSA -i DHFR_PEPM3.fasta -o DHFR_PEPM3.an -a 'ncbi' -l DHFR_PEPM3.gi

:By: Rama Ranganathan, Kim Reynolds
:On: 9.22.2014

Copyright (C) 2015 Olivier Rivoire, Rama Ranganathan, Kimberly Reynolds

This program is free software distributed under the BSD 3-clause license,
please see the file LICENSE for details.
"""

import sys
import argparse
import os
from pysca import scaTools as sca
from pysca import settings

if __name__ == "__main__":

    # parse inputs
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "-i",
        "--input",
        required=True,
        dest="Input_MSA",
        help="input sequence alignment",
    )
    parser.add_argument(
        "-o",
        "--output",
        dest="output",
        default="Output.an",
        help="Outputfile name. Default: Output.an",
    )
    parser.add_argument(
        "-a",
        "--annot",
        dest="annot",
        default="pfam",
        help="Annotation method. Options are 'pfam' or 'ncbi'."
        " Default: 'pfam'",
    )
    parser.add_argument(
        "-l",
        "--idList",
        dest="idList",
        default=None,
        help="This argument is necessary for the 'ncbi' "
        "method. Specifies a file containing a list of "
        "GI or accession numbers corresponding to the "
        "sequence order in the alignment; a number of 0 "
        "indicates that one wasn't assigned for a "
        "particular sequence.",
    )
    parser.add_argument(
        "-g",
        "--giList",
        dest="idList",
        default=None,
        help="Command kept for compatibility with previous "
        "versions. Use '-l' or '--idList' instead.",
    )
    parser.add_argument(
        "-p",
        "--pfam_seq",
        dest="pfamseq",
        default=None,
        help="Location of the pfamseq.txt file. Defaults to "
        "path2pfamseq (specified in settings.py)",
    )
    parser.add_argument(
        "-d",
        "--pfam_db",
        dest="pfamdb",
        default=None,
        help="Location of the pfamseq.db file. Priority over "
        "pfamseq.txt file. Defaults to path2pfamseqdb "
        "(specified in settings.py)",
    )
    parser.add_argument(
        "-e",
        "--entrez_email",
        dest="email",
        default=None,
        help="email address for querying Entrez web API",
    )
    parser.add_argument(
        "-m",
        "--delimiter",
        dest="delimiter",
        default="|",
        help="delimiter for fields for generated FASTA files.",
    )
    options = parser.parse_args()

    if (options.annot != "pfam") & (options.annot != "ncbi"):
        sys.exit(
            "The option -a must be set to 'pfam' or 'ncbi' - other"
            " keywords are not allowed."
        )

    if options.annot == "ncbi":
        if (options.idList is None) and (options.giList is None):
            sys.exit(
                "To use NCBI Entrez annotation, you must specify a file "
                "containing a list of GI numbers (see the --idList "
                "argument)."
            )

    if options.annot == "pfam":
        # Annotate a Pfam alignment
        if options.pfamdb is not None:  # default to db query over txt search
            sca.AnnotPfamDB(
                options.Input_MSA,
                options.output,
                options.pfamdb,
                options.delimiter,
            )
        elif options.pfamseq is not None:
            sca.AnnotPfam(
                options.Input_MSA,
                options.output,
                options.pfamseq,
                options.delimiter,
            )
        else:
            # If no database or text file supplied to annotateMSA, then default
            # to the files defined in settings.py.
            if os.path.exists(settings.path2pfamseqdb):
                sca.AnnotPfamDB(
                    options.Input_MSA, options.output, options.delimiter
                )
            elif os.path.exists(settings.path2pfamseq):
                sca.AnnotPfam(
                    options.Input_MSA, options.output, options.delimiter
                )
            else:
                sys.exit("No Pfam file found. Exiting.")
    elif options.annot == "ncbi":
        # Annotate using GI numbers/NCBI Entrez
        if options.email is None:
            sca.AnnotNCBI(options.Input_MSA, options.output, options.idList)
        else:
            sca.AnnotNCBI(
                options.Input_MSA,
                options.output,
                options.idList,
                options.email,
                options.delimiter,
            )
