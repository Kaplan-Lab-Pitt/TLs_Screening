#!/ihome/ckaplan/bid14/.conda/envs/sca/bin/python3
"""
The scaCore script runs the core calculations for SCA, and stores the output
using the Python tool pickle. These calculations can be divided into two parts:

    1) Sequence correlations:

        a) Compute simMat = the global sequence similarity matrix for the
           alignment
        b) Compute Useq and Uica = the eigenvectors (and independent
           components) for the following sequence correlation matrices:

            * unweighted (:math:`U^0`)
            * sequence weights applied (:math:`U^1`)
            * both sequence and position weights applied (:math:`U^2`)

    2) Positional correlations:

        a) Compute the single-site position weights and positional conservation
           values (:math:`D_i` and :math:`D_i^a`)
        b) Compute the dimension-reduced SCA correlation matrix
           :math:`\\tilde{C_{ij}}`, the projected alignment :math:`tX`, and the
           projector
        c) Compute Ntrials of the randomized SCA matrix, and the eigenvectors
           and eigenvalues associated with each

**Arguments**

**Keyword Arguments**
    -i               \*.db (the database produced by running scaProcessMSA)
    -n               norm type for dimension-reducing the sca matrix. Options
                     are: 'spec' (the spectral norm) or 'frob' (frobenius
                     norm). Default: frob
    -l               lambda parameter for pseudo-counting the alignment.
                     Default: 0.03
    --Ntrials, -t    number of randomization trials
    --matlab, -m     write out the results of these calculations to a MATLAB
                     workspace for further analysis

**Example**::

  scaCore -i PF00071_full.db

:By: Rama Ranganathan, Kim Reynolds
:On: 8.5.2014

Copyright (C) 2015 Olivier Rivoire, Rama Ranganathan, Kimberly Reynolds

This program is free software distributed under the BSD 3-clause license,
please see the file LICENSE for details.
"""

import sys
import time
import os
import pickle
import argparse
from scipy.io import savemat
from pysca import scaTools as sca

if __name__ == "__main__":

    # Parse inputs
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "-i" "--input",
        dest="inputdb",
        required=True,
        help="database from running scaProcessMSA",
    )
    parser.add_argument(
        "-o" "--output",
        dest="outputdb",
        default=None,
        help="output file for core calculations",
    )
    parser.add_argument(
        "-n",
        dest="norm",
        default="frob",
        help="norm type for dimension-reducing the sca matrix."
        "Options are: 'spec' (the spectral norm) or "
        "'frob' (frobenius norm). Default: frob",
    )
    parser.add_argument(
        "-t",
        "--Ntrials",
        dest="Ntrials",
        default=10,
        type=int,
        help="number of randomization trials",
    )
    parser.add_argument(
        "-l",
        dest="lbda",
        default=0.03,
        type=float,
        help="lambda parameter for pseudo-counting the "
        "alignment. Default: 0.03",
    )
    parser.add_argument(
        "-q",
        dest="kseq",
        default=30,
        type=int,
        help="number of eigenvectors to computes in sequence matrix. "
        "Default: 30",
    )
    parser.add_argument(
        "-c",
        dest="kica",
        default=15,
        type=int,
        help="number of independent components to compute from sequence "
        "alignment matrix. Default: 15",
    )
    parser.add_argument(
        "-m",
        "--matlab",
        dest="matfile",
        action="store_true",
        default=False,
        help="write out the results of these calculations to "
        "a MATLAB workspace for further analysis.",
    )
    options = parser.parse_args()

    if (options.norm != "frob") & (options.norm != "spec"):
        sys.exit(
            "The option -n must be set to 'frob' or 'spec' - other "
            "keywords are not allowed."
        )

    # extract the necessary stuff from the database...
    db_in = pickle.load(open(options.inputdb, "rb"))
    D_in = db_in["sequence"]

    msa_num = D_in["msa_num"]
    seqw = D_in["seqw"]
    Nseq = D_in["Nseq"]
    Npos = D_in["Npos"]
    ats = D_in["ats"]
    hd = D_in["hd"]

    # sequence analysis
    print("Computing the sequence projections.")
    Useq, Uica = sca.seqProj(
        msa_num, seqw, kseq=options.kseq, kica=options.kica
    )

    print("Computing sequence similarity matrix.")
    simMat = sca.seqSim(msa_num)

    # SCA calculations
    print("Computing the SCA conservation and correlation values.")
    Wia, Dia, Di = sca.posWeights(msa_num, seqw, options.lbda)
    Csca, tX, Proj = sca.scaMat(msa_num, seqw, options.norm, options.lbda)

    # Matrix randomizations
    print("Computing matrix randomizations...")
    start = time.time()
    Vrand, Lrand, Crand = sca.randomize(
        msa_num, options.Ntrials, seqw, options.lbda
    )
    end = time.time()
    print(
        "Randomizations complete, %i trials, time: %.1f minutes"
        % (options.Ntrials, (end - start) / 60)
    )

    # saving...
    if options.outputdb is None:
        fn = os.path.basename(options.inputdb)
        output_path = os.path.abspath(os.path.dirname(options.inputdb))
    else:
        fn = os.path.basename(options.outputdb)
        output_path = os.path.abspath(os.path.dirname(options.outputdb))
    fn_noext = os.path.splitext(fn)[0]

    D = {}
    D["Useq"] = Useq
    D["Uica"] = Uica
    D["simMat"] = simMat
    D["lbda"] = options.lbda
    D["Dia"] = Dia
    D["Di"] = Di
    D["Csca"] = Csca
    D["tX"] = tX
    D["Proj"] = Proj
    D["Ntrials"] = options.Ntrials
    D["Vrand"] = Vrand
    D["Lrand"] = Lrand
    D["Crand"] = Crand

    db = {}
    db["sequence"] = D_in
    db["sca"] = D

    print(
        "Calculations complete, writing to database file "
        + os.path.join(output_path, fn_noext)
    )
    pickle.dump(db, open(os.path.join(output_path, fn_noext) + ".db", "wb"))

    if options.matfile:
        savemat(
            os.path.join(output_path, fn_noext) + ".mat",
            db,
            appendmat=True,
            oned_as="column",
        )
