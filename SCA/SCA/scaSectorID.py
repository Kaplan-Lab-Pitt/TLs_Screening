#!/ihome/ckaplan/bid14/.conda/envs/sca/bin/python3
"""
The scaSectorID script does the preliminaries of sector identification and
stores the outputs using the python tool pickle:

    1) Chooses :math:`k_{max}` (the number of significant eigenmodes) by
       comparison of the :math:`\\tilde{C_{ij}}` eigenspectrum to that for the
       randomized matrices
    2) Rotates the top :math:`k_{max}` eigenvectors using independent
       components analysis
    3) Defines the amino acid positions that significantly contribute to each
       of the independent components (ICs) by empirically fitting each IC to
       the t-distribution and selecting positions with greater than a specified
       cutoff (default: p=0.95) on the CDF.
    4) Assign positions into groups based on the independent component with
       which it has the greatest degree of co-evolution.

**Key Arguments**
    --input, -i      \*.db (the database produced by running scaCore)
    --kpos, -k       number of significant eigenmodes for analysis (the default
                     is to automatically choose using the eigenspectrum)
    --cutoff, -p     empirically chosen cutoff for selecting AA positions with
                     a significant contribution to each IC, Default = 0.95
    --matlab, -m     write out the results of this script to a matlab workspace
                     for further analysis

**Example**::

  scaSectorID -i PF00071_full.db

:By: Kim Reynolds
:On: 8.19.2014

Copyright (C) 2015 Olivier Rivoire, Rama Ranganathan, Kimberly Reynolds

This program is free software distributed under the BSD 3-clause license,
please see the file LICENSE for details.
"""

import os
import pickle
import argparse
import numpy as np
from scipy.io import savemat
from pysca import scaTools as sca

if __name__ == "__main__":
    # parse inputs
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "-i",
        "--input",
        dest="inputdb",
        required=True,
        help="database from running scaCore",
    )
    parser.add_argument(
        "-o" "--output",
        dest="outputdb",
        default=None,
        help="output file for sector calculations",
    )
    parser.add_argument(
        "-k",
        "--kpos",
        dest="kpos",
        type=int,
        default=0,
        help="number of significant eigenmodes for analysis "
        "(the default is to automatically choose using "
        "the eigenspectrum)",
    )
    parser.add_argument(
        "-p",
        "--cutoff",
        dest="cutoff",
        type=float,
        default=0.95,
        help="number of significant eigenmodes for analysis "
        "(the default is to automatically choose using "
        "the eigenspectrum)",
    )
    parser.add_argument(
        "-m",
        "--matlab",
        action="store_true",
        dest="matfile",
        default=False,
        help="write out the results of this script to a "
        "matlab workspace for further analysis",
    )
    options = parser.parse_args()

    # extract the necessary stuff from the database...
    db_in = pickle.load(open(options.inputdb, "rb"))
    D_seq = db_in["sequence"]
    D_sca = db_in["sca"]

    msa_num = D_seq["msa_num"]
    seqw = D_seq["seqw"]
    lbda = D_sca["lbda"]
    Csca = D_sca["Csca"]
    tX = D_sca["tX"]
    Lrand = D_sca["Lrand"]

    # run the calculations
    Vsca, Lsca = sca.eigenVect(Csca)

    if options.kpos == 0:
        kpos = sca.chooseKpos(Lsca, Lrand)
    else:
        kpos = options.kpos
    print("Selected kpos=%i significant eigenmodes." % kpos)
    Vpica, Wpica = sca.rotICA(Vsca, kmax=kpos)
    ics, icsize, sortedpos, cutoff, scaled_pd, pd = sca.icList(
        Vpica, kpos, Csca, p_cut=options.cutoff
    )

    Usca = tX.dot(Vsca[:, :kpos]).dot(np.diag(1 / np.sqrt(Lsca[:kpos])))
    Upica = Wpica.dot(Usca.T).T
    for k in range(Upica.shape[1]):
        Upica[:, k] /= np.sqrt(Upica[:, k].T.dot(Upica[:, k]))
    Usica, Wsica = sca.rotICA(Usca, kmax=kpos)

    # saving...
    if options.outputdb is None:
        fn = os.path.basename(options.inputdb)
        output_path = os.path.abspath(os.path.dirname(options.inputdb))
    else:
        fn = os.path.basename(options.outputdb)
        output_path = os.path.abspath(os.path.dirname(options.outputdb))
    fn_noext = os.path.splitext(fn)[0]

    D = {}
    D["Vsca"] = Vsca
    D["Lsca"] = Lsca
    D["kpos"] = kpos
    D["Vpica"] = Vpica
    D["Wpica"] = Wpica
    D["Usca"] = Usca
    D["Upica"] = Upica
    D["Usica"] = Usica
    D["Wsica"] = Wsica
    D["ics"] = ics
    D["icsize"] = icsize
    D["sortedpos"] = sortedpos
    D["cutoff"] = cutoff
    D["scaled_pd"] = scaled_pd
    D["pd"] = pd

    db = {}
    db["sequence"] = D_seq
    db["sca"] = D_sca
    db["sector"] = D

    print(
        "Calculations complete, writing to database file "
        + os.path.join(output_path, fn_noext)
    )
    pickle.dump(db, open(os.path.join(output_path, fn_noext) + ".db", "wb"))

    if options.matfile:
        # increment indices by 1 for MATLAB
        db["sector"]["sortedpos"] = [pos + 1 for pos in D["sortedpos"]]
        for ic in ics:
            ic.items = [item + 1 for item in ic.items]
        db["sector"]["ics"] = ics
        savemat(
            os.path.join(output_path, fn_noext) + ".mat",
            db,
            appendmat=True,
            oned_as="column",
        )
