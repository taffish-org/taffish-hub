("debian" "taffish-org/debian"
 "de7d99272d27a8e30b5b01889ac5c06889b41ae1	refs/tags/v12" "404: Not Found")
("python" "taffish-org/python"
 "7ee0abc69392bf88bf93fc28b7e1845d05ba7bbe	refs/tags/v3.13.2
08757f22ff2e52f0f9e2cd62c692a0879d7cb205	refs/tags/v3.12.9
bc938374839b8b69491c0011fdbbaa3af9091c0d	refs/tags/v3.11.11
e9d5cfb3dd13cd9ea4d513d0c6688913cf68c390	refs/tags/v3.10.16
b9f7c3abdb8c0ae67d2505e24c481a50cb370095	refs/tags/v3.9.21"
 "404: Not Found")
("conda" "taffish-org/conda"
 "466994620aa00097595f898e3fdadf445793e012	refs/tags/v24.9.2
bcee65198024fe987e04c6110b00ce71052eda0d	refs/tags/v3.12"
 "404: Not Found")
("sbcl" "taffish-org/sbcl"
 "e0c768f956afcb509e44410bf5f8de82aeb6a501	refs/tags/v2.5.2" "404: Not Found")
("R" "taffish-org/R"
 "1343743f111871dae8e244df555036e5670f6727	refs/tags/v4.4.3" "404: Not Found")
("intel-oneapi" "taffish-org/intel-oneapi"
 "6fd5aef4c550c558d3c62ec8ce55b94466854583	refs/tags/v2025.0.2-0-devel-ubuntu24.04"
 "404: Not Found")
("hello" "taffish-org/hello"
 "fbf581402f6de57361a77c08bdaceb7cb01ee88b	refs/tags/v1.0.0" "# taf-hello

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is made by TAFFISH.

Show hello by taf-debian-v12.")
("gui" "taffish-org/gui"
 "61f60dd2b6e41457454427a424947bddfaba4cba	refs/tags/v1.0.0" "# taf-gui

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from VNC and noVNC.

Use VNC and no VNC to support gui for taf apps.

## gui:v1.0.0 -- Use gui in your browser (an examply by taf-juicebox:v3.1.4 [port :: 5802])

For the gui mode, we do it together with [noVNC](https://github.com/novnc/noVNC) through VNC, which makes it possible to access the gui version of taf-apps through a browser, as shown in the following tutorials:

### 1. Run the following command on the CLI:

  ```bash
  taf update
  taf install -y gui  # or any gui-taf-app, such like taf-pymol, taf-juicebox, ...
  # restart terminal or [$ source ~/.bashrc] or [$ source ~/.zshrc] ...
  taf-gui --passwd 12345678 --port 5809
  # --c only support docker and podman, chose yours              [default podman>docekr]
  # --passwd will be used when you try to connect the gui        [default 12345678]
  # --port will be used when you try to connect the gui          [default 5809]    # it will only be set once when you first run taf-gui
  ```

### 2. Check the output and the URL

  Wait for the run to finish, check for errors, and if everything is fine, look at the end of the output, you can get a web address, for example:
  
  ```
  >>> TAFFISH-GUI ===> http://localhost:5809/vnc.html <<<
  ```

  Copy the URL and open it in your browser to access taffish-gui:
  
  <img width=\"1446\" alt=\"image\" src=\"https://github.com/user-attachments/assets/ea6fab03-6ca5-4204-ad6f-b3b1fcf18eff\" />
  
### 3. Enter the password and enter the GUI interface (the password is the --passwd set at runtime, the default is 12345678)

  <img width=\"1448\" alt=\"image\" src=\"https://github.com/user-attachments/assets/528e28e8-a082-4fd3-ae43-190b0497cbae\" />
  
### 4. Open the terminal in the GUI and enter the gui command to open your app by gui in the GUI interface

  <img width=\"1449\" alt=\"image\" src=\"https://github.com/user-attachments/assets/fe2cd3d5-1a14-43ee-a3b7-42bd2bebe629\" />
  
### 5. Use juicebox-gui

  <img width=\"1450\" alt=\"image\" src=\"https://github.com/user-attachments/assets/aeceff72-80ed-4cf9-b3c4-b77f851bd2e7\" />
  
  <img width=\"1449\" alt=\"image\" src=\"https://github.com/user-attachments/assets/65817b22-18f0-4b04-84d6-1337479362b3\" />
  
  <img width=\"1449\" alt=\"image\" src=\"https://github.com/user-attachments/assets/9a909b8e-34f8-4d1b-846e-18351df2cce0\" />
  
  Note:
  - By default, the /root/ path in the container needs to be retrieved from the global path /home/$USER
  - For the remote server, you can change the localhost in the URL to the server IP to access the taffish-gui on the remote server
  - Only a single screen is created for the same port/container service, and different accesses share the same screen. If you want to start a new service, please open a new docker/podman container(use another user to run taf-gui) and select a new port")
("muscle" "taffish-org/muscle"
 "814ff2be0abea6b8d64ceb94bf8156b9e6d9e299	refs/tags/v5.3" "# taf-muscle

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/rcedgar/muscle

Muscle is widely-used software for making multiple alignments of biological sequences. Multiple sequence and structure alignment with top benchmark scores scalable to thousands of sequences. Generates replicate alignments, enabling assessment of downstream analyses such as trees and predicted structures.")
("MCScanX" "taffish-org/MCScanX"
 "9a615cd0779d911695c0bea4e48ad07d3a776831	refs/tags/v1.0.0" "# taf-MCScanX

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/wyp1125/MCScanX

MCScanX: Multiple Collinearity Scan toolkit X version. The most popular synteny analysis tool in the world!")
("uniprot-idmapping" "taffish-org/uniprot-idmapping"
 "e1601e464357a8bde79a1b54339e21a73950ad86	refs/tags/v1.0.0"
 "# taf-uniprot-idmapping

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is made by ShishiYuan

A tool for connecting Uniprot.")
("juicer" "taffish-org/juicer"
 "b1c05cd30422bf10c28e91cb4d68b5674579336b	refs/tags/v2.20.00" "# taf-juicer

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/aidenlab/juicer

A One-Click System for Analyzing Loop-Resolution Hi-C Experiments.")
("blast" "taffish-org/blast"
 "729ec93364b85c2284ecbb5dec11d57efc187d53	refs/tags/v2.16.0" "# taf-blast

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from docker.io/ncbi/blast(https://blast.ncbi.nlm.nih.gov/Blast.cgi).

BLAST finds regions of similarity between biological sequences. The program compares nucleotide or protein sequences to sequence databases and calculates the statistical significance.")
("cdhit" "taffish-org/cdhit"
 "2a39ddc83105c66e4f258eebc6fec343d3b9a297	refs/tags/v4.8.1" "# taf-cdhit

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is made from github: https://github.com/weizhongli/cdhit

CD-HIT is a very widely used program for clustering and comparing protein or nucleotide sequences")
("hmmer" "taffish-org/hmmer"
 "7e93db958148da4f292c83fdff1410ed764ca387	refs/tags/v3.4" "# taf-hmmer

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This is made by github: https://github.com/EddyRivasLab/hmmer

HMMER is used for searching sequence databases for sequence homologs, and for making sequence alignments. It implements methods using probabilistic models called profile hidden Markov models (profile HMMs).")
("get-seqs-from-ids" "taffish-org/get-seqs-from-ids"
 "34dec31f6b8552e50b2105d6caff96dce4a9595f	refs/tags/v1.0.0"
 "# taf-get-seqs-from-ids

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This taf-app is written by common lisp(with package hanky-lisp: https://github.com/HermitHan/hanky-lisp) with sbcl:2.5.0(docker.io/fukamachi/sbcl:2.5.0)

Get-Seqs-From-IDs: this app use \"common lisp\"(need hanky-lisp package) to search ids(from a txt file which have line by line ids) from a fasta file(ids in '> ...' line), and format the ids' fasta seqs(or save to an output file)")
("gene-family-search" "taffish-org/gene-family-search"
 "8f2a737e9fb72716bf207662ca26cf21024b7c17	refs/tags/v1.0.0"
 "# taf-gene-family-search

- This is a taf-app(taf-flow), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is made by these taf-apps(taf-tools):
  - taf-cdhit :: https://github.com/HermitHan/cdhit
  - taf-hmmer :: https://github.com/HermitHan/hmmer
  - taf-blast :: https://github.com/HermitHan/blast
  - taf-get-seqs-from-ids :: https://github.com/HermitHan/get-seqs-from-ids

This taf-app can search the gene-family from your input: hmm(.hmm) and blast(.fasta) db.")
("sra-tools" "taffish-org/sra-tools"
 "c522cc53eeb901bd0e272bff120477ac8635e00b	refs/tags/v3.2.0" "# taf-sra-tools

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/ncbi/sra-tools

The SRA Toolkit and SDK from NCBI is a collection of tools and libraries for using data in the INSDC Sequence Read Archives.")
("seqtk" "taffish-org/seqtk"
 "98899dd97c6c8c7d064c24cc77319548df266f92	refs/tags/v1.4" "# taf-seqtk

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This is made by github: https://github.com/lh3/seqtk

Seqtk is a fast and lightweight tool for processing sequences in the FASTA or FASTQ format. It seamlessly parses both FASTA and FASTQ files which can also be optionally compressed by gzip.")
("fastqc" "taffish-org/fastqc"
 "10e4617652945737e197e34f2953bbef334aaf8c	refs/tags/v0.11.9" "# taf-fastqc

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from debian12:(apt install fastqc) (https://github.com/s-andrews/FastQC)

A quality control analysis tool for high throughput sequencing data.")
("trim_galore" "taffish-org/trim_galore"
 "492baf60e05788bed1cbe7aa79eafa702dc483e4	refs/tags/v0.6.10" "# taf-trim_galore

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is made from github: https://github.com/FelixKrueger/TrimGalore

A wrapper around Cutadapt and FastQC to consistently apply adapter and quality trimming to FastQ files, with extra functionality for RRBS data.")
("STAR" "taffish-org/STAR"
 "4ea1ebb56d82f7af02e85472e5e92c7f1207a7fd	refs/tags/v2.7.11b" "# taf-STAR

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is made from github: https://github.com/alexdobin/STAR

RNA-seq aligner.")
("subread" "taffish-org/subread"
 "71fcee1906074e91dfc02af99e0badeff68a7727	refs/tags/v2.0.6" "# taf-subread

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is made from github: https://github.com/ShiLab-Bioinformatics/subread

The Subread software package is a tool kit for processing next-gen sequencing data. It includes Subread aligner, Subjunc exon-exon junction detector and featureCounts read summarization program.")
("bedtools" "taffish-org/bedtools"
 "024eed316694ad68e2ce2713579b198f7276c38e	refs/tags/v2.31.1" "# taf-bedtools

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/arq5x/bedtools2

bedtools - the swiss army knife for genome arithmetic")
("samtools" "taffish-org/samtools"
 "74f344d0b20cad84ef0a583b1e490bef3dca74b0	refs/tags/v1.21" "# taf-samtools

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/samtools/samtools

Tools (written in C using htslib) for manipulating next-generation sequencing data.")
("bcftools" "taffish-org/bcftools"
 "7af41cae6c6fb4dc4d16c610b66b0439aa8f2343	refs/tags/v1.21" "# taf-bcftools

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/samtools/bcftools

bcftools - utilities for variant calling and manipulating VCFs and BCFs.")
("bowtie2" "taffish-org/bowtie2"
 "db33c9723668ef76f21a43b233fcdc30a254a666	refs/tags/v2.5.4" "# taf-bowtie2

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/BenLangmead/bowtie2

A fast and sensitive gapped read aligner.")
("fastp" "taffish-org/fastp"
 "b02b1a8a611de9da4f3172e8040faac0a990c823	refs/tags/v0.24.0" "# taf-fastp

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/OpenGene/fastp

An ultra-fast all-in-one FASTQ preprocessor (QC/adapters/trimming/filtering/splitting/merging...).")
("kallisto" "taffish-org/kallisto"
 "9aef9d6defc3a58169ec42433a9b6700c0dc8f88	refs/tags/v0.51.1" "# taf-kallisto

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/pachterlab/kallisto

Near-optimal RNA-Seq quantification.")
("bustools" "taffish-org/bustools"
 "e19712c0d6f997e97eb6efd2359d56653cd8a871	refs/tags/v0.44.1" "# taf-bustools

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/BUStools/bustools

Tools for working with BUS files.")
("hisat2" "taffish-org/hisat2"
 "0004d29c4b895e18496632630041163b1f429dbf	refs/tags/v2.2.1" "# taf-hisat2

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/DaehwanKimLab/hisat2

Graph-based alignment (Hierarchical Graph FM index)

(For now, this app is only for Linux-AMD64, can not use on ARM64)")
("mafft" "taffish-org/mafft"
 "37981e088534726cef4c5dcfaa2cfa7f36bcaf0a	refs/tags/v7.525" "# taf-mafft

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://mafft.cbrc.jp/alignment/software/source.html

Multiple alignment program for amino acid or nucleotide sequences.")
("clustal-omega" "taffish-org/clustal-omega"
 "aba647ee26cf9932c7e5ad8af6029b8d2b235a21	refs/tags/v1.2.4"
 "# taf-clustal-omega

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from http://www.clustal.org/omega/

Clustal Omega is the latest addition to the Clustal family. It offers a significant increase in scalability over previous versions, allowing hundreds of thousands of sequences to be aligned in only a few hours. It will also make use of multiple processors, where present. In addition, the quality of alignments is superior to previous versions, as measured by a range of popular benchmarks.")
("bwa" "taffish-org/bwa"
 "81b689e5a1870c2903108db9cfc62fbfa0a3d631	refs/tags/v0.7.18" "# taf-bwa

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/lh3/bwa

Burrow-Wheeler Aligner for short-read alignment (see minimap2 for long-read alignment).")
("gatk" "taffish-org/gatk"
 "b73af9acc3487f1f0224822a612b74e2b2d18bc6	refs/tags/v4.6.1.0" "# taf-gatk

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/broadinstitute/gatk

Variant Discovery in High-Throughput Sequencing Data.")
("salmon" "taffish-org/salmon"
 "d13130da3c7c3e04874cffac0dd349cdcf8b5c6e	refs/tags/v1.10.3" "# taf-salmon

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/COMBINE-lab/salmon

Highly-accurate & wicked fast transcript-level quantification from RNA-seq reads using selective alignment.")
("Trinity" "taffish-org/Trinity"
 "9e75dda72b4280661cfdf91b2bb3a85a7c629ddb	refs/tags/v2.15.2" "# taf-Trinity

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/trinityrnaseq/trinityrnaseq

Trinity RNA-Seq de novo transcriptome assembly.")
("freebayes" "taffish-org/freebayes"
 "c76bbcd13fa2016deb94bbc4f5075bdb04bc4214	refs/tags/v1.3.9" "# taf-freebayes

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/freebayes/freebayes

Bayesian haplotype-based genetic polymorphism discovery and genotyping.")
("pymol" "taffish-org/pymol"
 "9661e2499d0ce631a584f5a95f4ac5863d3cd31d	refs/tags/v3.1.0.gui
868a6e3901ba0bae66d1b69b798a504b6b5c0513	refs/tags/v3.1.0"
 "# taf-pymol

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/schrodinger/pymol-open-source

PyMOL is a source-available molecular visualization system created by Warren Lyford DeLano.

> We provide normal command line mode and GUI mode for pymol, for users who need GUI please get pymol:v3.1.0.gui, and for users who only need command line, you can only get pymol:v3.1.0

## pymol:v3.1.0.gui -- Use gui of pymol in your browser

For the gui mode, we do it together with [noVNC](https://github.com/novnc/noVNC) through VNC, which makes it possible to access the gui version of pymol through a browser, as shown in the following tutorials:

### 1. Run the following command on the CLI:
  
  ```bash
  taf update
  taf install -y pymol:v3.1.0.gui
  # restart terminal or [$ source ~/.bashrc] or [$ source ~/.zshrc] ... 
  taf-pymol-v3.1.0.gui --c {docker/podman} --passwd 12345678 --port 5801
  # --c only support docker and podman, chose yours              [default container]
  # --passwd will be used when you try to connect the gui        [default 12345678]
  # --port will be used when you try to connect the gui          [default 5801]
  #        it will only be set once when you first run taf-pymol
  ```

### 2. Check the output and the URL

  Wait for the run to finish, check for errors, and if everything is fine, look at the end of the output, you can get a web address, for example:
  
  ```
  >>> pymol-GUI ===> http://localhost:5801/vnc.html <<<
  ```
  
  Copy the URL and open it in your browser to access pymol-gui:
  
  <img width=\"1448\" alt=\"image\" src=\"https://github.com/user-attachments/assets/fac4efc9-377a-4ddc-af7e-8b9295a33067\" />


### 3. Enter the password and enter the GUI interface (the password is the --passwd set at runtime, the default is 12345678)

  <img width=\"1449\" alt=\"image\" src=\"https://github.com/user-attachments/assets/f7babc62-b53f-43a1-b8fe-72a27329ab53\" />

  
### 4. Open the terminal in the GUI and enter pymol to open pymol in the GUI interface

  <img width=\"1448\" alt=\"image\" src=\"https://github.com/user-attachments/assets/ff73fb1c-2646-485c-99c6-33d4475bf76d\" />


### 5. Use pymol-gui

  <img width=\"1448\" alt=\"image\" src=\"https://github.com/user-attachments/assets/b57426c8-33b7-4492-ab69-049f4d204af7\" />
  
  <img width=\"1449\" alt=\"image\" src=\"https://github.com/user-attachments/assets/8bd8676e-3b78-4aab-bb6a-7963df6d1c76\" />

  Note:
  - By default, the /root/ path in the container needs to be retrieved from the global path /home/$USER
  - For the remote server, you can change the localhost in the URL to the server IP to access the pymol-gui on the remote server
  - Only a single screen is created for the same port/container service, and different accesses share the same screen. If you want to start a new service, please open a new docker/podman container(use another user to run taf-pymol-v3.1.0.gui) and select a new port")
("hifiasm" "taffish-org/hifiasm"
 "8dd4240739936e56e43cdb475bcb7e8509d5c0fa	refs/tags/v0.25.0" "# taf-hifiasm

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/chhylp123/hifiasm

Hifiasm: a haplotype-resolved assembler for accurate Hifi reads.")
("haphic" "taffish-org/haphic"
 "98fa6b9f81716cec8792d3325d0b699820ce7da3	refs/tags/v1.0.7
302c685b03fdab3fcfe9e0e9b871a4ff55b53512	refs/tags/v1.0.6-taf1
db0085ac089a4f9af0e9dbe8c0057b1cef6ed455	refs/tags/v1.0.6"
 "# taf-haphic

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/zengxiaofei/HapHiC

HapHiC: a fast, reference-independent, allele-aware scaffolding tool based on Hi-C data
(We also add 'bwa', 'samtools', 'samblaster' to this taf-app)")
("autodock-vina" "taffish-org/autodock-vina"
 "291c478eff742d831d757f75fb28b0a8cea643a3	refs/tags/v1.2.7"
 "# taf-autodock-vina

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/ccsb-scripps/AutoDock-Vina

AutoDock Vina is one of the fastest and most widely used open-source docking engines. It is a turnkey computational docking program that is based on a simple scoring function and rapid gradient-optimization conformational search. It was originally designed and implemented by Dr. Oleg Trott in the Molecular Graphics Lab, and it is now being maintained and develop by the Forli Lab at The Scripps Research Institute.")
("rosetta" "taffish-org/rosetta"
 "a41f7caa2df94e9e774596724df977a6a251eafc	refs/tags/v387.ml" "# taf-rosetta

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/RosettaCommons/RoseTTAFold(https://hub.docker.com/r/rosettacommons/rosetta)

Rosetta has been at the forefront of computational biology, offering groundbreaking capabilities in the modeling, design and analysis of protein structures.")
("gromacs" "taffish-org/gromacs"
 "16133b40c045081327b77bebe0b292e6575e13ab	refs/tags/v2025.1" "# taf-gromacs

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/gromacs/gromacs

A free and open-source software suite for high-performance molecular dynamics and output analysis.")
("MMseqs2" "taffish-org/MMseqs2"
 "4f2d8e6ea93151d7db606f5c79c4b7d4761e9a33	refs/tags/v17" "# taf-MMseqs2

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/soedinglab/MMseqs2

MMseqs2: ultra fast and sensitive search and clustering suite")
("foldseek" "taffish-org/foldseek"
 "c91521cec9d8d2171510014b1601fbeebbf1ffb0	refs/tags/v10" "# taf-foldseek

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/steineggerlab/foldseek

Foldseek enables fast and sensitive comparisons of large structure sets.")
("esm-fold" "taffish-org/esm-fold"
 "8994103df79f96d7377a02eacb3b37413cd2e550	refs/tags/v1.0.3" "# taf-esm-fold

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/facebookresearch/esm

Evolutionary Scale Modeling (esm): Pretrained language models for proteins.")
("jellyfish" "taffish-org/jellyfish"
 "261419caab5a9d0fec3c632ec774ef2d41c51090	refs/tags/v2.3.1" "# taf-jellyfish

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/gmarcais/Jellyfish

A fast multi-threaded k-mer counter.")
("genomescope" "taffish-org/genomescope"
 "5cd2725f18c86adfde4ed58a7b090cb9c49065d1	refs/tags/v2.0.1
6625f6e122920136f93134a7c3db5ae94be29a94	refs/tags/v1.0
6625f6e122920136f93134a7c3db5ae94be29a94	refs/tags/v0.0.0"
 "# taf-genomescope

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/schatzlab/genomescope   (1.0)
- This app is from https://github.com/tbenavi1/genomescope2.0 (2.0)

1.0: Fast genome analysis from unassembled short reads
2.0: Reference-free profiling of polyploid genomes")
("kmc" "taffish-org/kmc"
 "1954f302f912dfd3f4cc4a8d548148fdd9c3360f	refs/tags/v3.2.4" "# taf-kmc

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/refresh-bio/KMC

Fast and frugal disk based k-mer counter")
("bamtools" "taffish-org/bamtools"
 "61509b3b9d7e7325e54fa04978dccce704d19601	refs/tags/v2.5.2" "# taf-bamtools

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/pezmaster31/bamtools

C++ API & command-line toolkit for working with BAM data")
("gfatools" "taffish-org/gfatools"
 "a617f23a7d13b5b5704bc64a2d4139218c2d5743	refs/tags/v0.5" "# taf-gfatools

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/lh3/gfatools

Tools for manipulating sequence graphs in the GFA and rGFA formats")
("quast" "taffish-org/quast"
 "eaaf4fd183579710d7b460b0c1d3eaba5399f648	refs/tags/v5.3.0" "# taf-quast

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/ablab/quast

Genome assembly evaluation tool")
("minimap2" "taffish-org/minimap2"
 "1623a30fad035b22b8a633e8f649f4df19c9154d	refs/tags/v2.29" "# taf-minimap2

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/lh3/minimap2

A versatile pairwise aligner for genomic and spliced nucleotide sequences")
("tgsgapcloser" "taffish-org/tgsgapcloser"
 "22f96f102b22b709ba87443d92d1e6d430db1196	refs/tags/v1.2.1" "# taf-tgsgapcloser

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/BGI-Qingdao/TGS-GapCloser

A gap-closing software tool that uses long reads to enhance genome assembly.")
("juicebox" "taffish-org/juicebox"
 "0ef59eceea1d9395c5133bbb92b9b12a37b45f19	refs/tags/v3.1.4" "# taf-juicebox

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/aidenlab/JuiceboxGUI

Juicebox is visualization software for Hi-C data.



## juicebox:v3.1.4 -- Use gui of juicebox in your browser

For the gui mode, we do it together with [noVNC](https://github.com/novnc/noVNC) through VNC, which makes it possible to access the gui version of juicebox through a browser, as shown in the following tutorials:

### 1. Run the following command on the CLI:

  ```bash
  taf update
  taf install -y juicebox:v3.1.4
  # restart terminal or [$ source ~/.bashrc] or [$ source ~/.zshrc] ...
   taf-juicebox-v3.1.4 --c {docker/podman} --passwd 12345678 --port 5802
  # --c only support docker and podman, chose yours              [default container]
  # --passwd will be used when you try to connect the gui        [default 12345678]
  # --port will be used when you try to connect the gui          [default 5802]
  #        it will only be set once when you first run taf-juicebox
  ```

### 2. Check the output and the URL

  Wait for the run to finish, check for errors, and if everything is fine, look at the end of the output, you can get a web address, for example:
  
  ```
  >>> juicebox-GUI ===> http://localhost:5802/vnc.html <<<
  ```

  Copy the URL and open it in your browser to access juicebox-gui:
  
  <img width=\"1446\" alt=\"image\" src=\"https://github.com/user-attachments/assets/ea6fab03-6ca5-4204-ad6f-b3b1fcf18eff\" />

### 3. Enter the password and enter the GUI interface (the password is the --passwd set at runtime, the default is 12345678)

  <img width=\"1448\" alt=\"image\" src=\"https://github.com/user-attachments/assets/528e28e8-a082-4fd3-ae43-190b0497cbae\" />

### 4. Open the terminal in the GUI and enter juicebox to open juicebox in the GUI interface

  <img width=\"1449\" alt=\"image\" src=\"https://github.com/user-attachments/assets/fe2cd3d5-1a14-43ee-a3b7-42bd2bebe629\" />

### 5. Use juicebox-gui

  <img width=\"1450\" alt=\"image\" src=\"https://github.com/user-attachments/assets/aeceff72-80ed-4cf9-b3c4-b77f851bd2e7\" />

  <img width=\"1449\" alt=\"image\" src=\"https://github.com/user-attachments/assets/65817b22-18f0-4b04-84d6-1337479362b3\" />

  <img width=\"1449\" alt=\"image\" src=\"https://github.com/user-attachments/assets/9a909b8e-34f8-4d1b-846e-18351df2cce0\" />
  
  Note:
  - By default, the /root/ path in the container needs to be retrieved from the global path /home/$USER
  - For the remote server, you can change the localhost in the URL to the server IP to access the juicebox-gui on the remote server
  - Only a single screen is created for the same port/container service, and different accesses share the same screen. If you want to start a new service, please open a new docker/podman container(use another user to run taf-juicebox-v3.1.4) and select a new port")
("busco" "taffish-org/busco"
 "c895dd59efa3b9935a342c6631595b2d89f0c965	refs/tags/v5.8.2" "# taf-busco

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from docker.io/ezlabgva/busco

BUSCO: Assessing Genomic Data Quality and Beyond")
("trf" "taffish-org/trf"
 "f5c09de47538bc246c2ac5d99561348f2a61a7a4	refs/tags/v4.09.1" "# taf-trf

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/Benson-Genomics-Lab/TRF

Tandem Repeats Finder: a program to analyze DNA sequences")
("augustus" "taffish-org/augustus"
 "fafb97cceb70e64b5cc3614bf022d17e0891cdca	refs/tags/v3.5.0" "# taf-augustus

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/Gaius-Augustus/Augustus

Genome annotation with AUGUSTUS")
("EVidenceModeler" "taffish-org/EVidenceModeler"
 "3bb39a56ceabb05cd89a63886812f1ab0d42099c	refs/tags/v2.1.0"
 "# taf-EVidenceModeler

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/EVidenceModeler/EVidenceModeler

The EVidenceModeler (aka EVM) software combines ab intio gene predictions and protein and transcript alignments into weighted consensus gene structures.")
("RepeatMasker" "taffish-org/RepeatMasker"
 "7f9a93397e328979493177cd75cc9b9636593bac	refs/tags/v4.1.8" "# taf-RepeatMasker

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://www.repeatmasker.org/

RepeatMasker is a program that screens DNA sequences for interspersed repeats and low complexity DNA sequences.")
("samblaster" "taffish-org/samblaster"
 "a2b7cd3919c5069b0fb5d4c59ca6622bce5348d8	refs/tags/v0.1.26" "# taf-samblaster

- This is a taf-app(taf-tool), you can use taffish(https://www.taffish.com) to use this taf-app.
- This app is from https://github.com/GregoryFaust/samblaster

samblaster: a tool to mark duplicates and extract discordant and split reads from sam files.")
