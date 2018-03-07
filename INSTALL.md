# Install OSCAR.jl

This installation instruction is for Ubuntu 17.10 (artful).
Please note that this installation process takes about 3 hours on a recent 4 core computer.
We do not provide any guarantees that it works or not. If you needed a fix for your OS, please
provide a pull request containing the fix.


## System requirements

Install all necessary packages using the following `apt-get` command:

```
apt-get install \
    ant ant-optional autoconf autogen \
    bliss build-essential bzip2 \
    clang debhelper default-jdk git \
    language-pack-en language-pack-el-base libbliss-dev libboost-dev \
    libboost-python1.62-dev libboost-python-dev libcdd0d libcdd-dev libdatetime-perl libflint-2.5.2 \
    libflint-dev libglpk-dev libgmp-dev libgmp10 libgmpxx4ldbl libmpfr-dev libncurses5-dev libnormaliz-dev libntl27 libntl-dev \
    libperl-dev libppl-dev libreadline6-dev libterm-readline-gnu-perl libterm-readkey-perl \
    libsvn-perl libtool libxml-libxml-perl libxml-libxslt-perl libxml-perl libxml-writer-perl libxml2-dev libxslt-dev \
    m4 make nano python-dev sudo wget xsltproc ninja-build \
    4ti2 graphviz gfortran cmake pkg-config patch libjson-perl curl
```

## Install julia

Install the latest julia release candidate from source using the following commands

```
wget https://github.com/JuliaLang/julia/releases/download/v0.6.2/julia-0.6.2-full.tar.gz
tar xf julia-0.6.2-full.tar.gz
rm  julia-0.6.2-full.tar.gz
cd julia-0.6.2
make -j8
make install
```

## Install Nemo.jl/Hecke

To install Nemo and Hecke in your julia installation, start julia via the `julia` command and execute

```
Pkg.clone( "https://github.com/thofma/Hecke.jl.git" )
Pkg.build( "Hecke" )
```

## Install Cxx.jl

To have Cxx.jl working correctly with polymake, you need to setup CXX correctly. This step is necessary for every session,
so we suggest putting the following line in your `.bashrc`.
```
export JULIA_CXX_RTTI=1
```
Afterwards, execute the following lines in julia:
```
Pkg.add( "Cxx" )
using Cxx
```

## Install Polymake and Polymake.jl

Execute the following commands in your shell to install polymake

```
wget https://polymake.org/lib/exe/fetch.php/download/polymake-3.2r1.tar.bz2
tar xf polymake-3.2r1.tar.bz2
rm polymake-3.2r1.tar.bz2
cd polymake-3.2
./configure
ninja -C build/Opt install
```
To install Polymake.jl, execute the following lines in the shell

```
export POLYMAKE_CONFIG=$(which polymake-config)
export JULIA_CXX_RTTI=1
```
then start julia and run the following commands
```
Pkg.clone("https://github.com/benlorenz/Polymake.jl.git")
Pkg.build("Polymake")
```

## Install Singular.jl

To install Singular.jl, execute the following commands in julia:

```
Pkg.clone("https://github.com/oscar-system/Singular.jl")
Pkg.build("Singular")
```
