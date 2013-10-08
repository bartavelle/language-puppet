#!/bin/bash
VERSION=$1
BUILDNUMBER=$2
if [ -z "$VERSION" ]
then
    echo "please set version"
    exit 4
fi

if [ -z "$BUILDNUMBER" ]
then
    BUILDNUMBER="0"
fi

cp equivs dist/build/puppetresources/ && \
    cp ruby/*.rb dist/build/puppetresources/ && \
    cd dist/build/puppetresources && \
    sed -i -e "s/Version: .*/Version: $VERSION-$BUILDNUMBER/" equivs && \
    equivs-build -f equivs && \
    mv puppetresources_*{_amd64.deb,_amd64.changes,.dsc,.tar.gz} ../../../

