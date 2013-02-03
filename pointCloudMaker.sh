#!/bin/bash

java -Xmx4G -server -Djava.library.path=./ -XX:+UseCompressedOops -XX:+DoEscapeAnalysis -cp ./scala/*:./lib/*:./bin/ ayla.preprocess.PointCloudMaker $@
