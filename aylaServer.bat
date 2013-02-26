java -Xmx3G -server -Djava.library.path=.\ -XX:+UseCompressedOops -XX:+DoEscapeAnalysis -Dj3d.allowSoleUser=true -cp .\scala\*;.\lib\*;.\bin\ ayla.server.AylaServer %1
