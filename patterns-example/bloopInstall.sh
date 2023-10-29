mvn generate-sources  \
    ch.epfl.scala:bloop-maven-plugin:2.0.0:bloopInstall \
    -DdownloadSources=true \
    -DscalacOptions=""   ##cannot override the erronous release 1.8 option

#TODO target error
