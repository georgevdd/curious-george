pushd "C:\Projects\Java\Classes\"

jar cf "C:/Projects/Java/Jars/BSE.jar" gbmvdd/bse/ gbmvdd/util/Log.class gbmvdd/util/DefaultLog.class gbmvdd/util/AppletLog*.class gbmvdd/util/MenuLoader.class

popd

jar ufm "C:/Projects/Java/Jars/BSE.jar" "build/BSE-Manifest.mf"
