pushd "C:\Projects\Java\Classes\"

jar cf "C:/Projects/Java/Jars/TestLanguage.jar" gbmvdd/bse/ gbmvdd/util/Log.class

popd

jar ufm "C:/Projects/Java/Jars/TestLanguage.jar" "build/TestLanguage-Manifest.mf"
