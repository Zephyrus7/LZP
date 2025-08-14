@echo off
echo Lojistik Zeka Platformu baslatiliyor, lutfen bekleyin...
echo Bu pencere uygulama calistigi surece acik kalmalidir.
R-Portable\bin\Rscript.exe -e "shiny::runApp('.', host='127.0.0.1', port=4960, launch.browser=TRUE)"