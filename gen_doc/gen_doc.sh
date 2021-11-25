sphinx-quickstart 
# modify ./source/conf.py
## import os, sys
## sys.path.insert(0, os.path.abspath('../jxzhu_package'))
sphinx-apidoc -o ./source ../jxzhu_package
make html
# change theme (./source/conf.py)
## import sphinx_rtd_theme
## html_theme = "sphinx_rtd_theme"
## html_theme_path = [sphinx_rtd_theme.get_html_theme_path()]
make html
